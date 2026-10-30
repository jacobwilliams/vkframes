# Terrain tiles are drawn after their descriptors fail to compile, which crashes some drivers

## Summary

When the descriptors of a newly loaded terrain tile fail to compile, Rocky still puts the tile in the scene. `TerrainState::updateRenderModel()` creates the tile's `vsg::BindDescriptorSet`, calls `vsgcontext->compile(descriptors.bind)` and discards the returned `vsg::CompileResult`. The loader job then assigns the new render model to the tile, and the merge operation installs the bind in the tile's state group.

The bind's per-device Vulkan handles are never set, so recording the tile calls `vkCmdBindDescriptorSets` with a null `VkPipelineLayout` and a null `VkDescriptorSet`. MoltenVK records this without complaint. Mesa's KosmicKrisp driver dereferences the null set and the process crashes with `SIGSEGV` in `kk_bind_descriptor_sets`.

A compile can fail whenever VSG cannot reserve memory for the tile's images and buffers (`Context::reserve()` returns `VK_ERROR_OUT_OF_DEVICE_MEMORY`). We hit it because KosmicKrisp 26.2 reports a memory budget far below the real one (reported to Mesa separately), but the same thing would follow from any genuine shortage of device memory.

## Environment

- **Rocky:** `main` at a792a5f (2026-10-21, version 1.1.0). The code below is unchanged on `main` at ce5ac697 (2026-10-28).
- **VulkanSceneGraph:** 1.1.16 (conda-forge)
- **Vulkan driver:** Mesa KosmicKrisp 26.2.5 (conda-forge `mesa-kosmickrisp`); the Vulkan loader 1.4.357
- **Platform:** macOS 27.0 (26A428), Apple M5
- **Application:** vkframes, a Fortran library on VSG that embeds a Rocky `MapNode` (the Moon, with NASA Moon Trek imagery over the network and LOLA elevation from a GeoTIFF)

## Steps to reproduce

1. Run a Rocky map with network imagery on a driver whose `VK_EXT_memory_budget` is very small. KosmicKrisp 26.2.x on macOS reports a few hundred MB of a 24 GiB heap, and VSG trusts it when allocating.
2. Let tiles load for a few seconds.

Roughly half of our runs crashed 2 to 5 seconds after starting. Without the faulty budget, or on MoltenVK, none crashed.

## Actual result

With the Khronos validation layer:

```
Validation Error: [ UNASSIGNED-GeneralParameterError-RequiredHandle ]
vkCmdBindDescriptorSets(): layout is VK_NULL_HANDLE.
Validation Error: [ VUID-vkCmdBindDescriptorSets-layout-parameter ]
vkCmdBindDescriptorSets(): layout Invalid VkPipelineLayout Object 0x0.
```

followed by the crash:

```
0   libvulkan_kosmickrisp.dylib   kk_bind_descriptor_sets + 36
1   libvulkan_kosmickrisp.dylib   kk_CmdBindDescriptorSets2KHR + 48
2   libvulkan_kosmickrisp.dylib   vk_common_CmdBindDescriptorSets + 144
4   libvsg.1.1.16.dylib           vsg::RecordTraversal::apply(vsg::Geometry const&) + 124
6   libvsg.1.1.16.dylib           vsg::RecordTraversal::apply(vsg::StateGroup const&) + 112
9   librocky.61.dylib             rocky::TerrainTileNode::accept(vsg::RecordTraversal&) const + 1276
14  librocky.61.dylib             rocky::MapNode::traverse(vsg::RecordTraversal&) const + 872
```

We confirmed the cause with a check run on the main thread before each frame was recorded. It walked the scene's state groups and looked at each `BindDescriptorSet`'s own cached handles. The tile it found had the terrain's shared pipeline layout compiled, but neither the bind nor its descriptor set. Compiling that bind again through `viewer->compileManager->compile()` returned:

```
result -2 (Context::reserve() failed-2)
```

`compileManager->failedCompileCount` was already 1 or 2 by then: the earlier failures were Rocky's own compiles of new tiles.

## Expected result

A tile whose descriptors fail to compile is not drawn with them. It keeps its previous, compiled state (its parent's data, as before the load), and the failure is reported.

## Where

`src/rocky/vsg/terrain/TerrainState.cpp`, at the end of `TerrainState::updateRenderModel()`:

```cpp
    descriptors.bind = vsg::BindDescriptorSet::create(
        VK_PIPELINE_BIND_POINT_GRAPHICS,
        pipelineConfig->layout,
        DESCRIPTOR_SET_LOCAL, // first set
        descriptorSet
    );

    vsgcontext->compile(descriptors.bind);   // the CompileResult is discarded

    return renderModel;
```

`src/rocky/vsg/terrain/TerrainTilePager.cpp`, the loader job in `requestLoadData()`, then uses the new model unconditionally:

```cpp
            auto newRenderModel = tileFactory->state->updateRenderModel(tile->key, tile->renderModel, dataModel, vsgcontext);

            tile->renderModel = newRenderModel;
```

The merge in `requestMergeData()` then installs `tile->renderModel.descriptors.bind` in the tile's state group.

The subtile compile in `requestLoadSubtiles()` (`vsgcontext->compile(result);`) and `TerrainTileFactory::createTile()` also ignore the result, so a failure there would show up the same way.

## Suggested fix

Check the result, and keep the old (compiled) render model if the new one did not compile:

```diff
--- a/src/rocky/vsg/terrain/TerrainState.cpp
+++ b/src/rocky/vsg/terrain/TerrainState.cpp
@@ updateRenderModel
-    vsgcontext->compile(descriptors.bind);
+    auto cr = vsgcontext->compile(descriptors.bind);
+    if (cr.result != VK_SUCCESS)
+    {
+        Log()->warn("TerrainState: tile {} failed to compile ({}); keeping its previous state",
+            key.str(), cr.message);
+        return oldRenderModel;
+    }
 
     return renderModel;
```

Returning `oldRenderModel` keeps the tile drawable. It still holds references to the objects the function queued for disposal (the old color and elevation descriptors), so they stay alive.

With this change, the loader still reports success, so the tile stays at its inherited resolution until it is paged out and loaded again. A retry would be better: for example, `updateRenderModel()` could return a `Result` and the loader job could return `false` for a later attempt. That policy is yours to choose. The same check would apply to the subtile and root-tile compiles.
