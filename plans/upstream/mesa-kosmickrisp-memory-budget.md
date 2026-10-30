# kk: VK_EXT_memory_budget reports almost no budget on 26.2 (please backport 8ede4ccb)

## Summary

On Mesa 26.2.x, KosmicKrisp's `VK_EXT_memory_budget` heap budget is a small fraction of the heap. On an Apple M5 with 24 GiB, `vulkaninfo` reports between 298 MiB and 901 MiB, varying from run to run, while the process has allocated 64 KiB. Once an application has allocated a few hundred MB, the reported budget is no more than its current usage.

Applications and engines that respect the budget then refuse to allocate. VulkanSceneGraph, for example, sizes its allocations by `heapBudget - heapUsage`. In our application this made VSG allocate nothing, or made resource compiles fail with `VK_ERROR_OUT_OF_DEVICE_MEMORY`. In a library on top (Rocky), failed compiles were drawn anyway and crashed in `kk_bind_descriptor_sets`; that is the library's bug, reported to it separately.

Commit [8ede4ccb](https://gitlab.freedesktop.org/mesa/mesa/-/commit/8ede4ccb7300892dd004fb4cede8abb0823000f4) ("kk: Simplify memory budget calculations", 2026-08-10) replaced the calculation with `vk_physical_device_heap_budget_from_system()`. As far as we can tell from the source, that fixes it. The commit is on `main` and the `26.3` branch, so 26.3 will have the fix, but it is not on the 26.2 branch: 26.2.5 and 26.2.6 still have the old calculation. We'd like to ask for it to be backported to 26.2. We haven't built `main` or 26.3 to confirm the fix on this machine.

## Environment

- **Mesa:** 26.2.5 and 26.2.6 (conda-forge `mesa-kosmickrisp` 26.2.5 h2e9c0d1_0 and 26.2.6 h00e1ed3_0)
- **Vulkan loader:** 1.4.357 (conda-forge)
- **Hardware:** Apple M5 (Mac17,2), 24 GiB unified memory
- **OS:** macOS 27.0 (26A428)

## Steps to reproduce

Run `vulkaninfo` with KosmicKrisp as the only driver (`VK_DRIVER_FILES` pointing at its ICD) and look at the memory heap. Repeat a few times.

## Actual result

Three consecutive runs:

```
memoryHeaps[0]: size = 25769803776 (24.00 GiB); budget = 487030784 (464.47 MiB); usage = 65536 (64.00 KiB)
memoryHeaps[0]: size = 25769803776 (24.00 GiB); budget = 487079936 (464.52 MiB); usage = 65536 (64.00 KiB)
memoryHeaps[0]: size = 25769803776 (24.00 GiB); budget = 489357312 (466.69 MiB); usage = 65536 (64.00 KiB)
```

An earlier run reported 944881664 (901.11 MiB). With 26.2.6 (`driverInfo = Mesa 26.2.6`), three runs:

```
memoryHeaps[0]: size = 25769803776 (24.00 GiB); budget = 385679360 (367.81 MiB); usage = 65536 (64.00 KiB)
memoryHeaps[0]: size = 25769803776 (24.00 GiB); budget = 340672512 (324.89 MiB); usage = 65536 (64.00 KiB)
memoryHeaps[0]: size = 25769803776 (24.00 GiB); budget = 312426496 (297.95 MiB); usage = 65536 (64.00 KiB)
```

In our application, a VSG viewer with a Rocky terrain map, the budget read just after `vkCreateDevice` varied from about 120 MB to about 300 MB between runs. A few seconds later, with tiles loading, we read:

```
heap 0: size 25769803776, budget 292618240, usage 292618240
```

The budget equals the usage, so VSG computes 0 bytes available.

## Expected result

The budget is a plausible share of the 24 GiB heap: the memory this process could still allocate. It should not collapse to the process's current usage while gigabytes of system memory are free or reclaimable.

## Cause

`kk_get_sysmem_heap_budget()` in `src/kosmickrisp/vulkan/kk_physical_device.c` on the 26.2 branch (the same in the `mesa-26.2.6` tag):

```c
   uint64_t sysmem_used_B = sysmem_size_B - sysmem_available_B;
   uint64_t sysmem_budget_B =
      mtl_device_recommended_max_working_set_size(pdev->mtl_dev_handle);
   uint64_t remaining_budget_B =
      sysmem_budget_B > sysmem_used_B ? sysmem_budget_B - sysmem_used_B : 0u;
   return remaining_budget_B +
          mtl_device_current_allocated_size(pdev->mtl_dev_handle);
```

All memory in use on the system, by every process, is subtracted from the device's `recommendedMaxWorkingSetSize`. The used figure is the physical memory size minus free and inactive pages, from `os_get_available_system_memory()`. On a Mac in normal use, that is most of the physical memory. The remainder is then usually small or zero, leaving the budget at little more than `currentAllocatedSize`: the process's own usage.

On this machine, `vm_stat` showed about 6.9 GB in free and inactive pages. `main`'s `vk_physical_device_heap_budget_from_system(&pdev->vk, 0.9f, heap_size, used)` would then give a budget of about 6 GB plus usage, which is plausible.

## Workaround

In our application we ignore the extension when a heap's budget is below 10% of its size, and VSG then uses the heap sizes instead.
