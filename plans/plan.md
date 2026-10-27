# Plan: a Fortran OpenFrames on VulkanSceneGraph

A Fortran library with the features of
[OpenFrames](https://github.com/ravidavi/OpenFrames), the spacecraft-trajectory
visualization library used by NASA's Copernicus and GMAT. It renders with
[VulkanSceneGraph](https://github.com/vsg-dev/VulkanSceneGraph) (VSG), the
modern successor of OpenSceneGraph (OSG), which OpenFrames is built on.

This document stands on its own. It was written on 2026-10-25 from a survey
of OpenFrames (commit `798dc4a`, March 2026), conda-forge package versions as
of that date, and experience embedding OpenFrames in a Fortran Qt GUI.

## Goal

- **Features:** vkframes is a complete replacement for OpenFrames: **all**
  of its features, listed under *Feature map* below, improved where
  necessary. For each class, work from the full API in OpenFrames' header
  (`include/OpenFrames/*.hpp`), reuse its validated approach, and record any
  item not yet ported. Where vkframes deviates (better math, or what VSG and
  Vulkan allow), say so and why.
- **Language and backend:** written mainly in Modern Fortran, rendering with
  VSG (Vulkan).
- **Platforms:** macOS on Apple silicon, macOS 26 or later (Vulkan through
  KosmicKrisp; MoltenVK earlier), Linux and Windows. No Intel Macs.
- **Windows:** usable in its own window, or embedded in a Fortran GUI built
  with [qt-fortran](https://github.com/jacobwilliams/qt-fortran).
- **API:** a modern Fortran interface for trajectory simulations, designed
  for Fortran from the start.
  - OpenFrames is the reference for features and algorithms, not for its
    interface: its Fortran access (the `OF_Interface` C API plus
    openframes-fortran) was added on top of a C++ library, with flat
    functions and global state. Don't mirror it.
  - Derived types with type-bound procedures (`ReferenceFrame`,
    `Trajectory`, ...), Fortran arrays in double precision, optional and
    keyword arguments, object lifetimes through finalizers or explicit
    destroy calls, and no hidden global state.
  - The `bind(C)` handles in `src/vsg/vkf_layer.f90` are internal plumbing
    to the C++ layer, not the user API.

## Background

### OpenFrames today

- **Size:** about 27k lines of C++:
  - about 12k in core scene and rendering classes;
  - a 195-function C interface (`OF_Interface.h/.cpp`, about 5.4k lines);
  - OpenVR support (about 2.5k);
  - Qt adapters (about 1k);
  - 12 GLSL shaders (marker shapes, line pulse, shadow maps).
- **Fortran access:** through the C interface, via
  [openframes-fortran](https://github.com/jacobwilliams/openframes-fortran).
- **Main classes:**
  - `ReferenceFrame` (tree of frames with axes, labels, color), `Sphere`
    (textured body), `Model` (3D model file), `CoordinateAxes`,
    `LatLonGrid`, `RadialPlane`, cones (`PolyhedralCone`, `EllipticCone`,
    `RectangularCone`);
  - `Trajectory` (time, position, attitude and optional data),
    `TrajectoryFollower` (moves a frame along a trajectory over time),
    `DrawableTrajectory` with artists (`CurveArtist`, `SegmentArtist`,
    `MarkerArtist`);
  - `SkySphere` (star catalog background), `DepthPartitioner` (several depth
    slices to cover planet-to-spacecraft scales), `FocalPointShadowMap`,
    `SensorVisibilityCallback`;
  - `View` (camera following a frame: trackball, view between frames,
    perspective/orthographic), `RenderRectangle` (a viewport with views,
    background, HUD text), `WindowProxy` (a grid of viewports, a render
    thread, simulation time and time scale, input events, an embedded mode
    where the host supplies the OpenGL context).
- **OSG usage** (counts of uses in the source):
  - math types: `Vec3d` 191, `Matrixd` 73;
  - input and views: `GUIEventAdapter` 163, `Camera` 81;
  - rendering: `Texture2D` 77, `Geode` 75, `Geometry` 66, `StateSet` 49,
    `osgText::Text` 48, `Shader` 41, `Uniform` 39, `BlendFunc` 24,
    `MatrixTransform` 20, `Material` 20, `LineWidth` 17, `PointSprite` 8,
    `LineStipple` 4;
  - model loading through osgDB.

### Lessons from embedding OpenFrames in a Fortran Qt GUI

These come from a working demo that ran OpenFrames inside a qt-fortran window.

- **macOS threading.** With OpenGL, a context attached to a window must be
  updated on the main thread (`-[NSOpenGLContext setView:]` aborts
  elsewhere). The workaround:
  - pause the render thread;
  - `makeCurrent` + `doneCurrent` on the GUI thread at every expose and
    resize;
  - set `Qt::AA_DontCheckOpenGLContextThreadAffinity`.

  **Vulkan has no current context**, so a Vulkan design avoids this class of
  problem.
- **Trajectory artists** need their data sources set explicitly (x/y/z from
  positions). The C++ constructor does it only when given a trajectory. This
  kind of hidden default should be explicit in the new API.
- **Initialization.** The OpenFrames C interface must be initialized before
  any window call. The new API should not have global hidden state; use
  objects.
- **Assets** (textures, models, star catalog) need a data path. Plan a search
  path from the start.

### The VSG stack (all on conda-forge for osx-arm64, linux-64 and win-64)

| Package | Version | Role |
|---|---|---|
| `vulkanscenegraph` | 1.1.16 | Scene graph, Vulkan rendering, windows, viewer, trackball, text, `Builder` shapes |
| `vsgxchange` | 1.1.14 | Image and model loading: assimp (`.3ds`, `.obj`, glTF), freetype fonts, GDAL, `osg2vsg` for OSG formats |
| `vsgqt` | 0.5.0 | `QWindow`-based VSG windows for Qt (pre-1.0) |
| `vsgimgui` | 0.8.0 | Dear ImGui overlay (optional HUD/UI) |
| `mesa-kosmickrisp` | 26.2.5 | Vulkan on macOS (on Metal 4; Apple silicon, macOS 26+), the driver Khronos recommends since deprecating MoltenVK |
| `vulkan-loader`, `vulkan-headers` | 1.4.357 | Vulkan |
| `glslang`, `shaderc`, `spirv-tools` | current | GLSL → SPIR-V |
| `assimp` | 6.0.5 | Model import (pulled in by vsgxchange) |

The `vsg` package on conda-forge (3.35) is unrelated; don't use it.

VSG facts that shape the design:
- **Language:** C++17. Reference counting is `vsg::ref_ptr`; extensive
  templates. There is no C API, so Fortran needs a C++ shim with `extern "C"`
  functions.
- **Precision:** double-precision transforms (`dmat4`, `dvec3`) and reverse-Z
  depth are supported natively; together they handle huge scale ranges.
- **Shapes and text:** `vsg::Builder` creates spheres, boxes, cylinders and
  cones with textures; `vsg::Text` renders text.
- **Viewer:** `vsg::Viewer`, `vsg::View`, `vsg::RenderGraph` per viewport,
  `vsg::Trackball`.
- **Windows:** a VSG window can wrap a native window handle through
  `vsg::WindowTraits`. That is an alternative to vsgQt for embedding.

## Architecture

**The engine is in Fortran over a thin C++ VSG layer.**

```
 Fortran application (and optional qt-fortran GUI)
            |
 Fortran engine: frames, trajectories, followers, time, views, artists'
   geometry, viewports, API types             (about 8-12k lines of Fortran)
            |
 Fortran bind(C) interfaces to the layer below
            |
 C++ "vsg layer": opaque handles to VSG nodes, pipelines (thick lines,
   markers, stars, text), textures, model loading, windows, frame loop,
   screenshot; GLSL shaders compiled to SPIR-V   (about 4-6k lines + shaders)
            |
 VulkanSceneGraph, vsgXchange (+ vsgQt for Qt)
```

### Principles

- **Logic in Fortran, rendering in C++.** Everything that's logic lives in
  Fortran: the tree of frames, trajectory storage and interpolation,
  following, time control, camera logic, and geometry generation (orbit
  points, grids, cones, axes).
- **A small C++ layer** that knows nothing about spacecraft. It offers
  generic rendering primitives such as a transform node, a dynamic line
  strip, a point/marker set, text, a textured sphere, a model, and a
  viewport/camera.
- **Opaque handles across the boundary** (`type(c_ptr)`). Strings and arrays
  cross with explicit lengths. The C++ side owns VSG objects; Fortran
  releases them through `delete` calls or parent ownership.
- **Per-frame updates.** The frame loop calls a Fortran update (time
  advance, followers, camera), which updates transforms and dirty geometry
  through the layer. Use **one thread** (the GUI thread) for update and
  render to begin with. VSG and Vulkan don't need a separate render thread,
  and that avoids the threading complexity OpenFrames has.
- **Precision.** Positions in double precision throughout. Render
  camera-relative (subtract the eye position in double before converting to
  float for the GPU), with reverse-Z depth. This replaces OpenFrames'
  `DepthPartitioner`.
- **The C++ layer's bindings** can be hand-written at first. If the layer
  grows large, a small spec-driven generator (spec file → `extern "C"` shim
  and Fortran wrappers, as qt-fortran does) keeps it consistent.

### Embedding in a Qt GUI (qt-fortran)

qt-fortran is a Modern Fortran interface to Qt 6. It provides plain
`QWindow` handles, `QWidget_createWindowContainer`, and a general event
filter (`QtfEventFilter`) that forwards a window's expose, resize, mouse and
key events to Fortran slots.

Two ways to use it:
1. **vsgQt.** The C++ layer creates a `vsgQt::Window` (a `QWindow`) and hands
   its pointer to Fortran. Fortran wraps it as a qt-fortran `QWindow` (attach
   the pointer) and embeds it with `QWidget_createWindowContainer`.
2. **Native handle.** Fortran creates a `QWindow` with qt-fortran (a Vulkan
   surface type). The C++ layer builds a VSG window on its native handle
   (`winId()`) through `vsg::WindowTraits`. Fortran forwards events from a
   `QtfEventFilter` to the engine, and drives frames from a `QTimer` or the
   window's update requests.

Try option 1 first, and keep option 2 as the fallback while vsgQt is
pre-1.0. The engine itself should not depend on Qt: it must also run in a
plain VSG window.

### Python GUIs (PySide6)

The same native-handle route works from Python (`demos/pyside6`, done): the
application's Fortran (scene, windows, controls) is built by fpm as a shared
library (`[library] type = "shared"`, with vkframes as a shared library too)
exposing a small `bind(C)` interface; the PySide6 GUI loads it with ctypes,
owns the `QWindow`s (passing `winId()`, size and pixel ratio), forwards
events in VSG's conventions and renders a frame on each `UpdateRequest`.
vkframes needs nothing Python-specific: the C interface is the
application's own, not a generic binding of vkframes. Tested on macOS (one
and several windows); Linux and Windows are in CI.

## Feature map

| OpenFrames feature | Plan in VSG / Fortran | Difficulty |
|---|---|---|
| Reference-frame tree, positions, attitudes (quaternions), colors | Fortran types; `vsg::MatrixTransform` (`dmat4`) per frame | Easy |
| Trajectories (time, position, attitude, optionals), interpolation | Fortran (arrays, binary search, linear/Hermite interpolation) | Easy |
| `TrajectoryFollower` (loop/limit, position/attitude) | Fortran, per frame | Easy |
| Simulation time, time scale, pause, set time | Fortran, per frame | Easy |
| Models (`.3ds`, `.obj`, glTF; OSG formats) | vsgXchange (assimp; `osg2vsg`) | Easy |
| Textured sphere or ellipsoid (`Sphere`: radius, `setSphereScale` for ellipsoids, position/attitude within its frame, color, material), LOD | Ellipsoid mesh generated in Fortran (exact normals, geodetic texture mapping) + vsgXchange textures | Easy |
| Day/night textures on a sphere | Custom shader (sun direction uniform) | Moderate |
| Name labels, axis labels, HUD text | `vsg::Text` (font via vsgXchange/freetype, or `.vsgt`) | Moderate |
| Coordinate axes (with ticks), lat/lon grid, radial plane | Geometry generated in Fortran; line and transparent-surface pipelines | Moderate |
| Cones (polyhedral, elliptic, rectangular) | Geometry in Fortran; transparent pipeline | Moderate |
| `CurveArtist` (width, color, stipple pattern) | Thick lines as screen-space quads (vertex shader); dashes in the fragment shader | Moderate–hard (see Risks) |
| `SegmentArtist` (segments between data points) | Same line pipeline | Moderate |
| `MarkerArtist` (point sprites, start/intermediate/end markers, 9 marker-shape shaders, auto-attenuation) | Point pipeline (`gl_PointSize`); port the shaders to Vulkan GLSL → SPIR-V | Moderate |
| Star background (`SkySphere`, HYG catalog, magnitude → size and color) | Fortran reads the catalog; point pipeline drawn at infinity | Moderate |
| `DepthPartitioner` | Replaced by camera-relative double precision + reverse-Z | Moderate (design) |
| `View`: trackball, follow a frame, view between frames, ortho/perspective, default distance, reset | `vsg::Trackball` or Fortran camera logic setting `LookAt` per frame | Moderate |
| `RenderRectangle` / `WindowProxy` grid of viewports with separate views, backgrounds, HUD | Not planned as a grid: several windows (each with its views, background, HUD) instead | — |
| Input: mouse rotate/zoom/pan, keys | VSG events or forwarded Qt events → trackball | Easy–moderate |
| Window capture to image | Copy the swapchain image (VSG screenshot example) | Moderate |
| Shadows (`FocalPointShadowMap`), `SensorVisibilityCallback` | VSG 1.1 shadow support plus custom shaders | Hard (defer) |
| OpenVR | VSG's OpenXR work (vsgvr) | Defer |
| Qt widgets in 3D (`QWidgetPanel`), draggers, volumes, particles | — | Drop (unless needed) |

## Risks

1. **MoltenVK (macOS) has no wide lines, no geometry shaders and no line
   stipple.** OpenFrames uses line width and stipple, so lines must be
   rendered as quads expanded in the vertex shader, with dashes in the
   fragment shader. Prototype this in milestone 1. Linux and Windows drivers
   have wide lines, but use the same approach everywhere for consistency.
2. **vsgQt is pre-1.0.** API changes are possible. Keep the native-handle
   embedding as a fallback, and pin versions with pixi.
3. **Precision at planetary scales.** Design camera-relative rendering and
   reverse-Z in from the first milestone; retrofitting is painful.
4. **Text and fonts.** VSG text needs a font: a `.vsgt` file, or freetype
   loading through vsgXchange. Ship a font, or locate a system font.
5. **Shader toolchain.** Compile GLSL to SPIR-V at build time (glslang or
   shaderc from conda-forge) and embed the SPIR-V in the library, or load
   GLSL at run time through VSG's shader compiler. Choose one early (build
   time is simpler to ship).
6. **VSG API evolution.** VSG 1.x is stable but evolving; pin versions in
   pixi and keep the C++ layer thin.
7. **Fortran/C++ interop.** The usual rules apply:
   - `extern "C"` only, opaque pointers, no C++ exceptions across the
     boundary;
   - strings with explicit lengths;
   - callbacks from C++ into Fortran through `bind(C)` procedures, or a
     single dispatcher with ids (as qt-fortran does).

## Build and environment

- **Package manager:** pixi (conda-forge).
- **Builds:** fpm for the Fortran engine; CMake (or fpm's C++ support) for
  the C++ layer.

Starting `pixi.toml` dependencies:

```toml
[dependencies]
gfortran = "15.*"
fpm = ">=0.13.0,<0.14"
cxx-compiler = "*"
cmake = ">=3.25"
ninja = "*"
pkg-config = "*"
vulkanscenegraph = "1.1.*"
vsgxchange = "1.1.*"
glslang = "*"          # or shaderc, for SPIR-V at build time
python = "3.13.*"

[target.osx.dependencies]
moltenvk = "*"

# for the Qt-embedded variant (with qt-fortran as an fpm dependency):
# qt6-main = ">=6.11,<7"
```

(As set up: the Qt dependencies are a pixi feature and environment, `qt`,
used by `demos/qt`; vsgqt is not used.)

- **macOS environment (done):** KosmicKrisp (`mesa-kosmickrisp`) is the
  Vulkan driver. Qt depends on MoltenVK, so it is installed too; pixi's
  activation sets `VK_DRIVER_FILES` to KosmicKrisp's ICD alone. MoltenVK
  (deprecated by Khronos) was the driver until the switch.
- **Linking the C++ layer:** fpm can compile the C++ layer directly,
  supplying flags through `FPM_CXX` / `FPM_CXXFLAGS` / `FPM_LDFLAGS`, with VSG
  found via pkg-config or CMake. This is how qt-fortran works. Alternatively,
  build it as a shared library with CMake and link it from fpm.

Suggested layout:

```
src/            Fortran engine (frames, trajectories, time, views, artists, API)
src/vsg/        C++ VSG layer (extern "C") and its Fortran interface module
shaders/        GLSL sources (compiled to SPIR-V, embedded at build time)
data/           default font, test textures, star catalog subset
example/        examples: orbit around the Earth, multiple viewports, ...
test/           headless tests (geometry, interpolation, time) + render tests
```

## Milestones

### 1. Proof of concept (go/no-go)

Goal: settle the platform and design risks before writing the engine.

- [x] A pixi env with VSG, vsgXchange and MoltenVK. A C++ layer with a
      handful of `extern "C"` functions, called from a Fortran program.
- [x] A VSG window on macOS, Linux and Windows showing (macOS by hand;
      Linux in CI, with Mesa's software Vulkan driver; Windows: the demos,
      run by hand):
  - [x] a textured sphere (vsgXchange image loading);
  - [x] a loaded model (`.3ds` or glTF through vsgXchange);
  - [x] an orbit drawn with **thick lines** (screen-space quads), proven on
        MoltenVK;
  - [x] trackball control (VSG's trackball, driven by vkframes' View: every
        demo and example);
  - [x] a per-frame Fortran update moving a frame along a sampled orbit.
- [x] Camera-relative double-precision rendering with reverse-Z, checked
      with an Earth-sized body and a 10 m spacecraft in the same view
      (`example/precision.f90`).
- [x] Embedding in a qt-fortran GUI (the native-handle route; see below),
      including resize and mouse forwarding (`demos/qt`; resize, mouse and
      keys used by hand).
- [x] Screen capture to PNG (needed for automated render tests).

Exit: decide vsgQt or native handle, the shader build approach, and the
line-rendering technique.

Decided so far:
- **Shaders:** compiled to SPIR-V at build time (`glslangValidator`, by
  `tools/fpm_vsg.py`) into a generated `src/vsg/vkf_shaders.cpp`.
- **Lines:** no vertex buffers; points in a storage buffer, each segment
  expanded to a screen-space quad in the vertex shader (`gl_VertexIndex`),
  mitre joins between a strip's segments (square caps at its ends, at turns
  over 120 degrees and for separate segments), clipped at `w = ε`. Widths are in window units
  (logical pixels), scaled by the framebuffer/window pixel ratio on Retina.
- **Line data path:** Fortran writes points straight into the VSG-owned
  storage buffer (`c_f_pointer`, no intermediate copy): 6 floats per point
  (xyz high, xyz low; 24 bytes). Style (color, width) is a separate 32-byte
  buffer, so restyling never re-uploads points. The C++ layer only creates
  the buffers and marks them changed.
- **Meshes:** one generic C++ mesh primitive (vertices, normals, texture
  coordinates, indices; optional texture) drawn with VSG's Phong shader set;
  Fortran fills its arrays in place. Spheres are generated in Fortran
  (`vkf_geometry`: UV sphere, 128 x 64 segments by default, equirectangular
  texture, longitude 0 on +x), instead of OpenFrames' `ShapeDrawable` that is
  retessellated on the CPU when the view distance changes. `vsg::Builder` is
  not used.
- **Ellipsoids:** `vkf_geometry::ellipsoid` builds triaxial ellipsoids
  directly (spheres are the equal-axes case), parametrized by the surface
  normal: the vertex at geodetic (lat, lon) is the point whose normal is
  n(lat, lon). Normals are exact and texture rows are at geodetic latitude
  (`test/test_geometry.f90`: WGS84 within 4e-8 rad). OpenFrames instead
  scales a sphere (`setSphereScale` with `GL_RESCALE_NORMAL`), which skews
  the normals and maps the texture by parametric latitude.
- Still to port from OpenFrames' `Sphere`: body position/attitude within its
  frame (to reorient textures; a transform in the engine type), night
  texture on the dark side (needs a Sun light and a custom shader), material
  control, and auto LOD (a fixed 128 x 64 mesh may be too coarse for close
  views of large bodies; levels of a `vsg::LOD` rather than CPU
  retessellation). Auto LOD is done (2026-10-27): see the `Sphere` item
  under Milestones.
- **Models:** read through vsgXchange (`.3ds` from OpenFrames' demo data
  works, with textures); `vkf_node_bound` gives the bounding sphere for
  scaling and default view distances (as OpenFrames' `Model` uses it).
- **Test assets:** the examples take OpenFrames' demo data by path
  (`Demos/Images`, `Demos/Models`). Still to decide: which assets to ship
  in `data/`, and the search path.
- **Qt embedding: native handle, not vsgQt.**
  - conda-forge's `vsgqt` 0.5.0 is built against Qt 5, and qt-fortran is
    Qt 6. VSG 1.1.16 can itself render into an existing native window
    (`WindowTraits::nativeWindow`), which is all vsgQt does, so the C++
    layer needs no Qt.
  - Fortran owns the `QWindow` (qt-fortran; `winId()` was added to
    qt-fortran for this). On the first expose it creates the viewer on
    `winId()` with the `devicePixelRatio`. A `QtfEventFilter` forwards
    resize, mouse, wheel and keys, converted to VSG's conventions in Fortran
    (`vkf_viewer_mouse`, `_scroll`, `_key`, `_resize`, `_close` only push VSG
    events).
  - One thread: `updateRequest` advances the simulation, renders, and asks
    for the next frame.
  - macOS: Qt 6's `QNSView` keeps its own layer tree (a container layer), so
    MoltenVK finds no `CAMetalLayer` on it. The layer adds a plain child
    `NSView` filling Qt's view (Objective-C runtime calls, about 20 lines,
    `#if __APPLE__`) and gives that to VSG; events still reach Qt's view.
    The `QWindow` keeps the default surface type (with `VulkanSurface`, Qt
    manages the layer itself).
  - Windows/Linux: VSG's own embedding path (`HWND`, `xcb_window_t`), as
    vsgQt uses. **Not tried yet.** Risk on Linux: VSG's xcb window polls its
    own X connection, and only one X client may select `ButtonPress` on a
    window, so it may conflict with Qt's mouse handling. Wayland would need
    another route.
  - The demo is a separate fpm project (`demos/qt`, pixi environment `qt`:
    `pixi run -e qt qt-demo`); the vkframes library does not depend on Qt.
  - Pixel ratio (Qt's devicePixelRatio): passed to `WindowProxy%open`, sizes
    in framebuffer pixels (logical size times the ratio, rounded as Qt
    rounds), and a changed ratio passed to `WindowProxy%resize(...,
    pixel_ratio=)`. The demos check the ratio each frame, because moving a
    window to a screen of another scale needn't send a resize.
    - Retina (2x) is tested on macOS.
    - **Untested:** moving a window between screens of different scales,
      changing the display's scaling while running, and non-integer ratios
      (Windows 125% and 150%, fractional scaling on Linux).
  - The trajectory demo (`demos/trajectory`, `pixi run -e qt trajectory-demo
    [-- file.json]`) draws trajectory segments read from a JSON file
    (data/trajectory.json by default; json-fortran's `json_file`, a
    dependency of the demo only): a curve, markers and a body per segment,
    styled by an optional `style` object (documented in its main.f90). A
    dock editor shows each segment's style and changes it live (the
    artists' and Sphere's setters); File > Save / Save As write it back,
    replacing only the style keys the editor sets (the data and other keys
    as read); unsaved changes are asked about on closing.
  - libvsg exports no `type_info` for `vsg::Exception`, so on macOS a
    `vsg::Exception` thrown inside libvsg is not caught by type in our code;
    the layer's `guarded()` has a catch-all that recognizes it by name.
- **Build per platform** (`tools/fpm_vsg.py`): the libraries to link (VSG,
  Vulkan, the C++ runtime; the Objective-C runtime on macOS) are passed in
  `FPM_LDFLAGS`, not `fpm.toml`. On Windows, conda-forge's VSG is MSVC-built,
  so the layer is built with clang++ (MSVC target) into `build/vkf/vkf.dll`
  with a plain C interface (exports via `VKF_API`), and fpm compiles the
  layer's sources as empty files (`VKF_EXTERNAL_LAYER`); this is qt-fortran's
  approach for its Qt shim. **The Windows route is untested.**
- **CI** (`.github/workflows/ci.yml`): Ubuntu, macOS and Windows runners.
  Build everywhere; the geometry test everywhere; the Vulkan test and
  captured example renders on Linux (Mesa lavapipe, Xvfb) and macOS (Metal
  in the runner VM permitting); the Qt demo built everywhere and run on
  Linux and macOS (allowed to fail while those paths are unproven). Captures
  are uploaded as artifacts.
- **Efficiency follow-ups** (trajectories can be very large; see also
  [openframes-compare.md](openframes-compare.md), the comparison with
  OpenFrames and its list of future work):
  - VSG re-uploads a whole dynamic array when it is marked dirty. Growing
    trajectories should upload only the appended range, or use fixed-size
    chunks.
  - Each segment's quad transforms both endpoints in each of its 6
    vertices. Measured, this is only 11 to 16% of a frame, so instancing or
    an indexed layout isn't worth it for now. Most of the time is
    rasterizing; mitre joins (instead of square caps that overlapped) halved
    it (see openframes-compare.md).
  - Trajectory memory: `Trajectory%compact` frees the storage not used once
    a trajectory is complete (growth doubles, so up to about half can be
    unused; one copy of each array, an array at a time; nothing is redrawn),
    and `storage_bytes` reports what is held. `reserve` reserves attitudes
    (and colors) only when asked, so a trajectory without them holds no
    memory for them (it used to reserve attitudes by default).
  - Possible later: `Trajectory%adopt` (a program's allocatable arrays taken
    over with `move_alloc`, no copy) and `changed(i1, i2)` (after editing
    the data in place through `positions`, ..., only those points redrawn),
    so a simulation's data can live once in Fortran.
  - Decimation/LOD for very long trajectories seen from far away: done for
    curves (`CurveArtist%set_level_of_detail`; see openframes-compare.md),
    and for spheres (`Sphere%set_auto_lod`).
  - Optional future upgrades to consider (level of detail for the other
    elements; none is needed for speed so far):
    - `SegmentArtist` and `MarkerArtist`: thin out segments and markers seen
      from far away (they draw every one, as OpenFrames does).
    - `LatLonGrid`, `RadialPlane` and the cones: their circles and arcs have
      fixed point counts (as OpenFrames'), which could be too coarse up
      close.
    - `Model`: levels of detail for loaded models, if a model file provides
      them (VSG's `vsg::LOD`).
    - Spheres very close to the surface: past the finest mesh (1024
      segments), a body shown from close by needs terrain tiles instead
      (Rocky's `TerrainBody`).
- **Precision:** OpenFrames' method, adapted to VSG.
  - Trajectory points are (high, low) float pairs, drawn relative to the eye
    (RTE): a node in the record traversal inverts VSG's double model-view to
    get the eye, pushes it as a (high, low) pair (push constants), and zeroes
    the model-view translation; the shader uses OpenFrames' double-single
    subtraction. This is the same as OpenFrames' `DrawableTrajectory` /
    `TrajectoryArtist` (Cozzi & Ring, *3D Engine Design for Virtual Globes*).
  - Bodies and models under transforms need nothing extra: VSG accumulates
    transforms in double and converts only the final model-view to float.
  - Depth: reverse-Z with a 32-bit float depth buffer (VSG's default)
    replaces `DepthPartitioner`; 0.1 m to 1e8 m in one pass works.
  - Plain float puts a 7000 km orbit line about 1 m off a spacecraft seen
    from 30 m; RTE puts it through the center. (MoltenVK's fast math does
    not break the error-compensated subtraction.)
  - Still open: a planet's own surface vertices are float in the planet's
    frame (about 0.5 m at Earth radius). Fine for a textured sphere;
    close-up terrain would need tiles with local origins.

### 2. Core engine

- [x] Fortran types:
  - [x] `ReferenceFrame` (tree, position, attitude, color; axes, label and
        light settings stored, drawn later) — `src/vkf_frames.f90`;
  - [x] `Sphere` (radius, `set_sphere_scale` as an exact ellipsoid, sphere
        position/attitude, texture; night texture; radius, scale, color and
        material can change while shown: the mesh is scaled by its
        transform, the color goes through the material; auto LOD, on by
        default: meshes of 32 to 1024 segments around, the coarsest within
        half a pixel of the outline drawn per view (a `SphereLod` node), each
        built when first wanted) — `src/vkf_sphere.f90`; `Model` (file read at once so its
        bound is known, `share_model`, model position/attitude/scale/pivot)
        — `src/vkf_model.f90`;
  - [x] `Trajectory` (time, position, attitude, optionals; data sources) —
        `src/vkf_trajectory.f90`;
  - [x] `TrajectoryFollower` (LOOP/LIMIT, several trajectories, linear and
        slerp interpolation, fixed/offset time) — `src/vkf_follower.f90`;
  - [x] simulation clock (time, scale, pause, range, synchronization) —
        `src/vkf_clock.f90` (`SimulationClock`).
  - [x] the scene: `FrameManager` and `WindowProxy` (`src/vkf_window.f90`:
        own or host window, scene, OpenFrames' time calls, background,
        interim camera, `frame`/`run`, capture, host events). Frames get a
        transform node and a switch for their contents when first shown
        (`realize`, `build_contents`), and `sync` copies their transforms
        each frame. For now the tree must be complete before the first
        frame (adding nodes to a running window needs VSG's compile
        manager), and a frame's nodes are shared by all windows showing it.
  - [x] axes and labels drawn (`build_decorations`): OpenFrames' axis
        vectors (cylinder + cone, generated in Fortran: `vkf_geometry::arrow`)
        and labels; defaults as OpenFrames (all shown, label size 20, axes on
        a Sphere/Model's bound); `move_x_axis`/... as moveXAxis.

  Engine notes (deviations from OpenFrames):
  - Trajectories grow by doubling, positions and attitudes separately, and
    count changes (`revision`, `generation`) instead of notifying
    subscribers, so artists can upload only appended points. No locks (one
    thread). Indices are 1-based.
  - The follower is part of its frame (`frame%follow(traj)`), updated by
    `frame%update(t)`, rather than an OSG update callback.
  - Frames are owned by the program and the tree holds pointers; `detach`
    replaces OpenFrames' reference counting when a frame goes away.
- [x] `View` (`src/vkf_view.f90`): follow a frame (RELATIVE_FRAME or
      ABSOLUTE_FRAME), view between frames (VIEW_DIRECT / VIEW_AZEL),
      perspective/orthographic, default distance and parameters, reset; a
      window's list of views (add/select/next/previous), each keeping its
      own camera. As OpenFrames' FollowingTrackball, the trackball's eye,
      center and up live in the view frame's coordinates: the C++ camera is
      `LookAt * worldToView` (`ViewLookAt`), with worldToView computed in
      Fortran each frame; near/far follow the camera distance. The Qt demo
      has a view menu (Earth, spacecraft, spacecraft -> Earth) and a reset.
- Not planned: OpenFrames' grid of viewports in one window (nRow x nCol
  RenderRectangles). Several views at once are several windows instead
  (e.g. several Qt windows in a GUI), next item.
- [x] Several windows at once, showing the same scene (several views of it)
      or different scenes. Each WindowProxy keeps its own viewer, clock
      (synchronizable: `clock%synchronize(master)`) and views. All windows
      share the first one's Vulkan device (`WindowTraits::device`), so a
      scene's GPU data exist once. Line strips no longer depend on a window:
      the eye-relative node pushes the size of the view being recorded (and
      its pixel ratio, stored on the vsg::View) with the eye position, and
      the line pipeline is shared. `example/two_windows.f90` (standalone) and
      the Qt demo's "New window" (`--windows n`) show it.
- [x] Text labels and HUD text (`src/vkf_text.f90`, `src/vkf_hud.f90`,
      `shaders/label.vert`). Labels are vsg::Text with VSG's GPU layout (so
      text and color can change after they are shown; VSG's CPU layout
      cannot update) and VSG's text shader set, whose billboard GPU-layout
      variant is replaced by our shaders (compiled at build time). They size
      glyphs per view from the viewport pushed at record time, so labels are
      right in every window: name labels and HUD text at a fixed screen size,
      axis labels in object size capped on screen (OpenFrames' SCREEN_COORDS
      and OBJECT_COORDS_WITH_MAXIMUM_SCREEN_SIZE). Fonts by name on a search
      path (VKFRAMES_FONT_PATH, the environment's `fonts`, system font
      directories); DejaVu Sans from conda-forge (`font-ttf-dejavu`) is the
      default. HUD text (`HUDText`, `WindowProxy%add_hud_text`): OpenFrames'
      RenderRectangle HUD text (position in [0, 1], alignment, pixel size,
      color, font), any number per window and shown in several windows,
      drawn by a second VSG view with a fixed 0..1 projection, on top of the
      scene. Still to do: changing a frame's label text or color, or a HUD
      text's position/size/font, after it is shown.
- [x] Headless tests for interpolation, following, time and the frame
      transform chain (`test/test_trajectory.f90`, `test_follower`,
      `test_clock`, `test_frames`).
- [ ] Render tests by screen capture (deferred: captures are used by hand
      for now).

### 3. Artists and backgrounds

- [x] `DrawableTrajectory` (a frame holding artists; artists added after
      the frame is shown are built and compiled at the next update) and
      `TrajectoryArtist` (trajectory, trace mode) in `src/vkf_artists.f90`.
- [x] `CurveArtist`: data sources (x/y/z from time, position, optionals or
      attitude, scaled), width, color, trace mode (the points passed, a
      draw-range change, plus a two-point tail to the point interpolated at
      the current time; increasing or decreasing times). Drawn as a chain of
      line chunks: appended points are written in place into the last chunk
      (only that one is uploaded again), a full chain gets a new chunk (as
      large as the chain so far, up to 65536 points), so nothing is copied
      or reallocated; a cleared trajectory reuses the chunks; the last point
      is rewritten (`set_optional` changes it). The usual position sources
      are copied straight from the trajectory's storage (vectorized). Tested
      headless by reading the buffers back (`test/test_artists.f90`); the
      Qt demo draws its orbit with it (width, "Trace orbit").
- [x] Dash patterns (`CurveArtist%set_pattern`, `SegmentArtist%
      set_pattern`; low level `line_set_pattern`): OpenGL's stipple (16
      bits, bit 0 first, `factor` window units per bit). Each point carries
      its distance along the strip (8 floats per point: xyz and distance,
      high and low; measured in Fortran as points are written, only those
      written, across chunks). The shader's period is a power of two in
      scene units near the pattern's length on screen per segment (so
      fract(s / 2^n) is exact however long the trajectory), the two nearest
      blended so that dashes change size smoothly (0.7 to 1.4 times the
      pattern's length); they run on across the points (segments start
      their own, as GL_LINES). In `example/orbit.f90` and both demos
      ("Orbit pattern"); distances tested headless.
- [x] `SegmentArtist`: a segment at every `stride`-th point from the start
      to the end data sources (default, unlike OpenFrames' nothing: radius
      vectors from the origin), color, width, trace mode (the segments at the
      points passed). Same chunked buffers (`src/vkf_line_chain.f90`, shared
      with `CurveArtist`), drawn by the line shader in a separate-segments
      mode (a flag in the line's style buffer). Trace with decreasing times
      follows the trajectory's direction (OpenFrames counts from the wrong
      end there). Qt demo: "Radius vectors" adds/removes one while running.
- [x] Artists can be removed and added again while running: the layer keeps
      removed nodes alive for 32 more frames (frames in flight may still use
      them), the artist releases its handles and is built anew when added.
- [x] `MarkerArtist`: start/intermediate/end markers (colors per kind),
      intermediate spacing by time, arc length or data points, counted from
      the start (computed incrementally as points are appended) or the end;
      size in window units; auto-attenuation (OpenFrames' size *
      sqrt(R/d) for d > R, R from the points' bounds); shapes: OpenFrames'
      disk and all its Demos/Shaders/Marker_*.frag built into one fragment
      shader (`MARKER_*`; animated ones use the frame time), or an image
      (`set_marker_image`, tinted, transparent parts left out). Markers are
      screen-space quads (shaders/marker.vert), not point sprites: no
      device point-size limits, the same on all platforms. Trace mode (an
      addition): markers passed (the end marker once the end is reached).
- [ ] `MarkerArtist%set_marker_shader` (a user GLSL file): needs runtime
      GLSL → SPIR-V (VSG's ShaderCompiler / glslang); the shapes of
      OpenFrames' shader files are built in instead.
- [x] Sky background (`SkySphere`, `src/vkf_sky.f90`; a window shows one
      with `WindowProxy%set_sky`, several windows can share it): stars from
      a catalog in OpenFrames' format (its HYG v3 catalog is in
      `data/Stars_HYGv3.txt`; header `ra[_limit] dec mag ci proper`),
      magnitude range, number limit, size range and dimming as
      `setSkySphereStarData` (OpenFrames' size, color-index → RGB and dim
      formulas), and/or an equirectangular texture map (e.g. OpenFrames'
      `Images/StarMap.tif`, read by vsgXchange's GDAL); draw mode
      (`SKY_TEXTURE`, `SKY_STARS`), attitude. Rendered at infinity behind
      the scene (far plane, no depth writes): the texture as one
      viewport-covering triangle sampling along the view ray (no sphere
      mesh, no seam), stars as screen-space quads blended by the brighter
      color (OpenFrames' RGBA_MAX). Data files are found by name on
      `VKFRAMES_DATA_PATH` (`find_data_file`). Tested headless
      (`test/test_sky.f90`); the Qt demo has a "Sky" choice.
- [ ] Stars and sky texture in orthographic views (directions at infinity
      have no orthographic projection; drawn only in perspective views).

### 4. Remaining features and polish

- [x] Reference shapes (`src/vkf_shapes.f90`; OpenFrames' classes and
      settings): `CoordinateAxes` (axis lines, major/minor ticks as markers
      attenuated by the axis length, tick shape or image), `LatLonGrid`
      (geodetic latitude/longitude lines on an ellipsoid, equator and prime
      meridian thicker), `RadialPlane` (transparent disk, circles, radial
      lines), `PolyhedralCone`, `EllipticCone`, `RectangularCone` (sides,
      edges, base outline; `make_cone_look_at`; `is_visible`, which OpenFrames'
      PolyhedralCone lacks, for every cone). Rebuilt when a setting changes,
      also while shown. Surfaces are unlit, transparent, two-sided and write
      no depth (OpenFrames' state). `example/shapes.f90`; tested headless
      (`test/test_shapes.f90`); in the Qt demo.
- [x] Lighting (OpenFrames' model): a window's default light (headlight in
      eye coordinates; ambient 0.2, diffuse 1, no specular; `WindowProxy%
      set_light_*`), frame light sources (`ReferenceFrame%
      set_light_source_enabled`, ambient/diffuse/specular; a point light at
      the frame's origin, not dimmed with distance; while any is on, the
      window's light is off), sphere materials (`Sphere%set_material`), night
      textures (`set_night_texture_map`, shown where the diffuse light is
      weak). Lights are VSG light nodes read by vkframes' shader sets: VSG's
      Phong (and PBR, for models) with vkframes' fragment shaders, copies of
      VSG 1.1.16's with the changes marked (shaders/vsg/: separate specular
      colors as negative-color lights, no point-light falloff, the night map,
      view lighting), embedded as GLSL, which VSG compiles a variant of per
      set of #defines (its precompiled variants, of its own source, dropped);
      VSG's own headlight is not added. Earth day/night textures in `data/`
      (`2k_earth_daymap.jpg`, `2k_earth_nightmap.jpg`; equirectangular from
      -180 degrees longitude); textures are found by name on
      `VKFRAMES_DATA_PATH` (set to `data/` by the pixi tasks).
      `example/day_night.f90`; in both demos.
- [x] Vulkan on macOS through KosmicKrisp (Mesa's Vulkan on Metal, which
      Khronos recommends for Apple silicon after deprecating MoltenVK; macOS
      26 or later): `mesa-kosmickrisp` in pixi.toml, the loader pointed at it
      (`VK_DRIVER_FILES`; Qt still pulls in MoltenVK); the macOS platform's
      minimum is macOS 26, no Intel Macs. VSG's macOS window asks for
      MoltenVK's VK_MVK_macos_surface, so vkframes makes the windows'
      instance and surface itself with VK_EXT_metal_surface (on the window's
      CAMetalLayer; shared devices given after the window is made). A
      driver's memory budget is ignored when implausible (KosmicKrisp 26.2
      reports 64 KiB, and VSG then allocated nothing). The validation layer
      and vulkaninfo are in the environment (`VK_INSTANCE_LAYERS=
      VK_LAYER_KHRONOS_validation`).
- [ ] `WindowProxy%capture` reads the swapchain image after it is presented
      (the validation layer: non-acquired swapchain image used); copy it in
      the frame's own commands before presenting (with video recording).
- [x] Color as in OpenFrames: UNORM framebuffers and images read as UNORM
      (VSG defaults to sRGB, which brightened colors and blended in linear
      light), so colors, textures, transparency and lighting are in display
      values as in OpenGL.
- [x] Changing a shown scene: frames added to or removed from a shown tree
      (`add_child` realizes and compiles the new subtree; `remove_child`
      retires its node), axis arrows moved (`move_*_axis` rebuilds the arrow
      and label), a window's sky set or replaced (`set_sky`) and HUD text
      added or removed (`add_hud_text`, `remove_hud_text`), all while
      running; new nodes are compiled for the running viewer first.
      `example/live_changes.f90`; tested headless (`test/test_shapes.f90`).
- [x] Maps and terrain with Rocky (optional; https://github.com/pelicanmapping/rocky,
      osgEarth's successor on VSG): `TerrainBody` (src/vkf_terrain.f90; an
      addition to OpenFrames), a frame drawn from map layers in one of Rocky's
      profiles (the Earth, the Moon, ...) at `units_per_meter` (default km),
      image and elevation layers (TMS, GDAL files, geodetic and web Mercator
      tile templates; added before or after showing), lit by the scene's
      lights (`set_lighting`; Rocky's own default is unlit), the terrain's
      height (`elevation(lat, lon)`, read from its elevation file directly,
      also before it is shown), `rocky_available`.
      Build: `pixi run -e rocky rocky-build` builds a pinned Rocky
      (tools/build_rocky.py; not on conda-forge, its dependencies are) into
      the `rocky` environment (with Qt); fpm_vsg.py then builds the layer
      with it into build/rocky (C++20, -lrocky, ROCKY_FILE_PATH);
      src/vsg/vkf_rocky.cpp (one Rocky context per viewer, released before
      the viewer); without Rocky it only reports it unavailable. Changes to
      Rocky's C++ (tools/build_rocky.py):
      - `SRS.cpp`'s `contains()` calls qualified (as C++20, which EnTT 4.0
        requires, `unordered_map::contains` hides them). Bug report:
        plans/upstream/rocky-srs-contains-cxx20.md.
      - vsgXchange's image readers registered before Rocky's GDAL reader
        (GDAL decoded greyscale JPEG as one-channel 0-255 floats: a red
        Moon). Bug report: plans/upstream/rocky-gdal-reader-before-vsgxchange.md.
      - Rocky's tests and rocky_demo not built (they don't build with libc++
        in C++20; a workaround, not for upstream).
      In its shaders, copies of the two changed (shaders/rocky/shaders, first
      on ROCKY_FILE_PATH; their includes found in Rocky's shader directories)
      for sRGB-encoded terrain output on UNORM framebuffers and vkframes'
      lights (diffuse parts, and specular parts as negative colors) in
      Rocky's PBR lighting: specific to vkframes (upstream, these would be a
      request for hooks: the output encoding, the lights). The Moon's
      elevation: LRO LOLA's LDEM (tools/get_moon_dem.py, `pixi run -e rocky
      moon-data`, 4/16/64 pixels per degree) into data/. `example/rocky_map.f90`
      (the Earth from ReadyMap, the Moon from Moon Trek and LOLA, Sun-lit);
      the lunar Qt demo (demos/moon, `pixi run -e rocky moon-demo`; an
      orbiter, and a lander descending into Tycho, Moon-fixed, onto the
      terrain's height). Tested
      on macOS.
- [ ] Rocky, later: one map shown in several windows (now one per window:
      the window that first shows it), Linux and Windows (untested), a
      conda-forge recipe, submitting the two bug reports (plans/upstream),
      removing layers, a vertical exaggeration.
- [ ] Video recording (OpenFrames has only screenshots): `WindowProxy%
      start_recording(file, fps)` / `stop_recording`, frames sent as raw
      pixels to an `ffmpeg` process's stdin (`popen`, `_popen` on Windows;
      ffmpeg from conda-forge in `pixi.toml`, path and codec/quality
      options), no image files and no FFmpeg libraries linked. The frame is
      copied to one persistent readback buffer in the frame's own commands
      (no `deviceWaitIdle` per frame), the previous frame written while the
      current one renders. While recording the simulation time advances by
      exactly scale/fps per frame (Fortran drives the clock), so the video
      is smooth however long a frame takes. A Record toggle in both demos.
- [ ] Specular light on PBR models (glTF): ignored for now (PBR has no
      separate specular light color).
- [x] `CustomLineSegments` (OpenFrames' class of that name;
      `src/vkf_line_segments.f90`): a frame drawing line segments whose
      endpoints and colors (one per endpoint, blended along the segment) are
      set by the program (`set_segments`) or computed each frame by a
      callback (`set_callback`: an extension of the abstract
      `LineSegmentsCallback`, as OpenFrames' `CustomLineSegments::Callback`),
      e.g. lines of sight between spacecraft.
      - The callback runs in a new phase after the whole tree's update
        (`ReferenceFrame%late_update`), so other frames are at their positions
        for the same time whatever their order in the tree.
      - `ReferenceFrame%position_of(other)` gives another frame's origin in a
        frame's coordinates.
      - It's drawn by a `LineChain` of separate segments (width, dash
        patterns, double precision), with a color per point: lines made by
        `vkf_line_create_colored`, a storage buffer that `line.vert` reads.
      - Setting the same segments again uploads nothing.
      - Tested headless (`test/test_line_segments.f90`).
      - Shown in `example/line_of_sight.f90` (five orbits, every pair linked,
        blocked links dashed) and in the Qt demo ("Line of sight").
      - Not done: OpenFrames' custom line shader (`setLineShader`), which
        would need run-time GLSL, as `MarkerArtist%set_marker_shader`.
- [x] Day/night blending as OpenFrames', per pixel: the night texture is
      weighted by 1 − the lit color, per channel. The lit color is the
      material's ambient and diffuse in the lights' colors and intensities,
      plus its emission. So a brighter light narrows the twilight band and
      ambient light dims the night texture (OpenFrames' texture combiner does
      this per vertex). Nothing is body-specific: it follows each body's
      `set_material` and each light's colors.
      - `Sphere%set_twilight(angle)` (new): the diffuse light extended past
        the terminator, to where the light is `angle` below the horizon,
        meeting Lambert's smoothly. It's 0 (none) by default; the
        `day_night` example sets 6 degrees for the Earth.
      - The Phong shader has a per-mesh uniform `vkfBody` (binding 12) for
        it, and the C++ layer has `vkf_mesh_set_twilight`.
      - Not for `TerrainBody` (Rocky's own shaders) or loaded models.
- [x] `RibbonArtist` (new, not in OpenFrames): a translucent surface swept
      between two points at each trajectory point, a quad between each
      point's pair and the next. The pairs come from start and end data
      sources, as `SegmentArtist`'s. By default it runs from the origin to the
      positions, sweeping out an orbit's plane.
      - A trajectory artist, so it gets the artists' behaviour: appended points
        uploaded alone, a clear redrawn in place, trace mode (the quads
        passed as a draw range, plus a tail quad to the pair at the current
        time), and point colors with the artist's alpha.
      - The line chain has a new `CHAIN_RIBBON` kind: pairs of points, with
        chunks sharing their last pair.
      - The C++ layer draws it as a line node with a ribbon pipeline
        (`vkf_ribbon_create`, `shaders/ribbon.vert`): the same
        double-precision point buffer as the lines, drawn eye-relative, so
        its edge lies exactly on the orbit line. Translucent, unlit,
        two-sided, depth tested, writing no depth.
      - A trajectory going round several times covers the surface again each
        time, so it gets more opaque (documented).
      - Tested headless (`test_artists`: across chunks, appends, trace, other
        sources, colors). Shown in `example/orbit_planes.f90` and in the Qt
        demo ("Orbit plane (swept)", `--plane`).
      - [ ] Optional later: level of detail (fewer quads from far away, as the
            curves' stride).
- [x] Point colors (new, not in OpenFrames): a `Trajectory` can have a
      color per point (`add_color`, or `add`'s `color`, one at a time or in
      bulk). Like attitudes, colors may be fewer than times, and they take
      no memory unless used. They're stored as single-precision rgba, which
      is copied straight into the line buffers.
      - `CurveArtist` draws a trajectory with colors in them (on by
        default; `set_point_colors(.false.)` turns it off). Each segment
        blends from its first point's color to its second's.
      - Its chain holds colors only when they're drawn. It's made again,
        with or without colors, when that changes.
      - Points with no color yet are drawn in the artist's color, and are
        drawn again when their colors arrive.
      - In trace mode, the tail's color is interpolated too.
      - Tested headless (`test_artists`, `test_trajectory`). Shown in
        `example/colored_orbits.f90` and in the Qt demo ("Color orbit by
        altitude", `--altitude-colors`).
      - [ ] Optional later: point colors for `SegmentArtist` and
            `MarkerArtist`, and `"color"` per point in the trajectory
            demo's JSON.
- [x] Relative segment ends (new, not in OpenFrames): a `SegmentArtist`
      with `set_end_relative(.true.)` ends each segment at its start plus the
      end sources' values, so a vector stored per point (a thrust, a
      velocity) is drawn from each position; `set_end_scale` sets its length
      (e.g. km per m/s^2). OpenFrames' way, a tip stored per point, still
      works and is the default.
      - A new scale writes only this artist's segments again (all of them,
        once); the trajectory and its other artists are untouched. Drawing
        costs the same as before.
      - Where the vector is zero (no thrust) the segment's two ends are the
        same, and the line shader draws a separate segment of no length as
        nothing (it drew a cap's square before).
      - `set_start_data` and `set_end_data` set all three sources at once;
        `optional_sources(opt, scale=)` gives an optional's (beside
        `position_sources`).
      - Tested headless (`test_artists`: offsets, a changed scale, zero
        vectors, relative off again). Shown in `example/thrust_arcs.f90`.
      - [ ] Optional later: a scale on the GPU (start and vector uploaded
            once, `start + scale x vector` in the shader), for a length
            changed every frame (a slider, or following the zoom) without
            writing the segments again.
- [x] Shadow cones (new, not in OpenFrames; `src/vkf_shadow.f90`):
      `ShadowCones`, a frame showing a celestial body's shadow in the light
      of another as transparent cones: the umbra (to its apex, d R / (Rs - R)
      behind the body), the antumbra beyond it and the penumbra, their edges
      tangent to both spheres.
      - A separate frame, made a child of the body. It is turned on and off
        with `show_contents`, and a scene without one does no shadow work.
        A hidden one does nothing.
      - It runs in `late_update`: it moves to the body, turns its +z axis
        away from the light, and reshapes the cones when the distance or a
        setting changes. The cones are three meshes whose vertices are
        rewritten in place (`vkf_mesh_create` flag 4 and
        `vkf_mesh_vertices_changed`), so nothing is rebuilt or recompiled.
      - The radii come from the frames (a `Sphere`, a `TerrainBody`) or are
        set. Each region has a color and can be turned off. The length
        defaults to twice the umbra's.
      - Tested headless (`test/test_shadow.f90`, including the tangency).
      - Shown in `example/eclipse.f90` (a total solar eclipse) and in the Qt
        demo ("Earth's shadow", `--shadow`).
      - [ ] Deferred: darkening bodies and spacecraft inside a shadow. This
            would be an analytic sphere-occlusion test per light in the
            lighting shaders (it's also the cheap form of OpenFrames'
            `FocalPointShadowMap` for spherical bodies).
- [x] Window callbacks (OpenFrames' `setKeyPressCallback`,
      `setMouseMotionCallback`, `setButtonPressCallback`,
      `setButtonReleaseCallback`): `WindowEventHandler` (`src/vkf_window.f90`),
      an extension overriding the procedures it wants (`key_press`,
      `key_release`, `mouse_motion`, `button_press`, `button_release`; the
      others do nothing), set with `WindowProxy%set_event_handler`. It can
      hold its own state, where OpenFrames' plain function pointers could not.
      - The C++ layer only records the events (an event handler before the
        trackball, `vkf_viewer_events`). Fortran passes them on after the
        frame in which they arrived, so a handler may change anything,
        closing the window included. Embedded windows get the events their
        host forwards.
      - Positions are -1 to 1 across the window, y up, as OpenFrames'.
        `mouse_motion` covers drags as well, with the buttons held.
      - OpenFrames' built-in keys: `v` and `V` for the next and previous
        view, the space bar to reset the view (not with control, alt or
        meta). `key_press` can say it handled a key, which skips the built-in
        action (OpenFrames' callback can't), and `set_default_keys(.false.)`
        turns them off.
      - VSG's trackball keys are off (its space bar went back to the camera
        it started with, and w/a/s/d, q/e, i/o and the arrows moved the
        camera, which OpenFrames doesn't do and a program's keys would
        trigger). The mouse still moves the camera.
      - A view reset in a window of its own uses the window's current size
        (it used the size it was opened at, after a resize).
      - Tested with forwarded events (`test/test_events.f90`). Shown in the Qt
        demo (`v`/`V` through the view menu, `p` pauses, the pointer and the
        last click in the dock). Untested: key presses in VSG's own windows
        (they go through the same path).
- [ ] An OpenFrames-like convenience API (optional), examples (orbit,
      multiple viewports, a Qt GUI with controls), documentation (FORD).

### 5. Deferred

- Shadows cast on bodies, sensor visibility, VR (OpenXR), Qt widgets in 3D, draggers,
  volumes, particles.

### 6. Possible additional features

Ideas beyond OpenFrames for a spacecraft trajectory graphics library, not
yet planned in detail. Those marked **(first)** are the suggested order to
start in: the most useful for the least new machinery.

Interaction:
- [ ] **(first)** Picking: a click on a trajectory, frame or body gives the
      program what was hit (the frame, the trajectory point, its time and
      position), through the WindowEventHandler (e.g. a `pick` procedure, or
      `WindowProxy%pick(x, y)` from `button_press`). Built on the event
      handler; e.g. click a point of an orbit to go to its time.
- [ ] Built-in time controls: an optional time slider or HUD readout in a
      window of its own, with keys to play, pause and step; calendar times
      (UTC from a Julian date or MJD epoch) formatted for HUD text.
- [x] Smooth camera transitions, and the camera from a program
      (`src/vkf_window.f90`, `src/vkf_view.f90`):
      - `WindowProxy%get_camera` / `set_camera`: eye, center, up in the shown
        view's frame (OpenFrames' trackball getTransformation /
        setTransformation), or in its root frame (`in_root`, as the frames are
        now). `View%set_camera`: a view's camera for when it is next shown.
      - Flights: `set_camera(..., duration=)`; and `set_view_transition(s)`
        (default 0: a cut, as OpenFrames) for changes of view (`select_view`,
        `next_view`, `previous_view`, the keys v and V, or their own
        `duration`) and resets (the space bar). Both cameras are taken to the
        root frame each frame (their views' frames as they move), and
        `camera_between` (pure, in vkf_view) moves the center along a straight
        line, the distance geometrically and the orientation by slerp, eased in
        and out, over wall-clock time. A mouse press, `stop_transition`, or
        another flight or view ends it.
      - All in Fortran: the C++ layer only sets and reads the trackball's
        LookAt, as before (VSG's own animated viewpoints blend eye, center
        and up linearly, cutting through bodies between distant views).
      - Tested: `test_view` (camera_between), `test_camera` (in a window: set
        and get, the root frame, flights within and between views, a mouse
        press ending one, View%set_camera). Shown in `example/camera_tour.f90`
        and the Qt demo (the view change time in the dock).
      - [ ] Later: keyframed camera paths (several cameras with times, flown
            through; e.g. for presentations and video recording).

Astrodynamics graphics:
- [ ] **(first)** Ground tracks: a spacecraft's sub-satellite point traced on
      a rotating body (the ellipsoid geometry in vkf_geometry), as a curve on
      the surface.
- [ ] **(first)** Sensor footprints: where a sensor cone (EllipticCone,
      RectangularCone, PolyhedralCone) meets a body's ellipsoid, drawn as a
      curve on the surface, optionally sweeping out a swath along the
      trajectory.
- [ ] Osculating orbit: the two-body orbit through a frame's current state,
      redrawn as it moves (e.g. while thrusting), with its apsides and nodes
      marked.
- [ ] **(first)** Trajectories drawn in a moving frame, each point at its
      own time: one trajectory's data shown relative to a chief spacecraft
      (RIC/LVLH, for rendezvous), in a rotating frame (CR3BP, a body-fixed
      frame), without a second copy, by an artist transforming the points as
      it writes them (in Fortran, only the points written). Placing a
      DrawableTrajectory under a frame moves the whole curve rigidly with
      that frame's current position and attitude, which is right only for
      data computed in that frame. Today: transform the data into a second
      Trajectory, and show each copy in a scene of its own, in its own window
      (`example/earth_moon_frames.f90`: a lunar flyby, inertial and
      Moon-fixed, two scenes with synchronized clocks).
- [ ] Event markers and time labels: text at chosen trajectory points
      (maneuvers, apoapsis, eclipse entry) and tick marks at regular times.
- [ ] Covariance ellipsoids at chosen points of a trajectory (navigation,
      orbit determination).
- [ ] Ground stations: a site on a body, its elevation mask (a cone), and
      access lines while a spacecraft is above it (as the line_of_sight
      example, from a CustomLineSegments callback).

Data in and out:
- [ ] **(first)** Reading CCSDS OEM (positions, velocities) and AEM
      (attitudes) ephemeris files, as GMAT, STK and other flight dynamics
      tools write them, into Trajectories; and a plain CSV reader.
- [ ] Gaps in a trajectory: multi-arc data (e.g. arcs with a coast not
      propagated) drawn as separate pieces in one trajectory and artist.
- [ ] Interpolation choices: Hermite (from velocities, e.g. an optional) or
      Lagrange, for the points followed and, optionally, curves drawn smoothly
      between sparse points (now linear in both).

Running without a screen:
- [ ] **(first)** Offscreen rendering: drawing to an image with no window
      (VSG's offscreen framebuffer), for batch images from a simulation,
      servers, CI and the render tests by capture (milestone 2). Pairs with
      video recording.

Looks:
- [ ] An atmosphere's limb glow and a cloud layer (a second, slightly larger
      textured sphere, blended) for the Earth.
- [ ] Planetary rings (e.g. Saturn's): a textured annulus in the body's
      equatorial plane, its shadow on the body (with the shadow work).
- [ ] Articulated models: parts of a model (solar arrays, antennas) turned by
      data (an attitude per part from a trajectory or the program).

## Effort

- **Scope:** a rewrite of OpenFrames' rendering layer, not a port. About
  8–12k lines of Fortran plus 4–6k lines of C++ and about 15 shaders.
- **Time:** roughly 30–60 focused working sessions over several months.
  Milestone 1 is a few sessions and settles the real unknowns: MoltenVK
  lines, Qt embedding and precision. The rest is substantial but
  well-understood work.

## References

- OpenFrames: https://github.com/ravidavi/OpenFrames. Read `src/`,
  `include/OpenFrames/`, `Demos/Shaders/`, and `OFInterfaces/PyQtOF*.py`
  for the Qt integration.
- openframes-fortran: https://github.com/jacobwilliams/openframes-fortran
  (Fortran interface to the OpenFrames C API).
- VulkanSceneGraph: https://github.com/vsg-dev/VulkanSceneGraph. Also
  vsgExamples (https://github.com/vsg-dev/vsgExamples) for text, lines,
  screenshots and shadows, vsgXchange, and vsgQt.
- qt-fortran: https://github.com/jacobwilliams/qt-fortran (Fortran Qt 6
  bindings; `QWindow`, `QtfEventFilter`, `QWidget_createWindowContainer`,
  `QThread_create`, and `demos/openframes` for an OpenFrames embedding
  example).
