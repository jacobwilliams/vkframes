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
- **Platforms:** macOS (Vulkan through MoltenVK), Linux and Windows.
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
| `moltenvk` | 1.4.2 | Vulkan on macOS (on Metal) |
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
| `RenderRectangle` / `WindowProxy` grid of viewports with separate views, backgrounds, HUD | A `vsg::View` + `RenderGraph` per viewport | Moderate |
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

- **macOS (MoltenVK) environment:** the Vulkan loader must find MoltenVK's
  ICD JSON (`VK_ICD_FILENAMES` / `VK_DRIVER_FILES`). Check how conda-forge's
  `moltenvk` registers itself, and set it in a pixi activation script if
  needed.
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
- [ ] A VSG window on macOS, Linux and Windows showing:
  - [x] a textured sphere (vsgXchange image loading);
  - [x] a loaded model (`.3ds` or glTF through vsgXchange);
  - [x] an orbit drawn with **thick lines** (screen-space quads), proven on
        MoltenVK;
  - [ ] trackball control;
  - [x] a per-frame Fortran update moving a frame along a sampled orbit.
- [x] Camera-relative double-precision rendering with reverse-Z, checked
      with an Earth-sized body and a 10 m spacecraft in the same view
      (`example/precision.f90`).
- [x] Embedding in a qt-fortran GUI (the native-handle route; see below),
      including resize and mouse forwarding (`demos/qt`; resize checked by
      capture, mouse and keys not yet tried by hand).
- [x] Screen capture to PNG (needed for automated render tests).

Exit: decide vsgQt or native handle, the shader build approach, and the
line-rendering technique.

Decided so far:
- **Shaders:** compiled to SPIR-V at build time (`glslangValidator`, by
  `tools/fpm_vsg.py`) into a generated `src/vsg/vkf_shaders.cpp`.
- **Lines:** no vertex buffers; points in a storage buffer, each segment
  expanded to a screen-space quad in the vertex shader (`gl_VertexIndex`),
  square caps at the joins, clipped at `w = ε`. Widths are in window units
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
  retessellation).
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
- **Efficiency follow-ups** (trajectories can be very large):
  - VSG re-uploads a whole dynamic array when it is marked dirty. Growing
    trajectories should upload only the appended range, or use fixed-size
    chunks.
  - Each segment's quad transforms both endpoints in each of its 6
    vertices; instancing or a geometry-free indexed layout could cut that.
  - Decimation/LOD for very long trajectories seen from far away.
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

- [ ] Fortran types:
  - [x] `ReferenceFrame` (tree, position, attitude, color; axes, label and
        light settings stored, drawn later) — `src/vkf_frames.f90`;
  - [x] `Sphere` (radius, `set_sphere_scale` as an exact ellipsoid, sphere
        position/attitude, texture; night texture and auto LOD stored, drawn
        later) — `src/vkf_sphere.f90`; `Model` (file read at once so its
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
  - [ ] axes and labels drawn (needs text).

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
- [ ] A window with a grid of viewports (OpenFrames' RenderRectangles), each
      with its own views, scene and background.
- [ ] Several windows at once, showing the same scene (several views of it)
      or different scenes. Design notes: each WindowProxy keeps its own
      viewer, clock (synchronizable) and views; windows showing the same
      scene must share the Vulkan device so the scene's nodes (and their GPU
      data) are shared — create later windows with the first one's device
      (`WindowTraits::device` / `Window::share`). Frames' render nodes are
      already independent of any one window. Test with the Qt demo (two
      QWindows) and a standalone example.
- [ ] Text labels and HUD text.
- [ ] Headless tests for interpolation, following, time and the frame
      transform chain (done: `test/test_trajectory.f90`, `test_follower`,
      `test_clock`, `test_frames`); render tests by screen capture.

### 3. Artists and backgrounds

- [ ] `CurveArtist` (data sources, width, color, dash pattern),
      `SegmentArtist`.
- [ ] `MarkerArtist` (start/intermediate/end markers, spacing by time,
      distance or data; shapes from the ported marker shaders; size;
      auto-attenuation).
- [ ] Star background from the HYG catalog (magnitude range → size and
      color).

### 4. Remaining features and polish

- [ ] Coordinate axes with ticks, lat/lon grid, radial plane, cones.
- [ ] Day/night sphere textures, lighting controls (ambient, diffuse,
      specular, light position).
- [ ] An OpenFrames-like convenience API (optional), examples (orbit,
      multiple viewports, a Qt GUI with controls), documentation (FORD).

### 5. Deferred

- Shadows and sensor visibility, VR (OpenXR), Qt widgets in 3D, draggers,
  volumes, particles.

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
