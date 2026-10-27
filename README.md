# vkframes

Spacecraft trajectory visualization for modern Fortran.

[![Language](https://img.shields.io/badge/-Fortran-734f96?logo=fortran&logoColor=white)](https://github.com/topics/fortran)
[![Build Status](https://github.com/jacobwilliams/vkframes/actions/workflows/CI.yml/badge.svg)](https://github.com/jacobwilliams/vkframes/actions)
[![Docs](https://img.shields.io/badge/docs-API-blue)](https://jacobwilliams.github.io/vkframes/)

**vkFrames** is heavily inspired by [OpenFrames](https://github.com/ravidavi/OpenFrames), the [OpenSceneGraph](https://openscenegraph.github.io/openscenegraph.io/)-based
library used by tools such as NASA's GMAT and Copernicus. It has many of OpenFrames'
features, but an API designed for Fortran from the start: derived types with
type-bound procedures, double-precision arrays and optional arguments, and no
hidden global state. Vkframes is built on [VulkanSceneGraph](https://github.com/vsg-dev/VulkanSceneGraph) (VSG) and [Vulkan](https://www.vulkan.org/), providing high-performance, modern 3D graphics capabilities for Fortran applications.

> [!WARNING]
> **vkframes is highly experimental.** The API changes without notice, and
> some features are incomplete or missing. It has been used mainly on macOS.
> Linux is built and run only in CI, and Windows has had little testing (see
> [Platforms](#platforms)). Don't depend on it for anything important yet.

## Features

- **Reference frames**: a tree of frames with positions, attitudes
  (quaternions), axes, axis labels, name labels and colors. A `FrameManager`
  holds the scene.
- **Bodies**:
  - `Sphere`: a sphere or exact ellipsoid (such as WGS84) with day and night
    textures and a material. Its radius, color and material can change while
    it's shown.
  - `Model`: a 3D model file (`.3ds`, `.obj`, glTF, OSG formats through
    vsgXchange).
- **Trajectories**:
  - `Trajectory`: time, position, attitude and optional data, with
    interpolation.
  - `TrajectoryFollower`: moves a frame along a trajectory (loop or limit;
    position and/or attitude).
- **Drawing trajectories**: a `DrawableTrajectory` holds artists, which can
  be added, changed and removed at run time. Only changed data is uploaded.
  - `CurveArtist`: thick lines with dash patterns.
  - `SegmentArtist`: segments between data points.
  - `MarkerArtist`: markers at the start, the end and in between, in
    OpenFrames' marker shapes, with optional attenuation.
  - Trace mode: draw only up to the current time.
- **Reference shapes**: `CoordinateAxes` with tick marks, `LatLonGrid`,
  `RadialPlane`, and polyhedral, elliptic and rectangular cones.
- **Sky**: `SkySphere`, a star catalog (HYG v3) and/or a sky texture.
- **Lighting**: a frame can be a light source (such as the Sun), with
  ambient, diffuse and specular parts. Otherwise a headlight is used.
- **Views and time**:
  - `View`: a trackball camera that follows a frame, or looks from one frame
    toward another. Perspective or orthographic.
  - `SimulationClock`: time scale, pause and set time. Clocks can be
    synchronized between windows.
- **Windows**:
  - `WindowProxy`: a VSG window of its own, or one embedded in a host GUI on
    its native handle.
  - Several windows can share one scene and one Vulkan device.
  - HUD text and screen capture to PNG.
- **Precision**: double precision throughout. Lines are drawn relative to the
  eye and the depth buffer uses reverse-Z, so a 10 m spacecraft and an Earth
  orbit can share one view without OpenFrames' `DepthPartitioner`.
- **Maps and terrain** (optional, through
  [Rocky](https://github.com/pelicanmapping/rocky)): `TerrainBody`, a body
  with imagery and elevation layers (tile services or GeoTIFF), lit by the
  scene's lights. It can query terrain elevation. Earth and Moon profiles are
  available.
- **GUI embedding**: in Fortran Qt GUIs through
  [qt-fortran](https://github.com/jacobwilliams/qt-fortran), and in Python
  through PySide6.

Not done yet: shadows, sensor visibility, video recording, and level of
detail for very long trajectories. [plans/plan.md](plans/plan.md) has the
full feature map and what is still open.

## How it is built

- **Mostly Fortran.** The scene tree, trajectories and interpolation,
  following, time, cameras and all geometry (orbit points, ellipsoids,
  grids, cones, axes) are written in Fortran (`src/*.f90`).
- **A thin C++ layer** (`src/vsg/`) wraps VSG behind opaque handles and
  `bind(C)` interfaces. It covers nodes, pipelines, textures, model loading,
  windows and the frame loop, and knows nothing about spacecraft.
- **GLSL shaders** for lines, markers, stars, sky and labels
  (`shaders/`) are compiled to SPIR-V at build time and embedded in the
  library.
- **Everything runs on one thread** (the GUI thread). Vulkan has no current
  context, so there is no render thread and none of OpenGL's macOS
  workarounds.

### How it is developed

- vkframes is developed by Jacob Williams with AI assistance (Anthropic's
  Claude, through Claude Code). Most commits are co-authored with it.
- OpenFrames' headers are the reference for features and algorithms. Its C
  and Fortran interface is not the reference for the API.
- The design, decisions and remaining work are tracked in
  [plans/plan.md](plans/plan.md).
- All tools and dependencies come from conda-forge through
  [pixi](https://pixi.sh). The build uses [fpm](https://fpm.fortran-lang.org)
  (the Fortran Package Manager).
- CI (`.github/workflows/ci.yml`) builds on Ubuntu, macOS and Windows. It
  runs tests, renders examples on Linux with Mesa's software Vulkan driver,
  and builds the Qt demo.

## Dependencies

All of these are installed by pixi (see [pixi.toml](pixi.toml)):

| | |
|---|---|
| Fortran | gfortran 15, fpm 0.13 |
| C++ | a C++ compiler (clang++ on Windows, for conda-forge's MSVC-built VSG), CMake, Ninja |
| Rendering | VulkanSceneGraph 1.1, vsgXchange 1.1 (images, models, fonts), glslang (shader compiler) |
| Vulkan on macOS | [KosmicKrisp](https://docs.mesa3d.org/drivers/kosmickrisp.html) (Mesa's Vulkan on Metal) |
| Fonts | DejaVu Sans (the default label font) |
| Qt demos (`qt` environment) | Qt 6.11; qt-fortran and json-fortran are fetched by fpm |
| Python demo | Python 3.13, PySide6 |
| Rocky (`rocky` environment) | GDAL, PROJ, spdlog, glm, entt and others; Rocky itself is built from source |

## Platforms

| Platform | Status |
|---|---|
| macOS (Apple silicon, macOS 26 or later) | The main development platform. KosmicKrisp needs Metal 4, so Intel Macs and older macOS aren't supported. |
| Windows (x64) | Builds and runs (the C++ layer is a DLL built with clang++), but has had less testing. |
| Linux (x64) | Built and run in CI only, with software Vulkan (lavapipe) under Xvfb. Not tested on real GPUs or with the Qt demos on a desktop. |

Rocky has only been tried on macOS.

## Getting started

Install [pixi](https://pixi.sh), then from the repository's root:

```sh
pixi run build      # the library, the examples and the tests
pixi run test       # all tests (test_vulkan and others need a Vulkan device)
pixi run test test_geometry    # one test
```

Builds go through `tools/fpm_vsg.py`, which gives fpm the C++ compiler, the
VSG flags and the libraries to link, compiles the shaders, and puts
`data/` on `VKFRAMES_DATA_PATH`. Run fpm through these tasks rather than
directly.

If fpm reports odd errors or crashes after you pull changes that alter
derived types, remove its stale objects with `rm -rf build/gfortran_*` and
build again.

### Examples

```sh
pixi run example hello_sphere    # a sphere and trackball control
pixi run example earth -- texture.jpg model.3ds   # a textured Earth and a model following an orbit
pixi run example orbit           # an inclined orbit as a thick line, a dashed equator
pixi run example day_night       # day and night textures lit by the Sun
pixi run example shapes          # axes, lat/lon grid, plane and sensor cones
pixi run example two_windows     # one scene in two windows, clocks synchronized
pixi run example live_changes    # adding and removing frames while shown
pixi run example precision       # a 10 m spacecraft on a 7000 km orbit line
```

Most examples take an optional frame count and an image file to capture,
for example `pixi run example orbit -- 300 orbit.png`. The comment at the top
of each file in [example/](example/) lists its arguments. The `earth` example
needs a texture and a model file, such as OpenFrames' `Demos/Images/` and
`Demos/Models/Hubble.3ds`.

### Demos

Each demo is a separate fpm project in [demos/](demos/) that depends on
vkframes. The library itself doesn't depend on Qt or Python.

**Qt demo** ([demos/qt](demos/qt)): an orbiting spacecraft in a qt-fortran
main window. A dock of controls covers time, follow modes, line styles,
markers, the sky, the Sun's light and reference shapes. More windows can be
opened, each with its own view, and a time slider shows and sets the time.

```sh
pixi run -e qt qt-demo
pixi run -e qt qt-demo -- --windows 2 --shapes    # options are listed in demos/qt/app/main.f90
```

To use OpenFrames' Hubble model and star map, point `OPENFRAMES_DATA` (or
`--data`) at OpenFrames' `Demos` folder.

**Trajectory file demo** ([demos/trajectory](demos/trajectory)): reads
trajectory segments from a JSON file (with json-fortran) and draws each one
as a trajectory. A dock editor shows each segment's style (line, markers,
body). Changes appear at once, and File > Save / Save As write the file back.

```sh
pixi run -e qt trajectory-demo                    # data/trajectory.json
pixi run -e qt trajectory-demo -- my_segments.json
```

The file format and every style option are documented at the top of
[demos/trajectory/app/main.f90](demos/trajectory/app/main.f90).

**PySide6 demo** ([demos/pyside6](demos/pyside6)): the same kind of scene in
a Python GUI. The demo's Fortran is built as a shared library and loaded
with ctypes.

```sh
pixi run pyside6-demo
```

**Lunar demo** ([demos/moon](demos/moon)): the Moon with Rocky terrain (LRO
imagery and LOLA elevation), an orbiter, and a lander descending into Tycho
crater. Several views include one from the landing site. It needs the
`rocky` environment and a one-time Rocky build:

```sh
pixi run -e rocky rocky-build     # builds Rocky from source into the environment
pixi run -e rocky moon-data       # downloads LOLA's elevation (33 MB) into data/
pixi run -e rocky moon-demo
pixi run -e rocky example rocky_map -- moon     # or: earth
```

### Benchmark against OpenFrames

[bench/](bench/README.md) has two qt-fortran apps that draw the same scene,
one with vkframes and one with stock OpenFrames, in two scenarios: a large
static trajectory, and a growing one.

```sh
pixi run -e openframes openframes-build    # once
pixi run bench
```

## A small program

```fortran
program orbit_demo
    use vkframes
    implicit none
    real(wp), parameter :: re = 6378.137_wp
    type(Sphere), target :: earth, craft
    type(Trajectory), target :: orbit
    type(FrameManager), target :: scene
    type(WindowProxy), target :: window
    type(View), target :: earth_view
    real(wp) :: t
    integer :: k

    earth = Sphere('Earth', radius=re, color=[0.2_wp, 0.4_wp, 1.0_wp])
    craft = Sphere('Spacecraft', radius=300.0_wp, color=[1.0_wp, 0.3_wp, 0.2_wp])

    ! a circular orbit, sampled over one period (600 s here)
    orbit = Trajectory()
    do k = 0, 360
        t = 600.0_wp * k / 360.0_wp
        call orbit%add(t, pos=1.6_wp * re * [cos(2*pi*t/600), sin(2*pi*t/600), 0.0_wp])
    end do
    call craft%follow(orbit)
    if (.not. earth%add_child(craft)) error stop 'add_child failed'
    scene = FrameManager(earth)

    if (.not. window%open(800, 600, 'vkframes')) error stop 'could not open a window'
    call window%set_scene(scene)
    earth_view = View(earth, earth)
    call earth_view%set_default_view_distance(5.0_wp * re)
    call window%add_view(earth_view)
    do while (window%frame())       ! until the window is closed
    end do
    call window%close()
end program orbit_demo
```

Objects belong to your program: the scene tree, views and windows hold
pointers to them, so declare them `target` and keep them alive while they're
shown.

## Data files

[data/](data/) holds:
- the HYG v3 star catalog in OpenFrames' format;
- 2k Earth day and night textures;
- the trajectory demo's example file.

Data files are found by name on `VKFRAMES_DATA_PATH` (a list of
directories), which the pixi tasks set. Converted fonts are cached in the
user's cache directory, or in `VKFRAMES_CACHE_DIR` if it's set.

## License

[Apache](LICENSE.TXT)