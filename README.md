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
    it's shown. Level of detail keeps its outline smooth however close the
    view, with finer meshes only when needed.
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
  - Level of detail: seen from far away, a curve is drawn from fewer
    points, staying within half a pixel of all of them, chosen per view.
- **Custom line segments**: `CustomLineSegments` (as OpenFrames') draws
  segments with a color at each end, set by the program or computed each
  frame by a callback: for example, lines of sight between spacecraft.
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
  - Several windows can share one scene and one Vulkan device; a scene they
    share is updated once per simulation time.
  - Embedded windows handle resizing smoothly, Retina displays, and moving
    between screens of different scales.
  - Vsync by default; `vsync=.false.` renders as fast as possible (for
    measuring).
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

Not done yet: shadows, sensor visibility and video recording.
[plans/plan.md](plans/plan.md) has the full feature map and what is still
open.

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

The examples in [example/](example/) are short programs, each in a window
of its own (no GUI toolkit). Each shows one part of the API:

| Example | What it shows |
|---|---|
| `hello_sphere` | The smallest program: a window with a sphere, the trackball, and the frame loop driven from Fortran. |
| `orbit` | An inclined elliptical orbit drawn as a thick line (`CurveArtist`), a dashed equator, and a spacecraft moved along the orbit by Fortran each frame (Kepler's equation). |
| `earth` | A textured WGS84 Earth and a 3D model following an orbit (`Model`, `Trajectory`, `TrajectoryFollower`), with three views: of the Earth, of the spacecraft, and from the spacecraft toward the Earth. Needs a texture and a model file. |
| `day_night` | Lighting: the Sun as a light source an astronomical unit away, day and night textures (city lights on the night side), and a specular glint from the sphere's material. |
| `shapes` | The reference shapes: coordinate axes with tick marks, a latitude/longitude grid, an equatorial plane and sensor cones pointed at the Earth. |
| `two_windows` | One scene in two windows, each with its own view, their clocks synchronized so both show the same time. |
| `live_changes` | Changing a scene while it's shown: a Moon added and removed, the Earth's axes moved, HUD text and a starry sky added. |
| `precision` | Double precision: a 10 m spacecraft on a 7000 km orbit line, seen from 30 m away; the line passes through the spacecraft's center (in plain float it would be off by about a meter). |
| `line_of_sight` | `CustomLineSegments`: five spacecraft in different orbits (LEO, sun-synchronous, GPS, GEO, Molniya), shown as markers, with a line of sight between every pair, recomputed each frame by a callback: clear links in the spacecraft's colors, links the Earth blocks dashed. |
| `rocky_map` | Terrain with Rocky (`TerrainBody`; needs the `rocky` environment): the Earth from ReadyMap's tiles, or the Moon from NASA's Moon Trek and LOLA's elevation, from orbit and close to the surface. |

```sh
pixi run example hello_sphere
pixi run example orbit
pixi run example earth -- texture.jpg model.3ds     # e.g. OpenFrames' Demos/Images and Demos/Models/Hubble.3ds
pixi run example day_night
pixi run example shapes
pixi run example two_windows
pixi run example live_changes
pixi run example precision
pixi run example line_of_sight
pixi run -e rocky example rocky_map -- moon         # or: earth (after the Rocky build, see the lunar demo)
```

Most examples take an optional frame count and an image file to capture,
for example `pixi run example orbit -- 300 orbit.png`; without them they
run until the window is closed. The comment at the top of each file lists
its arguments.

### Demos

Each demo in [demos/](demos/) is a fuller application with a GUI, and a
separate fpm project that depends on vkframes (the library itself doesn't
depend on Qt or Python). They run vkframes embedded in a host window: the
GUI toolkit owns the window and forwards its events, and vkframes renders
into it.

#### Qt demo

[demos/qt](demos/qt): a spacecraft orbiting the Earth in a
[qt-fortran](https://github.com/jacobwilliams/qt-fortran) main window, next
to a dock of controls. It's the most complete demo, and it shows most of the
library:

- **Scene:** the WGS84 Earth with day and night textures, rotating; a
  spacecraft (OpenFrames' Hubble model, or a sphere) and a second spacecraft
  on part of another orbit; the Sun as a light source; a starry sky.
- **Trajectories:** the orbits drawn whole or traced up to the spacecraft,
  in a dash pattern; markers along them; radius vectors (a `SegmentArtist`)
  added and removed while running.
- **The spacecraft as markers:** shown as their bodies, or as a marker of
  any shape at their current positions.
- **Line of sight:** between the two spacecraft (`CustomLineSegments`),
  green while clear of the Earth, red when it's blocked.
- **Reference shapes:** a latitude/longitude grid, coordinate axes with tick
  marks, the equatorial plane, a sensor cone.
- **Views and time:** six views (of the Earth, of each spacecraft, and from
  each spacecraft toward the Earth or the other); a time scale, pause and a
  time slider.
- **Windows:** up to four, each with its own view, their clocks synchronized
  to the main window's; resizing, Retina displays and moving between screens
  of different scales are handled.

```sh
pixi run -e qt qt-demo
pixi run -e qt qt-demo -- --windows 2 --shapes --los --position-marker 6
```

Options include `--view n`, `--windows n`, `--trace`, `--radius`,
`--marker n`, `--position-marker n`, `--sky n`, `--shapes`, `--los`,
`--pattern n`, `--capture file` and `--quit-after seconds`; the comment at
the top of [demos/qt/app/main.f90](demos/qt/app/main.f90) explains them. To
use OpenFrames' Hubble model and star map, point `OPENFRAMES_DATA` (or
`--data`) at OpenFrames' `Demos` folder.

#### Trajectory file demo

[demos/trajectory](demos/trajectory): trajectory data from a file, with a
style editor. It reads segments (times and positions) from a JSON file with
[json-fortran](https://github.com/jacobwilliams/json-fortran), and draws
each as a trajectory: a curve, markers, and a body following it, around the
central body named in the file.

- A dock editor shows each segment's style: line color, width and dash
  pattern; markers (start, end, at an interval or every n points, shape,
  size, colors); the body (radius, color); visibility, trace and label.
- Every change is drawn at once.
- File > Save / Save As write the file back with the new styles (the data
  and any other keys are kept).

```sh
pixi run -e qt trajectory-demo                      # data/trajectory.json
pixi run -e qt trajectory-demo -- my_segments.json
```

The file format and every style option are documented at the top of
[demos/trajectory/app/main.f90](demos/trajectory/app/main.f90).

#### PySide6 demo

[demos/pyside6](demos/pyside6): the same kind of scene in a Python GUI
(PySide6). It shows how to use vkframes from Python:

- The scene and its windows are Fortran, built as a shared library with a
  small C interface of the demo's own.
- The Python GUI loads that library with ctypes, owns the Qt windows, and
  forwards their events.
- One or more windows can be opened, with a choice of view and sky.

```sh
pixi run pyside6-demo
pixi run pyside6-demo -- --windows 2 --trace
```

Options (`--view n`, `--windows n`, `--sky n`, `--trace`, `--capture file`,
`--quit-after seconds`) are listed at the top of
[demos/pyside6/app.py](demos/pyside6/app.py).

#### Lunar demo

[demos/moon](demos/moon): terrain with Rocky, in a qt-fortran GUI:

- **Terrain:** the Moon's surface from LRO imagery (NASA's Moon Trek, over
  the network) and LOLA's elevation, lit by the Sun, under a starry sky.
- **Spacecraft:** one in a 100 km polar orbit over Tycho crater; and a
  lander in the same orbit that descends and lands on Tycho's floor, at the
  terrain's height there.
- **Views:** seven, including Tycho close to the surface, and the lander
  seen from the landing site as it comes down.
- **Dock:** the time, the Sun's longitude (low Sun, long shadows), the
  terrain's lighting, and the curves.

It needs the `rocky` environment and a one-time Rocky build:

```sh
pixi run -e rocky rocky-build     # once: builds Rocky from source into the environment
pixi run -e rocky moon-data       # once: downloads LOLA's elevation (33 MB) into data/
pixi run -e rocky moon-demo
pixi run -e rocky moon-demo -- --view 6 --sun 60
```

Options (`--view n`, `--sun degrees`, `--time seconds`, `--capture file`,
`--quit-after seconds`) are at the top of
[demos/moon/app/main.f90](demos/moon/app/main.f90).

### Benchmark against OpenFrames

[bench/](bench/README.md) has two qt-fortran apps that draw the same scene,
one with vkframes and one with stock OpenFrames, in two scenarios: a large
static trajectory, and a growing one. On macOS, vkframes draws 10 to 22
times as many frames as OpenFrames at 1 and 5 million points; the results
and how they were measured are in [bench/README.md](bench/README.md), and
the comparison of the two libraries in
[plans/openframes-compare.md](plans/openframes-compare.md).

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
- the trajectory demo's example file;
- the Moon's elevation, once downloaded (`pixi run -e rocky moon-data`).

Data files are found by name on `VKFRAMES_DATA_PATH` (a list of
directories), which the pixi tasks set. Converted fonts are cached in the
user's cache directory, or in `VKFRAMES_CACHE_DIR` if it's set.

## License

[Apache](LICENSE.TXT)