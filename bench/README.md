# vkframes / OpenFrames benchmark

Two qt-fortran apps draw the same scene, one with vkframes and one with
[OpenFrames](https://github.com/ravidavi/OpenFrames) (through
[openframes-fortran](https://github.com/jacobwilliams/openframes-fortran)).
Both run the same scenarios and report the same measurements. This checks the
estimates in [plans/openframes-compare.md](../plans/openframes-compare.md).

The OpenFrames app follows qt-fortran's OpenFrames demo
(`demos/openframes` in [qt-fortran](https://github.com/jacobwilliams/qt-fortran)).

## Running it

From the vkframes root:

```sh
pixi run -e openframes openframes-build    # once: clone and build OpenFrames (a few minutes)
pixi run bench                             # both scenarios at every size, both apps (about 6 minutes)
```

`pixi run bench` builds both apps and runs each scenario at each size with
each app, one process per run. It then prints a table per scenario. Every run
appends one JSON line to `bench/results/results-<date>.jsonl`, which is not in
git.

`bench/run.py` takes these options:

| Option | Default | Meaning |
|---|---|---|
| `--points N ...` | `100000 1000000 5000000` | Trajectory sizes |
| `--scenarios S ...` | both | `static` or `grow` |
| `--libraries L ...` | both | `vkframes` or `openframes` |
| `--seconds S` | 10 | Measured time per run |
| `--warmup S` | 2 | Time before measuring |
| `--rate R` | 60000 | Points appended per second (`grow`) |
| `--out FILE` | dated file | Where to write the results |
| `--report FILE` | | Print the tables of an earlier results file |
| `--no-build` | | Don't build the apps first |

You can also run one app on its own with the same options:

```sh
pixi run -e qt bench-vkframes -- --scenario grow --points 1000000 [--per-point] [--vsync] [--capture vk.png]
pixi run -e openframes bench-openframes -- --scenario grow --points 1000000 [--vsync] [--capture of]
```

Three options change how the apps draw:

- **`--vsync`** presents frames at the display's refresh rate instead of as
  fast as possible. Use it to watch the apps (see
  [Flicker in the vkframes app](#flicker-in-the-vkframes-app-macos)); frame
  rates then stop at the display's rate.
- **`--paced`** (vkframes only) keeps vsync off but draws one frame per
  display refresh, from Qt's update requests, as the demos do.
- **`--no-lod`** (vkframes only) turns off the curve's level of detail, so
  every point is drawn, as OpenFrames does. `run.py` runs vkframes' scenarios
  both ways.

## What is measured

### The scene

Both apps draw the same scene:

- an Earth (a plain sphere);
- a trajectory of `--points` points, drawn by a `CurveArtist`:
  - a precessing orbit of 1000 points per revolution (`make_orbit` in
    `common/`);
  - 2 physical pixels wide, with no dash pattern;
- a small sphere that follows the trajectory.

The window is 1280 x 800 logical pixels, with the same camera distance in
both apps. Neither app uses multisampling.

### Scenarios

| Scenario | What happens while measuring | What it shows |
|---|---|---|
| `static` | Time runs and the body moves; the trajectory doesn't change. | The cost of drawing a large trajectory. |
| `grow` | Points are appended from a 60 Hz timer on the GUI thread, at `--rate` points per second. | The cost of new data: Fortran input, conversion and upload. |

### Results of a run

Each run produces one JSON line with these fields:

| Field | Meaning |
|---|---|
| `lod` | vkframes: whether the curve's level of detail was on (OpenFrames: always `off`, every point drawn). |
| `insert_s` | Time to add the first `--points` points from Fortran. The data is computed beforehand and isn't timed. |
| `first_frame_s` | Time from the window starting to its first frame. This includes building the GPU buffers for all the points. |
| `fps`, `frame_ms_*` | Frames finished during the measurement: the frame rate, and the frame times (mean, median, 95th and 99th percentiles, maximum). |
| `append_ms_*` | `grow` only: the time of each append on the GUI thread (mean and maximum). |
| `cpu_per_wall` | CPU time of the whole process (all threads) divided by wall time, while measuring. |
| `rss_mb_load`, `rss_mb_peak` | Peak resident memory of the process: after the first frame, and at the end. |

### Making the comparison fair

- **No vsync, no frame cap.** Frames are rendered as fast as possible, so
  frame times show work done, not waits for the display.
  - vkframes opens its window with `vsync=.false.` (Vulkan present mode
    `IMMEDIATE`). A zero-interval `QTimer` calls `WindowProxy%frame`.
  - OpenFrames gets an OpenGL swap interval of 0
    (`QSurfaceFormat%setSwapInterval`) and a frame limiter set to 10000
    frames per second.
- **Optimized builds.** Both apps are built with fpm's `release` profile,
  and vkframes' C++ layer with `-O2`. OpenFrames is a CMake `Release` build.
  OpenSceneGraph, VSG and Qt are conda-forge's release builds.
- **Data input as each library offers it.**
  - OpenFrames' C interface adds points only one at a time
    (`oftraj_addtime` and `oftraj_addposition` for each point).
  - vkframes adds them in bulk by default, with `Trajectory%add` on whole
    arrays.
  - `run.py` also runs vkframes' `static` scenario with `--per-point`, so
    one-at-a-time insertion can be compared directly.
- **Level of detail both ways.** vkframes' curve has a level of detail
  (fewer points seen from far away, within half a pixel). `run.py` runs
  vkframes with it on (the default) and off (`--no-lod`, every point drawn,
  as OpenFrames does).
- **Stock OpenFrames.** OpenFrames is
  [ravidavi/OpenFrames](https://github.com/ravidavi/OpenFrames) at commit
  `798dc4a` (`master`, March 2026). This is the version compared in
  `plans/openframes-compare.md`, and the one qt-fortran's OpenFrames demo
  uses.
- **No trace mode scenario.** Stock OpenFrames has no trace mode (drawing a
  trajectory only up to the current time), so there is nothing to compare
  vkframes' trace mode with.

### Reading the results

- **Threads.** OpenFrames renders on its own thread, and its frame times are
  taken there, in the `swap_buffers` callback. vkframes renders on the GUI
  thread, so its frame times include its Fortran update and, in `grow`, the
  appends.
- **CPU and GPU.** `cpu_per_wall` shows whether a library is limited by the
  CPU (near 1 or above) or by the GPU (low). OpenFrames' render thread busy
  waits between frames, so it stays near 1 or above.
- **Memory** includes everything in the process: Qt, the graphics driver,
  and VSG or OpenSceneGraph. Compare the change between sizes rather than the
  absolute numbers.
- **macOS.** Apple's OpenGL is deprecated, and frozen at version 4.1 running
  over Metal. It reports
  `Fallback to SW vertex for line stipple` for OpenFrames' line shader, so on
  macOS some of OpenFrames' vertex work runs on the CPU. That is a fair
  picture of OpenFrames on a Mac today, but not of OpenFrames on Linux or
  Windows, where OpenGL drivers are native.
- **The OpenFrames app's shutdown (macOS).** When the app stops OpenFrames,
  OpenFrames' render thread closes its OpenGL context, and that can make Qt
  update the context on that thread, which AppKit aborts
  (`-[NSOpenGLContext update] must be called from the main thread`). The
  app refuses `make_current` once it is stopping, so the render thread makes
  no more OpenGL calls. qt-fortran's OpenFrames demo stops OpenFrames the
  same way.
- **GPU line drawing differs.** vkframes draws each line segment as a
  screen-space quad (6 vertices). OpenFrames draws a `GL_LINE_STRIP` (one
  vertex per point). See `plans/openframes-compare.md`.

### Flicker in the vkframes app (macOS)

**The problem.** Without vsync, the vkframes window flickers, and some frames
show all black. The OpenFrames window doesn't. With `--vsync` there is no
flicker. The frames are still rendered completely, so the measurements are
unaffected; only what reaches the screen is.

**What was found** (2026-10-27):

| Window | Without vsync | With vsync |
|---|---|---|
| In Qt, timer loop (the default) | flickers | fine |
| In Qt, `--paced` (one frame per display refresh) | flickers | — |
| vkframes' own window (VSG's, no Qt) | fine | fine |

- **Not the driver's present ordering.** Mesa's Metal code
  (`src/vulkan/wsi/wsi_common_metal*` and `src/kosmickrisp/vulkan/kk_queue.c`)
  makes the queue wait for the drawable, copies the frame into it, signals
  it, and only then calls `[drawable present]`. This is Metal 4's intended
  pattern.
- **Not presenting faster than the display, and not Qt being starved of
  events:** `--paced` flickers too, at 98 fps on a 120 Hz display.
- **Not the Metal layer's settings.** The layer is the same in both windows
  (opaque, 3 drawables, the same pixel format); without vsync, KosmicKrisp
  turns its `displaySyncEnabled` off in both.
- **It is where the layer sits.**
  - In vkframes' own window, the Metal layer is the backing layer of VSG's
    view.
  - In Qt, it is nested inside two of Qt's `QContainerLayer`s, as the top
    sublayer above another of Qt's layers.
  - Core Animation composes an unsynchronized Metal layer (display sync
    off) badly inside Qt's layer tree. The black frames look like moments
    when the Metal layer shows no image and Qt's layer underneath shows
    through.

**What it means:**

- Only `WindowProxy%open(..., vsync=.false.)` in a window embedded in a host
  is affected, on macOS. Only this benchmark does that. vkframes' default is
  vsync, which is fine everywhere.
- The benchmark's numbers are still valid.
- If an embedded window without vsync is ever needed, there are things to
  try, watching the window each time:
  - the layer's `presentsWithTransaction`;
  - presenting into a layer outside Qt's layer tree (a separate child
    window).

## Results

These results come from macOS 27 on an Apple M5, on 2026-10-29, with stock
OpenFrames and the defaults (10 s measured, 2 s warmup, 60,000 points per
second in `grow`), on mains power. They include:
- the improvements of 2026-10-27: mitre joins for lines, growth headroom,
  chunks capped at 65,536 points, level of detail for curves;
- the plain line shaders of 2026-10-29 (see
  [The line shaders' variants](#the-line-shaders-variants)).

(See `plans/openframes-compare.md`.)

vkframes was run with its curve's level of detail on (the default), and off
(`--no-lod`: every point drawn, as OpenFrames does). OpenFrames has no level
of detail.

- **Noise.** Settings drawing every point repeat within a few percent
  between runs. The fastest settings (hundreds of frames per second and up)
  vary more, so treat those as approximate. The numbers below are from one
  run.
- **The display's rate.** Inside a Qt window on macOS, vkframes' frames are
  sometimes held to the display's rate (120 Hz here) even without vsync.
  Core Animation seems to throttle a Metal layer inside Qt's layer tree
  while the window is visible. Results at 120 fps would be that limit, not
  vkframes'. None of this run's were.

The full tables come from `pixi run bench -- --report <file>`.

### Frame rate

Frames per second:

| Scenario | Points | vkframes | vkframes, every point | OpenFrames | vkframes / OpenFrames |
|---|---|---|---|---|---|
| static | 100,000 | 1,255 | 1,230 | 360–558 | 2–3× |
| static | 1,000,000 | 901 | 244 | 57 | 16× (every point: 4.3×) |
| static | 5,000,000 | 236 | 52 | 11 | 21× (every point: 4.7×) |
| grow | 100,000 | 1,336 | 566 | 147–175 | 8× |
| grow | 1,000,000 | 548 | 194 | 41 | 13× (every point: 4.7×) |
| grow | 5,000,000 | 226 | 49 | 9 | 25× (every point: 5.4×) |

Earlier vkframes figures, at 1 and 5 million points:
- before the improvements of 2026-10-27: 98 and 19.4 fps (`static`), 77
  and 17.9 fps (`grow`);
- on 2026-10-27, drawing every point: 199 and 42 fps (`static`), 158 and 40
  fps (`grow`);
- earlier on 2026-10-29, before the plain shaders: 108 and 22 fps (`static`),
  85 and 21 fps (`grow`). See the next section.

### The line shaders' variants

A rerun on 2026-10-29 found vkframes drawing every point at about half its
2026-10-27 rate. OpenFrames was unchanged, but it is limited by the CPU and
vkframes by the GPU, so that showed nothing about the machine.
- **Ruled out:** mains or battery power, the display, and the driver (Mesa
  26.2.5 and 26.2.6 gave the same rate).
- **The cause:** `git bisect` and builds that mixed the line shaders of
  different commits put the whole loss in the line shaders.
- **Why:** each feature had added to the shaders: highlight arrows (198 to
  135 fps at 1 million points, every point drawn), fading trails (to 119)
  and the highlight's second pass (to 108). Every line paid for all of them,
  even when it used none: 22 floats passed per vertex instead of 4, and a
  larger, register-hungry vertex shader. A line draws 6 vertices per point,
  so with every point drawn that work dominates a frame on Apple's GPU.

The line shaders now come in two variants (`VKF_LINE_PLAIN`, compiled by
`tools/fpm_vsg.py`):
- **Plain:** for a line with no dashes, highlight or trail. It passes only
  the color to the fragment shader, which discards nothing.
- **Full:** for the rest.

Each line has both pipelines and draws with the one its style needs, each
frame (`EyeRelative`). In this benchmark every line is plain: 244 fps at 1
million points, against 108 before and 198 on 2026-10-27.

### Other measurements

| Measurement | vkframes | OpenFrames |
|---|---|---|
| Adding 1,000,000 points | 7 ms in bulk, 14 ms one at a time | 14 ms, one at a time |
| Adding 5,000,000 points | 30 ms in bulk, 80 ms one at a time | 78 ms, one at a time |
| Time to first frame | 0.17–0.27 s | about 0.03 s at every size |
| Slowest append, `grow` (1,000 points each) | 1.7–5.2 ms at 100,000 points; 4.5 ms at 1M; 0.05 ms at 5M | 1.5 ms; 3.2 ms; 0.07 ms |
| CPU / wall, large trajectories | 0.02–0.15 (limited by the GPU) | 1.3–1.4 (render thread always busy) |
| Memory with 5,000,000 points (`static`, bulk) | about 900 MB | about 1,000 MB |
| Memory with 100,000 points | about 285 MB | about 245 MB |

### What this shows

- **Drawing large trajectories:** with level of detail, vkframes draws 13 to
  25 times as many frames as OpenFrames at 1 and 5 million points. Drawing
  every point, it's still 4.3 to 5.4 times as fast.
  - vkframes is limited by the GPU, using under 20% CPU.
  - OpenFrames is limited by the CPU. Part of that is because Apple's
    OpenGL runs its line shader's vertex work on the CPU (see
    [Reading the results](#reading-the-results)).
  - On Linux or Windows, with native OpenGL drivers, the gap may be
    smaller.
  - The level of detail draws the curve from fewer points seen from far
    away, within half a pixel of all of them. A 5-million-point capture
    matches drawing every point, except for line edges moved by under half
    a pixel.
- **Adding data from Fortran:**
  - vkframes' bulk add is about twice as fast as OpenFrames' one call per
    value.
  - OpenFrames' per-point C interface costs about 14 ns per point, the
    same as vkframes adding one point at a time.
  - None of these matter next to frame times.
- **Appends:** the slow append after a large bulk add is gone (0.05 ms at 5
  million points, from 20 to 49 ms). A copy still happens when the room
  left runs out, once per quarter of the trajectory's size; the slowest
  appends at 100,000 and 1 million points are those copies.
- **First frame:** vkframes' first frame still takes 0.17 s or more. This is
  probably pipeline creation and the first upload, while OpenFrames uploads
  lazily.
- **Memory:** with 5 million points, vkframes uses less than OpenFrames,
  although its process starts about 100 MB larger (the Vulkan driver and
  VSG).

## Files

| Path | Contents |
|---|---|
| `common/` | Shared Fortran, a package used by both apps: options, data, timing, statistics, results |
| `vkframes/` | The vkframes app: `app/main.f90`, and `demo.py` (vkframes' and qt-fortran's build settings) |
| `openframes/` | The OpenFrames app: `app/main.f90`, and `demo.py` (builds OpenFrames, then the app) |
| `run.py` | Runs everything and prints the tables |

## Platforms

The benchmark has been run on macOS (Apple silicon). The `openframes` pixi
environment also lists linux-64, which hasn't been tried. Windows isn't
supported: OpenFrames' build and the app's peak-memory measurement are
untested there.
