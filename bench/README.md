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
pixi run bench                             # all scenarios and sizes, both apps (about 10 minutes)
```

`pixi run bench` builds both apps and runs each scenario at each size with
each app, one process per run. It then prints a table per scenario. Every run
appends one JSON line to `bench/results/results-<date>.jsonl`, which is not in
git.

`bench/run.py` takes these options:

| Option | Default | Meaning |
|---|---|---|
| `--points N ...` | `100000 1000000 5000000` | Trajectory sizes |
| `--scenarios S ...` | all | `static`, `grow` or `trace` |
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

`--vsync` presents frames at the display's refresh rate instead of as fast
as possible. Use it to watch the apps (see
[Flicker in the vkframes app](#flicker-in-the-vkframes-app-macos)); frame
rates then stop at the display's rate.

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
| `trace` | Trace mode is on: the curve is drawn only up to the current time, and time sweeps over the whole trajectory. | The per-frame cost of trace mode. |

#### Flicker in the vkframes app (macOS)

**The problem.** Without `--vsync`, the vkframes window flickers, and some
frames show all black. The OpenFrames window doesn't.

**The likely cause** is in the driver, not in vkframes:

- Without vsync, vkframes asks Vulkan for present mode `IMMEDIATE`.
- On macOS that goes through KosmicKrisp's Metal presentation code (Mesa's
  `src/vulkan/wsi/wsi_common_metal*.{c,m}`):
  - For `IMMEDIATE` it turns off `displaySyncEnabled`.
  - It presents with `[drawable present]` as soon as `vkQueuePresentKHR` is
    called, instead of scheduling the present on the Metal command buffer
    that draws the frame (`presentDrawable:`).
- So Core Animation can show a drawable before the GPU has finished drawing
  into it.
- OpenFrames goes through Apple's own OpenGL, which orders this correctly.

**What it means for the results.** Every frame is still fully rendered:
vkframes waits for each frame's GPU work before reusing its resources. The
frame times are the work done; only what appears on screen is affected. With
`--vsync` (Vulkan present mode `FIFO`, vkframes' default) the window doesn't
flicker.

**If it is confirmed**, it is worth reporting to Mesa. vkframes could also
avoid `IMMEDIATE` on KosmicKrisp, for example by waiting for each frame's GPU
work before presenting.

## Results

Each run produces one JSON line with these fields:

| Field | Meaning |
|---|---|
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
- **Same OpenFrames as the analysis.** OpenFrames is commit `4fba41c` of
  Jacob Williams' fork (branch `trace`), which adds trace mode. This is the
  version compared in `plans/openframes-compare.md`. Trace mode isn't in
  openframes-fortran yet, so the app declares `oftrajartist_settracemode`
  itself.

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
- **GPU line drawing differs.** vkframes draws each line segment as a
  screen-space quad (6 vertices). OpenFrames draws a `GL_LINE_STRIP` (one
  vertex per point). See `plans/openframes-compare.md`.

## Results

These results come from macOS 27 on an Apple M5, on 2026-10-26, with the
defaults (10 s measured, 2 s warmup, 60,000 points per second in `grow`).
Each number is from a single run. At 100,000 points the two vkframes `static`
runs differ by about 25% although they draw the same scene, so treat
differences of that size at small sizes as noise. The full tables come from
`pixi run bench -- --report <file>`.

### Frame rate

Frames per second, with the mean frame time in brackets:

| Scenario | Points | vkframes | OpenFrames | vkframes / OpenFrames |
|---|---|---|---|---|
| static | 100,000 | 612 (1.6 ms) | 426 (2.4 ms) | 1.4× |
| static | 1,000,000 | 99 (10.1 ms) | 55 (18.1 ms) | 1.8× |
| static | 5,000,000 | 20.3 (49 ms) | 11.1 (90 ms) | 1.8× |
| grow | 100,000 | 289 (3.5 ms) | 156 (6.4 ms) | 1.9× |
| grow | 1,000,000 | 77 (13.0 ms) | 40 (25.0 ms) | 1.9× |
| grow | 5,000,000 | 18.9 (53 ms) | 8.9 (112 ms) | 2.1× |
| trace | 100,000 | 609 (1.6 ms) | 978 (1.0 ms) | 0.6× |
| trace | 1,000,000 | 275 (3.6 ms) | 220 (4.5 ms) | 1.2× |
| trace | 5,000,000 | 85 (11.8 ms) | 64 (15.7 ms) | 1.3× |

### Other measurements

| Measurement | vkframes | OpenFrames |
|---|---|---|
| Adding 1,000,000 points | 6 ms in bulk, 14 ms one at a time | 14 ms, one at a time |
| Adding 5,000,000 points | 31 ms in bulk, 84 ms one at a time | 75 ms, one at a time |
| Time to first frame | 0.15–0.29 s | about 0.03 s at every size |
| Slowest append, `grow` (1,000 points each) | 1.7 ms at 100,000 points; 4.8 ms at 1M; **20 ms at 5M** | 1.7 ms; 2.5 ms; 0.06 ms |
| 95th-percentile frame, `trace` | 2.8 ms; 8.9 ms; 39 ms | 3.0 ms; 15.8 ms; 77 ms |
| CPU / wall, large trajectories | 0.01–0.05 (limited by the GPU) | 1.2–1.4 (render thread always busy) |
| Memory per point (100,000 to 5,000,000 points, `static`) | about 176 B | about 167 B |
| Memory with 100,000 points | about 280 MB | about 175 MB |

### What this shows

- **Drawing and growing large trajectories:** vkframes draws 1.8 to 2.1
  times as many frames.
  - vkframes is limited by the GPU, using under 5% CPU.
  - OpenFrames is limited by the CPU. Part of that is because Apple's
    OpenGL runs its line shader's vertex work on the CPU (see
    [Reading the results](#reading-the-results)).
  - On Linux or Windows, with native OpenGL drivers, the gap may be
    smaller.
- **Trace mode:**
  - On large trajectories, vkframes is faster and steadier: its
    95th-percentile frame time is about half of OpenFrames'.
  - OpenFrames uploads the whole trajectory again 20 times a second, so it
    has periodic slow frames.
  - On a small trajectory, OpenFrames' typical frame is faster: 0.6 ms
    against 1.6 ms. vkframes seems to have a fixed cost of about 1.5 ms per
    frame that OpenFrames doesn't have. That is worth investigating.
- **Adding data from Fortran:**
  - vkframes' bulk add is about 2.3 times as fast as OpenFrames' one call
    per value.
  - OpenFrames' per-point C interface costs about 14 ns per point, the
    same as vkframes adding one point at a time.
  - None of these matter next to frame times. The comparison document had
    overestimated the cost of OpenFrames' per-point calls.
- **Regressions found:**
  - vkframes' slowest append at 5,000,000 points is 20 ms. A bulk add sizes
    the trajectory's arrays exactly, so the first append afterwards doubles
    and copies all of them. OpenFrames' vectors, grown one point at a time,
    already have spare room.
  - vkframes' first frame is 0.15 s or more. This is probably pipeline
    creation and the first upload, while OpenFrames uploads lazily.
- **Memory:** per point, the two are about the same. vkframes' process
  starts about 100 MB larger (the Vulkan driver and VSG).

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
