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

`--vsync` presents frames at the display's refresh rate instead of as fast
as possible. `--paced` (vkframes only) keeps vsync off but draws one frame per
display refresh, from Qt's update requests, as the demos do. Use it to watch the apps (see
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

#### Flicker in the vkframes app (macOS)

**The problem.** Without `--vsync`, the vkframes window flickers, and some
frames show all black. The OpenFrames window doesn't. The frames are still
rendered completely, so the measurements are unaffected; only what reaches
the screen is.

**The cause isn't known yet.** It isn't KosmicKrisp presenting a drawable
before the GPU has finished it, which was the first guess. Mesa's Metal code
(`src/vulkan/wsi/wsi_common_metal*` and `src/kosmickrisp/vulkan/kk_queue.c`)
orders the present correctly:

- the queue waits for the drawable;
- it copies the frame into the drawable, then signals the drawable;
- only then is `[drawable present]` called.

This is Metal 4's intended pattern. What's left:

1. **Presenting much faster than the display, with display sync off.** This
   can tear and drop frames.
2. **Qt's own view showing through.** vkframes renders into a view of its own
   inside Qt's window view. In the zero-interval timer loop, Qt rarely gets
   to run, and may repaint its black view over vkframes'.
3. **Something in vkframes' new no-vsync path**, which only the benchmark
   uses.

**To narrow it down**, run each of these and note which ones flicker:

```sh
pixi run -e qt bench-vkframes -- --points 1000000 --seconds 8            # timer loop, no vsync
pixi run -e qt bench-vkframes -- --points 1000000 --seconds 8 --paced    # display-paced, no vsync
pixi run -e qt bench-vkframes -- --points 1000000 --seconds 8 --vsync    # timer loop, vsync
```

`--paced` draws one frame per Qt update request, as the demos do, without
vsync. If `--paced` doesn't flicker, the timer loop starving Qt is the cause
(2). If it does, the cause is presenting without vsync (1 or 3).

## Results

These results come from macOS 27 on an Apple M5, on 2026-10-27, with stock
OpenFrames and the defaults (10 s measured, 2 s warmup, 60,000 points per
second in `grow`). Each number is from a single run. At 100,000 points the
two vkframes `static` runs differ by about 50% although they draw the same
scene, so treat differences of that size at small sizes as noise. The full
tables come from `pixi run bench -- --report <file>`.

### Frame rate

Frames per second, with the mean frame time in brackets:

| Scenario | Points | vkframes | OpenFrames | vkframes / OpenFrames |
|---|---|---|---|---|
| static | 100,000 | 565 (1.8 ms) | 443 (2.3 ms) | 1.3× |
| static | 1,000,000 | 98 (10.2 ms) | 56 (17.9 ms) | 1.8× |
| static | 5,000,000 | 19.4 (51 ms) | 11.2 (89 ms) | 1.7× |
| grow | 100,000 | 287 (3.5 ms) | 157 (6.4 ms) | 1.8× |
| grow | 1,000,000 | 77 (12.9 ms) | 40.5 (24.7 ms) | 1.9× |
| grow | 5,000,000 | 17.9 (56 ms) | 9.1 (110 ms) | 2.0× |

### Other measurements

| Measurement | vkframes | OpenFrames |
|---|---|---|
| Adding 1,000,000 points | 7 ms in bulk, 14 ms one at a time | 13 ms, one at a time |
| Adding 5,000,000 points | 31 ms in bulk, 84 ms one at a time | 77 ms, one at a time |
| Time to first frame | 0.15–0.28 s | about 0.03 s at every size |
| Slowest append, `grow` (1,000 points each) | 1.6 ms at 100,000 points; 4.9 ms at 1M; **49 ms at 5M** | 1.5 ms; 2.7 ms; 0.07 ms |
| CPU / wall, large trajectories | 0.01–0.05 (limited by the GPU) | 1.3–1.4 (render thread always busy) |
| Memory per point (100,000 to 5,000,000 points, `static`) | about 177 B | about 168 B |
| Memory with 100,000 points | about 270 MB | about 175 MB |

### What this shows

- **Drawing and growing large trajectories:** vkframes draws 1.7 to 2.0
  times as many frames.
  - vkframes is limited by the GPU, using under 5% CPU.
  - OpenFrames is limited by the CPU. Part of that is because Apple's
    OpenGL runs its line shader's vertex work on the CPU (see
    [Reading the results](#reading-the-results)).
  - On Linux or Windows, with native OpenGL drivers, the gap may be
    smaller.
- **Adding data from Fortran:**
  - vkframes' bulk add is about twice as fast as OpenFrames' one call per
    value.
  - OpenFrames' per-point C interface costs about 13 ns per point, the
    same as vkframes adding one point at a time.
  - None of these matter next to frame times.
- **Regressions found:**
  - vkframes' slowest append at 5,000,000 points is 20 to 49 ms. A bulk add
    sizes the trajectory's arrays exactly, so the first append afterwards
    doubles and copies all of them. OpenFrames' vectors, grown one point at
    a time, already have spare room.
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
