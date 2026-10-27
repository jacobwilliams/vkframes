# Efficiency compared with OpenFrames

How vkframes and OpenFrames move trajectory data from a Fortran program to
the GPU, and where each is more efficient. This was worked out on 2026-10-26
by reading both code bases (stock OpenFrames, [ravidavi/OpenFrames](https://github.com/ravidavi/OpenFrames)
at commit `798dc4a`, March 2026). The
numbers in the sections after [Measured](#measured) are estimates from the
code. The benchmark in [bench/](../bench/README.md) measured both libraries
afterwards; its findings are under [Measured](#measured).

Items marked **[ ] Future work** are improvements for vkframes. The same
items are listed together under [Future work](#future-work) at the end.

## Summary

- vkframes is as efficient as OpenFrames, or more, at every step from
  Fortran to the GPU, except one: small trajectories that grow every frame
  upload more data per second.
- From the code, the biggest gain looked like how data gets in from
  Fortran. The benchmark shows Fortran input is faster but too cheap in both
  libraries to matter. The largest measured gain is in drawing large
  trajectories, where vkframes is about twice as fast on macOS.
- Trace mode (drawing only up to the current time) is a vkframes feature;
  stock OpenFrames doesn't have it.
- There are a few small inefficiencies on the Fortran side, none of them
  serious. The benchmark found two more: a slow append after a large bulk
  add, and a slow first frame.

## Measured

The benchmark in [bench/](../bench/README.md) runs two qt-fortran apps, one
for each library. They draw the same scene, with no vsync. The results below
are from macOS 27 on an Apple M5, on 2026-10-27, with stock OpenFrames; the
full tables are in `bench/README.md`.

- **Drawing large trajectories (`static`) and adding points while drawing
  (`grow`):** vkframes first drew 1.7 to 2.0 times as many frames, at 1
  million and 5 million points. After the improvements of 2026-10-27
  (mitre joins, growth headroom, capped chunks), it draws 3.7 to 4.5 times
  as many at 5 million points. At 1 million it is held to the display's
  rate inside Qt's window (see `bench/README.md`).
  - vkframes is limited by the GPU, at under 10% CPU.
  - OpenFrames is limited by the CPU. On macOS that is partly because
    Apple's OpenGL falls back to the CPU for OpenFrames' line shader. The
    gap may be smaller on Linux or Windows.
- **Adding data from Fortran:** the estimate below overstated the gain.
  - vkframes' bulk add takes 7 ms for a million points, about twice as fast
    as OpenFrames' per-point calls, which take 13 ms (about 13 ns per
    point).
  - vkframes adding one point at a time costs the same as OpenFrames.
  - None of these matter next to frame times.
- **Memory:** about the same per point at first (around 170 B, the whole
  process). With chunks capped at 65,536 points, vkframes now uses about
  870 MB at 5 million points against OpenFrames' 1,000 MB, although its
  process starts about 100 MB larger.
- **Two problems the estimates missed:**
  - A slow append after a large bulk add (20 to 49 ms at 5 million points;
    fixed, now 0.4 ms; see [growth after a bulk add](#growth-after-a-bulk-add)).
  - The first frame takes 0.14 s or more, against OpenFrames' 0.03 s (still
    open).

The upload estimates (the bytes uploaded) haven't been measured: the
benchmark measures time, not bytes.

## Where vkframes is more efficient

### Getting data in from Fortran

**OpenFrames.** From Fortran, data goes in through the C interface
(`OF_Interface.cpp`), one value at a time:

- The trajectory is first chosen with `oftraj_activate(name)`, which looks
  up a name in a `std::map<std::string>`. Later calls go through a global
  "current trajectory" pointer.
- Every point takes one C call per kind of data: `oftraj_addtime`,
  `oftraj_addposition`, `oftraj_addattitude`.
- Every add calls `informSubscribers()`, which loops over the trajectory's
  artists. `autoInformSubscribers` is on by default.
- When a `std::vector` is full, the add takes a write lock on a read/write
  mutex.

**vkframes.** Adding data never calls into C:

- `Trajectory%add(t(:), pos=pos(:,:), att=att(:,:))` adds all the points in
  one array assignment for each kind of data. A million points cost one bulk
  copy, not about 3 million calls.
- The arrays grow by doubling (`new_capacity`), so adding points one at a
  time costs about the same per point as `std::vector`.
- To see changes, artists compare a revision number with the trajectory's
  (`data_changed`, O(1)). There are no callbacks and no locks.

### Turning points into GPU data

Both libraries convert only the points that are new into (high, low) float
pairs, which is what allows drawing relative to the eye in double
precision.

| | OpenFrames (`CurveArtist.cpp`) | vkframes (`vkf_artists.f90`, `vkf_line_chain.f90`) |
|---|---|---|
| Buffer growth | `osg::Vec3Array::resize` in batches of 1000 points. Each resize reallocates and copies both arrays, and after 1 s without changes `shrink_to_fit` copies them again. | A chain of GPU buffers ("chunks") that is never reallocated or copied. A new chunk is as large as the chain so far, up to 65,536 points. |
| Writing | One point at a time, through `getPoint()`. | For x, y, z positions, the trajectory's array goes straight into the GPU buffer (`split_points` on a pointer to the positions). Fortran writes into VSG's own array through `c_f_pointer`, with no temporary array. |
| Upload after a change | The whole array: two `Vec3Array`s, 24 B per point. | Only the chunks that were written to. |

Example: a trajectory of a million points that keeps growing. OpenFrames
uploads about 24 MB per update. vkframes uploads at most one chunk, about
2 MB.

### Trace mode

Trace mode draws a trajectory only up to the current time. Stock OpenFrames
doesn't have it, so this is a vkframes feature rather than an efficiency
comparison. vkframes keeps it cheap:

- It finds the current point by binary search (`points_passed`,
  O(log n)).
- It changes only the draw range (`vkf_line_set_range`), which uploads
  nothing.
- The moving end of the line is a separate buffer of 2 points, so trace mode
  uploads 64 B per frame.

### Copies of the data

| | Copies kept |
|---|---|
| OpenFrames from Fortran | Fortran's array, then a C++ `std::vector`, then the float arrays (`Vec3Array`), then the driver's copy |
| vkframes | Fortran's array, then the `Trajectory`, then VSG's array (written in place), then the transfer to the GPU |

The number of copies is the same. vkframes avoids the extra copies that
OpenFrames makes when its buffers grow and shrink.

### Intermediate markers

Both libraries place intermediate markers incrementally: only the new part
of the trajectory is processed. They are equally efficient here.
vkframes finds each time-spaced marker with a binary search.

## Where OpenFrames is more efficient, or vkframes could improve

### Upload rate for small trajectories that grow

VSG uploads a whole chunk when it is marked dirty, not only the range that
changed. vkframes also uploads on every frame in which the data changed,
while OpenFrames works through changes at most 20 times a second.

Example: a 10,000-point trajectory that gains a point every frame at 60 fps.

| | Upload rate |
|---|---|
| vkframes | about 19 MB/s (the chunk, about 320 KB, every frame) |
| OpenFrames | about 5 MB/s (240 KB, 20 times a second) |

Both rates are small at this size. The balance reverses for large
trajectories (see [Turning points into GPU data](#turning-points-into-gpu-data)).

- [ ] **Future work:** upload only the range of points that was written.
  This means a partial buffer copy in the C++ layer. Alternatively, or as
  well, combine updates to at most about 20 per second as OpenFrames does.
  `plans/plan.md` also lists this under the efficiency follow-ups.

### Bytes per point

| | Bytes per point |
|---|---|
| vkframes | 32 B: x, y, z and the distance along the line, each as a (high, low) float pair |
| OpenFrames | 24 B |

The distance gives dash patterns that are measured along the line and stay
correct when zooming. OpenFrames uses OpenGL's screen-space line stipple
instead. The extra 8 bytes are a deliberate trade, not something to fix.

### Data sources other than x, y, z

This case covers time on one axis, scaled values, optional data or
attitudes.

- **vkframes:** `write_curve` builds these one point at a time. It calls
  `Trajectory%point()` for each point, and that function has a `select case`
  for each component, so the loop doesn't vectorize.
- **OpenFrames:** `getPoint` is the same kind of loop, so the two libraries
  are equal here.

- [x] **Done (2026-10-27):** `Trajectory%points(i1, i2, sources, v, stride)`
  fills each component of a whole range at once from an array slice (time,
  a column of positions and optionals, or attitudes), times its scale.
  Curves, segments (both ends, with their stride) and the markers' bounds
  use it.

### Syncing frames every frame

Every frame, `WindowProxy%frame` calls `root%update(t)` and `root%sync()`.

- Every reference frame converts its quaternion to a matrix and makes one C
  call (`vkf_transform_set_matrix`), even if it hasn't moved.
- With several windows, this happens once per window per frame.
- The cost grows with the number of frames times the number of windows.
  This is negligible for tens of frames, but it is wasted work.

- [x] **Done (2026-10-27):** `sync` skips frames whose position and attitude
  haven't changed since they were last sent.
- Not done: updating the scene once per simulation time across windows.
  Windows' frames are rendered at different moments, so their times differ
  even when their clocks are synchronized, and skipping on an equal time
  would also stop artists from seeing new data while time is paused.

### Time search in followers

A `TrajectoryFollower` searches for the time twice per frame, once in
`interpolate_position` and once in `interpolate_attitude`. OpenFrames
searches once. Each search is a small cost (O(log n)).

- [x] **Done (2026-10-27):** `update` searches once and passes the index to
  both.

### Distances along the line

`LineChain%measure` rebuilds double values from the (high, low) float pairs
it has just written, and allocates temporary arrays (`x(3, n)`, `s(n)`) on
each call. The running sum is sequential, so that part can't vectorize.

- [x] **Done (2026-10-27):** `LineChain%written` takes the points in double
  precision (`CurveArtist` passes the trajectory's positions), and
  `measure` adds the distances in one pass without temporary arrays. Other
  data sources still measure from the floats.

### Storage for 2-D trajectories

`Trajectory%posopt` always stores 3 components per point. OpenFrames stores
`dof` components, so 2-D trajectories use a third less memory there. This is
minor.

- [ ] **Future work (low priority):** store `dof` components per point.

### A first chunk the size of the trajectory

When a large trajectory is drawn for the first time, the first chunk holds
all of its points, to upload them in one go.

- Later appends go into new chunks, so they don't touch the first one.
- An edit to the last point already written does mark that chunk dirty
  again. This happens with `set_optional`, for example, because the artist
  rewrites from the last point it has written.
- For a trajectory of 10 million points, that edit uploads 320 MB again.

- [x] **Done (2026-10-27):** every chunk holds at most `max_chunk` points
  (65,536); a large bulk load gets several. A change uploads at most one
  chunk (2 MB) again.

### Growth after a bulk add

Measured by the benchmark: this was not in the estimates.

- A bulk add sizes the trajectory's arrays to exactly what is needed, because
  `new_capacity` takes the larger of what is needed and twice the current
  size.
- The first append after that doubles the arrays and copies all the points.
  At 5 million points this makes one append take 20 to 49 ms.
- OpenFrames' vectors, grown one point at a time, already have spare room.

- [x] **Done (2026-10-27):** growing for an add leaves a quarter more room
  than needed (`new_capacity`); `Trajectory%reserve` stays exact. The
  benchmark's slowest append at 5 million points went from 20 to 49 ms down
  to 0.07 ms: the appends fit in the room left. A copy still happens when
  the room runs out, once per quarter of the trajectory's size. Storage in
  chunks would avoid copies altogether, but `Trajectory%positions` and the
  artists rely on contiguous arrays.

### The first frame

Measured by the benchmark: vkframes' first frame takes 0.15 to 0.28 s,
against OpenFrames' 0.03 s. It probably goes to creating pipelines and
uploading all the points. OpenGL uploads lazily.

- [ ] **Future work:** profile the first frame (pipeline creation and
  compilation, the first upload).

## Not about Fortran, but relevant

- **GPU work for lines.** vkframes draws each segment as a quad: 6 vertices,
  each reading both endpoints. OpenFrames draws `GL_LINE_STRIP` with
  `glLineWidth`, one vertex per point. This is how wide lines are drawn on
  Vulkan and on macOS, where OpenGL's line width isn't available.
  - **Measured (2026-10-27):** the vertex work is only 11 to 16% of a frame.
    With every triangle culled, 1 million points render at 619 fps and 5
    million at 184, against 98 and 19.4 drawn. So transforming each
    endpoint once (instancing or an indexed layout) would save at most that.
    It isn't worth it now.
  - Most of the time went to rasterizing many tiny overlapping triangles.
    Each segment was extended by half the width at both ends (square caps,
    to cover the joins), which drew a dense trajectory about twice. Without
    the caps, the frame rate doubled.
  - [x] **Done (2026-10-27):** mitre joins (`shaders/line.vert`).
    - Consecutive segments of a strip meet on the line bisecting the turn,
      so they neither overlap nor leave a gap.
    - Square caps remain where there is no neighbouring point, where the
      segment is clipped at the eye, and at turns over 120 degrees.
    - The joins also look cleaner: the caps left small notches at turns.
    - The shader reads the number of points written from the line's style
      buffer (set by `vkf_line_points_changed`), to use only real
      neighbours.
    - Static drawing went from 98 to 194 fps at 1 million points, and from
      19.4 to 42 at 5 million.
- **Depth.** OpenFrames' `DepthPartitioner` renders the scene once for each
  depth slice. vkframes renders once, with reverse-Z depth.
- **Long trajectories seen from far away.** Neither library thins out
  points for distant views.
  - [ ] **Future work:** decimation or level of detail for very long
    trajectories. This is also in `plans/plan.md`.

## Future work

In rough order of benefit, after the benchmark.

- [x] Draw lines with less rasterizing: mitre joins instead of square caps
      (about twice as fast; [GPU work](#not-about-fortran-but-relevant)).
- [ ] ~~Use instancing or an indexed layout for line quads~~: measured at
      only 11 to 16% of a frame, so not worth it for now
      ([GPU work](#not-about-fortran-but-relevant)).
- [x] Leave room when growing for a bulk add
      ([growth](#growth-after-a-bulk-add)).
- [ ] Profile the first frame ([first frame](#the-first-frame)).
- [ ] Upload only the range of points written, or limit uploads to about 20
      per second ([upload rate](#upload-rate-for-small-trajectories-that-grow)).
- [x] Fill data sources other than x, y, z a range at a time
      ([data sources](#data-sources-other-than-x-y-z)).
- [x] Cap the first chunk at `max_chunk` points
      ([first chunk](#a-first-chunk-the-size-of-the-trajectory)).
- [x] Skip unchanged frames when syncing
      ([syncing](#syncing-frames-every-frame)). (Not done: updating once per
      simulation time across windows, which wouldn't help; see there.)
- [x] Measure line distances from the double positions
      ([distances](#distances-along-the-line)).
- [x] Search for the time once per follower update
      ([followers](#time-search-in-followers)).
- [ ] Decimation or level of detail for long trajectories
      ([GPU work](#not-about-fortran-but-relevant)).
- [ ] Store `dof` components per point (low priority;
      [2-D storage](#storage-for-2-d-trajectories)).
- [x] Benchmarks to check these estimates: adding points one at a time and
      in bulk, and appending while drawing, in both libraries
      ([bench/](../bench/README.md); see [Measured](#measured)).
- [ ] Measure the bytes uploaded per frame in both libraries, and run the
      benchmark on Linux and Windows, where OpenGL drivers are native.
