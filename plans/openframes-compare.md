# Efficiency compared with OpenFrames

How vkframes and OpenFrames move trajectory data from a Fortran program to
the GPU, and where each is more efficient. This was worked out on 2026-10-26
by reading both code bases (OpenFrames at commit `4fba41c`, July 2026). No
benchmarks were run, so the numbers below are estimates from the code.

Items marked **[ ] Future work** are improvements for vkframes. The same
items are listed together under [Future work](#future-work) at the end.

## Summary

- vkframes is as efficient as OpenFrames, or more, at every step from
  Fortran to the GPU, except one: small trajectories that grow every frame
  upload more data per second.
- The biggest gains are in trace mode and in how data gets in from Fortran.
- There are a few small inefficiencies on the Fortran side, none of them
  serious.

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

Trace mode, where the line is drawn only up to the current time, shows the
biggest difference.

**OpenFrames:**

- While time runs, it updates at most every 50 ms. Each update rewrites the
  interpolated end point and marks the whole vertex array dirty, so the
  whole trajectory is uploaded again on every update.
- When data is added, `dataAdded()` resets `_lastTracePointIdx` to 0. On a
  growing trajectory, finding the current point then becomes a linear scan
  from the start.

**vkframes:**

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

- [ ] **Future work:** fill each component of a whole range at once from an
  array slice (time, a column of positions and optionals, or attitudes),
  times its scale. Use this for curves, segments and markers.

### Syncing frames every frame

Every frame, `WindowProxy%frame` calls `root%update(t)` and `root%sync()`.

- Every reference frame converts its quaternion to a matrix and makes one C
  call (`vkf_transform_set_matrix`), even if it hasn't moved.
- With several windows, this happens once per window per frame.
- The cost grows with the number of frames times the number of windows.
  This is negligible for tens of frames, but it is wasted work.

- [ ] **Future work:** skip frames whose position and attitude haven't
  changed. Update the scene once for each new simulation time, not once
  per window. Windows whose clocks are synchronized share that time.

### Time search in followers

A `TrajectoryFollower` searches for the time twice per frame, once in
`interpolate_position` and once in `interpolate_attitude`. OpenFrames
searches once. Each search is a small cost (O(log n)).

- [ ] **Future work:** search once and use the index for both.

### Distances along the line

`LineChain%measure` rebuilds double values from the (high, low) float pairs
it has just written, and allocates temporary arrays (`x(3, n)`, `s(n)`) on
each call. The running sum is sequential, so that part can't vectorize.

- [ ] **Future work:** measure from the trajectory's double positions
  directly (passed in by the artist), without the temporary arrays. This is
  also a little more accurate.

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

- [ ] **Future work:** cap the first chunk at `max_chunk` points, or use
  partial uploads (see
  [Upload rate](#upload-rate-for-small-trajectories-that-grow)).

## Not about Fortran, but relevant

- **GPU work for lines.** vkframes draws each segment as a quad: 6 vertices,
  each reading both endpoints. OpenFrames draws `GL_LINE_STRIP` with
  `glLineWidth`, one vertex per point. vkframes does more vertex work, but
  this is how wide lines are drawn on Vulkan and on macOS, where OpenGL's
  line width isn't available.
  - [ ] **Future work:** use instancing, or an indexed layout without
    vertex data, so that each endpoint is transformed once. This is also in
    `plans/plan.md`.
- **Depth.** OpenFrames' `DepthPartitioner` renders the scene once for each
  depth slice. vkframes renders once, with reverse-Z depth.
- **Long trajectories seen from far away.** Neither library thins out
  points for distant views.
  - [ ] **Future work:** decimation or level of detail for very long
    trajectories. This is also in `plans/plan.md`.

## Future work

In rough order of benefit:

- [ ] Upload only the range of points written, or limit uploads to about 20
      per second ([upload rate](#upload-rate-for-small-trajectories-that-grow)).
- [ ] Fill data sources other than x, y, z a range at a time
      ([data sources](#data-sources-other-than-x-y-z)).
- [ ] Cap the first chunk at `max_chunk` points
      ([first chunk](#a-first-chunk-the-size-of-the-trajectory)).
- [ ] Skip unchanged frames when syncing, and update once per simulation
      time across windows ([syncing](#syncing-frames-every-frame)).
- [ ] Measure line distances from the double positions
      ([distances](#distances-along-the-line)).
- [ ] Search for the time once per follower update
      ([followers](#time-search-in-followers)).
- [ ] Use instancing or an indexed layout for line quads
      ([GPU work](#not-about-fortran-but-relevant)).
- [ ] Decimation or level of detail for long trajectories
      ([GPU work](#not-about-fortran-but-relevant)).
- [ ] Store `dof` components per point (low priority;
      [2-D storage](#storage-for-2-d-trajectories)).
- [ ] Benchmarks to check these estimates: adding a million points one at a
      time and in bulk, appending every frame, and trace mode. Time the
      Fortran update and the bytes uploaded, in vkframes and, with
      OpenFrames built, in OpenFrames.
