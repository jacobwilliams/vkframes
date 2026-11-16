# Plan: web export, what's left

What exporting a scene to a web page (`window%export_web`,
`window%start_web_recording`) doesn't do yet, written on 2026-11-15. The
three stages that are done (a snapshot, animated by the data, recorded
motion) and the design are in [plan.md](plan.md) under "Export to the web".
In short: the exporter writes the scene's data (frames, poses, geometry,
trajectories) into one self-contained `.html` file, and a small viewer
(`data/web/vkf_viewer.js`) rebuilds it with Three.js.

What the page doesn't draw is left out. A frame type that doesn't write
itself is exported as a plain frame (axes, labels, children), with a
warning once for each (`ReferenceFrame%export_web` in `src/vkf_frames.f90`).

## Suggested order

1. Night textures and twilight on spheres: the most visible gap on
   planets.
2. Dash patterns and highlights.
3. A link option on `export_web`.

Then the rest as needed.

## Frame types whose contents are left out

These types don't override `export_web`, so only their frame is written:

- [ ] `ShadowCones` (`src/vkf_shadow.f90`).
- [ ] The drag handles, `VectorHandle` and `TimeHandle`
      (`src/vkf_handles.f90`). Without picking they would only be drawn,
      not dragged.

## Exported, but with less

- [ ] **TerrainBody:** a plain sphere, with no height and no map tiles.
- [ ] **Models:** `.glb` and `.gltf` are inlined; any other format becomes
      a wireframe box.
- [ ] **CustomLineSegments:** the segments are written as they are at
      export. A callback that changes them over time isn't replayed.
- [ ] **Spheres:** no night texture and no twilight.
- [ ] **SkySphere:** stars, constellation lines and the texture are
      exported, but not the star names.

## Drawing features left out

- [ ] Shadows.
- [ ] Lens flares.
- [ ] Dash patterns on lines.
- [ ] Highlights (selected or hovered objects).
- [ ] Image markers, and the rose and spiral marker shapes. Disk, circle,
      square, plus, x, target and the two pulses are exported.
- [ ] Rounded joins on wide lines: sharp bends can show small gaps.
- [ ] Labels kept from overlapping.

## Not in the page itself

- [ ] Picking and dragging.
- [ ] The ImGui panels (`Gui`). Only HUD text is exported.
- [ ] Several windows in one page: a page holds one window.
- [ ] Video overlays.
- [ ] A link option on `export_web` (e.g. a link back to a site, as added
      by hand to `web/artemis1.html`, which a re-export loses).

## Recording (`start_web_recording`)

- [ ] Trajectories that grow during the recording are exported with their
      final data. Trace mode up to the current time still shows the growth.
- [ ] Settings changed during the recording aren't recorded: colors, shown
      or hidden, and so on.

## Practical limits

- [ ] **Offline pages:** Three.js 0.170.0 is loaded from jsDelivr, so the
      page needs a network connection the first time. An option could
      inline it.
- [ ] **Page size:** everything is inlined as base64 (the Artemis I page is
      6.4 MB). `max_points` is the only thinning.
- [ ] **Browser tests:** pages are checked only by
      `tools/check_web_page.py` and headless Chrome screenshots. No
      Playwright or node tests of the viewer.
- [ ] **Untested devices:** touch pinch and two-finger pan on a real iPhone
      or iPad, and pinch on a real Mac trackpad (Safari's gesture events,
      and ctrl+wheel in Chrome and Firefox), were tested only in headless
      Chrome.

## Small viewer fixes

From a review of the touch and trackpad changes (commits `ec150df3`,
`a339d32d`); both are unlikely:

- [ ] A pinch that starts with the two fingers a pixel or two apart can zoom
      out by 10x or more on its first move (the zoom is the old distance
      over the new). Use a minimum distance, e.g. 10 px.
- [ ] If a touch's pointer capture fails (the error is ignored) and the
      finger lifts outside the canvas, the pointer stays in the viewer's
      list, and the next single touch acts as a pinch. Handle
      `lostpointercapture` as a release.
