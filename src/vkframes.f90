!> The vkframes library, a modern Fortran library for visualizing spacecraft trajectories,
!> rendering with VulkanSceneGraph. `use vkframes` gives the whole public API.
!>
!> This umbrella module only re-exports the public types, procedures and
!> constants of the vkf_* modules (math, trajectories and followers, the
!> clock, reference frames and what extends them, windows and views, HUD text
!> and GUI panels, the sky, shapes, artists, line segments, shadows, terrain,
!> file search); see each module for its documentation.
module vkframes

    use vkf_math, only: wp, pi, quat_identity, quat_from_axis_angle, quat_multiply, quat_conjugate, quat_from_matrix, &
                        quat_normalize, quat_rotate, quat_to_matrix, quat_slerp, &
                        quat_from_vectors, identity_matrix, translation_matrix, frame_matrix, rigid_inverse
    use vkf_trajectory, only: Trajectory, DataSource, position_sources, optional_sources, &
                              SOURCE_ZERO, SOURCE_TIME, SOURCE_POSOPT, SOURCE_ATTITUDE, &
                              TIME_NOT_FOUND, TIME_OUT_OF_RANGE, TIME_AT_BOUND
    use vkf_follower, only: TrajectoryFollower, FOLLOW_LOOP, FOLLOW_LIMIT, FOLLOW_POSITION, FOLLOW_ATTITUDE
    use vkf_clock, only: SimulationClock, wall_time
    use vkf_frames, only: ReferenceFrame, FramePointer, NO_AXES, X_AXIS, Y_AXIS, Z_AXIS, ALL_AXES, &
                          PickHit, PICK_FRAME, PICK_BODY, PICK_CURVE, PICK_SEGMENT, PICK_MARKER, &
                          ALIGN_LEFT_TOP, ALIGN_LEFT_CENTER, ALIGN_LEFT_BOTTOM, &
                          ALIGN_CENTER_TOP, ALIGN_CENTER_CENTER, ALIGN_CENTER_BOTTOM, &
                          ALIGN_RIGHT_TOP, ALIGN_RIGHT_CENTER, ALIGN_RIGHT_BOTTOM, &
                          ALIGN_LEFT_BASE_LINE, ALIGN_CENTER_BASE_LINE, ALIGN_RIGHT_BASE_LINE
    use vkf_sphere, only: Sphere
    use vkf_model, only: Model
    use vkf_window, only: FrameManager, WindowProxy, MOUSE_MOVE, MOUSE_PRESS, MOUSE_RELEASE, &
                          WindowEventHandler, MOUSE_LEFT, MOUSE_MIDDLE, MOUSE_RIGHT, &
                          KEY_SPACE, KEY_BACKSPACE, KEY_TAB, KEY_RETURN, KEY_ESCAPE, KEY_DELETE, KEY_HOME, &
                          KEY_LEFT, KEY_UP, KEY_RIGHT, KEY_DOWN, KEY_PAGE_UP, KEY_PAGE_DOWN, KEY_END, KEY_INSERT, &
                          KEY_F1, MOD_SHIFT, MOD_CAPS_LOCK, MOD_CONTROL, MOD_ALT, MOD_NUM_LOCK, MOD_META
    use vkf_hud, only: HUDText
    use vkf_gui, only: Gui
    use vkf_sky, only: SkySphere, SKY_NONE, SKY_TEXTURE, SKY_STARS
    use vkf_shapes, only: CoordinateAxes, LatLonGrid, RadialPlane, PolyhedralCone, EllipticCone, RectangularCone, &
                          CONE_NONE, CONE_SIDES, CONE_EDGES, CONE_BASE_OUTLINE, CONE_ALL
    use vkf_files, only: find_data_file
    use vkf_artists, only: TrajectoryArtist, CurveArtist, SegmentArtist, MarkerArtist, RibbonArtist, DrawableTrajectory, &
                           MARKER_START, MARKER_INTERMEDIATE, MARKER_END, &
                           INTERMEDIATE_TIME, INTERMEDIATE_DISTANCE, INTERMEDIATE_DATA, &
                           MARKER_DISK, MARKER_CIRCLE, MARKER_SQUARE, MARKER_PLUS, MARKER_X, MARKER_TARGET, &
                           MARKER_ROSE, MARKER_CIRCLE_PULSE, MARKER_DISK_PULSE, MARKER_SPIRAL, MARKER_IMAGE, &
                           HIGHLIGHT_NONE, HIGHLIGHT_PULSE, HIGHLIGHT_ARROWS
    use vkf_line_segments, only: CustomLineSegments, LineSegmentsCallback
    use vkf_shadow, only: ShadowCones, SHADOW_UMBRA, SHADOW_PENUMBRA, SHADOW_ANTUMBRA, SHADOW_ALL
    use vkf_view, only: View, ABSOLUTE_FRAME, RELATIVE_FRAME, VIEW_DIRECT, VIEW_AZEL, ORTHOGRAPHIC, PERSPECTIVE, &
                        camera_between, transform_camera
    use vkf_terrain, only: TerrainBody, rocky_available, LAYER_TMS, LAYER_GDAL, LAYER_GEODETIC, LAYER_MERCATOR, &
                           ELEVATION_HEIGHTS, ELEVATION_MAPBOX, ELEVATION_TERRARIUM

    implicit none

    public

end module vkframes
