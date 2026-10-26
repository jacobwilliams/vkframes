!> vkframes: a modern Fortran library for visualizing spacecraft trajectories,
!> rendering with VulkanSceneGraph. `use vkframes` gives the whole public API.
module vkframes

    use vkf_math, only: wp, pi, quat_identity, quat_from_axis_angle, quat_multiply, quat_conjugate, &
                        quat_normalize, quat_rotate, quat_to_matrix, quat_slerp, &
                        quat_from_vectors, identity_matrix, translation_matrix, frame_matrix, rigid_inverse
    use vkf_trajectory, only: Trajectory, DataSource, position_sources, &
                              SOURCE_ZERO, SOURCE_TIME, SOURCE_POSOPT, SOURCE_ATTITUDE, &
                              TIME_NOT_FOUND, TIME_OUT_OF_RANGE, TIME_AT_BOUND
    use vkf_follower, only: TrajectoryFollower, FOLLOW_LOOP, FOLLOW_LIMIT, FOLLOW_POSITION, FOLLOW_ATTITUDE
    use vkf_clock, only: SimulationClock, wall_time
    use vkf_frames, only: ReferenceFrame, FramePointer, NO_AXES, X_AXIS, Y_AXIS, Z_AXIS, ALL_AXES, &
                          ALIGN_LEFT_TOP, ALIGN_LEFT_CENTER, ALIGN_LEFT_BOTTOM, &
                          ALIGN_CENTER_TOP, ALIGN_CENTER_CENTER, ALIGN_CENTER_BOTTOM, &
                          ALIGN_RIGHT_TOP, ALIGN_RIGHT_CENTER, ALIGN_RIGHT_BOTTOM, &
                          ALIGN_LEFT_BASE_LINE, ALIGN_CENTER_BASE_LINE, ALIGN_RIGHT_BASE_LINE
    use vkf_sphere, only: Sphere
    use vkf_model, only: Model
    use vkf_window, only: FrameManager, WindowProxy, MOUSE_MOVE, MOUSE_PRESS, MOUSE_RELEASE
    use vkf_hud, only: HUDText
    use vkf_sky, only: SkySphere, SKY_NONE, SKY_TEXTURE, SKY_STARS
    use vkf_files, only: find_data_file
    use vkf_artists, only: TrajectoryArtist, CurveArtist, SegmentArtist, MarkerArtist, DrawableTrajectory, &
                           MARKER_START, MARKER_INTERMEDIATE, MARKER_END, &
                           INTERMEDIATE_TIME, INTERMEDIATE_DISTANCE, INTERMEDIATE_DATA, &
                           MARKER_DISK, MARKER_CIRCLE, MARKER_SQUARE, MARKER_PLUS, MARKER_X, MARKER_TARGET, &
                           MARKER_ROSE, MARKER_CIRCLE_PULSE, MARKER_DISK_PULSE, MARKER_SPIRAL, MARKER_IMAGE
    use vkf_view, only: View, ABSOLUTE_FRAME, RELATIVE_FRAME, VIEW_DIRECT, VIEW_AZEL, ORTHOGRAPHIC, PERSPECTIVE

    implicit none

    public

end module vkframes
