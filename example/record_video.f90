!> Recording a video (WindowProxy%start_recording, not in OpenFrames): a
!> spacecraft on an orbit about the Earth (the orbit highlighted, the
!> spacecraft's marker pulsing), with a camera flight half way, recorded to
!> a video file by ffmpeg, then the window closes.
!>
!> While recording, each frame advances the simulation time by exactly
!> scale/fps, and the flight and the animations by 1/fps s: the video plays
!> smoothly at its own speed however long frames take to draw and encode
!> (the window may run slower or faster than the video meanwhile).
!>
!> Arguments (optional): the video file (default orbit.mp4), its length in
!> seconds (default 8): `pixi run example record_video -- orbit.mp4 8`
program record_video

    use, intrinsic :: iso_fortran_env, only: int64
    use vkframes

    implicit none

    real(wp), parameter :: re = 6378.137_wp, r_orbit = 2.2_wp * re, inc = 35.0_wp * pi / 180.0_wp
    real(wp), parameter :: period = 3600.0_wp                   !! s (simulated)
    real(wp), parameter :: fps = 30.0_wp

    type(Sphere), target :: earth, craft
    type(Trajectory), target :: orbit
    type(DrawableTrajectory), target :: orbit_frame, ring
    type(CurveArtist), target :: orbit_line
    type(MarkerArtist), target :: craft_marker
    type(Trajectory), target :: craft_point
    type(FrameManager), target :: scene
    type(WindowProxy), target :: window
    type(View), target :: earth_view
    type(HUDText), target :: hud
    character(len=1024) :: file, arg
    character(len=64) :: text
    real(wp) :: seconds, t, a, wall
    integer(int64) :: start, count, rate, written
    integer :: k, n_frames
    logical :: running, flown

    file = 'orbit.mp4'
    seconds = 8.0_wp
    if (command_argument_count() >= 1) call get_command_argument(1, file)
    if (command_argument_count() >= 2) then
        call get_command_argument(2, arg)
        read(arg, *) seconds
    end if
    n_frames = nint(seconds * fps)

    earth = Sphere('Earth', radius=re)
    call earth%set_texture_map('2k_earth_daymap.jpg')                    ! (data/, on VKFRAMES_DATA_PATH)
    call earth%show_decorations(.false.)
    craft = Sphere('Spacecraft', radius=250.0_wp, color=[1.0_wp, 0.35_wp, 0.2_wp])
    call craft%show_decorations(.false.)
    orbit = Trajectory()
    do k = 0, 720
        t = period * k / 720.0_wp
        a = 2.0_wp * pi * t / period
        call orbit%add(t, pos=r_orbit * [cos(a), sin(a) * cos(inc), sin(a) * sin(inc)])
    end do
    call craft%follow(orbit)
    orbit_line = CurveArtist(orbit, color=[0.3_wp, 0.75_wp, 1.0_wp], width=2.0_wp)
    call orbit_line%set_highlight(HIGHLIGHT_PULSE + HIGHLIGHT_ARROWS, color=[1.0_wp, 1.0_wp, 0.85_wp])
    craft_point = Trajectory()
    call craft_point%add(0.0_wp, pos=[0.0_wp, 0.0_wp, 0.0_wp])
    craft_marker = MarkerArtist(craft_point)                            ! (a pulsing ring around the spacecraft)
    call craft_marker%set_markers(MARKER_START)
    call craft_marker%set_marker_shape(MARKER_CIRCLE_PULSE)
    call craft_marker%set_marker_size(30.0_wp)
    call craft_marker%set_marker_color(MARKER_START, [1.0_wp, 0.6_wp, 0.3_wp])
    orbit_frame = DrawableTrajectory('Orbit')
    call orbit_frame%show_decorations(.false.)
    call orbit_frame%add_artist(orbit_line)
    if (.not. (earth%add_child(craft) .and. earth%add_child(orbit_frame))) error stop 'add_child failed'
    ring = DrawableTrajectory('Ring')
    call ring%show_decorations(.false.)
    call ring%add_artist(craft_marker)
    if (.not. craft%add_child(ring)) error stop 'add_child failed'
    scene = FrameManager(earth)

    if (.not. window%open(960, 540, 'vkframes: recording a video')) error stop 'could not open a window'
    call window%set_scene(scene)
    call window%set_background_color([0.0_wp, 0.0_wp, 0.02_wp])
    earth_view = View(earth, earth)
    call earth_view%set_default_view_distance(6.0_wp * re)
    call window%add_view(earth_view)
    hud = HUDText('', position=[0.01_wp, 0.01_wp], alignment=ALIGN_LEFT_BOTTOM, size=16.0_wp, &
                  color=[0.85_wp, 0.85_wp, 0.85_wp])
    call window%add_hud_text(hud)
    running = window%frame()                                     ! (shown first: the video starts drawn)

    if (.not. window%start_recording(trim(file), fps=fps)) error stop 'could not start recording'
    call window%set_time_scale(period / 12.0_wp)                 ! (an orbit in 12 s of video)
    call window%set_time(0.0_wp)
    call system_clock(start, rate)
    flown = .false.
    do k = 1, n_frames
        if (.not. running) exit
        if (.not. flown .and. k > n_frames / 2) then             ! (half way: fly to above the orbit)
            call window%set_camera([0.5_wp * re, -2.5_wp * re, 6.5_wp * re], [0.0_wp, 0.0_wp, 0.0_wp], &
                                   [0.0_wp, 1.0_wp, 0.0_wp], duration=2.0_wp, in_root=.true.)
            flown = .true.
        end if
        write(text, '(a,f7.1,a)') 't = ', window%time() / 60.0_wp, ' min'
        call hud%set_text(trim(text))
        running = window%frame()
    end do
    if (.not. window%stop_recording(written)) error stop 'the recording failed (see ffmpeg''s messages)'
    call system_clock(count)
    wall = real(count - start, wp) / real(rate, wp)
    write(*, '(a,i0,a,f5.1,a,f5.1,a)') 'recorded '//trim(file)//': ', written, ' frames (', written / fps, &
        ' s of video, in ', wall, ' s)'
    call window%close()

end program record_video
