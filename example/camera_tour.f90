!> The camera from a program (OpenFrames' trackball get/setTransformation) and
!> smooth transitions (not in OpenFrames): a scripted tour of a spacecraft in
!> an inclined orbit about the Earth.
!>
!> * Views change with a view transition (`set_view_transition`): the camera
!>   flies from the Earth view to one following the spacecraft and back,
!>   instead of cutting (the keys v and V, and the space bar's reset, fly too).
!> * `set_camera(..., in_root=.true., duration=)` flies to a camera given in
!>   the root (inertial) frame: above the north pole, then beside the orbit.
!> * `get_camera(..., in_root=.true.)` reports where the camera is at each
!>   step of the tour.
!>
!> A mouse press ends a flight, and the trackball takes over. Arguments
!> (optional): number of frames to run, then a file to capture the last frame
!> to: `pixi run example camera_tour -- 900 tour.png`
program camera_tour

    use, intrinsic :: iso_fortran_env, only: int64
    use vkframes

    implicit none

    real(wp), parameter :: re = 6378.137_wp, r_orbit = 1.6_wp * re, inc = 50.0_wp * pi / 180.0_wp
    real(wp), parameter :: period = 600.0_wp                    !! s (fast, to see it move)
    integer, parameter :: n_steps = 5
    real(wp), parameter :: step_times(n_steps) = [2.0_wp, 6.0_wp, 10.0_wp, 14.0_wp, 18.0_wp]   !! s from the start

    type(Sphere), target :: earth, craft
    type(Trajectory), target :: orbit
    type(DrawableTrajectory), target :: orbit_frame
    type(CurveArtist), target :: orbit_line
    type(FrameManager), target :: scene
    type(WindowProxy), target :: window
    type(View), target :: earth_view, craft_view
    type(HUDText), target :: hud
    character(len=1024) :: arg, file
    real(wp) :: t, a, eye(3), center(3), up(3), elapsed
    integer(int64) :: start, count, rate
    integer :: k, step, frames, frame
    logical :: running

    frames = huge(frames)
    file = ''
    if (command_argument_count() >= 1) then
        call get_command_argument(1, arg)
        read(arg, *) frames
    end if
    if (command_argument_count() >= 2) call get_command_argument(2, file)

    earth = Sphere('Earth', radius=re)
    call earth%set_texture_map('2k_earth_daymap.jpg')                    ! (data/, on VKFRAMES_DATA_PATH)
    call earth%show_axes(NO_AXES)
    call earth%show_axes_labels(NO_AXES)
    craft = Sphere('Spacecraft', radius=150.0_wp, color=[1.0_wp, 0.3_wp, 0.2_wp])
    orbit = Trajectory()
    do k = 0, 360
        t = period * k / 360.0_wp
        a = 2.0_wp * pi * t / period
        call orbit%add(t, pos=r_orbit * [cos(a), sin(a) * cos(inc), sin(a) * sin(inc)])
    end do
    call craft%follow(orbit)
    orbit_line = CurveArtist(orbit, color=[1.0_wp, 0.8_wp, 0.2_wp], width=2.0_wp)
    orbit_frame = DrawableTrajectory('Orbit')
    call orbit_frame%show_axes(NO_AXES)
    call orbit_frame%show_axes_labels(NO_AXES)
    call orbit_frame%show_name_label(.false.)
    call orbit_frame%add_artist(orbit_line)
    if (.not. (earth%add_child(craft) .and. earth%add_child(orbit_frame))) error stop 'add_child failed'
    scene = FrameManager(earth)

    if (.not. window%open(1000, 700, 'vkframes: camera tour')) error stop 'could not open a window'
    call window%set_scene(scene)
    call window%set_background_color([0.0_wp, 0.0_wp, 0.02_wp])
    earth_view = View(earth, earth)
    call earth_view%set_default_view_distance(2.5_wp * re)
    craft_view = View(earth, craft, frame_type=ABSOLUTE_FRAME)               ! (inertial axes, following it)
    call craft_view%set_default_view_distance(2000.0_wp)
    call window%add_view(earth_view)
    call window%add_view(craft_view)
    call window%set_view_transition(2.5_wp)                                  ! (s: views fly, not cut)
    hud = HUDText('', position=[0.01_wp, 0.01_wp], alignment=ALIGN_LEFT_BOTTOM, size=16.0_wp, &
                  color=[0.85_wp, 0.85_wp, 0.85_wp])
    call window%add_hud_text(hud)

    call system_clock(start, rate)
    step = 0
    running = .true.
    frame = 0
    do while (running .and. frame < frames)
        call system_clock(count)
        elapsed = real(count - start, wp) / real(rate, wp)
        if (step < n_steps) then
            if (elapsed >= step_times(step + 1)) then
                step = step + 1
                call tour_step(step)
            end if
        end if
        running = window%frame()
        frame = frame + 1
    end do
    if (running .and. len_trim(file) > 0) then
        if (window%capture(trim(file))) write(*, '(a)') 'captured '//trim(file)
    end if
    call window%close()

contains

    !> Step `s` of the tour (after reporting where the camera is).
    subroutine tour_step(s)
        integer, intent(in) :: s
        call window%get_camera(eye, center, up, in_root=.true.)
        write(*, '(a,i0,a,3f11.1,a,f9.1,a)') 'step ', s, ': the camera at', eye, ' km (', norm2(eye - center), &
            ' km from what it looks at)'
        select case (s)
        case (1)
            call hud%set_text('flying to the spacecraft view (select_view, a view transition)')
            call window%select_view(2)
        case (2)
            call hud%set_text('flying to a camera above the north pole (set_camera in the root frame)')
            call window%set_camera([0.0_wp, -0.5_wp * re, 5.0_wp * re], [0.0_wp, 0.0_wp, 0.0_wp], &
                                   [0.0_wp, 1.0_wp, 0.0_wp], duration=3.0_wp, in_root=.true.)
        case (3)
            call hud%set_text('back to the Earth view')
            call window%select_view(1)
        case (4)
            call hud%set_text('flying to a camera beside the orbit (set_camera in the root frame)')
            call window%set_camera([4.0_wp * re, -2.0_wp * re, 0.5_wp * re], [0.0_wp, 0.0_wp, 0.0_wp], &
                                   [0.0_wp, 0.0_wp, 1.0_wp], duration=3.0_wp, in_root=.true.)
        case (5)
            call hud%set_text('reset (the space bar): flies home   v / V: the other view')
            call earth_view%reset_view()
        end select
    end subroutine tour_step

end program camera_tour
