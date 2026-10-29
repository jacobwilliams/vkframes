!> Trace mode with trajectories propagated forwards and backwards in time:
!> one orbit about the Earth split at a match point, as in forward-backward
!> shooting. The forward trajectory runs from t01 = 0 to tf1 = tm (times
!> increasing); the backward one from t02 = P (a period) back to tf2 = tm
!> (times decreasing). Both end in the same state at tm, so together they are
!> one continuous orbit in real time, from 0 to P.
!>
!> Each is drawn in trace mode (orange: forward; cyan: backward), with a
!> spacecraft following it, its start marked (green) and the match point
!> (white). Trace mode draws the points whose times are up to the current
!> time, whatever order they were added in: as time runs from 0 to P the
!> forward trajectory traces from t01 to tm, then the backward one from tm
!> to t02 (its last points first), one history of the orbit; with time
!> reversed (r) it shrinks back. Arrows (a highlight, animated on the GPU)
!> move along each in the order its points were added: its propagation's
!> direction (the backward one's from t02 toward tm, against the time).
!> Each also sweeps out its orbit's plane (a RibbonArtist from the Earth's
!> center to the positions, translucent, in its color), traced with it.
!> The HUD shows the time and, for each, which of its points are drawn
!> (first_drawn, num_drawn) and whether its tail (to the point at the
!> current time) is drawn.
!>
!> Keys: t trace mode on/off (the curves and the planes), p the planes shown
!> or hidden, r the time's direction reversed, 0 the start (t = 0), 9 the
!> end (t = P), space resets the view.
!>
!> Arguments (optional): number of frames to run, a file to capture the last
!> frame to, and the time to start at as a fraction of the period (default
!> 0): `pixi run example trace_directions -- 120 trace.png 0.75`
module trace_directions_keys

    use vkframes

    implicit none

    private

    !> The keys (see the program's documentation).
    type, extends(WindowEventHandler), public :: TraceKeys
        type(CurveArtist), pointer :: curves(:) => null()   !! the forward and backward curves
        type(RibbonArtist), pointer :: planes(:) => null()  !! their planes
        type(DrawableTrajectory), pointer :: plane_frames(:) => null()  !! the planes' frames (shown or hidden)
        real(wp) :: period = 1.0_wp                         !! P (s)
    contains
        procedure :: key_press => trace_key
    end type TraceKeys

contains

    subroutine trace_key(self, window, key, modifiers, handled)
        class(TraceKeys), intent(inout) :: self
        class(WindowProxy), intent(inout) :: window
        integer, intent(in) :: key, modifiers
        logical, intent(inout) :: handled
        integer :: k
        handled = .true.
        select case (key)
        case (iachar('t'))
            do k = 1, size(self%curves)
                call self%curves(k)%set_trace_mode(.not. self%curves(k)%trace_mode())
                call self%planes(k)%set_trace_mode(self%curves(k)%trace_mode())
            end do
        case (iachar('p'))
            do k = 1, size(self%plane_frames)
                call self%plane_frames(k)%show_contents(.not. self%plane_frames(k)%contents_shown())
            end do
        case (iachar('r'))
            call window%set_time_scale(-window%time_scale())
        case (iachar('0'))
            call window%set_time(0.0_wp)
        case (iachar('9'))
            call window%set_time(self%period)
        case default
            handled = .false.
        end select
    end subroutine trace_key

end module trace_directions_keys

program trace_directions

    use vkframes
    use trace_directions_keys

    implicit none

    real(wp), parameter :: mu = 398600.4418_wp, re = 6378.137_wp           !! km^3/s^2, km
    real(wp), parameter :: a = 20000.0_wp, e = 0.5_wp, inc = 30.0_wp * pi / 180.0_wp
    integer, parameter :: n_half = 300                                      !! points per half orbit
    real(wp), parameter :: forward_color(3) = [1.0_wp, 0.6_wp, 0.2_wp], backward_color(3) = [0.3_wp, 0.8_wp, 1.0_wp]

    type(Sphere), target :: earth
    type(Trajectory), target :: forward, backward
    type(CurveArtist), target :: curves(2)
    type(RibbonArtist), target :: planes(2)
    type(DrawableTrajectory), target :: plane_frames(2)
    type(MarkerArtist), target :: forward_marks, backward_marks
    type(DrawableTrajectory), target :: forward_frame, backward_frame
    type(Sphere), target :: craft_f, craft_b
    type(FrameManager), target :: scene
    type(WindowProxy), target :: window
    type(View), target :: earth_view
    type(HUDText), target :: hud, legend
    type(TraceKeys), target :: keys
    character(len=1024) :: arg, file
    character(len=300) :: text
    real(wp) :: period, tm, t, v(3), start_fraction
    integer :: k, frames, frame
    logical :: running, ok_f, ok_b

    frames = huge(frames)
    file = ''
    start_fraction = 0.0_wp
    if (command_argument_count() >= 1) then
        call get_command_argument(1, arg)
        read(arg, *) frames
    end if
    if (command_argument_count() >= 2) call get_command_argument(2, file)
    if (command_argument_count() >= 3) then
        call get_command_argument(3, arg)
        read(arg, *) start_fraction
    end if

    period = 2.0_wp * pi * sqrt(a**3 / mu)
    tm = 0.5_wp * period                                    ! the match point: the apoapsis

    ! the forward trajectory: 0 -> tm (times increasing); the backward one: P -> tm (decreasing)
    forward = Trajectory()
    backward = Trajectory()
    do k = 0, n_half
        t = tm * k / n_half
        call forward%add(t, pos=kepler_position(t))
        t = period - (period - tm) * k / n_half
        call backward%add(t, pos=kepler_position(t))
    end do
    write(*, '(a,f10.1,a,f10.1,a,f10.1,a)') 'forward: t01 = ', forward%time(1), ' s -> tf1 = ', &
        forward%time(forward%num_times()), ' s;  backward: t02 = ', backward%time(1), ' s'
    write(*, '(a,f10.1,a,es10.3,a)') '          tf2 = ', backward%time(backward%num_times()), &
        ' s; the two final states differ by ', &
        norm2(forward%position(forward%num_times()) - backward%position(backward%num_times())), ' km'

    earth = Sphere('Earth', radius=re)
    call earth%set_texture_map('2k_earth_daymap.jpg')
    call earth%show_decorations(.false.)

    curves(1) = CurveArtist(forward, color=forward_color, width=3.0_wp)
    curves(2) = CurveArtist(backward, color=backward_color, width=3.0_wp)
    forward_marks = MarkerArtist(forward, markers=ior(MARKER_START, MARKER_END), size=14.0_wp)
    call forward_marks%set_marker_color(MARKER_START, [0.3_wp, 1.0_wp, 0.4_wp])
    call forward_marks%set_marker_color(MARKER_END, [1.0_wp, 1.0_wp, 1.0_wp])
    backward_marks = MarkerArtist(backward, markers=MARKER_START, size=14.0_wp)
    call backward_marks%set_marker_color(MARKER_START, [0.3_wp, 1.0_wp, 0.4_wp])
    ! the orbits' planes: from the Earth's center to the positions, traced too
    planes(1) = RibbonArtist(forward, color=[forward_color, 0.2_wp])
    planes(2) = RibbonArtist(backward, color=[backward_color, 0.2_wp])
    plane_frames(1) = DrawableTrajectory('Forward plane')
    plane_frames(2) = DrawableTrajectory('Backward plane')
    do k = 1, 2
        call planes(k)%set_trace_mode(.true.)
        call planes(k)%set_pickable(.false.)
        call plane_frames(k)%show_decorations(.false.)
        call plane_frames(k)%add_artist(planes(k))
    end do
    do k = 1, 2
        call curves(k)%set_trace_mode(.true.)
        call curves(k)%set_highlight(HIGHLIGHT_ARROWS, color=[1.0_wp, 1.0_wp, 0.85_wp], arrow_spacing=45.0_wp, &
                                     arrow_speed=40.0_wp, band_width=8.0_wp)    ! (the propagation's direction)
    end do
    forward_frame = DrawableTrajectory('Forward')
    call forward_frame%show_decorations(.false.)
    call forward_frame%add_artist(curves(1))
    call forward_frame%add_artist(forward_marks)
    backward_frame = DrawableTrajectory('Backward')
    call backward_frame%show_decorations(.false.)
    call backward_frame%add_artist(curves(2))
    call backward_frame%add_artist(backward_marks)

    ! a spacecraft on each (at the nearest end outside its times)
    craft_f = Sphere('forward', radius=400.0_wp, color=forward_color)
    call craft_f%show_axes(NO_AXES)
    call craft_f%show_axes_labels(NO_AXES)
    call craft_f%follow(forward, mode=FOLLOW_LIMIT)
    craft_b = Sphere('backward', radius=400.0_wp, color=backward_color)
    call craft_b%show_axes(NO_AXES)
    call craft_b%show_axes_labels(NO_AXES)
    call craft_b%follow(backward, mode=FOLLOW_LIMIT)
    if (.not. (earth%add_child(plane_frames(1)) .and. earth%add_child(plane_frames(2)))) error stop 'add_child failed'
    if (.not. (earth%add_child(forward_frame) .and. earth%add_child(backward_frame) .and. &
               earth%add_child(craft_f) .and. earth%add_child(craft_b))) error stop 'add_child failed'
    scene = FrameManager(earth)

    if (.not. window%open(1100, 800, 'vkframes: trace mode, forwards and backwards')) error stop 'could not open a window'
    call window%set_scene(scene)
    call window%set_background_color([0.0_wp, 0.0_wp, 0.02_wp])
    earth_view = View(earth, earth)
    call earth_view%set_default_view_distance(4.5_wp * a)
    call window%add_view(earth_view)
    call window%set_camera([0.0_wp, -0.6_wp * a, 4.2_wp * a], [-0.5_wp * a, 0.0_wp, 0.0_wp], [0.0_wp, 1.0_wp, 0.0_wp], &
                           in_root=.true.)
    hud = HUDText('', position=[0.01_wp, 0.01_wp], alignment=ALIGN_LEFT_BOTTOM, size=15.0_wp, &
                  color=[0.85_wp, 0.85_wp, 0.85_wp])
    call window%add_hud_text(hud)
    legend = HUDText('orange: forward (t01 = 0 -> tf1 = tm)    cyan: backward (t02 = P -> tf2 = tm)    ' // &
                     'green: starts    white: the match point (tm)    arrows: the propagation''s direction' // &
                     new_line('a') // &
                     't: trace on/off    p: planes    r: reverse time    0 / 9: t = 0 / P', &
                     position=[0.01_wp, 0.99_wp], alignment=ALIGN_LEFT_TOP, size=14.0_wp, color=[0.7_wp, 0.7_wp, 0.7_wp])
    call window%add_hud_text(legend)
    keys%curves => curves
    keys%planes => planes
    keys%plane_frames => plane_frames
    keys%period = period
    call window%set_event_handler(keys)
    call window%set_time_range(0.0_wp, period)
    call window%set_time_scale(period / 24.0_wp)            ! (an orbit in 24 s)
    call window%set_time(start_fraction * period)

    running = .true.
    frame = 0
    do while (running .and. frame < frames)
        t = window%time()
        ok_f = curves(1)%trace_end(v)
        ok_b = curves(2)%trace_end(v)
        write(text, '(a,f6.3,a,a,a,a,a,l1,a,a,a,l1,a,a)') 't = ', t / period, ' P   ', &
            trim(merge('running forwards ', 'running backwards', window%time_scale() >= 0.0_wp)), &
            '    forward: points ', drawn(curves(1), forward), ' (tail ', ok_f, ')    backward: points ', &
            drawn(curves(2), backward), ' (tail ', ok_b, ')', trim(merge('               ', '    [trace off]', &
            curves(1)%trace_mode()))
        call hud%set_text(trim(text))
        running = window%frame()
        frame = frame + 1
    end do
    if (running .and. len_trim(file) > 0) then
        write(*, '(a)') trim(text)
        if (window%capture(trim(file))) write(*, '(a)') 'captured '//trim(file)
    end if
    call window%close()

contains

    !> The points a curve draws: 'i to j of n', or 'none of n'.
    function drawn(curve, traj) result(text)
        type(CurveArtist), intent(in) :: curve
        type(Trajectory), intent(in) :: traj
        character(len=:), allocatable :: text
        character(len=40) :: buffer
        if (curve%num_drawn() == 0) then
            write(buffer, '(a,i0)') 'none of ', traj%num_times()
        else
            write(buffer, '(i0,a,i0,a,i0)') curve%first_drawn(), ' to ', curve%first_drawn() + curve%num_drawn() - 1, &
                ' of ', traj%num_times()
        end if
        text = trim(buffer)
    end function drawn

    !> The position (km) at time t on the orbit (periapsis at t = 0, Kepler's equation).
    function kepler_position(t) result(p)
        real(wp), intent(in) :: t
        real(wp) :: p(3), m, ea, nu, r
        integer :: i
        m = 2.0_wp * pi * t / period
        ea = m
        do i = 1, 30
            ea = ea - (ea - e * sin(ea) - m) / (1.0_wp - e * cos(ea))
        end do
        nu = 2.0_wp * atan2(sqrt(1.0_wp + e) * sin(0.5_wp * ea), sqrt(1.0_wp - e) * cos(0.5_wp * ea))
        r = a * (1.0_wp - e * cos(ea))
        p = r * [cos(nu), sin(nu) * cos(inc), sin(nu) * sin(inc)]
    end function kepler_position

end program trace_directions
