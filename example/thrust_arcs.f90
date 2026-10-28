!> Keys for the thrust_arcs example (a WindowEventHandler): + and - change the
!> thrust vectors' scale, t toggles trace mode.
module thrust_arcs_keys

    use vkframes

    implicit none

    private

    type, extends(WindowEventHandler), public :: ThrustKeys
        type(SegmentArtist), pointer :: thrust => null()
        type(CurveArtist), pointer :: curve => null()
        type(HUDText), pointer :: hud => null()
        real(wp) :: scale = 1000.0_wp                   !! km of vector per m/s^2 of thrust acceleration
    contains
        procedure :: key_press => thrust_key_press
        procedure :: show => show_scale
    end type ThrustKeys

contains

    subroutine thrust_key_press(self, window, key, modifiers, handled)
        class(ThrustKeys), intent(inout) :: self
        class(WindowProxy), intent(inout) :: window
        integer, intent(in) :: key, modifiers
        logical, intent(inout) :: handled
        logical :: trace
        if (key == iachar('+') .or. key == iachar('=')) then
            self%scale = 2.0_wp * self%scale
        else if (key == iachar('-')) then
            self%scale = 0.5_wp * self%scale
        else if (key == iachar('t')) then
            trace = .not. self%curve%trace_mode()
            call self%curve%set_trace_mode(trace)
            call self%thrust%set_trace_mode(trace)
        else
            return
        end if
        call self%thrust%set_end_scale(self%scale)     ! (only the vectors are written again)
        call self%show()
        handled = .true.
    end subroutine thrust_key_press

    subroutine show_scale(self)
        class(ThrustKeys), intent(inout) :: self
        character(len=128) :: text
        write(text, '(a,g0.4,a)') 'thrust vectors: ', self%scale, ' km per m/s^2   (+ / -: scale, t: trace)'
        call self%hud%set_text(trim(text))
    end subroutine show_scale

end module thrust_arcs_keys

!> Thrust vectors along a trajectory (SegmentArtist with relative ends, not
!> in OpenFrames): a transfer from an 8000 km circular orbit to a 16000 km
!> one, inclined 40 degrees to it, with a finite burn at the start (raising
!> the apoapsis; pitched 20 degrees away from the Earth) and another at the
!> apoapsis (circularizing and changing the plane, as a transfer to
!> geostationary orbit does), integrated with the thrust. Each point stores
!> its thrust acceleration (m/s^2, zero while coasting) as an optional; the
!> vectors are drawn from the positions, `scale` km per m/s^2, every 3rd
!> point, and nothing where there is no thrust. The orbit is orange while
!> thrusting (point colors); the initial and final orbits are dashed.
!>
!> Keys: + and - double and halve the vectors' scale (the trajectory is
!> untouched, only the vectors are written again), t toggles trace mode, the
!> space bar resets the view. Arguments (optional): the number of frames to
!> run, then a file to capture the last frame to:
!>   pixi run example thrust_arcs -- 300 thrust.png
program thrust_arcs

    use vkframes
    use thrust_arcs_keys, only: ThrustKeys

    implicit none

    real(wp), parameter :: mu = 398600.4418_wp                  !! km^3/s^2
    real(wp), parameter :: re = 6378.137_wp                     !! km
    real(wp), parameter :: r1 = 8000.0_wp, r2 = 16000.0_wp      !! km
    real(wp), parameter :: accel = 2.0_wp                       !! thrust acceleration, m/s^2
    real(wp), parameter :: pitch = 20.0_wp * pi / 180.0_wp      !! the first burn: away from the Earth
    real(wp), parameter :: plane_change = -40.0_wp * pi / 180.0_wp   !! the second burn: out of the plane
    real(wp), parameter :: dt = 1.0_wp, record = 10.0_wp        !! integration and recording steps, s
    real(wp), parameter :: burn_color(3) = [1.0_wp, 0.55_wp, 0.1_wp], coast_color(3) = [0.3_wp, 0.8_wp, 1.0_wp]
    integer, parameter :: COAST = 0

    type(ReferenceFrame), target :: root
    type(Sphere), target :: earth
    type(Trajectory), target :: orbit, initial, final
    type(DrawableTrajectory), target :: orbit_frame, reference_frame, craft
    type(CurveArtist), target :: orbit_curve, initial_curve, final_curve
    type(SegmentArtist), target :: thrust
    type(MarkerArtist), target :: marker
    type(HUDText), target :: hud
    type(ThrustKeys), target :: keys
    type(FrameManager), target :: scene
    type(WindowProxy), target :: window
    type(View), target :: top_view
    character(len=1024) :: arg, file
    real(wp) :: x(6), t, dv1, dv2, a
    real(wp) :: burn2_dir(3) = 0.0_wp                           !! the second burn's (inertial) direction
    integer :: frames, frame
    logical :: running, ok

    frames = huge(frames)
    file = ''
    if (command_argument_count() >= 1) then
        call get_command_argument(1, arg)
        read(arg, *) frames
    end if
    if (command_argument_count() >= 2) call get_command_argument(2, file)

    ! the first burn's delta-v (km/s): a Hohmann transfer's along the velocity
    a = 0.5_wp * (r1 + r2)
    dv1 = (sqrt(mu * (2.0_wp / r1 - 1.0_wp / a)) - sqrt(mu / r1)) / cos(pitch)

    ! the transfer: the first burn, a coast to the apoapsis, the second burn,
    ! and a little of the final orbit; a point every `record` s with its thrust
    orbit = Trajectory(nopt=1)
    x = [r1, 0.0_wp, 0.0_wp, 0.0_wp, sqrt(mu / r1), 0.0_wp]
    t = 0.0_wp
    initial = Trajectory()
    call sample_orbit(initial, x)
    call fly(1000.0_wp * dv1 / accel, 1)
    call coast_to_apoapsis()
    ! the second burn: toward the circular orbit's velocity there, in the plane
    ! turned by plane_change about the radius (an inertial direction)
    call second_burn(dv2)
    call fly(1000.0_wp * dv2 / accel, 2)
    final = Trajectory()
    call sample_orbit(final, x)
    call fly(1800.0_wp, COAST)

    ! the scene: the Earth, the orbits, the thrust vectors, the spacecraft
    root = ReferenceFrame('Inertial')
    call root%show_decorations(.false.)
    earth = Sphere('Earth', radius=re)
    call earth%set_texture_map('2k_earth_daymap.jpg')                    ! (data/, on VKFRAMES_DATA_PATH)
    call earth%show_decorations(.false.)

    initial_curve = CurveArtist(initial, color=[0.6_wp, 0.6_wp, 0.6_wp], width=1.0_wp)
    final_curve = CurveArtist(final, color=[0.6_wp, 0.6_wp, 0.6_wp], width=1.0_wp)
    call initial_curve%set_pattern(2, int(z'00FF'))
    call final_curve%set_pattern(2, int(z'00FF'))
    reference_frame = DrawableTrajectory('Reference orbits')
    call reference_frame%show_decorations(.false.)
    call reference_frame%add_artist(initial_curve)
    call reference_frame%add_artist(final_curve)

    orbit_curve = CurveArtist(orbit, color=coast_color, width=2.5_wp)    ! (in its point colors)
    ! the thrust vectors: from each position, the stored thrust times the scale
    thrust = SegmentArtist(orbit, color=burn_color, width=1.5_wp, stride=3)
    ok = thrust%set_start_data(position_sources(3))
    ok = ok .and. thrust%set_end_data(optional_sources(1, scale=keys%scale))
    if (.not. ok) error stop 'thrust: data sources'
    call thrust%set_end_relative(.true.)
    orbit_frame = DrawableTrajectory('Transfer')
    call orbit_frame%show_decorations(.false.)
    call orbit_frame%add_artist(thrust)
    call orbit_frame%add_artist(orbit_curve)

    marker = MarkerArtist(markers=MARKER_START, size=12.0_wp, color=[1.0_wp, 1.0_wp, 1.0_wp])
    craft = DrawableTrajectory('Spacecraft')
    call craft%show_decorations(.false.)
    call craft%add_artist(marker)
    call craft%follow(orbit, data=FOLLOW_POSITION)

    ok = root%add_child(earth) .and. root%add_child(reference_frame) .and. root%add_child(orbit_frame) .and. &
         root%add_child(craft)
    if (.not. ok) error stop 'add_child failed'
    scene = FrameManager(root)

    if (.not. window%open(1100, 800, 'vkframes: thrust arcs')) error stop 'could not open a window'
    call window%set_scene(scene)
    call window%set_background_color([0.0_wp, 0.0_wp, 0.02_wp])
    call window%set_time_scale(300.0_wp)                                 ! 5 minutes a second
    top_view = View(root, earth)
    call top_view%set_default_view_parameters(eye=[11000.0_wp, -42500.0_wp, 21500.0_wp], &
                                              center=[-4000.0_wp, 0.0_wp, 0.0_wp], up=[0.0_wp, 0.0_wp, 1.0_wp])
    call window%add_view(top_view)
    hud = HUDText('', position=[0.01_wp, 0.01_wp], alignment=ALIGN_LEFT_BOTTOM, size=16.0_wp, &
                  color=[0.85_wp, 0.85_wp, 0.85_wp])
    call window%add_hud_text(hud)

    ! keys: + and - for the vectors' scale, t for trace mode
    keys%thrust => thrust
    keys%curve => orbit_curve
    keys%hud => hud
    call keys%show()
    call window%set_event_handler(keys)

    running = .true.
    frame = 0
    do while (running .and. frame < frames)
        running = window%frame()
        frame = frame + 1
    end do
    if (running .and. len_trim(file) > 0) then
        if (window%capture(trim(file))) write(*, '(a)') 'captured '//trim(file)
    end if
    call window%close()

contains

    !> Integrate for `duration` s in phase `burn` (1, 2, or COAST), adding a
    !> point every `record` s (with its color and thrust).
    subroutine fly(duration, burn)
        real(wp), intent(in) :: duration
        integer, intent(in) :: burn
        real(wp) :: t_end
        t_end = t + duration
        call add_point(burn)
        do while (t < t_end - 0.5_wp * dt)
            call step(x, min(dt, t_end - t), burn)
            t = t + min(dt, t_end - t)
            if (modulo(t + 1.0e-9_wp, record) < dt .or. t >= t_end - 0.5_wp * dt) call add_point(burn)
        end do
    end subroutine fly

    !> Coast until the radial velocity turns negative (the apoapsis).
    subroutine coast_to_apoapsis()
        do while (dot_product(x(1:3), x(4:6)) >= 0.0_wp)
            call step(x, dt, COAST)
            t = t + dt
            if (modulo(t + 1.0e-9_wp, record) < dt) call add_point(COAST)
        end do
    end subroutine coast_to_apoapsis

    !> The point at time t: the position, its color, its thrust (m/s^2). (A
    !> point repeated at a phase's start is skipped; the new phase's thrust
    !> replaces the last one's.)
    subroutine add_point(burn)
        integer, intent(in) :: burn
        logical :: repeated
        repeated = .false.
        if (orbit%num_times() > 0) repeated = orbit%time(orbit%num_times()) >= t
        if (.not. repeated) call orbit%add(t, pos=x(1:3), color=merge(burn_color, coast_color, burn /= COAST))
        if (burn /= COAST .or. .not. repeated) call orbit%set_optional(1, accel * thrust_direction(x, burn))
    end subroutine add_point

    !> The second burn's delta-v (km/s) and direction (burn2_dir), from the
    !> state at the apoapsis: to the circular speed there, the velocity's
    !> direction turned by plane_change about the radius.
    subroutine second_burn(dv)
        real(wp), intent(out) :: dv
        real(wp) :: r(3), v(3), n(3), target(3), delta(3)
        r = x(1:3) / norm2(x(1:3))
        v = x(4:6) - dot_product(x(4:6), r) * r
        v = v / norm2(v)
        n = [r(2) * v(3) - r(3) * v(2), r(3) * v(1) - r(1) * v(3), r(1) * v(2) - r(2) * v(1)]
        target = sqrt(mu / norm2(x(1:3))) * (cos(plane_change) * v + sin(plane_change) * n)
        delta = target - x(4:6)
        dv = norm2(delta)
        burn2_dir = delta / dv
    end subroutine second_burn

    !> One orbit from state s (no thrust), a point a minute: for reference.
    subroutine sample_orbit(traj, s0)
        type(Trajectory), intent(inout) :: traj
        real(wp), intent(in) :: s0(6)
        real(wp) :: s(6), period, tau
        integer :: k
        s = s0
        period = 2.0_wp * pi * sqrt((1.0_wp / (2.0_wp / norm2(s(1:3)) - dot_product(s(4:6), s(4:6)) / mu))**3 / mu)
        tau = 0.0_wp
        call traj%add(tau, pos=s(1:3))
        do while (tau < period)
            do k = 1, 60
                call step(s, dt, COAST)
            end do
            tau = tau + 60.0_wp * dt
            call traj%add(tau, pos=s(1:3))
        end do
    end subroutine sample_orbit

    !> One RK4 step of h s of state s: two-body gravity, and burn `burn`'s thrust.
    subroutine step(s, h, burn)
        real(wp), intent(inout) :: s(6)
        real(wp), intent(in) :: h
        integer, intent(in) :: burn
        real(wp) :: k1(6), k2(6), k3(6), k4(6)
        k1 = deriv(s, burn)
        k2 = deriv(s + 0.5_wp * h * k1, burn)
        k3 = deriv(s + 0.5_wp * h * k2, burn)
        k4 = deriv(s + h * k3, burn)
        s = s + h / 6.0_wp * (k1 + 2.0_wp * k2 + 2.0_wp * k3 + k4)
    end subroutine step

    !> The state's rate of change (km, km/s): gravity, and the thrust.
    pure function deriv(s, burn) result(d)
        real(wp), intent(in) :: s(6)
        integer, intent(in) :: burn
        real(wp) :: d(6)
        d(1:3) = s(4:6)
        d(4:6) = -mu * s(1:3) / norm2(s(1:3))**3 + 1.0e-3_wp * accel * thrust_direction(s, burn)   ! (km/s^2)
    end function deriv

    !> The thrust's direction in burn `burn` (zero while coasting): burn 1
    !> along the velocity, pitched away from the Earth; burn 2 fixed
    !> (second_burn).
    pure function thrust_direction(s, burn) result(u)
        real(wp), intent(in) :: s(6)
        integer, intent(in) :: burn
        real(wp) :: u(3), v(3), r(3)
        u = 0.0_wp
        if (burn == COAST) return
        if (burn == 2) then
            u = burn2_dir
            return
        end if
        v = s(4:6) / norm2(s(4:6))
        r = s(1:3) - dot_product(s(1:3), v) * v                      ! (away from the Earth, across the velocity)
        u = cos(pitch) * v + sin(pitch) * r / norm2(r)
    end function thrust_direction

end program thrust_arcs
