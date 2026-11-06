!> One trajectory in two frames, in two windows at once: a free-return lunar
!> flyby (from low Earth orbit, around the Moon's far side and back), computed
!> in inertial coordinates about the Earth, drawn both as it is (cyan) and in
!> the Moon's body-fixed frame (orange), each point where it is relative to
!> the Moon at its own time. One window looks from the inertial frame, the
!> other rides with the Moon.
!>
!> One scene and one copy of the data: the Moon is a frame following its
!> ephemeris (position and attitude); the Moon-fixed curve is a
!> DrawableTrajectory under it whose data frame is the inertial root
!> (`set_data_frame`), so vkframes transforms each point by the Moon's pose
!> at that point's time. A spacecraft marker follows the data in each frame
!> (the Moon-fixed one with `follow(..., data_frame=inertial)`): the two
!> curves meet where the spacecraft is now. (Before set_data_frame this
!> needed a second, transformed Trajectory and a scene for each window; the
!> example checks the drawn Moon-fixed points against that transformed copy
!> and prints the largest difference.)
!>
!> The model, simple but enough to show the frames: the Earth fixed at the
!> origin; the Moon on a circular orbit in the x-y plane, tidally locked (its
!> body-fixed x axis toward the Earth); the spacecraft under both bodies'
!> gravity, integrated (RK4) backwards and forwards from its periapsis at the
!> Moon, 2000 km above the far side, to its perigees at the Earth.
!>
!> Arguments (optional): number of frames to run, then a file prefix to capture
!> both windows' last frames to (prefix_1.png, prefix_2.png):
!>   pixi run example earth_moon_frames -- 300 frames
program earth_moon_frames

    use vkframes

    implicit none

    real(wp), parameter :: mu_earth = 398600.4418_wp, mu_moon = 4902.800_wp   !! km^3/s^2
    real(wp), parameter :: re = 6378.137_wp, rm = 1737.4_wp                    !! km
    real(wp), parameter :: moon_distance = 384400.0_wp                         !! km
    real(wp), parameter :: moon_rate = 2.0_wp * pi / (27.321661_wp * 86400.0_wp)   !! rad/s
    real(wp), parameter :: flyby_altitude = 2000.0_wp, flyby_speed = 2.09_wp   !! km, km/s (relative to the Moon)
    real(wp), parameter :: dt = 60.0_wp, record = 600.0_wp                     !! integration and recording steps, s
    integer, parameter :: max_points = 4000
    real(wp), parameter :: path_color(3) = [0.3_wp, 0.85_wp, 1.0_wp], moon_color(3) = [0.75_wp, 0.75_wp, 0.72_wp]

    real(wp), parameter :: mf_color(3) = [1.0_wp, 0.6_wp, 0.2_wp]

    ! one scene: the inertial root (the Earth at its origin), the Moon following its ephemeris
    type(ReferenceFrame), target :: inertial
    type(Sphere), target :: earth, moon
    type(Trajectory), target :: path, moon_orbit, moon_circle
    type(DrawableTrajectory), target :: path_frame, path_mf_frame, moon_orbit_frame, craft1, craft2, moon_mark_frame
    type(CurveArtist), target :: path_curve, path_mf_curve, moon_orbit_curve
    type(MarkerArtist), target :: marker1, marker2, moon_mark
    type(FrameManager), target :: scene
    type(WindowProxy), target :: w1, w2
    type(View), target :: view1, view2
    type(HUDText), target :: title1, title2, legend, clock_text
    type(SimulationClock), pointer :: master, clock2

    character(len=1024) :: arg, prefix
    character(len=64) :: text
    real(wp) :: times(max_points), states(6, max_points), s(6), t, angle, d(3), worst
    integer :: n, k, frame, max_frames
    logical :: running, ok

    max_frames = huge(max_frames)
    prefix = ''
    if (command_argument_count() >= 1) then
        call get_command_argument(1, arg)
        read(arg, *) max_frames
    end if
    if (command_argument_count() >= 2) call get_command_argument(2, prefix)

    ! the flyby, t = 0 at the periapsis (the Moon on the +x axis), integrated
    ! backwards to the first perigee, then forwards to the second
    s = [moon_distance + rm + flyby_altitude, 0.0_wp, 0.0_wp, &
         0.0_wp, moon_rate * moon_distance - flyby_speed, 0.0_wp]
    n = 0
    call integrate(s, -dt)
    times(1:n) = times(n:1:-1)                              ! (in time order)
    states(:, 1:n) = states(:, n:1:-1)
    n = n - 1                                               ! (the periapsis again, forwards)
    call integrate(s, dt)

    ! the trajectory (inertial), and the Moon's orbit and attitude at the same times
    path = Trajectory()
    moon_orbit = Trajectory()
    call path%reserve(n)
    call moon_orbit%reserve(n, attitudes=.true.)
    do k = 1, n
        t = times(k)
        angle = moon_angle(t)                               ! the Moon-fixed x axis, toward the Earth
        call path%add(t, pos=states(1:3, k))
        call moon_orbit%add(t, pos=moon_position(t), att=quat_from_axis_angle([0.0_wp, 0.0_wp, 1.0_wp], angle))
    end do
    write(*, '(a,i0,a,f6.2,a,f6.2,a)') 'flyby: ', n, ' points, from ', times(1) / 86400.0_wp, ' to ', &
        times(n) / 86400.0_wp, ' days'

    ! the scene: the Earth at the origin, the Moon (its body-fixed frame)
    ! following its ephemeris, the flyby drawn in both frames
    inertial = ReferenceFrame('Inertial')
    call inertial%show_decorations(.false.)
    earth = Sphere('Earth', radius=re)
    call earth%set_texture_map('2k_earth_daymap.jpg')                     ! (data/, on VKFRAMES_DATA_PATH)
    moon = Sphere('Moon', radius=rm, color=moon_color)
    call moon%follow(moon_orbit, mode=FOLLOW_LIMIT)                       ! (position and attitude; not looping)
    path_curve = CurveArtist(path, color=path_color, width=2.0_wp)
    path_frame = DrawableTrajectory('Flyby (inertial)')
    call path_frame%add_artist(path_curve)
    ! the same data in the Moon's frame: each point at its time
    path_mf_curve = CurveArtist(path, color=mf_color, width=2.0_wp)
    path_mf_frame = DrawableTrajectory('Flyby (Moon-fixed)')
    call path_mf_frame%add_artist(path_mf_curve)
    call path_mf_frame%set_data_frame(inertial)
    moon_circle = Trajectory()                              ! (the Moon's whole orbit, for reference)
    do k = 0, 360
        call moon_circle%add(real(k, wp), pos=moon_distance * [cos(k * pi / 180.0_wp), sin(k * pi / 180.0_wp), 0.0_wp])
    end do
    moon_orbit_curve = CurveArtist(moon_circle, color=moon_color, width=1.0_wp)
    call moon_orbit_curve%set_pattern(2, int(z'00FF'))
    moon_orbit_frame = DrawableTrajectory('Moon orbit')
    call moon_orbit_frame%add_artist(moon_orbit_curve)
    ! the spacecraft: a marker on each curve (they meet there)
    marker1 = MarkerArtist(markers=MARKER_START, size=12.0_wp, color=[1.0_wp, 1.0_wp, 1.0_wp])
    craft1 = DrawableTrajectory('Spacecraft')
    call craft1%add_artist(marker1)
    call craft1%follow(path, data=FOLLOW_POSITION)
    marker2 = MarkerArtist(markers=MARKER_START, size=12.0_wp, color=[1.0_wp, 1.0_wp, 1.0_wp])
    craft2 = DrawableTrajectory('Spacecraft (Moon-fixed)')
    call craft2%add_artist(marker2)
    call craft2%follow(path, data=FOLLOW_POSITION, data_frame=inertial)
    ! (the Moon is far smaller than a pixel from the Earth: a marker at it too)
    moon_mark = MarkerArtist(markers=MARKER_START, size=10.0_wp, color=moon_color)
    moon_mark_frame = DrawableTrajectory('Moon marker')
    call moon_mark_frame%add_artist(moon_mark)
    call moon_mark_frame%show_decorations(.false.)
    call hide_axes(earth)
    call hide_axes(moon)
    call path_frame%show_decorations(.false.)
    call path_mf_frame%show_decorations(.false.)
    call moon_orbit_frame%show_decorations(.false.)
    call craft1%show_decorations(.false.)
    call craft2%show_decorations(.false.)
    ok = inertial%add_child(earth) .and. inertial%add_child(moon) .and. inertial%add_child(path_frame) .and. &
         inertial%add_child(moon_orbit_frame) .and. inertial%add_child(craft1) .and. &
         moon%add_child(path_mf_frame) .and. moon%add_child(craft2) .and. moon%add_child(moon_mark_frame)
    if (.not. ok) error stop 'add_child failed'
    scene = FrameManager(inertial)

    ! two windows on the one scene, one clock
    if (.not. w1%open(800, 600, 'vkframes: inertial, about the Earth')) error stop 'could not open window 1'
    if (.not. w2%open(800, 600, 'vkframes: Moon-fixed, about the Moon')) error stop 'could not open window 2'
    call w1%set_scene(scene)
    call w2%set_scene(scene)
    call w1%set_background_color([0.0_wp, 0.0_wp, 0.02_wp])
    call w2%set_background_color([0.0_wp, 0.0_wp, 0.02_wp])
    view1 = View(inertial, earth)
    call view1%set_default_view_parameters(eye=[0.0_wp, -1.6_wp, 2.6_wp] * moon_distance, &
                                           center=[0.0_wp, 0.0_wp, 0.0_wp] * moon_distance, up=[0.0_wp, 0.0_wp, 1.0_wp])
    view2 = View(inertial, moon)                            ! (riding with the Moon: its frame turns with it)
    call view2%set_default_view_parameters(eye=[-20000.0_wp, -90000.0_wp, 65000.0_wp], &
                                           center=[15000.0_wp, 0.0_wp, 0.0_wp], up=[0.0_wp, 0.0_wp, 1.0_wp])
    call w1%add_view(view1)
    call w2%add_view(view2)
    title1 = HUDText('Inertial, about the Earth', position=[0.01_wp, 0.99_wp], alignment=ALIGN_LEFT_TOP, size=18.0_wp)
    title2 = HUDText('Moon-fixed, about the Moon', position=[0.01_wp, 0.99_wp], alignment=ALIGN_LEFT_TOP, size=18.0_wp)
    clock_text = HUDText('', position=[0.01_wp, 0.01_wp], alignment=ALIGN_LEFT_BOTTOM, size=16.0_wp, &
                         color=[0.85_wp, 0.85_wp, 0.85_wp])
    legend = HUDText('cyan: the flyby, inertial;  orange: in the Moon''s frame', position=[0.01_wp, 0.94_wp], &
                     alignment=ALIGN_LEFT_TOP, size=15.0_wp, color=[0.85_wp, 0.85_wp, 0.85_wp])
    call w1%add_hud_text(title1)
    call w2%add_hud_text(title2)
    call w1%add_hud_text(legend)
    call w2%add_hud_text(legend)
    call w1%add_hud_text(clock_text)                        ! (one HUD text, in both windows)
    call w2%add_hud_text(clock_text)
    call w1%set_time_range(times(1), times(n))
    call w1%set_time(times(1))
    call w1%set_time_scale(20000.0_wp)                     ! about 5.5 hours a second
    master => w1%clock()
    clock2 => w2%clock()
    if (.not. clock2%synchronize(master)) error stop 'could not synchronize the clocks'

    ! the Moon-fixed curve as drawn, against the data transformed by hand (each
    ! point by the Moon's position and attitude at its time)
    running = w1%frame()
    running = w2%frame() .and. running
    worst = 0.0_wp
    do k = 1, n
        d = states(1:3, k) - moon_position(times(k))
        worst = max(worst, maxval(abs(path_mf_curve%drawn_point(k) - to_moon_fixed(d, moon_angle(times(k))))))
    end do
    write(*, '(a,es9.2,a)') 'Moon-fixed points drawn: largest difference from the transformed data ', worst, ' km'

    frame = 0
    do while (running .and. frame < max_frames)
        write(text, '(a,f6.2,a)') 't = ', w1%time() / 86400.0_wp, ' days from the flyby'
        call clock_text%set_text(trim(text))
        running = w1%frame()
        running = w2%frame() .and. running
        frame = frame + 1
    end do
    if (len_trim(prefix) > 0 .and. running) then
        if (w1%capture(trim(prefix)//'_1.png')) write(*, '(a)') 'captured '//trim(prefix)//'_1.png'
        if (w2%capture(trim(prefix)//'_2.png')) write(*, '(a)') 'captured '//trim(prefix)//'_2.png'
    end if
    call w2%close()
    call w1%close()

contains

    !> The Moon's position (km) at time t (s): a circular orbit in the x-y plane.
    pure function moon_position(t) result(r)
        real(wp), intent(in) :: t
        real(wp) :: r(3)
        r = moon_distance * [cos(moon_rate * t), sin(moon_rate * t), 0.0_wp]
    end function moon_position

    !> The angle of the Moon-fixed x axis from the inertial x axis at time t:
    !> tidally locked, toward the Earth.
    pure real(wp) function moon_angle(t)
        real(wp), intent(in) :: t
        moon_angle = moon_rate * t + pi
    end function moon_angle

    !> Vector v (inertial axes) in the Moon-fixed axes, turned by `angle` about z.
    pure function to_moon_fixed(v, angle) result(w)
        real(wp), intent(in) :: v(3), angle
        real(wp) :: w(3)
        w = [cos(angle) * v(1) + sin(angle) * v(2), -sin(angle) * v(1) + cos(angle) * v(2), v(3)]
    end function to_moon_fixed

    !> Integrate state s from t = 0 in steps of h (s; negative: backwards) until
    !> the perigee near the Earth, appending a point every `record` s.
    subroutine integrate(s, h)
        real(wp), intent(in) :: s(6)
        real(wp), intent(in) :: h
        real(wp) :: tau, r_prev, r
        real(wp) :: x(6)
        x = s
        tau = 0.0_wp
        call append(tau, x)
        r_prev = norm2(x(1:3))
        do
            call step(tau, x, h)
            tau = tau + h
            r = norm2(x(1:3))
            if (r < 0.2_wp * moon_distance .and. r > r_prev) exit       ! past the perigee
            r_prev = r
            if (modulo(abs(tau) + 1.0e-6_wp, record) < abs(h)) call append(tau, x)
            if (n == max_points) exit
        end do
    end subroutine integrate

    subroutine append(tau, x)
        real(wp), intent(in) :: tau, x(6)
        n = n + 1
        times(n) = tau
        states(:, n) = x
    end subroutine append

    !> One RK4 step of h s from time tau.
    subroutine step(tau, x, h)
        real(wp), intent(in) :: tau, h
        real(wp), intent(inout) :: x(6)
        real(wp) :: k1(6), k2(6), k3(6), k4(6)
        k1 = deriv(tau, x)
        k2 = deriv(tau + 0.5_wp * h, x + 0.5_wp * h * k1)
        k3 = deriv(tau + 0.5_wp * h, x + 0.5_wp * h * k2)
        k4 = deriv(tau + h, x + h * k3)
        x = x + h / 6.0_wp * (k1 + 2.0_wp * k2 + 2.0_wp * k3 + k4)
    end subroutine step

    !> The state's rate of change: the Earth's and the Moon's gravity.
    pure function deriv(tau, x) result(dx)
        real(wp), intent(in) :: tau, x(6)
        real(wp) :: dx(6), dm(3)
        dm = x(1:3) - moon_position(tau)
        dx(1:3) = x(4:6)
        dx(4:6) = -mu_earth * x(1:3) / norm2(x(1:3))**3 - mu_moon * dm / norm2(dm)**3
    end function deriv


    !> No axes (the name label kept).
    subroutine hide_axes(frame)
        class(ReferenceFrame), intent(inout) :: frame
        call frame%show_axes(NO_AXES)
        call frame%show_axes_labels(NO_AXES)
    end subroutine hide_axes

end program earth_moon_frames
