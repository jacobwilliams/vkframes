!> One trajectory in two frames, in two windows at once: a free-return lunar
!> flyby (from low Earth orbit, around the Moon's far side and back), shown in
!> inertial coordinates about the Earth in one window and in the Moon's
!> body-fixed frame about the Moon in the other.
!>
!> vkframes draws a trajectory in the coordinates of the frame it is in, so
!> the Moon-fixed view needs the points transformed into that frame, each at
!> its own time (the Moon's position and attitude then): a second Trajectory,
!> computed here. Each window has a scene of its own (a FrameManager with its
!> own root and frames), and their clocks are synchronized, so both show the
!> same moment, with a spacecraft marker following each copy.
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

    ! the two scenes: inertial about the Earth, and Moon-fixed about the Moon
    type(ReferenceFrame), target :: inertial, moon_fixed
    type(Sphere), target :: earth1, moon1, earth2, moon2
    type(Trajectory), target :: path, path_mf, moon_orbit, earth_mf, moon_circle
    type(DrawableTrajectory), target :: path_frame, path_mf_frame, moon_orbit_frame, craft1, craft2, moon_mark_frame
    type(CurveArtist), target :: path_curve, path_mf_curve, moon_orbit_curve
    type(MarkerArtist), target :: marker1, marker2, moon_mark
    type(FrameManager), target :: scene_inertial, scene_moon
    type(WindowProxy), target :: w1, w2
    type(View), target :: view1, view2
    type(HUDText), target :: title1, title2, clock_text
    type(SimulationClock), pointer :: master, clock2

    character(len=1024) :: arg, prefix
    character(len=64) :: text
    real(wp) :: times(max_points), states(6, max_points), s(6), t, angle, d(3)
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

    ! the trajectory twice: inertial, and in the Moon-fixed frame (each point
    ! at its own time); the Moon's orbit and attitude, and the Earth as seen
    ! from the Moon, at the same times
    path = Trajectory()
    path_mf = Trajectory()
    moon_orbit = Trajectory()
    earth_mf = Trajectory()
    call path%reserve(n)
    call path_mf%reserve(n)
    call moon_orbit%reserve(n, attitudes=.true.)
    call earth_mf%reserve(n)
    do k = 1, n
        t = times(k)
        angle = moon_angle(t)                               ! the Moon-fixed x axis, toward the Earth
        call path%add(t, pos=states(1:3, k))
        call moon_orbit%add(t, pos=moon_position(t), att=quat_from_axis_angle([0.0_wp, 0.0_wp, 1.0_wp], angle))
        d = states(1:3, k) - moon_position(t)
        call path_mf%add(t, pos=to_moon_fixed(d, angle))
        call earth_mf%add(t, pos=to_moon_fixed(-moon_position(t), angle))
    end do
    write(*, '(a,i0,a,f6.2,a,f6.2,a)') 'flyby: ', n, ' points, from ', times(1) / 86400.0_wp, ' to ', &
        times(n) / 86400.0_wp, ' days'

    ! scene 1: the Earth at the origin, the Moon on its orbit, the trajectory
    inertial = ReferenceFrame('Inertial')
    call inertial%show_decorations(.false.)
    earth1 = Sphere('Earth', radius=re)
    call earth1%set_texture_map('2k_earth_daymap.jpg')                    ! (data/, on VKFRAMES_DATA_PATH)
    moon1 = Sphere('Moon', radius=rm, color=moon_color)
    call moon1%follow(moon_orbit)                                         ! (position and attitude)
    path_curve = CurveArtist(path, color=path_color, width=2.0_wp)
    path_frame = DrawableTrajectory('Flyby (inertial)')
    call path_frame%add_artist(path_curve)
    moon_circle = Trajectory()                              ! (the Moon's whole orbit, for reference)
    do k = 0, 360
        call moon_circle%add(real(k, wp), pos=moon_distance * [cos(k * pi / 180.0_wp), sin(k * pi / 180.0_wp), 0.0_wp])
    end do
    moon_orbit_curve = CurveArtist(moon_circle, color=moon_color, width=1.0_wp)
    call moon_orbit_curve%set_pattern(2, int(z'00FF'))
    moon_orbit_frame = DrawableTrajectory('Moon orbit')
    call moon_orbit_frame%add_artist(moon_orbit_curve)
    marker1 = MarkerArtist(markers=MARKER_START, size=12.0_wp, color=[1.0_wp, 1.0_wp, 1.0_wp])
    craft1 = DrawableTrajectory('Spacecraft')
    call craft1%add_artist(marker1)
    call craft1%follow(path, data=FOLLOW_POSITION)
    ! (the Moon is far smaller than a pixel from here: a marker at it too)
    moon_mark = MarkerArtist(markers=MARKER_START, size=10.0_wp, color=moon_color)
    moon_mark_frame = DrawableTrajectory('Moon marker')
    call moon_mark_frame%add_artist(moon_mark)
    call moon_mark_frame%show_decorations(.false.)
    if (.not. moon1%add_child(moon_mark_frame)) error stop 'add_child failed'
    call hide_axes(earth1)
    call hide_axes(moon1)
    call path_frame%show_decorations(.false.)
    call moon_orbit_frame%show_decorations(.false.)
    call craft1%show_decorations(.false.)
    ok = inertial%add_child(earth1) .and. inertial%add_child(moon1) .and. inertial%add_child(path_frame) .and. &
         inertial%add_child(moon_orbit_frame) .and. inertial%add_child(craft1)
    if (.not. ok) error stop 'add_child failed'
    scene_inertial = FrameManager(inertial)

    ! scene 2: the Moon at the origin (not turning: the frame is its own), the
    ! Earth where it is seen from the Moon, the transformed trajectory
    moon_fixed = ReferenceFrame('Moon-fixed')
    call moon_fixed%show_decorations(.false.)
    moon2 = Sphere('Moon', radius=rm, color=moon_color)
    earth2 = Sphere('Earth', radius=re)
    call earth2%set_texture_map('2k_earth_daymap.jpg')
    call earth2%follow(earth_mf, data=FOLLOW_POSITION)
    path_mf_curve = CurveArtist(path_mf, color=path_color, width=2.0_wp)
    path_mf_frame = DrawableTrajectory('Flyby (Moon-fixed)')
    call path_mf_frame%add_artist(path_mf_curve)
    marker2 = MarkerArtist(markers=MARKER_START, size=12.0_wp, color=[1.0_wp, 1.0_wp, 1.0_wp])
    craft2 = DrawableTrajectory('Spacecraft')
    call craft2%add_artist(marker2)
    call craft2%follow(path_mf, data=FOLLOW_POSITION)
    call hide_axes(moon2)
    call hide_axes(earth2)
    call path_mf_frame%show_decorations(.false.)
    call craft2%show_decorations(.false.)
    ok = moon_fixed%add_child(moon2) .and. moon_fixed%add_child(earth2) .and. moon_fixed%add_child(path_mf_frame) .and. &
         moon_fixed%add_child(craft2)
    if (.not. ok) error stop 'add_child failed'
    scene_moon = FrameManager(moon_fixed)

    ! two windows, one scene each, one clock
    if (.not. w1%open(800, 600, 'vkframes: inertial, about the Earth')) error stop 'could not open window 1'
    if (.not. w2%open(800, 600, 'vkframes: Moon-fixed, about the Moon')) error stop 'could not open window 2'
    call w1%set_scene(scene_inertial)
    call w2%set_scene(scene_moon)
    call w1%set_background_color([0.0_wp, 0.0_wp, 0.02_wp])
    call w2%set_background_color([0.0_wp, 0.0_wp, 0.02_wp])
    view1 = View(inertial, earth1)
    call view1%set_default_view_parameters(eye=[0.0_wp, -1.6_wp, 2.6_wp] * moon_distance, &
                                           center=[0.0_wp, 0.0_wp, 0.0_wp] * moon_distance, up=[0.0_wp, 0.0_wp, 1.0_wp])
    view2 = View(moon_fixed, moon2)
    call view2%set_default_view_parameters(eye=[-20000.0_wp, -90000.0_wp, 65000.0_wp], &
                                           center=[15000.0_wp, 0.0_wp, 0.0_wp], up=[0.0_wp, 0.0_wp, 1.0_wp])
    call w1%add_view(view1)
    call w2%add_view(view2)
    title1 = HUDText('Inertial, about the Earth', position=[0.01_wp, 0.99_wp], alignment=ALIGN_LEFT_TOP, size=18.0_wp)
    title2 = HUDText('Moon-fixed, about the Moon', position=[0.01_wp, 0.99_wp], alignment=ALIGN_LEFT_TOP, size=18.0_wp)
    clock_text = HUDText('', position=[0.01_wp, 0.01_wp], alignment=ALIGN_LEFT_BOTTOM, size=16.0_wp, &
                         color=[0.85_wp, 0.85_wp, 0.85_wp])
    call w1%add_hud_text(title1)
    call w2%add_hud_text(title2)
    call w1%add_hud_text(clock_text)                        ! (one HUD text, in both windows)
    call w2%add_hud_text(clock_text)
    call w1%set_time_range(times(1), times(n))
    call w1%set_time(times(1))
    call w1%set_time_scale(20000.0_wp)                     ! about 5.5 hours a second
    master => w1%clock()
    clock2 => w2%clock()
    if (.not. clock2%synchronize(master)) error stop 'could not synchronize the clocks'

    frame = 0
    running = .true.
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
