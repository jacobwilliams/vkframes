!> One scene seen from any of its frames, in a qt-fortran GUI: the Earth and
!> the Moon, each with an inertial and a rotating (body-fixed) frame, and a
!> spacecraft orbiting each, whose frames carry the attitudes the program
!> computed for them (LVLH: x along the velocity, z toward the body's center).
!> All the data is computed here (circular orbits, three days, a point a
!> minute) and only followed and drawn by vkframes: vkframes builds no frames
!> of its own.
!>
!> * ECI (the root): fixed.
!> * Earth-fixed: follows the Earth's rotation (attitudes only); the Earth
!>   (textured) turns with it.
!> * Earth orbiter: follows its orbit (400 km, 51.6 deg) and its LVLH
!>   attitudes.
!> * Moon-centered inertial: follows the Moon's positions (axes as ECI's).
!> * Moon-fixed: follows the Moon's positions and attitudes (tidally locked:
!>   x toward the Earth); the Moon turns with it.
!> * Lunar orbiter: its orbit (100 km, polar) computed in Moon-centered
!>   inertial coordinates, so it follows them under that frame, with its
!>   LVLH attitudes.
!> * A companion near each orbiter, on an orbit of the same size but slightly
!>   eccentric and inclined (Kepler's equation solved here), with its own
!>   LVLH frame: seen from its leader's frame it loops about it, the
!>   relative motion of formation flying (a 2:1 ellipse along-track and
!>   radial, and a cross-track swing; the Earth pair's about 136 by 68 by 59
!>   km, the lunar pair's 77 by 39 by 34 km).
!>
!> The dock's Frame menu shows the scene from the frame chosen (a View of
!> it: the camera rides with it, turning with it), with ticked axes at its
!> origin in place of its arrows (a CoordinateAxes moved to it: lines with
!> major and minor tick marks, sized to the frame: 15000 km for the Earth's,
!> 4000 km for the Moon's, 200 and 120 km for the spacecraft's). Trajectories:
!>
!> * "drawn in this frame": the trajectories (the orbiters', their
!>   companions' and the Moon's) move under the chosen frame, their data
!>   frames set to the frames they were computed in (DrawableTrajectory's
!>   set_data_frame): each point where it is in the chosen frame at the
!>   point's own time. In the Earth-fixed frame the Earth orbiter's orbit
!>   becomes its ground track's spiral; in the Moon-fixed frame the Moon's
!>   orbit shrinks to a point; in the orbiters' own frames their orbits are
!>   a point, their companions loop about them (relative motion), and
!>   everything else wheels around them.
!> * "as computed (only the view moves)": the trajectories stay in their own
!>   frames; only the camera is centered on (and turns with) the chosen frame.
!>
!> Either way each spacecraft rides on its curve: the curves drawn in a frame
!> and the frames following the data agree at the current time.
!>
!> Trails (on by default) draw the last orbit of each (the Moon's: the last
!> day); off, the whole three days. Pause stops the time (the camera still
!> moves), Resume starts it again.
!>
!> Run with `pixi run -e qt frames-demo` (from the vkframes root). Options:
!> `--frame k` (1 to 8) and `--mode m` (1: drawn in the frame, 2: as
!> computed) to start with, `--capture file`, `--capture-frame n`,
!> `--quit-after seconds`, and for tests `--frame-test`: every frame in both
!> modes, each spacecraft's distance from its curve (the curve's point at the
!> current time, as drawn, against the spacecraft's frame) printed; then
!> Pause and Resume clicked, the time stopped and running again.
module frames_demo

    use qt
    use vkframes
    use vkframes_qt, only: connect_view, ViewLink, DemoOptions
    use vkf_math, only: cross
    use iso_c_binding

    implicit none

    real(wp), parameter :: mu_earth = 398600.4418_wp, mu_moon = 4902.800_wp   !! km^3/s^2
    real(wp), parameter :: re = 6378.137_wp, rm = 1737.4_wp                    !! km
    real(wp), parameter :: moon_distance = 384400.0_wp                         !! km
    real(wp), parameter :: earth_rate = 7.2921159e-5_wp                        !! rad/s
    real(wp), parameter :: moon_rate = 2.0_wp * pi / (27.321661_wp * 86400.0_wp)   !! rad/s
    real(wp), parameter :: leo_radius = re + 400.0_wp, llo_radius = rm + 100.0_wp  !! km
    real(wp), parameter :: leo2_e = 0.01_wp, leo2_di = 0.5_wp, llo2_e = 0.02_wp, llo2_di = 1.0_wp  !! companions: e, deg
    !> the data's span, and its steps: a minute for the bodies' motion (the
    !> Moon's chords bow a metre; the Earth's spin is exact), 10 s for the
    !> spacecraft (a minute's chord of a low orbit bows 4 km inside it: kinks
    !> seen from close by; 10 s, 0.1 km)
    real(wp), parameter :: span = 3.0_wp * 86400.0_wp, step = 60.0_wp, craft_step = 10.0_wp   !! s
    integer, parameter :: n_points = nint(span / step) + 1, n_craft_points = nint(span / craft_step) + 1
    real(wp), parameter :: start_time = 12.0_wp * 3600.0_wp                    !! s
    integer, parameter :: n_frames = 8, n_paths = 5
    character(len=28), parameter :: frame_names(n_frames) = [character(len=28) :: &
        'ECI (Earth inertial)', 'Earth-fixed (rotating)', 'Earth orbiter (LVLH)', &
        'Moon-centered inertial', 'Moon-fixed (rotating)', 'Lunar orbiter (LVLH)', &
        'Earth companion (LVLH)', 'Lunar companion (LVLH)']
    character(len=20), parameter :: path_names(n_paths) = [character(len=20) :: &
        'Earth orbiter', 'Lunar orbiter', 'Moon', 'Earth companion', 'Lunar companion']
    character(len=40), parameter :: mode_names(2) = [character(len=40) :: &
        'Drawn in this frame', 'As computed (only the view moves)']
    real(wp), parameter :: leo_color(3) = [0.3_wp, 0.85_wp, 1.0_wp], llo_color(3) = [1.0_wp, 0.6_wp, 0.2_wp]
    real(wp), parameter :: moon_color(3) = [0.8_wp, 0.8_wp, 0.75_wp]
    real(wp), parameter :: leo2_color(3) = [0.75_wp, 0.5_wp, 1.0_wp], llo2_color(3) = [1.0_wp, 0.35_wp, 0.55_wp]
    !> each frame's default view: from this direction (x toward the viewer, y to the right, z up), this far (km)
    real(wp), parameter :: view_direction(3) = [0.92_wp, 0.28_wp, 0.27_wp] / norm2([0.92_wp, 0.28_wp, 0.27_wp])
    real(wp), parameter :: view_distance(n_frames) = [50000.0_wp, 50000.0_wp, 450.0_wp, 13000.0_wp, 13000.0_wp, &
                                                      260.0_wp, 450.0_wp, 260.0_wp]
    !> the ticked axes in each frame: length, major and minor tick spacings (km)
    real(wp), parameter :: tick_axes(3, n_frames) = reshape([ &
        15000.0_wp, 5000.0_wp, 1000.0_wp,   15000.0_wp, 5000.0_wp, 1000.0_wp,   200.0_wp, 50.0_wp, 10.0_wp, &
        4000.0_wp, 1000.0_wp, 250.0_wp,   4000.0_wp, 1000.0_wp, 250.0_wp,   120.0_wp, 25.0_wp, 5.0_wp, &
        200.0_wp, 50.0_wp, 10.0_wp,   120.0_wp, 25.0_wp, 5.0_wp], [3, n_frames])

    !> A frame pointer (for the frames' menu and the trajectories' parents).
    type :: FrameRef
        class(ReferenceFrame), pointer :: p => null()
    end type FrameRef

    !> Each frame: the time looped over the data's span.
    type, extends(WindowEventHandler) :: FramesHandler
    contains
        procedure :: before_update => frames_before_update
    end type FramesHandler

    !> The GUI and the scene.
    type :: app_t
        ! the window
        type(QMainWindow) :: main
        type(QWindow) :: window
        type(QWidget) :: container
        type(QtfEventFilter) :: events
        type(ViewLink) :: view_link                         !! the window's events to the proxy (connect_view)
        type(WindowProxy) :: proxy
        type(View) :: views(n_frames)
        integer :: frame = 0
        ! controls
        type(QComboBox) :: frame_choice, mode_choice
        type(QCheckBox) :: trails
        type(QDoubleSpinBox) :: time_scale
        type(QPushButton) :: pause                          !! Pause / Resume
        logical :: paused = .false.
        type(HUDText) :: hud_title, hud_time
        ! the scene: the frames
        type(ReferenceFrame) :: eci, ecef, moon_ci
        type(Sphere) :: earth, moon, leo_craft, llo_craft   !! (the Moon: its body-fixed frame; the crafts: theirs)
        type(Sphere) :: leo2_craft, llo2_craft              !! the companions
        type(FrameRef) :: frames(n_frames)
        type(CoordinateAxes) :: ticks                       !! ticked axes at the selected frame's origin
        type(FrameRef) :: ticks_in                          !! the frame they are in
        ! the data (computed here) and the trajectories drawn
        type(Trajectory) :: earth_spin, moon_eph, leo, llo, leo2, llo2
        type(CurveArtist) :: curves(n_paths)                !! the orbiters', the Moon's, the companions' (path_names)
        type(DrawableTrajectory) :: paths(n_paths)
        type(FrameRef) :: native(n_paths)                   !! the frames their data are in
        type(FrameRef) :: parent(n_paths)                   !! where each is now
        real(wp) :: trail_past(n_paths)                     !! each trail's length (s)
        type(SkySphere) :: sky
        type(FrameManager) :: scene
        ! options
        integer :: start_frame = 1, start_mode = 1
        type(DemoOptions) :: options                        !! --capture, --capture-frame, --quit-after
        logical :: frame_test = .false.
    end type app_t

    type(app_t), target, save :: app
    type(FramesHandler), target, save :: handler

contains

    ! ==================================================================
    ! the data

    !> Rotation about x by a.
    pure function rot_x(a) result(m)
        real(wp), intent(in) :: a
        real(wp) :: m(3, 3)
        m = reshape([1.0_wp, 0.0_wp, 0.0_wp, 0.0_wp, cos(a), sin(a), 0.0_wp, -sin(a), cos(a)], [3, 3])
    end function rot_x

    !> Rotation about z by a.
    pure function rot_z(a) result(m)
        real(wp), intent(in) :: a
        real(wp) :: m(3, 3)
        m = reshape([cos(a), sin(a), 0.0_wp, -sin(a), cos(a), 0.0_wp, 0.0_wp, 0.0_wp, 1.0_wp], [3, 3])
    end function rot_z

    !> A circular orbit of radius a and rate n in the plane turned by m, at
    !> angle u: the position and velocity.
    pure subroutine circular(m, a, n, u, r, v)
        real(wp), intent(in) :: m(3, 3), a, n, u
        real(wp), intent(out) :: r(3), v(3)
        r = matmul(m, a * [cos(u), sin(u), 0.0_wp])
        v = matmul(m, a * n * [-sin(u), cos(u), 0.0_wp])
    end subroutine circular

    !> An orbit of semi-major axis a, eccentricity e and mean motion n in the
    !> plane turned by m (periapsis on its x axis), at mean anomaly n t: the
    !> position and velocity (Kepler's equation solved by Newton's method).
    pure subroutine elliptic(m, a, e, n, t, r, v)
        real(wp), intent(in) :: m(3, 3), a, e, n, t
        real(wp), intent(out) :: r(3), v(3)
        real(wp) :: big_m, big_e, b
        integer :: k
        big_m = modulo(n * t, 2.0_wp * pi)
        big_e = big_m
        do k = 1, 20
            big_e = big_e - (big_e - e * sin(big_e) - big_m) / (1.0_wp - e * cos(big_e))
        end do
        b = sqrt(1.0_wp - e**2)
        r = matmul(m, a * [cos(big_e) - e, b * sin(big_e), 0.0_wp])
        v = matmul(m, n * a / (1.0_wp - e * cos(big_e)) * [-sin(big_e), b * cos(big_e), 0.0_wp])
    end subroutine elliptic

    !> The LVLH attitude of a spacecraft at r with velocity v (relative to the
    !> body it orbits): z toward the body, y against the orbit's angular
    !> momentum, x completing them (along the velocity, for a circular orbit).
    pure function lvlh(r, v) result(q)
        real(wp), intent(in) :: r(3), v(3)
        real(wp) :: q(4), m(3, 3)
        m(:, 3) = -r / norm2(r)
        m(:, 2) = -cross(r, v)
        m(:, 2) = m(:, 2) / norm2(m(:, 2))
        m(:, 1) = cross(m(:, 2), m(:, 3))
        q = quat_from_matrix(m)
    end function lvlh

    !> All the data: the Earth's rotation, the Moon's ephemeris (positions,
    !> and attitudes: x toward the Earth, z along its orbit's normal), the
    !> Earth orbiter's orbit and attitudes (ECI), and the lunar orbiter's
    !> (Moon-centered inertial); each companion's: the same size, slightly
    !> eccentric (periapsis where its leader starts) and more inclined.
    subroutine make_data()
        real(wp) :: t, r(3), v(3), m_moon(3, 3), m_leo(3, 3), m_llo(3, 3), axes(3, 3), n_leo, n_llo
        real(wp) :: m_leo2(3, 3), m_llo2(3, 3)
        integer :: k
        m_moon = rot_x(5.14_wp * pi / 180.0_wp)
        m_leo = matmul(rot_z(30.0_wp * pi / 180.0_wp), rot_x(51.6_wp * pi / 180.0_wp))
        m_llo = matmul(rot_z(20.0_wp * pi / 180.0_wp), rot_x(0.5_wp * pi))
        m_leo2 = matmul(rot_z(30.0_wp * pi / 180.0_wp), rot_x((51.6_wp + leo2_di) * pi / 180.0_wp))
        m_llo2 = matmul(rot_z(20.0_wp * pi / 180.0_wp), rot_x(0.5_wp * pi + llo2_di * pi / 180.0_wp))
        n_leo = sqrt(mu_earth / leo_radius**3)
        n_llo = sqrt(mu_moon / llo_radius**3)
        app%earth_spin = Trajectory()
        app%moon_eph = Trajectory()
        app%leo = Trajectory()
        app%llo = Trajectory()
        app%leo2 = Trajectory()
        app%llo2 = Trajectory()
        call app%earth_spin%reserve(n_points, attitudes=.true.)
        call app%moon_eph%reserve(n_points, attitudes=.true.)
        call app%leo%reserve(n_craft_points, attitudes=.true.)
        call app%llo%reserve(n_craft_points, attitudes=.true.)
        call app%leo2%reserve(n_craft_points, attitudes=.true.)
        call app%llo2%reserve(n_craft_points, attitudes=.true.)
        do k = 1, n_points
            t = (k - 1) * step
            call app%earth_spin%add(t, pos=[0.0_wp, 0.0_wp, 0.0_wp], &
                                    att=quat_from_axis_angle([0.0_wp, 0.0_wp, 1.0_wp], earth_rate * t))
            call circular(m_moon, moon_distance, moon_rate, moon_rate * t + pi / 3.0_wp, r, v)
            axes(:, 1) = -r / norm2(r)                      ! (tidally locked: x toward the Earth)
            axes(:, 3) = matmul(m_moon, [0.0_wp, 0.0_wp, 1.0_wp])
            axes(:, 2) = cross(axes(:, 3), axes(:, 1))
            call app%moon_eph%add(t, pos=r, att=quat_from_matrix(axes))
        end do
        do k = 1, n_craft_points
            t = (k - 1) * craft_step
            call circular(m_leo, leo_radius, n_leo, n_leo * t, r, v)
            call app%leo%add(t, pos=r, att=lvlh(r, v))
            call circular(m_llo, llo_radius, n_llo, n_llo * t, r, v)
            call app%llo%add(t, pos=r, att=lvlh(r, v))
            call elliptic(m_leo2, leo_radius, leo2_e, n_leo, t, r, v)
            call app%leo2%add(t, pos=r, att=lvlh(r, v))
            call elliptic(m_llo2, llo_radius, llo2_e, n_llo, t, r, v)
            call app%llo2%add(t, pos=r, att=lvlh(r, v))
        end do
        app%trail_past = [2.0_wp * pi / n_leo, 2.0_wp * pi / n_llo, 86400.0_wp, 2.0_wp * pi / n_leo, 2.0_wp * pi / n_llo]
    end subroutine make_data

    ! ==================================================================
    ! the scene

    !> The frames, the bodies, the spacecraft and the trajectories (each in
    !> the frame its data are in, at first).
    subroutine build_model()
        integer :: k
        call make_data()
        app%eci = ReferenceFrame('ECI', color=[0.6_wp, 0.6_wp, 0.65_wp, 0.8_wp])
        call axes_of(app%eci, 12000.0_wp)
        app%ecef = ReferenceFrame('Earth-fixed', color=[0.4_wp, 0.9_wp, 0.4_wp, 0.9_wp])
        call axes_of(app%ecef, 9000.0_wp)
        call app%ecef%follow(app%earth_spin, data=FOLLOW_ATTITUDE, mode=FOLLOW_LIMIT)
        app%earth = Sphere('Earth', radius=re)
        call app%earth%set_texture_map('2k_earth_daymap.jpg')
        call app%earth%show_decorations(.false.)

        app%moon_ci = ReferenceFrame('Moon-centered inertial', color=[0.6_wp, 0.6_wp, 0.65_wp, 0.8_wp])
        call axes_of(app%moon_ci, 3500.0_wp)
        call app%moon_ci%follow(app%moon_eph, data=FOLLOW_POSITION, mode=FOLLOW_LIMIT)
        app%moon = Sphere('Moon', radius=rm, color=moon_color)
        call app%moon%set_texture_map('lroc_color_2k.jpg')
        call axes_of(app%moon, 2600.0_wp)                   ! (the Moon-fixed frame's axes)
        call app%moon%set_color([0.4_wp, 0.9_wp, 0.4_wp, 0.9_wp])
        call app%moon%follow(app%moon_eph, mode=FOLLOW_LIMIT)

        ! the spacecraft: small (their companions loop tens of km away), their axes their LVLH attitudes
        call make_craft(app%leo_craft, 'Earth orbiter', leo_color, 4.0_wp, 40.0_wp, app%leo)
        call make_craft(app%llo_craft, 'Lunar orbiter', llo_color, 2.5_wp, 25.0_wp, app%llo)
        call make_craft(app%leo2_craft, 'Earth companion', leo2_color, 4.0_wp, 40.0_wp, app%leo2)
        call make_craft(app%llo2_craft, 'Lunar companion', llo2_color, 2.5_wp, 25.0_wp, app%llo2)

        ! the trajectories: the Earth orbiter's (ECI), the lunar orbiter's (Moon-centered), the Moon's (ECI)
        app%curves(1) = CurveArtist(app%leo, color=leo_color, width=2.0_wp)
        app%curves(2) = CurveArtist(app%llo, color=llo_color, width=2.0_wp)
        app%curves(3) = CurveArtist(app%moon_eph, color=moon_color, width=1.5_wp)
        app%curves(4) = CurveArtist(app%leo2, color=leo2_color, width=2.0_wp)
        app%curves(5) = CurveArtist(app%llo2, color=llo2_color, width=2.0_wp)
        app%native(1)%p => app%eci
        app%native(2)%p => app%moon_ci
        app%native(3)%p => app%eci
        app%native(4)%p => app%eci
        app%native(5)%p => app%moon_ci
        do k = 1, n_paths
            app%paths(k) = DrawableTrajectory(trim(path_names(k)))
            call app%paths(k)%show_decorations(.false.)
            call app%paths(k)%add_artist(app%curves(k))
            app%parent(k)%p => app%native(k)%p
            if (.not. app%parent(k)%p%add_child(app%paths(k))) error stop 'frames demo: a trajectory'
        end do
        call apply_trails(.true.)

        if (.not. (app%eci%add_child(app%ecef) .and. app%ecef%add_child(app%earth) .and. &
                   app%eci%add_child(app%moon_ci) .and. app%eci%add_child(app%moon) .and. &
                   app%eci%add_child(app%leo_craft) .and. app%moon_ci%add_child(app%llo_craft) .and. &
                   app%eci%add_child(app%leo2_craft) .and. app%moon_ci%add_child(app%llo2_craft))) &
            error stop 'frames demo: could not build the scene'
        app%frames(1)%p => app%eci
        app%frames(2)%p => app%ecef
        app%frames(3)%p => app%leo_craft
        app%frames(4)%p => app%moon_ci
        app%frames(5)%p => app%moon
        app%frames(6)%p => app%llo_craft
        app%frames(7)%p => app%leo2_craft
        app%frames(8)%p => app%llo2_craft
        ! the ticked axes (lines and tick marks, no arrows), in the selected frame
        app%ticks = CoordinateAxes('Ticks', color=[1.0_wp, 0.92_wp, 0.55_wp, 0.95_wp])
        call app%ticks%show_axes(NO_AXES)
        call app%ticks%show_name_label(.false.)
        call app%ticks%set_label_size(14)
        call app%ticks%set_axis_width(1.5_wp)
        call app%ticks%set_pickable(.false.)
        app%ticks_in%p => app%eci
        if (.not. app%eci%add_child(app%ticks)) error stop 'frames demo: the ticked axes'
        app%scene = FrameManager(app%eci)

        app%sky = SkySphere()
        if (.not. app%sky%set_star_data('Stars_HYGv3.txt', min_mag=-2.0_wp, max_mag=6.0_wp, max_stars=20000)) &
            write(*, '(a)') 'frames demo: no star catalog'
    end subroutine build_model

    !> A spacecraft: a small sphere following its trajectory (position and
    !> attitude), its axes (its attitude) `length` long.
    subroutine make_craft(craft, name, color, radius, length, traj)
        type(Sphere), intent(inout) :: craft
        character(len=*), intent(in) :: name
        real(wp), intent(in) :: color(3), radius, length    !! (km)
        type(Trajectory), intent(in), target :: traj        !! its data (positions and attitudes)
        craft = Sphere(name, radius=radius, color=color)
        call axes_of(craft, length)
        call craft%set_color([color, 1.0_wp])
        call craft%follow(traj, mode=FOLLOW_LIMIT)
    end subroutine make_craft

    !> A frame's axes, `length` long, with small labels.
    subroutine axes_of(f, length)
        class(ReferenceFrame), intent(inout) :: f
        real(wp), intent(in) :: length                      !! (km)
        call f%move_x_axis([0.0_wp, 0.0_wp, 0.0_wp], length, 0.15_wp, 0.008_wp * length, 0.03_wp * length)
        call f%move_y_axis([0.0_wp, 0.0_wp, 0.0_wp], length, 0.15_wp, 0.008_wp * length, 0.03_wp * length)
        call f%move_z_axis([0.0_wp, 0.0_wp, 0.0_wp], length, 0.15_wp, 0.008_wp * length, 0.03_wp * length)
        call f%set_label_size(13)
    end subroutine axes_of

    !> The trajectories drawn in frame `k` (mode 1: moved under it, their
    !> data frames the frames they were computed in), or in their own frames
    !> (mode 2).
    subroutine apply_mode(k, mode)
        integer, intent(in) :: k, mode                      !! the frame (1 to n_frames); 1: drawn in it, 2: as computed
        class(ReferenceFrame), pointer :: target_frame
        integer :: j
        do j = 1, n_paths
            if (mode == 1) then
                target_frame => app%frames(k)%p
            else
                target_frame => app%native(j)%p
            end if
            if (.not. associated(target_frame, app%parent(j)%p)) then
                if (.not. app%parent(j)%p%remove_child(app%paths(j))) write(*, '(a)') 'frames demo: could not move a trajectory'
                if (.not. target_frame%add_child(app%paths(j))) write(*, '(a)') 'frames demo: could not move a trajectory'
                app%parent(j)%p => target_frame
            end if
            if (associated(target_frame, app%native(j)%p)) then
                call app%paths(j)%set_data_frame()          ! (in its own frame: as it is)
            else
                call app%paths(j)%set_data_frame(app%native(j)%p)
            end if
        end do
        call place_ticks(k)
        call app%hud_title%set_text(trim(frame_names(k))//':  '//trim(merge('trajectories drawn in this frame', &
                                    'trajectories as computed        ', mode == 1)))
    end subroutine apply_mode

    !> The ticked axes moved to frame k (sized to it: tick_axes), in place of
    !> its arrows (the other frames' shown).
    subroutine place_ticks(k)
        integer, intent(in) :: k                            !! the frame (1 to n_frames)
        real(wp) :: length
        integer :: j
        if (.not. associated(app%frames(k)%p, app%ticks_in%p)) then
            if (.not. (app%ticks_in%p%remove_child(app%ticks) .and. app%frames(k)%p%add_child(app%ticks))) &
                write(*, '(a)') 'frames demo: could not move the ticked axes'
            app%ticks_in%p => app%frames(k)%p
        end if
        length = tick_axes(1, k)
        call app%ticks%set_axis_length(length)
        call app%ticks%set_tick_spacing(tick_axes(2, k), tick_axes(3, k))
        call app%ticks%move_x_axis([0.0_wp, 0.0_wp, 0.0_wp], 1.04_wp * length)   ! (its labels just past the ends)
        call app%ticks%move_y_axis([0.0_wp, 0.0_wp, 0.0_wp], 1.04_wp * length)
        call app%ticks%move_z_axis([0.0_wp, 0.0_wp, 0.0_wp], 1.04_wp * length)
        do j = 1, n_frames                                  ! (the arrows of the frame with the ticks hidden)
            call app%frames(j)%p%show_axes(merge(NO_AXES, ALL_AXES, j == k))
            call app%frames(j)%p%show_axes_labels(merge(NO_AXES, ALL_AXES, j == k))
        end do
    end subroutine place_ticks

    !> Trails (each the last orbit; the Moon's the last day), or the whole span.
    subroutine apply_trails(on)
        logical, intent(in) :: on
        integer :: j
        do j = 1, n_paths
            if (on) then
                call app%curves(j)%set_trail(app%trail_past(j), 0.0_wp, fade=0.5_wp)
            else
                call app%curves(j)%remove_trail()
            end if
        end do
    end subroutine apply_trails

    ! ==================================================================
    ! the frames

    !> Before the scene's update: the time looped over the data's span.
    subroutine frames_before_update(self, window, t)
        class(FramesHandler), intent(inout) :: self
        class(WindowProxy), intent(inout) :: window         !! the window about to update
        real(wp), intent(in) :: t                           !! simulation time (s)
        if (t >= span .or. t < 0.0_wp) call window%set_time(0.0_wp)   ! (from the next frame)
    end subroutine frames_before_update

    !> The views (one per frame, the camera riding with it), the HUD texts,
    !> and the QWindow the scene is drawn in.
    subroutine build_window()
        integer :: k
        do k = 1, n_frames
            app%views(k) = View(app%eci, app%frames(k)%p)
        end do
        ! every frame seen the same way at first: x toward the viewer, y to the
        ! right, z up (the eye mostly along +x, raised a little and turned a
        ! little toward +y so that all three axes show), at a distance for the
        ! frame's scale
        do k = 1, n_frames
            call app%views(k)%set_default_view_parameters(eye=view_distance(k) * view_direction, &
                center=[0.0_wp, 0.0_wp, 0.0_wp], up=[0.0_wp, 0.0_wp, 1.0_wp])
        end do
        app%hud_title = HUDText('', position=[0.01_wp, 0.99_wp], alignment=ALIGN_LEFT_TOP, size=17.0_wp, &
                                color=[0.95_wp, 0.95_wp, 0.95_wp])
        app%hud_time = HUDText('', position=[0.01_wp, 0.01_wp], alignment=ALIGN_LEFT_BOTTOM, size=15.0_wp, &
                               color=[0.8_wp, 0.85_wp, 1.0_wp])
        app%window = QWindow()
        app%container = QWidget_createWindowContainer(app%window)
        call app%container%setMinimumSize(320, 240)
        app%events = QtfEventFilter(app%window)
        call app%events%exposeEvent%connect(on_expose)
        call app%events%updateRequest%connect(on_update)
        call connect_view(app%view_link, app%events, app%proxy, app%window)   ! (mouse, keys, resizes, the surface)
    end subroutine build_window

    !> Open the view (on the window's first expose) and show the scene.
    subroutine open_view()
        integer :: k
        if (.not. app%proxy%open(app%window%width(), app%window%height(), 'vkframes frames', &
                                 native_window=app%window%winId(), pixel_ratio=app%window%devicePixelRatio())) then
            write(*, '(a)') 'frames demo: could not open the view'
            return
        end if
        if (app%proxy%num_views() > 0) return                    ! (opened again)
        call app%proxy%set_scene(app%scene)
        call app%proxy%set_event_handler(handler)
        call app%proxy%set_background_color([0.0_wp, 0.0_wp, 0.0_wp])
        call app%proxy%set_sky(app%sky)
        do k = 1, n_frames
            call app%proxy%add_view(app%views(k))
        end do
        call app%proxy%set_view_transition(1.0_wp)
        call app%proxy%select_view(app%frame_choice%currentIndex() + 1)
        call app%proxy%add_hud_text(app%hud_title)
        call app%proxy%add_hud_text(app%hud_time)
        call app%proxy%set_time_scale(app%time_scale%value())
        call app%proxy%set_time(start_time)                 ! (trails: an orbit already flown)
        call app%proxy%pause_time(app%paused)
    end subroutine open_view

    !> The window shown: the view opened (the first time), the frames started.
    subroutine on_expose(data)
        class(*), intent(inout), optional :: data           !! the connection's data (unused)
        if (app%proxy%is_open() .or. .not. app%window%isExposed()) return
        call open_view()
        if (app%proxy%is_open()) call app%window%requestUpdate()
    end subroutine on_expose

    !> Render a frame, ask for the next one.
    subroutine on_update(data)
        class(*), intent(inout), optional :: data           !! the connection's data (unused)
        character(len=64) :: text
        if (app%window%devicePixelRatio() /= app%proxy%pixel_ratio()) &
            call app%proxy%resize(nint(app%window%width() * app%window%devicePixelRatio()), &
                                  nint(app%window%height() * app%window%devicePixelRatio()), &
                                  pixel_ratio=app%window%devicePixelRatio())
        write(text, '(a,f6.2,a)') 't = ', app%proxy%time() / 3600.0_wp, ' h'
        call app%hud_time%set_text(trim(text))
        if (.not. app%proxy%frame()) return
        app%frame = app%frame + 1
        if (app%frame_test) call frame_test_step()
        if (app%options%after_frame(app%proxy, app%window, app%frame)) return   ! (a capture; --recreate-surface)
        call app%window%requestUpdate()
    end subroutine on_update

    !> --frame-test: each frame in both modes (one every 6 frames), and each
    !> spacecraft's distance from its curve printed: the curve's point at
    !> the current time (the data point at or before it, as drawn: in its
    !> drawable's frame, to the root) against the spacecraft's frame at that
    !> point's time.
    subroutine frame_test_step()
        integer :: step_no, k, mode
        real(wp) :: off(4)
        real(wp), save :: paused_at = 0.0_wp
        if (app%frame < 10) return
        step_no = (app%frame - 10) / 6
        if (step_no >= 2 * n_frames) then                  ! then Pause (8 frames), Resume (8 frames)
            select case (app%frame - (10 + 2 * n_frames * 6))
            case (0)
                call app%pause%click()
            case (2)
                paused_at = app%proxy%time()
            case (8)
                write(*, '(a,l1,a,l1)') 'frame test: paused ', app%proxy%is_time_paused(), &
                    ', the time stopped ', app%proxy%time() == paused_at
                call app%pause%click()
            case (16)
                write(*, '(a,l1,a,l1)') 'frame test: resumed ', .not. app%proxy%is_time_paused(), &
                    ', the time running ', app%proxy%time() > paused_at
                write(*, '(a)') 'frame test: done'
                call app%main%close()
            end select
            return
        end if
        k = mod(step_no, n_frames) + 1
        mode = step_no / n_frames + 1
        if (mod(app%frame - 10, 6) == 0) then
            call app%mode_choice%setCurrentIndex(mode - 1)
            call app%frame_choice%setCurrentIndex(k - 1)
        else if (mod(app%frame - 10, 6) == 4) then
            off = [off_curve(1, app%leo_craft), off_curve(2, app%llo_craft), off_curve(4, app%leo2_craft), &
                   off_curve(5, app%llo2_craft)]
            write(*, '(a,a,a,i0,a,es9.2,a)') 'frame test: ', frame_names(k), ' mode ', mode, &
                ': the spacecraft off their curves at most ', maxval(off), ' km'
            if (k == 3 .or. k == 6) write(*, '(a,f8.1,a)') 'frame test: the companion from its leader: ', &
                norm2(merge(app%leo2_craft%position() - app%leo_craft%position(), &
                            app%llo2_craft%position() - app%llo_craft%position(), k == 3)), ' km'
        end if
    end subroutine frame_test_step

    !> How far a spacecraft is from its curve's point at the current time
    !> (the data point at or before it): the curve's point as drawn, taken
    !> to the root frame by its drawable's frame at the point's time, against
    !> the spacecraft's frame at that time (in either mode, both are where
    !> the data put the spacecraft then).
    real(wp) function off_curve(j, craft) result(d)
        integer, intent(in) :: j                            !! the trajectory
        class(ReferenceFrame), intent(in) :: craft          !! the spacecraft following its data
        type(Trajectory), pointer :: traj
        real(wp) :: m(4, 4), c(4, 4), p(3), ti
        integer :: i
        traj => app%curves(j)%get_trajectory()
        i = max(1, min(traj%num_times(), int(app%proxy%time() / (traj%time(2) - traj%time(1))) + 1))
        ti = traj%time(i)
        m = app%paths(j)%world_matrix_at(ti)
        p = app%curves(j)%drawn_point(i)
        p = matmul(m(1:3, 1:3), p) + m(1:3, 4)
        c = craft%world_matrix_at(ti)
        d = norm2(p - c(1:3, 4))
    end function off_curve

    ! ==================================================================
    ! controls

    !> A frame chosen: its view (flown to).
    subroutine on_frame(index, data)
        integer, intent(in) :: index                        !! the frame (0-based)
        class(*), intent(inout), optional :: data           !! the connection's data (unused)
        call apply_mode(index + 1, app%mode_choice%currentIndex() + 1)
        if (app%proxy%is_open()) call app%proxy%select_view(index + 1)
    end subroutine on_frame

    !> The trajectories drawn in the frame, or as computed.
    subroutine on_mode(index, data)
        integer, intent(in) :: index                        !! the mode (0-based)
        class(*), intent(inout), optional :: data           !! the connection's data (unused)
        call apply_mode(app%frame_choice%currentIndex() + 1, index + 1)
    end subroutine on_mode

    subroutine on_trails(checked, data)
        logical, intent(in) :: checked                      !! trails on
        class(*), intent(inout), optional :: data           !! the connection's data (unused)
        call apply_trails(checked)
    end subroutine on_trails

    subroutine on_time_scale(value, data)
        real(c_double), intent(in) :: value                 !! simulation seconds per second
        class(*), intent(inout), optional :: data           !! the connection's data (unused)
        if (app%proxy%is_open()) call app%proxy%set_time_scale(real(value, wp))
    end subroutine on_time_scale

    !> The time paused, or resumed (the button's text the other).
    subroutine on_pause(data)
        class(*), intent(inout), optional :: data           !! the connection's data (unused)
        app%paused = .not. app%paused
        if (app%proxy%is_open()) call app%proxy%pause_time(app%paused)
        call app%pause%setText(trim(merge('Resume', 'Pause ', app%paused)))
    end subroutine on_pause

    !> The view's camera reset.
    subroutine on_reset_view(data)
        class(*), intent(inout), optional :: data           !! the connection's data (unused)
        type(View), pointer :: v
        v => app%proxy%current_view()
        call v%reset_view()
    end subroutine on_reset_view

    !> The main window closes: close the view (waiting for the GPU) first.
    subroutine on_close(event, data)
        type(QCloseEvent), intent(in) :: event              !! the close (accepted)
        class(*), intent(inout), optional :: data           !! the connection's data (unused)
        call app%proxy%close()
    end subroutine on_close

    !> The main window: the scene in the middle, the dock (the frame, how the
    !> trajectories are drawn, trails, the time scale) on the left.
    subroutine build_ui()
        type(QDockWidget) :: dock
        type(QWidget) :: panel
        type(QFormLayout) :: form
        type(QPushButton) :: reset_view
        type(QHBoxLayout) :: buttons
        type(QLabel) :: help

        app%main = QMainWindow()
        call app%main%setWindowTitle('vkframes: one scene, seen from any of its frames')
        call app%main%closeEvent%connect(on_close)
        call build_window()
        call app%main%setCentralWidget(app%container)

        panel = QWidget()
        form = QFormLayout(panel)
        app%frame_choice = QComboBox()
        call app%frame_choice%addItems(frame_names)
        call app%frame_choice%setCurrentIndex(app%start_frame - 1)
        call app%frame_choice%currentIndexChanged%connect(on_frame)
        call form%addRow('Frame', app%frame_choice)
        app%mode_choice = QComboBox()
        call app%mode_choice%addItems(mode_names)
        call app%mode_choice%setCurrentIndex(app%start_mode - 1)
        call app%mode_choice%currentIndexChanged%connect(on_mode)
        call form%addRow('Trajectories', app%mode_choice)
        app%trails = QCheckBox('Trails (the last orbit; the Moon''s last day)')
        call app%trails%setChecked(.true.)
        call app%trails%toggled%connect(on_trails)
        call form%addRow('', app%trails)
        app%time_scale = QDoubleSpinBox()
        call app%time_scale%setRange(0.0_c_double, 100000.0_c_double)
        call app%time_scale%setSingleStep(100.0_c_double)
        call app%time_scale%setDecimals(0)
        call app%time_scale%setValue(600.0_c_double)
        call app%time_scale%setSuffix(' x')
        call app%time_scale%valueChanged%connect(on_time_scale)
        call form%addRow('Time scale', app%time_scale)
        app%pause = QPushButton('Pause')
        call app%pause%clicked%connect(on_pause)
        reset_view = QPushButton('Reset view')
        call reset_view%clicked%connect(on_reset_view)
        buttons = QHBoxLayout()
        call buttons%addWidget(app%pause)
        call buttons%addWidget(reset_view)
        call form%addRow('', buttons)
        help = QLabel('<small>The camera rides with the frame chosen (turning with it). "Drawn in ' // &
                      'this frame": each trajectory is drawn where its points are in that frame at their ' // &
                      'own times (in the Earth-fixed frame the orbiter''s orbit becomes its ground ' // &
                      'track''s spiral; in its own frame, a point, its companion looping about it). ' // &
                      '"As computed": the trajectories ' // &
                      'stay in the frames they were computed in; only the view moves. Each ' // &
                      'spacecraft (its axes: its LVLH attitude) rides on its curve either way. ' // &
                      'Left drag rotates, right drag zooms, middle drag pans; space resets the view.</small>')
        call help%setWordWrap(.true.)
        call form%addRow(help)
        call panel%setMinimumWidth(260)
        dock = QDockWidget('Frames', app%main)
        call dock%setWidget(panel)
        call app%main%addDockWidget(Qt_LeftDockWidgetArea, dock)
        call apply_mode(app%start_frame, app%start_mode)
    end subroutine build_ui

end module frames_demo

!> The frames demo (see the module documentation).
program vkframes_frames_demo

    use qt
    use frames_demo
    implicit none

    type(QApplication) :: qapp
    character(len=1024) :: arg
    integer :: i, ierr

    qapp = QApplication()
    i = 1
    do while (i <= command_argument_count())
        if (app%options%take(i)) cycle                      ! (--capture, --quit-after, ...: vkframes_qt)
        call get_command_argument(i, arg)
        select case (arg)
        case ('--frame')
            call get_command_argument(i + 1, arg)
            read(arg, *) app%start_frame
            app%start_frame = max(1, min(n_frames, app%start_frame))
            i = i + 1
        case ('--mode')
            call get_command_argument(i + 1, arg)
            read(arg, *) app%start_mode
            app%start_mode = max(1, min(2, app%start_mode))
            i = i + 1
        case ('--frame-test')
            app%frame_test = .true.
        end select
        i = i + 1
    end do

    call build_model()
    call build_ui()
    call app%options%start_quit_timer(app%main)
    call app%main%resize(1400, 860)
    call app%main%show()
    ierr = qapp%exec()

end program vkframes_frames_demo
