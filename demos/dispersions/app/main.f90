!> Monte Carlo dispersions of a transfer burn, in a qt-fortran GUI: a
!> spacecraft in a 300 km circular orbit burns into a transfer orbit to
!> geostationary altitude, with execution errors in the burn (its size and
!> pointing) and errors in knowing its state (position and velocity). Many
!> samples are drawn from those errors, each propagated (two-body, exact:
!> kepler in vkframes_qt) to the nominal transfer's apoapsis, and shown
!> together:
!>
!> * each sample's trajectory, faint, colored by how far it ends from the
!>   nominal (blue: near, red: far);
!> * the nominal trajectory (white), and the orbit before the burn (dashed);
!> * the cloud of the samples' positions at the current time (dots), and its
!>   3-sigma ellipsoid (translucent): from the cloud's covariance, its axes
!>   the eigenvectors, their lengths three times the square roots of the
!>   eigenvalues; computed again every frame, so as time runs the cloud
!>   spreads and the ellipsoid stretches and turns.
!>
!> With "Animate", the trajectories are traced up to the current time (trace
!> mode) and the time loops over the transfer; without it, they are drawn
!> whole and the cloud is at the end. The HUD gives the cloud's spread
!> (1-sigma, radial, in-track and cross-track from the nominal) and the
!> ellipsoid's semi-axes. The dock: the number of samples, the errors' 1-sigma
!> sizes, the random seed (the same seed: the same samples), what is shown,
!> and the view (the whole transfer, or following the cloud's center).
!>
!> Run with `pixi run -e qt dispersions-demo` (from the vkframes root).
!> Options: `--samples n`, `--capture file` saves frame `--capture-frame n`
!> (default 120), `--quit-after seconds`, and, for tests, `--still` (not
!> animated: the cloud at the end) with the statistics printed.
module dispersions_demo

    use qt
    use vkframes
    use vkframes_qt, only: connect_view, ViewLink, DemoOptions
    use kepler
    use iso_c_binding

    implicit none

    integer, parameter :: max_samples = 2000                !! the most samples (their trajectories are made once)
    integer, parameter :: n_points = 240                    !! points along each sample
    real(wp), parameter :: r_leo = earth_radius + 300.0_wp  !! the orbit before the burn: circular (km)
    real(wp), parameter :: r_geo = 42164.0_wp               !! the transfer's apoapsis (km)
    real(wp), parameter :: inclination = 28.5_wp            !! deg
    integer, parameter :: n_views = 2
    character(len=20), parameter :: view_names(n_views) = [character(len=20) :: 'The transfer', 'Following the cloud']
    real(wp), parameter :: nominal_color(4) = [1.0_wp, 1.0_wp, 1.0_wp, 0.95_wp]
    real(wp), parameter :: ellipsoid_color(3) = [1.0_wp, 0.85_wp, 0.3_wp]

    !> Each frame, just before drawing: the cloud and its ellipsoid at the
    !> frame's time; the time looped.
    type, extends(WindowEventHandler) :: CloudHandler
    contains
        procedure :: before_draw => cloud_before_draw
    end type CloudHandler

    !> The errors' 1-sigma sizes.
    type :: Errors
        real(wp) :: dv_size = 0.5_wp                        !! the burn's size (% of it)
        real(wp) :: dv_pointing = 0.3_wp                    !! the burn's direction (deg)
        real(wp) :: position = 0.5_wp                       !! the position known (km, each axis)
        real(wp) :: velocity = 0.5_wp                       !! the velocity known (m/s, each axis)
    end type Errors

    !> The GUI and the scene.
    type :: app_t
        ! the window
        type(QMainWindow) :: main
        type(QWindow) :: window
        type(QWidget) :: container
        type(QtfEventFilter) :: events
        type(ViewLink) :: view_link                         !! the window's events to the proxy (connect_view)
        type(WindowProxy) :: proxy
        type(View) :: views(n_views)
        integer :: frame = 0
        ! controls
        type(QSpinBox) :: samples_box, seed_box
        type(QDoubleSpinBox) :: dv_size_box, dv_pointing_box, position_box, velocity_box
        type(QCheckBox) :: animate, show_samples, show_cloud, show_ellipsoid, show_nominal
        type(QComboBox) :: view_choice
        type(QLabel) :: stats_label
        type(HUDText) :: hud_title, hud_stats
        ! the scene
        type(ReferenceFrame) :: root                        !! Earth-centered inertial (km)
        type(Sphere) :: earth
        type(Trajectory), allocatable :: samples(:)         !! (max_samples): the samples' trajectories (n_samples used)
        type(CurveArtist), allocatable :: sample_curves(:)  !! (max_samples)
        type(DrawableTrajectory) :: samples_frame
        type(Trajectory) :: nominal, initial, cloud
        type(CurveArtist) :: nominal_curve, initial_curve
        type(MarkerArtist) :: cloud_dots
        type(DrawableTrajectory) :: nominal_frame, cloud_frame
        type(Sphere) :: ellipsoid                           !! the cloud's 3-sigma ellipsoid (its frame at the cloud's center)
        type(SkySphere) :: sky
        type(FrameManager) :: scene
        ! the dispersions
        integer :: n_samples = 400
        type(Errors) :: err
        real(wp) :: r0(3), v0(3), dv0(3)                    !! the nominal state at the burn, and the burn (km, km/s)
        real(wp) :: t_end                                   !! the transfer's apoapsis (s after the burn)
        real(wp) :: spread(3) = 0.0_wp                      !! the cloud's 1-sigma radial, in-track, cross-track spread (km)
        real(wp) :: axes(3) = 0.0_wp                        !! the ellipsoid's semi-axes (km, largest first)
        ! options
        integer :: start_view = 1
        type(DemoOptions) :: options                        !! --capture, --capture-frame, --quit-after
        logical :: still = .false.                          !! --still: not animated, the statistics printed
    end type app_t

    type(app_t), target, save :: app
    type(CloudHandler), target, save :: handler

contains

    ! ==================================================================
    ! the scene

    !> The scene: the Earth, the samples' trajectories (made once, for the
    !> most samples), the nominal and initial orbits, the cloud and its
    !> ellipsoid, and the sky.
    subroutine build_model()
        integer :: k
        app%root = ReferenceFrame('ECI')
        call app%root%show_decorations(.false.)
        app%earth = Sphere('Earth', radius=earth_radius)
        call app%earth%set_texture_map('2k_earth_daymap.jpg')
        call app%earth%show_decorations(.false.)

        allocate(app%samples(max_samples), app%sample_curves(max_samples))
        app%samples_frame = DrawableTrajectory('Samples')
        call app%samples_frame%show_decorations(.false.)
        do k = 1, max_samples
            app%samples(k) = Trajectory()
            call app%samples(k)%reserve(n_points)
            app%sample_curves(k) = CurveArtist(app%samples(k), width=1.0_wp)
            call app%sample_curves(k)%set_pickable(.false.)
            call app%samples_frame%add_artist(app%sample_curves(k))
        end do

        app%nominal = Trajectory()
        app%initial = Trajectory()
        app%nominal_curve = CurveArtist(app%nominal, color=nominal_color, width=2.5_wp)
        app%initial_curve = CurveArtist(app%initial, color=[0.7_wp, 0.7_wp, 0.7_wp, 0.6_wp], width=1.5_wp)
        call app%initial_curve%set_pattern(4, int(z'00FF'))
        app%nominal_frame = DrawableTrajectory('Nominal')
        call app%nominal_frame%show_decorations(.false.)
        call app%nominal_frame%add_artist(app%initial_curve)
        call app%nominal_frame%add_artist(app%nominal_curve)

        ! the cloud: the samples' positions now, as dots
        app%cloud = Trajectory()
        call app%cloud%reserve(max_samples)
        app%cloud_dots = MarkerArtist(app%cloud, markers=ior(ior(MARKER_START, MARKER_END), MARKER_INTERMEDIATE), &
                                      size=4.0_wp, color=[1.0_wp, 1.0_wp, 0.85_wp])
        call app%cloud_dots%set_intermediate_spacing(1.0_wp)
        call app%cloud_dots%set_auto_attenuate(.false.)
        app%cloud_frame = DrawableTrajectory('Cloud')
        call app%cloud_frame%show_decorations(.false.)
        call app%cloud_frame%add_artist(app%cloud_dots)

        ! the 3-sigma ellipsoid: a unit sphere, scaled and turned to the covariance's axes
        app%ellipsoid = Sphere('3-sigma', radius=1.0_wp, color=ellipsoid_color)
        call app%ellipsoid%show_decorations(.false.)
        call app%ellipsoid%set_opacity(0.3_wp)
        call app%ellipsoid%set_pickable(.false.)

        if (.not. (app%root%add_child(app%earth) .and. app%root%add_child(app%samples_frame) .and. &
                   app%root%add_child(app%nominal_frame) .and. app%root%add_child(app%cloud_frame) .and. &
                   app%root%add_child(app%ellipsoid))) error stop 'dispersions demo: could not build the scene'
        app%scene = FrameManager(app%root)

        app%sky = SkySphere()
        if (.not. app%sky%set_star_data('Stars_HYGv3.txt', min_mag=-2.0_wp, max_mag=6.0_wp, max_stars=20000)) &
            write(*, '(a)') 'dispersions demo: no star catalog'
    end subroutine build_model

    !> The nominal: the state at the burn (on the circular orbit, at its
    !> ascending node), the burn (prograde, to reach r_geo at apoapsis), the
    !> transfer to apoapsis, and the orbit before the burn.
    subroutine build_nominal()
        real(wp) :: inc, vc, dv, t(n_points), r(3, n_points), a
        integer :: n
        logical :: cut0, cut1
        inc = inclination * pi / 180.0_wp
        vc = sqrt(mu / r_leo)
        app%r0 = [r_leo, 0.0_wp, 0.0_wp]
        app%v0 = vc * [0.0_wp, cos(inc), sin(inc)]
        dv = vc * (sqrt(2.0_wp * r_geo / (r_leo + r_geo)) - 1.0_wp)
        app%dv0 = dv * app%v0 / vc
        a = 0.5_wp * (r_leo + r_geo)
        app%t_end = pi * sqrt(a**3 / mu)                     ! (half the transfer's period: its apoapsis)
        n = n_points
        call propagate(app%r0, app%v0 + app%dv0, 0.0_wp, app%t_end, n, t, r, cut0, cut1)
        call app%nominal%clear()
        call app%nominal%add(t(1:n), pos=r(:, 1:n))
        n = n_points
        call propagate(app%r0, app%v0, 0.0_wp, 2.0_wp * pi * sqrt(r_leo**3 / mu), n, t, r, cut0, cut1)
        call app%initial%clear()
        call app%initial%add(t(1:n), pos=r(:, 1:n))
    end subroutine build_nominal

    !> The samples drawn again (from the seed) and propagated, colored by how
    !> far each ends from the nominal; the ones beyond n_samples emptied.
    subroutine resample()
        real(wp) :: r(3), v(3), dv(3), g(3), u(3), e1(3), e2(3), size, t(n_points), p(3, n_points)
        real(wp) :: miss(max_samples), end_nominal(3), worst, f
        integer :: k, n
        logical :: cut0, cut1
        call seed_random(app%seed_box%value())
        u = app%dv0 / norm2(app%dv0)
        e1 = perpendicular(u)
        e2 = cross(u, e1)
        end_nominal = app%nominal%point_at(app%t_end)
        do k = 1, app%n_samples
            ! the state known with errors; the burn's size and direction executed with errors
            call gauss3(g)
            r = app%r0 + app%err%position * g
            call gauss3(g)
            v = app%v0 + 1.0e-3_wp * app%err%velocity * g
            call gauss3(g)
            size = norm2(app%dv0) * (1.0_wp + 0.01_wp * app%err%dv_size * g(1))
            dv = u + (app%err%dv_pointing * pi / 180.0_wp) * (g(2) * e1 + g(3) * e2)
            dv = size * dv / norm2(dv)
            n = n_points
            call propagate(r, v + dv, 0.0_wp, app%t_end, n, t, p, cut0, cut1)
            call app%samples(k)%clear()
            call app%samples(k)%add(t(1:n), pos=p(:, 1:n))
            miss(k) = norm2(p(:, n) - end_nominal)
        end do
        do k = app%n_samples + 1, max_samples
            if (app%samples(k)%num_times() > 0) call app%samples(k)%clear()
        end do
        ! colors: blue (near the nominal's end) to red (the farthest)
        worst = max(maxval(miss(1:app%n_samples)), tiny(1.0_wp))
        do k = 1, app%n_samples
            f = miss(k) / worst
            call app%sample_curves(k)%set_color([0.25_wp + 0.75_wp * f, 0.55_wp * (1.0_wp - f) + 0.25_wp, &
                                                 1.0_wp - 0.8_wp * f, 0.35_wp])
        end do
        call place_cloud(app%proxy%time())
    end subroutine resample

    !> The cloud at time t (each sample's position then), its spread from
    !> the nominal (radial, in-track, cross-track), and its 3-sigma ellipsoid.
    subroutine place_cloud(t)
        real(wp), intent(in) :: t                           !! the time (s after the burn)
        real(wp) :: p(3, max_samples), mean(3), d(3), cov(3, 3), vecs(3, 3), vals(3), nom(3), vel(3), ric(3, 3), tt
        integer :: k, n
        n = app%n_samples
        tt = max(0.0_wp, min(app%t_end, t))
        call app%cloud%clear()
        do k = 1, n
            p(:, k) = app%samples(k)%point_at(tt)
            call app%cloud%add(real(k, wp), pos=p(:, k))
        end do
        mean = sum(p(:, 1:n), dim=2) / n
        cov = 0.0_wp
        do k = 1, n
            d = p(:, k) - mean
            cov = cov + spread_product(d) / max(1, n - 1)
        end do
        ! the spread about the nominal, in its radial, in-track and cross-track directions
        nom = app%nominal%point_at(tt)
        vel = app%nominal%point_at(min(app%t_end, tt + 1.0_wp)) - app%nominal%point_at(max(0.0_wp, tt - 1.0_wp))
        ric(:, 1) = nom / norm2(nom)
        ric(:, 3) = cross(nom, vel)
        ric(:, 3) = ric(:, 3) / norm2(ric(:, 3))
        ric(:, 2) = cross(ric(:, 3), ric(:, 1))
        do k = 1, 3
            app%spread(k) = sqrt(max(0.0_wp, dot_product(ric(:, k), matmul(cov, ric(:, k)))))
        end do
        ! the ellipsoid: the covariance's axes, three times their 1-sigma sizes
        call symmetric_eigen(cov, vals, vecs)
        app%axes = 3.0_wp * sqrt(max(vals, 0.0_wp))
        call app%ellipsoid%set_position(mean)
        call app%ellipsoid%set_sphere_attitude(quat_from_matrix(vecs))
        call app%ellipsoid%set_sphere_scale(max(app%axes, 1.0e-3_wp))
        call update_readouts(tt)
    end subroutine place_cloud

    !> d d^T.
    pure function spread_product(d) result(m)
        real(wp), intent(in) :: d(3)                        !! a sample's offset from the mean (km)
        real(wp) :: m(3, 3)
        integer :: i
        do i = 1, 3
            m(:, i) = d * d(i)
        end do
    end function spread_product

    !> The spread and the ellipsoid in the HUD and the dock.
    subroutine update_readouts(t)
        real(wp), intent(in) :: t                           !! the cloud's time (s after the burn)
        character(len=:), allocatable :: text
        text = 't = '//num(t / 3600.0_wp, 2)//' h after the burn,  '//int_text(app%n_samples)//' samples'// &
               new_line('a')//'1-sigma spread: radial '//num(app%spread(1), 1)//' km, in-track '// &
               num(app%spread(2), 1)//' km, cross-track '//num(app%spread(3), 1)//' km'//new_line('a')// &
               '3-sigma ellipsoid: '//num(app%axes(1), 1)//' x '//num(app%axes(2), 1)//' x '//num(app%axes(3), 1)//' km'
        call app%hud_stats%set_text(text)
        call app%stats_label%setText(text)
    end subroutine update_readouts

    ! ==================================================================
    ! numbers

    !> The eigenvalues (largest first) and unit eigenvectors (columns, a
    !> right-handed set: a rotation) of a symmetric 3x3 matrix, by Jacobi
    !> rotations.
    pure subroutine symmetric_eigen(a, vals, vecs)
        real(wp), intent(in) :: a(3, 3)                     !! the symmetric matrix
        real(wp), intent(out) :: vals(3)                    !! its eigenvalues, largest first
        real(wp), intent(out) :: vecs(3, 3)                 !! their eigenvectors (columns)
        real(wp) :: m(3, 3), c, s, theta, t, tmp(3)
        integer :: sweep, p, q, i, order(3)
        m = a
        vecs = 0.0_wp
        do i = 1, 3
            vecs(i, i) = 1.0_wp
        end do
        do sweep = 1, 50
            if (abs(m(1, 2)) + abs(m(1, 3)) + abs(m(2, 3)) <= 1.0e-14_wp * (abs(m(1, 1)) + abs(m(2, 2)) + abs(m(3, 3)))) &
                exit
            do p = 1, 2
                do q = p + 1, 3
                    if (m(p, q) == 0.0_wp) cycle
                    theta = (m(q, q) - m(p, p)) / (2.0_wp * m(p, q))
                    t = sign(1.0_wp, theta) / (abs(theta) + sqrt(theta**2 + 1.0_wp))
                    c = 1.0_wp / sqrt(t**2 + 1.0_wp)
                    s = t * c
                    call rotate(m, vecs, p, q, c, s)
                end do
            end do
        end do
        vals = [m(1, 1), m(2, 2), m(3, 3)]
        order = [maxloc(vals, 1), 0, 0]
        order(3) = minloc(vals, 1)
        if (order(3) == order(1)) order(3) = merge(2, 1, order(1) == 1)
        order(2) = 6 - order(1) - order(3)
        vals = vals(order)
        vecs = vecs(:, order)
        tmp = cross(vecs(:, 1), vecs(:, 2))                  ! (right-handed)
        if (dot_product(tmp, vecs(:, 3)) < 0.0_wp) vecs(:, 3) = -vecs(:, 3)
    end subroutine symmetric_eigen

    !> One Jacobi rotation in the (p, q) plane: m = J^T m J, vecs = vecs J.
    pure subroutine rotate(m, vecs, p, q, c, s)
        real(wp), intent(inout) :: m(3, 3)                  !! the matrix being diagonalized
        real(wp), intent(inout) :: vecs(3, 3)               !! the eigenvectors so far (columns)
        integer, intent(in) :: p, q                         !! the plane (p < q)
        real(wp), intent(in) :: c, s                        !! the rotation's cosine and sine
        real(wp) :: j(3, 3)
        integer :: i
        j = 0.0_wp
        do i = 1, 3
            j(i, i) = 1.0_wp
        end do
        j(p, p) = c
        j(q, q) = c
        j(p, q) = s
        j(q, p) = -s
        m = matmul(transpose(j), matmul(m, j))
        vecs = matmul(vecs, j)
    end subroutine rotate

    !> The random numbers started from `seed` (the same seed: the same samples).
    subroutine seed_random(seed)
        integer, intent(in) :: seed                         !! the seed
        integer, allocatable :: put(:)
        integer :: n, i
        call random_seed(size=n)
        allocate(put(n))
        do i = 1, n
            put(i) = seed + 7919 * i
        end do
        call random_seed(put=put)
    end subroutine seed_random

    !> Three independent standard normal numbers (Box-Muller).
    subroutine gauss3(g)
        real(wp), intent(out) :: g(3)                       !! the numbers (mean 0, sigma 1)
        real(wp) :: u(4)
        call random_number(u)
        u(1) = max(u(1), tiny(1.0_wp))
        u(3) = max(u(3), tiny(1.0_wp))
        g(1) = sqrt(-2.0_wp * log(u(1))) * cos(2.0_wp * pi * u(2))
        g(2) = sqrt(-2.0_wp * log(u(1))) * sin(2.0_wp * pi * u(2))
        g(3) = sqrt(-2.0_wp * log(u(3))) * cos(2.0_wp * pi * u(4))
    end subroutine gauss3

    !> A unit vector perpendicular to the unit vector u.
    pure function perpendicular(u) result(p)
        real(wp), intent(in) :: u(3)                        !! a unit vector
        real(wp) :: p(3)
        if (abs(u(1)) < 0.9_wp) then
            p = cross(u, [1.0_wp, 0.0_wp, 0.0_wp])
        else
            p = cross(u, [0.0_wp, 1.0_wp, 0.0_wp])
        end if
        p = p / norm2(p)
    end function perpendicular

    !> a x b.
    pure function cross(a, b) result(c)
        real(wp), intent(in) :: a(3)                        !! the first vector
        real(wp), intent(in) :: b(3)                        !! the second vector
        real(wp) :: c(3)
        c = [a(2) * b(3) - a(3) * b(2), a(3) * b(1) - a(1) * b(3), a(1) * b(2) - a(2) * b(1)]
    end function cross

    !> x with `decimals` decimals (a leading zero kept: 0.5, -0.5).
    function num(x, decimals) result(text)
        real(wp), intent(in) :: x                           !! the number
        integer, intent(in) :: decimals                     !! decimals shown
        character(len=:), allocatable :: text
        character(len=32) :: buf, fmt
        write(fmt, '(a,i0,a)') '(f32.', decimals, ')'
        write(buf, fmt) x
        text = trim(adjustl(buf))
    end function num

    !> An integer as text.
    function int_text(n) result(text)
        integer, intent(in) :: n                            !! the number
        character(len=:), allocatable :: text
        character(len=16) :: buf
        write(buf, '(i0)') n
        text = trim(buf)
    end function int_text

    ! ==================================================================
    ! the frames

    !> Just before drawing: the time looped over the transfer (animated), and
    !> the cloud and its ellipsoid at the frame's time.
    subroutine cloud_before_draw(self, window, t)
        class(CloudHandler), intent(inout) :: self
        class(WindowProxy), intent(inout) :: window         !! the window about to draw
        real(wp), intent(in) :: t                           !! the simulation time (s after the burn)
        if (app%animate%isChecked() .and. t >= app%t_end) call window%set_time(0.0_wp)   ! (from the next frame)
        call place_cloud(t)
    end subroutine cloud_before_draw

    !> The views, the HUD texts, and the QWindow the scene is drawn in, with
    !> its events connected.
    subroutine build_window()
        app%views(1) = View(app%root, app%root)
        call app%views(1)%set_default_view_parameters(eye=[-22000.0_wp, -62000.0_wp, 42000.0_wp], &
            center=[-16000.0_wp, 0.0_wp, 0.0_wp], up=[0.0_wp, 0.0_wp, 1.0_wp])
        app%views(2) = View(app%root, app%ellipsoid, frame_type=ABSOLUTE_FRAME)
        call app%views(2)%set_default_view_parameters(eye=[6000.0_wp, -8500.0_wp, 5000.0_wp], &
            center=[0.0_wp, 0.0_wp, 0.0_wp], up=[0.0_wp, 0.0_wp, 1.0_wp])
        app%hud_title = HUDText('Monte Carlo dispersions of a transfer burn', position=[0.01_wp, 0.99_wp], &
                                alignment=ALIGN_LEFT_TOP, size=17.0_wp, color=[0.95_wp, 0.95_wp, 0.95_wp])
        app%hud_stats = HUDText('', position=[0.01_wp, 0.01_wp], alignment=ALIGN_LEFT_BOTTOM, size=15.0_wp, &
                                color=ellipsoid_color)
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
        if (.not. app%proxy%open(app%window%width(), app%window%height(), 'vkframes dispersions', &
                                 native_window=app%window%winId(), pixel_ratio=app%window%devicePixelRatio())) then
            write(*, '(a)') 'dispersions demo: could not open the view'
            return
        end if
        if (app%proxy%num_views() > 0) return                    ! (opened again)
        call app%proxy%set_scene(app%scene)
        call app%proxy%set_event_handler(handler)
        call app%proxy%set_background_color([0.0_wp, 0.0_wp, 0.0_wp])
        call app%proxy%set_sky(app%sky)
        do k = 1, n_views
            call app%proxy%add_view(app%views(k))
        end do
        call app%proxy%select_view(app%view_choice%currentIndex() + 1)
        call app%proxy%add_hud_text(app%hud_title)
        call app%proxy%add_hud_text(app%hud_stats)
        call apply_animation()
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
        if (app%window%devicePixelRatio() /= app%proxy%pixel_ratio()) &
            call app%proxy%resize(nint(app%window%width() * app%window%devicePixelRatio()), &
                                  nint(app%window%height() * app%window%devicePixelRatio()), &
                                  pixel_ratio=app%window%devicePixelRatio())
        if (.not. app%proxy%frame()) return
        app%frame = app%frame + 1
        if (app%still .and. app%frame == 10) then
            write(*, '(a)') 'dispersions: '//int_text(app%n_samples)//' samples to '// &
                            num(app%t_end / 3600.0_wp, 2)//' h after the burn'
            write(*, '(a)') 'dispersions: 1-sigma radial '//num(app%spread(1), 1)//' km, in-track '// &
                            num(app%spread(2), 1)//' km, cross-track '//num(app%spread(3), 1)//' km; 3-sigma axes '// &
                            num(app%axes(1), 1)//', '//num(app%axes(2), 1)//', '//num(app%axes(3), 1)//' km'
        end if
        if (app%options%after_frame(app%proxy, app%window, app%frame)) return   ! (a capture; --recreate-surface)
        call app%window%requestUpdate()
    end subroutine on_update

    ! ==================================================================
    ! controls

    !> Animated: the trajectories traced up to the current time, the time
    !> running over the transfer (looped); else drawn whole, the cloud at the end.
    subroutine apply_animation()
        integer :: k
        logical :: on
        on = app%animate%isChecked()
        do k = 1, max_samples
            call app%sample_curves(k)%set_trace_mode(on)
        end do
        call app%nominal_curve%set_trace_mode(on)
        if (.not. app%proxy%is_open()) return
        call app%proxy%set_time_scale(app%t_end / 12.0_wp)       ! (the transfer in 12 s)
        if (on) then
            call app%proxy%set_time(0.0_wp)
            call app%proxy%pause_time(.false.)
        else
            call app%proxy%set_time(app%t_end)
            call app%proxy%pause_time(.true.)
        end if
    end subroutine apply_animation

    subroutine on_animate(checked, data)
        logical, intent(in) :: checked                      !! true: animated (read by apply_animation)
        class(*), intent(inout), optional :: data           !! the connection's data (unused)
        call apply_animation()
    end subroutine on_animate

    !> A sample count, an error size or the seed changed: the samples drawn again.
    subroutine on_settings(value, data)
        real(c_double), intent(in) :: value                 !! the changed box's value (all are read)
        class(*), intent(inout), optional :: data           !! the connection's data (unused)
        call read_settings()
        call resample()
    end subroutine on_settings

    subroutine on_count(value, data)
        integer, intent(in) :: value                        !! the changed box's value (all are read)
        class(*), intent(inout), optional :: data           !! the connection's data (unused)
        call read_settings()
        call resample()
    end subroutine on_count

    !> The settings from the dock.
    subroutine read_settings()
        app%n_samples = app%samples_box%value()
        app%err%dv_size = app%dv_size_box%value()
        app%err%dv_pointing = app%dv_pointing_box%value()
        app%err%position = app%position_box%value()
        app%err%velocity = app%velocity_box%value()
    end subroutine read_settings

    !> What is shown, from the check boxes.
    subroutine on_shown(checked, data)
        logical, intent(in) :: checked                      !! (all the boxes are read)
        class(*), intent(inout), optional :: data           !! the connection's data (unused)
        call app%samples_frame%show_contents(app%show_samples%isChecked())
        call app%cloud_frame%show_contents(app%show_cloud%isChecked())
        call app%ellipsoid%show_contents(app%show_ellipsoid%isChecked())
        call app%nominal_frame%show_contents(app%show_nominal%isChecked())
    end subroutine on_shown

    !> A view chosen: shown.
    subroutine on_view(index, data)
        integer, intent(in) :: index                        !! the view (0-based)
        class(*), intent(inout), optional :: data           !! the connection's data (unused)
        if (app%proxy%is_open()) call app%proxy%select_view(index + 1)
    end subroutine on_view

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

    !> A spin box for an error's 1-sigma size.
    function error_box(value, high, step, decimals, suffix) result(box)
        real(wp), intent(in) :: value                       !! the value shown first
        real(wp), intent(in) :: high                        !! the largest allowed (the smallest: 0)
        real(wp), intent(in) :: step                        !! the arrows' step
        integer, intent(in) :: decimals                     !! decimals shown
        character(len=*), intent(in) :: suffix              !! the unit shown after the value
        type(QDoubleSpinBox) :: box
        box = QDoubleSpinBox()
        call box%setRange(0.0_c_double, real(high, c_double))
        call box%setDecimals(decimals)
        call box%setSingleStep(real(step, c_double))
        call box%setValue(real(value, c_double))
        call box%setSuffix(suffix)
        call box%valueChanged%connect(on_settings)
    end function error_box

    !> A check box for what is shown.
    function shown_box(text) result(box)
        character(len=*), intent(in) :: text                !! its label
        type(QCheckBox) :: box
        box = QCheckBox(text)
        call box%setChecked(.true.)
        call box%toggled%connect(on_shown)
    end function shown_box

    !> The main window: the scene in the middle, the dock (the samples, the
    !> errors, what is shown, the view, the statistics) on the left.
    subroutine build_ui()
        type(QDockWidget) :: dock
        type(QWidget) :: panel
        type(QFormLayout) :: form
        type(QPushButton) :: reset_view
        type(QHBoxLayout) :: view_row

        app%main = QMainWindow()
        call app%main%setWindowTitle('vkframes: Monte Carlo dispersions of a transfer burn')
        call app%main%closeEvent%connect(on_close)
        call build_window()
        call app%main%setCentralWidget(app%container)

        panel = QWidget()
        form = QFormLayout(panel)

        app%samples_box = QSpinBox()
        call app%samples_box%setRange(10, max_samples)
        call app%samples_box%setSingleStep(50)
        call app%samples_box%setValue(app%n_samples)
        call app%samples_box%valueChanged%connect(on_count)
        call form%addRow('Samples', app%samples_box)
        app%seed_box = QSpinBox()
        call app%seed_box%setRange(1, 99999)
        call app%seed_box%setValue(1)
        call app%seed_box%valueChanged%connect(on_count)
        call form%addRow('Random seed', app%seed_box)

        call form%addRow(QLabel('<b>Errors (1-sigma)</b>'))
        app%dv_size_box = error_box(app%err%dv_size, 10.0_wp, 0.1_wp, 2, ' %')
        call form%addRow('Burn size', app%dv_size_box)
        app%dv_pointing_box = error_box(app%err%dv_pointing, 10.0_wp, 0.1_wp, 2, ' deg')
        call form%addRow('Burn pointing', app%dv_pointing_box)
        app%position_box = error_box(app%err%position, 100.0_wp, 0.5_wp, 2, ' km')
        call form%addRow('Position known', app%position_box)
        app%velocity_box = error_box(app%err%velocity, 100.0_wp, 0.5_wp, 2, ' m/s')
        call form%addRow('Velocity known', app%velocity_box)

        call form%addRow(QLabel('<b>Shown</b>'))
        app%animate = QCheckBox('Animate (trace, the time looping)')
        call app%animate%setChecked(.not. app%still)
        call app%animate%toggled%connect(on_animate)
        call form%addRow('', app%animate)
        app%show_samples = shown_box('The samples (colored by their miss)')
        call form%addRow('', app%show_samples)
        app%show_cloud = shown_box('The cloud now')
        call form%addRow('', app%show_cloud)
        app%show_ellipsoid = shown_box('Its 3-sigma ellipsoid')
        call form%addRow('', app%show_ellipsoid)
        app%show_nominal = shown_box('The nominal, and the orbit before the burn')
        call form%addRow('', app%show_nominal)

        app%view_choice = QComboBox()
        call app%view_choice%addItems(view_names)
        call app%view_choice%setCurrentIndex(app%start_view - 1)
        call app%view_choice%currentIndexChanged%connect(on_view)
        reset_view = QPushButton('Reset view')
        call reset_view%clicked%connect(on_reset_view)
        view_row = QHBoxLayout()
        call view_row%addWidget(app%view_choice)
        call view_row%addWidget(reset_view)
        call form%addRow('View', view_row)

        app%stats_label = QLabel('')
        call app%stats_label%setWordWrap(.true.)
        call form%addRow('Statistics', app%stats_label)
        call form%addRow(QLabel('<small>Each sample: the burn executed with errors in its size and ' // &
                                'direction, from a state known with errors, propagated to the nominal ' // &
                                'transfer''s apoapsis. Left drag rotates, right drag zooms, middle drag pans; ' // &
                                'space resets the view.</small>'))
        call panel%setMaximumWidth(420)
        dock = QDockWidget('Dispersions', app%main)
        call dock%setWidget(panel)
        call app%main%addDockWidget(Qt_LeftDockWidgetArea, dock)
    end subroutine build_ui

end module dispersions_demo

!> The dispersions demo (see the module documentation).
program vkframes_dispersions_demo

    use qt
    use dispersions_demo
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
        case ('--samples')
            call get_command_argument(i + 1, arg)
            read(arg, *) app%n_samples
            app%n_samples = max(10, min(max_samples, app%n_samples))
            i = i + 1
        case ('--view')
            call get_command_argument(i + 1, arg)
            read(arg, *) app%start_view
            app%start_view = max(1, min(n_views, app%start_view))
            i = i + 1
        case ('--still')
            app%still = .true.
        end select
        i = i + 1
    end do

    call build_model()
    call build_ui()
    call build_nominal()
    call resample()
    call app%options%start_quit_timer(app%main)
    call app%main%resize(1400, 860)
    call app%main%show()
    ierr = qapp%exec()

end program vkframes_dispersions_demo
