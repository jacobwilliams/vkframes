!> A lunar vkframes scene in a qt-fortran GUI: the Moon's terrain drawn by
!> Rocky (a TerrainBody: the LRO WAC mosaic from NASA's Moon Trek, over the
!> network, and LOLA's elevation, data/moon_lola_ldem16.tif from
!> `pixi run -e rocky moon-data`, or the finer moon_lola_ldem64.tif with
!> `-- --resolution 64`), lit by the Sun (a frame with a light
!> source) under a starry sky, and a spacecraft in a 100 km polar orbit that
!> passes over Tycho, its orbit drawn by a CurveArtist. The Moon turns at its
!> sidereal rate under the inertial orbit. A lander (NASA's Apollo Lunar
!> Module model, data/Models/apollo_lunar_module.glb from `pixi run -e rocky
!> lander-model`, enlarged to be seen; else a sphere), in Moon-fixed
!> coordinates (a child of the Moon's frame), coasts in the same 100 km
!> orbit, starts its descent at t = 1200 s and lands on Tycho's floor north
!> of the central peak 13 minutes later, at the terrain's height there
!> (TerrainBody%elevation): braking (a constant deceleration along its
!> track), pitching over (the deceleration easing to nothing as it stops
!> over the site), then a vertical descent from 1.5 km, its altitude on
!> smooth curves; its attitude points its engine along the thrust the path
!> needs (its acceleration less the Moon's gravity), so it flies engine
!> first while braking and stands upright for the vertical descent. Its
!> path is drawn too, with its altitude above the terrain below it (a
!> dashed line down to the ground, the height in its label, which follows
!> it, and in the HUD) and, as it passes, a drop line to the ground every
!> 10 s of its descent (the terrain's profile under the path: Tycho's rim,
!> then its central peak). At startup the demo prints how
!> closely the path passes over ground higher than the site.
!>
!> Views (from the dock, or with the window's keys): the Moon; the spacecraft
!> (body-fixed); from the spacecraft toward the Moon (the surface going by
!> below); Tycho, close to the surface (Moon-fixed); the lander; the landing
!> site, close up (Moon-fixed); the lander seen from the landing site (an
!> observer on the ground 3 km east of it, turning to follow the lander from
!> the horizon to its touchdown). The dock sets the time
!> (scale, pause, reset, and a slider over a day that follows the animation),
!> the Sun's longitude (the terminator: low Sun, long shadows), the terrain's
!> lighting (off: the imagery as it is), the orbit's and descent's curves,
!> and the base frame of the Moon-fixed views (Tycho, the landing site): the
!> Moon's center (its axes) or the landing site (a frame on the ground
!> there: x east, y north, z up), shown with ticked axes (1 km ticks).
!>
!> Run with `pixi run -e rocky moon-demo` (after `pixi run -e rocky
!> rocky-build`, and `pixi run -e rocky moon-data` for the elevation). Options:
!> `--view n` starts in view n (1 to 7); `--site-frame` with the landing
!> site as the base frame; `--sun degrees` the Sun's longitude;
!> `--time seconds` the start time; `--capture file` saves the frame
!> `--capture-frame n` (default 600) as an image; `--quit-after seconds`.
module moondemo

    use qt
    use vkframes
    use vkframes_qt, only: connect_view, ViewLink, DemoOptions
    use iso_c_binding
    implicit none

    real(wp), parameter :: deg = pi / 180.0_wp
    real(wp), parameter :: mu = 4902.800066_wp                   !! the Moon's gravitational parameter (km^3/s^2)
    real(wp), parameter :: rm = 1737.4_wp                        !! the Moon's radius (km)
    real(wp), parameter :: sidereal_month = 27.321661_wp * 86400.0_wp   !! the Moon's rotation period (s)
    real(wp), parameter :: au = 1.495978707e8_wp                 !! km
    real(wp), parameter :: altitude = 100.0_wp                   !! the orbit's (km)
    real(wp), parameter :: tycho_lat = -43.31_wp, tycho_lon = -11.36_wp   !! degrees
    real(wp), parameter :: sc_size = 0.4_wp                      !! the spacecraft's radius (km, enlarged to be seen)
    real(wp), parameter :: day = 86400.0_wp
    integer, parameter :: n_orbit = 721
    character(len=*), parameter :: moon_imagery = &
        'https://trek.nasa.gov/tiles/Moon/EQ/LRO_WAC_Mosaic_Global_303ppd_v02/1.0.0/default/default028mm/{z}/{y}/{x}.jpg'
    !> the Moon's elevation (`pixi run -e rocky moon-data [-- --resolution 64]`): the finest found
    character(len=*), parameter :: moon_elevation(3) = [character(len=20) :: 'moon_lola_ldem64.tif', &
                                                        'moon_lola_ldem16.tif', 'moon_lola_ldem4.tif']
    real(wp), parameter :: orbit_color(3) = [1.0_wp, 0.85_wp, 0.2_wp]
    ! the lander: coasting, then descending from t_descent for descent_time to its site
    real(wp), parameter :: site_lat = -43.0_wp, site_lon = tycho_lon      !! on Tycho's floor, north of the peak
    real(wp), parameter :: t_descent = 1200.0_wp, descent_time = 780.0_wp  !! s
    real(wp), parameter :: braking_time = 520.0_wp, pitch_time = 170.0_wp  !! s (then the vertical descent)
    real(wp), parameter :: vertical_height = 1.5_wp                        !! km above the site, where it starts
    real(wp), parameter :: vertical_speed = 0.025_wp                       !! km/s, then
    ! the lander drawn: NASA's Apollo Lunar Module (`pixi run -e rocky lander-model`), else a sphere
    character(len=*), parameter :: lander_model = 'Models/apollo_lunar_module.glb'
    real(wp), parameter :: lm_bottom = 0.103_wp, lm_top = 5.115_wp        !! its footpads' bottom, its top (m, up)
    real(wp), parameter :: lander_height = 0.4_wp                          !! km (its 5 m enlarged to be seen)
    real(wp), parameter :: lander_size = 0.5_wp * lander_height            !! its center above its footpads (km)
    real(wp), parameter :: lander_color(3) = [0.3_wp, 0.9_wp, 1.0_wp]
    real(wp), parameter :: site_axes_length = 10.0_wp, site_axes_tick = 1.0_wp   !! km (minor ticks: a fifth)
    real(wp), parameter :: observer_east = 3.0_wp, observer_height = 0.05_wp   !! km: from the site, above the ground
    integer, parameter :: n_views = 7
    character(len=20), parameter :: view_names(n_views) = [character(len=20) :: 'Moon', 'Spacecraft', &
        'Spacecraft -> Moon', 'Tycho', 'Lander', 'Landing site', 'Lander from the site']

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
        type(QComboBox) :: view_choice
        type(QDoubleSpinBox) :: time_scale
        type(QCheckBox) :: pause_time, terrain_lighting, show_orbit
        type(QSlider) :: time_slider, sun_slider
        type(QLabel) :: time_label, sun_label
        logical :: slider_held = .false.                       !! the user holds the time slider's handle
        logical :: slider_setting = .false.                    !! the demo moves it (to the time)
        type(HUDText) :: hud_view, hud_time, hud_lander
        type(QCheckBox) :: show_descent, site_base
        ! the scene
        type(ReferenceFrame) :: inertial                       !! the root: the Moon's center, inertial axes
        type(TerrainBody) :: moon                              !! turns at its sidereal rate
        type(ReferenceFrame) :: sun
        type(Sphere) :: spacecraft
        type(Trajectory) :: orbit                              !! one period of positions and attitudes
        type(DrawableTrajectory) :: orbit_frame
        type(CurveArtist) :: orbit_curve
        type(SkySphere) :: sky
        type(FrameManager) :: scene
        real(wp) :: period = 0.0_wp
        ! the lander (in the Moon's frame)
        type(Model) :: lander                                 !! the LM model, centered on the frame's origin
        type(Sphere) :: lander_ball                           !! (without the model: a sphere in its frame)
        type(Trajectory) :: descent
        type(DrawableTrajectory) :: descent_frame
        type(CurveArtist) :: descent_curve
        type(SegmentArtist) :: curtain                        !! drop lines to the ground along the descent
        type(Trajectory) :: drop                              !! one point: the lander (optional 1), the ground below (2)
        type(SegmentArtist) :: drop_line                      !! the lander's altitude line
        type(MarkerArtist) :: drop_foot                       !! where it meets the ground
        type(ReferenceFrame) :: observer                      !! on the ground near the site (Moon-fixed; z up, y north)
        real(wp) :: site_height = 0.0_wp                      !! the ground's at the site (km above the sphere)
        type(ReferenceFrame) :: site                          !! on the ground at the site (Moon-fixed; x east, y north, z up)
        type(CoordinateAxes) :: site_axes                     !! ticked, in the site frame
        logical :: site_frame = .false.                       !! the Moon-fixed views' base: the site (else the Moon)
        ! options
        integer :: start_view = 1
        real(wp) :: start_sun = tycho_lon + 75.0_wp            !! low over Tycho, from the east
        real(wp) :: start_time = 0.0_wp
        type(DemoOptions) :: options                        !! --capture, --capture-frame, --quit-after, --recreate-surface
    end type app_t

    !> Each frame, at its time, before it is drawn: the lander's altitude line
    !> and label (set after a frame, they would be drawn a frame late: behind
    !> the lander, by as much as it moves in a frame).
    type, extends(WindowEventHandler) :: MoonHandler
    contains
        procedure :: before_draw => moon_before_draw
    end type MoonHandler

    type(app_t), target, save :: app
    type(MoonHandler), target, save :: handler

contains

    ! ==================================================================
    ! the scene

    !> Position (km) on the circular polar orbit at time t: its plane through
    !> Tycho's longitude (at t = 0), heading north, south of Tycho at t = 0.
    pure function orbit_position(t, period) result(r)
        real(wp), intent(in) :: t, period
        real(wp) :: r(3)
        real(wp) :: u, node
        u = (tycho_lat - 20.0_wp) * deg + 2.0_wp * pi * t / period      ! argument of latitude
        node = tycho_lon * deg
        r = (rm + altitude) * [cos(node) * cos(u), sin(node) * cos(u), sin(u)]
    end function orbit_position

    !> The spacecraft's attitude at time t: x up (radial), y along the
    !> velocity, z along the orbit normal.
    function orbit_attitude(t, period) result(q)
        real(wp), intent(in) :: t, period
        real(wp) :: q(4)
        real(wp) :: x(3), y(3), z(3), dt
        dt = 1.0e-3_wp * period
        x = orbit_position(t, period)
        y = orbit_position(t + dt, period) - orbit_position(t - dt, period)
        x = x / norm2(x)
        y = y - dot_product(y, x) * x
        y = y / norm2(y)
        z = [x(2) * y(3) - x(3) * y(2), x(3) * y(1) - x(1) * y(3), x(1) * y(2) - x(2) * y(1)]
        q = quat_from_matrix(reshape([x, y, z], [3, 3]))
    end function orbit_attitude

    !> The lander's position (km, Moon-fixed) and height above the site's
    !> ground at time t: on its meridian, northbound; coasting in the orbit
    !> until t_descent, then braking (a constant deceleration of its angular
    !> rate), pitching over (the deceleration easing to nothing, a smoothstep,
    !> as it stops over the site: no jump in acceleration) and descending
    !> vertically to the ground. Its altitude: from the orbit's to
    !> vertical_height above the site, descending at vertical_speed (a
    !> quartic, starting level and with no vertical acceleration: still in
    !> orbit, it can't fall faster than that), then to the ground (level; a
    !> cubic Hermite curve).
    subroutine lander_state(t, r, height)
        real(wp), intent(in) :: t
        real(wp), intent(out) :: r(3), height
        real(wp) :: n, decel, v_b, x_b, arc, x, tau, q, h, th, tv, d, c3, c4
        n = 2.0_wp * pi / app%period                             ! the orbit's angular rate
        th = braking_time + pitch_time                           ! the horizontal phases
        tv = descent_time - th                                   ! the vertical descent
        decel = n / (braking_time + 0.5_wp * pitch_time)         ! (stopped at th: the smoothstep's mean is 1/2)
        v_b = n - decel * braking_time                           ! the rate when pitching over starts
        x_b = n * braking_time - 0.5_wp * decel * braking_time**2
        arc = x_b + v_b * pitch_time - 0.35_wp * decel * pitch_time**2   ! the arc flown (to the site)
        if (t <= t_descent) then
            x = -n * (t_descent - t)
            h = altitude
        else
            tau = min(t - t_descent, descent_time)
            if (tau <= braking_time) then
                x = n * tau - 0.5_wp * decel * tau**2
            else if (tau <= th) then                             ! deceleration decel * (1 - 3 q^2 + 2 q^3)
                q = (tau - braking_time) / pitch_time
                x = x_b + v_b * pitch_time * q - decel * pitch_time**2 * (q**2 / 2 - q**4 / 4 + q**5 / 10)
            else
                x = arc
            end if
            if (tau <= th) then                                  ! h = altitude + c3 s^3 + c4 s^4
                d = app%site_height + vertical_height - altitude
                c4 = -vertical_speed * th - 3.0_wp * d
                c3 = d - c4
                h = altitude + c3 * (tau / th)**3 + c4 * (tau / th)**4
            else
                h = hermite((tau - th) / tv, app%site_height + vertical_height, -vertical_speed * tv, &
                            app%site_height, 0.0_wp)
            end if
        end if
        x = site_lat * deg - arc + x                             ! the latitude (rad)
        r = (rm + h + lander_size) * [cos(x) * cos(site_lon * deg), cos(x) * sin(site_lon * deg), sin(x)]
        height = h - app%site_height
    end subroutine lander_state

    !> The cubic Hermite curve from p0 (slope m0) at s = 0 to p1 (slope m1) at s = 1.
    pure real(wp) function hermite(s, p0, m0, p1, m1) result(p)
        real(wp), intent(in) :: s, p0, m0, p1, m1
        p = (2 * s**3 - 3 * s**2 + 1) * p0 + (s**3 - 2 * s**2 + s) * m0 + (3 * s**2 - 2 * s**3) * p1 + (s**3 - s**2) * m1
    end function hermite

    !> The descent's phase at time t ('' before or after it).
    function lander_phase(t) result(phase)
        real(wp), intent(in) :: t
        character(len=:), allocatable :: phase
        phase = ''
        if (t <= t_descent .or. t >= t_descent + descent_time) return
        if (t - t_descent <= braking_time) then
            phase = 'braking'
        else if (t - t_descent <= braking_time + pitch_time) then
            phase = 'pitching over'
        else
            phase = 'vertical descent'
        end if
    end function lander_phase

    !> The lander's attitude at time t: z (its up, the engine's thrust) along
    !> the thrust the path needs, its acceleration less the Moon's gravity
    !> (central differences of lander_state), in the meridian plane it flies
    !> in, never below horizontal; y west (it pitches about it), so x is
    !> north when it is upright (its heading) and up when it flies engine
    !> first. Coasting: as
    !> when the burn starts (engine first), in its local axes; landed:
    !> upright (no acceleration).
    function lander_attitude(t) result(q)
        real(wp), intent(in) :: t
        real(wp) :: q(4)
        real(wp) :: tt, r(3), r1(3), r2(3), h, a(3), thrust(3), x(3), y(3), z(3), zn, zu
        real(wp), parameter :: dt = 0.5_wp
        tt = max(t, t_descent + 2.0_wp * dt)
        call lander_state(tt, r, h)
        call lander_state(tt - dt, r1, h)
        call lander_state(tt + dt, r2, h)
        a = (r2 - 2.0_wp * r + r1) / dt**2
        thrust = a + mu * r / norm2(r)**3                        ! (a - g, g = -mu r / |r|^3)
        zn = dot_product(thrust, north_at(r))                    ! (its north and up parts)
        zu = max(0.0_wp, dot_product(thrust, unit(r)))           ! (not below horizontal: engine first at most)
        call lander_state(t, r, h)                               ! (where the lander is)
        x = north_at(r)
        z = unit(zn * x + zu * unit(r))
        y = unit(cross(unit(r), x))                              ! west: it pitches about its y axis
        x = cross(y, z)                                          ! (upright: north)
        q = quat_from_matrix(reshape([x, y, z], [3, 3]))
    end function lander_attitude

    !> The cross product a x b.
    pure function cross(a, b) result(c)
        real(wp), intent(in) :: a(3), b(3)
        real(wp) :: c(3)
        c = [a(2) * b(3) - a(3) * b(2), a(3) * b(1) - a(1) * b(3), a(1) * b(2) - a(2) * b(1)]
    end function cross

    !> North at a point on the lander's meridian (its direction of flight).
    function north_at(r) result(x)
        real(wp), intent(in) :: r(3)
        real(wp) :: x(3), up(3), lon
        up = unit(r)
        lon = site_lon * deg
        x = [-up(3) * cos(lon), -up(3) * sin(lon), sqrt(up(1)**2 + up(2)**2)]   ! (d/dlatitude of up)
        if (r(1) * cos(lon) + r(2) * sin(lon) < 0.0_wp) x(3) = -x(3)            ! (past a pole)
    end function north_at

    !> The point on the terrain straight below a Moon-fixed point (km), from
    !> LOLA's heights (TerrainBody%elevation: the elevation file, at its full
    !> resolution, whatever the view has loaded).
    function ground_below(r) result(g)
        real(wp), intent(in) :: r(3)
        real(wp) :: g(3)
        real(wp) :: u(3)
        u = r / norm2(r)
        g = (rm + app%moon%elevation(asin(u(3)) / deg, atan2(u(2), u(1)) / deg)) * u
    end function ground_below

    !> The lander's altitude above the terrain below it (km) at time t, and
    !> the altitude line's ends (the lander and the ground below) set to it.
    function update_drop(t) result(agl)
        real(wp), intent(in) :: t
        real(wp) :: agl
        real(wp) :: r(3), g(3), h
        call lander_state(max(0.0_wp, min(t, t_descent + descent_time)), r, h)   ! (followed with FOLLOW_LIMIT)
        g = ground_below(r)
        agl = norm2(r) - lander_size - norm2(g)
        call app%drop%set_optional(1, r)
        call app%drop%set_optional(2, g)
    end function update_drop

    !> Print how closely the descent passes over terrain higher than the
    !> landing site (the crater's central peak, its rim): the lowest altitude
    !> above the ground where the ground is at least 0.5 km above the site.
    subroutine report_clearance()
        real(wp) :: t, r(3), h, agl, low, t_low, rise
        low = huge(1.0_wp)
        t_low = -1.0_wp
        t = t_descent
        do while (t < t_descent + descent_time)
            call lander_state(t, r, h)
            agl = norm2(r) - lander_size - norm2(ground_below(r))
            if (h - agl > 0.5_wp .and. agl < low) then
                low = agl
                t_low = t
                rise = h - agl
            end if
            t = t + 1.0_wp
        end do
        if (t_low < 0.0_wp) return
        write(*, '(a,i0,a)') 'moon demo: the descent passes '//km(low)//' km over ground '//km(rise)// &
            ' km above the site (t = ', nint(t_low), ' s)'
    end subroutine report_clearance

    !> A point at latitude and longitude (degrees) and height (km) above the Moon's sphere.
    pure function surface(lat, lon, h) result(p)
        real(wp), intent(in) :: lat, lon, h
        real(wp) :: p(3)
        p = (rm + h) * [cos(lat * deg) * cos(lon * deg), cos(lat * deg) * sin(lon * deg), sin(lat * deg)]
    end function surface

    !> The local axes at latitude and longitude (degrees), as matrix columns: east, north, up.
    pure function local_axes(lat, lon) result(m)
        real(wp), intent(in) :: lat, lon
        real(wp) :: m(3,3)
        real(wp) :: a, b
        a = lat * deg
        b = lon * deg
        m(:, 1) = [-sin(b), cos(b), 0.0_wp]
        m(:, 2) = [-sin(a) * cos(b), -sin(a) * sin(b), cos(a)]
        m(:, 3) = [cos(a) * cos(b), cos(a) * sin(b), sin(a)]
    end function local_axes

    pure function unit(v) result(u)
        real(wp), intent(in) :: v(3)
        real(wp) :: u(3)
        u = v / norm2(v)
    end function unit

    !> The scene: the Moon, the Sun, the spacecraft and its orbit, the sky.
    subroutine build_model()
        real(wp) :: t, r(3), h, lon
        integer :: k
        logical :: found
        type(DataSource) :: foot(3)
        real(wp), parameter :: o(3) = 0.0_wp

        app%inertial = ReferenceFrame('Inertial')
        call app%inertial%show_decorations(.false.)

        app%moon = TerrainBody('Moon', profile='moon', color=[0.45_wp, 0.45_wp, 0.45_wp])
        call app%moon%add_image_layer(moon_imagery, LAYER_GEODETIC)
        do k = 1, size(moon_elevation)
            if (len(find_data_file(trim(moon_elevation(k)))) == 0) cycle
            call app%moon%add_elevation_layer(trim(moon_elevation(k)), LAYER_GDAL)
            write(*, '(a)') 'moon demo: elevation from '//trim(moon_elevation(k))
            exit
        end do
        if (k > size(moon_elevation)) write(*, '(a)') 'moon demo: no elevation (pixi run -e rocky moon-data)'
        call app%moon%show_decorations(.false.)

        app%sun = ReferenceFrame('Sun')
        call app%sun%set_light_source_enabled(.true.)
        call app%sun%set_light_diffuse([1.0_wp, 1.0_wp, 1.0_wp])
        call app%sun%set_light_ambient([0.02_wp, 0.02_wp, 0.02_wp])
        call app%sun%show_decorations(.false.)
        call set_sun_longitude(app%start_sun)

        ! the orbit: one period of positions and attitudes
        app%period = 2.0_wp * pi * sqrt((rm + altitude)**3 / mu)
        app%orbit = Trajectory()
        call app%orbit%reserve(n_orbit, attitudes=.true.)
        do k = 1, n_orbit
            t = app%period * real(k - 1, wp) / real(n_orbit - 1, wp)
            call app%orbit%add(t, pos=orbit_position(t, app%period), att=orbit_attitude(t, app%period))
        end do
        app%spacecraft = Sphere('Spacecraft', radius=sc_size, color=[1.0_wp, 0.4_wp, 0.25_wp])
        call app%spacecraft%follow(app%orbit)                 ! position and attitude, looped
        call app%spacecraft%move_x_axis(o, 4.0_wp * sc_size, body_radius=0.06_wp * sc_size, head_radius=0.16_wp * sc_size)
        call app%spacecraft%move_y_axis(o, 4.0_wp * sc_size, body_radius=0.06_wp * sc_size, head_radius=0.16_wp * sc_size)
        call app%spacecraft%move_z_axis(o, 4.0_wp * sc_size, body_radius=0.06_wp * sc_size, head_radius=0.16_wp * sc_size)

        app%orbit_curve = CurveArtist(app%orbit, color=orbit_color, width=2.0_wp)
        app%orbit_frame = DrawableTrajectory('Orbit')
        call app%orbit_frame%show_decorations(.false.)
        call app%orbit_frame%add_artist(app%orbit_curve)

        ! the lander: Moon-fixed, landing at the terrain's height at its site
        app%site_height = app%moon%elevation(site_lat, site_lon, found)
        if (found) then
            write(*, '(a,f0.3,a)') 'moon demo: landing site at ', app%site_height, ' km (LOLA)'
        else
            write(*, '(a)') 'moon demo: no elevation at the landing site: landing on the sphere'
        end if
        app%descent = Trajectory()
        call app%descent%set_num_optionals(1)                    ! the drop to the ground below (descending)
        t = 0.0_wp
        do while (t < t_descent + descent_time + 1.0_wp)
            call lander_state(t, r, h)
            call app%descent%add(t, pos=r, att=lander_attitude(t))
            if (t > t_descent) call app%descent%set_optional(1, ground_below(r) - r)
            t = t + merge(10.0_wp, 2.0_wp, t < t_descent)        ! (finer during the descent)
        end do
        call report_clearance()
        app%lander = Model('Lander', color=lander_color)
        if (len(find_data_file(lander_model)) > 0) then
            if (.not. app%lander%set_model(find_data_file(lander_model))) error stop 'moon demo: the lander model'
            ! its center at the origin, its up the frame's z (radial: glTF's y up is read as z up), lander_height tall
            call app%lander%set_model_pivot([0.0_wp, 0.0_wp, 0.5_wp * (lm_bottom + lm_top)])
            call app%lander%set_model_scale([1.0_wp, 1.0_wp, 1.0_wp] * lander_height / (lm_top - lm_bottom))
        else
            write(*, '(a)') 'moon demo: no lander model (pixi run -e rocky lander-model): a sphere'
            app%lander_ball = Sphere('Lander body', radius=lander_size, color=lander_color)
            call app%lander_ball%show_decorations(.false.)
            if (.not. app%lander%add_child(app%lander_ball)) error stop 'moon demo: could not build the scene'
        end if
        call app%lander%follow(app%descent, mode=FOLLOW_LIMIT)  ! then stays at the site
        call app%lander%move_x_axis(o, 1.5_wp * lander_height, body_radius=0.02_wp * lander_height, &
                                    head_radius=0.05_wp * lander_height)
        call app%lander%move_y_axis(o, 1.5_wp * lander_height, body_radius=0.02_wp * lander_height, &
                                    head_radius=0.05_wp * lander_height)
        call app%lander%move_z_axis(o, 1.5_wp * lander_height, body_radius=0.02_wp * lander_height, &
                                    head_radius=0.05_wp * lander_height)
        app%descent_curve = CurveArtist(app%descent, color=lander_color, width=2.0_wp)
        app%descent_frame = DrawableTrajectory('Descent')
        call app%descent_frame%show_decorations(.false.)
        call app%descent_frame%add_artist(app%descent_curve)
        ! the terrain under the path: a drop line every 10 s, drawn as the lander passes
        app%curtain = SegmentArtist(app%descent, color=[lander_color, 0.35_wp], width=1.0_wp, stride=5)
        if (.not. (app%curtain%set_start_data(position_sources(3)) .and. &
                   app%curtain%set_end_data(optional_sources(1)))) error stop 'moon demo: the curtain'
        call app%curtain%set_end_relative(.true.)
        call app%curtain%set_trace_mode(.true.)
        call app%descent_frame%add_artist(app%curtain)
        ! the lander's altitude line, from it straight down to the ground (updated each frame)
        app%drop = Trajectory()
        call app%drop%set_num_optionals(2)
        call app%drop%add(0.0_wp, pos=o)
        h = update_drop(0.0_wp)
        app%drop_line = SegmentArtist(app%drop, color=lander_color, width=1.5_wp)
        if (.not. (app%drop_line%set_start_data(optional_sources(1)) .and. &
                   app%drop_line%set_end_data(optional_sources(2)))) error stop 'moon demo: the altitude line'
        call app%drop_line%set_pattern(1, int(z'F0F0'))
        app%drop_foot = MarkerArtist(app%drop, size=10.0_wp, color=lander_color)
        call app%drop_foot%set_marker_shape(MARKER_PLUS)
        foot = optional_sources(2)
        if (.not. (app%drop_foot%set_x_data(foot(1)) .and. app%drop_foot%set_y_data(foot(2)) .and. &
                   app%drop_foot%set_z_data(foot(3)))) error stop 'moon demo: the altitude line'
        call app%descent_frame%add_artist(app%drop_line)
        call app%descent_frame%add_artist(app%drop_foot)

        ! an observer on the ground east of the site: local axes (x east, y north, z up)
        lon = site_lon + observer_east / ((rm + app%site_height) * cos(site_lat * deg)) / deg
        app%observer = ReferenceFrame('Observer')
        call app%observer%show_decorations(.false.)
        call app%observer%set_position(surface(site_lat, lon, app%moon%elevation(site_lat, lon) + observer_height))
        call app%observer%set_attitude(quat_from_matrix(local_axes(site_lat, lon)))
        ! the landing site's frame, with ticked axes (no arrows or labels), shown while it is the base frame
        app%site = ReferenceFrame('Site')
        call app%site%show_decorations(.false.)
        call app%site%set_position(surface(site_lat, site_lon, app%site_height))
        call app%site%set_attitude(quat_from_matrix(local_axes(site_lat, site_lon)))
        app%site_axes = CoordinateAxes('Site axes', color=[0.9_wp, 0.9_wp, 0.9_wp])
        call app%site_axes%set_axis_length(site_axes_length)
        call app%site_axes%set_tick_spacing(site_axes_tick, site_axes_tick / 5.0_wp)
        call app%site_axes%set_axis_width(1.5_wp)
        call app%site_axes%set_tick_size(10.0_wp, 5.0_wp)
        call app%site_axes%show_name_label(.false.)
        call app%site_axes%show_axes(NO_AXES)
        call app%site_axes%show_axes_labels(NO_AXES)
        call app%site_axes%show_contents(app%site_frame)
        if (.not. (app%moon%add_child(app%lander) .and. app%moon%add_child(app%descent_frame) .and. &
                   app%moon%add_child(app%observer) .and. app%moon%add_child(app%site) .and. &
                   app%site%add_child(app%site_axes))) &
            error stop 'moon demo: could not build the scene'

        if (.not. (app%inertial%add_child(app%moon) .and. app%inertial%add_child(app%sun) .and. &
                   app%inertial%add_child(app%spacecraft) .and. app%inertial%add_child(app%orbit_frame))) &
            error stop 'moon demo: could not build the scene'
        app%scene = FrameManager(app%inertial)

        app%sky = SkySphere()
        if (.not. app%sky%set_star_data('Stars_HYGv3.txt', min_mag=-2.0_wp, max_mag=6.0_wp, max_stars=40000)) &
            write(*, '(a)') 'moon demo: no star catalog'
    end subroutine build_model

    !> The Sun in the Moon's equatorial plane, at longitude `lon` (degrees) of the inertial frame.
    subroutine set_sun_longitude(lon)
        real(wp), intent(in) :: lon
        call app%sun%set_position(au * [cos(lon * deg), sin(lon * deg), 0.0_wp])
    end subroutine set_sun_longitude

    !> The Moon's attitude at time t: turning about its pole at the sidereal rate.
    subroutine turn_moon(t)
        real(wp), intent(in) :: t
        call app%moon%set_attitude(quat_from_axis_angle([0.0_wp, 0.0_wp, 1.0_wp], 2.0_wp * pi * t / sidereal_month))
    end subroutine turn_moon

    !> The base frame of the Moon-fixed views (4, Tycho, and 6, the landing
    !> site): the landing site's frame (`at_site`) or the Moon's; their
    !> default eyes, centers and ups in its axes, and the views reset to them.
    !> The site's ticked axes are shown with it.
    subroutine set_base_frame(at_site)
        logical, intent(in) :: at_site
        real(wp) :: eye(3,2), center(3,2), up(3,2), m(3,3), o(3)
        integer, parameter :: ks(2) = [4, 6]
        integer :: k
        ! in the Moon's axes (km)
        eye(:, 1) = surface(tycho_lat - 2.0_wp, tycho_lon - 0.8_wp, 40.0_wp)
        center(:, 1) = surface(tycho_lat, tycho_lon, 0.0_wp)
        up(:, 1) = unit(surface(tycho_lat, tycho_lon, 0.0_wp))
        eye(:, 2) = surface(site_lat - 0.25_wp, site_lon - 0.12_wp, 4.0_wp)
        center(:, 2) = surface(site_lat, site_lon, app%site_height)
        up(:, 2) = unit(surface(site_lat, site_lon, 0.0_wp))
        app%site_frame = at_site
        call app%site_axes%show_contents(at_site)
        if (at_site) then                                        ! into the site's: x' = m^T (x - o)
            m = local_axes(site_lat, site_lon)
            o = surface(site_lat, site_lon, app%site_height)
            eye = matmul(transpose(m), eye - spread(o, 2, 2))
            center = matmul(transpose(m), center - spread(o, 2, 2))
            up = matmul(transpose(m), up)
        end if
        do k = 1, 2
            if (at_site) then
                call app%views(ks(k))%set_view_frame(app%inertial, app%site)
            else
                call app%views(ks(k))%set_view_frame(app%inertial, app%moon)
            end if
            call app%views(ks(k))%set_default_view_parameters(eye=eye(:, k), center=center(:, k), up=up(:, k))
            call app%views(ks(k))%reset_view()
        end do
    end subroutine set_base_frame

    ! ==================================================================
    ! the window

    subroutine build_window()
        app%views(1) = View(app%inertial, app%moon, frame_type=ABSOLUTE_FRAME)   ! inertial axes: the Moon turns
        call app%views(1)%set_default_view_parameters(eye=3.2_wp * rm * unit(surface(-20.0_wp, tycho_lon + 25.0_wp, 0.0_wp)), &
                                                      center=[0.0_wp, 0.0_wp, 0.0_wp], up=[0.0_wp, 0.0_wp, 1.0_wp])
        app%views(2) = View(app%inertial, app%spacecraft)
        call app%views(2)%set_default_view_distance(12.0_wp * sc_size)
        app%views(3) = View(app%inertial, app%spacecraft, lookat=app%moon, frame_type=ABSOLUTE_FRAME)
        call app%views(3)%set_default_view_distance(30.0_wp * sc_size)
        app%views(5) = View(app%inertial, app%lander)
        call app%views(5)%set_default_view_distance(4.0_wp * lander_height)
        call set_base_frame(app%site_frame)                     ! views 4 and 6: Moon-fixed
        ! from the observer at the lander: the view frame's +y turns toward it (z kept up); the eye on the observer
        app%views(7) = View(app%inertial, app%observer, lookat=app%lander)
        call app%views(7)%set_default_view_parameters(eye=[0.0_wp, 0.0_wp, 0.0_wp], center=[0.0_wp, 1.0_wp, 0.0_wp], &
                                                      up=[0.0_wp, 0.0_wp, 1.0_wp])
        app%hud_lander = HUDText('', position=[0.99_wp, 0.01_wp], alignment=ALIGN_RIGHT_BOTTOM, size=16.0_wp, &
                                 color=lander_color)
        app%hud_view = HUDText(trim(view_names(app%start_view)), position=[0.01_wp, 0.99_wp], alignment=ALIGN_LEFT_TOP, &
                               size=16.0_wp, color=[0.8_wp, 0.8_wp, 0.8_wp])
        app%hud_time = HUDText('t = 0 s', position=[0.01_wp, 0.01_wp], alignment=ALIGN_LEFT_BOTTOM, size=18.0_wp, &
                               color=orbit_color)

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
        if (.not. app%proxy%open(app%window%width(), app%window%height(), 'vkframes moon', &
                                 native_window=app%window%winId(), pixel_ratio=app%window%devicePixelRatio())) then
            write(*, '(a)') 'moon demo: could not open the view'
            return
        end if
        if (app%proxy%num_views() > 0) return                    ! (opened again, on a new native window)
        call app%proxy%set_scene(app%scene)
        call app%proxy%set_event_handler(handler)
        call app%proxy%set_background_color([0.0_wp, 0.0_wp, 0.0_wp])
        call app%proxy%set_sky(app%sky)
        do k = 1, n_views
            call app%proxy%add_view(app%views(k))
        end do
        call app%proxy%select_view(app%start_view)
        call app%proxy%add_hud_text(app%hud_view)
        call app%proxy%add_hud_text(app%hud_time)
        call app%proxy%add_hud_text(app%hud_lander)
        call app%proxy%set_time_range(0.0_wp, day)
        call app%proxy%set_time_scale(app%time_scale%value())
        call app%proxy%pause_time(app%pause_time%isChecked())
        call app%proxy%set_time(app%start_time)
        call turn_moon(app%start_time)
    end subroutine open_view

    !> The scene is at the frame's time t: the altitude line's ends (its two
    !> artists written again now, to be in this frame) and the lander's label
    !> and HUD line.
    subroutine moon_before_draw(self, window, t)
        class(MoonHandler), intent(inout) :: self
        class(WindowProxy), intent(inout) :: window
        real(wp), intent(in) :: t
        real(wp) :: agl
        agl = update_drop(t)
        call app%drop_line%update(t)
        call app%drop_foot%update(t)
        call app%hud_lander%set_text(lander_status(t, agl))
        call label_lander(t, agl)
    end subroutine moon_before_draw

    subroutine on_expose(data)
        class(*), intent(inout), optional :: data
        if (app%proxy%is_open() .or. .not. app%window%isExposed()) return
        call open_view()
        if (app%proxy%is_open()) call app%window%requestUpdate()
    end subroutine on_expose

    !> Render a frame, turn the Moon to the new time, show the time, ask for the next frame.
    subroutine on_update(data)
        class(*), intent(inout), optional :: data
        character(len=48) :: text
        real(wp) :: t
        ! moved to a screen of another scale (or the display's scaling changed):
        ! Qt may send no resize, the window's size in window units being the same
        if (app%window%devicePixelRatio() /= app%proxy%pixel_ratio()) &
            call app%proxy%resize(nint(app%window%width() * app%window%devicePixelRatio()), &
                                  nint(app%window%height() * app%window%devicePixelRatio()), &
                                  pixel_ratio=app%window%devicePixelRatio())
        if (.not. app%proxy%frame()) return
        app%frame = app%frame + 1
        t = app%proxy%time()
        call turn_moon(t)
        write(text, '(a,f9.1,a,f6.2,a)') 'Simulation time: ', t, ' s (', t / app%period, ' orbits)'
        call app%time_label%setText(trim(text))
        write(text, '(a,f0.1,a)') 't = ', t, ' s'
        call app%hud_time%set_text(trim(text))
        if (.not. app%slider_held) then
            app%slider_setting = .true.
            call app%time_slider%setValue(nint(t))
            app%slider_setting = .false.
        end if
        if (app%options%after_frame(app%proxy, app%window, app%frame)) return   ! (a capture; --recreate-surface)
        call app%window%requestUpdate()
    end subroutine on_update

    ! ==================================================================
    ! controls

    subroutine on_view(index, data)
        integer, intent(in) :: index
        class(*), intent(inout), optional :: data
        call app%proxy%select_view(index + 1)
        call app%hud_view%set_text(trim(view_names(index + 1)))
    end subroutine on_view

    subroutine on_reset_view(data)
        class(*), intent(inout), optional :: data
        type(View), pointer :: v
        v => app%proxy%current_view()
        call v%reset_view()
    end subroutine on_reset_view

    subroutine on_time_scale(value, data)
        real(c_double), intent(in) :: value
        class(*), intent(inout), optional :: data
        call app%proxy%set_time_scale(value)
    end subroutine on_time_scale

    subroutine on_pause_time(checked, data)
        logical, intent(in) :: checked
        class(*), intent(inout), optional :: data
        call app%proxy%pause_time(checked)
    end subroutine on_pause_time

    subroutine on_reset_time(data)
        class(*), intent(inout), optional :: data
        call app%proxy%set_time(0.0_wp)
    end subroutine on_reset_time

    !> The time slider moved (dragged, clicked or with the keys): set the time.
    subroutine on_time_slider(value, data)
        integer, intent(in) :: value
        class(*), intent(inout), optional :: data
        if (app%slider_setting) return
        call app%proxy%set_time(real(value, wp))
    end subroutine on_time_slider

    subroutine on_slider_pressed(data)
        class(*), intent(inout), optional :: data
        app%slider_held = .true.
    end subroutine on_slider_pressed

    subroutine on_slider_released(data)
        class(*), intent(inout), optional :: data
        app%slider_held = .false.
    end subroutine on_slider_released

    !> The Sun's longitude (degrees): the terminator moves across the Moon.
    subroutine on_sun(value, data)
        integer, intent(in) :: value
        class(*), intent(inout), optional :: data
        character(len=24) :: text
        call set_sun_longitude(real(value, wp))
        write(text, '(i0,a)') value, ' deg'
        call app%sun_label%setText(trim(text))
    end subroutine on_sun

    subroutine on_terrain_lighting(checked, data)
        logical, intent(in) :: checked
        class(*), intent(inout), optional :: data
        call app%moon%set_lighting(checked)
    end subroutine on_terrain_lighting

    !> The lander's state for the HUD: coasting, descending (height above the
    !> site's ground and speed) or landed.
    function lander_status(t, agl) result(text)
        real(wp), intent(in) :: t
        real(wp), intent(in) :: agl                     !! altitude above the terrain below (km)
        character(len=:), allocatable :: text
        character(len=96) :: buf
        real(wp) :: r1(3), r2(3), h, h2
        if (t >= t_descent + descent_time) then
            text = 'Lander: landed in Tycho'
            return
        end if
        call lander_state(t, r1, h)
        call lander_state(t + 1.0_wp, r2, h2)
        if (t < t_descent) then
            write(buf, '(a,i0,a)') 'Lander: coasting, '//km(agl)//' km above the ground, descent in ', &
                nint(t_descent - t), ' s'
        else
            write(buf, '(a,i0,a)') 'Lander: '//lander_phase(t)//', '//km(agl)//' km above the ground, ', &
                nint(1000.0_wp * norm2(r2 - r1)), ' m/s'
        end if
        text = trim(buf)
    end function lander_status

    !> The lander's label (following it): its name and, in flight, its
    !> altitude above the terrain below it, e.g. 'Lander (11.17 km)'.
    subroutine label_lander(t, agl)
        real(wp), intent(in) :: t, agl
        character(len=:), allocatable :: text
        if (t >= t_descent + descent_time) then
            text = 'Lander'
        else
            text = 'Lander ('//km(agl)//' km)'
        end if
        if (text /= app%lander%name()) call app%lander%set_name(text)   ! (the label's text, rewritten in place)
    end subroutine label_lander

    !> A distance (km) with two decimals (and a leading zero, which f0.2 leaves out).
    function km(x) result(text)
        real(wp), intent(in) :: x
        character(len=:), allocatable :: text
        character(len=16) :: buf
        write(buf, '(f16.2)') x
        text = trim(adjustl(buf))
    end function km

    subroutine on_show_descent(checked, data)
        logical, intent(in) :: checked
        class(*), intent(inout), optional :: data
        call app%descent_frame%show_contents(checked)
    end subroutine on_show_descent

    subroutine on_site_base(checked, data)
        logical, intent(in) :: checked
        class(*), intent(inout), optional :: data
        call set_base_frame(checked)
    end subroutine on_site_base

    subroutine on_show_orbit(checked, data)
        logical, intent(in) :: checked
        class(*), intent(inout), optional :: data
        call app%orbit_frame%show_contents(checked)
    end subroutine on_show_orbit

    !> The main window closes: close the view (waiting for the GPU) first.
    subroutine on_close(event, data)
        type(QCloseEvent), intent(in) :: event
        class(*), intent(inout), optional :: data
        call app%proxy%close()
    end subroutine on_close

    subroutine build_ui()
        type(QDockWidget) :: dock
        type(QWidget) :: panel
        type(QFormLayout) :: form
        type(QPushButton) :: reset, reset_view

        app%main = QMainWindow()
        call app%main%setWindowTitle('vkframes: the Moon (Rocky terrain)')
        call app%main%closeEvent%connect(on_close)
        call build_window()
        call app%main%setCentralWidget(app%container)

        panel = QWidget()
        form = QFormLayout(panel)
        app%view_choice = QComboBox()
        call app%view_choice%addItems(view_names)
        call app%view_choice%setCurrentIndex(app%start_view - 1)
        call app%view_choice%currentIndexChanged%connect(on_view)
        call form%addRow('View', app%view_choice)
        reset_view = QPushButton('Reset view')
        call reset_view%clicked%connect(on_reset_view)
        call form%addRow('', reset_view)

        app%time_scale = QDoubleSpinBox()
        call app%time_scale%setRange(0.0_c_double, 5000.0_c_double)
        call app%time_scale%setValue(20.0_c_double)
        call app%time_scale%setSuffix(' x')
        call app%time_scale%valueChanged%connect(on_time_scale)
        call form%addRow('Time scale', app%time_scale)
        app%pause_time = QCheckBox('Pause time')
        call app%pause_time%toggled%connect(on_pause_time)
        call form%addRow('', app%pause_time)
        reset = QPushButton('Reset time')
        call reset%clicked%connect(on_reset_time)
        call form%addRow('', reset)
        app%time_label = QLabel('Simulation time:')
        call form%addRow(app%time_label)
        app%time_slider = QSlider(Qt_Horizontal)
        call app%time_slider%setRange(0, nint(day))
        call app%time_slider%setSingleStep(60)
        call app%time_slider%setPageStep(600)
        call app%time_slider%valueChanged%connect(on_time_slider)
        call app%time_slider%sliderPressed%connect(on_slider_pressed)
        call app%time_slider%sliderReleased%connect(on_slider_released)
        call form%addRow('Time', app%time_slider)

        app%sun_slider = QSlider(Qt_Horizontal)
        call app%sun_slider%setRange(-180, 180)
        app%sun_label = QLabel('')
        call app%sun_slider%valueChanged%connect(on_sun)
        call app%sun_slider%setValue(nint(app%start_sun))
        call form%addRow('Sun longitude', app%sun_slider)
        call form%addRow('', app%sun_label)
        app%terrain_lighting = QCheckBox('Light the terrain (Sun)')
        call app%terrain_lighting%setChecked(.true.)
        call app%terrain_lighting%toggled%connect(on_terrain_lighting)
        call form%addRow('', app%terrain_lighting)
        app%show_orbit = QCheckBox('Show orbit')
        call app%show_orbit%setChecked(.true.)
        call app%show_orbit%toggled%connect(on_show_orbit)
        call form%addRow('', app%show_orbit)
        app%show_descent = QCheckBox('Show descent')
        call app%show_descent%setChecked(.true.)
        call app%show_descent%toggled%connect(on_show_descent)
        call form%addRow('', app%show_descent)
        app%site_base = QCheckBox('Base frame at the landing site')
        call app%site_base%setChecked(app%site_frame)
        call app%site_base%toggled%connect(on_site_base)
        call form%addRow('', app%site_base)

        call form%addRow(QLabel('<small>Left drag: rotate<br>Right drag: zoom<br>Middle drag: pan<br>' // &
                                'Wheel: zoom<br>Space: reset the view</small>'))
        dock = QDockWidget('Controls', app%main)
        call dock%setWidget(panel)
        call app%main%addDockWidget(Qt_LeftDockWidgetArea, dock)
    end subroutine build_ui

end module moondemo

!> The lunar demo (see the module documentation).
program vkframes_moon_demo

    use qt
    use moondemo
    implicit none

    type(QApplication) :: qapp
    character(len=1024) :: arg
    integer :: i, ierr

    qapp = QApplication()

    if (.not. rocky_available()) then
        write(*, '(a)') 'moon demo: vkframes was built without Rocky (pixi run -e rocky rocky-build; run in the rocky environment)'
        stop 1
    end if
    i = 1
    app%options%capture_frame = 600                         ! (the descent well under way)
    do while (i <= command_argument_count())
        if (app%options%take(i)) cycle                      ! (--capture, --quit-after, ...: vkframes_qt)
        call get_command_argument(i, arg)
        select case (arg)
        case ('--view')
            call get_command_argument(i + 1, arg)
            read(arg, *) app%start_view
            app%start_view = max(1, min(n_views, app%start_view))
            i = i + 1
        case ('--site-frame')
            app%site_frame = .true.
        case ('--sun')
            call get_command_argument(i + 1, arg)
            read(arg, *) app%start_sun
            i = i + 1
        case ('--time')
            call get_command_argument(i + 1, arg)
            read(arg, *) app%start_time
            i = i + 1
        end select
        i = i + 1
    end do

    call build_model()
    call build_ui()
    call app%options%start_quit_timer(app%main)
    call app%main%resize(1200, 780)
    call app%main%show()
    ierr = qapp%exec()

end program vkframes_moon_demo
