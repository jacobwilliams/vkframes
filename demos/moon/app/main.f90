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
!> coordinates (a child of the Moon's frame), coasts in the same 100 km orbit, starts its
!> descent at t = 1200 s and lands on Tycho's floor north of the central peak
!> 12 minutes later (constant braking of its horizontal speed; altitude from
!> 100 km to the ground on a smooth curve, level at both ends), at the
!> terrain's height there (TerrainBody%elevation); its path is drawn too,
!> with its altitude above the terrain below it (a dashed line down to the
!> ground, the height in its label, which follows it, and in the HUD) and,
!> as it passes, a drop line to the ground every 10 s of its descent (the terrain's profile under the path:
!> Tycho's rim, then its central peak). At startup the demo prints how
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
    real(wp), parameter :: t_descent = 1200.0_wp, descent_time = 720.0_wp  !! s
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
        character(len=:), allocatable :: capture
        integer :: capture_frame = 600
        real(wp) :: quit_after = -1.0_wp
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

    !> The lander's position (km, Moon-fixed) and height above the site's ground
    !> at time t: on its meridian, northbound; coasting in the orbit until
    !> t_descent, then braking at a constant rate to stop over the site (no
    !> jump in speed), its altitude easing from the orbit's to the ground's.
    subroutine lander_state(t, r, height)
        real(wp), intent(in) :: t
        real(wp), intent(out) :: r(3), height
        real(wp) :: n, delta, angle, s, h
        n = 2.0_wp * pi / app%period                             ! the orbit's angular rate
        delta = 0.5_wp * n * descent_time                        ! the descent's arc (constant braking)
        if (t <= t_descent) then
            angle = site_lat * deg - delta - n * (t_descent - t)
            h = altitude
        else
            s = min(1.0_wp, (t - t_descent) / descent_time)
            angle = site_lat * deg - delta + delta * (2.0_wp * s - s**2)
            h = app%site_height + (altitude - app%site_height) * (1.0_wp - 3.0_wp * s**2 + 2.0_wp * s**3)
        end if
        r = (rm + h + lander_size) * [cos(angle) * cos(site_lon * deg), cos(angle) * sin(site_lon * deg), sin(angle)]
        height = h - app%site_height
    end subroutine lander_state

    !> The lander's attitude at time t: z up (radial), x north along its path.
    function lander_attitude(t) result(q)
        real(wp), intent(in) :: t
        real(wp) :: q(4)
        real(wp) :: r(3), h, x(3), y(3), z(3), lon
        call lander_state(t, r, h)
        lon = site_lon * deg
        z = r / norm2(r)
        x = [-z(3) * cos(lon), -z(3) * sin(lon), sqrt(z(1)**2 + z(2)**2)]   ! north (d/dlatitude of z)
        if (r(1) * cos(lon) + r(2) * sin(lon) < 0.0_wp) x(3) = -x(3)      ! (past a pole)
        x = x - dot_product(x, z) * z
        x = x / norm2(x)
        y = [z(2) * x(3) - z(3) * x(2), z(3) * x(1) - z(1) * x(3), z(1) * x(2) - z(2) * x(1)]
        q = quat_from_matrix(reshape([x, y, z], [3, 3]))
    end function lander_attitude

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
        call app%events%resizeEvent%connect(on_resize)
        call app%events%mousePressEvent%connect(on_press)
        call app%events%mouseReleaseEvent%connect(on_release)
        call app%events%mouseMoveEvent%connect(on_move)
        call app%events%wheelEvent%connect(on_wheel)
        call app%events%keyPressEvent%connect(on_key_press)
        call app%events%keyReleaseEvent%connect(on_key_release)
    end subroutine build_window

    !> Open the view (on the window's first expose) and show the scene.
    subroutine open_view()
        integer :: k
        if (.not. app%proxy%open(app%window%width(), app%window%height(), 'vkframes moon', &
                                 native_window=app%window%winId(), pixel_ratio=app%window%devicePixelRatio())) then
            write(*, '(a)') 'moon demo: could not open the view'
            return
        end if
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
        if (allocated(app%capture) .and. app%frame == app%capture_frame) then
            if (app%proxy%capture(app%capture)) write(*, '(a)') 'captured '//app%capture
        end if
        call app%window%requestUpdate()
    end subroutine on_update

    subroutine on_resize(event, data)
        type(QResizeEvent), intent(in) :: event
        class(*), intent(inout), optional :: data
        real(wp) :: dpr
        if (.not. app%proxy%is_open()) return
        dpr = app%window%devicePixelRatio()
        call app%proxy%resize(nint(event%width() * dpr), nint(event%height() * dpr), pixel_ratio=dpr)
        call app%window%requestUpdate()
    end subroutine on_resize

    ! ------------------------------------------------------------------
    ! mouse and keys, forwarded to the trackball (VSG's conventions)

    integer function vsg_button(qt_button)
        integer, intent(in) :: qt_button
        select case (qt_button)
        case (Qt_LeftButton)
            vsg_button = 1
        case (Qt_MiddleButton)
            vsg_button = 2
        case (Qt_RightButton)
            vsg_button = 3
        case default
            vsg_button = 0
        end select
    end function vsg_button

    integer function vsg_mask(qt_buttons)
        integer, intent(in) :: qt_buttons
        vsg_mask = 0
        if (iand(qt_buttons, Qt_LeftButton) /= 0) vsg_mask = ior(vsg_mask, 256)
        if (iand(qt_buttons, Qt_MiddleButton) /= 0) vsg_mask = ior(vsg_mask, 512)
        if (iand(qt_buttons, Qt_RightButton) /= 0) vsg_mask = ior(vsg_mask, 1024)
    end function vsg_mask

    subroutine forward_mouse(kind, event)
        integer, intent(in) :: kind
        type(QMouseEvent), intent(in) :: event
        real(wp) :: dpr
        dpr = app%window%devicePixelRatio()
        call app%proxy%mouse(kind, nint(event%x() * dpr), nint(event%y() * dpr), vsg_mask(event%buttons()), &
                             vsg_button(event%button()))
        call event%accept()
    end subroutine forward_mouse

    subroutine on_move(event, data)
        type(QMouseEvent), intent(in) :: event
        class(*), intent(inout), optional :: data
        call forward_mouse(MOUSE_MOVE, event)
    end subroutine on_move

    subroutine on_press(event, data)
        type(QMouseEvent), intent(in) :: event
        class(*), intent(inout), optional :: data
        call forward_mouse(MOUSE_PRESS, event)
    end subroutine on_press

    subroutine on_release(event, data)
        type(QMouseEvent), intent(in) :: event
        class(*), intent(inout), optional :: data
        call forward_mouse(MOUSE_RELEASE, event)
    end subroutine on_release

    subroutine on_wheel(event, data)
        type(QWheelEvent), intent(in) :: event
        class(*), intent(inout), optional :: data
        if (event%angleDeltaY() == 0) return
        call app%proxy%scroll(event%angleDeltaY() / 120.0_wp)     ! (wheel notches: a trackpad's are fractions)
        call event%accept()
    end subroutine on_wheel

    !> VSG key symbol for a Qt key (ASCII, or the X11-style codes VSG's trackball uses; 0 if unmapped).
    subroutine vsg_key(event, symbol, modified_symbol, modifier)
        type(QKeyEvent), intent(in) :: event
        integer, intent(out) :: symbol, modified_symbol, modifier
        character(len=:), allocatable :: text
        integer :: key, mods
        key = event%key()
        mods = event%modifiers()
        modifier = 0
        if (iand(mods, Qt_ShiftModifier) /= 0) modifier = ior(modifier, 1)
        if (iand(mods, Qt_ControlModifier) /= 0) modifier = ior(modifier, 4)
        if (iand(mods, Qt_AltModifier) /= 0) modifier = ior(modifier, 8)
        if (iand(mods, Qt_MetaModifier) /= 0) modifier = ior(modifier, 128)
        select case (key)
        case (Qt_Key_Left)
            symbol = int(z'FF51')
        case (Qt_Key_Up)
            symbol = int(z'FF52')
        case (Qt_Key_Right)
            symbol = int(z'FF53')
        case (Qt_Key_Down)
            symbol = int(z'FF54')
        case (Qt_Key_Home)
            symbol = int(z'FF50')
        case (Qt_Key_Escape)
            symbol = int(z'FF1B')
        case (Qt_Key_Return)
            symbol = int(z'FF0D')
        case (Qt_Key_Space)
            symbol = 32
        case (Qt_Key_A:Qt_Key_Z)
            symbol = key - Qt_Key_A + iachar('a')
        case default
            symbol = 0
            text = event%text()
            if (len(text) == 1) then
                if (iachar(text) >= 32 .and. iachar(text) < 127) symbol = iachar(text)
            end if
        end select
        modified_symbol = symbol
        if (key >= Qt_Key_A .and. key <= Qt_Key_Z .and. iand(modifier, 1) /= 0) modified_symbol = key
    end subroutine vsg_key

    subroutine forward_key(pressed, event)
        logical, intent(in) :: pressed
        type(QKeyEvent), intent(in) :: event
        integer :: symbol, modified_symbol, modifier
        call vsg_key(event, symbol, modified_symbol, modifier)
        if (symbol == 0) return
        call app%proxy%key(pressed, symbol, modified_symbol, modifier)
        call event%accept()
    end subroutine forward_key

    subroutine on_key_press(event, data)
        type(QKeyEvent), intent(in) :: event
        class(*), intent(inout), optional :: data
        call forward_key(.true., event)
    end subroutine on_key_press

    subroutine on_key_release(event, data)
        type(QKeyEvent), intent(in) :: event
        class(*), intent(inout), optional :: data
        call forward_key(.false., event)
    end subroutine on_key_release

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
            write(buf, '(a,i0,a)') 'Lander: descending, '//km(agl)//' km above the ground, ', &
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

    subroutine on_quit(data)
        class(*), intent(inout), optional :: data
        call app%main%close()
    end subroutine on_quit

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
    do while (i <= command_argument_count())
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
        case ('--capture')
            call get_command_argument(i + 1, arg)
            app%capture = trim(arg)
            i = i + 1
        case ('--capture-frame')
            call get_command_argument(i + 1, arg)
            read(arg, *) app%capture_frame
            i = i + 1
        case ('--quit-after')
            call get_command_argument(i + 1, arg)
            read(arg, *) app%quit_after
            i = i + 1
        end select
        i = i + 1
    end do

    call build_model()
    call build_ui()
    if (app%quit_after > 0) call QTimer_singleShot(int(1000 * app%quit_after), on_quit)
    call app%main%resize(1200, 780)
    call app%main%show()
    ierr = qapp%exec()

end program vkframes_moon_demo
