!> vkframes in a qt-fortran GUI: a spacecraft orbiting the Earth, rendered by
!> VulkanSceneGraph into QWindows: one embedded in a QMainWindow, next to a dock
!> of qt-fortran controls, and more on demand ("New window"), each showing the
!> same scene with its own view.
!>
!> The scene uses vkframes' API: the Earth is a Sphere (the WGS84 ellipsoid,
!> textured; its axes and labels, and the spacecraft's, can be hidden from the
!> dock; HUD text shows each window's view and the time), the spacecraft a Model (or a small Sphere without the data) that
!> follows a Trajectory (positions and attitudes over one period; LOOP or LIMIT
!> from the dock). The Earth rotates: it follows an attitude-only Trajectory
!> over one sidereal day, at a speed set with a slider. Both are children of an inertial root frame, so the
!> orbit stays inertial. A second spacecraft (a small Sphere) flies only part
!> of another orbit, from 20 to 70 minutes (it waits at its start and stops at
!> its end: FOLLOW_LIMIT), drawn with start and end markers. Instead of their
!> bodies, the spacecraft can be shown as markers at their current positions,
!> in a shape chosen from the dock (OpenFrames' way: a DrawableTrajectory child
!> of each spacecraft with a MarkerArtist and no trajectory, which draws one
!> marker at its frame's origin; the bodies' contents are hidden, their frames
!> still move and the views follow them). A line of sight between the two
!> spacecraft can be shown from the dock: a CustomLineSegments whose callback
!> sets its segment each frame, green while the line is clear of the Earth (and
!> 100 km of atmosphere), red when the Earth blocks it. A FrameManager
!> holds the scene. Each window has a WindowProxy showing it, with six Views
!> (the Earth; spacecraft 1 body-fixed; from spacecraft 1 toward the Earth;
!> spacecraft 2; from spacecraft 1 toward spacecraft 2, and back; chosen per
!> window). The main window's simulation time (time scale, pause and reset from
!> the dock) drives the others, whose clocks are synchronized to it; it runs
!> over three orbits, shown by a time slider, which follows the animation and
!> can be dragged (or clicked, or moved with the keys) to set the time. The orbit
!> is drawn by a CurveArtist in a DrawableTrajectory (a child of the inertial
!> frame), whole or traced up to the spacecraft (trace mode, from the dock),
!> solid or in a dash pattern (dashed, dotted, dash-dot: OpenGL stipples);
!> radius vectors (a SegmentArtist at every 12th point) are added to it and
!> removed while running; markers (a MarkerArtist: start, end, and every 10
!> minutes) in a shape chosen from the dock, optionally auto-attenuated. The
!> sky (a SkySphere, behind the scene in every window) shows the HYG v3 star
!> catalog (data/Stars_HYGv3.txt, found on VKFRAMES_DATA_PATH, which demo.py
!> sets) and, with OpenFrames' data, its StarMap.tif texture (chosen from the
!> dock). The Sun (a frame with a light source, an astronomical unit away)
!> lights the scene: the Earth's day texture on the day side, its night texture
!> (city lights) on the night side (data/2k_earth_daymap.jpg and
!> 2k_earth_nightmap.jpg); without it, the window's headlight. Reference shapes
!> can be shown from the dock: a latitude/longitude grid on the Earth (a
!> LatLonGrid), the inertial frame's coordinate axes with tick marks every
!> 1000 km (a CoordinateAxes at its center, fixed while the Earth's body-fixed
!> axes, from its surface, turn with it; its arrows hidden), the equatorial
!> plane (a RadialPlane) and spacecraft 1's nadir sensor (an EllipticCone).
!> The orbit's plane can be swept out from the dock (a RibbonArtist from the
!> Earth's center to the orbit, translucent, added and removed while running:
!> traced with the orbit, drawn whole otherwise).
!> The orbit can be colored by altitude from the dock (a color per point of
!> its trajectory, blended along the line: blue at perigee to red at apogee).
!> The Earth's shadow in sunlight (ShadowCones, a child of the Earth: umbra
!> and penumbra, out to 10 Earth radii) can be shown from the dock; it turns
!> away from the Sun as the Earth turns, and the spacecraft pass through it.
!> Changing a window's view flies its camera to the new view (a view
!> transition, set from the dock; 0 s: a cut), as does resetting it.
!> Each window has a WindowEventHandler (OpenFrames' window callbacks): `v` and
!> `V` show the next and previous views (the handler does it, so the window's
!> view menu follows, and says it handled the key), `p` pauses and resumes the
!> time, the space bar resets the view (vkframes' built-in key); the pointer's
!> position in the window (-1 to 1) is shown in the dock, and a click picks
!> what is under it (WindowProxy%pick): an orbit or marker (its frame, the time
!> there and the altitude; g goes to that time) or a body (the latitude and
!> longitude), shown in the dock and in a GUI panel at the point picked
!> (vkf_gui: Dear ImGui, kept there each frame in the handler's before_draw),
!> with buttons to go to the time there and to close it.
!>
!> How the pieces fit:
!>
!> * The host owns plain QWindows. On a window's first expose, its WindowProxy
!>   opens on the window's native handle (`winId()`): VSG renders into that
!>   window and leaves its events to Qt. All windows share one Vulkan device, so
!>   the scene's GPU data exist once.
!> * A QtfEventFilter per window forwards its events to its WindowProxy:
!>   resizes, and mouse, wheel and keys for the trackball, converted here to
!>   VSG's conventions (framebuffer pixels: logical * devicePixelRatio).
!> * Everything runs on the GUI thread (Vulkan has no current context, so there
!>   is no render thread and none of OpenGL's macOS workarounds): each window's
!>   `updateRequest` renders a frame (WindowProxy%frame: time, frames, draw)
!>   and asks for the next one, so frames follow the display's refresh.
!>
!> Run with `pixi run -e qt qt-demo` (from the vkframes root). Options:
!> `--data dir` OpenFrames' Demos folder, for the Earth texture and Hubble model
!> (default: $OPENFRAMES_DATA); `--capture file` saves a frame of each window as
!> an image (file, then file_2, ... for the other windows; `--capture-frame n`,
!> default 120); `--quit-after seconds` closes the demo; `--test-resize`
!> resizes the main window after 2 s; `--view n` starts the main window in view
!> n (1 to 6, as in the view menu); `--windows n` opens n
!> windows at startup (the others start in the next views); `--rotation-speed n`
!> sets the Earth's rotation speed slider (0 to 50 x the real rate); `--trace`
!> starts with the orbit traced; `--plane` with the orbit's plane swept out; `--altitude-colors` with the orbit colored by
!> altitude; `--radius` with the radius vectors shown;
!> `--marker n` with marker shape n (0 none, 1 disk, ... 10 spiral);
!> `--position-marker n` with the spacecraft shown as marker shape n (0: their
!> bodies; 1 disk, ... 10 spiral); `--sky n`
!> with sky n (0 none, 1 stars, 2 texture, 3 both); `--shapes` with the grid,
!> coordinate axes, plane and sensor cone shown; `--los` with the line of
!> sight between the spacecraft shown; `--shadow` with the Earth's shadow
!> shown; `--pattern n` with orbit
!> pattern n (0 solid, 1 dashed, 2 dotted, 3 dash-dot).
module vkdemo

    use qt
    use vkframes
    use vkf_text, only: get_font
    use iso_c_binding
    implicit none

    real(wp), parameter :: mu = 398600.4418_wp                  !! Earth's gravitational parameter (km^3/s^2)
    real(wp), parameter :: re = 6378.137_wp                     !! WGS84 equatorial radius (km)
    real(wp), parameter :: flattening = 1.0_wp / 298.257223563_wp
    real(wp), parameter :: sidereal_day = 86164.0905_wp            !! s
    real(wp), parameter :: sma = 9000.0_wp, ecc = 0.2_wp, inc = 35.0_wp * pi / 180.0_wp
    real(wp), parameter :: model_size = 800.0_wp                 !! spacecraft bounding radius (km, enlarged to be seen)
    integer, parameter :: n_orbit = 721
    integer, parameter :: max_windows = 4
    real(wp), parameter :: orbit_color(3) = [1.0_wp, 0.85_wp, 0.2_wp]
    ! the second spacecraft's orbit: part of it, from t2_start to t2_end (s)
    real(wp), parameter :: sma2 = 7800.0_wp, ecc2 = 0.05_wp, inc2 = 70.0_wp * pi / 180.0_wp, raan2 = 60.0_wp * pi / 180.0_wp
    real(wp), parameter :: t2_start = 1200.0_wp, t2_end = 4200.0_wp
    integer, parameter :: n_orbit2 = 301
    real(wp), parameter :: orbit2_color(3) = [0.3_wp, 1.0_wp, 0.8_wp]
    integer, parameter :: n_views = 6
    character(len=28), parameter :: view_names(n_views) = [character(len=28) :: 'Earth', 'Spacecraft 1', &
        'Spacecraft 1 -> Earth', 'Spacecraft 2', 'Spacecraft 1 -> Spacecraft 2', 'Spacecraft 2 -> Spacecraft 1']

    !> The line of sight between the spacecraft (CustomLineSegments' callback,
    !> called each frame after the tree's update): green if clear of the Earth,
    !> red if blocked.
    type, extends(LineSegmentsCallback) :: LineOfSightLink
    contains
        procedure :: compute => line_of_sight_compute
    end type LineOfSightLink

    !> A window's keyboard and mouse (vkframes' WindowEventHandler, OpenFrames'
    !> window callbacks): keys for the views and the time, the pointer shown
    !> in the dock.
    type, extends(WindowEventHandler) :: DemoEvents
        integer :: window = 0                                  !! the window's index (app%windows)
    contains
        procedure :: key_press => demo_key_press
        procedure :: mouse_motion => demo_mouse_motion
        procedure :: button_press => demo_button_press
        procedure :: before_draw => demo_before_draw
    end type DemoEvents

    !> One window showing the scene: a QWindow (in the main window, or in a
    !> top-level widget of its own) with its WindowProxy and views.
    type :: view_window_t
        integer :: index = 0                                   !! 1: the main window's
        logical :: used = .false.
        type(QWidget) :: top                                   !! the top-level widget (windows 2, 3, ...)
        type(QWindow) :: window
        type(QWidget) :: container
        type(QtfEventFilter) :: events
        type(QComboBox) :: view_choice
        type(WindowProxy) :: proxy
        type(View) :: views(n_views)                           !! as view_names
        type(HUDText) :: hud_view                              !! the view's name, top left
        type(DemoEvents) :: events_handler                     !! its keys and mouse
        integer :: frame = 0
    end type view_window_t

    !> The GUI and the scene.
    type :: app_t
        type(QMainWindow) :: main
        type(QStackedWidget) :: stack                          !! the main window's center: a startup message, then the view
        type(QLabel) :: startup_message
        type(QDoubleSpinBox) :: time_scale
        type(QDoubleSpinBox) :: view_transition                !! seconds to fly between views (0: a cut)
        type(QCheckBox) :: pause_time
        type(QDoubleSpinBox) :: line_width
        type(QComboBox) :: follow_mode
        type(QCheckBox) :: show_axes, show_labels, show_hud, trace_orbit, show_radius, attenuate, altitude_colors, show_plane_swept
        type(QCheckBox) :: sun_light, show_grid, show_plane, show_sensor, show_ticks, show_los, show_shadow
        type(QComboBox) :: marker_shape                        !! none, or a MARKER_* shape
        type(QComboBox) :: position_marker                     !! the spacecraft: bodies, or a MARKER_* shape
        type(QComboBox) :: sky_choice                          !! none, stars, texture, both
        type(QComboBox) :: line_pattern                        !! the orbits' dash pattern
        type(HUDText) :: hud_time                              !! the simulation time, bottom left of every window
        type(QSlider) :: rotation_speed                        !! the Earth's rotation, in multiples of the real rate
        type(QLabel) :: rotation_label
        type(QLabel) :: time_label
        type(QLabel) :: pointer_label                          !! the pointer and the last click (DemoEvents)
        character(len=160) :: last_click = ''
        real(wp) :: picked_time = 0.0_wp                       !! the time at the last orbit or marker picked
        logical :: have_picked_time = .false.
        type(QSlider) :: time_slider                           !! the simulation time (s), 0 to time_end
        logical :: slider_held = .false.                       !! the user holds the slider's handle
        logical :: slider_setting = .false.                    !! the demo moves the slider (to the time)
        real(wp) :: time_end = 0.0_wp                          !! the end of the time range: three orbits
        type(view_window_t) :: windows(max_windows)
        ! the scene
        type(Trajectory) :: orbit                              !! one period of positions and attitudes
        type(ReferenceFrame) :: inertial                       !! the root: inertial axes at the Earth's center
        type(Sphere) :: earth                                  !! rotating (follows earth_rotation)
        type(LatLonGrid) :: grid                               !! on the Earth (rotates with it)
        type(CoordinateAxes) :: ticks                          !! the inertial frame's axes, with tick marks
        type(RadialPlane) :: equator                           !! the equatorial plane (inertial)
        type(EllipticCone) :: sensor                           !! spacecraft 1's nadir sensor
        type(ReferenceFrame) :: sun                            !! the light source
        type(ShadowCones) :: earth_shadow                      !! the Earth's shadow in the Sun's light
        type(Trajectory) :: earth_rotation                     !! attitudes over one sidereal day
        real(wp) :: rotation_time = 0.0_wp                     !! time along earth_rotation (advances at speed x)
        real(wp) :: last_time = 0.0_wp                         !! the simulation time at the last frame
        class(ReferenceFrame), allocatable :: spacecraft      !! a Model, or a Sphere without the data
        type(FrameManager) :: scene
        type(DrawableTrajectory) :: orbit_frame                !! draws the orbit (in inertial coordinates)
        type(CurveArtist) :: orbit_curve
        type(SegmentArtist) :: radius_vectors                  !! from the Earth's center, added from the dock
        type(RibbonArtist) :: orbit_plane                      !! swept from the Earth's center, added from the dock
        type(MarkerArtist) :: orbit_markers                    !! start, end, every 10 minutes
        ! the second spacecraft, on part of another orbit
        type(Trajectory) :: orbit2
        type(Sphere) :: spacecraft2
        type(DrawableTrajectory) :: orbit2_frame
        type(CurveArtist) :: orbit2_curve
        type(MarkerArtist) :: orbit2_markers                   !! start and end
        ! the spacecraft's current positions as markers (instead of their bodies):
        ! a marker artist without a trajectory draws one marker at its frame's
        ! origin, in a frame that is a child of each spacecraft (OpenFrames'
        ! way: its oftrajectoryfollower demo)
        type(DrawableTrajectory) :: position_frame1, position_frame2
        type(MarkerArtist) :: position_marker1, position_marker2
        ! the line of sight between the spacecraft
        type(CustomLineSegments) :: line_of_sight
        type(LineOfSightLink) :: line_of_sight_link
        type(SkySphere) :: sky                                 !! stars and (with OpenFrames' data) a texture
        type(ReferenceFrame) :: picked                         !! at the point picked (where its GUI panel is)
        character(len=96) :: picked_text = ''                  !! the panel's readout
        integer :: picked_window = 0                           !! the window it was picked in (0: none)
        ! options
        character(len=:), allocatable :: data_dir, capture
        integer :: capture_frame = 120
        integer :: start_view = 1
        integer :: start_windows = 1
        integer :: start_rotation_speed = 1
        real(wp) :: quit_after = -1.0_wp
        logical :: test_resize = .false.
        logical :: start_trace = .false.
        logical :: start_altitude_colors = .false.
        logical :: start_radius = .false.
        logical :: start_plane = .false.
        integer :: start_marker = 1
        integer :: start_position_marker = 0
        integer :: start_sky = 1
        integer :: start_pattern = 0
        logical :: start_shapes = .false.
        logical :: start_los = .false.
        logical :: start_shadow = .false.
    end type app_t

    !> The application (module-level, so that windows created later from slots
    !> are targets too: frames, views and windows keep pointers to each other).
    type(app_t), target, save :: app

contains

    ! ==================================================================
    ! the scene

    !> Position at time t (km) on the orbit of semi-major axis a, eccentricity
    !> e, inclination i and ascending node raan (default 0), at perigee at t = 0
    !> (Kepler's equation by Newton iteration).
    !> A color for x in [0, 1] (altitude from perigee to apogee): blue, cyan,
    !> green, yellow, red.
    pure function altitude_color(x) result(c)
        real(wp), intent(in) :: x
        real(wp) :: c(3)
        real(wp), parameter :: stops(3, 5) = reshape([0.2_wp, 0.3_wp, 1.0_wp, 0.0_wp, 0.9_wp, 1.0_wp, &
                                                       0.2_wp, 1.0_wp, 0.3_wp, 1.0_wp, 0.9_wp, 0.1_wp, &
                                                       1.0_wp, 0.15_wp, 0.1_wp], [3, 5])
        real(wp) :: s
        integer :: k
        s = 4.0_wp * max(0.0_wp, min(1.0_wp, x))
        k = min(4, int(s) + 1)
        s = s - real(k - 1, wp)
        c = stops(:, k) + s * (stops(:, k + 1) - stops(:, k))
    end function altitude_color

    pure function orbit_position(t, a, e, i, raan) result(r)
        real(wp), intent(in) :: t, a, e, i
        real(wp), intent(in), optional :: raan
        real(wp) :: r(3)
        real(wp) :: m, ea, x, y, node
        integer :: k
        m = sqrt(mu / a**3) * t
        ea = m
        do k = 1, 20
            ea = ea - (ea - e * sin(ea) - m) / (1.0_wp - e * cos(ea))
        end do
        x = a * (cos(ea) - e)
        y = a * sqrt(1.0_wp - e**2) * sin(ea)
        r = [x, y * cos(i), y * sin(i)]
        node = 0.0_wp
        if (present(raan)) node = raan
        r = [cos(node) * r(1) - sin(node) * r(2), sin(node) * r(1) + cos(node) * r(2), r(3)]
    end function orbit_position

    !> The spacecraft's attitude at orbit position r: its x axis radial (from
    !> the Earth), z along the orbit normal.
    pure function orbit_attitude(r) result(q)
        real(wp), intent(in) :: r(3)
        real(wp) :: q(4)
        real(wp) :: x, y
        x = r(1)                                   ! in the orbit plane (before the inclination)
        y = r(2) * cos(inc) + r(3) * sin(inc)
        q = quat_multiply(quat_from_axis_angle([1.0_wp, 0.0_wp, 0.0_wp], inc), &
                          quat_from_axis_angle([0.0_wp, 0.0_wp, 1.0_wp], atan2(y, x)))
    end function orbit_attitude

    !> The scene: the orbit trajectory (one period), the Earth, the spacecraft
    !> following the orbit, and the orbit's curve.
    subroutine build_model()
        real(wp) :: period, t, r(3), center(3), radius
        logical :: ok
        integer :: k

        app%orbit = Trajectory()
        call app%orbit%reserve(n_orbit, attitudes=.true.)
        period = 2.0_wp * pi * sqrt(sma**3 / mu)
        app%time_end = 3.0_wp * period
        do k = 1, n_orbit
            t = period * real(k - 1, wp) / real(n_orbit - 1, wp)
            r = orbit_position(t, sma, ecc, inc)
            call app%orbit%add(t, pos=r, att=orbit_attitude(r), &         ! (colors drawn from the dock)
                               color=altitude_color((norm2(r) - sma * (1.0_wp - ecc)) / (2.0_wp * sma * ecc)))
        end do

        app%earth = Sphere('Earth', radius=re, color=[0.3_wp, 0.5_wp, 1.0_wp])
        call app%earth%set_sphere_scale([1.0_wp, 1.0_wp, 1.0_wp - flattening])
        call app%earth%set_texture_map('2k_earth_daymap.jpg')          ! (data/, on VKFRAMES_DATA_PATH)
        call app%earth%set_night_texture_map('2k_earth_nightmap.jpg')
        call app%earth%set_material(specular=[0.3_wp, 0.3_wp, 0.3_wp], shininess=30.0_wp)

        ! a latitude/longitude grid just above the Earth's surface, every 15 degrees
        app%grid = LatLonGrid('Grid', color=[0.6_wp, 0.8_wp, 1.0_wp, 0.4_wp])
        call app%grid%set_parameters(1.002_wp * re, 1.002_wp * re, 1.002_wp * (1.0_wp - flattening) * re, &
                                     15.0_wp * pi / 180.0_wp, 15.0_wp * pi / 180.0_wp)
        call hide_decorations(app%grid)
        call app%grid%show_contents(.false.)
        if (.not. app%earth%add_child(app%grid)) write(*, '(a)') 'vkframes: add_child failed'

        ! the inertial frame's coordinate axes: from its center (the Earth's), 2
        ! Earth radii, ticks every 1000 km; fixed while the Earth turns; their
        ! arrows hidden (the Earth's body-fixed ones, from its surface, show)
        app%ticks = CoordinateAxes('Ticks', color=[0.85_wp, 0.85_wp, 0.85_wp])
        call app%ticks%set_axis_length(2.0_wp * re)
        call app%ticks%set_tick_spacing(1000.0_wp, 250.0_wp)
        call hide_decorations(app%ticks)
        call app%ticks%show_contents(.false.)


        ! the spacecraft: OpenFrames' Hubble model, or a small sphere without the data
        ok = .false.
        if (len(app%data_dir) > 0) then
            allocate(app%spacecraft, source=Model('Spacecraft 1', color=[1.0_wp, 0.85_wp, 0.2_wp]))
            select type (sc => app%spacecraft)
            type is (Model)
                ok = sc%set_model(app%data_dir//'/Models/Hubble.3ds')
                if (ok) then
                    call sc%model_bound(center, radius)
                    call sc%set_model_pivot(center)
                    call sc%set_model_scale([1.0_wp, 1.0_wp, 1.0_wp] * model_size / radius)
                end if
            end select
            if (.not. ok) deallocate(app%spacecraft)
        end if
        if (.not. ok) allocate(app%spacecraft, source=Sphere('Spacecraft 1', radius=model_size, &
                                                             color=[1.0_wp, 0.3_wp, 0.2_wp]))
        call app%spacecraft%follow(app%orbit)                 ! position and attitude, FOLLOW_LOOP
        call axes_from_center(app%spacecraft, 2.0_wp * model_size)

        ! the Earth's rotation: attitudes about z over one sidereal day, every 15
        ! degrees (slerp in between), looped; attitude only
        app%earth_rotation = Trajectory()
        do k = 0, 24
            call app%earth_rotation%add(sidereal_day * real(k, wp) / 24.0_wp, &
                                        att=quat_from_axis_angle([0.0_wp, 0.0_wp, 1.0_wp], 2.0_wp * pi * real(k, wp) / 24.0_wp))
        end do
        call app%earth%follow(app%earth_rotation, data=FOLLOW_ATTITUDE)
        call set_rotation_time(0.0_wp)            ! the rotation's own time (see advance_rotation)

        ! the orbit's curve, in a frame of its own (no decorations)
        app%orbit_curve = CurveArtist(app%orbit, color=orbit_color, width=2.0_wp)
        call app%orbit_curve%set_point_colors(.false.)                   ! (its own color until chosen)
        app%orbit_frame = DrawableTrajectory('Orbit')
        call app%orbit_frame%show_axes(NO_AXES)
        call app%orbit_frame%show_axes_labels(NO_AXES)
        call app%orbit_frame%show_name_label(.false.)
        call app%orbit_frame%add_artist(app%orbit_curve)
        app%radius_vectors = SegmentArtist(app%orbit, color=[0.4_wp, 0.8_wp, 1.0_wp, 0.8_wp], stride=12)
        app%orbit_plane = RibbonArtist(app%orbit, color=[orbit_color(1:3), 0.2_wp])
        call app%orbit_plane%set_point_colors(.false.)                   ! (with the orbit's, by altitude)
        app%orbit_markers = MarkerArtist(app%orbit, markers=ior(ior(MARKER_START, MARKER_INTERMEDIATE), MARKER_END), &
                                         size=12.0_wp)
        call app%orbit_markers%set_marker_color(MARKER_START, [0.2_wp, 1.0_wp, 0.3_wp])
        call app%orbit_markers%set_marker_color(MARKER_INTERMEDIATE, orbit_color)
        call app%orbit_markers%set_intermediate_type(INTERMEDIATE_TIME)
        call app%orbit_markers%set_intermediate_spacing(600.0_wp)
        call app%orbit_frame%add_artist(app%orbit_markers)

        ! the second spacecraft: part of another orbit (positions only), waiting
        ! at its start before t2_start and stopping at its end
        app%orbit2 = Trajectory()
        call app%orbit2%reserve(n_orbit2)
        do k = 1, n_orbit2
            t = t2_start + (t2_end - t2_start) * real(k - 1, wp) / real(n_orbit2 - 1, wp)
            call app%orbit2%add(t, pos=orbit_position(t, sma2, ecc2, inc2, raan2))
        end do
        app%spacecraft2 = Sphere('Spacecraft 2', radius=0.6_wp * model_size, color=orbit2_color)
        call axes_from_center(app%spacecraft2, 1.2_wp * model_size)
        call app%spacecraft2%follow(app%orbit2, data=FOLLOW_POSITION, mode=FOLLOW_LIMIT)
        app%orbit2_curve = CurveArtist(app%orbit2, color=orbit2_color, width=2.0_wp)
        app%orbit2_markers = MarkerArtist(app%orbit2, size=16.0_wp)            ! start and end (default)
        call app%orbit2_markers%set_marker_color(MARKER_START, [0.2_wp, 1.0_wp, 0.3_wp])
        call app%orbit2_markers%set_marker_color(MARKER_END, [1.0_wp, 0.25_wp, 0.2_wp])
        app%orbit2_frame = DrawableTrajectory('Orbit 2')
        call app%orbit2_frame%show_axes(NO_AXES)
        call app%orbit2_frame%show_axes_labels(NO_AXES)
        call app%orbit2_frame%show_name_label(.false.)
        call app%orbit2_frame%add_artist(app%orbit2_curve)
        call app%orbit2_frame%add_artist(app%orbit2_markers)

        ! the equatorial plane, out to 2.5 Earth radii
        app%equator = RadialPlane('Equator')
        call app%equator%set_parameters(2.5_wp * re, re, 30.0_wp * pi / 180.0_wp)
        call app%equator%set_line_color([1.0_wp, 1.0_wp, 1.0_wp, 0.4_wp])
        call hide_decorations(app%equator)
        call app%equator%show_contents(.false.)

        ! spacecraft 1's sensor: its boresight (-z) along the body's -x (nadir)
        app%sensor = EllipticCone('Sensor', color=[0.3_wp, 1.0_wp, 0.4_wp, 0.3_wp])
        call app%sensor%set_primary_angles(35.0_wp * pi / 180.0_wp, 20.0_wp * pi / 180.0_wp)
        call app%sensor%set_cone_length(0.6_wp * sma)
        call app%sensor%set_attitude(quat_from_axis_angle([0.0_wp, 1.0_wp, 0.0_wp], 0.5_wp * pi))
        call hide_decorations(app%sensor)
        call app%sensor%show_contents(.false.)
        if (.not. app%spacecraft%add_child(app%sensor)) write(*, '(a)') 'vkframes: add_child failed'

        ! the spacecraft's current positions as markers (shown instead of the
        ! bodies from the dock): a frame at each spacecraft's origin with a marker
        ! artist and no trajectory (a start marker at the origin)
        call position_marker_frame(app%position_frame1, app%position_marker1, 'Spacecraft 1 marker', orbit_color)
        call position_marker_frame(app%position_frame2, app%position_marker2, 'Spacecraft 2 marker', orbit2_color)
        if (.not. (app%spacecraft%add_child(app%position_frame1) .and. app%spacecraft2%add_child(app%position_frame2))) &
            write(*, '(a)') 'vkframes: add_child failed'

        ! the line of sight between the spacecraft (inertial; hidden at first)
        app%line_of_sight = CustomLineSegments('Line of sight', width=2.5_wp)
        call hide_decorations(app%line_of_sight)
        call app%line_of_sight%set_callback(app%line_of_sight_link)
        call app%line_of_sight%show_contents(.false.)

        ! the Sun: a light source an astronomical unit away (inertial)
        app%sun = ReferenceFrame('Sun')
        call app%sun%set_position(1.495978707e8_wp * [cos(0.4_wp), -sin(0.4_wp), 0.2_wp])
        call hide_decorations(app%sun)
        call app%sun%set_light_source_enabled(.true.)
        call app%sun%set_light_diffuse([1.0_wp, 1.0_wp, 1.0_wp])
        call app%sun%set_light_ambient([0.08_wp, 0.08_wp, 0.08_wp])
        call app%sun%set_light_specular([1.0_wp, 1.0_wp, 0.9_wp])

        ! the Earth's shadow: a child of the Earth, pointing away from the Sun
        ! (a plain frame here: its radius given), out to 10 Earth radii (hidden at first)
        app%earth_shadow = ShadowCones('Earth shadow', body=app%earth, light=app%sun, light_radius=695700.0_wp)
        call app%earth_shadow%set_length(10.0_wp * re)
        call app%earth_shadow%show_contents(.false.)
        if (.not. app%earth%add_child(app%earth_shadow)) write(*, '(a)') 'vkframes: add_child failed'

        ! the tree: an inertial root (no decorations of its own) with the
        ! rotating Earth, the spacecraft and their orbits (all inertial)
        app%inertial = ReferenceFrame('Inertial')
        call app%inertial%show_axes(NO_AXES)
        call app%inertial%show_axes_labels(NO_AXES)
        call app%inertial%show_name_label(.false.)
        if (.not. (app%inertial%add_child(app%earth) .and. app%inertial%add_child(app%spacecraft) .and. &
                   app%inertial%add_child(app%orbit_frame) .and. app%inertial%add_child(app%spacecraft2) .and. &
                   app%inertial%add_child(app%orbit2_frame) .and. app%inertial%add_child(app%equator) .and. &
                   app%inertial%add_child(app%sun) .and. app%inertial%add_child(app%ticks) .and. &
                   app%inertial%add_child(app%line_of_sight))) &
            write(*, '(a)') 'vkframes: add_child failed'
        ! the readout of a click (a callout): a frame moved to the point picked,
        ! whose name label is the readout, on top of the scene
        app%picked = ReferenceFrame('Picked')
        call app%picked%show_axes(NO_AXES)
        call app%picked%show_axes_labels(NO_AXES)
        call app%picked%show_name_label(.false.)
        call app%picked%set_name_label_on_top(.true.)
        call app%picked%set_label_size(15)
        call app%picked%set_label_alignment(ALIGN_LEFT_CENTER)
        if (.not. app%inertial%add_child(app%picked)) write(*, '(a)') 'vkframes: add_child failed'
        call app%scene%set_frame(app%inertial)
        write(*, '(a)', advance='no') app%inertial%frame_string()

        ! the sky: OpenFrames' star catalog (magnitudes -2 to 6, as its demos),
        ! and its star map image if available
        if (.not. app%sky%set_star_data('Stars_HYGv3.txt', min_mag=-2.0_wp, max_mag=6.0_wp, max_stars=40000)) &
            write(*, '(a)') 'vkframes demo: no star catalog'
        if (len(app%data_dir) > 0) then
            ok = app%sky%set_texture_map(app%data_dir//'/Images/StarMap.tif')     ! (a message if missing)
        end if
        write(*, '(a,i0,a)') 'sky: ', app%sky%num_stars(), ' stars'
    end subroutine build_model

    !> A frame (no decorations, contents hidden at first) with a marker artist
    !> drawing one marker of `color` at its origin.
    subroutine position_marker_frame(frame, marker, name, color)
        type(DrawableTrajectory), intent(inout), target :: frame
        type(MarkerArtist), intent(inout), target :: marker
        character(len=*), intent(in) :: name
        real(wp), intent(in) :: color(3)
        marker = MarkerArtist(markers=MARKER_START, size=18.0_wp, color=color)
        frame = DrawableTrajectory(name)
        call hide_decorations(frame)
        call frame%add_artist(marker)
        call frame%show_contents(.false.)
    end subroutine position_marker_frame

    !> The line of sight from spacecraft 1 to spacecraft 2 at time t (both at
    !> their positions for t), green if it stays more than 100 km above the
    !> Earth, red if the Earth blocks it.
    subroutine line_of_sight_compute(self, segments, t)
        class(LineOfSightLink), intent(inout) :: self
        class(CustomLineSegments), intent(inout) :: segments
        real(wp), intent(in) :: t
        real(wp) :: a(3, 1), b(3, 1), d(3), s, color(4, 1)
        a(:, 1) = segments%position_of(app%spacecraft)
        b(:, 1) = segments%position_of(app%spacecraft2)
        d = b(:, 1) - a(:, 1)
        s = 0.0_wp                                  ! the closest point to the Earth's center
        if (dot_product(d, d) > 0.0_wp) s = max(0.0_wp, min(1.0_wp, -dot_product(a(:, 1), d) / dot_product(d, d)))
        if (norm2(a(:, 1) + s * d) > re + 100.0_wp) then
            color(:, 1) = [0.3_wp, 1.0_wp, 0.4_wp, 1.0_wp]
        else
            color(:, 1) = [1.0_wp, 0.3_wp, 0.25_wp, 0.8_wp]
        end if
        call segments%set_segments(a, b, color)
    end subroutine line_of_sight_compute

    !> A spacecraft's body axes from its center, `length` long (beyond its
    !> body; a Sphere or Model starts them at its surface by default).
    subroutine axes_from_center(frame, length)
        class(ReferenceFrame), intent(inout) :: frame
        real(wp), intent(in) :: length
        real(wp), parameter :: o(3) = 0.0_wp
        ! (thinner than the default, 5% and 10% of the length, for long axes)
        call frame%move_x_axis(o, length, body_radius=0.015_wp * length, head_radius=0.04_wp * length)
        call frame%move_y_axis(o, length, body_radius=0.015_wp * length, head_radius=0.04_wp * length)
        call frame%move_z_axis(o, length, body_radius=0.015_wp * length, head_radius=0.04_wp * length)
    end subroutine axes_from_center

    !> No axes or labels (for the reference shapes and the Sun).
    subroutine hide_decorations(frame)
        class(ReferenceFrame), intent(inout) :: frame
        call frame%show_axes(NO_AXES)
        call frame%show_axes_labels(NO_AXES)
        call frame%show_name_label(.false.)
    end subroutine hide_decorations

    !> The Earth follows its rotation trajectory at a fixed time, which the demo
    !> advances itself: a speed change then changes the rotation rate without
    !> a jump in the Earth's attitude.
    subroutine set_rotation_time(t)
        real(wp), intent(in) :: t
        type(TrajectoryFollower), pointer :: follower
        app%rotation_time = t
        follower => app%earth%follower()
        call follower%set_time(t)
    end subroutine set_rotation_time

    !> After a frame of the main window: advance the rotation time by the
    !> simulation time elapsed, times the speed from the slider.
    subroutine advance_rotation(t)
        real(wp), intent(in) :: t
        call set_rotation_time(app%rotation_time + real(app%rotation_speed%value(), wp) * (t - app%last_time))
        app%last_time = t
    end subroutine advance_rotation

    ! ==================================================================
    ! windows

    !> Set up window `w` (index k): its QWindow, event forwarding and views.
    !> Window 1 goes in the main window; the others in top-level widgets.
    subroutine build_window(k, start_view)
        integer, intent(in) :: k, start_view
        type(view_window_t), pointer :: w
        type(QVBoxLayout) :: layout
        character(len=12) :: title
        w => app%windows(k)
        w%index = k
        w%used = .true.
        w%frame = 0

        ! views: the Earth, spacecraft 1 (body-fixed), from spacecraft 1 toward
        ! the Earth (inertial axes, a little behind the spacecraft), spacecraft
        ! 2, and from each spacecraft toward the other
        w%views(1) = View(app%inertial, app%earth, frame_type=ABSOLUTE_FRAME)   ! inertial axes: the Earth turns
        call w%views(1)%set_default_view_distance(2.5_wp * sma)                ! the whole orbit in view
        w%views(2) = View(app%inertial, app%spacecraft)
        w%views(3) = View(app%inertial, app%spacecraft, lookat=app%earth, frame_type=ABSOLUTE_FRAME)
        call w%views(3)%set_default_view_distance(1.5_wp * model_size)
        w%views(4) = View(app%inertial, app%spacecraft2)
        call w%views(4)%set_default_view_distance(8.0_wp * model_size)
        ! (behind and above the spacecraft, looking past it: the other one is
        ! along +y, just above the view's center, clear of this one)
        w%views(5) = View(app%inertial, app%spacecraft, lookat=app%spacecraft2, frame_type=ABSOLUTE_FRAME)
        call w%views(5)%set_default_view_parameters(eye=[0.0_wp, -4.0_wp, 2.0_wp] * model_size, &
                                                    center=[0.0_wp, 10.0_wp, 0.0_wp] * model_size, up=[0.0_wp, 0.0_wp, 1.0_wp])
        w%views(6) = View(app%inertial, app%spacecraft2, lookat=app%spacecraft, frame_type=ABSOLUTE_FRAME)
        call w%views(6)%set_default_view_parameters(eye=[0.0_wp, -2.4_wp, 1.2_wp] * model_size, &
                                                    center=[0.0_wp, 6.0_wp, 0.0_wp] * model_size, up=[0.0_wp, 0.0_wp, 1.0_wp])
        w%hud_view = HUDText(trim(view_names(modulo(start_view - 1, n_views) + 1)), position=[0.01_wp, 0.99_wp], &
                             alignment=ALIGN_LEFT_TOP, size=16.0_wp, color=[0.8_wp, 0.8_wp, 0.8_wp])
        call w%hud_view%show(app%show_hud%isChecked())

        ! default surface type, as vsgQt uses: VSG gives the native view its own
        ! CAMetalLayer on macOS (with QSurface_VulkanSurface, Qt manages the
        ! layer itself and Vulkan finds no CAMetalLayer)
        w%window = QWindow()
        w%container = QWidget_createWindowContainer(w%window)
        call w%container%setMinimumSize(320, 240)
        w%view_choice = QComboBox()
        call w%view_choice%addItems(view_names)
        call w%view_choice%setCurrentIndex(modulo(start_view - 1, n_views))
        call w%view_choice%currentIndexChanged%connect(on_view, w)

        w%events = QtfEventFilter(w%window)
        call w%events%exposeEvent%connect(on_expose, w)
        call w%events%updateRequest%connect(on_update, w)
        call w%events%resizeEvent%connect(on_resize, w)
        call w%events%mousePressEvent%connect(on_press, w)
        call w%events%mouseReleaseEvent%connect(on_release, w)
        call w%events%mouseMoveEvent%connect(on_move, w)
        call w%events%wheelEvent%connect(on_wheel, w)
        call w%events%keyPressEvent%connect(on_key_press, w)
        call w%events%keyReleaseEvent%connect(on_key_release, w)

        if (k > 1) then
            w%top = QWidget()
            write(title, '(a,i0)') 'vkframes ', k
            call w%top%setWindowTitle(trim(title))
            layout = QVBoxLayout(w%top)
            call layout%addWidget(w%view_choice)
            call layout%addWidget(w%container, 1)
            call w%top%closeEvent%connect(on_close_window, w)
            call w%top%resize(640, 480)
            call w%top%show()
        end if
    end subroutine build_window

    !> Open window w's view (on its first expose) and show the scene. Windows
    !> after the first follow the main window's clock.
    subroutine open_view(w)
        type(view_window_t), intent(inout), target :: w
        type(SimulationClock), pointer :: clock, master
        integer :: k
        if (.not. w%proxy%open(w%window%width(), w%window%height(), 'vkframes', &
                               native_window=w%window%winId(), pixel_ratio=w%window%devicePixelRatio())) then
            write(*, '(a)') 'vkframes: could not open the view'
            return
        end if
        call w%proxy%set_scene(app%scene)
        call w%proxy%set_background_color([0.0_wp, 0.0_wp, 0.0_wp])
        call w%proxy%set_sky(app%sky)
        do k = 1, size(w%views)
            call w%proxy%add_view(w%views(k))
        end do
        call w%proxy%select_view(w%view_choice%currentIndex() + 1)
        call w%proxy%add_hud_text(w%hud_view)
        call w%proxy%add_hud_text(app%hud_time)
        call w%proxy%set_view_transition(app%view_transition%value())
        w%events_handler%window = w%index
        call w%proxy%set_event_handler(w%events_handler)
        if (w%index == 1) then
            call w%proxy%set_time_range(0.0_wp, app%time_end)
            call w%proxy%set_time_scale(app%time_scale%value())
            call w%proxy%pause_time(app%pause_time%isChecked())
            call w%proxy%set_time(0.0_wp)
        else
            clock => w%proxy%clock()
            master => app%windows(1)%proxy%clock()
            if (.not. clock%synchronize(master)) write(*, '(a)') 'vkframes: could not synchronize the clocks'
        end if
    end subroutine open_view

    !> Close window w's view (waiting for the GPU) before Qt destroys its native window.
    subroutine close_view(w)
        type(view_window_t), intent(inout) :: w
        type(SimulationClock), pointer :: clock
        if (w%proxy%is_open() .and. w%index > 1) then
            clock => w%proxy%clock()
            if (clock%synchronize(null())) continue
        end if
        call w%proxy%close()
    end subroutine close_view

    ! ==================================================================
    ! window events (GUI thread; `data` is the view_window_t)

    !> First expose: open the view, then start the window's frame loop.
    subroutine on_expose(data)
        class(*), intent(inout), optional :: data
        select type (w => data)
        type is (view_window_t)
            if (.not. w%window%isExposed()) return
            if (w%proxy%is_open()) then                    ! (the main window's view opens before it is shown: on_startup)
                if (w%frame == 0) call w%window%requestUpdate()
                return
            end if
            if (w%index > 1 .and. .not. app%windows(1)%proxy%is_open()) return   ! the main window opens first
            call open_view(app%windows(w%index))
            if (w%proxy%is_open()) call w%window%requestUpdate()
        end select
    end subroutine on_expose

    !> Render a frame (time, frames, draw) and ask for the next one.
    subroutine on_update(data)
        class(*), intent(inout), optional :: data
        character(len=40) :: text
        character(len=:), allocatable :: file
        integer :: dot
        real(wp) :: dpr
        select type (w => data)
        type is (view_window_t)
            ! moved to a screen of another scale (or the display's scaling changed):
            ! Qt may send no resize, the window's size in window units being the same
            dpr = w%window%devicePixelRatio()
            if (dpr /= w%proxy%pixel_ratio()) &
                call w%proxy%resize(nint(w%window%width() * dpr), nint(w%window%height() * dpr), pixel_ratio=dpr)
            if (.not. w%proxy%frame()) return
            w%frame = w%frame + 1
            if (w%index == 1) then
                write(text, '(a,f10.1,a)') 'Simulation time: ', w%proxy%time(), ' s'
                call app%time_label%setText(trim(text))
                write(text, '(a,f0.1,a)') 't = ', w%proxy%time(), ' s'
                call app%hud_time%set_text(trim(text))
                call advance_rotation(w%proxy%time())
                if (.not. app%slider_held) then           ! the slider follows the time (unless dragged)
                    app%slider_setting = .true.
                    call app%time_slider%setValue(nint(w%proxy%time()))
                    app%slider_setting = .false.
                end if
            end if
            if (allocated(app%capture) .and. w%frame == app%capture_frame) then
                file = app%capture
                if (w%index > 1) then            ! file_2.png, ...
                    dot = index(file, '.', back=.true.)
                    if (dot == 0) dot = len(file) + 1
                    write(text, '(a,i0)') '_', w%index
                    file = file(1:dot-1)//trim(text)//file(dot:)
                end if
                if (w%proxy%capture(file)) write(*, '(a)') 'captured '//file
            end if
            call w%window%requestUpdate()
        end select
    end subroutine on_update

    !> New size: rebuild the swapchain (framebuffer pixels).
    subroutine on_resize(event, data)
        type(QResizeEvent), intent(in) :: event
        class(*), intent(inout), optional :: data
        real(wp) :: dpr
        select type (w => data)
        type is (view_window_t)
            if (.not. w%proxy%is_open()) return
            dpr = w%window%devicePixelRatio()
            call w%proxy%resize(nint(event%width() * dpr), nint(event%height() * dpr), pixel_ratio=dpr)
            call w%window%requestUpdate()
        end select
    end subroutine on_resize

    !> VSG's mouse button number: 1 left, 2 middle, 3 right (0: other).
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

    !> VSG's button mask (BUTTON_MASK_1/2/3 = 256/512/1024) for Qt's held buttons.
    integer function vsg_mask(qt_buttons)
        integer, intent(in) :: qt_buttons
        vsg_mask = 0
        if (iand(qt_buttons, Qt_LeftButton) /= 0) vsg_mask = ior(vsg_mask, 256)
        if (iand(qt_buttons, Qt_MiddleButton) /= 0) vsg_mask = ior(vsg_mask, 512)
        if (iand(qt_buttons, Qt_RightButton) /= 0) vsg_mask = ior(vsg_mask, 1024)
    end function vsg_mask

    !> Forward a mouse event (MOUSE_MOVE, MOUSE_PRESS or MOUSE_RELEASE), in framebuffer pixels.
    subroutine forward_mouse(w, kind, event)
        type(view_window_t), intent(inout) :: w
        integer, intent(in) :: kind
        type(QMouseEvent), intent(in) :: event
        real(wp) :: dpr
        dpr = w%window%devicePixelRatio()
        call w%proxy%mouse(kind, nint(event%x() * dpr), nint(event%y() * dpr), vsg_mask(event%buttons()), &
                           vsg_button(event%button()))
        call event%accept()
    end subroutine forward_mouse

    subroutine on_move(event, data)
        type(QMouseEvent), intent(in) :: event
        class(*), intent(inout), optional :: data
        select type (w => data)
        type is (view_window_t)
            call forward_mouse(w, MOUSE_MOVE, event)
        end select
    end subroutine on_move

    subroutine on_press(event, data)
        type(QMouseEvent), intent(in) :: event
        class(*), intent(inout), optional :: data
        select type (w => data)
        type is (view_window_t)
            call forward_mouse(w, MOUSE_PRESS, event)
        end select
    end subroutine on_press

    subroutine on_release(event, data)
        type(QMouseEvent), intent(in) :: event
        class(*), intent(inout), optional :: data
        select type (w => data)
        type is (view_window_t)
            call forward_mouse(w, MOUSE_RELEASE, event)
        end select
    end subroutine on_release

    subroutine on_wheel(event, data)
        type(QWheelEvent), intent(in) :: event
        class(*), intent(inout), optional :: data
        select type (w => data)
        type is (view_window_t)
            if (event%angleDeltaY() == 0) return
            call w%proxy%scroll(merge(1.0_wp, -1.0_wp, event%angleDeltaY() > 0))
            call event%accept()
        end select
    end subroutine on_wheel

    !> VSG key symbol for a Qt key: ASCII for printable keys (lower case for the
    !> base symbol), X11-style codes for the others VSG's trackball uses; 0 if
    !> unmapped.
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
        if (key >= Qt_Key_A .and. key <= Qt_Key_Z .and. iand(modifier, 1) /= 0) modified_symbol = key   ! upper case
    end subroutine vsg_key

    subroutine forward_key(w, pressed, event)
        type(view_window_t), intent(inout) :: w
        logical, intent(in) :: pressed
        type(QKeyEvent), intent(in) :: event
        integer :: symbol, modified_symbol, modifier
        call vsg_key(event, symbol, modified_symbol, modifier)
        if (symbol == 0) return
        call w%proxy%key(pressed, symbol, modified_symbol, modifier)
        call event%accept()
    end subroutine forward_key

    subroutine on_key_press(event, data)
        type(QKeyEvent), intent(in) :: event
        class(*), intent(inout), optional :: data
        select type (w => data)
        type is (view_window_t)
            call forward_key(w, .true., event)
        end select
    end subroutine on_key_press

    subroutine on_key_release(event, data)
        type(QKeyEvent), intent(in) :: event
        class(*), intent(inout), optional :: data
        select type (w => data)
        type is (view_window_t)
            call forward_key(w, .false., event)
        end select
    end subroutine on_key_release

    !> Show the chosen view in this window (each view keeps its own camera).
    subroutine on_view(index, data)
        integer, intent(in) :: index
        class(*), intent(inout), optional :: data
        select type (w => data)
        type is (view_window_t)
            call w%proxy%select_view(index + 1)
            call w%hud_view%set_text(trim(view_names(index + 1)))
        end select
    end subroutine on_view

    !> Keys (DemoEvents): v and V the next and previous views, through the
    !> window's view menu (which shows the view, and its name in the HUD), p
    !> pauses and resumes the time; the others (the space bar: vkframes' reset
    !> of the view) are left to the window.
    subroutine demo_key_press(self, window, key, modifiers, handled)
        class(DemoEvents), intent(inout) :: self
        class(WindowProxy), intent(inout) :: window
        integer, intent(in) :: key, modifiers
        logical, intent(inout) :: handled
        integer :: current
        if (iand(modifiers, MOD_CONTROL + MOD_ALT + MOD_META) /= 0) return
        associate (w => app%windows(self%window))
            current = w%view_choice%currentIndex()
            if (key == iachar('v')) then
                call w%view_choice%setCurrentIndex(modulo(current + 1, n_views))
                handled = .true.
            else if (key == iachar('V')) then
                call w%view_choice%setCurrentIndex(modulo(current - 1, n_views))
                handled = .true.
            else if (key == iachar('p') .or. key == iachar('P')) then
                call app%pause_time%setChecked(.not. app%pause_time%isChecked())
                handled = .true.
            else if (key == iachar('g') .and. app%have_picked_time) then      ! (go to the time picked)
                call app%windows(1)%proxy%set_time(app%picked_time)
                handled = .true.
            end if
        end associate
    end subroutine demo_key_press

    !> Each frame, before drawing: a GUI panel (vkf_gui) at the point last
    !> picked in this window, kept at its place on the screen, with its readout
    !> and buttons to go to the time there and to close it.
    subroutine demo_before_draw(self, window, t)
        class(DemoEvents), intent(inout) :: self
        class(WindowProxy), intent(inout) :: window
        real(wp), intent(in) :: t
        type(Gui) :: g
        real(wp) :: at(2)
        if (app%picked_window /= self%window) return
        if (.not. window%screen_position(app%picked%position(), at)) return
        g = window%gui()
        call g%begin_panel('##picked', position=at + [0.01_wp, 0.01_wp], pivot=[0.0_wp, 1.0_wp], follow=.true.)
        call g%text(trim(app%picked_text))
        if (app%have_picked_time) then
            if (g%button('go to this time')) call app%windows(1)%proxy%set_time(app%picked_time)
            call g%same_line()
        end if
        if (g%button('close')) app%picked_window = 0
        call g%end_panel()
    end subroutine demo_before_draw

    !> The pointer's position (-1 to 1 across the window, y up), in the dock.
    subroutine demo_mouse_motion(self, window, x, y, buttons)
        class(DemoEvents), intent(inout) :: self
        class(WindowProxy), intent(inout) :: window
        real(wp), intent(in) :: x, y
        logical, intent(in) :: buttons(3)
        character(len=160) :: text
        write(text, '(a,i0,a,f6.2,a,f6.2,a)') 'Pointer (window ', self%window, '): ', x, ', ', y, &
            merge(' (dragging)', '           ', any(buttons))
        call app%pointer_label%setText('<small>'//trim(text)//'<br>'//trim(app%last_click)//'</small>')
    end subroutine demo_mouse_motion

    !> A click: what is under it (WindowProxy%pick), in the dock. An orbit or
    !> a marker: its time there (g goes to it) and altitude; a body: the
    !> latitude and longitude.
    subroutine demo_button_press(self, window, x, y, button)
        class(DemoEvents), intent(inout) :: self
        class(WindowProxy), intent(inout) :: window
        real(wp), intent(in) :: x, y
        integer, intent(in) :: button
        type(PickHit), allocatable :: hits(:)
        character(len=96) :: readout
        if (button /= MOUSE_LEFT) return
        call window%pick(x, y, hits)
        app%have_picked_time = .false.
        app%picked_window = 0
        if (size(hits) == 0) then
            write(app%last_click, '(a,i0,a)') 'Clicked (window ', self%window, '): nothing there'
        else
            associate (h => hits(1))
                select case (h%kind)
                case (PICK_CURVE, PICK_SEGMENT, PICK_MARKER)
                    write(app%last_click, '(a,a,a,f8.1,a,f8.1,a)') 'Picked: ', h%frame%name(), &
                        trim(merge(' (marker)', '         ', h%kind == PICK_MARKER)) // ', t = ', h%time, &
                        ' s, altitude ', norm2(h%position) - re, ' km (g: go there)'
                    app%picked_time = h%time
                    app%have_picked_time = .true.
                    write(readout, '(a,i0,a,i0,a)') '   t = ', nint(h%time), ' s, ', nint(norm2(h%position) - re), ' km'
                case (PICK_BODY)
                    write(app%last_click, '(a,a,a,f7.2,a,f8.2)') 'Picked: ', h%frame%name(), ', latitude ', &
                        h%latitude * 180.0_wp / pi, ', longitude ', h%longitude * 180.0_wp / pi
                    write(readout, '(a,f6.1,a,f7.1)') '   ', h%latitude * 180.0_wp / pi, ', ', h%longitude * 180.0_wp / pi
                case default
                    app%last_click = 'Picked: '//h%frame%name()
                    readout = '   '//h%frame%name()
                end select
                ! a GUI panel at the point picked (demo_before_draw)
                app%picked_text = adjustl(readout)
                call app%picked%set_position(h%position)
                app%picked_window = self%window
            end associate
        end if
        call app%pointer_label%setText('<small>'//trim(app%last_click)//'</small>')
    end subroutine demo_button_press

    !> A window other than the main one closes.
    subroutine on_close_window(event, data)
        type(QCloseEvent), intent(in) :: event
        class(*), intent(inout), optional :: data
        select type (w => data)
        type is (view_window_t)
            call close_view(w)
            w%used = .false.
        end select
    end subroutine on_close_window

    ! ==================================================================
    ! controls and lifetime (GUI thread; they act on the main window, whose
    ! clock the others follow)

    !> The time to fly between views (and to a reset view), in every window.
    subroutine on_view_transition(value, data)
        real(c_double), intent(in) :: value
        class(*), intent(inout), optional :: data
        integer :: k
        do k = 1, max_windows
            if (app%windows(k)%used) call app%windows(k)%proxy%set_view_transition(value)
        end do
    end subroutine on_view_transition

    subroutine on_time_scale(value, data)
        real(c_double), intent(in) :: value
        class(*), intent(inout), optional :: data
        call app%windows(1)%proxy%set_time_scale(value)
    end subroutine on_time_scale

    subroutine on_pause_time(checked, data)
        logical, intent(in) :: checked
        class(*), intent(inout), optional :: data
        call app%windows(1)%proxy%pause_time(checked)
    end subroutine on_pause_time

    subroutine on_reset_time(data)
        class(*), intent(inout), optional :: data
        call app%windows(1)%proxy%set_time(0.0_wp)
    end subroutine on_reset_time

    !> The time slider moved (dragged, clicked or with the keys): set the time.
    !> (Not when the demo moves it to follow the time.)
    subroutine on_time_slider(value, data)
        integer, intent(in) :: value
        class(*), intent(inout), optional :: data
        if (app%slider_setting) return
        call app%windows(1)%proxy%set_time(real(value, wp))
    end subroutine on_time_slider

    subroutine on_slider_pressed(data)
        class(*), intent(inout), optional :: data
        app%slider_held = .true.
    end subroutine on_slider_pressed

    subroutine on_slider_released(data)
        class(*), intent(inout), optional :: data
        app%slider_held = .false.
    end subroutine on_slider_released

    !> Put the main window's current view back at its default position.
    subroutine on_reset_view(data)
        class(*), intent(inout), optional :: data
        type(View), pointer :: v
        v => app%windows(1)%proxy%current_view()
        call v%reset_view()
    end subroutine on_reset_view

    !> How the spacecraft follows the orbit after one period: loop, or stop at the end.
    subroutine on_follow_mode(index, data)
        integer, intent(in) :: index
        class(*), intent(inout), optional :: data
        type(TrajectoryFollower), pointer :: follower
        follower => app%spacecraft%follower()
        call follower%set_follow_type(ior(FOLLOW_POSITION, FOLLOW_ATTITUDE), merge(FOLLOW_LIMIT, FOLLOW_LOOP, index == 1))
    end subroutine on_follow_mode

    !> Show or hide the frames' axes (OpenFrames' showAxes).
    subroutine on_show_axes(checked, data)
        logical, intent(in) :: checked
        class(*), intent(inout), optional :: data
        call app%earth%show_axes(merge(ALL_AXES, NO_AXES, checked))
        call app%spacecraft%show_axes(merge(ALL_AXES, NO_AXES, checked))
        call app%spacecraft2%show_axes(merge(ALL_AXES, NO_AXES, checked))
    end subroutine on_show_axes

    !> Show or hide the frames' name and axis labels.
    subroutine on_show_labels(checked, data)
        logical, intent(in) :: checked
        class(*), intent(inout), optional :: data
        call app%earth%show_axes_labels(merge(ALL_AXES, NO_AXES, checked))
        call app%earth%show_name_label(checked)
        call app%spacecraft%show_axes_labels(merge(ALL_AXES, NO_AXES, checked))
        call app%spacecraft%show_name_label(checked)
        call app%spacecraft2%show_axes_labels(merge(ALL_AXES, NO_AXES, checked))
        call app%spacecraft2%show_name_label(checked)
    end subroutine on_show_labels

    !> The Earth's rotation speed changed (the slider): show it.
    subroutine on_rotation_speed(value, data)
        integer, intent(in) :: value
        class(*), intent(inout), optional :: data
        character(len=24) :: text
        write(text, '(i0,a)') value, ' x the real rate'
        call app%rotation_label%setText(trim(text))
    end subroutine on_rotation_speed

    !> Show or hide the HUD text in all windows.
    subroutine on_show_hud(checked, data)
        logical, intent(in) :: checked
        class(*), intent(inout), optional :: data
        integer :: k
        call app%hud_time%show(checked)
        do k = 1, max_windows
            if (app%windows(k)%used) call app%windows(k)%hud_view%show(checked)
        end do
    end subroutine on_show_hud

    !> Line width: a style change only (no point data is re-uploaded).
    subroutine on_line_width(value, data)
        real(c_double), intent(in) :: value
        class(*), intent(inout), optional :: data
        call app%orbit_curve%set_width(real(value, wp))
        call app%orbit2_curve%set_width(real(value, wp))
    end subroutine on_line_width

    !> Trace mode: the orbit drawn up to the spacecraft only (with a loop
    !> follow mode, whole again after the first period: the trace follows the
    !> trajectory's own times).
    !> The orbit in its points' colors (by altitude), or in its own color.
    subroutine on_altitude_colors(checked, data)
        logical, intent(in) :: checked
        class(*), intent(inout), optional :: data
        call app%orbit_curve%set_point_colors(checked)
        call app%orbit_plane%set_point_colors(checked)
    end subroutine on_altitude_colors

    subroutine on_trace_orbit(checked, data)
        logical, intent(in) :: checked
        class(*), intent(inout), optional :: data
        call app%orbit_curve%set_trace_mode(checked)
        call app%radius_vectors%set_trace_mode(checked)
        call app%orbit_plane%set_trace_mode(checked)
        call app%orbit_markers%set_trace_mode(checked)
        call app%orbit2_curve%set_trace_mode(checked)
        call app%orbit2_markers%set_trace_mode(checked)
    end subroutine on_trace_orbit

    !> The orbits' and radius vectors' dash pattern: solid, dashed, dotted or
    !> dash-dot (OpenGL stipples: 16 bits, bit 0 first, each `factor` window units).
    subroutine on_line_pattern(index, data)
        integer, intent(in) :: index
        class(*), intent(inout), optional :: data
        integer, parameter :: bits(0:3) = [int(z'FFFF'), int(z'00FF'), int(z'3333'), int(z'18FF')]
        integer :: k
        k = max(0, min(3, index))
        call app%orbit_curve%set_pattern(2, bits(k))
        call app%orbit2_curve%set_pattern(2, bits(k))
        call app%radius_vectors%set_pattern(2, bits(k))
    end subroutine on_line_pattern

    !> Marker shape (0: none; 1 + a MARKER_* shape).
    subroutine on_marker_shape(index, data)
        integer, intent(in) :: index
        class(*), intent(inout), optional :: data
        if (index == 0) then
            call app%orbit_markers%set_markers(0)
            call app%orbit2_markers%set_markers(0)
        else
            call app%orbit_markers%set_markers(ior(ior(MARKER_START, MARKER_INTERMEDIATE), MARKER_END))
            call app%orbit_markers%set_marker_shape(index - 1)
            call app%orbit2_markers%set_markers(ior(MARKER_START, MARKER_END))
            call app%orbit2_markers%set_marker_shape(index - 1)
        end if
    end subroutine on_marker_shape

    !> How the spacecraft are shown: their bodies (0), or a marker of shape
    !> index - 1 at their current positions (the bodies hidden; their frames
    !> still move, and the views follow them).
    subroutine on_position_marker(index, data)
        integer, intent(in) :: index
        class(*), intent(inout), optional :: data
        logical :: as_marker
        as_marker = index > 0
        call app%spacecraft%show_contents(.not. as_marker)
        call app%spacecraft2%show_contents(.not. as_marker)
        call app%position_frame1%show_contents(as_marker)
        call app%position_frame2%show_contents(as_marker)
        if (as_marker) then
            call app%position_marker1%set_marker_shape(index - 1)
            call app%position_marker2%set_marker_shape(index - 1)
        end if
    end subroutine on_position_marker

    !> What the sky shows: nothing, the stars, the texture, or both.
    subroutine on_sky(index, data)
        integer, intent(in) :: index
        class(*), intent(inout), optional :: data
        integer, parameter :: modes(0:3) = [SKY_NONE, SKY_STARS, SKY_TEXTURE, SKY_TEXTURE + SKY_STARS]
        call app%sky%set_draw_mode(modes(max(0, min(3, index))))
    end subroutine on_sky

    subroutine on_attenuate(checked, data)
        logical, intent(in) :: checked
        class(*), intent(inout), optional :: data
        call app%orbit_markers%set_auto_attenuate(checked)
        call app%orbit2_markers%set_auto_attenuate(checked)
    end subroutine on_attenuate

    !> Radius vectors: an artist added to the orbit's frame while running (built
    !> and compiled at the next frame), or removed.
    !> The Sun's light (day and night sides), or the window's headlight.
    subroutine on_sun_light(checked, data)
        logical, intent(in) :: checked
        class(*), intent(inout), optional :: data
        call app%sun%set_light_source_enabled(checked)
    end subroutine on_sun_light

    subroutine on_show_grid(checked, data)
        logical, intent(in) :: checked
        class(*), intent(inout), optional :: data
        call app%grid%show_contents(checked)
    end subroutine on_show_grid

    subroutine on_show_ticks(checked, data)
        logical, intent(in) :: checked
        class(*), intent(inout), optional :: data
        call app%ticks%show_contents(checked)
    end subroutine on_show_ticks

    subroutine on_show_plane(checked, data)
        logical, intent(in) :: checked
        class(*), intent(inout), optional :: data
        call app%equator%show_contents(checked)
    end subroutine on_show_plane

    subroutine on_show_los(checked, data)
        logical, intent(in) :: checked
        class(*), intent(inout), optional :: data
        call app%line_of_sight%show_contents(checked)
    end subroutine on_show_los

    subroutine on_show_shadow(checked, data)
        logical, intent(in) :: checked
        class(*), intent(inout), optional :: data
        call app%earth_shadow%show_contents(checked)
    end subroutine on_show_shadow

    subroutine on_show_sensor(checked, data)
        logical, intent(in) :: checked
        class(*), intent(inout), optional :: data
        call app%sensor%show_contents(checked)
    end subroutine on_show_sensor

    subroutine on_show_radius(checked, data)
        logical, intent(in) :: checked
        class(*), intent(inout), optional :: data
        if (checked) then
            call app%orbit_frame%add_artist(app%radius_vectors)
        else
            call app%orbit_frame%remove_artist(app%radius_vectors)
        end if
    end subroutine on_show_radius

    !> The orbit's plane, swept from the Earth's center (added to the orbit's
    !> frame while running, or removed).
    subroutine on_show_plane_swept(checked, data)
        logical, intent(in) :: checked
        class(*), intent(inout), optional :: data
        if (checked) then
            call app%orbit_frame%add_artist(app%orbit_plane)
        else
            call app%orbit_frame%remove_artist(app%orbit_plane)
        end if
    end subroutine on_show_plane_swept

    !> Open another window on the scene (up to max_windows).
    subroutine on_new_window(data)
        class(*), intent(inout), optional :: data
        call new_window()
    end subroutine on_new_window

    !> After the main window is first painted: the slow parts of the startup
    !> (reading the label font, creating the Vulkan device) while it shows a
    !> message, then the view in its place, and the other windows (--windows).
    subroutine on_startup(data)
        class(*), intent(inout), optional :: data
        type(c_ptr) :: font
        integer :: k
        call show_startup_message('Loading fonts...')
        font = get_font('')                                  ! (read once, for all labels)
        call show_startup_message('Starting Vulkan...')
        call open_view(app%windows(1))
        call app%stack%setCurrentIndex(1)
        do k = 2, min(app%start_windows, max_windows)
            call new_window()
        end do
    end subroutine on_startup

    !> The main window's startup message, painted now (the GUI thread is busy until the view opens).
    subroutine show_startup_message(text)
        character(len=*), intent(in) :: text
        call app%startup_message%setText(text)
        call QApplication_processEvents()
    end subroutine show_startup_message

    subroutine new_window()
        integer :: k
        do k = 2, max_windows
            if (.not. app%windows(k)%used) then
                call build_window(k, k)
                return
            end if
        end do
        write(*, '(a,i0,a)') 'vkframes demo: at most ', max_windows, ' windows'
    end subroutine new_window

    !> --test-resize: resize the main window (checks the swapchain rebuild).
    subroutine on_test_resize(data)
        class(*), intent(inout), optional :: data
        call app%main%resize(800, 520)
    end subroutine on_test_resize

    !> --quit-after: close all windows.
    subroutine on_quit(data)
        class(*), intent(inout), optional :: data
        integer :: k
        do k = max_windows, 2, -1
            if (app%windows(k)%used) call app%windows(k)%top%close()
        end do
        call app%main%close()
    end subroutine on_quit

    !> The main window closes: close the other windows, then its own view.
    subroutine on_close(event, data)
        type(QCloseEvent), intent(in) :: event
        class(*), intent(inout), optional :: data
        integer :: k
        do k = max_windows, 2, -1
            if (app%windows(k)%used) call app%windows(k)%top%close()
        end do
        call close_view(app%windows(1))
    end subroutine on_close

    ! ==================================================================
    ! construction

    !> The main window: the first view in the center, controls in a dock.
    subroutine build_ui()
        type(QDockWidget) :: dock
        type(QWidget) :: panel
        type(QFormLayout) :: form
        type(QPushButton) :: reset, reset_view, new_win

        app%main = QMainWindow()
        call app%main%setWindowTitle('vkframes in qt-fortran')
        call app%main%closeEvent%connect(on_close)

        panel = QWidget()
        form = QFormLayout(panel)
        app%show_hud = QCheckBox('Show HUD')
        call app%show_hud%setChecked(.true.)
        app%hud_time = HUDText('t = 0 s', position=[0.01_wp, 0.01_wp], alignment=ALIGN_LEFT_BOTTOM, size=18.0_wp, &
                               color=[1.0_wp, 0.85_wp, 0.2_wp])
        call build_window(1, app%start_view)
        app%stack = QStackedWidget()
        app%startup_message = QLabel('Starting...')
        call app%startup_message%setAlignment(Qt_AlignCenter)
        call app%startup_message%setStyleSheet('background-color: black; color: #cccccc; font-size: 16pt;')
        call app%stack%addWidget(app%startup_message)
        call app%stack%addWidget(app%windows(1)%container)
        call app%main%setCentralWidget(app%stack)
        call form%addRow('View', app%windows(1)%view_choice)
        reset_view = QPushButton('Reset view')
        call reset_view%clicked%connect(on_reset_view)
        call form%addRow('', reset_view)
        new_win = QPushButton('New window')
        call new_win%clicked%connect(on_new_window)
        call form%addRow('', new_win)
        app%time_scale = QDoubleSpinBox()
        call app%time_scale%setRange(0.0_c_double, 5000.0_c_double)
        call app%time_scale%setValue(200.0_c_double)
        call app%time_scale%setSuffix(' x')
        call app%time_scale%valueChanged%connect(on_time_scale)
        call form%addRow('Time scale', app%time_scale)
        app%view_transition = QDoubleSpinBox()
        call app%view_transition%setRange(0.0_c_double, 5.0_c_double)
        call app%view_transition%setSingleStep(0.5_c_double)
        call app%view_transition%setValue(1.5_c_double)
        call app%view_transition%setSuffix(' s')
        call app%view_transition%valueChanged%connect(on_view_transition)
        call form%addRow('View change', app%view_transition)
        app%pause_time = QCheckBox('Pause time')
        call app%pause_time%toggled%connect(on_pause_time)
        call form%addRow('', app%pause_time)
        reset = QPushButton('Reset time')
        call reset%clicked%connect(on_reset_time)
        call form%addRow('', reset)
        app%follow_mode = QComboBox()
        call app%follow_mode%addItems([character(len=23) :: 'Loop the orbit', 'Stop at the end (limit)'])
        call app%follow_mode%currentIndexChanged%connect(on_follow_mode)
        call form%addRow('After one period', app%follow_mode)
        app%rotation_speed = QSlider(Qt_Horizontal)
        call app%rotation_speed%setRange(0, 50)
        app%rotation_label = QLabel('')
        call app%rotation_speed%valueChanged%connect(on_rotation_speed)
        call app%rotation_speed%setValue(1)
        call form%addRow('Earth rotation', app%rotation_speed)
        call form%addRow('', app%rotation_label)
        app%show_axes = QCheckBox('Show axes')
        call app%show_axes%setChecked(.true.)
        call app%show_axes%toggled%connect(on_show_axes)
        call form%addRow('', app%show_axes)
        call app%show_hud%toggled%connect(on_show_hud)
        call form%addRow('', app%show_hud)
        app%show_labels = QCheckBox('Show labels')
        call app%show_labels%setChecked(.true.)
        call app%show_labels%toggled%connect(on_show_labels)
        call form%addRow('', app%show_labels)
        app%line_width = QDoubleSpinBox()
        call app%line_width%setRange(0.5_c_double, 20.0_c_double)
        call app%line_width%setValue(2.0_c_double)
        call app%line_width%setSuffix(' pt')
        call app%line_width%valueChanged%connect(on_line_width)
        call form%addRow('Orbit width', app%line_width)
        app%line_pattern = QComboBox()
        call app%line_pattern%addItems([character(len=8) :: 'Solid', 'Dashed', 'Dotted', 'Dash-dot'])
        call app%line_pattern%currentIndexChanged%connect(on_line_pattern)
        call form%addRow('Orbit pattern', app%line_pattern)
        app%trace_orbit = QCheckBox('Trace orbit')
        call app%trace_orbit%toggled%connect(on_trace_orbit)
        call form%addRow('', app%trace_orbit)
        app%altitude_colors = QCheckBox('Color orbit by altitude')
        call app%altitude_colors%toggled%connect(on_altitude_colors)
        call form%addRow('', app%altitude_colors)
        app%show_radius = QCheckBox('Radius vectors')
        call app%show_radius%toggled%connect(on_show_radius)
        call form%addRow('', app%show_radius)
        app%show_plane_swept = QCheckBox('Orbit plane (swept)')
        call app%show_plane_swept%toggled%connect(on_show_plane_swept)
        call form%addRow('', app%show_plane_swept)
        app%sun_light = QCheckBox('Sun light (day and night)')
        call app%sun_light%setChecked(.true.)
        call app%sun_light%toggled%connect(on_sun_light)
        call form%addRow('', app%sun_light)
        app%show_grid = QCheckBox('Lat/lon grid')
        call app%show_grid%toggled%connect(on_show_grid)
        call form%addRow('', app%show_grid)
        app%show_ticks = QCheckBox('Coordinate axes (ticks)')
        call app%show_ticks%toggled%connect(on_show_ticks)
        call form%addRow('', app%show_ticks)
        app%show_plane = QCheckBox('Equatorial plane')
        call app%show_plane%toggled%connect(on_show_plane)
        call form%addRow('', app%show_plane)
        app%show_sensor = QCheckBox('Sensor cone')
        call app%show_sensor%toggled%connect(on_show_sensor)
        call form%addRow('', app%show_sensor)
        app%show_los = QCheckBox('Line of sight (1 - 2)')
        call app%show_los%toggled%connect(on_show_los)
        call form%addRow('', app%show_los)
        app%show_shadow = QCheckBox("Earth's shadow")
        call app%show_shadow%toggled%connect(on_show_shadow)
        call form%addRow('', app%show_shadow)
        app%marker_shape = QComboBox()
        call app%marker_shape%addItems([character(len=15) :: 'None', 'Disk', 'Circle', 'Square', 'Plus', 'X', 'Target', &
                                        'Rose', 'Pulsing circle', 'Pulsing disk', 'Spiral'])
        call app%marker_shape%setCurrentIndex(1)
        call app%marker_shape%currentIndexChanged%connect(on_marker_shape)
        call form%addRow('Markers', app%marker_shape)
        app%position_marker = QComboBox()
        call app%position_marker%addItems([character(len=22) :: 'Bodies (model, sphere)', 'Marker: disk', 'Marker: circle', &
                                           'Marker: square', 'Marker: plus', 'Marker: X', 'Marker: target', 'Marker: rose', &
                                           'Marker: pulsing circle', 'Marker: pulsing disk', 'Marker: spiral'])
        call app%position_marker%currentIndexChanged%connect(on_position_marker)
        call form%addRow('Spacecraft as', app%position_marker)
        app%sky_choice = QComboBox()
        call app%sky_choice%addItems([character(len=16) :: 'None', 'Stars', 'Texture', 'Texture and stars'])
        call app%sky_choice%currentIndexChanged%connect(on_sky)
        call form%addRow('Sky', app%sky_choice)
        app%attenuate = QCheckBox('Shrink markers with distance')
        call app%attenuate%toggled%connect(on_attenuate)
        call form%addRow('', app%attenuate)
        app%time_label = QLabel('Simulation time:')
        call form%addRow(app%time_label)
        app%time_slider = QSlider(Qt_Horizontal)
        call app%time_slider%setRange(0, nint(app%time_end))
        call app%time_slider%setSingleStep(60)                ! a minute (arrow keys), ten minutes (page keys)
        call app%time_slider%setPageStep(600)
        call app%time_slider%valueChanged%connect(on_time_slider)
        call app%time_slider%sliderPressed%connect(on_slider_pressed)
        call app%time_slider%sliderReleased%connect(on_slider_released)
        call form%addRow('Time', app%time_slider)
        call form%addRow(QLabel('<small>Left drag: rotate<br>Right drag: zoom<br>Middle drag: pan<br>' // &
                                'Wheel: zoom<br>v / V: next / previous view<br>Space: reset the view<br>' // &
                                'p: pause or resume the time<br>Click: pick an orbit, marker or body<br>' // &
                                'g: go to the time picked</small>'))
        app%pointer_label = QLabel('<small>Pointer: (move over a view)</small>')
        call form%addRow(app%pointer_label)
        dock = QDockWidget('Controls', app%main)
        call dock%setWidget(panel)
        call app%main%addDockWidget(Qt_LeftDockWidgetArea, dock)
    end subroutine build_ui

end module vkdemo

!> vkframes in a qt-fortran GUI (see the module documentation).
program vkframes_qt_demo

    use qt
    use vkdemo
    implicit none

    type(QApplication) :: qapp
    character(len=1024) :: arg
    integer :: i, ierr, status

    qapp = QApplication()

    call get_environment_variable('OPENFRAMES_DATA', arg, status=status)
    app%data_dir = ''
    if (status == 0) app%data_dir = trim(arg)
    i = 1
    do while (i <= command_argument_count())
        call get_command_argument(i, arg)
        select case (arg)
        case ('--data')
            call get_command_argument(i + 1, arg)
            app%data_dir = trim(arg)
            i = i + 1
        case ('--capture')
            call get_command_argument(i + 1, arg)
            app%capture = trim(arg)
            i = i + 1
        case ('--capture-frame')
            call get_command_argument(i + 1, arg)
            read(arg, *) app%capture_frame
            i = i + 1
        case ('--view')
            call get_command_argument(i + 1, arg)
            read(arg, *) app%start_view
            i = i + 1
        case ('--windows')
            call get_command_argument(i + 1, arg)
            read(arg, *) app%start_windows
            i = i + 1
        case ('--rotation-speed')
            call get_command_argument(i + 1, arg)
            read(arg, *) app%start_rotation_speed
            i = i + 1
        case ('--test-resize')
            app%test_resize = .true.
        case ('--trace')
            app%start_trace = .true.
        case ('--altitude-colors')
            app%start_altitude_colors = .true.
        case ('--radius')
            app%start_radius = .true.
        case ('--plane')
            app%start_plane = .true.
        case ('--shapes')
            app%start_shapes = .true.
        case ('--los')
            app%start_los = .true.
        case ('--shadow')
            app%start_shadow = .true.
        case ('--sky')
            call get_command_argument(i + 1, arg)
            read(arg, *) app%start_sky
            i = i + 1
        case ('--position-marker')
            call get_command_argument(i + 1, arg)
            read(arg, *) app%start_position_marker
            i = i + 1
        case ('--marker')
            call get_command_argument(i + 1, arg)
            read(arg, *) app%start_marker
            i = i + 1
        case ('--pattern')
            call get_command_argument(i + 1, arg)
            read(arg, *) app%start_pattern
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
    call app%rotation_speed%setValue(app%start_rotation_speed)
    call app%trace_orbit%setChecked(app%start_trace)
    call app%altitude_colors%setChecked(app%start_altitude_colors)
    call app%show_radius%setChecked(app%start_radius)
    call app%show_plane_swept%setChecked(app%start_plane)
    call app%marker_shape%setCurrentIndex(app%start_marker)
    call app%position_marker%setCurrentIndex(app%start_position_marker)
    call app%sky_choice%setCurrentIndex(app%start_sky)
    call app%line_pattern%setCurrentIndex(app%start_pattern)
    call app%show_grid%setChecked(app%start_shapes)
    call app%show_plane%setChecked(app%start_shapes)
    call app%show_ticks%setChecked(app%start_shapes)
    call app%show_sensor%setChecked(app%start_shapes)
    call app%show_los%setChecked(app%start_los)
    call app%show_shadow%setChecked(app%start_shadow)
    call on_sky(app%start_sky)
    if (app%quit_after > 0) call QTimer_singleShot(int(1000 * app%quit_after), on_quit)
    if (app%test_resize) call QTimer_singleShot(2000, on_test_resize)
    call app%main%resize(1100, 700)
    call app%main%show()
    call QTimer_singleShot(0, on_startup)
    ierr = qapp%exec()

end program vkframes_qt_demo
