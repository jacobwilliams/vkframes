!> vkframes behind a Python GUI: the scene and its windows in Fortran, driven
!> through a C interface (bind(C) procedures, `vkdemo_*`) that the PySide6 app
!> (app.py) calls with ctypes. The library is built as a shared library by fpm
!> (fpm.toml: [library] type = "shared"; see demo.py).
!>
!> The scene (vkframes' API): the Earth (a WGS84 Sphere with day and night
!> textures, data/2k_earth_daymap.jpg and 2k_earth_nightmap.jpg, lit by a Sun
!> frame's light source; a LatLonGrid on it), the inertial frame's
!> CoordinateAxes (tick marks; the Earth's own axes start at its surface), a spacecraft following an orbit (positions and
!> attitudes over one period, looped), the orbit drawn by a CurveArtist with
!> markers (start, end, every 10 minutes) in a DrawableTrajectory, all
!> children of an inertial root frame, and a SkySphere (the HYG star catalog,
!> data/Stars_HYGv3.txt, and OpenFrames' StarMap.tif texture if given).
!>
!> Windows: the GUI owns the native windows (QWindows); vkdemo_window_open
!> opens a WindowProxy on one (its native handle, size in logical pixels and
!> pixel ratio) and returns its id (1 to max_windows). The first window's
!> clock drives the others'. Each window has three views (the Earth, the
!> spacecraft, from the spacecraft toward the Earth). Events come in VSG's
!> conventions: framebuffer pixels, VSG button numbers and masks, key symbols.
module vkdemo

    use, intrinsic :: iso_c_binding, only: c_int, c_double, c_long_long, c_char
    use vkframes

    implicit none

    private

    real(wp), parameter :: mu = 398600.4418_wp                  !! km^3/s^2
    real(wp), parameter :: re = 6378.137_wp                     !! WGS84 equatorial radius (km)
    real(wp), parameter :: flattening = 1.0_wp / 298.257223563_wp
    real(wp), parameter :: sma = 9000.0_wp, ecc = 0.2_wp, inc = 35.0_wp * pi / 180.0_wp
    real(wp), parameter :: craft_size = 800.0_wp                !! km (enlarged to be seen)
    real(wp), parameter :: sidereal_day = 86164.0905_wp          !! s
    real(wp), parameter :: orbit_color(3) = [1.0_wp, 0.85_wp, 0.2_wp]
    integer, parameter :: n_orbit = 721
    integer, parameter :: max_windows = 4
    integer, parameter :: n_views = 3
    character(len=21), parameter :: view_names(n_views) = [character(len=21) :: 'Earth', 'Spacecraft', &
                                                            'Spacecraft -> Earth']

    type :: demo_window_t
        logical :: used = .false.
        type(WindowProxy) :: proxy
        type(View) :: views(n_views)
        type(HUDText) :: hud_view
    end type demo_window_t

    type :: demo_t
        logical :: ready = .false.
        type(ReferenceFrame) :: inertial
        type(Sphere) :: earth, craft
        type(LatLonGrid) :: grid
        type(CoordinateAxes) :: ticks
        type(ReferenceFrame) :: sun
        type(Trajectory) :: orbit
        type(DrawableTrajectory) :: orbit_frame
        type(CurveArtist) :: orbit_curve
        type(MarkerArtist) :: orbit_markers
        type(SkySphere) :: sky
        type(FrameManager) :: scene
        type(HUDText) :: hud_time
        type(demo_window_t) :: windows(max_windows)
        real(wp) :: time_scale = 200.0_wp
        logical :: paused = .false.
    end type demo_t

    !> The demo (module-level: frames, views and windows keep pointers to each other).
    type(demo_t), target, save :: app

contains

    ! ==================================================================
    ! the scene

    !> Build the scene (once); `data_dir`: OpenFrames' Demos folder for the
    !> Earth and sky textures ('' for none). Returns 0.
    integer(c_int) function vkdemo_init(data_dir, data_len) bind(C, name="vkdemo_init") result(status)
        character(kind=c_char), intent(in) :: data_dir(*)
        integer(c_int), value :: data_len
        character(len=:), allocatable :: data
        real(wp) :: period, t, r(3)
        logical :: ok
        integer :: k
        status = 0
        if (app%ready) return
        data = to_string(data_dir, data_len)

        app%orbit = Trajectory()
        call app%orbit%reserve(n_orbit)
        period = 2.0_wp * pi * sqrt(sma**3 / mu)
        do k = 1, n_orbit
            t = period * real(k - 1, wp) / real(n_orbit - 1, wp)
            r = orbit_position(t)
            call app%orbit%add(t, pos=r, att=orbit_attitude(r))
        end do

        app%earth = Sphere('Earth', radius=re, color=[0.3_wp, 0.5_wp, 1.0_wp])
        call app%earth%set_sphere_scale([1.0_wp, 1.0_wp, 1.0_wp - flattening])
        call app%earth%set_texture_map('2k_earth_daymap.jpg')          ! found on VKFRAMES_DATA_PATH (data/)
        call app%earth%set_night_texture_map('2k_earth_nightmap.jpg')
        app%grid = LatLonGrid('Grid', color=[0.6_wp, 0.8_wp, 1.0_wp, 0.4_wp])
        call app%grid%set_parameters(1.002_wp * re, 1.002_wp * re, 1.002_wp * (1.0_wp - flattening) * re, &
                                     15.0_wp * pi / 180.0_wp, 15.0_wp * pi / 180.0_wp)
        call app%grid%show_axes(NO_AXES)
        call app%grid%show_axes_labels(NO_AXES)
        call app%grid%show_name_label(.false.)
        call app%grid%show_contents(.false.)
        if (.not. app%earth%add_child(app%grid)) status = -1
        app%ticks = CoordinateAxes('Ticks', color=[0.85_wp, 0.85_wp, 0.85_wp])
        call app%ticks%set_axis_length(2.0_wp * re)
        call app%ticks%set_tick_spacing(1000.0_wp, 250.0_wp)
        call app%ticks%show_axes(NO_AXES)
        call app%ticks%show_axes_labels(NO_AXES)
        call app%ticks%show_name_label(.false.)
        call app%ticks%show_contents(.false.)

        app%sun = ReferenceFrame('Sun')
        call app%sun%set_position(1.495978707e8_wp * [cos(0.4_wp), -sin(0.4_wp), 0.2_wp])
        call app%sun%show_axes(NO_AXES)
        call app%sun%show_axes_labels(NO_AXES)
        call app%sun%show_name_label(.false.)
        call app%sun%set_light_source_enabled(.true.)
        call app%sun%set_light_diffuse([1.0_wp, 1.0_wp, 1.0_wp])
        call app%sun%set_light_ambient([0.08_wp, 0.08_wp, 0.08_wp])
        app%craft = Sphere('Spacecraft', radius=craft_size, color=[1.0_wp, 0.3_wp, 0.2_wp])
        call app%craft%follow(app%orbit)

        app%orbit_curve = CurveArtist(app%orbit, color=orbit_color, width=2.0_wp)
        app%orbit_markers = MarkerArtist(app%orbit, markers=ior(ior(MARKER_START, MARKER_INTERMEDIATE), MARKER_END), &
                                         size=12.0_wp)
        call app%orbit_markers%set_marker_color(MARKER_START, [0.2_wp, 1.0_wp, 0.3_wp])
        call app%orbit_markers%set_marker_color(MARKER_INTERMEDIATE, orbit_color)
        call app%orbit_markers%set_intermediate_type(INTERMEDIATE_TIME)
        call app%orbit_markers%set_intermediate_spacing(600.0_wp)
        app%orbit_frame = DrawableTrajectory('Orbit')
        call app%orbit_frame%show_axes(NO_AXES)
        call app%orbit_frame%show_axes_labels(NO_AXES)
        call app%orbit_frame%show_name_label(.false.)
        call app%orbit_frame%add_artist(app%orbit_curve)
        call app%orbit_frame%add_artist(app%orbit_markers)

        app%inertial = ReferenceFrame('Inertial')
        call app%inertial%show_axes(NO_AXES)
        call app%inertial%show_axes_labels(NO_AXES)
        call app%inertial%show_name_label(.false.)
        if (.not. (app%inertial%add_child(app%earth) .and. app%inertial%add_child(app%craft) .and. &
                   app%inertial%add_child(app%orbit_frame) .and. app%inertial%add_child(app%sun) .and. &
                   app%inertial%add_child(app%ticks))) status = -1
        call app%scene%set_frame(app%inertial)

        ok = app%sky%set_star_data('Stars_HYGv3.txt', min_mag=-2.0_wp, max_mag=6.0_wp, max_stars=40000)
        if (len(data) > 0) ok = app%sky%set_texture_map(data//'/Images/StarMap.tif')
        call app%sky%set_draw_mode(SKY_STARS)

        app%hud_time = HUDText('t = 0 s', position=[0.01_wp, 0.01_wp], alignment=ALIGN_LEFT_BOTTOM, size=18.0_wp, &
                               color=[1.0_wp, 0.85_wp, 0.2_wp])
        app%ready = .true.
    end function vkdemo_init

    !> Position on the orbit at time t (Kepler's equation by Newton iteration), km.
    pure function orbit_position(t) result(r)
        real(wp), intent(in) :: t
        real(wp) :: r(3)
        real(wp) :: m, ea, x, y
        integer :: k
        m = sqrt(mu / sma**3) * t
        ea = m
        do k = 1, 20
            ea = ea - (ea - ecc * sin(ea) - m) / (1.0_wp - ecc * cos(ea))
        end do
        x = sma * (cos(ea) - ecc)
        y = sma * sqrt(1.0_wp - ecc**2) * sin(ea)
        r = [x, y * cos(inc), y * sin(inc)]
    end function orbit_position

    !> The spacecraft's attitude: x radial, z along the orbit normal.
    pure function orbit_attitude(r) result(q)
        real(wp), intent(in) :: r(3)
        real(wp) :: q(4)
        real(wp) :: y
        y = r(2) * cos(inc) + r(3) * sin(inc)
        q = quat_multiply(quat_from_axis_angle([1.0_wp, 0.0_wp, 0.0_wp], inc), &
                          quat_from_axis_angle([0.0_wp, 0.0_wp, 1.0_wp], atan2(y, r(1))))
    end function orbit_attitude

    ! ==================================================================
    ! windows

    !> Open a window on a native window handle (logical size, pixel ratio)
    !> showing view `start_view` (1 to vkdemo_num_views). Returns its id (1 to
    !> max_windows), or 0 on failure.
    integer(c_int) function vkdemo_window_open(native_window, width, height, pixel_ratio, start_view) &
            bind(C, name="vkdemo_window_open") result(id)
        integer(c_long_long), value :: native_window
        integer(c_int), value :: width, height, start_view
        real(c_double), value :: pixel_ratio
        type(SimulationClock), pointer :: clock, master
        integer :: k
        id = 0
        if (.not. app%ready) return
        do k = 1, max_windows
            if (.not. app%windows(k)%used) then
                id = k
                exit
            end if
        end do
        if (id == 0) return
        associate (w => app%windows(id))
            if (.not. w%proxy%open(int(width), int(height), 'vkframes', native_window=native_window, &
                                   pixel_ratio=real(pixel_ratio, wp))) then
                id = 0
                return
            end if
            w%used = .true.
            w%views(1) = View(app%inertial, app%earth, frame_type=ABSOLUTE_FRAME)
            call w%views(1)%set_default_view_distance(2.5_wp * sma)
            w%views(2) = View(app%inertial, app%craft)
            w%views(3) = View(app%inertial, app%craft, lookat=app%earth, frame_type=ABSOLUTE_FRAME)
            call w%views(3)%set_default_view_distance(1.5_wp * craft_size)
            call w%proxy%set_scene(app%scene)
            call w%proxy%set_sky(app%sky)
            do k = 1, n_views
                call w%proxy%add_view(w%views(k))
            end do
            k = max(1, min(n_views, int(start_view)))
            call w%proxy%select_view(k)
            w%hud_view = HUDText(trim(view_names(k)), position=[0.01_wp, 0.99_wp], alignment=ALIGN_LEFT_TOP, &
                                 size=16.0_wp, color=[0.8_wp, 0.8_wp, 0.8_wp])
            call w%proxy%add_hud_text(w%hud_view)
            call w%proxy%add_hud_text(app%hud_time)
            if (id == 1) then
                call w%proxy%set_time_scale(app%time_scale)
                call w%proxy%pause_time(app%paused)
            else
                clock => w%proxy%clock()
                master => app%windows(1)%proxy%clock()
                if (.not. clock%synchronize(master)) write(*, '(a)') 'vkdemo: could not synchronize the clocks'
            end if
        end associate
    end function vkdemo_window_open

    !> Render a frame of window id. Returns 1 while it runs, 0 otherwise.
    integer(c_int) function vkdemo_window_frame(id) bind(C, name="vkdemo_window_frame") result(running)
        integer(c_int), value :: id
        character(len=32) :: text
        running = 0
        if (.not. valid(id)) return
        if (id == 1) then
            write(text, '(a,f0.1,a)') 't = ', app%windows(1)%proxy%time(), ' s'
            call app%hud_time%set_text(trim(text))
            ! the Earth turns (its body-fixed axes and grid with it) in the inertial frame
            call app%earth%set_attitude(quat_from_axis_angle([0.0_wp, 0.0_wp, 1.0_wp], &
                                                             2.0_wp * pi * app%windows(1)%proxy%time() / sidereal_day))
        end if
        if (app%windows(id)%proxy%frame()) running = 1
    end function vkdemo_window_frame

    !> Close window id (before the GUI destroys its native window). The other
    !> windows' clocks stop following the first window's when it closes.
    subroutine vkdemo_window_close(id) bind(C, name="vkdemo_window_close")
        integer(c_int), value :: id
        type(SimulationClock), pointer :: clock
        integer :: k
        if (.not. valid(id)) return
        if (id == 1) then
            do k = 2, max_windows
                if (.not. valid(k)) cycle
                clock => app%windows(k)%proxy%clock()
                if (clock%synchronize(null())) continue
            end do
        else
            clock => app%windows(id)%proxy%clock()
            if (clock%synchronize(null())) continue
        end if
        call app%windows(id)%proxy%close()
        app%windows(id)%used = .false.
    end subroutine vkdemo_window_close

    !> The native window was resized (framebuffer pixels).
    subroutine vkdemo_window_resize(id, width, height) bind(C, name="vkdemo_window_resize")
        integer(c_int), value :: id, width, height
        if (valid(id)) call app%windows(id)%proxy%resize(int(width), int(height))
    end subroutine vkdemo_window_resize

    !> A mouse event: kind 1 move, 2 press, 3 release; position in framebuffer
    !> pixels; VSG's button mask (256 left, 512 middle, 1024 right) and button
    !> (1 left, 2 middle, 3 right).
    subroutine vkdemo_window_mouse(id, kind, x, y, mask, button) bind(C, name="vkdemo_window_mouse")
        integer(c_int), value :: id, kind, x, y, mask, button
        integer, parameter :: kinds(3) = [MOUSE_MOVE, MOUSE_PRESS, MOUSE_RELEASE]
        if (.not. valid(id) .or. kind < 1 .or. kind > 3) return
        call app%windows(id)%proxy%mouse(kinds(kind), int(x), int(y), int(mask), int(button))
    end subroutine vkdemo_window_mouse

    !> The scroll wheel (dy > 0 away from the user).
    subroutine vkdemo_window_scroll(id, dy) bind(C, name="vkdemo_window_scroll")
        integer(c_int), value :: id
        real(c_double), value :: dy
        if (valid(id)) call app%windows(id)%proxy%scroll(real(dy, wp))
    end subroutine vkdemo_window_scroll

    !> A key press (pressed = 1) or release, as VSG key symbols and modifiers.
    subroutine vkdemo_window_key(id, pressed, symbol, modified_symbol, modifier) bind(C, name="vkdemo_window_key")
        integer(c_int), value :: id, pressed, symbol, modified_symbol, modifier
        if (valid(id)) call app%windows(id)%proxy%key(pressed /= 0, int(symbol), int(modified_symbol), int(modifier))
    end subroutine vkdemo_window_key

    !> Show view k (1 to vkdemo_num_views) in window id.
    subroutine vkdemo_window_set_view(id, k) bind(C, name="vkdemo_window_set_view")
        integer(c_int), value :: id, k
        if (.not. valid(id) .or. k < 1 .or. k > n_views) return
        call app%windows(id)%proxy%select_view(int(k))
        call app%windows(id)%hud_view%set_text(trim(view_names(k)))
    end subroutine vkdemo_window_set_view

    !> Put window id's camera back at its view's default.
    subroutine vkdemo_window_reset_view(id) bind(C, name="vkdemo_window_reset_view")
        integer(c_int), value :: id
        type(View), pointer :: v
        if (.not. valid(id)) return
        v => app%windows(id)%proxy%current_view()
        if (associated(v)) call v%reset_view()
    end subroutine vkdemo_window_reset_view

    !> Save window id's last frame as an image. Returns 0 on success.
    integer(c_int) function vkdemo_window_capture(id, file, file_len) bind(C, name="vkdemo_window_capture") &
            result(status)
        integer(c_int), value :: id, file_len
        character(kind=c_char), intent(in) :: file(*)
        status = -1
        if (.not. valid(id)) return
        if (app%windows(id)%proxy%capture(to_string(file, file_len))) status = 0
    end function vkdemo_window_capture

    logical function valid(id)
        integer(c_int), intent(in) :: id
        valid = .false.
        if (id < 1 .or. id > max_windows) return
        valid = app%windows(id)%used
    end function valid

    ! ==================================================================
    ! views and controls

    integer(c_int) function vkdemo_num_views() bind(C, name="vkdemo_num_views")
        vkdemo_num_views = n_views
    end function vkdemo_num_views

    !> The name of view k into buf (buf_len bytes, NUL-terminated). Returns its length.
    integer(c_int) function vkdemo_view_name(k, buf, buf_len) bind(C, name="vkdemo_view_name") result(n)
        integer(c_int), value :: k, buf_len
        character(kind=c_char), intent(out) :: buf(*)
        character(len=:), allocatable :: name
        integer :: i
        n = 0
        if (k < 1 .or. k > n_views .or. buf_len < 1) return
        name = trim(view_names(k))
        n = min(len(name), buf_len - 1)
        do i = 1, n
            buf(i) = name(i:i)
        end do
        buf(n + 1) = achar(0)
    end function vkdemo_view_name

    !> The simulation time of the first window (s).
    real(c_double) function vkdemo_time() bind(C, name="vkdemo_time")
        vkdemo_time = 0.0_c_double
        if (valid(1_c_int)) vkdemo_time = app%windows(1)%proxy%time()
    end function vkdemo_time

    subroutine vkdemo_set_time_scale(scale) bind(C, name="vkdemo_set_time_scale")
        real(c_double), value :: scale
        app%time_scale = scale
        if (valid(1_c_int)) call app%windows(1)%proxy%set_time_scale(app%time_scale)
    end subroutine vkdemo_set_time_scale

    subroutine vkdemo_pause_time(paused) bind(C, name="vkdemo_pause_time")
        integer(c_int), value :: paused
        app%paused = paused /= 0
        if (valid(1_c_int)) call app%windows(1)%proxy%pause_time(app%paused)
    end subroutine vkdemo_pause_time

    subroutine vkdemo_reset_time() bind(C, name="vkdemo_reset_time")
        if (valid(1_c_int)) call app%windows(1)%proxy%set_time(0.0_wp)
    end subroutine vkdemo_reset_time

    !> Trace mode for the orbit's curve and markers.
    subroutine vkdemo_set_trace(enabled) bind(C, name="vkdemo_set_trace")
        integer(c_int), value :: enabled
        call app%orbit_curve%set_trace_mode(enabled /= 0)
        call app%orbit_markers%set_trace_mode(enabled /= 0)
    end subroutine vkdemo_set_trace

    subroutine vkdemo_set_line_width(width) bind(C, name="vkdemo_set_line_width")
        real(c_double), value :: width
        call app%orbit_curve%set_width(real(width, wp))
    end subroutine vkdemo_set_line_width

    !> The sky: 0 none, 1 stars, 2 texture, 3 both.
    subroutine vkdemo_set_sky(mode) bind(C, name="vkdemo_set_sky")
        integer(c_int), value :: mode
        integer, parameter :: modes(0:3) = [SKY_NONE, SKY_STARS, SKY_TEXTURE, SKY_TEXTURE + SKY_STARS]
        call app%sky%set_draw_mode(modes(max(0, min(3, int(mode)))))
    end subroutine vkdemo_set_sky

    !> The Sun's light (1; day and night) or the windows' headlight (0).
    subroutine vkdemo_sun_light(enabled) bind(C, name="vkdemo_sun_light")
        integer(c_int), value :: enabled
        call app%sun%set_light_source_enabled(enabled /= 0)
    end subroutine vkdemo_sun_light

    !> Show (1) or hide (0) the latitude/longitude grid.
    subroutine vkdemo_show_grid(shown) bind(C, name="vkdemo_show_grid")
        integer(c_int), value :: shown
        call app%grid%show_contents(shown /= 0)
    end subroutine vkdemo_show_grid

    !> Show (1) or hide (0) the inertial frame's coordinate axes with tick marks
    !> (the Earth's own axes start at its surface).
    subroutine vkdemo_show_ticks(shown) bind(C, name="vkdemo_show_ticks")
        integer(c_int), value :: shown
        call app%ticks%show_contents(shown /= 0)
    end subroutine vkdemo_show_ticks

    !> Show (1) or hide (0) the Earth's and the spacecraft's axes and labels.
    subroutine vkdemo_show_axes(shown) bind(C, name="vkdemo_show_axes")
        integer(c_int), value :: shown
        integer :: axes
        axes = merge(ALL_AXES, NO_AXES, shown /= 0)
        call app%earth%show_axes(axes)
        call app%earth%show_axes_labels(axes)
        call app%craft%show_axes(axes)
        call app%craft%show_axes_labels(axes)
    end subroutine vkdemo_show_axes

    function to_string(s, n) result(f)
        character(kind=c_char), intent(in) :: s(*)
        integer(c_int), intent(in) :: n
        character(len=:), allocatable :: f
        integer :: i
        allocate(character(len=max(0, n)) :: f)
        do i = 1, n
            f(i:i) = s(i)
        end do
    end function to_string

end module vkdemo
