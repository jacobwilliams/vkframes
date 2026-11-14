!> Exporting a scene to the web (WindowProxy%export_web): a page that shows
!> the scene in a browser (Three.js), to turn and zoom, at a time or
!> animated, as video recording is to a movie.
!>
!> The scene: the Earth (textured, turning, with a latitude/longitude grid),
!> ticked inertial axes, a spacecraft (the pepper model, if found) in a low
!> orbit with its attitude, a sensor cone and a fading trail, a second
!> spacecraft in an elliptical orbit drawn in trace mode with its plane filled
!> (a ribbon), radial segments and time markers, the stars and the
!> constellations, three views and HUD text.
!>
!> Three pages are written:
!> * web_export_snapshot.html: the scene at t = 30 minutes;
!> * web_export.html: animated over the data's time range (play, pause,
!>   the time slider, the speed, the views);
!> * web_export_recorded.html: recorded while the window runs
!>   (start_web_recording ... stop_web_recording), so the sensor cone's scan,
!>   which the program sets each frame, is in it too.
!>
!> Arguments (optional): the number of frames to record, then the directory to
!> write the pages to:
!>   pixi run example web_export
!>   pixi run example web_export -- 300 /tmp
program web_export

    use vkframes

    implicit none

    real(wp), parameter :: mu = 398600.4418_wp, re = 6378.137_wp     !! km^3/s^2, km
    real(wp), parameter :: earth_rate = 7.2921159e-5_wp               !! rad/s
    real(wp), parameter :: dt = 30.0_wp                               !! data step (s)
    real(wp), parameter :: leo_a = re + 700.0_wp, leo_inc = 51.6_wp * pi / 180.0_wp
    real(wp), parameter :: heo_a = 18000.0_wp, heo_e = 0.55_wp, heo_inc = 28.5_wp * pi / 180.0_wp
    real(wp), parameter :: model_size = 600.0_wp                      !! the model drawn this big (km), to be seen
    real(wp), parameter :: logo_radius = 4.6_wp                       !! the pepper model's extent (its units)

    type(ReferenceFrame), target :: eci
    type(Sphere), target :: earth
    type(LatLonGrid), target :: grid
    type(CoordinateAxes), target :: axes
    type(Model), target :: craft
    type(EllipticCone), target :: sensor
    type(Trajectory), target :: spin, leo, heo
    type(DrawableTrajectory), target :: leo_frame, heo_frame
    type(CurveArtist), target :: leo_curve, heo_curve
    type(MarkerArtist), target :: heo_marks
    type(SegmentArtist), target :: heo_radials
    type(RibbonArtist), target :: heo_plane
    type(SkySphere), target :: sky
    type(FrameManager), target :: scene
    type(WindowProxy) :: window
    type(View), target :: global, earth_fixed, chase
    type(HUDText), target :: title, note
    character(len=:), allocatable :: file, dir
    character(len=1024) :: arg
    real(wp) :: t, t_end, r(3), v(3)
    integer :: k, n, frames, max_frames
    logical :: ok

    max_frames = 600
    dir = '.'
    if (command_argument_count() >= 1) then
        call get_command_argument(1, arg)
        read(arg, *) max_frames
    end if
    if (command_argument_count() >= 2) then
        call get_command_argument(2, arg)
        dir = trim(arg)
    end if

    ! the data: two orbits (positions, and the low one's attitudes) and the Earth's turn, over 4 hours
    t_end = 4.0_wp * 3600.0_wp
    n = nint(t_end / dt) + 1
    spin = Trajectory()
    leo = Trajectory()
    heo = Trajectory()
    call spin%reserve(n, attitudes=.true.)
    call leo%reserve(n, attitudes=.true.)
    call heo%reserve(n)
    do k = 1, n
        t = (k - 1) * dt
        call spin%add(t, pos=[0.0_wp, 0.0_wp, 0.0_wp], &
                      att=quat_from_axis_angle([0.0_wp, 0.0_wp, 1.0_wp], earth_rate * t))
        call kepler(leo_a, 0.0_wp, leo_inc, 0.3_wp, 0.0_wp, t, r, v)
        call leo%add(t, pos=r, att=pointing(r, v))
        call kepler(heo_a, heo_e, heo_inc, -0.6_wp, 4.0_wp, t, r, v)
        call heo%add(t, pos=r)
    end do

    ! the scene
    eci = ReferenceFrame('ECI')
    call eci%show_decorations(.false.)
    axes = CoordinateAxes('Inertial axes', color=[0.7_wp, 0.75_wp, 0.85_wp, 0.8_wp])
    call axes%set_axis_length(2.2_wp * re)
    call axes%set_tick_spacing(re, 0.5_wp * re)
    call axes%show_decorations(.false.)
    earth = Sphere('Earth', radius=re)
    if (len(find_data_file('2k_earth_daymap.jpg')) > 0) then
        call earth%set_texture_map('2k_earth_daymap.jpg')
    else
        call earth%set_color([0.2_wp, 0.4_wp, 0.9_wp])
    end if
    call earth%follow(spin, data=FOLLOW_ATTITUDE, mode=FOLLOW_LIMIT)
    call earth%show_decorations(.false.)
    grid = LatLonGrid('Grid', color=[0.9_wp, 0.9_wp, 1.0_wp, 0.25_wp])
    call grid%set_parameters(1.002_wp * re, 1.002_wp * re, 1.002_wp * re, pi / 6.0_wp, pi / 6.0_wp)
    call grid%show_decorations(.false.)

    ! the low spacecraft: the model (or a frame), following the orbit with its attitude, with a sensor cone
    craft = Model('Explorer')
    file = find_data_file('Models/pepper/pepper_logo.glb')
    if (len(file) > 0) then
        if (craft%set_model(file)) then
            call craft%set_model_pivot([0.0_wp, 0.0_wp, 0.0_wp])
            call craft%set_model_scale([1.0_wp, 1.0_wp, 1.0_wp] * model_size / logo_radius)
        end if
    end if
    call craft%follow(leo, mode=FOLLOW_LIMIT)
    call craft%show_axes(NO_AXES)
    call craft%show_axes_labels(NO_AXES)
    call craft%set_color([1.0_wp, 0.85_wp, 0.4_wp])
    sensor = EllipticCone('Sensor', color=[0.4_wp, 1.0_wp, 0.6_wp])
    call sensor%set_primary_angles(18.0_wp * pi / 180.0_wp, 10.0_wp * pi / 180.0_wp)
    call sensor%set_cone_length(650.0_wp)
    call sensor%set_cone_color([0.4_wp, 1.0_wp, 0.6_wp, 0.25_wp])
    call sensor%show_decorations(.false.)

    leo_curve = CurveArtist(leo, color=[1.0_wp, 0.8_wp, 0.3_wp], width=2.5_wp)
    call leo_curve%set_trail(70.0_wp * 60.0_wp, 15.0_wp * 60.0_wp, fade=0.6_wp)
    leo_frame = DrawableTrajectory('Low orbit')
    call leo_frame%add_artist(leo_curve)
    call leo_frame%show_decorations(.false.)

    ! the elliptical orbit: traced, its plane filled, radial lines and markers every 10 minutes
    heo_curve = CurveArtist(heo, color=[0.35_wp, 0.8_wp, 1.0_wp], width=2.0_wp)
    call heo_curve%set_trace_mode(.true.)
    heo_plane = RibbonArtist(heo, color=[0.35_wp, 0.8_wp, 1.0_wp, 0.12_wp])
    call heo_plane%set_trace_mode(.true.)
    heo_radials = SegmentArtist(heo, color=[0.35_wp, 0.8_wp, 1.0_wp, 0.5_wp], width=1.0_wp, stride=40)
    call heo_radials%set_trace_mode(.true.)
    heo_marks = MarkerArtist(heo, markers=ior(ior(MARKER_START, MARKER_INTERMEDIATE), MARKER_END), size=9.0_wp, &
                             color=[1.0_wp, 1.0_wp, 1.0_wp])
    call heo_marks%set_intermediate_type(INTERMEDIATE_TIME)
    call heo_marks%set_intermediate_spacing(600.0_wp)
    call heo_marks%set_trace_mode(.true.)
    heo_frame = DrawableTrajectory('Elliptical orbit')
    call heo_frame%add_artist(heo_plane)
    call heo_frame%add_artist(heo_curve)
    call heo_frame%add_artist(heo_radials)
    call heo_frame%add_artist(heo_marks)
    call heo_frame%show_decorations(.false.)

    ok = eci%add_child(axes) .and. eci%add_child(earth) .and. earth%add_child(grid) .and. &
         eci%add_child(craft) .and. craft%add_child(sensor) .and. eci%add_child(leo_frame) .and. &
         eci%add_child(heo_frame)
    if (.not. ok) error stop 'web_export: add_child failed'
    scene = FrameManager(eci)

    sky = SkySphere()
    if (.not. sky%set_star_data('Stars_HYGv3.txt', min_mag=-2.0_wp, max_mag=6.5_wp, max_stars=20000)) &
        write(*, '(a)') 'web_export: no star catalog'
    if (.not. sky%set_constellations(.true., 'inp_Constellation.txt', rgba=[0.35_wp, 0.45_wp, 0.7_wp, 0.35_wp])) &
        write(*, '(a)') 'web_export: no constellations'

    ! the window: three views, a title
    if (.not. window%open(1100, 750, 'vkframes: export to the web')) error stop 'web_export: could not open the window'
    call window%set_scene(scene)
    call window%set_sky(sky)
    global = View(eci, eci)
    call global%set_default_view_parameters(eye=[0.9_wp, -2.6_wp, 1.2_wp] * heo_a, &
                                            center=[-0.3_wp, 0.0_wp, 0.0_wp] * heo_a, up=[0.0_wp, 0.0_wp, 1.0_wp])
    earth_fixed = View(eci, earth)
    call earth_fixed%set_default_view_parameters(eye=[3.5_wp, -1.5_wp, 2.0_wp] * re, &
                                                 center=[0.0_wp, 0.0_wp, 0.0_wp], up=[0.0_wp, 0.0_wp, 1.0_wp])
    chase = View(eci, craft)
    call chase%set_default_view_parameters(eye=[-3.0_wp, -2.0_wp, 1.6_wp] * model_size, &
                                           center=[0.0_wp, 0.0_wp, 0.0_wp], up=[0.0_wp, 0.0_wp, 1.0_wp])
    call window%add_view(global)
    call window%add_view(earth_fixed)
    call window%add_view(chase)
    title = HUDText('vkframes: export to the web', position=[0.015_wp, 0.985_wp], alignment=ALIGN_LEFT_TOP, &
                    size=20.0_wp)
    note = HUDText('two orbits over four hours; the Earth turning', position=[0.015_wp, 0.945_wp], &
                   alignment=ALIGN_LEFT_TOP, size=14.0_wp, color=[0.75_wp, 0.8_wp, 0.9_wp])
    call window%add_hud_text(title)
    call window%add_hud_text(note)
    call window%set_time_range(0.0_wp, t_end)
    call window%set_time(0.0_wp)
    call window%set_time_scale(300.0_wp)                    ! (5 minutes a second)
    do k = 1, 3
        if (.not. window%frame()) exit
    end do

    ! a snapshot at 30 minutes, then the whole range animated
    if (.not. window%export_web(dir//'/web_export_snapshot.html', time=1800.0_wp, &
                                title='vkframes: a snapshot')) error stop 'web_export: export failed'
    if (.not. window%export_web(dir//'/web_export.html', animate=.true., &
                                title='vkframes: two orbits')) error stop 'web_export: export failed'

    ! recorded: the window runs, the program scans the sensor (+-25 degrees, every 20 s of the window's time)
    if (.not. window%start_web_recording(dir//'/web_export_recorded.html', title='vkframes: recorded')) &
        error stop 'web_export: could not start recording'
    frames = 0
    do while (frames < max_frames)
        call sensor%set_attitude(quat_from_axis_angle([1.0_wp, 0.0_wp, 0.0_wp], &
                                 25.0_wp * pi / 180.0_wp * sin(2.0_wp * pi * window%time() / 1200.0_wp)))
        if (.not. window%frame()) exit
        frames = frames + 1
    end do
    if (.not. window%stop_web_recording()) write(*, '(a)') 'web_export: the recorded page was not written'
    write(*, '(a,i0,a,f8.1,a)') 'recorded ', frames, ' frames, to t = ', window%time(), ' s'
    call window%close()

contains

    !> Position and velocity on an orbit (semi-major axis a, eccentricity e,
    !> inclination i, node raan, argument of periapsis w) at time t (periapsis at 0).
    subroutine kepler(a, e, i, raan, w, t, r, v)
        real(wp), intent(in) :: a, e, i, raan, w, t
        real(wp), intent(out) :: r(3), v(3)
        real(wp) :: n, m, ea, p(3), dp(3), rot(3,3), edot
        integer :: k
        n = sqrt(mu / a**3)
        m = n * t
        ea = m
        do k = 1, 30
            ea = ea - (ea - e * sin(ea) - m) / (1.0_wp - e * cos(ea))
        end do
        edot = n / (1.0_wp - e * cos(ea))
        p = [a * (cos(ea) - e), a * sqrt(1.0_wp - e**2) * sin(ea), 0.0_wp]
        dp = [-a * sin(ea), a * sqrt(1.0_wp - e**2) * cos(ea), 0.0_wp] * edot
        rot = matmul(rot_z(raan), matmul(rot_x(i), rot_z(w)))
        r = matmul(rot, p)
        v = matmul(rot, dp)
    end subroutine kepler

    pure function rot_z(a) result(m)
        real(wp), intent(in) :: a
        real(wp) :: m(3,3)
        m = reshape([cos(a), sin(a), 0.0_wp, -sin(a), cos(a), 0.0_wp, 0.0_wp, 0.0_wp, 1.0_wp], [3, 3])
    end function rot_z

    pure function rot_x(a) result(m)
        real(wp), intent(in) :: a
        real(wp) :: m(3,3)
        m = reshape([1.0_wp, 0.0_wp, 0.0_wp, 0.0_wp, cos(a), sin(a), 0.0_wp, -sin(a), cos(a)], [3, 3])
    end function rot_x

    !> The spacecraft's attitude: x along the track, z up (away from the
    !> Earth), so its sensor (along -z) looks down.
    function pointing(r, v) result(q)
        real(wp), intent(in) :: r(3), v(3)
        real(wp) :: q(4), x(3), y(3), z(3)
        z = r / norm2(r)
        x = v - dot_product(v, z) * z
        x = x / norm2(x)
        y = [z(2) * x(3) - z(3) * x(2), z(3) * x(1) - z(1) * x(3), z(1) * x(2) - z(2) * x(1)]
        q = quat_from_matrix(reshape([x, y, z], [3, 3]))
    end function pointing

end program web_export
