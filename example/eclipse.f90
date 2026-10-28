!> A solar eclipse: the Moon's shadow on the Earth (ShadowCones). The Earth
!> orbits the Sun (a light source, an astronomical unit away) and the Moon the
!> Earth (on a circular orbit near perigee, 363,300 km, inclined 5.1 degrees,
!> with the new moon at t = 0 at a node: a total eclipse). The Moon's shadow
!> shows its umbra (red: total eclipse), which just reaches the Earth, the
!> antumbra beyond its apex (blue: annular eclipse) and the wide penumbra
!> (yellow: partial eclipse); the Earth's shadow points away from the Sun the
!> other way (the Moon passes near it at the full moon, two weeks later). Both
!> shadows follow their bodies along their orbits and are reshaped for their
!> distances to the Sun. The view is on the Earth's day side; zoom out (mouse
!> wheel) to see the whole of the Moon's shadow. A HUD line shows the time
!> from the new moon and the umbra's length.
!>
!> Arguments (optional): number of frames to run, then a file to capture the
!> last frame to: `pixi run example eclipse -- 300 eclipse.png`
program eclipse

    use vkframes

    implicit none

    real(wp), parameter :: re = 6378.137_wp, rm = 1737.4_wp, rs = 695700.0_wp  !! radii (km)
    real(wp), parameter :: au = 1.495978707e8_wp                                !! km
    real(wp), parameter :: moon_distance = 363300.0_wp                          !! km (near perigee)
    real(wp), parameter :: year = 365.256_wp * 86400.0_wp, month = 27.3217_wp * 86400.0_wp   !! sidereal (s)
    real(wp), parameter :: inclination = 5.14_wp * pi / 180.0_wp
    real(wp), parameter :: flattening = 1.0_wp / 298.257223563_wp
    real(wp), parameter :: step = 600.0_wp, span = 30.0_wp * 86400.0_wp        !! the orbits' sampling (s)

    type(ReferenceFrame), target :: root, earth_center
    type(Sphere), target :: sun, earth, moon
    type(ShadowCones), target :: moon_shadow, earth_shadow
    type(Trajectory), target :: earth_orbit, moon_orbit
    type(FrameManager), target :: scene
    type(WindowProxy), target :: window
    type(View), target :: earth_view
    type(HUDText), target :: info
    character(len=1024) :: arg, file
    character(len=96) :: text
    real(wp) :: t, u
    integer :: k, frames, frame
    logical :: running

    frames = huge(frames)
    file = ''
    if (command_argument_count() >= 1) then
        call get_command_argument(1, arg)
        read(arg, *) frames
    end if
    if (command_argument_count() >= 2) call get_command_argument(2, file)

    ! the orbits, from a day before the new moon (t = 0) for a month: the
    ! Earth's around the Sun, the Moon's around the Earth (its ascending node
    ! toward the Sun at t = 0)
    earth_orbit = Trajectory()
    moon_orbit = Trajectory()
    do k = 0, nint(span / step)
        t = -86400.0_wp + k * step
        u = 2.0_wp * pi * t / year
        call earth_orbit%add(t, pos=au * [cos(u), sin(u), 0.0_wp])
        u = pi + 2.0_wp * pi * t / month
        call moon_orbit%add(t, pos=moon_distance * [cos(u), sin(u) * cos(inclination), sin(u) * sin(inclination)])
    end do

    root = ReferenceFrame('Heliocentric')
    call root%show_decorations(.false.)
    sun = Sphere('Sun', radius=rs, color=[1.0_wp, 0.85_wp, 0.4_wp])
    call sun%set_material(emission=[1.0_wp, 0.85_wp, 0.4_wp])
    call sun%set_light_source_enabled(.true.)
    call sun%set_light_diffuse([1.0_wp, 1.0_wp, 1.0_wp])
    call sun%set_light_ambient([0.04_wp, 0.04_wp, 0.04_wp])
    call sun%show_decorations(.false.)

    ! the Earth's center moves along its orbit; the Earth and the Moon's orbit around it
    earth_center = ReferenceFrame('Earth center')
    call earth_center%show_decorations(.false.)
    call earth_center%follow(earth_orbit, data=FOLLOW_POSITION)
    earth = Sphere('Earth', radius=re)
    call earth%set_sphere_scale([1.0_wp, 1.0_wp, 1.0_wp - flattening])
    call earth%set_texture_map('2k_earth_daymap.jpg')                    ! (data/, on VKFRAMES_DATA_PATH)
    call earth%set_night_texture_map('2k_earth_nightmap.jpg')
    call earth%set_attitude(quat_from_axis_angle([0.0_wp, 0.0_wp, 1.0_wp], 1.2_wp))
    call earth%show_decorations(.false.)
    moon = Sphere('Moon', radius=rm, color=[0.75_wp, 0.75_wp, 0.72_wp])
    call moon%show_decorations(.false.)
    call moon%follow(moon_orbit, data=FOLLOW_POSITION)

    ! the shadows: each a child of its body, the radii from the Spheres
    moon_shadow = ShadowCones('Moon shadow', body=moon, light=sun)
    earth_shadow = ShadowCones('Earth shadow', body=earth, light=sun)
    if (.not. (root%add_child(sun) .and. root%add_child(earth_center) .and. earth_center%add_child(earth) .and. &
               earth_center%add_child(moon) .and. moon%add_child(moon_shadow) .and. earth%add_child(earth_shadow))) &
        error stop 'add_child failed'
    scene = FrameManager(root)

    if (.not. window%open(1100, 800, 'vkframes: a solar eclipse')) error stop 'could not open a window'
    call window%set_scene(scene)
    call window%set_background_color([0.0_wp, 0.0_wp, 0.02_wp])
    call window%set_time_scale(300.0_wp)                                 ! 5 minutes a second
    call window%set_time(-2.0_wp * 3600.0_wp)
    earth_view = View(root, earth_center)
    call earth_view%set_default_view_parameters(eye=[-26000.0_wp, -19000.0_wp, 9000.0_wp], &
                                                center=[0.0_wp, 0.0_wp, 0.0_wp], up=[0.0_wp, 0.0_wp, 1.0_wp])
    call window%add_view(earth_view)
    info = HUDText('', position=[0.01_wp, 0.01_wp], alignment=ALIGN_LEFT_BOTTOM, size=18.0_wp, &
                   color=[0.8_wp, 0.8_wp, 0.8_wp])
    call window%add_hud_text(info)

    running = .true.
    frame = 0
    do while (running .and. frame < frames)
        running = window%frame()
        frame = frame + 1
        write(text, '(a,f7.2,a,f9.0,a,f9.0,a)') 't =', window%time() / 3600.0_wp, ' h from the new moon   umbra:', &
            moon_shadow%umbra_length(), ' km long, Moon', norm2(earth%position_of(moon)), ' km away'
        call info%set_text(trim(text))
    end do
    if (running .and. len_trim(file) > 0) then
        if (window%capture(trim(file))) write(*, '(a)') 'captured '//trim(file)
    end if
    call window%close()

end program eclipse
