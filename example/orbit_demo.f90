!> The program in the README's "A small program": the Earth, a spacecraft
!> following an inclined circular orbit (a Trajectory), the orbit drawn as a
!> line, and a view of the Earth, in a window of its own until it is closed.
!> (Kept the same as the README's.)
program orbit_demo
    use vkframes
    implicit none
    real(wp), parameter :: re = 6378.137_wp, inc = 50.0_wp * pi / 180.0_wp
    type(Sphere), target :: earth, craft
    type(Trajectory), target :: orbit
    type(DrawableTrajectory), target :: orbit_frame
    type(CurveArtist), target :: orbit_line
    type(FrameManager), target :: scene
    type(WindowProxy), target :: window
    type(View), target :: earth_view
    real(wp) :: t, a
    integer :: k

    earth = Sphere('Earth', radius=re)
    call earth%set_texture_map('2k_earth_daymap.jpg')   ! (in data/, found on VKFRAMES_DATA_PATH)
    craft = Sphere('Spacecraft', radius=300.0_wp, color=[1.0_wp, 0.3_wp, 0.2_wp])

    ! a circular orbit inclined 50 degrees, sampled over one period (600 s here)
    orbit = Trajectory()
    do k = 0, 360
        t = 600.0_wp * k / 360.0_wp
        a = 2.0_wp * pi * t / 600.0_wp
        call orbit%add(t, pos=1.6_wp * re * [cos(a), sin(a) * cos(inc), sin(a) * sin(inc)])
    end do
    call craft%follow(orbit)

    ! the orbit drawn as a line
    orbit_line = CurveArtist(orbit, color=[1.0_wp, 0.8_wp, 0.2_wp], width=2.0_wp)
    orbit_frame = DrawableTrajectory('Orbit')
    call orbit_frame%add_artist(orbit_line)
    if (.not. (earth%add_child(craft) .and. earth%add_child(orbit_frame))) error stop 'add_child failed'
    scene = FrameManager(earth)

    if (.not. window%open(800, 600, 'vkframes')) error stop 'could not open a window'
    call window%set_scene(scene)
    earth_view = View(earth, earth)
    call earth_view%set_default_view_distance(2.5_wp * re)
    call window%add_view(earth_view)
    do while (window%frame())       ! until the window is closed
    end do
    call window%close()
end program orbit_demo
