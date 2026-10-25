!> A textured Earth (the WGS84 ellipsoid) and a spacecraft model following an
!> orbit, with vkframes' API: a Sphere, a Model that follows a Trajectory, a
!> FrameManager, a WindowProxy and three Views (of the Earth, of the
!> spacecraft, and from the spacecraft toward the Earth).
!>
!> Arguments: texture file, model file, then optionally the number of frames to
!> run, a file to capture the last frame to, and the view to show (1 to 3).
!> For example, with OpenFrames' demo data:
!>   earth ~/git/OpenFrames/Demos/Images/land_shallow_topo_2048.jpg \
!>         ~/git/OpenFrames/Demos/Models/Hubble.3ds
program earth

    use vkframes

    implicit none

    real(wp), parameter :: re = 6378.137_wp                         !! WGS84 equatorial radius (km)
    real(wp), parameter :: flattening = 1.0_wp / 298.257223563_wp
    real(wp), parameter :: r_orbit = 1.6_wp * re, inc = 30.0_wp * pi / 180.0_wp
    real(wp), parameter :: period = 600.0_wp                        !! s (sped up)
    real(wp), parameter :: model_size = 0.15_wp * re                 !! bounding radius of the drawn model (km)
    integer, parameter :: n = 361

    type(Sphere), target :: globe
    type(Model), target :: spacecraft
    type(Trajectory), target :: orbit
    type(FrameManager), target :: scene
    type(WindowProxy) :: window
    type(View), target :: earth_view, craft_view, nadir_view
    character(len=1024) :: texture_file, model_file, arg, capture_file
    real(wp) :: t, u, center(3), radius
    integer :: max_frames, k, which

    if (command_argument_count() < 2) error stop 'usage: earth <texture file> <model file> [frames] [capture file]'
    call get_command_argument(1, texture_file)
    call get_command_argument(2, model_file)
    max_frames = huge(max_frames)
    capture_file = ''
    if (command_argument_count() >= 3) then
        call get_command_argument(3, arg)
        read(arg, *) max_frames
    end if
    if (command_argument_count() >= 4) call get_command_argument(4, capture_file)
    which = 1
    if (command_argument_count() >= 5) then
        call get_command_argument(5, arg)
        read(arg, *) which
    end if

    ! the Earth: a sphere scaled to the WGS84 ellipsoid, textured
    globe = Sphere('Earth', radius=re)
    call globe%set_sphere_scale([1.0_wp, 1.0_wp, 1.0_wp - flattening])
    call globe%set_texture_map(trim(texture_file))

    ! the spacecraft: a model centered on its bound and sized, following a circular orbit
    spacecraft = Model('Spacecraft', color=[1.0_wp, 0.85_wp, 0.2_wp])
    if (.not. spacecraft%set_model(trim(model_file))) error stop 'could not read the model'
    call spacecraft%model_bound(center, radius)
    call spacecraft%set_model_pivot(center)
    call spacecraft%set_model_scale([1.0_wp, 1.0_wp, 1.0_wp] * model_size / radius)
    orbit = Trajectory()
    do k = 1, n
        t = period * real(k - 1, wp) / real(n - 1, wp)
        u = 2.0_wp * pi * t / period
        call orbit%add(t, pos=r_orbit * [cos(u), sin(u) * cos(inc), sin(u) * sin(inc)])
    end do
    call spacecraft%follow(orbit)
    if (.not. globe%add_child(spacecraft)) error stop 'add_child failed'
    write(*, '(a)', advance='no') globe%frame_string()

    ! views: the Earth (framed by its bound), the spacecraft (body-fixed), and
    ! from the spacecraft toward the Earth (inertial axes)
    earth_view = View(globe, globe)
    craft_view = View(globe, spacecraft)
    nadir_view = View(globe, spacecraft, lookat=globe, frame_type=ABSOLUTE_FRAME)
    call nadir_view%set_default_view_distance(0.5_wp * model_size)

    ! the window
    scene = FrameManager(globe)
    if (.not. window%open(1000, 700, 'vkframes: earth')) error stop 'could not open the window'
    call window%set_scene(scene)
    call window%add_view(earth_view)
    call window%add_view(craft_view)
    call window%add_view(nadir_view)
    call window%select_view(which)
    call window%run(max_frames)

    if (len_trim(capture_file) > 0 .and. window%is_open()) then
        if (window%capture(trim(capture_file))) write(*, '(a)') 'captured '//trim(capture_file)
    end if
    call window%close()

end program earth
