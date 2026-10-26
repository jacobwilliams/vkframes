!> Reference shapes around the Earth (OpenFrames' CoordinateAxes, LatLonGrid,
!> RadialPlane, EllipticCone and RectangularCone): a latitude/longitude grid
!> just above the surface, coordinate axes with tick marks every 1000 km, an
!> equatorial radial plane, and a spacecraft's sensor cones pointed at the
!> Earth (make_cone_look_at), lit by the Sun.
!>
!> Arguments (optional): the number of frames to run, and a file to capture
!> the last frame to:
!>   pixi run example shapes -- 60 shapes.png
program shapes

    use vkframes

    implicit none

    real(wp), parameter :: re = 6378.137_wp                         !! km
    real(wp), parameter :: au = 1.495978707e8_wp                    !! km
    real(wp), parameter :: flattening = 1.0_wp / 298.257223563_wp
    real(wp), parameter :: deg = pi / 180.0_wp

    type(ReferenceFrame), target :: root, sun, craft
    type(Sphere), target :: earth
    type(LatLonGrid), target :: grid
    type(CoordinateAxes), target :: axes
    type(RadialPlane), target :: plane
    type(EllipticCone), target :: sensor
    type(RectangularCone), target :: camera
    type(FrameManager), target :: scene
    type(WindowProxy) :: window
    type(View), target :: earth_view
    character(len=1024) :: arg, capture_file
    real(wp) :: craft_pos(3)
    integer :: max_frames, frame
    logical :: running

    max_frames = huge(max_frames)
    capture_file = ''
    if (command_argument_count() >= 1) then
        call get_command_argument(1, arg)
        read(arg, *) max_frames
    end if
    if (command_argument_count() >= 2) call get_command_argument(2, capture_file)

    root = ReferenceFrame('Root')
    call root%show_axes(NO_AXES)
    call root%show_axes_labels(NO_AXES)
    call root%show_name_label(.false.)

    earth = Sphere('Earth', radius=re)
    call earth%set_sphere_scale([1.0_wp, 1.0_wp, 1.0_wp - flattening])
    call earth%set_texture_map('2k_earth_daymap.jpg')
    call earth%set_night_texture_map('2k_earth_nightmap.jpg')
    call earth%show_axes(NO_AXES)
    call earth%show_axes_labels(NO_AXES)

    ! a grid just above the surface (every 15 degrees), in the Earth's frame
    grid = LatLonGrid('Grid', color=[0.6_wp, 0.8_wp, 1.0_wp, 0.5_wp])
    call grid%set_parameters(1.003_wp * re, 1.003_wp * re, 1.003_wp * (1.0_wp - flattening) * re, 15.0_wp * deg, 15.0_wp * deg)
    call grid%show_axes(NO_AXES)
    call grid%show_axes_labels(NO_AXES)
    call grid%show_name_label(.false.)
    if (.not. earth%add_child(grid)) error stop 'add_child failed'

    ! axes 2 Earth radii long, ticks every 1000 km (major) and 250 km (minor)
    axes = CoordinateAxes('Axes', color=[1.0_wp, 1.0_wp, 0.6_wp])
    call axes%set_axis_length(2.0_wp * re)
    call axes%set_tick_spacing(1000.0_wp, 250.0_wp)
    call axes%set_axis_width(2.0_wp)
    call axes%show_name_label(.false.)

    ! the equatorial plane, 3 Earth radii, circles every Earth radius
    plane = RadialPlane('Equator')
    call plane%set_parameters(3.0_wp * re, re, 30.0_wp * deg)
    call plane%set_line_color([1.0_wp, 1.0_wp, 1.0_wp, 0.5_wp])
    call plane%show_axes(NO_AXES)
    call plane%show_axes_labels(NO_AXES)
    call plane%show_name_label(.false.)

    ! a spacecraft above the plane, its sensors pointed at the Earth's center
    craft_pos = 2.2_wp * re * [cos(0.3_wp), -sin(0.3_wp), 0.5_wp]
    craft = ReferenceFrame('Spacecraft', color=[1.0_wp, 0.5_wp, 0.3_wp])
    call craft%set_position(craft_pos)
    call craft%show_axes(NO_AXES)
    sensor = EllipticCone('Sensor', color=[0.3_wp, 1.0_wp, 0.4_wp, 0.35_wp])
    call sensor%set_primary_angles(12.0_wp * deg, 6.0_wp * deg)
    call sensor%set_cone_length(norm2(craft_pos) - re)
    call sensor%make_cone_look_at([0.0_wp, 0.0_wp, 0.0_wp], -craft_pos, [0.0_wp, 0.0_wp, 1.0_wp])
    call sensor%show_axes(NO_AXES)
    call sensor%show_axes_labels(NO_AXES)
    call sensor%show_name_label(.false.)
    camera = RectangularCone('Camera', color=[1.0_wp, 0.6_wp, 0.2_wp, 0.25_wp])
    call camera%set_primary_angles(4.0_wp * deg, 3.0_wp * deg)
    call camera%set_cone_length(norm2(craft_pos) - re)
    call camera%make_cone_look_at([0.0_wp, 0.0_wp, 0.0_wp], -craft_pos + [0.0_wp, 0.0_wp, 0.4_wp * re], &
                                  [0.0_wp, 0.0_wp, 1.0_wp])
    call camera%show_axes(NO_AXES)
    call camera%show_axes_labels(NO_AXES)
    call camera%show_name_label(.false.)
    if (.not. (craft%add_child(sensor) .and. craft%add_child(camera))) error stop 'add_child failed'

    ! the Sun lights the scene
    sun = ReferenceFrame('Sun')
    call sun%set_position(au * [cos(1.0_wp), -sin(1.0_wp), 0.3_wp])
    call sun%set_light_source_enabled(.true.)
    call sun%set_light_diffuse([1.0_wp, 1.0_wp, 1.0_wp])
    call sun%set_light_ambient([0.1_wp, 0.1_wp, 0.1_wp])

    if (.not. (root%add_child(earth) .and. root%add_child(axes) .and. root%add_child(plane) .and. &
               root%add_child(craft) .and. root%add_child(sun))) error stop 'add_child failed'
    scene = FrameManager(root)

    if (.not. window%open(1000, 750, 'vkframes: reference shapes')) error stop 'could not open a window'
    call window%set_scene(scene)
    earth_view = View(root, earth, frame_type=ABSOLUTE_FRAME)
    call earth_view%set_default_view_parameters(eye=[2.5_wp, -6.0_wp, 3.0_wp] * re, center=[0.0_wp, 0.0_wp, 0.0_wp], &
                                                up=[0.0_wp, 0.0_wp, 1.0_wp])
    call window%add_view(earth_view)

    frame = 0
    running = .true.
    do while (running .and. frame < max_frames)
        running = window%frame()
        frame = frame + 1
    end do
    if (len_trim(capture_file) > 0 .and. running) then
        if (window%capture(trim(capture_file))) write(*, '(a)') 'captured '//trim(capture_file)
    end if
    call window%close()

end program shapes
