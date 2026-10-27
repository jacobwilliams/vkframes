!> The Earth lit by the Sun: day and night textures (data/2k_earth_daymap.jpg
!> and 2k_earth_nightmap.jpg), a light source on a Sun frame an astronomical
!> unit away (OpenFrames' setLightSourceEnabled), and a specular glint
!> (Sphere%set_material). The night side shows the night texture (city
!> lights), as OpenFrames' setNightTextureMap, fading in where the Earth is
!> lit weakly; the Earth's twilight (Sphere%set_twilight: 6 degrees, civil
!> twilight, the atmosphere's) carries a little light past the terminator and
!> fades the lights in more gradually.
!>
!> Arguments (optional): the number of frames to run, and a file to capture
!> the last frame to:
!>   pixi run example day_night -- 60 day_night.png
program day_night

    use vkframes

    implicit none

    real(wp), parameter :: re = 6378.137_wp                         !! km
    real(wp), parameter :: au = 1.495978707e8_wp                    !! km
    real(wp), parameter :: flattening = 1.0_wp / 298.257223563_wp

    type(ReferenceFrame), target :: root, sun
    type(Sphere), target :: earth
    type(FrameManager), target :: scene
    type(WindowProxy) :: window
    type(View), target :: earth_view
    character(len=1024) :: arg, capture_file
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

    ! textures found by name on VKFRAMES_DATA_PATH (data/)
    earth = Sphere('Earth', radius=re)
    call earth%set_sphere_scale([1.0_wp, 1.0_wp, 1.0_wp - flattening])
    call earth%set_texture_map('2k_earth_daymap.jpg')
    call earth%set_night_texture_map('2k_earth_nightmap.jpg')
    call earth%set_material(specular=[0.4_wp, 0.4_wp, 0.4_wp], shininess=30.0_wp)
    call earth%set_twilight(6.0_wp * pi / 180.0_wp)                    ! (an atmosphere's; none for an airless body)
    call earth%set_attitude(quat_from_axis_angle([0.0_wp, 0.0_wp, 1.0_wp], 2.6_wp))

    ! the Sun: 1 AU away, to the right of the view and a little toward the viewer
    sun = ReferenceFrame('Sun', color=[1.0_wp, 0.9_wp, 0.5_wp])
    call sun%set_position(au * [cos(0.6_wp), -sin(0.6_wp), 0.0_wp])
    call sun%set_light_source_enabled(.true.)
    call sun%set_light_diffuse([1.0_wp, 1.0_wp, 1.0_wp])
    call sun%set_light_ambient([0.05_wp, 0.05_wp, 0.05_wp])
    call sun%set_light_specular([1.0_wp, 1.0_wp, 0.9_wp])

    if (.not. (root%add_child(earth) .and. root%add_child(sun))) error stop 'add_child failed'
    scene = FrameManager(root)

    if (.not. window%open(900, 700, 'vkframes: day and night')) error stop 'could not open a window'
    call window%set_scene(scene)
    earth_view = View(root, earth, frame_type=ABSOLUTE_FRAME)
    call earth_view%set_default_view_distance(1.5_wp * re)
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

end program day_night
