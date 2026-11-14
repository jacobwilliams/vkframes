!> A 3D model turning in front of the stars: the pepper logo (a glTF model in
!> data/Models/pepper) centered on its bound and sized, spinning about its
!> vertical axis, over a star map (the HYG catalog's stars to magnitude 6.5,
!> and faint constellation lines).
!>
!> The model is a Model frame (its file read with vsgXchange: glTF, 3ds, obj,
!> ...), placed by its pivot, scale and attitude; each frame the frame's
!> attitude is set from the time, a turn every `period` seconds. The data files are found on VKFRAMES_DATA_PATH (data/).
!>
!> Arguments (optional): the number of frames to run, then a file to capture
!> the last frame to:
!>   pixi run example spinning_model
!>   pixi run example spinning_model -- 240 spinning_model.png
program spinning_model

    use vkframes
    use vkf_files, only: find_data_file

    implicit none

    character(len=*), parameter :: model_name = 'Models/pepper/pepper_logo.glb'
    real(wp), parameter :: logo_radius = 4.6_wp             !! the logo's extent about its origin (model units)
    real(wp), parameter :: size = 1.0_wp                    !! that radius, drawn (scene units)
    real(wp), parameter :: period = 12.0_wp                 !! s per turn
    real(wp), parameter :: tilt = 15.0_wp * pi / 180.0_wp   !! its spin axis leaned toward the viewer

    type(ReferenceFrame), target :: root
    type(Model), target :: logo
    type(SkySphere), target :: sky
    type(FrameManager), target :: scene
    type(WindowProxy) :: window
    type(View), target :: front
    character(len=:), allocatable :: file
    character(len=1024) :: arg, capture_file
    real(wp) :: spin(4), lean(4)
    integer :: max_frames, frame

    max_frames = huge(max_frames)
    capture_file = ''
    if (command_argument_count() >= 1) then
        call get_command_argument(1, arg)
        read(arg, *) max_frames
    end if
    if (command_argument_count() >= 2) call get_command_argument(2, capture_file)

    ! the model: centered on its bound, sized; turned (below) about z
    file = find_data_file(model_name)
    if (len(file) == 0) error stop 'spinning_model: '//model_name//' not found (on VKFRAMES_DATA_PATH)'
    logo = Model('Pepper')
    if (.not. logo%set_model(file)) error stop 'spinning_model: could not read '//file
    ! (its bound would also take in the camera the file holds, 14 units in
    ! front of it: the logo itself is within about logo_radius of its
    ! origin; vsgXchange turns glTF's y up into z up, its face toward -y)
    call logo%set_model_pivot([0.0_wp, 0.0_wp, 0.0_wp])
    call logo%set_model_scale([1.0_wp, 1.0_wp, 1.0_wp] * size / logo_radius)
    call logo%show_decorations(.false.)

    root = ReferenceFrame('Root')
    call root%show_decorations(.false.)
    if (.not. root%add_child(logo)) error stop 'spinning_model: add_child failed'
    scene = FrameManager(root)

    ! the stars and the constellations
    sky = SkySphere()
    if (.not. sky%set_star_data('Stars_HYGv3.txt', min_mag=-2.0_wp, max_mag=6.5_wp, max_stars=40000)) &
        write(*, '(a)') 'spinning_model: no star catalog (the background stays black)'
    if (.not. sky%set_constellations(.true., 'inp_Constellation.txt', rgba=[0.35_wp, 0.45_wp, 0.7_wp, 0.35_wp])) &
        write(*, '(a)') 'spinning_model: no constellation file'

    ! a window looking at the model from the front, z up
    if (.not. window%open(1000, 700, 'vkframes: spinning model')) error stop 'spinning_model: could not open the window'
    call window%set_scene(scene)
    call window%set_sky(sky)
    call window%set_background_color([0.0_wp, 0.0_wp, 0.0_wp])
    front = View(root, root)
    call front%set_default_view_parameters(eye=[0.0_wp, -2.9_wp * size, 0.4_wp * size], &
                                           center=[0.0_wp, 0.0_wp, 0.0_wp], up=[0.0_wp, 0.0_wp, 1.0_wp])
    call window%add_view(front)

    ! turn it: a turn every `period` seconds about its axis, leaned toward the viewer
    lean = quat_from_axis_angle([1.0_wp, 0.0_wp, 0.0_wp], -tilt)
    frame = 0
    do while (frame < max_frames)
        spin = quat_from_axis_angle([0.0_wp, 0.0_wp, 1.0_wp], 2.0_wp * pi * window%time() / period)
        call logo%set_attitude(quat_multiply(lean, spin))
        if (.not. window%frame()) exit
        frame = frame + 1
    end do

    if (len_trim(capture_file) > 0 .and. window%is_open()) then
        if (window%capture(trim(capture_file))) write(*, '(a)') 'captured '//trim(capture_file)
    end if
    call window%close()

end program spinning_model
