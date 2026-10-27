!> Opens a VSG window with a sphere and trackball control, and runs the frame
!> loop from Fortran until the window is closed (or for N frames, if a frame
!> count is given as the first command-line argument).
program hello_sphere

    use, intrinsic :: iso_c_binding
    use vkf_layer
    use vkf_geometry, only: sphere_create

    implicit none

    character(len=*), parameter :: title = 'vkframes: hello sphere'
    type(c_ptr) :: viewer, sphere
    integer :: max_frames, frame, status
    character(len=32) :: arg

    max_frames = huge(max_frames)
    if (command_argument_count() >= 1) then
        call get_command_argument(1, arg)
        read(arg, *) max_frames
    end if

    write(*, '(a)') 'Vulkan device: '//vulkan_device_name()

    viewer = vkf_viewer_create(800_c_int, 600_c_int, title, len(title, kind=c_int), 0_c_long_long, 1.0_c_double, 1_c_int)
    if (.not. c_associated(viewer)) error stop 'could not create the viewer'

    sphere = sphere_create(1.0_c_double, [0.2_c_float, 0.4_c_float, 1.0_c_float, 1.0_c_float])
    if (.not. c_associated(sphere)) error stop 'could not create the sphere'
    if (vkf_viewer_add_node(viewer, sphere) /= 0) error stop 'could not add the sphere'
    call vkf_node_delete(sphere)

    frame = 0
    do while (frame < max_frames)
        status = vkf_viewer_frame(viewer)
        if (status < 0) error stop 'frame failed'
        if (status == 0) exit
        frame = frame + 1
    end do
    write(*, '(a,i0,a)') 'rendered ', frame, ' frames'

    call vkf_viewer_delete(viewer)

end program hello_sphere
