!> A textured, rotating Earth (equirectangular texture on the WGS84 ellipsoid,
!> generated in Fortran) and a spacecraft model (read through vsgXchange)
!> orbiting it.
!>
!> Arguments: texture file, model file, then optionally the number of frames to
!> run and a file to capture the last frame to. For example, with OpenFrames'
!> demo data:
!>   earth ~/git/OpenFrames/Demos/Images/land_shallow_topo_2048.jpg \
!>         ~/git/OpenFrames/Demos/Models/Hubble.3ds
program earth

    use, intrinsic :: iso_c_binding
    use vkf_layer
    use vkf_geometry, only: ellipsoid_create

    implicit none

    integer, parameter :: wp = c_double
    real(wp), parameter :: pi = acos(-1.0_wp)
    real(wp), parameter :: radii(3) = [1.0_wp, 1.0_wp, 6356752.314245_wp / 6378137.0_wp]  !! WGS84, equatorial radii
    real(wp), parameter :: r_orbit = 1.6_wp             !! equatorial radii
    real(wp), parameter :: inc = 30.0_wp * pi / 180.0_wp
    real(wp), parameter :: model_size = 0.15_wp         !! radius of the model's bounding sphere, equatorial radii
    real(wp), parameter :: orbit_period = 600.0_wp      !! frames
    real(wp), parameter :: day = 2400.0_wp              !! frames

    character(len=*), parameter :: title = 'vkframes: earth'
    type(c_ptr) :: viewer, texture, globe, earth_transform, model, model_transform
    real(wp) :: model_center(3), model_radius, u, scale
    integer :: max_frames, frame, status
    character(len=1024) :: texture_file, model_file, arg, capture_file

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

    viewer = vkf_viewer_create(1000_c_int, 700_c_int, title, len(title, kind=c_int), 0_c_long_long, 1.0_c_double)
    if (.not. c_associated(viewer)) error stop 'could not create the viewer'
    call check(vkf_viewer_look_at(viewer, [3.0_wp, -3.0_wp, 1.5_wp], [0.0_wp, 0.0_wp, 0.0_wp], [0.0_wp, 0.0_wp, 1.0_wp]))

    texture = vkf_image_read(trim(texture_file), len_trim(texture_file, kind=c_int))
    if (.not. c_associated(texture)) error stop 'could not read the texture'
    globe = ellipsoid_create(radii, [1.0_c_float, 1.0_c_float, 1.0_c_float, 1.0_c_float], texture)
    if (.not. c_associated(globe)) error stop 'could not create the globe'
    earth_transform = vkf_transform_create()
    call check(vkf_node_add_child(earth_transform, globe))
    call check(vkf_viewer_add_node(viewer, earth_transform))
    call vkf_node_delete(globe)

    model = vkf_model_read(trim(model_file), len_trim(model_file, kind=c_int))
    if (.not. c_associated(model)) error stop 'could not read the model'
    call check(vkf_node_bound(model, model_center, model_radius))
    write(*, '(a,3es12.4,a,es12.4)') 'model bound: center', model_center, ', radius', model_radius
    scale = model_size / model_radius
    model_transform = vkf_transform_create()
    call check(vkf_node_add_child(model_transform, model))
    call check(vkf_viewer_add_node(viewer, model_transform))
    call vkf_node_delete(model)

    frame = 0
    status = 1
    do while (frame < max_frames)
        call check(vkf_transform_set_matrix(earth_transform, rotation_z(2.0_wp * pi * real(frame, wp) / day)))
        u = 2.0_wp * pi * real(frame, wp) / orbit_period
        ! model: centered on its bound, scaled, then placed on the orbit
        call check(vkf_transform_set_matrix(model_transform, &
            matmul(translation(r_orbit * [cos(u), sin(u) * cos(inc), sin(u) * sin(inc)]), &
                   matmul(scaling(scale), translation(-model_center)))))
        status = vkf_viewer_frame(viewer)
        if (status < 0) error stop 'frame failed'
        if (status == 0) exit
        frame = frame + 1
    end do
    write(*, '(a,i0,a)') 'rendered ', frame, ' frames'

    if (len_trim(capture_file) > 0 .and. status == 1) then
        call check(vkf_viewer_capture(viewer, trim(capture_file), len_trim(capture_file, kind=c_int)))
        write(*, '(a)') 'captured '//trim(capture_file)
    end if

    call vkf_node_delete(model_transform)
    call vkf_node_delete(earth_transform)
    call vkf_image_delete(texture)
    call vkf_viewer_delete(viewer)

contains

    subroutine check(status)
        integer(c_int), intent(in) :: status
        if (status /= 0) error stop 'vkframes layer call failed'
    end subroutine check

    pure function identity() result(m)
        real(wp) :: m(4,4)
        integer :: k
        m = 0.0_wp
        do k = 1, 4
            m(k, k) = 1.0_wp
        end do
    end function identity

    pure function translation(t) result(m)
        real(wp), intent(in) :: t(3)
        real(wp) :: m(4,4)
        m = identity()
        m(1:3, 4) = t
    end function translation

    pure function scaling(s) result(m)
        real(wp), intent(in) :: s
        real(wp) :: m(4,4)
        m = identity()
        m(1, 1) = s
        m(2, 2) = s
        m(3, 3) = s
    end function scaling

    pure function rotation_z(a) result(m)
        real(wp), intent(in) :: a
        real(wp) :: m(4,4)
        m = identity()
        m(1:2, 1:2) = reshape([cos(a), sin(a), -sin(a), cos(a)], [2, 2])
    end function rotation_z

end program earth
