!> Fortran interfaces to the C++ VSG layer (src/vsg/vkf_layer.cpp).
!>
!> Objects are opaque `type(c_ptr)` handles owned by the layer; release each
!> one with its `*_delete` routine. Strings cross as (pointer, length).
module vkf_layer

    use, intrinsic :: iso_c_binding

    implicit none

    private

    public :: vkf_vulkan_device_name
    public :: vkf_viewer_create, vkf_viewer_delete, vkf_viewer_look_at, vkf_viewer_perspective
    public :: vkf_viewer_add_node, vkf_viewer_frame, vkf_viewer_capture
    public :: vkf_node_delete, vkf_node_add_child
    public :: vkf_transform_create, vkf_transform_set_matrix
    public :: vkf_sphere_create
    public :: vkf_line_create, vkf_line_set_points, vkf_line_set_style
    public :: vulkan_device_name

    interface

        integer(c_int) function vkf_vulkan_device_name(buf, buflen) bind(C, name="vkf_vulkan_device_name")
            import :: c_int, c_char
            character(kind=c_char), dimension(*), intent(out) :: buf
            integer(c_int), value :: buflen
        end function vkf_vulkan_device_name

        type(c_ptr) function vkf_viewer_create(width, height, title, title_len) bind(C, name="vkf_viewer_create")
            import :: c_ptr, c_int, c_char
            integer(c_int), value :: width, height
            character(kind=c_char), dimension(*), intent(in) :: title
            integer(c_int), value :: title_len
        end function vkf_viewer_create

        subroutine vkf_viewer_delete(viewer) bind(C, name="vkf_viewer_delete")
            import :: c_ptr
            type(c_ptr), value :: viewer
        end subroutine vkf_viewer_delete

        integer(c_int) function vkf_viewer_look_at(viewer, eye, center, up) bind(C, name="vkf_viewer_look_at")
            import :: c_ptr, c_int, c_double
            type(c_ptr), value :: viewer
            real(c_double), dimension(3), intent(in) :: eye, center, up
        end function vkf_viewer_look_at

        integer(c_int) function vkf_viewer_perspective(viewer, fovy, near_distance, far_distance) &
                bind(C, name="vkf_viewer_perspective")
            import :: c_ptr, c_int, c_double
            type(c_ptr), value :: viewer
            real(c_double), value :: fovy, near_distance, far_distance
        end function vkf_viewer_perspective

        integer(c_int) function vkf_viewer_add_node(viewer, node) bind(C, name="vkf_viewer_add_node")
            import :: c_ptr, c_int
            type(c_ptr), value :: viewer, node
        end function vkf_viewer_add_node

        integer(c_int) function vkf_viewer_frame(viewer) bind(C, name="vkf_viewer_frame")
            import :: c_ptr, c_int
            type(c_ptr), value :: viewer
        end function vkf_viewer_frame

        integer(c_int) function vkf_viewer_capture(viewer, filename, filename_len) bind(C, name="vkf_viewer_capture")
            import :: c_ptr, c_int, c_char
            type(c_ptr), value :: viewer
            character(kind=c_char), dimension(*), intent(in) :: filename
            integer(c_int), value :: filename_len
        end function vkf_viewer_capture

        integer(c_int) function vkf_node_add_child(parent, child) bind(C, name="vkf_node_add_child")
            import :: c_ptr, c_int
            type(c_ptr), value :: parent, child
        end function vkf_node_add_child

        type(c_ptr) function vkf_transform_create() bind(C, name="vkf_transform_create")
            import :: c_ptr
        end function vkf_transform_create

        integer(c_int) function vkf_transform_set_matrix(transform, m) bind(C, name="vkf_transform_set_matrix")
            import :: c_ptr, c_int, c_double
            type(c_ptr), value :: transform
            real(c_double), dimension(4,4), intent(in) :: m
        end function vkf_transform_set_matrix

        type(c_ptr) function vkf_sphere_create(radius, rgba) bind(C, name="vkf_sphere_create")
            import :: c_ptr, c_double, c_float
            real(c_double), value :: radius
            real(c_float), dimension(4), intent(in) :: rgba
        end function vkf_sphere_create

        subroutine vkf_node_delete(node) bind(C, name="vkf_node_delete")
            import :: c_ptr
            type(c_ptr), value :: node
        end subroutine vkf_node_delete

        type(c_ptr) function vkf_line_create(viewer, capacity, rgba, width) bind(C, name="vkf_line_create")
            import :: c_ptr, c_int, c_float
            type(c_ptr), value :: viewer
            integer(c_int), value :: capacity
            real(c_float), dimension(4), intent(in) :: rgba
            real(c_float), value :: width
        end function vkf_line_create

        integer(c_int) function vkf_line_set_points(line, n, xyz) bind(C, name="vkf_line_set_points")
            import :: c_ptr, c_int, c_double
            type(c_ptr), value :: line
            integer(c_int), value :: n
            real(c_double), dimension(3,*), intent(in) :: xyz
        end function vkf_line_set_points

        integer(c_int) function vkf_line_set_style(line, rgba, width) bind(C, name="vkf_line_set_style")
            import :: c_ptr, c_int, c_float
            type(c_ptr), value :: line
            real(c_float), dimension(4), intent(in) :: rgba
            real(c_float), value :: width
        end function vkf_line_set_style

    end interface

contains

    !> Name of the first Vulkan physical device, or an empty string if there is none.
    function vulkan_device_name() result(name)
        character(len=:), allocatable :: name
        character(kind=c_char, len=256) :: buf
        integer(c_int) :: n
        n = vkf_vulkan_device_name(buf, len(buf, kind=c_int))
        if (n < 0) then
            name = ''
        else
            name = buf(1:min(n, len(buf)))
        end if
    end function vulkan_device_name

end module vkf_layer
