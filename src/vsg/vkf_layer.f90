!> Fortran interfaces to the C++ VSG layer (src/vsg/vkf_layer.cpp).
!>
!> Objects are opaque `type(c_ptr)` handles owned by the layer; release each
!> one with its `*_delete` routine. Strings cross as (pointer, length).
module vkf_layer

    use, intrinsic :: iso_c_binding

    implicit none

    private

    public :: vkf_vulkan_device_name
    public :: vkf_viewer_create, vkf_viewer_delete, vkf_viewer_add_node, vkf_viewer_frame
    public :: vkf_sphere_create, vkf_node_delete
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

        integer(c_int) function vkf_viewer_add_node(viewer, node) bind(C, name="vkf_viewer_add_node")
            import :: c_ptr, c_int
            type(c_ptr), value :: viewer, node
        end function vkf_viewer_add_node

        integer(c_int) function vkf_viewer_frame(viewer) bind(C, name="vkf_viewer_frame")
            import :: c_ptr, c_int
            type(c_ptr), value :: viewer
        end function vkf_viewer_frame

        type(c_ptr) function vkf_sphere_create(radius, rgba) bind(C, name="vkf_sphere_create")
            import :: c_ptr, c_double, c_float
            real(c_double), value :: radius
            real(c_float), dimension(4), intent(in) :: rgba
        end function vkf_sphere_create

        subroutine vkf_node_delete(node) bind(C, name="vkf_node_delete")
            import :: c_ptr
            type(c_ptr), value :: node
        end subroutine vkf_node_delete

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
