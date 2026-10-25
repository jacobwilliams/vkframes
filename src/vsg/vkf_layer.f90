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
    public :: vkf_viewer_get_look_at, vkf_viewer_set_view_transform, vkf_viewer_orthographic
    public :: vkf_viewer_set_background
    public :: vkf_viewer_add_node, vkf_viewer_frame, vkf_viewer_capture
    public :: vkf_viewer_mouse, vkf_viewer_scroll, vkf_viewer_key, vkf_viewer_resize, vkf_viewer_close
    public :: vkf_node_delete, vkf_node_add_child, vkf_node_bound
    public :: vkf_switch_create, vkf_switch_set
    public :: vkf_transform_create, vkf_transform_set_matrix
    public :: vkf_image_read, vkf_image_delete, vkf_model_read
    public :: vkf_mesh_create, vkf_mesh_array
    public :: vkf_font_read, vkf_font_delete, vkf_label_create
    public :: vkf_line_create, vkf_line_points, vkf_line_style, vkf_line_points_changed, vkf_line_style_changed
    public :: vulkan_device_name, line_set_points, line_set_style, mesh_arrays

    interface

        integer(c_int) function vkf_vulkan_device_name(buf, buflen) bind(C, name="vkf_vulkan_device_name")
            import :: c_int, c_char
            implicit none
            character(kind=c_char), dimension(*), intent(out) :: buf
            integer(c_int), value :: buflen
        end function vkf_vulkan_device_name

        type(c_ptr) function vkf_viewer_create(width, height, title, title_len, native_window, pixel_ratio) &
                bind(C, name="vkf_viewer_create")
            import :: c_ptr, c_int, c_char, c_long_long, c_double
            implicit none
            integer(c_int), value :: width, height
            character(kind=c_char), dimension(*), intent(in) :: title
            integer(c_int), value :: title_len
            integer(c_long_long), value :: native_window
            real(c_double), value :: pixel_ratio
        end function vkf_viewer_create

        integer(c_int) function vkf_viewer_mouse(viewer, kind, x, y, mask, button) bind(C, name="vkf_viewer_mouse")
            import :: c_ptr, c_int
            implicit none
            type(c_ptr), value :: viewer
            integer(c_int), value :: kind, x, y, mask, button
        end function vkf_viewer_mouse

        integer(c_int) function vkf_viewer_scroll(viewer, dy) bind(C, name="vkf_viewer_scroll")
            import :: c_ptr, c_int, c_float
            implicit none
            type(c_ptr), value :: viewer
            real(c_float), value :: dy
        end function vkf_viewer_scroll

        integer(c_int) function vkf_viewer_key(viewer, pressed, symbol, modified_symbol, modifier) &
                bind(C, name="vkf_viewer_key")
            import :: c_ptr, c_int
            implicit none
            type(c_ptr), value :: viewer
            integer(c_int), value :: pressed, symbol, modified_symbol, modifier
        end function vkf_viewer_key

        integer(c_int) function vkf_viewer_resize(viewer, width, height) bind(C, name="vkf_viewer_resize")
            import :: c_ptr, c_int
            implicit none
            type(c_ptr), value :: viewer
            integer(c_int), value :: width, height
        end function vkf_viewer_resize

        integer(c_int) function vkf_viewer_close(viewer) bind(C, name="vkf_viewer_close")
            import :: c_ptr, c_int
            implicit none
            type(c_ptr), value :: viewer
        end function vkf_viewer_close

        subroutine vkf_viewer_delete(viewer) bind(C, name="vkf_viewer_delete")
            import :: c_ptr
            implicit none
            type(c_ptr), value :: viewer
        end subroutine vkf_viewer_delete

        integer(c_int) function vkf_viewer_look_at(viewer, eye, center, up) bind(C, name="vkf_viewer_look_at")
            import :: c_ptr, c_int, c_double
            implicit none
            type(c_ptr), value :: viewer
            real(c_double), dimension(3), intent(in) :: eye, center, up
        end function vkf_viewer_look_at

        integer(c_int) function vkf_viewer_set_background(viewer, rgba) bind(C, name="vkf_viewer_set_background")
            import :: c_ptr, c_int, c_float
            implicit none
            type(c_ptr), value :: viewer
            real(c_float), dimension(4), intent(in) :: rgba
        end function vkf_viewer_set_background

        integer(c_int) function vkf_viewer_get_look_at(viewer, eye, center, up) bind(C, name="vkf_viewer_get_look_at")
            import :: c_ptr, c_int, c_double
            implicit none
            type(c_ptr), value :: viewer
            real(c_double), dimension(3), intent(out) :: eye, center, up
        end function vkf_viewer_get_look_at

        integer(c_int) function vkf_viewer_set_view_transform(viewer, m) bind(C, name="vkf_viewer_set_view_transform")
            import :: c_ptr, c_int, c_double
            implicit none
            type(c_ptr), value :: viewer
            real(c_double), dimension(4,4), intent(in) :: m
        end function vkf_viewer_set_view_transform

        integer(c_int) function vkf_viewer_orthographic(viewer, left, right, bottom, top, near_distance, far_distance) &
                bind(C, name="vkf_viewer_orthographic")
            import :: c_ptr, c_int, c_double
            implicit none
            type(c_ptr), value :: viewer
            real(c_double), value :: left, right, bottom, top, near_distance, far_distance
        end function vkf_viewer_orthographic

        integer(c_int) function vkf_viewer_perspective(viewer, fovy, near_distance, far_distance) &
                bind(C, name="vkf_viewer_perspective")
            import :: c_ptr, c_int, c_double
            implicit none
            type(c_ptr), value :: viewer
            real(c_double), value :: fovy, near_distance, far_distance
        end function vkf_viewer_perspective

        integer(c_int) function vkf_viewer_add_node(viewer, node) bind(C, name="vkf_viewer_add_node")
            import :: c_ptr, c_int
            implicit none
            type(c_ptr), value :: viewer, node
        end function vkf_viewer_add_node

        integer(c_int) function vkf_viewer_frame(viewer) bind(C, name="vkf_viewer_frame")
            import :: c_ptr, c_int
            implicit none
            type(c_ptr), value :: viewer
        end function vkf_viewer_frame

        integer(c_int) function vkf_viewer_capture(viewer, filename, filename_len) bind(C, name="vkf_viewer_capture")
            import :: c_ptr, c_int, c_char
            implicit none
            type(c_ptr), value :: viewer
            character(kind=c_char), dimension(*), intent(in) :: filename
            integer(c_int), value :: filename_len
        end function vkf_viewer_capture

        integer(c_int) function vkf_node_add_child(parent, child) bind(C, name="vkf_node_add_child")
            import :: c_ptr, c_int
            implicit none
            type(c_ptr), value :: parent, child
        end function vkf_node_add_child

        integer(c_int) function vkf_node_bound(node, center, radius) bind(C, name="vkf_node_bound")
            import :: c_ptr, c_int, c_double
            implicit none
            type(c_ptr), value :: node
            real(c_double), dimension(3), intent(out) :: center
            real(c_double), intent(out) :: radius
        end function vkf_node_bound

        type(c_ptr) function vkf_switch_create() bind(C, name="vkf_switch_create")
            import :: c_ptr
            implicit none
        end function vkf_switch_create

        integer(c_int) function vkf_switch_set(node, visible) bind(C, name="vkf_switch_set")
            import :: c_ptr, c_int
            implicit none
            type(c_ptr), value :: node
            integer(c_int), value :: visible
        end function vkf_switch_set

        type(c_ptr) function vkf_transform_create() bind(C, name="vkf_transform_create")
            import :: c_ptr
            implicit none
        end function vkf_transform_create

        integer(c_int) function vkf_transform_set_matrix(transform, m) bind(C, name="vkf_transform_set_matrix")
            import :: c_ptr, c_int, c_double
            implicit none
            type(c_ptr), value :: transform
            real(c_double), dimension(4,4), intent(in) :: m
        end function vkf_transform_set_matrix

        subroutine vkf_node_delete(node) bind(C, name="vkf_node_delete")
            import :: c_ptr
            implicit none
            type(c_ptr), value :: node
        end subroutine vkf_node_delete

        type(c_ptr) function vkf_image_read(filename, filename_len) bind(C, name="vkf_image_read")
            import :: c_ptr, c_int, c_char
            implicit none
            character(kind=c_char), dimension(*), intent(in) :: filename
            integer(c_int), value :: filename_len
        end function vkf_image_read

        subroutine vkf_image_delete(image) bind(C, name="vkf_image_delete")
            import :: c_ptr
            implicit none
            type(c_ptr), value :: image
        end subroutine vkf_image_delete

        type(c_ptr) function vkf_model_read(filename, filename_len) bind(C, name="vkf_model_read")
            import :: c_ptr, c_int, c_char
            implicit none
            character(kind=c_char), dimension(*), intent(in) :: filename
            integer(c_int), value :: filename_len
        end function vkf_model_read

        type(c_ptr) function vkf_mesh_create(n_vertices, n_indices, rgba, texture) bind(C, name="vkf_mesh_create")
            import :: c_ptr, c_int, c_float
            implicit none
            integer(c_int), value :: n_vertices, n_indices
            real(c_float), dimension(4), intent(in) :: rgba
            type(c_ptr), value :: texture
        end function vkf_mesh_create

        type(c_ptr) function vkf_mesh_array(mesh, which) bind(C, name="vkf_mesh_array")
            import :: c_ptr, c_int
            implicit none
            type(c_ptr), value :: mesh
            integer(c_int), value :: which
        end function vkf_mesh_array

        type(c_ptr) function vkf_font_read(filename, filename_len) bind(C, name="vkf_font_read")
            import :: c_ptr, c_int, c_char
            implicit none
            character(kind=c_char), dimension(*), intent(in) :: filename
            integer(c_int), value :: filename_len
        end function vkf_font_read

        subroutine vkf_font_delete(font) bind(C, name="vkf_font_delete")
            import :: c_ptr
            implicit none
            type(c_ptr), value :: font
        end subroutine vkf_font_delete

        type(c_ptr) function vkf_label_create(font, text, text_len, rgba, position, size, max_size, halign, valign) &
                bind(C, name="vkf_label_create")
            import :: c_ptr, c_int, c_char, c_float, c_double
            implicit none
            type(c_ptr), value :: font
            character(kind=c_char), dimension(*), intent(in) :: text
            integer(c_int), value :: text_len
            real(c_float), dimension(4), intent(in) :: rgba
            real(c_double), dimension(3), intent(in) :: position
            real(c_float), value :: size, max_size
            integer(c_int), value :: halign, valign
        end function vkf_label_create

        type(c_ptr) function vkf_line_create(capacity) bind(C, name="vkf_line_create")
            import :: c_ptr, c_int
            implicit none
            integer(c_int), value :: capacity
        end function vkf_line_create

        type(c_ptr) function vkf_line_points(line, capacity) bind(C, name="vkf_line_points")
            import :: c_ptr, c_int
            implicit none
            type(c_ptr), value :: line
            integer(c_int), intent(out) :: capacity
        end function vkf_line_points

        type(c_ptr) function vkf_line_style(line) bind(C, name="vkf_line_style")
            import :: c_ptr
            implicit none
            type(c_ptr), value :: line
        end function vkf_line_style

        integer(c_int) function vkf_line_points_changed(line, n) bind(C, name="vkf_line_points_changed")
            import :: c_ptr, c_int
            implicit none
            type(c_ptr), value :: line
            integer(c_int), value :: n
        end function vkf_line_points_changed

        integer(c_int) function vkf_line_style_changed(line) bind(C, name="vkf_line_style_changed")
            import :: c_ptr, c_int
            implicit none
            type(c_ptr), value :: line
        end function vkf_line_style_changed

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

    !> Set the points of a line strip (`xyz(3, n)`, double precision), splitting
    !> each coordinate into the (high, low) float pair the line shader reads, in
    !> place in the strip's buffer. Returns 0, or -1 if `n` exceeds the capacity.
    integer(c_int) function line_set_points(line, xyz) result(status)
        type(c_ptr), intent(in) :: line
        real(c_double), dimension(:,:), intent(in) :: xyz
        real(c_float), dimension(:,:,:), pointer :: p  !! (xyz, high/low, point)
        type(c_ptr) :: buffer
        integer(c_int) :: capacity, n
        integer :: i
        n = int(size(xyz, 2), c_int)
        buffer = vkf_line_points(line, capacity)
        if (.not. c_associated(buffer) .or. n > capacity) then
            status = -1
            return
        end if
        call c_f_pointer(buffer, p, [3, 2, int(capacity)])
        do i = 1, n
            p(:, 1, i) = real(xyz(:, i), c_float)
            p(:, 2, i) = real(xyz(:, i) - real(p(:, 1, i), c_double), c_float)
        end do
        status = vkf_line_points_changed(line, n)
    end function line_set_points

    !> Set the color (`rgba`) and width (window units: logical pixels) of a line strip.
    integer(c_int) function line_set_style(line, rgba, width) result(status)
        type(c_ptr), intent(in) :: line
        real(c_float), dimension(4), intent(in) :: rgba
        real(c_float), intent(in) :: width
        real(c_float), dimension(:), pointer :: style
        type(c_ptr) :: buffer
        buffer = vkf_line_style(line)
        if (.not. c_associated(buffer)) then
            status = -1
            return
        end if
        call c_f_pointer(buffer, style, [8])
        style(1:4) = rgba
        style(5) = width
        status = vkf_line_style_changed(line)
    end function line_set_style

    !> Pointers to the arrays of a mesh of `n_vertices` vertices and `n_indices`
    !> indices (as created by `vkf_mesh_create`), to fill in place before the
    !> first frame. Indices are 0-based.
    subroutine mesh_arrays(mesh, n_vertices, n_indices, xyz, normals, uv, indices)
        type(c_ptr), intent(in) :: mesh
        integer, intent(in) :: n_vertices, n_indices
        real(c_float), dimension(:,:), pointer, intent(out) :: xyz      !! (3, n_vertices)
        real(c_float), dimension(:,:), pointer, intent(out) :: normals  !! (3, n_vertices)
        real(c_float), dimension(:,:), pointer, intent(out) :: uv       !! (2, n_vertices)
        integer(c_int32_t), dimension(:), pointer, intent(out) :: indices
        call c_f_pointer(vkf_mesh_array(mesh, 1_c_int), xyz, [3, n_vertices])
        call c_f_pointer(vkf_mesh_array(mesh, 2_c_int), normals, [3, n_vertices])
        call c_f_pointer(vkf_mesh_array(mesh, 3_c_int), uv, [2, n_vertices])
        call c_f_pointer(vkf_mesh_array(mesh, 4_c_int), indices, [n_indices])
    end subroutine mesh_arrays

end module vkf_layer
