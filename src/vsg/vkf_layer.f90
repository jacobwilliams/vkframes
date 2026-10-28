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
    public :: vkf_viewer_set_pixel_ratio
    public :: vkf_viewer_get_look_at, vkf_viewer_set_view_transform, vkf_viewer_orthographic
    public :: vkf_viewer_set_background
    public :: vkf_viewer_add_node, vkf_viewer_frame, vkf_viewer_capture
    public :: vkf_viewer_mouse, vkf_viewer_scroll, vkf_viewer_key, vkf_viewer_resize, vkf_viewer_close
    public :: vkf_viewer_events, vkf_viewer_size, vkf_viewer_pixel_ratio, vkf_viewer_frame_begin, vkf_viewer_frame_draw
    public :: vkf_viewer_gui, vkf_gui_begin, vkf_gui_end, vkf_gui_text, vkf_gui_button, vkf_gui_checkbox, vkf_gui_slider, &
              vkf_gui_separator, vkf_gui_same_line
    public :: vkf_node_delete, vkf_node_add_child, vkf_node_remove_child, vkf_node_bound, vkf_compile_node, vkf_viewer_any
    public :: vkf_switch_create, vkf_switch_set
    public :: vkf_transform_create, vkf_transform_set_matrix
    public :: vkf_image_read, vkf_image_delete, vkf_model_read
    public :: vkf_mesh_create, vkf_mesh_array, vkf_mesh_material, vkf_mesh_material_changed, vkf_mesh_vertices_changed, &
              vkf_mesh_set_twilight
    public :: vkf_light_create, vkf_light_set, vkf_viewer_light, vkf_viewer_light_enable
    public :: vkf_font_read, vkf_font_delete, vkf_label_create, vkf_label_set_text, vkf_label_set_color, vkf_label_set_layout, vkf_label_set_on_top
    public :: vkf_viewer_hud_add, vkf_viewer_hud_remove, vkf_viewer_remove_node
    public :: vkf_line_create, vkf_line_points, vkf_line_style, vkf_line_points_changed, vkf_line_style_changed
    public :: vkf_line_set_range, vkf_line_set_lod, vkf_marker_create, vkf_line_create_colored, vkf_line_colors
    public :: vkf_ribbon_create
    public :: vkf_stars_create, vkf_sky_texture_create, vkf_sky_stars, vkf_sky_stars_changed
    public :: vkf_sky_style, vkf_sky_style_changed
    public :: vulkan_device_name, line_set_points, line_set_style, line_set_pattern, line_set_segments, marker_set_style
    public :: mesh_arrays
    public :: vkf_rocky_available, vkf_rocky_map_create, vkf_rocky_map_settings, vkf_rocky_map_radius, vkf_rocky_map_alive
    public :: vkf_rocky_add_image, vkf_rocky_add_elevation, vkf_rocky_elevation
    public :: mesh_set_material
    public :: vkf_sphere_lod_create, vkf_sphere_lod_add, vkf_sphere_lod_requested

    interface

        integer(c_int) function vkf_vulkan_device_name(buf, buflen) bind(C, name="vkf_vulkan_device_name")
            import :: c_int, c_char
            implicit none
            character(kind=c_char), dimension(*), intent(out) :: buf
            integer(c_int), value :: buflen
        end function vkf_vulkan_device_name

        !> A host's window now has `ratio` framebuffer pixels per window unit.
        integer(c_int) function vkf_viewer_set_pixel_ratio(viewer, ratio) bind(C, name="vkf_viewer_set_pixel_ratio")
            import :: c_ptr, c_int, c_double
            implicit none
            type(c_ptr), value :: viewer
            real(c_double), value :: ratio
        end function vkf_viewer_set_pixel_ratio

        type(c_ptr) function vkf_viewer_create(width, height, title, title_len, native_window, pixel_ratio, vsync) &
                bind(C, name="vkf_viewer_create")
            import :: c_ptr, c_int, c_char, c_long_long, c_double
            implicit none
            integer(c_int), value :: width, height
            character(kind=c_char), dimension(*), intent(in) :: title
            integer(c_int), value :: title_len
            integer(c_long_long), value :: native_window
            real(c_double), value :: pixel_ratio
            integer(c_int), value :: vsync
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

        integer(c_int) function vkf_viewer_events(viewer, events, capacity) bind(C, name="vkf_viewer_events")
            import :: c_ptr, c_int
            implicit none
            type(c_ptr), value :: viewer
            integer(c_int), intent(out) :: events(12, *)
            integer(c_int), value :: capacity
        end function vkf_viewer_events

        real(c_double) function vkf_viewer_pixel_ratio(viewer) bind(C, name="vkf_viewer_pixel_ratio")
            import :: c_ptr, c_double
            implicit none
            type(c_ptr), value :: viewer
        end function vkf_viewer_pixel_ratio

        integer(c_int) function vkf_viewer_size(viewer, width, height) bind(C, name="vkf_viewer_size")
            import :: c_ptr, c_int
            implicit none
            type(c_ptr), value :: viewer
            integer(c_int), intent(out) :: width, height
        end function vkf_viewer_size

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

        ! Rocky maps (src/vsg/vkf_rocky.cpp; optional: vkf_rocky_available() is 0 without it)

        integer(c_int) function vkf_rocky_available() bind(C, name="vkf_rocky_available")
            import :: c_int
            implicit none
        end function vkf_rocky_available

        type(c_ptr) function vkf_rocky_map_create(viewer, profile, profile_len, scale) bind(C, name="vkf_rocky_map_create")
            import :: c_ptr, c_int, c_char, c_double
            implicit none
            type(c_ptr), value :: viewer
            character(kind=c_char), dimension(*), intent(in) :: profile
            integer(c_int), value :: profile_len
            real(c_double), value :: scale
        end function vkf_rocky_map_create

        integer(c_int) function vkf_rocky_map_settings(map, lighting, background) bind(C, name="vkf_rocky_map_settings")
            import :: c_ptr, c_int, c_float
            implicit none
            type(c_ptr), value :: map
            integer(c_int), value :: lighting
            real(c_float), dimension(4), intent(in) :: background
        end function vkf_rocky_map_settings

        !> 1 while a map can be drawn (the viewer it was made for exists), else 0.
        integer(c_int) function vkf_rocky_map_alive(map) bind(C, name="vkf_rocky_map_alive")
            import :: c_ptr, c_int
            implicit none
            type(c_ptr), value :: map
        end function vkf_rocky_map_alive

        real(c_double) function vkf_rocky_map_radius(map) bind(C, name="vkf_rocky_map_radius")
            import :: c_ptr, c_double
            implicit none
            type(c_ptr), value :: map
        end function vkf_rocky_map_radius

        real(c_double) function vkf_rocky_elevation(uri, uri_len, profile, profile_len, lat, lon, ok) &
                bind(C, name="vkf_rocky_elevation")
            import :: c_int, c_char, c_double
            implicit none
            character(kind=c_char), dimension(*), intent(in) :: uri, profile
            integer(c_int), value :: uri_len, profile_len
            real(c_double), value :: lat, lon
            integer(c_int), intent(out) :: ok
        end function vkf_rocky_elevation

        integer(c_int) function vkf_rocky_add_image(map, kind, uri, uri_len) bind(C, name="vkf_rocky_add_image")
            import :: c_ptr, c_int, c_char
            implicit none
            type(c_ptr), value :: map
            integer(c_int), value :: kind, uri_len
            character(kind=c_char), dimension(*), intent(in) :: uri
        end function vkf_rocky_add_image

        integer(c_int) function vkf_rocky_add_elevation(map, kind, uri, uri_len, encoding) &
                bind(C, name="vkf_rocky_add_elevation")
            import :: c_ptr, c_int, c_char
            implicit none
            type(c_ptr), value :: map
            integer(c_int), value :: kind, uri_len, encoding
            character(kind=c_char), dimension(*), intent(in) :: uri
        end function vkf_rocky_add_elevation

        type(c_ptr) function vkf_viewer_gui(viewer, font, font_len, size) bind(C, name="vkf_viewer_gui")
            import :: c_ptr, c_int, c_char, c_double
            implicit none
            type(c_ptr), value :: viewer
            character(kind=c_char), intent(in) :: font(*)
            integer(c_int), value :: font_len
            real(c_double), value :: size
        end function vkf_viewer_gui

        subroutine vkf_gui_begin(gui, title, title_len, x, y, pivot_x, pivot_y, follow) bind(C, name="vkf_gui_begin")
            import :: c_ptr, c_int, c_char, c_double
            implicit none
            type(c_ptr), value :: gui
            character(kind=c_char), intent(in) :: title(*)
            integer(c_int), value :: title_len, follow
            real(c_double), value :: x, y, pivot_x, pivot_y
        end subroutine vkf_gui_begin

        subroutine vkf_gui_end(gui) bind(C, name="vkf_gui_end")
            import :: c_ptr
            implicit none
            type(c_ptr), value :: gui
        end subroutine vkf_gui_end

        subroutine vkf_gui_text(gui, text, text_len) bind(C, name="vkf_gui_text")
            import :: c_ptr, c_int, c_char
            implicit none
            type(c_ptr), value :: gui
            character(kind=c_char), intent(in) :: text(*)
            integer(c_int), value :: text_len
        end subroutine vkf_gui_text

        integer(c_int) function vkf_gui_button(gui, label, label_len) bind(C, name="vkf_gui_button")
            import :: c_ptr, c_int, c_char
            implicit none
            type(c_ptr), value :: gui
            character(kind=c_char), intent(in) :: label(*)
            integer(c_int), value :: label_len
        end function vkf_gui_button

        integer(c_int) function vkf_gui_checkbox(gui, label, label_len, value) bind(C, name="vkf_gui_checkbox")
            import :: c_ptr, c_int, c_char
            implicit none
            type(c_ptr), value :: gui
            character(kind=c_char), intent(in) :: label(*)
            integer(c_int), value :: label_len
            integer(c_int), intent(inout) :: value
        end function vkf_gui_checkbox

        integer(c_int) function vkf_gui_slider(gui, label, label_len, value, low, high) bind(C, name="vkf_gui_slider")
            import :: c_ptr, c_int, c_char, c_double
            implicit none
            type(c_ptr), value :: gui
            character(kind=c_char), intent(in) :: label(*)
            integer(c_int), value :: label_len
            real(c_double), intent(inout) :: value
            real(c_double), value :: low, high
        end function vkf_gui_slider

        subroutine vkf_gui_separator(gui) bind(C, name="vkf_gui_separator")
            import :: c_ptr
            implicit none
            type(c_ptr), value :: gui
        end subroutine vkf_gui_separator

        subroutine vkf_gui_same_line(gui) bind(C, name="vkf_gui_same_line")
            import :: c_ptr
            implicit none
            type(c_ptr), value :: gui
        end subroutine vkf_gui_same_line

        integer(c_int) function vkf_viewer_frame_begin(viewer) bind(C, name="vkf_viewer_frame_begin")
            import :: c_ptr, c_int
            implicit none
            type(c_ptr), value :: viewer
        end function vkf_viewer_frame_begin

        integer(c_int) function vkf_viewer_frame_draw(viewer) bind(C, name="vkf_viewer_frame_draw")
            import :: c_ptr, c_int
            implicit none
            type(c_ptr), value :: viewer
        end function vkf_viewer_frame_draw

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

        integer(c_int) function vkf_node_remove_child(parent, child) bind(C, name="vkf_node_remove_child")
            import :: c_ptr, c_int
            implicit none
            type(c_ptr), value :: parent, child
        end function vkf_node_remove_child

        !> Compile a node for the running viewers (to add it to a shown scene).
        integer(c_int) function vkf_compile_node(node) bind(C, name="vkf_compile_node")
            import :: c_ptr, c_int
            implicit none
            type(c_ptr), value :: node
        end function vkf_compile_node

        !> A viewer that exists (the oldest running one), or null.
        type(c_ptr) function vkf_viewer_any() bind(C, name="vkf_viewer_any")
            import :: c_ptr
            implicit none
        end function vkf_viewer_any

        !> A sphere's level of detail for a body of `radius` (its largest semi-axis).
        type(c_ptr) function vkf_sphere_lod_create(radius) bind(C, name="vkf_sphere_lod_create")
            import :: c_ptr, c_double
            implicit none
            real(c_double), value :: radius
        end function vkf_sphere_lod_create

        !> Add a mesh of `segments` segments around the equator.
        integer(c_int) function vkf_sphere_lod_add(lod, mesh, segments) bind(C, name="vkf_sphere_lod_add")
            import :: c_ptr, c_int
            implicit none
            type(c_ptr), value :: lod, mesh
            integer(c_int), value :: segments
        end function vkf_sphere_lod_add

        !> The most segments any view wanted since the last call (0 if not drawn).
        integer(c_int) function vkf_sphere_lod_requested(lod) bind(C, name="vkf_sphere_lod_requested")
            import :: c_ptr, c_int
            implicit none
            type(c_ptr), value :: lod
        end function vkf_sphere_lod_requested

        integer(c_int) function vkf_line_set_range(line, first, n) bind(C, name="vkf_line_set_range")
            import :: c_ptr, c_int
            implicit none
            type(c_ptr), value :: line
            integer(c_int), value :: first, n
        end function vkf_line_set_range

        !> A line strip's level of detail: the bounding sphere of its points and
        !> error(k): how far the points skipped can be from the segments drawn at
        !> stride 2^k (k = 1 .. levels).
        integer(c_int) function vkf_line_set_lod(line, center, radius, error, levels) bind(C, name="vkf_line_set_lod")
            import :: c_ptr, c_int, c_double, c_float
            implicit none
            type(c_ptr), value :: line
            real(c_double), dimension(3), intent(in) :: center
            real(c_double), value :: radius
            real(c_float), dimension(*), intent(in) :: error
            integer(c_int), value :: levels
        end function vkf_line_set_lod

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

        type(c_ptr) function vkf_mesh_create(n_vertices, n_indices, rgba, texture, night_texture, flags) &
                bind(C, name="vkf_mesh_create")
            import :: c_ptr, c_int, c_float
            implicit none
            integer(c_int), value :: n_vertices, n_indices, flags
            real(c_float), dimension(4), intent(in) :: rgba
            type(c_ptr), value :: texture, night_texture
        end function vkf_mesh_create

        type(c_ptr) function vkf_mesh_material(mesh) bind(C, name="vkf_mesh_material")
            import :: c_ptr
            implicit none
            type(c_ptr), value :: mesh
        end function vkf_mesh_material

        integer(c_int) function vkf_mesh_material_changed(mesh) bind(C, name="vkf_mesh_material_changed")
            import :: c_ptr, c_int
            implicit none
            type(c_ptr), value :: mesh
        end function vkf_mesh_material_changed

        !> Upload a mesh's vertices again after rewriting them in place (a mesh
        !> created with flag 4).
        integer(c_int) function vkf_mesh_vertices_changed(mesh) bind(C, name="vkf_mesh_vertices_changed")
            import :: c_ptr, c_int
            implicit none
            type(c_ptr), value :: mesh
        end function vkf_mesh_vertices_changed

        !> A lit mesh's twilight: its diffuse light extended to `angle` (radians)
        !> past the terminator, fading out (0: none).
        integer(c_int) function vkf_mesh_set_twilight(mesh, angle) bind(C, name="vkf_mesh_set_twilight")
            import :: c_ptr, c_int, c_float
            implicit none
            type(c_ptr), value :: mesh
            real(c_float), value :: angle
        end function vkf_mesh_set_twilight

        integer(c_int) function vkf_viewer_remove_node(viewer, node) bind(C, name="vkf_viewer_remove_node")
            import :: c_ptr, c_int
            implicit none
            type(c_ptr), value :: viewer, node
        end function vkf_viewer_remove_node

        integer(c_int) function vkf_viewer_hud_remove(viewer, label) bind(C, name="vkf_viewer_hud_remove")
            import :: c_ptr, c_int
            implicit none
            type(c_ptr), value :: viewer, label
        end function vkf_viewer_hud_remove

        type(c_ptr) function vkf_light_create() bind(C, name="vkf_light_create")
            import :: c_ptr
            implicit none
        end function vkf_light_create

        integer(c_int) function vkf_light_set(light, ambient, diffuse, specular, position) bind(C, name="vkf_light_set")
            import :: c_ptr, c_int, c_float, c_double
            implicit none
            type(c_ptr), value :: light
            real(c_float), dimension(3), intent(in) :: ambient, diffuse, specular
            real(c_double), dimension(4), intent(in) :: position
        end function vkf_light_set

        type(c_ptr) function vkf_viewer_light(viewer) bind(C, name="vkf_viewer_light")
            import :: c_ptr
            implicit none
            type(c_ptr), value :: viewer
        end function vkf_viewer_light

        integer(c_int) function vkf_viewer_light_enable(viewer, enabled) bind(C, name="vkf_viewer_light_enable")
            import :: c_ptr, c_int
            implicit none
            type(c_ptr), value :: viewer
            integer(c_int), value :: enabled
        end function vkf_viewer_light_enable

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

        integer(c_int) function vkf_label_set_text(label, text, text_len) bind(C, name="vkf_label_set_text")
            import :: c_ptr, c_int, c_char
            implicit none
            type(c_ptr), value :: label
            character(kind=c_char), dimension(*), intent(in) :: text
            integer(c_int), value :: text_len
        end function vkf_label_set_text

        integer(c_int) function vkf_label_set_on_top(label, on) bind(C, name="vkf_label_set_on_top")
            import :: c_ptr, c_int
            implicit none
            type(c_ptr), value :: label
            integer(c_int), value :: on
        end function vkf_label_set_on_top

        integer(c_int) function vkf_label_set_layout(label, position, size, max_size, halign, valign) &
                bind(C, name="vkf_label_set_layout")
            import :: c_ptr, c_int, c_double, c_float
            implicit none
            type(c_ptr), value :: label
            real(c_double), intent(in) :: position(3)
            real(c_float), value :: size, max_size
            integer(c_int), value :: halign, valign
        end function vkf_label_set_layout

        integer(c_int) function vkf_label_set_color(label, rgba) bind(C, name="vkf_label_set_color")
            import :: c_ptr, c_int, c_float
            implicit none
            type(c_ptr), value :: label
            real(c_float), dimension(4), intent(in) :: rgba
        end function vkf_label_set_color

        integer(c_int) function vkf_viewer_hud_add(viewer, label) bind(C, name="vkf_viewer_hud_add")
            import :: c_ptr, c_int
            implicit none
            type(c_ptr), value :: viewer, label
        end function vkf_viewer_hud_add

        type(c_ptr) function vkf_line_create(capacity) bind(C, name="vkf_line_create")
            import :: c_ptr, c_int
            implicit none
            integer(c_int), value :: capacity
        end function vkf_line_create

        type(c_ptr) function vkf_stars_create(capacity) bind(C, name="vkf_stars_create")
            import :: c_ptr, c_int
            implicit none
            integer(c_int), value :: capacity
        end function vkf_stars_create

        type(c_ptr) function vkf_sky_texture_create(image) bind(C, name="vkf_sky_texture_create")
            import :: c_ptr
            implicit none
            type(c_ptr), value :: image
        end function vkf_sky_texture_create

        type(c_ptr) function vkf_sky_stars(node, capacity) bind(C, name="vkf_sky_stars")
            import :: c_ptr, c_int
            implicit none
            type(c_ptr), value :: node
            integer(c_int), intent(out) :: capacity
        end function vkf_sky_stars

        integer(c_int) function vkf_sky_stars_changed(node, n) bind(C, name="vkf_sky_stars_changed")
            import :: c_ptr, c_int
            implicit none
            type(c_ptr), value :: node
            integer(c_int), value :: n
        end function vkf_sky_stars_changed

        type(c_ptr) function vkf_sky_style(node) bind(C, name="vkf_sky_style")
            import :: c_ptr
            implicit none
            type(c_ptr), value :: node
        end function vkf_sky_style

        integer(c_int) function vkf_sky_style_changed(node) bind(C, name="vkf_sky_style_changed")
            import :: c_ptr, c_int
            implicit none
            type(c_ptr), value :: node
        end function vkf_sky_style_changed

        type(c_ptr) function vkf_marker_create(capacity, image) bind(C, name="vkf_marker_create")
            import :: c_ptr, c_int
            implicit none
            integer(c_int), value :: capacity
            type(c_ptr), value :: image
        end function vkf_marker_create

        !> A line with a color per point (vkf_line_colors), blended along each segment.
        type(c_ptr) function vkf_line_create_colored(capacity) bind(C, name="vkf_line_create_colored")
            import :: c_ptr, c_int
            implicit none
            integer(c_int), value :: capacity
        end function vkf_line_create_colored

        !> A ribbon of up to `capacity` points (even): a translucent surface through
        !> pairs of points, a quad between each pair and the next; a color per
        !> point (vkf_line_colors) if `colored` is not 0. The buffers are a line's.
        type(c_ptr) function vkf_ribbon_create(capacity, colored) bind(C, name="vkf_ribbon_create")
            import :: c_ptr, c_int
            implicit none
            integer(c_int), value :: capacity, colored
        end function vkf_ribbon_create

        !> The colors (4 floats per point) of a line made by vkf_line_create_colored (null for others).
        type(c_ptr) function vkf_line_colors(line, capacity) bind(C, name="vkf_line_colors")
            import :: c_ptr, c_int
            implicit none
            type(c_ptr), value :: line
            integer(c_int), intent(out) :: capacity
        end function vkf_line_colors

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
    !> each coordinate, and the distance along the strip, into the (high, low)
    !> float pair the line shader reads, in place in the strip's buffer.
    !> Returns 0, or -1 if `n` exceeds the capacity.
    integer(c_int) function line_set_points(line, xyz) result(status)
        type(c_ptr), intent(in) :: line
        real(c_double), dimension(:,:), intent(in) :: xyz
        real(c_float), dimension(:,:,:), pointer :: p  !! (xyz and distance, high/low, point)
        type(c_ptr) :: buffer
        integer(c_int) :: capacity, n
        real(c_double) :: s                            !! distance along the strip
        integer :: i
        n = int(size(xyz, 2), c_int)
        buffer = vkf_line_points(line, capacity)
        if (.not. c_associated(buffer) .or. n > capacity) then
            status = -1
            return
        end if
        call c_f_pointer(buffer, p, [4, 2, int(capacity)])
        s = 0.0_c_double
        do i = 1, n
            if (i > 1) s = s + norm2(xyz(:, i) - xyz(:, i - 1))
            p(1:3, 1, i) = real(xyz(:, i), c_float)
            p(1:3, 2, i) = real(xyz(:, i) - real(p(1:3, 1, i), c_double), c_float)
            p(4, 1, i) = real(s, c_float)
            p(4, 2, i) = real(s - real(p(4, 1, i), c_double), c_float)
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

    !> Set a line's dash pattern, as OpenGL's line stipple: each bit of the
    !> 16-bit `pattern` (bit 0 first; z'FFFF' solid) covers `factor` window
    !> units (see shaders/line.vert).
    integer(c_int) function line_set_pattern(line, factor, pattern) result(status)
        type(c_ptr), intent(in) :: line
        integer, intent(in) :: factor, pattern
        real(c_float), dimension(:), pointer :: style
        type(c_ptr) :: buffer
        buffer = vkf_line_style(line)
        if (.not. c_associated(buffer)) then
            status = -1
            return
        end if
        call c_f_pointer(buffer, style, [8])
        style(7) = real(iand(pattern, int(z'FFFF')), c_float)
        style(8) = real(max(factor, 1), c_float)
        status = vkf_line_style_changed(line)
    end function line_set_pattern

    !> Draw a line's points as separate segments (pairs of points: 1-2, 3-4,
    !> ...) instead of a strip. Set before its points are drawn.
    integer(c_int) function line_set_segments(line, segments) result(status)
        type(c_ptr), intent(in) :: line
        logical, intent(in) :: segments
        real(c_float), dimension(:), pointer :: style
        type(c_ptr) :: buffer
        buffer = vkf_line_style(line)
        if (.not. c_associated(buffer)) then
            status = -1
            return
        end if
        call c_f_pointer(buffer, style, [8])
        style(6) = merge(1.0_c_float, 0.0_c_float, segments)
        status = vkf_line_style_changed(line)
    end function line_set_segments

    !> Set the color (`rgba`), size (window units), shape (see shaders/
    !> marker.frag) and attenuation radius (scene units; 0 for none) of markers.
    integer(c_int) function marker_set_style(marker, rgba, size, shape, radius) result(status)
        type(c_ptr), intent(in) :: marker
        real(c_float), dimension(4), intent(in) :: rgba
        real(c_float), intent(in) :: size, radius
        integer, intent(in) :: shape
        real(c_float), dimension(:), pointer :: style
        type(c_ptr) :: buffer
        buffer = vkf_line_style(marker)
        if (.not. c_associated(buffer)) then
            status = -1
            return
        end if
        call c_f_pointer(buffer, style, [8])
        style(1:4) = rgba
        style(5:7) = [size, real(shape, c_float), radius]
        status = vkf_line_style_changed(marker)
    end function marker_set_style

    !> Set a lit mesh's material: ambient, diffuse, specular and emissive
    !> colors (rgba) and shininess (VSG's Phong: the ambient color scales the
    !> diffuse color reflected from ambient light).
    integer(c_int) function mesh_set_material(mesh, ambient, diffuse, specular, emissive, shininess) result(status)
        type(c_ptr), intent(in) :: mesh
        real(c_float), dimension(4), intent(in) :: ambient, diffuse, specular, emissive
        real(c_float), intent(in) :: shininess
        real(c_float), dimension(:), pointer :: m
        type(c_ptr) :: buffer
        buffer = vkf_mesh_material(mesh)
        if (.not. c_associated(buffer)) then
            status = -1
            return
        end if
        call c_f_pointer(buffer, m, [19])
        m(1:4) = ambient
        m(5:8) = diffuse
        m(9:12) = specular
        m(13:16) = emissive
        m(17) = shininess
        status = vkf_mesh_material_changed(mesh)
    end function mesh_set_material

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
