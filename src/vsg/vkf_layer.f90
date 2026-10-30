!> Fortran interfaces to the C++ VSG layer (src/vsg/vkf_layer.cpp, with
!> src/vsg/vkf_rocky.cpp for Rocky maps and src/vsg/vkf_gui.cpp for GUI panels),
!> and a few helpers that fill the layer's buffers in place.
!>
!> Objects are opaque `type(c_ptr)` handles owned by the layer; release each
!> one with its `*_delete` routine. Strings cross as (pointer, length).
!> Functions returning `integer(c_int)` return 0 on success and -1 on failure
!> unless documented otherwise; those returning a handle return null on failure.
module vkf_layer

    use, intrinsic :: iso_c_binding

    implicit none

    private

    public :: vkf_vulkan_device_name, vkf_driver_preload
    public :: vkf_viewer_create, vkf_viewer_delete, vkf_viewer_look_at, vkf_viewer_perspective
    public :: vkf_viewer_set_pixel_ratio
    public :: vkf_viewer_get_look_at, vkf_viewer_set_view_transform, vkf_viewer_orthographic
    public :: vkf_viewer_set_background
    public :: vkf_viewer_add_node, vkf_viewer_frame, vkf_viewer_capture
    public :: vkf_viewer_record_start, vkf_viewer_record_stop, vkf_viewer_record_frames
    public :: vkf_viewer_set_camera_controls
    public :: vkf_viewer_mouse, vkf_viewer_scroll, vkf_viewer_key, vkf_viewer_resize, vkf_viewer_close
    public :: vkf_viewer_events, vkf_viewer_size, vkf_viewer_pixel_ratio
    public :: vkf_viewer_frame_begin, vkf_viewer_frame_draw
    public :: check_status
    public :: vkf_viewer_gui, vkf_gui_begin, vkf_gui_end, vkf_gui_text, vkf_gui_button, vkf_gui_checkbox, vkf_gui_slider, &
              vkf_gui_separator, vkf_gui_same_line, vkf_gui_combo
    public :: vkf_node_delete, vkf_node_add_child, vkf_node_remove_child, vkf_node_bound, vkf_compile_node, vkf_viewer_any
    public :: vkf_switch_create, vkf_switch_set
    public :: vkf_transform_create, vkf_transform_set_matrix
    public :: vkf_image_read, vkf_image_delete, vkf_model_read
    public :: vkf_mesh_set_bound
    public :: vkf_mesh_create, vkf_mesh_array, vkf_mesh_material, vkf_mesh_material_changed, vkf_mesh_vertices_changed, &
              vkf_mesh_set_twilight
    public :: vkf_light_create, vkf_light_set, vkf_viewer_light, vkf_viewer_light_enable
    public :: vkf_font_read, vkf_font_delete, vkf_label_create, vkf_label_set_text, vkf_label_set_color
    public :: vkf_label_set_layout, vkf_label_set_on_top
    public :: vkf_viewer_hud_add, vkf_viewer_hud_remove, vkf_viewer_remove_node
    public :: vkf_line_create, vkf_line_points, vkf_line_style, vkf_line_points_changed, vkf_line_style_changed
    public :: vkf_line_set_range, vkf_line_set_lod, vkf_marker_create, vkf_line_create_colored, vkf_line_colors
    public :: vkf_ribbon_create, vkf_line_set_highlight, vkf_line_set_trail, vkf_line_set_dash_mode
    public :: vkf_stars_create, vkf_sky_texture_create, vkf_sky_stars, vkf_sky_stars_changed
    public :: vkf_sky_style, vkf_sky_style_changed
    public :: vulkan_device_name, line_set_points, line_set_style, line_set_pattern, line_set_segments, marker_set_style
    public :: mesh_arrays
    public :: vkf_rocky_available, vkf_rocky_map_create, vkf_rocky_map_settings, vkf_rocky_map_radius, vkf_rocky_map_alive
    public :: vkf_rocky_add_image, vkf_rocky_add_elevation, vkf_rocky_elevation
    public :: mesh_set_material
    public :: vkf_sphere_lod_create, vkf_sphere_lod_add, vkf_sphere_lod_requested

    interface

        !> Name of the first Vulkan physical device, into `buf`; returns its length, or -1 on failure.
        !> Needs no window, so it checks that the Vulkan loader finds a driver (MoltenVK on macOS).
        integer(c_int) function vkf_vulkan_device_name(buf, buflen) bind(C, name="vkf_vulkan_device_name")
            import :: c_int, c_char
            implicit none
            character(kind=c_char), dimension(*), intent(out) :: buf  !! the name (not null-terminated)
            integer(c_int), value :: buflen                           !! the length of `buf`
        end function vkf_vulkan_device_name

        !> Start loading the Vulkan driver, on a thread of its own, for the first window (see
        !> DriverPreload in vkf_layer.cpp): called when a scene is set up, before its window opens.
        subroutine vkf_driver_preload() bind(C, name="vkf_driver_preload")
            implicit none
        end subroutine vkf_driver_preload

        !> A host's window now has `ratio` framebuffer pixels per window unit.
        !> (Qt's devicePixelRatio changed: moved to a screen of another scale, or the display's scaling
        !> changed.) Sizes in window units (line widths, markers, labels) are scaled by it; the host also
        !> resizes (vkf_viewer_resize) to the new framebuffer size. Returns 0, or -1 on failure.
        integer(c_int) function vkf_viewer_set_pixel_ratio(viewer, ratio) bind(C, name="vkf_viewer_set_pixel_ratio")
            import :: c_ptr, c_int, c_double
            implicit none
            type(c_ptr), value :: viewer     !! viewer handle (vkf_viewer_create)
            real(c_double), value :: ratio   !! framebuffer pixels per window unit (> 0)
        end function vkf_viewer_set_pixel_ratio

        !> A viewer with one window, an empty scene and a trackball camera; null on failure.
        !> With an existing window (`native_window` not 0) the host owns that window's events and
        !> forwards them (vkf_viewer_mouse, ..., vkf_viewer_resize).
        type(c_ptr) function vkf_viewer_create(width, height, title, title_len, native_window, pixel_ratio, vsync) &
                bind(C, name="vkf_viewer_create")
            import :: c_ptr, c_int, c_char, c_long_long, c_double
            implicit none
            integer(c_int), value :: width, height                   !! window size (window units: logical pixels)
            character(kind=c_char), dimension(*), intent(in) :: title !! window title
            integer(c_int), value :: title_len                        !! the length of `title`
            integer(c_long_long), value :: native_window  !! 0 for a window of its own, or the native handle of
                                                          !! an existing window to render into (e.g. a Qt
                                                          !! QWindow's winId(): an NSView* on macOS, an HWND on
                                                          !! Windows, an xcb window on Linux)
            real(c_double), value :: pixel_ratio  !! framebuffer pixels per window unit of an existing window
                                                  !! (Qt's devicePixelRatio); ignored for a window of its own
            integer(c_int), value :: vsync  !! 1 to present at the display's refresh (FIFO), 0 as soon as a frame is
                                            !! ready (IMMEDIATE where the driver has it, else FIFO), e.g. for benchmarks
        end function vkf_viewer_create

        !> A mouse event from the host's window. Returns 0, or -1 on failure (or an unknown `kind`).
        integer(c_int) function vkf_viewer_mouse(viewer, kind, x, y, mask, button) bind(C, name="vkf_viewer_mouse")
            import :: c_ptr, c_int
            implicit none
            type(c_ptr), value :: viewer   !! viewer handle (vkf_viewer_create)
            integer(c_int), value :: kind  !! 1 = move, 2 = button press, 3 = button release
            integer(c_int), value :: x     !! pointer position (framebuffer pixels from the left)
            integer(c_int), value :: y     !! pointer position (framebuffer pixels from the top)
            integer(c_int), value :: mask  !! the buttons held (VSG's BUTTON_MASK_1/2/3 = 256/512/1024)
            integer(c_int), value :: button  !! the button pressed or released (1 left, 2 middle, 3 right)
        end function vkf_viewer_mouse

        !> A scroll wheel event from the host's window. Returns 0, or -1 on failure.
        integer(c_int) function vkf_viewer_scroll(viewer, dy) bind(C, name="vkf_viewer_scroll")
            import :: c_ptr, c_int, c_float
            implicit none
            type(c_ptr), value :: viewer  !! viewer handle (vkf_viewer_create)
            real(c_float), value :: dy    !! the scroll amount: > 0 away from the user
        end function vkf_viewer_scroll

        !> A key press or release from the host's window, as VSG key symbols and modifiers
        !> (vsg/ui/KeyEvent.h: ASCII for printable keys, X11-style codes otherwise). Returns 0, or -1 on failure.
        integer(c_int) function vkf_viewer_key(viewer, pressed, symbol, modified_symbol, modifier) &
                bind(C, name="vkf_viewer_key")
            import :: c_ptr, c_int
            implicit none
            type(c_ptr), value :: viewer              !! viewer handle (vkf_viewer_create)
            integer(c_int), value :: pressed          !! 1 for a press, 0 for a release
            integer(c_int), value :: symbol           !! the key's base symbol (unmodified)
            integer(c_int), value :: modified_symbol  !! the key's symbol with the modifiers applied ('V' with shift)
            integer(c_int), value :: modifier         !! the modifier keys held (vsg::KeyModifier mask)
        end function vkf_viewer_key

        !> The keyboard and mouse events since the last call, oldest first, up to `capacity` of them.
        !> Returns how many were copied (the rest are kept for the next call), or -1 on failure.
        !> Each event (a column of `events`): kind (1 key press, 2 key release, 3 pointer move, 4 button press,
        !> 5 button release), key (modified: 'V' with shift), key base, modifiers, button, buttons held (mask),
        !> x, y (framebuffer pixels from the top left), and the render area (offset x, y, width, height).
        !> Events taken by a GUI panel are not included.
        integer(c_int) function vkf_viewer_events(viewer, events, capacity) bind(C, name="vkf_viewer_events")
            import :: c_ptr, c_int
            implicit none
            type(c_ptr), value :: viewer                !! viewer handle (vkf_viewer_create)
            integer(c_int), intent(out) :: events(12, *) !! the events, 12 integers each
            integer(c_int), value :: capacity           !! the most events `events` holds
        end function vkf_viewer_events

        !> Framebuffer pixels per window unit (2 on Retina; a host's window's as given).
        real(c_double) function vkf_viewer_pixel_ratio(viewer) bind(C, name="vkf_viewer_pixel_ratio")
            import :: c_ptr, c_double
            implicit none
            type(c_ptr), value :: viewer  !! viewer handle (vkf_viewer_create)
        end function vkf_viewer_pixel_ratio

        !> The swapchain's size (framebuffer pixels): a VSG window's size as it is resized (a host's window
        !> has the size it gives, vkf_viewer_resize). Returns 0, or -1 on failure.
        integer(c_int) function vkf_viewer_size(viewer, width, height) bind(C, name="vkf_viewer_size")
            import :: c_ptr, c_int
            implicit none
            type(c_ptr), value :: viewer                 !! viewer handle (vkf_viewer_create)
            integer(c_int), intent(out) :: width, height !! the size (framebuffer pixels)
        end function vkf_viewer_size

        !> The host resized its window: rebuild the swapchain. A window of no width or height (e.g. a
        !> collapsed splitter) is not drawn until it has a size again. Returns 0, or -1 on failure.
        integer(c_int) function vkf_viewer_resize(viewer, width, height) bind(C, name="vkf_viewer_resize")
            import :: c_ptr, c_int
            implicit none
            type(c_ptr), value :: viewer            !! viewer handle (vkf_viewer_create)
            integer(c_int), value :: width, height  !! the new size (framebuffer pixels)
        end function vkf_viewer_resize

        !> The host is closing the window: the next vkf_viewer_frame returns 0. Returns 0, or -1 on failure.
        integer(c_int) function vkf_viewer_close(viewer) bind(C, name="vkf_viewer_close")
            import :: c_ptr, c_int
            implicit none
            type(c_ptr), value :: viewer  !! viewer handle (vkf_viewer_create)
        end function vkf_viewer_close

        !> Delete a viewer (its window, GUI and default light; waits for the device to be idle first).
        subroutine vkf_viewer_delete(viewer) bind(C, name="vkf_viewer_delete")
            import :: c_ptr
            implicit none
            type(c_ptr), value :: viewer  !! viewer handle (vkf_viewer_create)
        end subroutine vkf_viewer_delete

        !> Set the camera's eye, center and up vectors (view-frame coordinates). Returns 0, or -1 on failure.
        integer(c_int) function vkf_viewer_look_at(viewer, eye, center, up) bind(C, name="vkf_viewer_look_at")
            import :: c_ptr, c_int, c_double
            implicit none
            type(c_ptr), value :: viewer                               !! viewer handle (vkf_viewer_create)
            real(c_double), dimension(3), intent(in) :: eye, center, up !! view-frame coordinates
        end function vkf_viewer_look_at

        !> Set the background (clear) color. Returns 0, or -1 on failure.
        integer(c_int) function vkf_viewer_set_background(viewer, rgba) bind(C, name="vkf_viewer_set_background")
            import :: c_ptr, c_int, c_float
            implicit none
            type(c_ptr), value :: viewer                    !! viewer handle (vkf_viewer_create)
            real(c_float), dimension(4), intent(in) :: rgba !! color (rgba, 0 to 1)
        end function vkf_viewer_set_background

        !> The camera's eye, center and up vectors (view-frame coordinates), as the trackball left them.
        !> Returns 0, or -1 on failure.
        integer(c_int) function vkf_viewer_get_look_at(viewer, eye, center, up) bind(C, name="vkf_viewer_get_look_at")
            import :: c_ptr, c_int, c_double
            implicit none
            type(c_ptr), value :: viewer                                !! viewer handle (vkf_viewer_create)
            real(c_double), dimension(3), intent(out) :: eye, center, up !! view-frame coordinates
        end function vkf_viewer_get_look_at

        !> Set the transform from world (scene root) to view-frame coordinates, applied after the trackball's
        !> eye/center/up (see ViewLookAt). Returns 0, or -1 on failure.
        integer(c_int) function vkf_viewer_set_view_transform(viewer, m) bind(C, name="vkf_viewer_set_view_transform")
            import :: c_ptr, c_int, c_double
            implicit none
            type(c_ptr), value :: viewer                       !! viewer handle (vkf_viewer_create)
            real(c_double), dimension(4,4), intent(in) :: m    !! world to view-frame matrix
        end function vkf_viewer_set_view_transform

        !> Use an orthographic projection with the given view-volume bounds. Returns 0, or -1 on failure.
        integer(c_int) function vkf_viewer_orthographic(viewer, left, right, bottom, top, near_distance, far_distance) &
                bind(C, name="vkf_viewer_orthographic")
            import :: c_ptr, c_int, c_double
            implicit none
            type(c_ptr), value :: viewer  !! viewer handle (vkf_viewer_create)
            real(c_double), value :: left, right, bottom, top, near_distance, far_distance
        end function vkf_viewer_orthographic

        !> Use a perspective projection. Returns 0, or -1 on failure.
        integer(c_int) function vkf_viewer_perspective(viewer, fovy, near_distance, far_distance) &
                bind(C, name="vkf_viewer_perspective")
            import :: c_ptr, c_int, c_double
            implicit none
            type(c_ptr), value :: viewer  !! viewer handle (vkf_viewer_create)
            real(c_double), value :: fovy  !! vertical field of view (degrees)
            real(c_double), value :: near_distance, far_distance  !! near and far plane distances (scene units)
        end function vkf_viewer_perspective

        !> Add a node to the viewer's scene, also while running (it is compiled first). The scene holds a
        !> reference: the node handle can be deleted afterwards. Returns 0, or -1 on failure.
        integer(c_int) function vkf_viewer_add_node(viewer, node) bind(C, name="vkf_viewer_add_node")
            import :: c_ptr, c_int
            implicit none
            type(c_ptr), value :: viewer, node  !! viewer handle (vkf_viewer_create) and node handle
        end function vkf_viewer_add_node

        ! Rocky maps (src/vsg/vkf_rocky.cpp; optional: vkf_rocky_available() is 0 without it)

        !> 1 if vkframes was built with Rocky, else 0.
        integer(c_int) function vkf_rocky_available() bind(C, name="vkf_rocky_available")
            import :: c_int
            implicit none
        end function vkf_rocky_available

        !> A map for the viewer (a node handle: vkf_node_*); null without Rocky or on failure.
        type(c_ptr) function vkf_rocky_map_create(viewer, profile, profile_len, scale) bind(C, name="vkf_rocky_map_create")
            import :: c_ptr, c_int, c_char, c_double
            implicit none
            type(c_ptr), value :: viewer  !! viewer handle (vkf_viewer_create)
            character(kind=c_char), dimension(*), intent(in) :: profile  !! Rocky's profile (e.g.
                                                                         !! "global-geodetic", "global-qsc", "moon";
                                                                         !! empty: Rocky's default)
            integer(c_int), value :: profile_len  !! the length of `profile`
            real(c_double), value :: scale        !! scene units per meter
        end function vkf_rocky_map_create

        !> A map's settings. Returns 0, or -1 on failure.
        integer(c_int) function vkf_rocky_map_settings(map, lighting, background) bind(C, name="vkf_rocky_map_settings")
            import :: c_ptr, c_int, c_float
            implicit none
            type(c_ptr), value :: map        !! map handle (vkf_rocky_map_create)
            integer(c_int), value :: lighting  !! (1: lit by the scene's lights, as vkframes' spheres;
                                               !! 0: imagery as is (Rocky's default))
            real(c_float), dimension(4), intent(in) :: background  !! the color where there is no imagery (rgba, 0 to 1)
        end function vkf_rocky_map_settings

        !> 1 while a map can be drawn (the viewer it was made for exists), else 0.
        integer(c_int) function vkf_rocky_map_alive(map) bind(C, name="vkf_rocky_map_alive")
            import :: c_ptr, c_int
            implicit none
            type(c_ptr), value :: map  !! map handle (vkf_rocky_map_create)
        end function vkf_rocky_map_alive

        !> The equatorial radius of a map's body (meters); 0 without Rocky or on failure.
        real(c_double) function vkf_rocky_map_radius(map) bind(C, name="vkf_rocky_map_radius")
            import :: c_ptr, c_double
            implicit none
            type(c_ptr), value :: map  !! map handle (vkf_rocky_map_create)
        end function vkf_rocky_map_radius

        !> The height (meters) of the elevation data in a file (GDAL) at a latitude and longitude on the body
        !> of a Rocky profile, sampled directly (no map or window needed; files are kept open for later samples).
        real(c_double) function vkf_rocky_elevation(uri, uri_len, profile, profile_len, lat, lon, ok) &
                bind(C, name="vkf_rocky_elevation")
            import :: c_int, c_char, c_double
            implicit none
            character(kind=c_char), dimension(*), intent(in) :: uri, profile  !! the file, and the profile
                                                                              !! (as vkf_rocky_map_create)
            integer(c_int), value :: uri_len, profile_len  !! the lengths of `uri` and `profile`
            real(c_double), value :: lat, lon              !! latitude and longitude (degrees)
            integer(c_int), intent(out) :: ok              !! 1 if there was data there
        end function vkf_rocky_elevation

        !> Add an image layer to a map. Returns 0, or -1 on failure.
        integer(c_int) function vkf_rocky_add_image(map, kind, uri, uri_len) bind(C, name="vkf_rocky_add_image")
            import :: c_ptr, c_int, c_char
            implicit none
            type(c_ptr), value :: map  !! map handle (vkf_rocky_map_create)
            integer(c_int), value :: kind  !! 0 a TMS service (`uri` its root), 1 a file or service GDAL reads,
                                           !! 2 a template ({z}, {x}, {y}) of geodetic tiles on the map's body,
                                           !! rows from the top (WMTS-style, e.g. NASA's Moon Trek),
                                           !! 3 a template of web Mercator tiles (OpenStreetMap-style)
            integer(c_int), value :: uri_len  !! the length of `uri`
            character(kind=c_char), dimension(*), intent(in) :: uri
        end function vkf_rocky_add_image

        !> Add an elevation layer to a map. Returns 0, or -1 on failure.
        integer(c_int) function vkf_rocky_add_elevation(map, kind, uri, uri_len, encoding) &
                bind(C, name="vkf_rocky_add_elevation")
            import :: c_ptr, c_int, c_char
            implicit none
            type(c_ptr), value :: map  !! map handle (vkf_rocky_map_create)
            integer(c_int), value :: kind      !! as vkf_rocky_add_image's
            integer(c_int), value :: uri_len   !! the length of `uri`
            integer(c_int), value :: encoding  !! 0 single-channel heights (meters), 1 Mapbox RGB, 2 Terrarium RGB
            character(kind=c_char), dimension(*), intent(in) :: uri
        end function vkf_rocky_add_elevation

        !> The viewer's GUI panels (vkf_gui.cpp), made on first use: a handle for the vkf_gui_* functions,
        !> valid while the viewer exists; null on failure.
        type(c_ptr) function vkf_viewer_gui(viewer, font, font_len, size) bind(C, name="vkf_viewer_gui")
            import :: c_ptr, c_int, c_char, c_double
            implicit none
            type(c_ptr), value :: viewer                    !! viewer handle (vkf_viewer_create)
            character(kind=c_char), intent(in) :: font(*)   !! font file (empty: ImGui's)
            integer(c_int), value :: font_len               !! the length of `font`
            real(c_double), value :: size                   !! font size (window units)
        end function vkf_viewer_gui

        !> Begin a panel: the widgets up to vkf_gui_end are in it.
        subroutine vkf_gui_begin(gui, title, title_len, x, y, pivot_x, pivot_y, follow) bind(C, name="vkf_gui_begin")
            import :: c_ptr, c_int, c_char, c_double
            implicit none
            type(c_ptr), value :: gui                     !! GUI handle (vkf_viewer_gui)
            character(kind=c_char), intent(in) :: title(*) !! panel title ("##name": no title bar, e.g. a callout)
            integer(c_int), value :: title_len            !! the length of `title`
            integer(c_int), value :: follow  !! not 0: placed at (x, y) every frame (and not movable),
                                             !! else only at first
            real(c_double), value :: x, y    !! position (0 to 1 across the window, y up; x < 0: where ImGui puts it)
            real(c_double), value :: pivot_x, pivot_y  !! the panel's point placed there
                                                       !! (0 to 1 across it, from the top left)
        end subroutine vkf_gui_begin

        !> End the panel begun by vkf_gui_begin.
        subroutine vkf_gui_end(gui) bind(C, name="vkf_gui_end")
            import :: c_ptr
            implicit none
            type(c_ptr), value :: gui  !! GUI handle (vkf_viewer_gui)
        end subroutine vkf_gui_end

        !> A line of text in the current panel.
        subroutine vkf_gui_text(gui, text, text_len) bind(C, name="vkf_gui_text")
            import :: c_ptr, c_int, c_char
            implicit none
            type(c_ptr), value :: gui                     !! GUI handle (vkf_viewer_gui)
            character(kind=c_char), intent(in) :: text(*)
            integer(c_int), value :: text_len             !! the length of `text`
        end subroutine vkf_gui_text

        !> A button: 1 if it was clicked since the last call for it (in this panel), else 0.
        integer(c_int) function vkf_gui_button(gui, label, label_len) bind(C, name="vkf_gui_button")
            import :: c_ptr, c_int, c_char
            implicit none
            type(c_ptr), value :: gui                      !! GUI handle (vkf_viewer_gui)
            character(kind=c_char), intent(in) :: label(*)
            integer(c_int), value :: label_len             !! the length of `label`
        end function vkf_gui_button

        !> A checkbox showing `value`: 1 (and `value` set) if the user changed it since the last call for it, else 0.
        integer(c_int) function vkf_gui_checkbox(gui, label, label_len, value) bind(C, name="vkf_gui_checkbox")
            import :: c_ptr, c_int, c_char
            implicit none
            type(c_ptr), value :: gui                      !! GUI handle (vkf_viewer_gui)
            character(kind=c_char), intent(in) :: label(*)
            integer(c_int), value :: label_len             !! the length of `label`
            integer(c_int), intent(inout) :: value         !! 1 checked, 0 not
        end function vkf_gui_checkbox

        !> A slider showing `value` from `low` to `high` (as `format`, printf's; empty: "%.4g"): 1 (and `value`
        !> set) if the user changed it since the last call for it, else 0.
        integer(c_int) function vkf_gui_slider(gui, label, label_len, value, low, high, format, format_len) &
                bind(C, name="vkf_gui_slider")
            import :: c_ptr, c_int, c_char, c_double
            implicit none
            type(c_ptr), value :: gui                      !! GUI handle (vkf_viewer_gui)
            character(kind=c_char), intent(in) :: label(*)
            integer(c_int), value :: label_len             !! the length of `label`
            real(c_double), intent(inout) :: value
            real(c_double), value :: low, high             !! the slider's range
            character(kind=c_char), intent(in) :: format(*)
            integer(c_int), value :: format_len            !! the length of `format` (0: the default)
        end function vkf_gui_slider

        !> A combo box (drop-down menu) of `items` (newline-separated), showing item `index` (0-based): 1 (and
        !> `index` set) if the user chose another since the last call for it, else 0.
        integer(c_int) function vkf_gui_combo(gui, label, label_len, index, items, items_len) &
                bind(C, name="vkf_gui_combo")
            import :: c_ptr, c_int, c_char
            implicit none
            type(c_ptr), value :: gui                      !! GUI handle (vkf_viewer_gui)
            character(kind=c_char), intent(in) :: label(*)
            integer(c_int), value :: label_len             !! the length of `label`
            integer(c_int), intent(inout) :: index
            character(kind=c_char), intent(in) :: items(*)
            integer(c_int), value :: items_len             !! the length of `items`
        end function vkf_gui_combo

        !> A horizontal separator in the current panel.
        subroutine vkf_gui_separator(gui) bind(C, name="vkf_gui_separator")
            import :: c_ptr
            implicit none
            type(c_ptr), value :: gui  !! GUI handle (vkf_viewer_gui)
        end subroutine vkf_gui_separator

        !> Put the next widget on the same line as the last one.
        subroutine vkf_gui_same_line(gui) bind(C, name="vkf_gui_same_line")
            import :: c_ptr
            implicit none
            type(c_ptr), value :: gui  !! GUI handle (vkf_viewer_gui)
        end subroutine vkf_gui_same_line

        !> The first part of a frame (compiling the scene on the first call): up to the window's events handled
        !> (the trackball has moved the camera), before the scene is drawn (vkf_viewer_frame_draw): a program can
        !> still change the scene for this frame in between. Returns 1 while the viewer is running, 0 once its
        !> window has been closed, -1 on failure.
        integer(c_int) function vkf_viewer_frame_begin(viewer) bind(C, name="vkf_viewer_frame_begin")
            import :: c_ptr, c_int
            implicit none
            type(c_ptr), value :: viewer  !! viewer handle (vkf_viewer_create)
        end function vkf_viewer_frame_begin

        !> The rest of a frame begun by vkf_viewer_frame_begin: the scene's updates, drawn and presented.
        !> Returns 1, or -1 on failure.
        integer(c_int) function vkf_viewer_frame_draw(viewer) bind(C, name="vkf_viewer_frame_draw")
            import :: c_ptr, c_int
            implicit none
            type(c_ptr), value :: viewer  !! viewer handle (vkf_viewer_create)
        end function vkf_viewer_frame_draw

        !> Render one frame (vkf_viewer_frame_begin, then vkf_viewer_frame_draw). Returns 1 while the viewer is
        !> running, 0 once its window has been closed, -1 on failure.
        integer(c_int) function vkf_viewer_frame(viewer) bind(C, name="vkf_viewer_frame")
            import :: c_ptr, c_int
            implicit none
            type(c_ptr), value :: viewer  !! viewer handle (vkf_viewer_create)
        end function vkf_viewer_frame

        !> Write the last presented frame to an image file (format from the extension, e.g. .png).
        !> Call after vkf_viewer_frame. Returns 0 on success.
        integer(c_int) function vkf_viewer_capture(viewer, filename, filename_len) bind(C, name="vkf_viewer_capture")
            import :: c_ptr, c_int, c_char
            implicit none
            type(c_ptr), value :: viewer                                  !! viewer handle (vkf_viewer_create)
            character(kind=c_char), dimension(*), intent(in) :: filename
            integer(c_int), value :: filename_len                         !! the length of `filename`
        end function vkf_viewer_capture

        !> Start recording the frames drawn to a video of `fps` frames per second: piped as raw video to
        !> `ffmpeg` (the program), encoded with `output` (ffmpeg's output options, then the file, quoted).
        !> Returns 0, or -1 if already recording.
        integer(c_int) function vkf_viewer_record_start(viewer, ffmpeg, ffmpeg_len, output, output_len, fps) &
                bind(C, name="vkf_viewer_record_start")
            import :: c_ptr, c_int, c_char, c_double
            implicit none
            type(c_ptr), value :: viewer                                  !! viewer handle (vkf_viewer_create)
            character(kind=c_char), dimension(*), intent(in) :: ffmpeg    !! the ffmpeg command (quoted if need be)
            integer(c_int), value :: ffmpeg_len                           !! the length of `ffmpeg`
            character(kind=c_char), dimension(*), intent(in) :: output    !! ffmpeg's output options and file
            integer(c_int), value :: output_len                           !! the length of `output`
            real(c_double), value :: fps                                  !! frames per second of the video
        end function vkf_viewer_record_start

        !> Stop recording: the last frames written and ffmpeg's input closed (it finishes the file).
        !> Returns the frames written, or -1 if not recording or on failure.
        integer(c_long_long) function vkf_viewer_record_stop(viewer) bind(C, name="vkf_viewer_record_stop")
            import :: c_ptr, c_long_long
            implicit none
            type(c_ptr), value :: viewer                                  !! viewer handle (vkf_viewer_create)
        end function vkf_viewer_record_stop

        !> The trackball's sensitivity: `zoom_speed` scales each scroll step's zoom and a right drag's;
        !> `throw` (1) keeps the camera moving after a drag released while moving. Returns 0, or -1.
        integer(c_int) function vkf_viewer_set_camera_controls(viewer, zoom_speed, throw) &
                bind(C, name="vkf_viewer_set_camera_controls")
            import :: c_ptr, c_int, c_double
            implicit none
            type(c_ptr), value :: viewer                                  !! viewer handle (vkf_viewer_create)
            real(c_double), value :: zoom_speed                           !! (> 0; 1: VSG's)
            integer(c_int), value :: throw                                !! 1: keep moving after a released drag
        end function vkf_viewer_set_camera_controls

        !> Frames recorded so far, or -1 if not recording or recording failed.
        integer(c_long_long) function vkf_viewer_record_frames(viewer) bind(C, name="vkf_viewer_record_frames")
            import :: c_ptr, c_long_long
            implicit none
            type(c_ptr), value :: viewer                                  !! viewer handle (vkf_viewer_create)
        end function vkf_viewer_record_frames

        !> Add a child to a group node (a transform or a switch); while running, compile the child first
        !> (vkf_compile_node). Returns 0, or -1 on failure.
        integer(c_int) function vkf_node_add_child(parent, child) bind(C, name="vkf_node_add_child")
            import :: c_ptr, c_int
            implicit none
            type(c_ptr), value :: parent, child  !! node handles
        end function vkf_node_add_child

        !> Remove a child from a group node (a transform or a switch), also while running: its GPU data are
        !> released a few frames later. Returns 0, or -1 if it is not a child.
        integer(c_int) function vkf_node_remove_child(parent, child) bind(C, name="vkf_node_remove_child")
            import :: c_ptr, c_int
            implicit none
            type(c_ptr), value :: parent, child  !! node handles
        end function vkf_node_remove_child

        !> Compile a node for the running viewers (to add it to a shown scene).
        !> Viewers not yet running compile it with their scene at their first frame. Returns 0 on success.
        integer(c_int) function vkf_compile_node(node) bind(C, name="vkf_compile_node")
            import :: c_ptr, c_int
            implicit none
            type(c_ptr), value :: node  !! node handle
        end function vkf_compile_node

        !> A viewer that exists (the oldest running one, else the oldest), or null.
        type(c_ptr) function vkf_viewer_any() bind(C, name="vkf_viewer_any")
            import :: c_ptr
            implicit none
        end function vkf_viewer_any

        !> A sphere's level of detail for a body of `radius` (its largest semi-axis), centered on the node's
        !> origin: a node handle; meshes are added with vkf_sphere_lod_add.
        type(c_ptr) function vkf_sphere_lod_create(radius) bind(C, name="vkf_sphere_lod_create")
            import :: c_ptr, c_double
            implicit none
            real(c_double), value :: radius  !! scene units
        end function vkf_sphere_lod_create

        !> Add a mesh of `segments` segments around the equator. Returns 0, or -1 on failure.
        integer(c_int) function vkf_sphere_lod_add(lod, mesh, segments) bind(C, name="vkf_sphere_lod_add")
            import :: c_ptr, c_int
            implicit none
            type(c_ptr), value :: lod, mesh  !! handles (vkf_sphere_lod_create, vkf_mesh_create)
            integer(c_int), value :: segments
        end function vkf_sphere_lod_add

        !> The most segments any view wanted since the last call (0 if not drawn).
        integer(c_int) function vkf_sphere_lod_requested(lod) bind(C, name="vkf_sphere_lod_requested")
            import :: c_ptr, c_int
            implicit none
            type(c_ptr), value :: lod  !! handle (vkf_sphere_lod_create)
        end function vkf_sphere_lod_requested

        !> Draw only points `first` .. `first`+`n`-1 (no data is uploaded): e.g. for trace mode.
        !> (Separate segments: `first` odd, `n` even.) Returns 0, or -1 on failure (out of range).
        integer(c_int) function vkf_line_set_range(line, first, n) bind(C, name="vkf_line_set_range")
            import :: c_ptr, c_int
            implicit none
            type(c_ptr), value :: line        !! line, marker or ribbon handle
            integer(c_int), value :: first, n  !! the first point (1-based) and the number of points
        end function vkf_line_set_range

        !> A line strip's level of detail: the bounding sphere of its points and
        !> error(k): how far the points skipped can be from the segments drawn at
        !> stride 2^k (k = 1 .. levels).
        !> Returns 0, or -1 on failure.
        integer(c_int) function vkf_line_set_lod(line, center, radius, error, levels) bind(C, name="vkf_line_set_lod")
            import :: c_ptr, c_int, c_double, c_float
            implicit none
            type(c_ptr), value :: line                          !! line handle
            real(c_double), dimension(3), intent(in) :: center  !! bounding sphere center
            real(c_double), value :: radius                     !! bounding sphere radius
            real(c_float), dimension(*), intent(in) :: error    !! (levels), in the points' units
            integer(c_int), value :: levels                     !! (0: all points drawn)
        end function vkf_line_set_lod

        !> Bounding sphere of a node in its own coordinates. Returns 0, or -1 if the node is empty.
        integer(c_int) function vkf_node_bound(node, center, radius) bind(C, name="vkf_node_bound")
            import :: c_ptr, c_int, c_double
            implicit none
            type(c_ptr), value :: node                           !! node handle
            real(c_double), dimension(3), intent(out) :: center
            real(c_double), intent(out) :: radius
        end function vkf_node_bound

        !> A switch node: a group whose children can be hidden and shown together; null on failure.
        type(c_ptr) function vkf_switch_create() bind(C, name="vkf_switch_create")
            import :: c_ptr
            implicit none
        end function vkf_switch_create

        !> Show or hide all children of a switch. Returns 0, or -1 on failure.
        integer(c_int) function vkf_switch_set(node, visible) bind(C, name="vkf_switch_set")
            import :: c_ptr, c_int
            implicit none
            type(c_ptr), value :: node        !! switch handle (vkf_switch_create)
            integer(c_int), value :: visible  !! 1 to show, 0 to hide
        end function vkf_switch_set

        !> A transform node (a group whose children are placed by a 4x4 matrix), initially identity;
        !> null on failure.
        type(c_ptr) function vkf_transform_create() bind(C, name="vkf_transform_create")
            import :: c_ptr
            implicit none
        end function vkf_transform_create

        !> Set a transform node's matrix. Returns 0, or -1 on failure (not a transform).
        integer(c_int) function vkf_transform_set_matrix(transform, m) bind(C, name="vkf_transform_set_matrix")
            import :: c_ptr, c_int, c_double
            implicit none
            type(c_ptr), value :: transform                  !! transform handle (vkf_transform_create)
            real(c_double), dimension(4,4), intent(in) :: m
        end function vkf_transform_set_matrix

        !> Delete a node handle (a scene or parent that holds the node keeps it).
        subroutine vkf_node_delete(node) bind(C, name="vkf_node_delete")
            import :: c_ptr
            implicit none
            type(c_ptr), value :: node  !! node handle
        end subroutine vkf_node_delete

        !> Read an image file (png, jpg, ...): an image handle, or null on failure.
        type(c_ptr) function vkf_image_read(filename, filename_len) bind(C, name="vkf_image_read")
            import :: c_ptr, c_int, c_char
            implicit none
            character(kind=c_char), dimension(*), intent(in) :: filename
            integer(c_int), value :: filename_len  !! the length of `filename`
        end function vkf_image_read

        !> Delete an image handle.
        subroutine vkf_image_delete(image) bind(C, name="vkf_image_delete")
            import :: c_ptr
            implicit none
            type(c_ptr), value :: image  !! image handle (vkf_image_read)
        end subroutine vkf_image_delete

        !> Read a model file (3ds, obj, gltf, ...): a node handle, or null on failure.
        type(c_ptr) function vkf_model_read(filename, filename_len) bind(C, name="vkf_model_read")
            import :: c_ptr, c_int, c_char
            implicit none
            character(kind=c_char), dimension(*), intent(in) :: filename
            integer(c_int), value :: filename_len  !! the length of `filename`
        end function vkf_model_read

        !> A triangle mesh (a node handle), filled from Fortran (vkf_mesh_array, before the first frame);
        !> null on failure.
        !> A translucent mesh's bounding sphere (its coordinates), for depth sorting and culling.
        !> Returns 0, or -1 if it isn't a translucent mesh (vkf_mesh_create's flag 8).
        integer(c_int) function vkf_mesh_set_bound(mesh, x, y, z, radius) bind(C, name="vkf_mesh_set_bound")
            import :: c_ptr, c_int, c_double
            implicit none
            type(c_ptr), value :: mesh                    !! mesh handle (vkf_mesh_create)
            real(c_double), value :: x, y, z, radius      !! the bound's center and radius
        end function vkf_mesh_set_bound

        type(c_ptr) function vkf_mesh_create(n_vertices, n_indices, rgba, texture, night_texture, flags) &
                bind(C, name="vkf_mesh_create")
            import :: c_ptr, c_int, c_float
            implicit none
            integer(c_int), value :: n_vertices  !! number of vertices
            integer(c_int), value :: n_indices   !! number of triangle indices
            integer(c_int), value :: flags  !! 1 for an unlit transparent surface (no textures), 2 for view lighting
                                            !! (frames' axes), else lit by the scene's lights with a material
                                            !! (vkf_mesh_material); plus 4 for vertices that change after the mesh is
                                            !! compiled (rewritten in place, then vkf_mesh_vertices_changed); plus 8
                                            !! for a lit translucent surface (depth sorted: vkf_mesh_set_bound); plus
                                            !! 16 two-sided (lit: no faces culled); plus 32 a color per vertex
                                            !! (vkf_mesh_array 5; rgba each one's initial color)
            real(c_float), dimension(4), intent(in) :: rgba  !! color (rgba, 0 to 1; multiplies the texture)
            type(c_ptr), value :: texture, night_texture     !! image handles (vkf_image_read), or null
        end function vkf_mesh_create

        !> The material of a lit mesh: 19 floats (vsg::PhongMaterial: ambient, diffuse, specular, emissive rgba,
        !> shininess, alpha mask, alpha mask cutoff); call vkf_mesh_material_changed after writing them.
        !> Null for a mesh without one.
        type(c_ptr) function vkf_mesh_material(mesh) bind(C, name="vkf_mesh_material")
            import :: c_ptr
            implicit none
            type(c_ptr), value :: mesh  !! mesh handle (vkf_mesh_create)
        end function vkf_mesh_material

        !> Upload a mesh's material again after writing it (vkf_mesh_material). Returns 0, or -1 on failure.
        integer(c_int) function vkf_mesh_material_changed(mesh) bind(C, name="vkf_mesh_material_changed")
            import :: c_ptr, c_int
            implicit none
            type(c_ptr), value :: mesh  !! mesh handle (vkf_mesh_create)
        end function vkf_mesh_material_changed

        !> Upload a mesh's vertices again after rewriting them in place (a mesh
        !> created with flag 4).
        !> Returns 0, or -1 on failure.
        integer(c_int) function vkf_mesh_vertices_changed(mesh) bind(C, name="vkf_mesh_vertices_changed")
            import :: c_ptr, c_int
            implicit none
            type(c_ptr), value :: mesh  !! mesh handle (vkf_mesh_create)
        end function vkf_mesh_vertices_changed

        !> A lit mesh's twilight: its diffuse light extended to `angle` (radians)
        !> past the terminator, fading out (0: none).
        !> Returns 0, or -1 on failure.
        integer(c_int) function vkf_mesh_set_twilight(mesh, angle) bind(C, name="vkf_mesh_set_twilight")
            import :: c_ptr, c_int, c_float
            implicit none
            type(c_ptr), value :: mesh   !! mesh handle (vkf_mesh_create)
            real(c_float), value :: angle !! radians
        end function vkf_mesh_set_twilight

        !> Remove a node from the viewer's scene (its GPU data are released a few frames later).
        !> Returns 0, or -1 if it is not in the scene.
        integer(c_int) function vkf_viewer_remove_node(viewer, node) bind(C, name="vkf_viewer_remove_node")
            import :: c_ptr, c_int
            implicit none
            type(c_ptr), value :: viewer, node  !! viewer handle (vkf_viewer_create) and node handle
        end function vkf_viewer_remove_node

        !> Remove a label from the viewer's HUD (its GPU data are released a few frames later).
        !> Returns 0, or -1 on failure.
        integer(c_int) function vkf_viewer_hud_remove(viewer, label) bind(C, name="vkf_viewer_hud_remove")
            import :: c_ptr, c_int
            implicit none
            type(c_ptr), value :: viewer, label  !! viewer handle (vkf_viewer_create) and label handle
        end function vkf_viewer_hud_remove

        !> A light (a node handle), initially a directional light along -z (in its node's coordinates) of white
        !> diffuse color and no ambient or specular part; null on failure.
        type(c_ptr) function vkf_light_create() bind(C, name="vkf_light_create")
            import :: c_ptr
            implicit none
        end function vkf_light_create

        !> Set a light's colors and position, in its node's coordinates. Returns 0 (-1 for a bad handle).
        integer(c_int) function vkf_light_set(light, ambient, diffuse, specular, position) bind(C, name="vkf_light_set")
            import :: c_ptr, c_int, c_float, c_double
            implicit none
            type(c_ptr), value :: light  !! light handle (vkf_light_create, vkf_viewer_light)
            real(c_float), dimension(3), intent(in) :: ambient, diffuse, specular  !! colors (rgb, 0 to 1)
            real(c_double), dimension(4), intent(in) :: position  !! OpenGL's (x, y, z, w): w = 0 for a directional
                                                                  !! light from direction (x, y, z), else a point
                                                                  !! light at (x/w, y/w, z/w)
        end function vkf_light_set

        !> The viewer's default light (a vkf_light_create light in eye coordinates, owned by the viewer);
        !> null on failure.
        type(c_ptr) function vkf_viewer_light(viewer) bind(C, name="vkf_viewer_light")
            import :: c_ptr
            implicit none
            type(c_ptr), value :: viewer  !! viewer handle (vkf_viewer_create)
        end function vkf_viewer_light

        !> Turn the viewer's default light on or off. Returns 0, or -1 on failure.
        integer(c_int) function vkf_viewer_light_enable(viewer, enabled) bind(C, name="vkf_viewer_light_enable")
            import :: c_ptr, c_int
            implicit none
            type(c_ptr), value :: viewer      !! viewer handle (vkf_viewer_create)
            integer(c_int), value :: enabled  !! 1 on, 0 off
        end function vkf_viewer_light_enable

        !> One of the mesh's arrays, to fill in place; valid while the mesh handle exists.
        type(c_ptr) function vkf_mesh_array(mesh, which) bind(C, name="vkf_mesh_array")
            import :: c_ptr, c_int
            implicit none
            type(c_ptr), value :: mesh     !! mesh handle (vkf_mesh_create)
            integer(c_int), value :: which !! 1 = vertices (3 floats each), 2 = normals (3 floats), 3 = texture
                                           !! coordinates (2 floats), 4 = triangle indices (uint32, 0-based)
        end function vkf_mesh_array

        !> Read a font file (TrueType, ... through vsgXchange's freetype): a font handle, or null on failure.
        !> Converting a font takes seconds, so the result is cached and read from there next time.
        type(c_ptr) function vkf_font_read(filename, filename_len) bind(C, name="vkf_font_read")
            import :: c_ptr, c_int, c_char
            implicit none
            character(kind=c_char), dimension(*), intent(in) :: filename
            integer(c_int), value :: filename_len  !! the length of `filename`
        end function vkf_font_read

        !> Delete a font handle.
        subroutine vkf_font_delete(font) bind(C, name="vkf_font_delete")
            import :: c_ptr
            implicit none
            type(c_ptr), value :: font  !! font handle (vkf_font_read)
        end subroutine vkf_font_delete

        !> A label (a node handle): `text` at `position`, facing the screen; null on failure.
        type(c_ptr) function vkf_label_create(font, text, text_len, rgba, position, size, max_size, halign, valign) &
                bind(C, name="vkf_label_create")
            import :: c_ptr, c_int, c_char, c_float, c_double
            implicit none
            type(c_ptr), value :: font                              !! font handle (vkf_font_read)
            character(kind=c_char), dimension(*), intent(in) :: text
            integer(c_int), value :: text_len                       !! the length of `text`
            real(c_float), dimension(4), intent(in) :: rgba         !! color (rgba, 0 to 1)
            real(c_double), dimension(3), intent(in) :: position    !! in its parent's coordinates
            real(c_float), value :: size      !! > 0: `size` window units tall (a fixed screen size);
                                              !! < 0: -`size` scene units tall, but at most `max_size` on screen
            real(c_float), value :: max_size  !! the most window units tall on screen (`size` < 0)
            integer(c_int), value :: halign, valign  !! (vsg::StandardLayout's alignments (0 baseline,
                                                     !! 1 left/top, 2 center, 3 right/bottom))
        end function vkf_label_create

        !> Change a label's text, in place: a text longer than the label's capacity (glyphs allocated when it
        !> was made) is cut to it, with a warning. Returns 0, or -1 on failure.
        integer(c_int) function vkf_label_set_text(label, text, text_len) bind(C, name="vkf_label_set_text")
            import :: c_ptr, c_int, c_char
            implicit none
            type(c_ptr), value :: label                             !! label handle (vkf_label_create)
            character(kind=c_char), dimension(*), intent(in) :: text
            integer(c_int), value :: text_len                       !! the length of `text`
        end function vkf_label_set_text

        !> Draw a label on top of the scene (at the near plane, as HUD labels: not hidden by what is in front of
        !> it) or depth-tested. Returns 0, or -1 on failure.
        integer(c_int) function vkf_label_set_on_top(label, on) bind(C, name="vkf_label_set_on_top")
            import :: c_ptr, c_int
            implicit none
            type(c_ptr), value :: label  !! label handle (vkf_label_create)
            integer(c_int), value :: on  !! not 0: on top; 0: depth-tested
        end function vkf_label_set_on_top

        !> Change a label's layout, in place. Returns 0, or -1 on failure.
        integer(c_int) function vkf_label_set_layout(label, position, size, max_size, halign, valign) &
                bind(C, name="vkf_label_set_layout")
            import :: c_ptr, c_int, c_double, c_float
            implicit none
            type(c_ptr), value :: label                  !! label handle (vkf_label_create)
            real(c_double), intent(in) :: position(3)    !! as vkf_label_create's
            real(c_float), value :: size, max_size       !! as vkf_label_create's
            integer(c_int), value :: halign, valign      !! as vkf_label_create's
        end function vkf_label_set_layout

        !> Change a label's color. Returns 0, or -1 on failure.
        integer(c_int) function vkf_label_set_color(label, rgba) bind(C, name="vkf_label_set_color")
            import :: c_ptr, c_int, c_float
            implicit none
            type(c_ptr), value :: label                      !! label handle (vkf_label_create)
            real(c_float), dimension(4), intent(in) :: rgba  !! color (rgba, 0 to 1)
        end function vkf_label_set_color

        !> Show a label in the viewer's HUD, on top of the scene, also while running: the label's position is
        !> (x, y) from 0 to 1 across the viewport, bottom to top. Returns 0, or -1 on failure.
        integer(c_int) function vkf_viewer_hud_add(viewer, label) bind(C, name="vkf_viewer_hud_add")
            import :: c_ptr, c_int
            implicit none
            type(c_ptr), value :: viewer, label  !! viewer handle (vkf_viewer_create) and label handle
        end function vkf_viewer_hud_add

        !> A line strip holding up to `capacity` points (initially none): a node handle, or null on failure.
        !> Fortran fills its point and style buffers in place (vkf_line_points, vkf_line_style), then calls
        !> vkf_line_points_changed / vkf_line_style_changed. It can be shown in any number of windows.
        type(c_ptr) function vkf_line_create(capacity) bind(C, name="vkf_line_create")
            import :: c_ptr, c_int
            implicit none
            integer(c_int), value :: capacity  !! the most points
        end function vkf_line_create

        !> Stars (up to `capacity`; initially none): a node handle, or null on failure. Fortran fills the
        !> buffer in place (vkf_sky_stars) and calls vkf_sky_stars_changed.
        type(c_ptr) function vkf_stars_create(capacity) bind(C, name="vkf_stars_create")
            import :: c_ptr, c_int
            implicit none
            integer(c_int), value :: capacity  !! the most stars
        end function vkf_stars_create

        !> The sky texture: an image (equirectangular, as a Sphere's texture) on the sky seen from inside;
        !> a node handle, or null on failure.
        type(c_ptr) function vkf_sky_texture_create(image) bind(C, name="vkf_sky_texture_create")
            import :: c_ptr
            implicit none
            type(c_ptr), value :: image  !! image handle (vkf_image_read)
        end function vkf_sky_texture_create

        !> The stars' buffer, 8 floats per star (direction x, y, z, size in window units, then r, g, b, unused),
        !> to fill in place; null for a sky texture.
        type(c_ptr) function vkf_sky_stars(node, capacity) bind(C, name="vkf_sky_stars")
            import :: c_ptr, c_int
            implicit none
            type(c_ptr), value :: node              !! stars handle (vkf_stars_create)
            integer(c_int), intent(out) :: capacity !! the buffer's capacity (stars)
        end function vkf_sky_stars

        !> The first `n` stars were written: draw them, and upload the buffer. Returns 0, or -1 on failure.
        integer(c_int) function vkf_sky_stars_changed(node, n) bind(C, name="vkf_sky_stars_changed")
            import :: c_ptr, c_int
            implicit none
            type(c_ptr), value :: node  !! stars handle (vkf_stars_create)
            integer(c_int), value :: n  !! the number of stars (at most the capacity)
        end function vkf_sky_stars_changed

        !> The sky's style buffer: 20 floats, the attitude (a column-major 4x4 matrix, sky frame to root frame),
        !> then 4 parameters (unused); call vkf_sky_style_changed after writing them. Null on failure.
        type(c_ptr) function vkf_sky_style(node) bind(C, name="vkf_sky_style")
            import :: c_ptr
            implicit none
            type(c_ptr), value :: node  !! sky handle (vkf_stars_create, vkf_sky_texture_create)
        end function vkf_sky_style

        !> Upload a sky's style again after writing it (vkf_sky_style). Returns 0, or -1 on failure.
        integer(c_int) function vkf_sky_style_changed(node) bind(C, name="vkf_sky_style_changed")
            import :: c_ptr, c_int
            implicit none
            type(c_ptr), value :: node  !! sky handle (vkf_stars_create, vkf_sky_texture_create)
        end function vkf_sky_style_changed

        !> Markers at up to `capacity` points (initially none): a screen-space quad per point, of the shape and
        !> size in the style buffer (shaders/marker.vert); a node handle, or null on failure. The point and style
        !> buffers are those of a line (vkf_line_points, vkf_line_style, vkf_line_points_changed,
        !> vkf_line_set_range, vkf_line_style_changed); with `colored` (1), a color per marker
        !> (vkf_line_colors), times the style's.
        type(c_ptr) function vkf_marker_create(capacity, image, colored) bind(C, name="vkf_marker_create")
            import :: c_ptr, c_int
            implicit none
            integer(c_int), value :: capacity  !! the most points
            type(c_ptr), value :: image  !! image handle (vkf_image_read) for the image shape, or null
            integer(c_int), value :: colored  !! 1: a color per marker, 0: the style's only
        end function vkf_marker_create

        !> A line with a color per point (vkf_line_colors), blended along each segment.
        !> A node handle, or null on failure.
        type(c_ptr) function vkf_line_create_colored(capacity) bind(C, name="vkf_line_create_colored")
            import :: c_ptr, c_int
            implicit none
            integer(c_int), value :: capacity  !! the most points
        end function vkf_line_create_colored

        !> A ribbon of up to `capacity` points (even): a translucent surface through
        !> pairs of points, a quad between each pair and the next; a color per
        !> point (vkf_line_colors) if `colored` is not 0. The buffers are a line's.
        !> A node handle, or null on failure.
        type(c_ptr) function vkf_ribbon_create(capacity, colored) bind(C, name="vkf_ribbon_create")
            import :: c_ptr, c_int
            implicit none
            integer(c_int), value :: capacity  !! the most points (even)
            integer(c_int), value :: colored   !! not 0: a color per point
        end function vkf_ribbon_create

        !> The colors (4 floats per point) of a line made by vkf_line_create_colored (null for others).
        type(c_ptr) function vkf_line_colors(line, capacity) bind(C, name="vkf_line_colors")
            import :: c_ptr, c_int
            implicit none
            type(c_ptr), value :: line              !! line or ribbon handle
            integer(c_int), intent(out) :: capacity !! the buffer's capacity (points)
        end function vkf_line_colors

        !> A line's point buffer, 8 floats per point (x, y, z and the distance along the strip from its start,
        !> high parts, then their low parts; see shaders/line.vert; markers ignore the distance), to fill in
        !> place; valid while the line handle exists. Null on failure.
        type(c_ptr) function vkf_line_points(line, capacity) bind(C, name="vkf_line_points")
            import :: c_ptr, c_int
            implicit none
            type(c_ptr), value :: line              !! line, marker or ribbon handle
            integer(c_int), intent(out) :: capacity !! the buffer's capacity (points)
        end function vkf_line_points

        !> A line's style buffer: 8 floats (r, g, b, a, width in window units, 1 for separate segments (pairs of
        !> points) or 0 for a strip, the dash pattern (16 bits; 65535 or 0: solid), window units per bit of it).
        !> Set the segment flag before drawing points. Null on failure.
        type(c_ptr) function vkf_line_style(line) bind(C, name="vkf_line_style")
            import :: c_ptr
            implicit none
            type(c_ptr), value :: line  !! line, marker or ribbon handle
        end function vkf_line_style

        !> The first `n` points of the buffer have been written: draw `n` points and upload the buffer.
        !> Returns 0, or -1 if `n` exceeds the capacity.
        integer(c_int) function vkf_line_points_changed(line, n) bind(C, name="vkf_line_points_changed")
            import :: c_ptr, c_int
            implicit none
            type(c_ptr), value :: line  !! line, marker or ribbon handle
            integer(c_int), value :: n  !! the number of points
        end function vkf_line_points_changed

        !> Upload a line's style again after writing it (vkf_line_style). Returns 0, or -1 on failure.
        integer(c_int) function vkf_line_style_changed(line) bind(C, name="vkf_line_style_changed")
            import :: c_ptr, c_int
            implicit none
            type(c_ptr), value :: line  !! line, marker or ribbon handle
        end function vkf_line_style_changed

        !> Set a line's highlight (shaders/line.vert) and upload it: `values`
        !> are its color (rgba), (flags: 1 pulse, 2 arrows; arrow spacing
        !> (window units), arrow speed (window units per second), band width
        !> (window units)), (pulse period (s), 0, 0, 0). Returns 0, or -1 if
        !> `line` isn't a line.
        integer(c_int) function vkf_line_set_highlight(line, values) bind(C, name="vkf_line_set_highlight")
            import :: c_ptr, c_int, c_float
            implicit none
            type(c_ptr), value :: line                      !! line handle
            real(c_float), dimension(12), intent(in) :: values
        end function vkf_line_set_highlight

        !> Set a line strip's trail (shaders/line.vert) and upload it: `values`
        !> are (the distance at the current time as a high/low pair, the
        !> direction (1, -1; 0: no trail), the color flags), (the past's cutoff
        !> and fade start, the future's fade start and cutoff: distances from the
        !> current time's), the past's color (rgba), the future's, (the past's
        !> dash pattern and factor, the future's). Returns 0, or -1 if `line`
        !> isn't a line strip.
        integer(c_int) function vkf_line_set_trail(line, values) bind(C, name="vkf_line_set_trail")
            import :: c_ptr, c_int, c_float
            implicit none
            type(c_ptr), value :: line                      !! line handle
            real(c_float), dimension(20), intent(in) :: values
        end function vkf_line_set_trail

        !> A line's dashes: `screen` 1 screen-locked (a constant length in pixels, following the line on
        !> screen), 0 anchored to the line (the default). Returns 0, or -1 if `line` isn't a line.
        integer(c_int) function vkf_line_set_dash_mode(line, mode, length) bind(C, name="vkf_line_set_dash_mode")
            import :: c_ptr, c_int, c_double
            implicit none
            type(c_ptr), value :: line                      !! line handle
            integer(c_int), value :: mode                   !! 0 anchored, 1 screen-locked, 2 a fixed length
            real(c_double), value :: length                 !! (2) the pattern's length, scene units (0: from the first view)
        end function vkf_line_set_dash_mode

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
        type(c_ptr), intent(in) :: line                   !! line, marker or ribbon handle
        real(c_double), dimension(:,:), intent(in) :: xyz !! (3, n) points
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
    !> Returns 0, or -1 on failure.
    integer(c_int) function line_set_style(line, rgba, width) result(status)
        type(c_ptr), intent(in) :: line                  !! line or ribbon handle
        real(c_float), dimension(4), intent(in) :: rgba  !! color (rgba, 0 to 1)
        real(c_float), intent(in) :: width               !! width (window units)
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
    !> Returns 0, or -1 on failure.
    integer(c_int) function line_set_pattern(line, factor, pattern) result(status)
        type(c_ptr), intent(in) :: line  !! line handle
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
    !> Returns 0, or -1 on failure.
    integer(c_int) function line_set_segments(line, segments) result(status)
        type(c_ptr), intent(in) :: line      !! line handle
        logical, intent(in) :: segments      !! true for separate segments, false for a strip
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
    !> Returns 0, or -1 on failure.
    integer(c_int) function marker_set_style(marker, rgba, size, shape, radius) result(status)
        type(c_ptr), intent(in) :: marker                !! marker handle (vkf_marker_create)
        real(c_float), dimension(4), intent(in) :: rgba  !! color (rgba, 0 to 1)
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
    !> Returns 0, or -1 on failure (e.g. a mesh without a material).
    integer(c_int) function mesh_set_material(mesh, ambient, diffuse, specular, emissive, shininess) result(status)
        type(c_ptr), intent(in) :: mesh  !! mesh handle (vkf_mesh_create)
        real(c_float), dimension(4), intent(in) :: ambient, diffuse, specular, emissive  !! colors (rgba, 0 to 1)
        real(c_float), intent(in) :: shininess  !! Phong exponent
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
        type(c_ptr), intent(in) :: mesh  !! mesh handle (vkf_mesh_create)
        integer, intent(in) :: n_vertices, n_indices
        real(c_float), dimension(:,:), pointer, intent(out) :: xyz      !! (3, n_vertices)
        real(c_float), dimension(:,:), pointer, intent(out) :: normals  !! (3, n_vertices)
        real(c_float), dimension(:,:), pointer, intent(out) :: uv       !! (2, n_vertices)
        integer(c_int32_t), dimension(:), pointer, intent(out) :: indices  !! (n_indices) triangle indices, 0-based
        call c_f_pointer(vkf_mesh_array(mesh, 1_c_int), xyz, [3, n_vertices])
        call c_f_pointer(vkf_mesh_array(mesh, 2_c_int), normals, [3, n_vertices])
        call c_f_pointer(vkf_mesh_array(mesh, 3_c_int), uv, [2, n_vertices])
        call c_f_pointer(vkf_mesh_array(mesh, 4_c_int), indices, [n_indices])
    end subroutine mesh_arrays

    !> Report a failed rendering-layer call (a status not 0), saying where
    !> (`context`, e.g. 'shapes').
    subroutine check_status(status, context)
        integer(c_int), intent(in) :: status                !! a rendering-layer call's result (0: success)
        character(len=*), intent(in), optional :: context  !! where the call was made
        if (status == 0) return
        if (present(context)) then
            write(*, '(a)') 'vkframes: a rendering-layer call failed ('//context//')'
        else
            write(*, '(a)') 'vkframes: a rendering-layer call failed'
        end if
    end subroutine check_status

end module vkf_layer
