!> Reference shapes, as OpenFrames' frames of the same names: CoordinateAxes
!> (axis lines with tick marks), LatLonGrid (latitude and longitude lines on
!> an ellipsoid), RadialPlane (a transparent disk with circles and radial
!> lines) and cones (PolyhedralCone, and its EllipticCone and RectangularCone:
!> e.g. sensor fields of view).
!>
!> Each is a frame drawing in its own coordinates. Its drawing is made when a
!> window first shows it and made again when a setting changes (also while
!> shown; the new nodes are compiled and swapped in). Lines are vkframes'
!> double-precision lines (widths in window units); tick marks are markers
!> (MarkerArtist's shapes); surfaces are unlit and transparent, seen from both
!> sides, and write no depth (as OpenFrames').
module vkf_shapes

    use, intrinsic :: iso_c_binding, only: c_ptr, c_null_ptr, c_associated, c_int, c_int32_t, c_float, c_double
    use vkf_math, only: wp, pi, quat_from_matrix
    use vkf_frames, only: ReferenceFrame, X_AXIS, Y_AXIS, Z_AXIS, ALL_AXES
    use vkf_artists, only: MARKER_DISK, MARKER_IMAGE
    use vkf_files, only: find_data_file
    use vkf_layer, only: vkf_line_create, vkf_marker_create, vkf_mesh_create, vkf_switch_create, &
                         vkf_node_add_child, vkf_node_remove_child, vkf_node_delete, vkf_viewer_compile_node, &
                         vkf_image_read, vkf_image_delete, line_set_points, line_set_style, line_set_segments, &
                         marker_set_style, mesh_arrays

    implicit none

    private

    ! what a cone draws (OpenFrames' PolyhedralCone::DrawMode; combine with ior)
    integer, parameter, public :: CONE_NONE = 0, CONE_SIDES = 1, CONE_EDGES = 2, CONE_BASE_OUTLINE = 4
    integer, parameter, public :: CONE_ALL = 7

    !> A shape's nodes: made under the frame's contents, replaced when the
    !> shape changes.
    type :: ShapeNodes
        type(c_ptr) :: viewer = c_null_ptr, contents = c_null_ptr
        type(c_ptr) :: shape = c_null_ptr
    contains
        procedure :: replace
    end type ShapeNodes

    !> Coordinate axes with tick marks (OpenFrames' CoordinateAxes): lines from
    !> the origin along x, y and z, major and minor tick marks at their
    !> spacings, in the frame's color. Tick marks shrink beyond the axis length
    !> from the camera (OpenFrames' tick attenuation). The frame's own axes
    !> (arrows) are moved to the lines' ends when the length is set (as
    !> OpenFrames'); move them after that with move_x_axis, ..., or, for axes
    !> on a body, hide them (show_axes(NO_AXES)) and show the body's, which
    !> start at its surface.
    type, extends(ReferenceFrame), public :: CoordinateAxes
        private
        real(wp) :: length_ = 1.0_wp
        integer :: drawn = ALL_AXES
        real(wp) :: width_ = 1.0_wp
        real(wp) :: major = 0.5_wp, minor = 0.25_wp
        real(wp) :: major_size = 8.0_wp, minor_size = 4.0_wp
        integer :: shape_ = MARKER_DISK
        character(len=:), allocatable :: image_file
        type(c_ptr) :: image = c_null_ptr
        type(ShapeNodes) :: nodes
    contains
        procedure :: set_axis_length, axis_length
        procedure :: set_draw_axes, draw_axes
        procedure :: set_axis_width, axis_width
        procedure :: set_tick_spacing, tick_spacing
        procedure :: set_tick_size, tick_size
        procedure :: set_tick_shape, tick_shape
        procedure :: set_tick_image, tick_image
        procedure :: set_color => axes_set_color
        procedure :: frame_info => axes_frame_info
        procedure :: bound => axes_bound
        procedure :: build_contents => axes_build_contents
    end type CoordinateAxes

    interface CoordinateAxes
        module procedure new_coordinate_axes
    end interface CoordinateAxes

    !> Latitude and longitude lines on an ellipsoid (OpenFrames' LatLonGrid):
    !> at geodetic latitudes (matching a Sphere's texture), every `lat_spacing`
    !> from the equator and every `lon_spacing` from the prime meridian (the
    !> equator and prime meridian thicker), in the frame's color.
    type, extends(ReferenceFrame), public :: LatLonGrid
        private
        real(wp) :: radii_(3) = 1.0_wp
        real(wp) :: lat_spacing = 30.0_wp * pi / 180.0_wp, lon_spacing = 30.0_wp * pi / 180.0_wp
        real(wp) :: width_ = 1.0_wp, main_width = 3.0_wp
        type(ShapeNodes) :: nodes
    contains
        procedure :: set_parameters => grid_set_parameters, parameters => grid_parameters
        procedure :: set_line_widths, line_widths
        procedure :: set_color => grid_set_color
        procedure :: frame_info => grid_frame_info
        procedure :: bound => grid_bound
        procedure :: build_contents => grid_build_contents
    end type LatLonGrid

    interface LatLonGrid
        module procedure new_lat_lon_grid
    end interface LatLonGrid

    !> A plane disk in the xy plane (OpenFrames' RadialPlane): transparent, with
    !> circles every `radial_spacing` and radial lines every `lon_spacing` (the
    !> one along +x thicker).
    type, extends(ReferenceFrame), public :: RadialPlane
        private
        real(wp) :: radius_ = 1.0_wp, radial_spacing = 0.5_wp, lon_spacing = 30.0_wp * pi / 180.0_wp
        real(wp) :: plane_color_(4) = [0.8_wp, 0.8_wp, 0.8_wp, 0.2_wp]
        real(wp) :: line_color_(4) = [1.0_wp, 1.0_wp, 1.0_wp, 0.2_wp]
        type(ShapeNodes) :: nodes
    contains
        procedure :: set_parameters => plane_set_parameters, parameters => plane_parameters
        procedure :: set_plane_color, plane_color
        procedure :: set_line_color => plane_set_line_color, line_color => plane_line_color
        procedure :: frame_info => plane_frame_info
        procedure :: bound => plane_bound
        procedure :: build_contents => plane_build_contents
    end type RadialPlane

    interface RadialPlane
        module procedure new_radial_plane
    end interface RadialPlane

    !> A cone of polygonal cross-section (OpenFrames' PolyhedralCone), with its
    !> apex at the origin and boresight along -z: vertex i at clock angle
    !> clock(i) (from +x toward +y) and cone angle cone(i) (from the
    !> boresight), `cone_length` along the boresight. Drawn as transparent sides,
    !> edges from the apex and the base outline (`set_draw_mode`).
    type, extends(ReferenceFrame), public :: PolyhedralCone
        private
        real(wp), allocatable :: clock(:), cone(:)
        real(wp) :: length_ = 1.0_wp
        real(wp) :: cone_color_(4) = 1.0_wp, line_color_(4) = 1.0_wp
        integer :: mode = CONE_ALL
        real(wp) :: width_ = 1.0_wp
        type(ShapeNodes) :: nodes
    contains
        procedure :: set_vertex_angles, clock_angles, cone_angles
        procedure :: set_cone_length, cone_length
        procedure :: set_cone_color, cone_color
        procedure :: set_line_color => cone_set_line_color, line_color => cone_line_color
        procedure :: set_draw_mode, draw_mode
        procedure :: make_cone_look_at
        procedure :: is_visible => cone_is_visible
        procedure :: frame_info => cone_frame_info
        procedure :: bound => cone_bound
        procedure :: build_contents => cone_build_contents
    end type PolyhedralCone

    interface PolyhedralCone
        module procedure new_polyhedral_cone
    end interface PolyhedralCone

    !> An elliptic cone (OpenFrames' EllipticCone): half-angles a along x and b
    !> along y (`set_primary_angles`; default 60 and 30 degrees).
    type, extends(PolyhedralCone), public :: EllipticCone
        private
        real(wp) :: a = 60.0_wp * pi / 180.0_wp, b = 30.0_wp * pi / 180.0_wp
    contains
        procedure :: set_primary_angles => elliptic_set_primary_angles
        procedure :: primary_angles => elliptic_primary_angles
        procedure :: is_visible => elliptic_is_visible
        procedure :: frame_info => elliptic_frame_info
    end type EllipticCone

    interface EllipticCone
        module procedure new_elliptic_cone
    end interface EllipticCone

    !> A rectangular cone (OpenFrames' RectangularCone): half-angles x and y
    !> (`set_primary_angles`; default 60 and 30 degrees).
    type, extends(PolyhedralCone), public :: RectangularCone
        private
        real(wp) :: x = 60.0_wp * pi / 180.0_wp, y = 30.0_wp * pi / 180.0_wp
    contains
        procedure :: set_primary_angles => rect_set_primary_angles
        procedure :: primary_angles => rect_primary_angles
        procedure :: frame_info => rect_frame_info
    end type RectangularCone

    interface RectangularCone
        module procedure new_rectangular_cone
    end interface RectangularCone

contains

    ! ==================================================================
    ! nodes

    !> Show `new` (a switch made by the shape) in place of the shape's current
    !> nodes; compiled first if the window already runs.
    subroutine replace(self, new)
        class(ShapeNodes), intent(inout) :: self
        type(c_ptr), intent(in) :: new
        if (.not. c_associated(self%contents)) then          ! not shown yet
            if (c_associated(new)) call vkf_node_delete(new)
            return
        end if
        if (c_associated(self%shape)) then
            call check(vkf_node_remove_child(self%contents, self%shape))
            call vkf_node_delete(self%shape)
        end if
        self%shape = new
        if (.not. c_associated(new)) return
        if (c_associated(self%viewer)) call check(vkf_viewer_compile_node(self%viewer, new))
        call check(vkf_node_add_child(self%contents, new))
    end subroutine replace

    !> Add `child` to `group` (its handle is not kept).
    subroutine add(group, child)
        type(c_ptr), intent(in) :: group, child
        if (.not. c_associated(child)) return
        call check(vkf_node_add_child(group, child))
        call vkf_node_delete(child)
    end subroutine add

    !> A line node through points xyz(3, n): a strip, or separate segments
    !> (points 1-2, 3-4, ...).
    function line_node(xyz, rgba, width, segments) result(node)
        real(wp), intent(in) :: xyz(:,:), rgba(4), width
        logical, intent(in) :: segments
        type(c_ptr) :: node
        node = c_null_ptr
        if (size(xyz, 2) < 2) return
        node = vkf_line_create(int(size(xyz, 2), c_int))
        if (.not. c_associated(node)) return
        if (segments) call check(line_set_segments(node, .true.))
        call check(line_set_style(node, real(rgba, c_float), real(width, c_float)))
        call check(line_set_points(node, real(xyz, c_double)))
    end function line_node

    !> A marker node at points xyz(3, n), `px` window units (MarkerArtist's
    !> shapes and attenuation).
    function marker_node(xyz, rgba, px, shape, radius, image) result(node)
        real(wp), intent(in) :: xyz(:,:), rgba(4), px, radius
        integer, intent(in) :: shape
        type(c_ptr), intent(in) :: image
        type(c_ptr) :: node
        node = c_null_ptr
        if (size(xyz, 2) < 1) return
        node = vkf_marker_create(int(size(xyz, 2), c_int), image)
        if (.not. c_associated(node)) return
        call check(marker_set_style(node, real(rgba, c_float), real(px, c_float), shape, real(radius, c_float)))
        call check(line_set_points(node, real(xyz, c_double)))
    end function marker_node

    !> An unlit transparent surface of triangles idx(3, m) (1-based) through
    !> points xyz(3, n).
    function surface_node(xyz, idx, rgba) result(node)
        real(wp), intent(in) :: xyz(:,:), rgba(4)
        integer, intent(in) :: idx(:,:)
        type(c_ptr) :: node
        real(c_float), dimension(:,:), pointer :: v, normals, uv
        integer(c_int32_t), dimension(:), pointer :: indices
        integer :: nv, ni
        node = c_null_ptr
        nv = size(xyz, 2)
        ni = size(idx)
        if (nv < 3 .or. ni < 3) return
        node = vkf_mesh_create(int(nv, c_int), int(ni, c_int), real(rgba, c_float), c_null_ptr, c_null_ptr, 1_c_int)
        if (.not. c_associated(node)) return
        call mesh_arrays(node, nv, ni, v, normals, uv, indices)
        v = real(xyz, c_float)
        normals = 0.0_c_float
        normals(3, :) = 1.0_c_float
        uv = 0.0_c_float
        indices = int(reshape(idx, [ni]) - 1, c_int32_t)
    end function surface_node

    !> Points along a closed curve, as separate segments: pairs (p(i), p(i+1)).
    pure function closed_segments(p) result(s)
        real(wp), intent(in) :: p(:,:)
        real(wp) :: s(3, 2 * size(p, 2))
        integer :: i, n
        n = size(p, 2)
        do i = 1, n
            s(:, 2*i-1) = p(:, i)
            s(:, 2*i) = p(:, modulo(i, n) + 1)
        end do
    end function closed_segments

    !> Points along an open curve, as separate segments.
    pure function open_segments(p) result(s)
        real(wp), intent(in) :: p(:,:)
        real(wp) :: s(3, 2 * (size(p, 2) - 1))
        integer :: i
        do i = 1, size(p, 2) - 1
            s(:, 2*i-1) = p(:, i)
            s(:, 2*i) = p(:, i + 1)
        end do
    end function open_segments

    !> Append points b to a (allocatable, (3, n)).
    pure subroutine append(a, b)
        real(wp), allocatable, intent(inout) :: a(:,:)
        real(wp), intent(in) :: b(:,:)
        real(wp), allocatable :: tmp(:,:)
        if (.not. allocated(a)) allocate(a(3, 0))
        allocate(tmp(3, size(a, 2) + size(b, 2)))
        tmp(:, 1:size(a, 2)) = a
        tmp(:, size(a, 2)+1:) = b
        call move_alloc(tmp, a)
    end subroutine append

    subroutine check(status)
        integer(c_int), intent(in) :: status
        if (status /= 0) write(*, '(a)') 'vkframes: a rendering-layer call failed (shapes)'
    end subroutine check

    !> Color [r, g, b] (alpha unchanged) or [r, g, b, a] into rgba.
    pure subroutine set_rgba(rgba, color)
        real(wp), intent(inout) :: rgba(4)
        real(wp), intent(in) :: color(:)
        integer :: n
        n = min(4, size(color))
        rgba(1:n) = color(1:n)
    end subroutine set_rgba

    ! ==================================================================
    ! CoordinateAxes

    !> Coordinate axes named `name`, of color `color` ([r, g, b] or [r, g, b,
    !> a]), 1 long, with major ticks every 0.5 and minor every 0.25 (OpenFrames'
    !> defaults).
    function new_coordinate_axes(name, color) result(a)
        character(len=*), intent(in) :: name
        real(wp), intent(in), optional :: color(:)
        type(CoordinateAxes) :: a
        a%ReferenceFrame = ReferenceFrame(name, color)
        a%image_file = ''
        call place_axes(a, a%length_)
    end function new_coordinate_axes

    !> The frame's own axes (arrows) at `length` along x, y and z, half as long.
    subroutine place_axes(self, length)
        class(ReferenceFrame), intent(inout) :: self
        real(wp), intent(in) :: length
        call self%move_x_axis([length, 0.0_wp, 0.0_wp], 0.5_wp * length)
        call self%move_y_axis([0.0_wp, length, 0.0_wp], 0.5_wp * length)
        call self%move_z_axis([0.0_wp, 0.0_wp, length], 0.5_wp * length)
    end subroutine place_axes

    subroutine set_axis_length(self, length)
        class(CoordinateAxes), intent(inout) :: self
        real(wp), intent(in) :: length
        if (length < 0.0_wp) return
        self%length_ = length
        call place_axes(self, length)
        call make_axes(self)
    end subroutine set_axis_length

    pure real(wp) function axis_length(self)
        class(CoordinateAxes), intent(in) :: self
        axis_length = self%length_
    end function axis_length

    !> Which axes to draw: an ior of X_AXIS, Y_AXIS, Z_AXIS.
    subroutine set_draw_axes(self, axes)
        class(CoordinateAxes), intent(inout) :: self
        integer, intent(in) :: axes
        self%drawn = axes
        call make_axes(self)
    end subroutine set_draw_axes

    pure integer function draw_axes(self)
        class(CoordinateAxes), intent(in) :: self
        draw_axes = self%drawn
    end function draw_axes

    !> The axis lines' width (window units; default 1).
    subroutine set_axis_width(self, width)
        class(CoordinateAxes), intent(inout) :: self
        real(wp), intent(in) :: width
        if (width <= 0.0_wp) return
        self%width_ = width
        call make_axes(self)
    end subroutine set_axis_width

    pure real(wp) function axis_width(self)
        class(CoordinateAxes), intent(in) :: self
        axis_width = self%width_
    end function axis_width

    !> Major and minor tick spacings (>= 0; 0 for none).
    subroutine set_tick_spacing(self, major, minor)
        class(CoordinateAxes), intent(inout) :: self
        real(wp), intent(in) :: major, minor
        if (major >= 0.0_wp) self%major = major
        if (minor >= 0.0_wp) self%minor = minor
        call make_axes(self)
    end subroutine set_tick_spacing

    pure subroutine tick_spacing(self, major, minor)
        class(CoordinateAxes), intent(in) :: self
        real(wp), intent(out) :: major, minor
        major = self%major
        minor = self%minor
    end subroutine tick_spacing

    !> Major and minor tick sizes (window units; default 8 and 4).
    subroutine set_tick_size(self, major, minor)
        class(CoordinateAxes), intent(inout) :: self
        real(wp), intent(in) :: major, minor
        if (major > 0.0_wp) self%major_size = major
        if (minor > 0.0_wp) self%minor_size = minor
        call make_axes(self)
    end subroutine set_tick_size

    pure subroutine tick_size(self, major, minor)
        class(CoordinateAxes), intent(in) :: self
        real(wp), intent(out) :: major, minor
        major = self%major_size
        minor = self%minor_size
    end subroutine tick_size

    !> Tick shape: a MARKER_* shape (default MARKER_DISK; OpenFrames'
    !> setTickShader with its marker shaders).
    subroutine set_tick_shape(self, shape)
        class(CoordinateAxes), intent(inout) :: self
        integer, intent(in) :: shape
        self%shape_ = shape
        call make_axes(self)
    end subroutine set_tick_shape

    pure integer function tick_shape(self)
        class(CoordinateAxes), intent(in) :: self
        tick_shape = self%shape_
    end function tick_shape

    !> Draw the ticks as an image (OpenFrames' setTickImage; found with
    !> find_data_file; '' for disks again). False if it cannot be read.
    logical function set_tick_image(self, file) result(ok)
        class(CoordinateAxes), intent(inout) :: self
        character(len=*), intent(in) :: file
        character(len=:), allocatable :: path
        type(c_ptr) :: image
        ok = .true.
        image = c_null_ptr
        if (len_trim(file) > 0) then
            path = find_data_file(file)
            if (len(path) > 0) image = vkf_image_read(path, len(path, kind=c_int))
            ok = c_associated(image)
            if (.not. ok) return
        end if
        if (c_associated(self%image)) call vkf_image_delete(self%image)
        self%image = image
        self%image_file = trim(file)
        self%shape_ = merge(MARKER_IMAGE, MARKER_DISK, c_associated(image))
        call make_axes(self)
    end function set_tick_image

    function tick_image(self) result(file)
        class(CoordinateAxes), intent(in) :: self
        character(len=:), allocatable :: file
        file = self%image_file
    end function tick_image

    subroutine axes_set_color(self, color)
        class(CoordinateAxes), intent(inout) :: self
        real(wp), intent(in) :: color(:)
        call self%ReferenceFrame%set_color(color)
        call make_axes(self)
    end subroutine axes_set_color

    function axes_frame_info(self) result(info)
        class(CoordinateAxes), intent(in) :: self
        character(len=:), allocatable :: info
        info = 'CoordinateAxes'
    end function axes_frame_info

    subroutine axes_bound(self, center, radius)
        class(CoordinateAxes), intent(in) :: self
        real(wp), intent(out) :: center(3), radius
        center = 0.0_wp
        radius = self%length_
    end subroutine axes_bound

    subroutine axes_build_contents(self, viewer, group)
        class(CoordinateAxes), intent(inout) :: self
        type(c_ptr), intent(in) :: viewer, group
        self%nodes%contents = group
        call make_axes(self)
        self%nodes%viewer = viewer
    end subroutine axes_build_contents

    subroutine make_axes(self)
        class(CoordinateAxes), intent(inout) :: self
        real(wp), allocatable :: lines(:,:), major(:,:), minor(:,:)
        real(wp) :: e(3), mark, f
        type(c_ptr) :: group
        integer :: k, i
        if (.not. c_associated(self%nodes%contents)) return
        allocate(lines(3, 0), major(3, 0), minor(3, 0))
        do k = 1, 3
            if (iand(self%drawn, 2**(k-1)) == 0) cycle
            e = 0.0_wp
            e(k) = 1.0_wp
            call append(lines, reshape([0.0_wp, 0.0_wp, 0.0_wp, self%length_ * e], [3, 2]))
            if (self%major > 0.0_wp) then
                do i = 1, int(self%length_ / self%major + 1.0e-9_wp)
                    call append(major, reshape(i * self%major * e, [3, 1]))
                end do
            end if
            if (self%minor > 0.0_wp) then
                do i = 1, int(self%length_ / self%minor + 1.0e-9_wp)
                    mark = i * self%minor
                    if (self%major > 0.0_wp) then                   ! not where a major tick is
                        f = mark / self%major
                        if (abs(f - nint(f)) < 1.0e-6_wp) cycle
                    end if
                    call append(minor, reshape(mark * e, [3, 1]))
                end do
            end if
        end do
        group = vkf_switch_create()
        call add(group, line_node(lines, self%color(), self%width_, .true.))
        call add(group, marker_node(major, self%color(), self%major_size, self%shape_, self%length_, self%image))
        call add(group, marker_node(minor, self%color(), self%minor_size, self%shape_, self%length_, self%image))
        call self%nodes%replace(group)
    end subroutine make_axes

    ! ==================================================================
    ! LatLonGrid

    !> A latitude/longitude grid named `name`, of color `color`, on a unit
    !> sphere, every 30 degrees (OpenFrames' defaults).
    function new_lat_lon_grid(name, color) result(g)
        character(len=*), intent(in) :: name
        real(wp), intent(in), optional :: color(:)
        type(LatLonGrid) :: g
        g%ReferenceFrame = ReferenceFrame(name, color)
        call place_grid_axes(g)
    end function new_lat_lon_grid

    subroutine place_grid_axes(self)
        type(LatLonGrid), intent(inout) :: self
        call self%move_x_axis([self%radii_(1), 0.0_wp, 0.0_wp], 0.5_wp * self%radii_(1))
        call self%move_y_axis([0.0_wp, self%radii_(2), 0.0_wp], 0.5_wp * self%radii_(2))
        call self%move_z_axis([0.0_wp, 0.0_wp, self%radii_(3)], 0.5_wp * self%radii_(3))
    end subroutine place_grid_axes

    !> The ellipsoid's semi-axes (> 0; e.g. a Sphere's radii, a little larger
    !> to draw above its surface) and the line spacings (radians; 0 for no
    !> latitude or longitude lines), as OpenFrames' setParameters.
    subroutine grid_set_parameters(self, radius_x, radius_y, radius_z, lat_spacing, lon_spacing)
        class(LatLonGrid), intent(inout) :: self
        real(wp), intent(in) :: radius_x, radius_y, radius_z, lat_spacing, lon_spacing
        if (radius_x > 0.0_wp) self%radii_(1) = radius_x
        if (radius_y > 0.0_wp) self%radii_(2) = radius_y
        if (radius_z > 0.0_wp) self%radii_(3) = radius_z
        if (lat_spacing >= 0.0_wp) self%lat_spacing = lat_spacing
        if (lon_spacing >= 0.0_wp) self%lon_spacing = lon_spacing
        call place_grid_axes(self)
        call make_grid(self)
    end subroutine grid_set_parameters

    pure subroutine grid_parameters(self, radius_x, radius_y, radius_z, lat_spacing, lon_spacing)
        class(LatLonGrid), intent(in) :: self
        real(wp), intent(out) :: radius_x, radius_y, radius_z, lat_spacing, lon_spacing
        radius_x = self%radii_(1)
        radius_y = self%radii_(2)
        radius_z = self%radii_(3)
        lat_spacing = self%lat_spacing
        lon_spacing = self%lon_spacing
    end subroutine grid_parameters

    !> Widths of the grid lines and of the equator and prime meridian (window
    !> units; defaults 1 and 3).
    subroutine set_line_widths(self, width, main_width)
        class(LatLonGrid), intent(inout) :: self
        real(wp), intent(in) :: width, main_width
        if (width > 0.0_wp) self%width_ = width
        if (main_width > 0.0_wp) self%main_width = main_width
        call make_grid(self)
    end subroutine set_line_widths

    pure subroutine line_widths(self, width, main_width)
        class(LatLonGrid), intent(in) :: self
        real(wp), intent(out) :: width, main_width
        width = self%width_
        main_width = self%main_width
    end subroutine line_widths

    subroutine grid_set_color(self, color)
        class(LatLonGrid), intent(inout) :: self
        real(wp), intent(in) :: color(:)
        call self%ReferenceFrame%set_color(color)
        call make_grid(self)
    end subroutine grid_set_color

    function grid_frame_info(self) result(info)
        class(LatLonGrid), intent(in) :: self
        character(len=:), allocatable :: info
        info = 'LatLonGrid'
    end function grid_frame_info

    subroutine grid_bound(self, center, radius)
        class(LatLonGrid), intent(in) :: self
        real(wp), intent(out) :: center(3), radius
        center = 0.0_wp
        radius = maxval(self%radii_)
    end subroutine grid_bound

    subroutine grid_build_contents(self, viewer, group)
        class(LatLonGrid), intent(inout) :: self
        type(c_ptr), intent(in) :: viewer, group
        self%nodes%contents = group
        call make_grid(self)
        self%nodes%viewer = viewer
    end subroutine grid_build_contents

    !> The point at geodetic latitude lat and longitude lon on the ellipsoid
    !> (OpenFrames' LatLonGrid formula: the surface point whose normal has
    !> those angles).
    pure function surface_point(r, lat, lon) result(p)
        real(wp), intent(in) :: r(3), lat, lon
        real(wp) :: p(3), n(3), r2(3)
        n = [cos(lat) * cos(lon), cos(lat) * sin(lon), sin(lat)]
        r2 = r**2
        p = r2 * n / sqrt(dot_product(r2, n**2))
    end function surface_point

    subroutine make_grid(self)
        type(LatLonGrid), intent(inout) :: self
        integer, parameter :: n_around = 360, n_meridian = 360          ! 1 and 0.5 degree steps
        real(wp), allocatable :: grid(:,:), main(:,:)
        real(wp) :: circle(3, n_around), meridian(3, n_meridian + 1), lat, lon
        type(c_ptr) :: group
        integer :: i, k, sgn
        if (.not. c_associated(self%nodes%contents)) return
        allocate(grid(3, 0), main(3, 0))
        if (self%lat_spacing > 0.0_wp) then                    ! the equator, then pairs north and south
            k = 0
            do while (k * self%lat_spacing < 0.5_wp * pi - 1.0e-6_wp)
                do sgn = 1, merge(1, 2, k == 0)
                    lat = merge(1.0_wp, -1.0_wp, sgn == 1) * k * self%lat_spacing
                    do i = 1, n_around
                        circle(:, i) = surface_point(self%radii_, lat, 2.0_wp * pi * (i - 1) / n_around)
                    end do
                    if (k == 0) then
                        call append(main, closed_segments(circle))
                    else
                        call append(grid, closed_segments(circle))
                    end if
                end do
                k = k + 1
            end do
        end if
        if (self%lon_spacing > 0.0_wp) then                    ! the prime meridian, then the others
            k = 0
            do while (k * self%lon_spacing < 2.0_wp * pi - 1.0e-6_wp)
                lon = k * self%lon_spacing
                do i = 1, n_meridian + 1
                    meridian(:, i) = surface_point(self%radii_, -0.5_wp * pi + pi * (i - 1) / n_meridian, lon)
                end do
                if (k == 0) then
                    call append(main, open_segments(meridian))
                else
                    call append(grid, open_segments(meridian))
                end if
                k = k + 1
            end do
        end if
        group = vkf_switch_create()
        call add(group, line_node(grid, self%color(), self%width_, .true.))
        call add(group, line_node(main, self%color(), self%main_width, .true.))
        call self%nodes%replace(group)
    end subroutine make_grid

    ! ==================================================================
    ! RadialPlane

    !> A radial plane named `name` (decorations of color `color`): radius 1,
    !> circles every 0.5, radial lines every 30 degrees, light gray and
    !> transparent (OpenFrames' defaults).
    function new_radial_plane(name, color) result(p)
        character(len=*), intent(in) :: name
        real(wp), intent(in), optional :: color(:)
        type(RadialPlane) :: p
        p%ReferenceFrame = ReferenceFrame(name, color)
        call place_axes(p, p%radius_)
    end function new_radial_plane

    !> The plane's radius (> 0), circle spacing and radial line spacing
    !> (radians; 0 for none), as OpenFrames' setParameters.
    subroutine plane_set_parameters(self, radius, radial_spacing, lon_spacing)
        class(RadialPlane), intent(inout) :: self
        real(wp), intent(in) :: radius, radial_spacing, lon_spacing
        if (radius > 0.0_wp) self%radius_ = radius
        if (radial_spacing >= 0.0_wp) self%radial_spacing = radial_spacing
        if (lon_spacing >= 0.0_wp) self%lon_spacing = lon_spacing
        call place_axes(self, self%radius_)
        call make_plane(self)
    end subroutine plane_set_parameters

    pure subroutine plane_parameters(self, radius, radial_spacing, lon_spacing)
        class(RadialPlane), intent(in) :: self
        real(wp), intent(out) :: radius, radial_spacing, lon_spacing
        radius = self%radius_
        radial_spacing = self%radial_spacing
        lon_spacing = self%lon_spacing
    end subroutine plane_parameters

    !> The plane's color ([r, g, b] or [r, g, b, a]; default 0.8 gray, 0.2 opaque).
    subroutine set_plane_color(self, color)
        class(RadialPlane), intent(inout) :: self
        real(wp), intent(in) :: color(:)
        call set_rgba(self%plane_color_, color)
        call make_plane(self)
    end subroutine set_plane_color

    pure function plane_color(self) result(c)
        class(RadialPlane), intent(in) :: self
        real(wp) :: c(4)
        c = self%plane_color_
    end function plane_color

    !> The lines' color (default white, 0.2 opaque).
    subroutine plane_set_line_color(self, color)
        class(RadialPlane), intent(inout) :: self
        real(wp), intent(in) :: color(:)
        call set_rgba(self%line_color_, color)
        call make_plane(self)
    end subroutine plane_set_line_color

    pure function plane_line_color(self) result(c)
        class(RadialPlane), intent(in) :: self
        real(wp) :: c(4)
        c = self%line_color_
    end function plane_line_color

    function plane_frame_info(self) result(info)
        class(RadialPlane), intent(in) :: self
        character(len=:), allocatable :: info
        info = 'RadialPlane'
    end function plane_frame_info

    subroutine plane_bound(self, center, radius)
        class(RadialPlane), intent(in) :: self
        real(wp), intent(out) :: center(3), radius
        center = 0.0_wp
        radius = self%radius_
    end subroutine plane_bound

    subroutine plane_build_contents(self, viewer, group)
        class(RadialPlane), intent(inout) :: self
        type(c_ptr), intent(in) :: viewer, group
        self%nodes%contents = group
        call make_plane(self)
        self%nodes%viewer = viewer
    end subroutine plane_build_contents

    subroutine make_plane(self)
        type(RadialPlane), intent(inout) :: self
        integer, parameter :: n = 360                          ! points around a circle (1 degree)
        real(wp) :: disk(3, n + 1), circle(3, n), a, r
        integer :: idx(3, n), i, k
        real(wp), allocatable :: lines(:,:), main(:,:)
        type(c_ptr) :: group
        if (.not. c_associated(self%nodes%contents)) return
        ! the disk: a fan around the center
        disk(:, n + 1) = 0.0_wp
        do i = 1, n
            a = 2.0_wp * pi * (i - 1) / n
            disk(:, i) = self%radius_ * [cos(a), sin(a), 0.0_wp]
            idx(:, i) = [n + 1, i, modulo(i, n) + 1]
        end do
        allocate(lines(3, 0), main(3, 0))
        ! circles: the edge, and every radial_spacing from the center
        if (self%radial_spacing > 0.0_wp) then
            call append(lines, closed_segments(disk(:, 1:n)))
            k = 1
            do while (k * self%radial_spacing < self%radius_ * (1.0_wp - 1.0e-6_wp))
                r = k * self%radial_spacing
                circle = r / self%radius_ * disk(:, 1:n)
                call append(lines, closed_segments(circle))
                k = k + 1
            end do
        end if
        ! radial lines: along +x (thicker), then every lon_spacing
        if (self%lon_spacing > 0.0_wp) then
            k = 0
            do while (k * self%lon_spacing < 2.0_wp * pi - 1.0e-6_wp)
                a = k * self%lon_spacing
                if (k == 0) then
                    call append(main, reshape([0.0_wp, 0.0_wp, 0.0_wp, self%radius_ * [cos(a), sin(a), 0.0_wp]], [3, 2]))
                else
                    call append(lines, reshape([0.0_wp, 0.0_wp, 0.0_wp, self%radius_ * [cos(a), sin(a), 0.0_wp]], [3, 2]))
                end if
                k = k + 1
            end do
        end if
        group = vkf_switch_create()
        call add(group, surface_node(disk, idx, self%plane_color_))
        call add(group, line_node(lines, self%line_color_, 1.0_wp, .true.))
        call add(group, line_node(main, self%line_color_, 3.0_wp, .true.))
        call self%nodes%replace(group)
    end subroutine make_plane

    ! ==================================================================
    ! PolyhedralCone

    !> A polyhedral cone named `name`, of color `color` (its sides and lines):
    !> 3 vertices at clock angles 0, 120 and 240 degrees, cone angles 45
    !> degrees, 1 long (OpenFrames' defaults).
    function new_polyhedral_cone(name, color) result(c)
        character(len=*), intent(in) :: name
        real(wp), intent(in), optional :: color(:)
        type(PolyhedralCone) :: c
        c%ReferenceFrame = ReferenceFrame(name, color)
        c%cone_color_ = c%color()
        c%line_color_ = c%color()
        c%clock = [0.0_wp, 120.0_wp, 240.0_wp] * pi / 180.0_wp
        c%cone = [45.0_wp, 45.0_wp, 45.0_wp] * pi / 180.0_wp
    end function new_polyhedral_cone

    !> The vertices: clock angles (from +x toward +y) and cone angles (from the
    !> boresight, -z; below 90 degrees), radians, the same number of each (>= 3).
    subroutine set_vertex_angles(self, clock, cone)
        class(PolyhedralCone), intent(inout) :: self
        real(wp), intent(in) :: clock(:), cone(:)
        if (size(clock) /= size(cone) .or. size(clock) < 3) return
        self%clock = clock
        self%cone = cone
        call make_cone(self)
    end subroutine set_vertex_angles

    pure function clock_angles(self) result(a)
        class(PolyhedralCone), intent(in) :: self
        real(wp), allocatable :: a(:)
        a = self%clock
    end function clock_angles

    pure function cone_angles(self) result(a)
        class(PolyhedralCone), intent(in) :: self
        real(wp), allocatable :: a(:)
        a = self%cone
    end function cone_angles

    !> The cone's length along the boresight (> 0; default 1).
    subroutine set_cone_length(self, length)
        class(PolyhedralCone), intent(inout) :: self
        real(wp), intent(in) :: length
        if (length <= 0.0_wp) return
        self%length_ = length
        call make_cone(self)
    end subroutine set_cone_length

    pure real(wp) function cone_length(self)
        class(PolyhedralCone), intent(in) :: self
        cone_length = self%length_
    end function cone_length

    !> The sides' color ([r, g, b] or [r, g, b, a]; their alpha makes them
    !> transparent).
    subroutine set_cone_color(self, color)
        class(PolyhedralCone), intent(inout) :: self
        real(wp), intent(in) :: color(:)
        call set_rgba(self%cone_color_, color)
        call make_cone(self)
    end subroutine set_cone_color

    pure function cone_color(self) result(c)
        class(PolyhedralCone), intent(in) :: self
        real(wp) :: c(4)
        c = self%cone_color_
    end function cone_color

    !> The edges' and base outline's color.
    subroutine cone_set_line_color(self, color)
        class(PolyhedralCone), intent(inout) :: self
        real(wp), intent(in) :: color(:)
        call set_rgba(self%line_color_, color)
        call make_cone(self)
    end subroutine cone_set_line_color

    pure function cone_line_color(self) result(c)
        class(PolyhedralCone), intent(in) :: self
        real(wp) :: c(4)
        c = self%line_color_
    end function cone_line_color

    !> What to draw: an ior of CONE_SIDES, CONE_EDGES, CONE_BASE_OUTLINE.
    subroutine set_draw_mode(self, mode)
        class(PolyhedralCone), intent(inout) :: self
        integer, intent(in) :: mode
        self%mode = mode
        call make_cone(self)
    end subroutine set_draw_mode

    pure integer function draw_mode(self)
        class(PolyhedralCone), intent(in) :: self
        draw_mode = self%mode
    end function draw_mode

    !> Place the cone's apex at `apex` (in the parent frame) with its boresight
    !> along `dir` and its +y axis toward `up` (OpenFrames' makeConeLookAt).
    subroutine make_cone_look_at(self, apex, dir, up)
        class(PolyhedralCone), intent(inout) :: self
        real(wp), intent(in) :: apex(3), dir(3), up(3)
        real(wp) :: f(3), s(3), u(3), m(3,3)
        if (norm2(dir) <= 0.0_wp) return
        f = dir / norm2(dir)
        s = cross(f, up)
        if (norm2(s) <= 1.0e-12_wp) s = cross(f, [1.0_wp, 0.0_wp, 0.0_wp])
        if (norm2(s) <= 1.0e-12_wp) s = cross(f, [0.0_wp, 1.0_wp, 0.0_wp])
        s = s / norm2(s)
        u = cross(s, f)
        m(:, 1) = s                                             ! the cone's axes in the parent frame
        m(:, 2) = u
        m(:, 3) = -f
        call self%set_position(apex)
        call self%set_attitude(quat_from_matrix(m))
    end subroutine make_cone_look_at

    !> Whether `point` (in the cone's frame) is inside the cone, between
    !> distances `min_distance` and `max_distance` along the boresight
    !> (defaults 0 and unlimited): inside the polygon of the vertices' directions
    !> (OpenFrames' isVisible; its PolyhedralCone always answers false).
    logical function cone_is_visible(self, point, min_distance, max_distance) result(visible)
        class(PolyhedralCone), intent(in) :: self
        real(wp), intent(in) :: point(3)
        real(wp), intent(in), optional :: min_distance, max_distance
        real(wp) :: q(2), a(2), b(2)
        integer :: i, n, winding
        visible = in_range(point, min_distance, max_distance)
        if (.not. visible .or. norm2(point) == 0.0_wp) return
        q = point(1:2) / (-point(3))                            ! on the plane z = -1
        n = size(self%clock)
        winding = 0                                             ! winding number of the polygon around q
        do i = 1, n
            a = tan(self%cone(i)) * [cos(self%clock(i)), sin(self%clock(i))]
            b = tan(self%cone(modulo(i, n) + 1)) * [cos(self%clock(modulo(i, n) + 1)), sin(self%clock(modulo(i, n) + 1))]
            if (a(2) <= q(2)) then
                if (b(2) > q(2) .and. side(a, b, q) > 0.0_wp) winding = winding + 1
            else
                if (b(2) <= q(2) .and. side(a, b, q) < 0.0_wp) winding = winding - 1
            end if
        end do
        visible = winding /= 0
    end function cone_is_visible

    !> Along the boresight (-z) between the distances (a point ahead of the apex).
    pure logical function in_range(point, min_distance, max_distance)
        real(wp), intent(in) :: point(3)
        real(wp), intent(in), optional :: min_distance, max_distance
        real(wp) :: d
        d = -point(3)
        in_range = d >= 0.0_wp
        if (present(min_distance)) in_range = in_range .and. d >= min_distance
        if (present(max_distance)) in_range = in_range .and. d <= max_distance
    end function in_range

    pure real(wp) function side(a, b, q)
        real(wp), intent(in) :: a(2), b(2), q(2)
        side = (b(1) - a(1)) * (q(2) - a(2)) - (q(1) - a(1)) * (b(2) - a(2))
    end function side

    pure function cross(a, b) result(c)
        real(wp), intent(in) :: a(3), b(3)
        real(wp) :: c(3)
        c = [a(2) * b(3) - a(3) * b(2), a(3) * b(1) - a(1) * b(3), a(1) * b(2) - a(2) * b(1)]
    end function cross

    function cone_frame_info(self) result(info)
        class(PolyhedralCone), intent(in) :: self
        character(len=:), allocatable :: info
        info = 'PolyhedralCone'
    end function cone_frame_info

    !> The bound of the apex and the base.
    subroutine cone_bound(self, center, radius)
        class(PolyhedralCone), intent(in) :: self
        real(wp), intent(out) :: center(3), radius
        real(wp) :: r
        r = self%length_ * maxval(tan(self%cone))
        center = [0.0_wp, 0.0_wp, -0.5_wp * self%length_]
        radius = sqrt((0.5_wp * self%length_)**2 + r**2)
    end subroutine cone_bound

    subroutine cone_build_contents(self, viewer, group)
        class(PolyhedralCone), intent(inout) :: self
        type(c_ptr), intent(in) :: viewer, group
        self%nodes%contents = group
        call make_cone(self)
        self%nodes%viewer = viewer
    end subroutine cone_build_contents

    subroutine make_cone(self)
        class(PolyhedralCone), intent(inout) :: self
        real(wp), allocatable :: v(:,:), edges(:,:)
        integer, allocatable :: idx(:,:)
        type(c_ptr) :: group
        integer :: i, n
        if (.not. c_associated(self%nodes%contents)) return
        n = size(self%clock)
        allocate(v(3, n + 1), idx(3, n), edges(3, 2 * n))
        v(:, n + 1) = 0.0_wp                                    ! the apex
        do i = 1, n
            v(:, i) = self%length_ * [tan(self%cone(i)) * cos(self%clock(i)), tan(self%cone(i)) * sin(self%clock(i)), -1.0_wp]
            idx(:, i) = [n + 1, i, modulo(i, n) + 1]
            edges(:, 2*i-1) = 0.0_wp
            edges(:, 2*i) = v(:, i)
        end do
        group = vkf_switch_create()
        if (iand(self%mode, CONE_SIDES) /= 0) call add(group, surface_node(v, idx, self%cone_color_))
        if (iand(self%mode, CONE_EDGES) /= 0) call add(group, line_node(edges, self%line_color_, self%width_, .true.))
        if (iand(self%mode, CONE_BASE_OUTLINE) /= 0) &
            call add(group, line_node(closed_segments(v(:, 1:n)), self%line_color_, self%width_, .true.))
        call self%nodes%replace(group)
    end subroutine make_cone

    ! ==================================================================
    ! EllipticCone and RectangularCone

    !> An elliptic cone named `name`, of color `color` (sides and base outline drawn).
    function new_elliptic_cone(name, color) result(c)
        character(len=*), intent(in) :: name
        real(wp), intent(in), optional :: color(:)
        type(EllipticCone) :: c
        c%PolyhedralCone = PolyhedralCone(name, color)
        c%mode = ior(CONE_SIDES, CONE_BASE_OUTLINE)
        call elliptic_vertices(c)
    end function new_elliptic_cone

    !> The half-angles along x (a) and y (b), radians (0 to 90 degrees).
    subroutine elliptic_set_primary_angles(self, a, b)
        class(EllipticCone), intent(inout) :: self
        real(wp), intent(in) :: a, b
        self%a = a
        self%b = b
        call elliptic_vertices(self)
        call make_cone(self)
    end subroutine elliptic_set_primary_angles

    pure subroutine elliptic_primary_angles(self, a, b)
        class(EllipticCone), intent(in) :: self
        real(wp), intent(out) :: a, b
        a = self%a
        b = self%b
    end subroutine elliptic_primary_angles

    !> 120 vertices (every 3 degrees), cone angle a b / sqrt(b^2 cos^2 + a^2 sin^2)
    !> (OpenFrames' createEllipticCone).
    subroutine elliptic_vertices(self)
        type(EllipticCone), intent(inout) :: self
        integer, parameter :: n = 120
        integer :: i
        real(wp) :: t
        if (allocated(self%clock)) deallocate(self%clock, self%cone)
        allocate(self%clock(n), self%cone(n))
        do i = 1, n
            t = 2.0_wp * pi * (i - 1) / n
            self%clock(i) = t
            self%cone(i) = self%a * self%b / sqrt(self%b**2 * cos(t)**2 + self%a**2 * sin(t)**2)
        end do
    end subroutine elliptic_vertices

    !> Inside the elliptic cone (exactly; OpenFrames' EllipticCone::isVisible).
    logical function elliptic_is_visible(self, point, min_distance, max_distance) result(visible)
        class(EllipticCone), intent(in) :: self
        real(wp), intent(in) :: point(3)
        real(wp), intent(in), optional :: min_distance, max_distance
        real(wp) :: p(3), cone, clock
        visible = in_range(point, min_distance, max_distance)
        if (.not. visible .or. norm2(point) == 0.0_wp) return
        p = point / (-point(3))
        cone = atan(norm2(p(1:2)))
        clock = atan2(p(2), p(1))
        visible = cone <= self%a * self%b / sqrt(self%b**2 * cos(clock)**2 + self%a**2 * sin(clock)**2)
    end function elliptic_is_visible

    function elliptic_frame_info(self) result(info)
        class(EllipticCone), intent(in) :: self
        character(len=:), allocatable :: info
        info = 'EllipticCone'
    end function elliptic_frame_info

    !> A rectangular cone named `name`, of color `color`.
    function new_rectangular_cone(name, color) result(c)
        character(len=*), intent(in) :: name
        real(wp), intent(in), optional :: color(:)
        type(RectangularCone) :: c
        c%PolyhedralCone = PolyhedralCone(name, color)
        call rect_vertices(c)
    end function new_rectangular_cone

    !> The half-angles along x and y, radians (0 to 90 degrees).
    subroutine rect_set_primary_angles(self, x, y)
        class(RectangularCone), intent(inout) :: self
        real(wp), intent(in) :: x, y
        self%x = x
        self%y = y
        call rect_vertices(self)
        call make_cone(self)
    end subroutine rect_set_primary_angles

    pure subroutine rect_primary_angles(self, x, y)
        class(RectangularCone), intent(in) :: self
        real(wp), intent(out) :: x, y
        x = self%x
        y = self%y
    end subroutine rect_primary_angles

    !> The 4 corners (OpenFrames' createRectangularCone).
    subroutine rect_vertices(self)
        type(RectangularCone), intent(inout) :: self
        real(wp) :: xl, yl, cone, clock
        xl = tan(self%x)
        yl = tan(self%y)
        cone = atan(sqrt(xl**2 + yl**2))
        clock = atan2(yl, xl)
        self%clock = [clock, pi - clock, pi + clock, -clock]
        self%cone = [cone, cone, cone, cone]
    end subroutine rect_vertices

    function rect_frame_info(self) result(info)
        class(RectangularCone), intent(in) :: self
        character(len=:), allocatable :: info
        info = 'RectangularCone'
    end function rect_frame_info

end module vkf_shapes
