!> Reference shapes, as OpenFrames' frames of the same names: CoordinateAxes
!> (axis lines with tick marks), LatLonGrid (latitude and longitude lines on
!> an ellipsoid), RadialPlane (a transparent disk with circles and radial
!> lines) and cones (PolyhedralCone, and its EllipticCone and RectangularCone:
!> e.g. sensor fields of view); and (not in OpenFrames) Polyhedron, any
!> surface of polygons or triangles, or a grid of points (e.g. a tube of
!> trajectories), fixed in its frame, lit or not, opaque or translucent.
!>
!> Each is a frame drawing in its own coordinates. Its drawing is made when a
!> window first shows it and made again when a setting changes (also while
!> shown; the new nodes are compiled and swapped in). Lines are vkframes'
!> double-precision lines (widths in window units); tick marks are markers
!> (MarkerArtist's shapes); surfaces are unlit and transparent, seen from both
!> sides, and write no depth (as OpenFrames'), except a Polyhedron's when lit.
!> Surfaces are made by one routine (surface_node), as their lines
!> (line_node) and points (marker_node).
module vkf_shapes

    use, intrinsic :: iso_c_binding, only: c_ptr, c_null_ptr, c_associated, c_int, c_int32_t, c_float, &
                                           c_double, c_f_pointer
    use, intrinsic :: iso_fortran_env, only: int64
    use vkf_math, only: wp, set_rgba, pi, quat_from_matrix, cross
    use vkf_frames, only: ReferenceFrame, X_AXIS, Y_AXIS, Z_AXIS, ALL_AXES
    use vkf_artists, only: MARKER_DISK, MARKER_IMAGE
    use vkf_files, only: find_data_file
    use vkf_layer, only: check_status, vkf_line_create, vkf_marker_create, vkf_mesh_create, vkf_switch_create, &
                         vkf_mesh_array, vkf_mesh_set_bound, &
                         vkf_node_add_child, vkf_node_remove_child, vkf_node_delete, vkf_compile_node, &
                         vkf_image_read, vkf_image_delete, line_set_points, line_set_style, line_set_segments, &
                         marker_set_style, mesh_arrays

    implicit none

    private

    !> What a cone draws (OpenFrames' PolyhedralCone::DrawMode; combine with ior).
    integer, parameter, public :: CONE_NONE = 0, CONE_SIDES = 1, CONE_EDGES = 2, CONE_BASE_OUTLINE = 4
    integer, parameter, public :: CONE_ALL = 7              !! CONE_SIDES + CONE_EDGES + CONE_BASE_OUTLINE

    !> What a Polyhedron draws (combine with ior).
    integer, parameter, public :: POLY_NONE = 0, POLY_FACES = 1, POLY_EDGES = 2, POLY_VERTICES = 4
    integer, parameter, public :: POLY_ALL = 7              !! POLY_FACES + POLY_EDGES + POLY_VERTICES

    !> A shape's nodes: made under the frame's contents, replaced when the
    !> shape changes.
    type :: ShapeNodes
        type(c_ptr) :: contents = c_null_ptr                !! the frame's contents group (null until built)
        logical :: shown = .false.                          !! built for a window: rebuilt nodes are compiled
        type(c_ptr) :: shape = c_null_ptr                   !! the switch now shown (null: none)
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
        real(wp) :: length_ = 1.0_wp                        !! axis length (scene units; default 1)
        integer :: drawn = ALL_AXES                         !! axes drawn: an ior of X_AXIS, Y_AXIS, Z_AXIS
        real(wp) :: width_ = 1.0_wp                         !! axis lines' width (window units; default 1)
        real(wp) :: major = 0.5_wp                          !! major tick spacing (scene units; 0: none)
        real(wp) :: minor = 0.25_wp                         !! minor tick spacing (scene units; 0: none)
        real(wp) :: major_size = 8.0_wp                     !! major tick size (window units)
        real(wp) :: minor_size = 4.0_wp                     !! minor tick size (window units)
        integer :: shape_ = MARKER_DISK                     !! tick shape: a MARKER_* shape
        character(len=:), allocatable :: image_file         !! tick image file ('' for none)
        type(c_ptr) :: image = c_null_ptr                   !! tick image handle (null: none)
        type(ShapeNodes) :: nodes                           !! the drawing
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

    !> The CoordinateAxes constructor: `CoordinateAxes(...)` makes one (see `new_coordinate_axes` for its
    !> arguments).
    interface CoordinateAxes
        module procedure new_coordinate_axes
    end interface CoordinateAxes

    !> Latitude and longitude lines on an ellipsoid (OpenFrames' LatLonGrid):
    !> at geodetic latitudes (matching a Sphere's texture), every `lat_spacing`
    !> from the equator and every `lon_spacing` from the prime meridian (the
    !> equator and prime meridian thicker), in the frame's color.
    type, extends(ReferenceFrame), public :: LatLonGrid
        private
        real(wp) :: radii_(3) = 1.0_wp                      !! the ellipsoid's semi-axes (scene units)
        real(wp) :: lat_spacing = 30.0_wp * pi / 180.0_wp   !! between latitude lines (radians; 0: none)
        real(wp) :: lon_spacing = 30.0_wp * pi / 180.0_wp   !! between meridians (radians; 0: none)
        real(wp) :: width_ = 1.0_wp                         !! grid lines' width (window units)
        real(wp) :: main_width = 3.0_wp                     !! equator's and prime meridian's width (window units)
        type(ShapeNodes) :: nodes                           !! the drawing
    contains
        procedure :: set_parameters => grid_set_parameters, parameters => grid_parameters
        procedure :: set_line_widths, line_widths
        procedure :: set_color => grid_set_color
        procedure :: frame_info => grid_frame_info
        procedure :: bound => grid_bound
        procedure :: build_contents => grid_build_contents
    end type LatLonGrid

    !> The LatLonGrid constructor: `LatLonGrid(...)` makes one (see `new_lat_lon_grid` for its
    !> arguments).
    interface LatLonGrid
        module procedure new_lat_lon_grid
    end interface LatLonGrid

    !> A plane disk in the xy plane (OpenFrames' RadialPlane): transparent, with
    !> circles every `radial_spacing` and radial lines every `lon_spacing` (the
    !> one along +x thicker).
    type, extends(ReferenceFrame), public :: RadialPlane
        private
        real(wp) :: radius_ = 1.0_wp                        !! the disk's radius (scene units)
        real(wp) :: radial_spacing = 0.5_wp                 !! between circles (scene units; 0: none)
        real(wp) :: lon_spacing = 30.0_wp * pi / 180.0_wp   !! between radial lines (radians; 0: none)
        real(wp) :: plane_color_(4) = [0.8_wp, 0.8_wp, 0.8_wp, 0.2_wp]  !! the disk's color [r, g, b, a]
        real(wp) :: line_color_(4) = [1.0_wp, 1.0_wp, 1.0_wp, 0.2_wp]  !! circles' and radial lines' color [r, g, b, a]
        type(ShapeNodes) :: nodes                           !! the drawing
    contains
        procedure :: set_parameters => plane_set_parameters, parameters => plane_parameters
        procedure :: set_plane_color, plane_color
        procedure :: set_line_color => plane_set_line_color, line_color => plane_line_color
        procedure :: frame_info => plane_frame_info
        procedure :: bound => plane_bound
        procedure :: build_contents => plane_build_contents
    end type RadialPlane

    !> The RadialPlane constructor: `RadialPlane(...)` makes one (see `new_radial_plane` for its
    !> arguments).
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
        real(wp), allocatable :: clock(:)                   !! vertices' clock angles (radians, from +x toward +y)
        real(wp), allocatable :: cone(:)                    !! vertices' cone angles (radians, from the boresight)
        real(wp) :: length_ = 1.0_wp                        !! length along the boresight (scene units)
        real(wp) :: cone_color_(4) = 1.0_wp                 !! sides' color [r, g, b, a]
        real(wp) :: line_color_(4) = 1.0_wp                 !! edges' and base outline's color [r, g, b, a]
        integer :: mode = CONE_ALL                          !! an ior of CONE_SIDES, CONE_EDGES, CONE_BASE_OUTLINE
        real(wp) :: width_ = 1.0_wp                         !! edges' and base outline's width (window units)
        type(ShapeNodes) :: nodes                           !! the drawing
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

    !> The PolyhedralCone constructor: `PolyhedralCone(...)` makes one (see `new_polyhedral_cone` for its
    !> arguments).
    interface PolyhedralCone
        module procedure new_polyhedral_cone
    end interface PolyhedralCone

    !> An elliptic cone (OpenFrames' EllipticCone): half-angles a along x and b
    !> along y (`set_primary_angles`; default 60 and 30 degrees).
    type, extends(PolyhedralCone), public :: EllipticCone
        private
        real(wp) :: a = 60.0_wp * pi / 180.0_wp             !! half-angle along x (radians)
        real(wp) :: b = 30.0_wp * pi / 180.0_wp             !! half-angle along y (radians)
    contains
        procedure :: set_primary_angles => elliptic_set_primary_angles
        procedure :: primary_angles => elliptic_primary_angles
        procedure :: is_visible => elliptic_is_visible
        procedure :: frame_info => elliptic_frame_info
    end type EllipticCone

    !> The EllipticCone constructor: `EllipticCone(...)` makes one (see `new_elliptic_cone` for its
    !> arguments).
    interface EllipticCone
        module procedure new_elliptic_cone
    end interface EllipticCone

    !> A rectangular cone (OpenFrames' RectangularCone): half-angles x and y
    !> (`set_primary_angles`; default 60 and 30 degrees).
    type, extends(PolyhedralCone), public :: RectangularCone
        private
        real(wp) :: x = 60.0_wp * pi / 180.0_wp             !! half-angle along x (radians)
        real(wp) :: y = 30.0_wp * pi / 180.0_wp             !! half-angle along y (radians)
    contains
        procedure :: set_primary_angles => rect_set_primary_angles
        procedure :: primary_angles => rect_primary_angles
        procedure :: frame_info => rect_frame_info
    end type RectangularCone

    !> The RectangularCone constructor: `RectangularCone(...)` makes one (see `new_rectangular_cone` for its
    !> arguments).
    interface RectangularCone
        module procedure new_rectangular_cone
    end interface RectangularCone

    !> A surface fixed in its frame (not in OpenFrames): vertices and faces,
    !> given as polygons (set_faces: e.g. a polyhedron's), triangles
    !> (set_triangles) or a grid of points (set_grid: e.g. a tube of
    !> trajectories, an invariant manifold). Drawn as faces, edges (the
    !> polygons' outlines, the triangles' sides, or the grid's lines) and
    !> vertices (`set_draw_mode`).
    !>
    !> Faces: lit by the scene's lights (the default; flat, or smooth across
    !> the vertices with set_smooth) or unlit (as a cone's sides); in the
    !> face color, or a color per vertex (set_vertex_colors); opaque, or
    !> translucent (set_opacity, or the colors' alpha: drawn after the opaque
    !> scene, sorted by depth, writing none); seen from both sides (the
    !> default, for open surfaces; set_two_sided(.false.) for a closed one,
    !> whose back faces are then not drawn through it).
    !>
    !> Vertices are single precision on the GPU, relative to the frame: for
    !> a shape far from the frame's origin, put it in a frame placed near it.
    type, extends(ReferenceFrame), public :: Polyhedron
        private
        real(wp), allocatable :: v(:,:)                     !! vertices (3, n), frame coordinates
        integer, allocatable :: tri(:,:)                    !! triangles (3, m), vertex numbers (1-based)
        integer, allocatable :: edge(:,:)                   !! edges (2, k), vertex numbers
        real(wp), allocatable :: vertex_colors_(:,:)        !! a color per vertex (4, n) (none: the face color)
        real(wp) :: face_color_(4) = 1.0_wp                 !! faces' color [r, g, b, a]
        real(wp) :: line_color_(4) = 1.0_wp                 !! edges' color
        real(wp) :: point_color_(4) = 1.0_wp                !! vertices' color
        real(wp) :: opacity_ = 1.0_wp                       !! the faces' (times their colors' alpha)
        real(wp) :: width_ = 1.0_wp                         !! edges' width (window units)
        real(wp) :: point_size_ = 6.0_wp                    !! vertices' size (window units)
        integer :: mode = ior(POLY_FACES, POLY_EDGES)       !! an ior of POLY_FACES, POLY_EDGES, POLY_VERTICES
        logical :: lit_ = .true.                            !! lit by the scene's lights (else unlit, transparent)
        logical :: smooth_ = .false.                        !! normals smooth across the vertices (else flat faces)
        logical :: two_sided_ = .true.                      !! both sides drawn (and lit)
        type(ShapeNodes) :: nodes                           !! the drawing
    contains
        procedure :: set_faces, set_triangles, set_grid, set_edges, set_vertices
        procedure :: num_vertices, num_triangles, num_edges
        procedure :: set_vertex_colors, remove_vertex_colors, has_vertex_colors
        procedure :: set_face_color, face_color
        procedure :: set_line_color => poly_set_line_color, line_color => poly_line_color
        procedure :: set_point_color, point_color
        procedure :: set_opacity => poly_set_opacity, opacity => poly_opacity
        procedure :: set_line_width => poly_set_line_width, line_width => poly_line_width
        procedure :: set_point_size, point_size
        procedure :: set_draw_mode => poly_set_draw_mode, draw_mode => poly_draw_mode
        procedure :: set_lit, lit, set_smooth, smooth, set_two_sided, two_sided
        procedure :: frame_info => poly_frame_info
        procedure :: bound => poly_bound
        procedure :: build_contents => poly_build_contents
    end type Polyhedron

    !> The Polyhedron constructor: `Polyhedron(...)` makes one (see `new_polyhedron` for its
    !> arguments).
    interface Polyhedron
        module procedure new_polyhedron
    end interface Polyhedron

contains

    ! ==================================================================
    ! nodes

    !> Show `new` (a switch made by the shape) in place of the shape's current
    !> nodes; compiled first if the window already runs.
    subroutine replace(self, new)
        class(ShapeNodes), intent(inout) :: self
        type(c_ptr), intent(in) :: new                      !! a switch made by the shape (null: none)
        if (.not. c_associated(self%contents)) then          ! not shown yet
            if (c_associated(new)) call vkf_node_delete(new)
            return
        end if
        if (c_associated(self%shape)) then
            call check_status(vkf_node_remove_child(self%contents, self%shape), 'shapes')
            call vkf_node_delete(self%shape)
        end if
        self%shape = new
        if (.not. c_associated(new)) return
        if (self%shown) call check_status(vkf_compile_node(new), 'shapes')
        call check_status(vkf_node_add_child(self%contents, new), 'shapes')
    end subroutine replace

    !> Add `child` to `group` (its handle is not kept).
    subroutine add(group, child)
        type(c_ptr), intent(in) :: group, child             !! the group, and a node to add to it
        if (.not. c_associated(child)) return
        call check_status(vkf_node_add_child(group, child), 'shapes')
        call vkf_node_delete(child)
    end subroutine add

    !> A line node through points xyz(3, n): a strip, or separate segments
    !> (points 1-2, 3-4, ...).
    function line_node(xyz, rgba, width, segments) result(node)
        real(wp), intent(in) :: xyz(:,:)
        real(wp), intent(in) :: rgba(4)                     !! color [r, g, b, a] (0 to 1)
        real(wp), intent(in) :: width                       !! line width (window units)
        logical, intent(in) :: segments                     !! separate segments (else a strip)
        type(c_ptr) :: node
        node = c_null_ptr
        if (size(xyz, 2) < 2) return
        node = vkf_line_create(int(size(xyz, 2), c_int))
        if (.not. c_associated(node)) return
        if (segments) call check_status(line_set_segments(node, .true.), 'shapes')
        call check_status(line_set_style(node, real(rgba, c_float), real(width, c_float)), 'shapes')
        call check_status(line_set_points(node, real(xyz, c_double)), 'shapes')
    end function line_node

    !> A marker node at points xyz(3, n), `px` window units (MarkerArtist's
    !> shapes and attenuation).
    function marker_node(xyz, rgba, px, shape, radius, image) result(node)
        real(wp), intent(in) :: xyz(:,:)
        real(wp), intent(in) :: rgba(4)                     !! color [r, g, b, a] (0 to 1)
        real(wp), intent(in) :: px                          !! marker size (window units)
        real(wp), intent(in) :: radius                      !! markers shrink beyond this camera distance (scene units)
        integer, intent(in) :: shape                        !! a MARKER_* shape
        type(c_ptr), intent(in) :: image                    !! image for MARKER_IMAGE (null: none)
        type(c_ptr) :: node
        node = c_null_ptr
        if (size(xyz, 2) < 1) return
        node = vkf_marker_create(int(size(xyz, 2), c_int), image, 0_c_int)
        if (.not. c_associated(node)) return
        call check_status(marker_set_style(node, real(rgba, c_float), real(px, c_float), shape, real(radius, c_float)), 'shapes')
        call check_status(line_set_points(node, real(xyz, c_double)), 'shapes')
    end function marker_node

    !> A surface of triangles idx(3, m) (1-based) through points xyz(3, n), of
    !> color rgba: unlit and transparent (seen from both sides, writing no
    !> depth: planes, cones' sides), or, given `normals`, lit by the scene's
    !> lights: opaque, or translucent if an alpha is below 1 (drawn after the
    !> opaque scene, sorted by depth by the bounding sphere `sphere`);
    !> `two_sided`: both faces drawn and lit. `colors`: a color per vertex
    !> (4, n), times rgba.
    function surface_node(xyz, idx, rgba, normals, colors, two_sided, sphere) result(node)
        real(wp), intent(in) :: xyz(:,:)
        integer, intent(in) :: idx(:,:)
        real(wp), intent(in) :: rgba(4)                     !! color [r, g, b, a] (0 to 1)
        real(wp), intent(in), optional :: normals(:,:)      !! (3, n): lit
        real(wp), intent(in), optional :: colors(:,:)       !! (4, n)
        logical, intent(in), optional :: two_sided          !! (lit) both faces (default: front only)
        real(wp), intent(in), optional :: sphere(4)         !! (lit, translucent) center and radius
        type(c_ptr) :: node
        real(c_float), dimension(:,:), pointer :: v, n, uv, c
        integer(c_int32_t), dimension(:), pointer :: indices
        integer :: nv, ni, flags, i
        logical :: lit, translucent
        node = c_null_ptr
        nv = size(xyz, 2)
        ni = size(idx)
        if (nv < 3 .or. ni < 3) return
        lit = present(normals)
        flags = 1
        if (lit) then
            translucent = rgba(4) < 1.0_wp
            if (present(colors)) translucent = translucent .or. any(colors(4, :) < 1.0_wp)
            flags = 0
            if (translucent) flags = flags + 8
            if (present(two_sided)) then
                if (two_sided) flags = flags + 16
            end if
        end if
        if (present(colors)) flags = flags + 32
        node = vkf_mesh_create(int(nv, c_int), int(ni, c_int), real(rgba, c_float), c_null_ptr, c_null_ptr, &
                               int(flags, c_int))
        if (.not. c_associated(node)) return
        call mesh_arrays(node, nv, ni, v, n, uv, indices)
        v = real(xyz, c_float)
        if (lit) then
            n = real(normals, c_float)
        else
            n = 0.0_c_float
            n(3, :) = 1.0_c_float
        end if
        uv = 0.0_c_float
        indices = int(reshape(idx, [ni]) - 1, c_int32_t)
        if (present(colors)) then
            call c_f_pointer(vkf_mesh_array(node, 5_c_int), c, [4, nv])
            do i = 1, nv
                c(:, i) = real(colors(:, i) * rgba, c_float)
            end do
        end if
        if (lit .and. iand(flags, 8) /= 0 .and. present(sphere)) &
            call check_status(vkf_mesh_set_bound(node, real(sphere(1), c_double), real(sphere(2), c_double), &
                                                 real(sphere(3), c_double), real(sphere(4), c_double)), 'shapes')
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
        real(wp), intent(in) :: length                      !! scene units
        call self%move_x_axis([length, 0.0_wp, 0.0_wp], 0.5_wp * length)
        call self%move_y_axis([0.0_wp, length, 0.0_wp], 0.5_wp * length)
        call self%move_z_axis([0.0_wp, 0.0_wp, length], 0.5_wp * length)
    end subroutine place_axes

    !> The axes' length (scene units, >= 0; default 1); the frame's own axes are
    !> moved to the lines' ends.
    subroutine set_axis_length(self, length)
        class(CoordinateAxes), intent(inout) :: self
        real(wp), intent(in) :: length                      !! scene units (< 0: ignored)
        if (length < 0.0_wp) return
        self%length_ = length
        call place_axes(self, length)
        call make_axes(self)
    end subroutine set_axis_length

    !> The axes' length (scene units).
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

    !> Which axes are drawn: an ior of X_AXIS, Y_AXIS, Z_AXIS.
    pure integer function draw_axes(self)
        class(CoordinateAxes), intent(in) :: self
        draw_axes = self%drawn
    end function draw_axes

    !> The axis lines' width (window units; default 1).
    subroutine set_axis_width(self, width)
        class(CoordinateAxes), intent(inout) :: self
        real(wp), intent(in) :: width                       !! window units (<= 0: ignored)
        if (width <= 0.0_wp) return
        self%width_ = width
        call make_axes(self)
    end subroutine set_axis_width

    !> The axis lines' width (window units).
    pure real(wp) function axis_width(self)
        class(CoordinateAxes), intent(in) :: self
        axis_width = self%width_
    end function axis_width

    !> Major and minor tick spacings (>= 0; 0 for none).
    subroutine set_tick_spacing(self, major, minor)
        class(CoordinateAxes), intent(inout) :: self
        real(wp), intent(in) :: major, minor                !! scene units (< 0: unchanged)
        if (major >= 0.0_wp) self%major = major
        if (minor >= 0.0_wp) self%minor = minor
        call make_axes(self)
    end subroutine set_tick_spacing

    !> Major and minor tick spacings.
    pure subroutine tick_spacing(self, major, minor)
        class(CoordinateAxes), intent(in) :: self
        real(wp), intent(out) :: major, minor               !! scene units
        major = self%major
        minor = self%minor
    end subroutine tick_spacing

    !> Major and minor tick sizes (window units; default 8 and 4).
    subroutine set_tick_size(self, major, minor)
        class(CoordinateAxes), intent(inout) :: self
        real(wp), intent(in) :: major, minor                !! window units (<= 0: unchanged)
        if (major > 0.0_wp) self%major_size = major
        if (minor > 0.0_wp) self%minor_size = minor
        call make_axes(self)
    end subroutine set_tick_size

    !> Major and minor tick sizes (window units).
    pure subroutine tick_size(self, major, minor)
        class(CoordinateAxes), intent(in) :: self
        real(wp), intent(out) :: major, minor               !! window units
        major = self%major_size
        minor = self%minor_size
    end subroutine tick_size

    !> Tick shape: a MARKER_* shape (default MARKER_DISK; OpenFrames'
    !> setTickShader with its marker shaders).
    subroutine set_tick_shape(self, shape)
        class(CoordinateAxes), intent(inout) :: self
        integer, intent(in) :: shape                        !! a MARKER_* shape
        self%shape_ = shape
        call make_axes(self)
    end subroutine set_tick_shape

    !> Tick shape: a MARKER_* shape.
    pure integer function tick_shape(self)
        class(CoordinateAxes), intent(in) :: self
        tick_shape = self%shape_
    end function tick_shape

    !> Draw the ticks as an image (OpenFrames' setTickImage; found with
    !> find_data_file; '' for disks again). False if it cannot be read.
    logical function set_tick_image(self, file) result(ok)
        class(CoordinateAxes), intent(inout) :: self
        character(len=*), intent(in) :: file                !! image file ('' for disks)
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

    !> The tick image file ('' for none).
    function tick_image(self) result(file)
        class(CoordinateAxes), intent(in) :: self
        character(len=:), allocatable :: file
        file = self%image_file
    end function tick_image

    !> The axes' color (lines and ticks).
    subroutine axes_set_color(self, color)
        class(CoordinateAxes), intent(inout) :: self
        real(wp), intent(in) :: color(:)                    !! [r, g, b] or [r, g, b, a] (0 to 1)
        call self%ReferenceFrame%set_color(color)
        call make_axes(self)
    end subroutine axes_set_color

    !> What kind of frame this is: 'CoordinateAxes'.
    function axes_frame_info(self) result(info)
        class(CoordinateAxes), intent(in) :: self
        character(len=:), allocatable :: info
        info = 'CoordinateAxes'
    end function axes_frame_info

    !> The bounding sphere: the origin, radius the axis length.
    subroutine axes_bound(self, center, radius)
        class(CoordinateAxes), intent(in) :: self
        real(wp), intent(out) :: center(3), radius          !! the bounding sphere, frame coordinates (scene units)
        center = 0.0_wp
        radius = self%length_
    end subroutine axes_bound

    !> Draw the axes under `group` (the contents); with a window's `viewer`, later
    !> drawings are compiled.
    subroutine axes_build_contents(self, viewer, group)
        class(CoordinateAxes), intent(inout) :: self
        type(c_ptr), intent(in) :: viewer                   !! a window's viewer (null: headless)
        type(c_ptr), intent(in) :: group                    !! the frame's contents group
        self%nodes%contents = group
        call make_axes(self)
        self%nodes%shown = c_associated(viewer)
    end subroutine axes_build_contents

    !> Make the axes' lines and tick marks, in place of the old (nothing before
    !> build_contents).
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

    !> The frame's own axes (arrows) at the ellipsoid's semi-axes, half as long.
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
        real(wp), intent(in) :: radius_x, radius_y, radius_z  !! scene units (<= 0: unchanged)
        real(wp), intent(in) :: lat_spacing, lon_spacing    !! radians (< 0: unchanged)
        if (radius_x > 0.0_wp) self%radii_(1) = radius_x
        if (radius_y > 0.0_wp) self%radii_(2) = radius_y
        if (radius_z > 0.0_wp) self%radii_(3) = radius_z
        if (lat_spacing >= 0.0_wp) self%lat_spacing = lat_spacing
        if (lon_spacing >= 0.0_wp) self%lon_spacing = lon_spacing
        call place_grid_axes(self)
        call make_grid(self)
    end subroutine grid_set_parameters

    !> The ellipsoid's semi-axes and the line spacings, as OpenFrames' getParameters.
    pure subroutine grid_parameters(self, radius_x, radius_y, radius_z, lat_spacing, lon_spacing)
        class(LatLonGrid), intent(in) :: self
        real(wp), intent(out) :: radius_x, radius_y, radius_z  !! scene units
        real(wp), intent(out) :: lat_spacing, lon_spacing   !! radians
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
        real(wp), intent(in) :: width, main_width           !! window units (<= 0: unchanged)
        if (width > 0.0_wp) self%width_ = width
        if (main_width > 0.0_wp) self%main_width = main_width
        call make_grid(self)
    end subroutine set_line_widths

    !> Widths of the grid lines and of the equator and prime meridian (window
    !> units).
    pure subroutine line_widths(self, width, main_width)
        class(LatLonGrid), intent(in) :: self
        real(wp), intent(out) :: width, main_width          !! window units
        width = self%width_
        main_width = self%main_width
    end subroutine line_widths

    !> The lines' color.
    subroutine grid_set_color(self, color)
        class(LatLonGrid), intent(inout) :: self
        real(wp), intent(in) :: color(:)                    !! [r, g, b] or [r, g, b, a] (0 to 1)
        call self%ReferenceFrame%set_color(color)
        call make_grid(self)
    end subroutine grid_set_color

    !> What kind of frame this is: 'LatLonGrid'.
    function grid_frame_info(self) result(info)
        class(LatLonGrid), intent(in) :: self
        character(len=:), allocatable :: info
        info = 'LatLonGrid'
    end function grid_frame_info

    !> The bounding sphere: the origin, radius the largest semi-axis.
    subroutine grid_bound(self, center, radius)
        class(LatLonGrid), intent(in) :: self
        real(wp), intent(out) :: center(3), radius          !! the bounding sphere, frame coordinates (scene units)
        center = 0.0_wp
        radius = maxval(self%radii_)
    end subroutine grid_bound

    !> Draw the grid under `group` (the contents); with a window's `viewer`, later
    !> drawings are compiled.
    subroutine grid_build_contents(self, viewer, group)
        class(LatLonGrid), intent(inout) :: self
        type(c_ptr), intent(in) :: viewer                   !! a window's viewer (null: headless)
        type(c_ptr), intent(in) :: group                    !! the frame's contents group
        self%nodes%contents = group
        call make_grid(self)
        self%nodes%shown = c_associated(viewer)
    end subroutine grid_build_contents

    !> The point at geodetic latitude lat and longitude lon on the ellipsoid
    !> (OpenFrames' LatLonGrid formula: the surface point whose normal has
    !> those angles).
    pure function surface_point(r, lat, lon) result(p)
        real(wp), intent(in) :: r(3)                        !! semi-axes (scene units)
        real(wp), intent(in) :: lat, lon                    !! radians
        real(wp) :: p(3), n(3), r2(3)
        n = [cos(lat) * cos(lon), cos(lat) * sin(lon), sin(lat)]
        r2 = r**2
        p = r2 * n / sqrt(dot_product(r2, n**2))
    end function surface_point

    !> Make the grid's lines (circles in 1 degree steps, meridians in 0.5 degree
    !> steps), in place of the old.
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
        real(wp), intent(in) :: radius                      !! scene units (<= 0: unchanged)
        real(wp), intent(in) :: radial_spacing              !! scene units (< 0: unchanged)
        real(wp), intent(in) :: lon_spacing                 !! radians (< 0: unchanged)
        if (radius > 0.0_wp) self%radius_ = radius
        if (radial_spacing >= 0.0_wp) self%radial_spacing = radial_spacing
        if (lon_spacing >= 0.0_wp) self%lon_spacing = lon_spacing
        call place_axes(self, self%radius_)
        call make_plane(self)
    end subroutine plane_set_parameters

    !> The plane's radius, circle spacing and radial line spacing, as OpenFrames'
    !> getParameters.
    pure subroutine plane_parameters(self, radius, radial_spacing, lon_spacing)
        class(RadialPlane), intent(in) :: self
        real(wp), intent(out) :: radius, radial_spacing     !! scene units
        real(wp), intent(out) :: lon_spacing                !! radians
        radius = self%radius_
        radial_spacing = self%radial_spacing
        lon_spacing = self%lon_spacing
    end subroutine plane_parameters

    !> The plane's color ([r, g, b] or [r, g, b, a]; default 0.8 gray, 0.2 opaque).
    subroutine set_plane_color(self, color)
        class(RadialPlane), intent(inout) :: self
        real(wp), intent(in) :: color(:)                    !! [r, g, b] or [r, g, b, a] (0 to 1)
        call set_rgba(self%plane_color_, color)
        call make_plane(self)
    end subroutine set_plane_color

    !> The plane's color [r, g, b, a].
    pure function plane_color(self) result(c)
        class(RadialPlane), intent(in) :: self
        real(wp) :: c(4)
        c = self%plane_color_
    end function plane_color

    !> The lines' color (default white, 0.2 opaque).
    subroutine plane_set_line_color(self, color)
        class(RadialPlane), intent(inout) :: self
        real(wp), intent(in) :: color(:)                    !! [r, g, b] or [r, g, b, a] (0 to 1)
        call set_rgba(self%line_color_, color)
        call make_plane(self)
    end subroutine plane_set_line_color

    !> The lines' color [r, g, b, a].
    pure function plane_line_color(self) result(c)
        class(RadialPlane), intent(in) :: self
        real(wp) :: c(4)
        c = self%line_color_
    end function plane_line_color

    !> What kind of frame this is: 'RadialPlane'.
    function plane_frame_info(self) result(info)
        class(RadialPlane), intent(in) :: self
        character(len=:), allocatable :: info
        info = 'RadialPlane'
    end function plane_frame_info

    !> The bounding sphere: the origin, radius the plane's radius.
    subroutine plane_bound(self, center, radius)
        class(RadialPlane), intent(in) :: self
        real(wp), intent(out) :: center(3), radius          !! the bounding sphere, frame coordinates (scene units)
        center = 0.0_wp
        radius = self%radius_
    end subroutine plane_bound

    !> Draw the plane under `group` (the contents); with a window's `viewer`, later
    !> drawings are compiled.
    subroutine plane_build_contents(self, viewer, group)
        class(RadialPlane), intent(inout) :: self
        type(c_ptr), intent(in) :: viewer                   !! a window's viewer (null: headless)
        type(c_ptr), intent(in) :: group                    !! the frame's contents group
        self%nodes%contents = group
        call make_plane(self)
        self%nodes%shown = c_associated(viewer)
    end subroutine plane_build_contents

    !> Make the disk, its circles and its radial lines (1 degree steps), in place of
    !> the old.
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
        real(wp), intent(in) :: clock(:), cone(:)           !! radians
        if (size(clock) /= size(cone) .or. size(clock) < 3) return
        self%clock = clock
        self%cone = cone
        call make_cone(self)
    end subroutine set_vertex_angles

    !> The vertices' clock angles (radians).
    pure function clock_angles(self) result(a)
        class(PolyhedralCone), intent(in) :: self
        real(wp), allocatable :: a(:)
        a = self%clock
    end function clock_angles

    !> The vertices' cone angles (radians).
    pure function cone_angles(self) result(a)
        class(PolyhedralCone), intent(in) :: self
        real(wp), allocatable :: a(:)
        a = self%cone
    end function cone_angles

    !> The cone's length along the boresight (> 0; default 1).
    subroutine set_cone_length(self, length)
        class(PolyhedralCone), intent(inout) :: self
        real(wp), intent(in) :: length                      !! scene units (<= 0: ignored)
        if (length <= 0.0_wp) return
        self%length_ = length
        call make_cone(self)
    end subroutine set_cone_length

    !> The cone's length along the boresight (scene units).
    pure real(wp) function cone_length(self)
        class(PolyhedralCone), intent(in) :: self
        cone_length = self%length_
    end function cone_length

    !> The sides' color ([r, g, b] or [r, g, b, a]; their alpha makes them
    !> transparent).
    subroutine set_cone_color(self, color)
        class(PolyhedralCone), intent(inout) :: self
        real(wp), intent(in) :: color(:)                    !! [r, g, b] or [r, g, b, a] (0 to 1)
        call set_rgba(self%cone_color_, color)
        call make_cone(self)
    end subroutine set_cone_color

    !> The sides' color [r, g, b, a].
    pure function cone_color(self) result(c)
        class(PolyhedralCone), intent(in) :: self
        real(wp) :: c(4)
        c = self%cone_color_
    end function cone_color

    !> The edges' and base outline's color.
    subroutine cone_set_line_color(self, color)
        class(PolyhedralCone), intent(inout) :: self
        real(wp), intent(in) :: color(:)                    !! [r, g, b] or [r, g, b, a] (0 to 1)
        call set_rgba(self%line_color_, color)
        call make_cone(self)
    end subroutine cone_set_line_color

    !> The edges' and base outline's color [r, g, b, a].
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

    !> What is drawn: an ior of CONE_SIDES, CONE_EDGES, CONE_BASE_OUTLINE.
    pure integer function draw_mode(self)
        class(PolyhedralCone), intent(in) :: self
        draw_mode = self%mode
    end function draw_mode

    !> Place the cone's apex at `apex` (in the parent frame) with its boresight
    !> along `dir` and its +y axis toward `up` (OpenFrames' makeConeLookAt).
    subroutine make_cone_look_at(self, apex, dir, up)
        class(PolyhedralCone), intent(inout) :: self
        real(wp), intent(in) :: apex(3)                     !! parent frame coordinates (scene units)
        real(wp), intent(in) :: dir(3)                      !! boresight direction (any length > 0)
        real(wp), intent(in) :: up(3)                       !! toward the cone's +y axis (any length)
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
        real(wp), intent(in) :: point(3)                    !! in the cone's frame (scene units)
        real(wp), intent(in), optional :: min_distance, max_distance  !! along the boresight (scene units)
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
        real(wp), intent(in) :: point(3)                    !! in the cone's frame (scene units)
        real(wp), intent(in), optional :: min_distance, max_distance  !! along the boresight (scene units)
        real(wp) :: d
        d = -point(3)
        in_range = d >= 0.0_wp
        if (present(min_distance)) in_range = in_range .and. d >= min_distance
        if (present(max_distance)) in_range = in_range .and. d <= max_distance
    end function in_range

    !> Which side of the line from a to b q is on: > 0 left, < 0 right, 0 on it
    !> (twice the signed area of the triangle a, b, q).
    pure real(wp) function side(a, b, q)
        real(wp), intent(in) :: a(2), b(2), q(2)
        side = (b(1) - a(1)) * (q(2) - a(2)) - (q(1) - a(1)) * (b(2) - a(2))
    end function side

    !> What kind of frame this is: 'PolyhedralCone'.
    function cone_frame_info(self) result(info)
        class(PolyhedralCone), intent(in) :: self
        character(len=:), allocatable :: info
        info = 'PolyhedralCone'
    end function cone_frame_info

    !> The bound of the apex and the base.
    subroutine cone_bound(self, center, radius)
        class(PolyhedralCone), intent(in) :: self
        real(wp), intent(out) :: center(3), radius          !! the bounding sphere, frame coordinates (scene units)
        real(wp) :: r
        r = self%length_ * maxval(tan(self%cone))
        center = [0.0_wp, 0.0_wp, -0.5_wp * self%length_]
        radius = sqrt((0.5_wp * self%length_)**2 + r**2)
    end subroutine cone_bound

    !> Draw the cone under `group` (the contents); with a window's `viewer`, later
    !> drawings are compiled.
    subroutine cone_build_contents(self, viewer, group)
        class(PolyhedralCone), intent(inout) :: self
        type(c_ptr), intent(in) :: viewer                   !! a window's viewer (null: headless)
        type(c_ptr), intent(in) :: group                    !! the frame's contents group
        self%nodes%contents = group
        call make_cone(self)
        self%nodes%shown = c_associated(viewer)
    end subroutine cone_build_contents

    !> Make the sides, edges and base outline (those the draw mode says), in place
    !> of the old.
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
        real(wp), intent(in) :: a, b                        !! radians
        self%a = a
        self%b = b
        call elliptic_vertices(self)
        call make_cone(self)
    end subroutine elliptic_set_primary_angles

    !> The half-angles along x (a) and y (b), radians.
    pure subroutine elliptic_primary_angles(self, a, b)
        class(EllipticCone), intent(in) :: self
        real(wp), intent(out) :: a, b                       !! radians
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
        real(wp), intent(in) :: point(3)                    !! in the cone's frame (scene units)
        real(wp), intent(in), optional :: min_distance, max_distance  !! along the boresight (scene units)
        real(wp) :: p(3), cone, clock
        visible = in_range(point, min_distance, max_distance)
        if (.not. visible .or. norm2(point) == 0.0_wp) return
        p = point / (-point(3))
        cone = atan(norm2(p(1:2)))
        clock = atan2(p(2), p(1))
        visible = cone <= self%a * self%b / sqrt(self%b**2 * cos(clock)**2 + self%a**2 * sin(clock)**2)
    end function elliptic_is_visible

    !> What kind of frame this is: 'EllipticCone'.
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
        real(wp), intent(in) :: x, y                        !! radians
        self%x = x
        self%y = y
        call rect_vertices(self)
        call make_cone(self)
    end subroutine rect_set_primary_angles

    !> The half-angles along x and y, radians.
    pure subroutine rect_primary_angles(self, x, y)
        class(RectangularCone), intent(in) :: self
        real(wp), intent(out) :: x, y                       !! radians
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

    !> What kind of frame this is: 'RectangularCone'.
    function rect_frame_info(self) result(info)
        class(RectangularCone), intent(in) :: self
        character(len=:), allocatable :: info
        info = 'RectangularCone'
    end function rect_frame_info

    ! ==================================================================
    ! Polyhedron

    !> A polyhedron named `name`, of color `color` (its faces, edges and
    !> vertices), with no faces yet (set_faces, set_triangles or set_grid).
    function new_polyhedron(name, color) result(p)
        character(len=*), intent(in) :: name
        real(wp), intent(in), optional :: color(:)          !! [r, g, b] or [r, g, b, a] (0 to 1)
        type(Polyhedron) :: p
        p%ReferenceFrame = ReferenceFrame(name, color)
        p%face_color_ = p%color()
        p%line_color_ = p%color()
        p%point_color_ = p%color()
        allocate(p%v(3, 0), p%tri(3, 0), p%edge(2, 0))
    end function new_polyhedron

    !> Faces given as polygons: `faces` the vertex numbers (1-based) of each
    !> polygon in turn, `sizes` how many each has (>= 3; convex, or at least
    !> star-shaped from its first vertex: drawn as a fan of triangles from
    !> it); the edges are the polygons' sides. E.g. a cube: 8 vertices, 6
    !> faces of 4. A face's front: its vertices counterclockwise.
    subroutine set_faces(self, vertices, faces, sizes)
        class(Polyhedron), intent(inout) :: self
        real(wp), intent(in) :: vertices(:,:)               !! (3, n), frame coordinates
        integer, intent(in) :: faces(:)                     !! (sum(sizes)) vertex numbers
        integer, intent(in) :: sizes(:)                     !! each face's number of vertices
        integer :: f, k, j, nt, ne, first
        integer, allocatable :: sides(:,:)
        self%v = vertices
        nt = sum(max(sizes - 2, 0))
        if (allocated(self%tri)) deallocate(self%tri)
        allocate(self%tri(3, nt), sides(2, sum(sizes)))
        nt = 0
        ne = 0
        first = 0
        do f = 1, size(sizes)
            do k = 2, sizes(f) - 1                          ! a fan from the first vertex
                nt = nt + 1
                self%tri(:, nt) = [faces(first + 1), faces(first + k), faces(first + k + 1)]
            end do
            do k = 1, sizes(f)
                j = modulo(k, sizes(f)) + 1
                ne = ne + 1
                sides(:, ne) = [faces(first + k), faces(first + j)]
            end do
            first = first + sizes(f)
        end do
        self%edge = unique_edges(sides(:, 1:ne), size(vertices, 2))
        call drop_colors(self)
        call make_polyhedron(self)
    end subroutine set_faces

    !> Faces given as triangles (3, m) of vertex numbers (1-based); the edges
    !> are their sides (each once).
    subroutine set_triangles(self, vertices, triangles)
        class(Polyhedron), intent(inout) :: self
        real(wp), intent(in) :: vertices(:,:)               !! (3, n), frame coordinates
        integer, intent(in) :: triangles(:,:)               !! (3, m)
        self%v = vertices
        self%tri = triangles
        self%edge = unique_edges(reshape([triangles(1:2, :), triangles(2:3, :), triangles(3:1:-2, :)], &
                                         [2, 3 * size(triangles, 2)]), size(vertices, 2))
        call drop_colors(self)
        call make_polyhedron(self)
    end subroutine set_triangles

    !> Faces given as a grid of points (3, nu, nv): quadrilaterals between
    !> neighbours (two triangles each), joined around if `closed_u` (the last
    !> point i to the first, for each j; e.g. a tube's circumference) or
    !> `closed_v`. The edges: the grid's lines, those of constant i (along j)
    !> every lines(1) and of constant j (along i) every lines(2) (default:
    !> every one; 0 for none).
    subroutine set_grid(self, points, closed_u, closed_v, lines)
        class(Polyhedron), intent(inout) :: self
        real(wp), intent(in) :: points(:,:,:)               !! (3, nu, nv), frame coordinates
        logical, intent(in), optional :: closed_u, closed_v
        integer, intent(in), optional :: lines(2)
        integer :: nu, nv, mu, mv, i, j, i2, j2, nt, ne, every(2)
        logical :: cu, cv
        nu = size(points, 2)
        nv = size(points, 3)
        cu = .false.
        cv = .false.
        if (present(closed_u)) cu = closed_u .and. nu > 2
        if (present(closed_v)) cv = closed_v .and. nv > 2
        every = 1
        if (present(lines)) every = max(lines, 0)
        self%v = reshape(points, [3, nu * nv])
        mu = merge(nu, nu - 1, cu)                          ! quadrilaterals along i, j
        mv = merge(nv, nv - 1, cv)
        if (allocated(self%tri)) deallocate(self%tri)
        allocate(self%tri(3, 2 * max(mu, 0) * max(mv, 0)))
        nt = 0
        do j = 1, mv
            j2 = modulo(j, nv) + 1
            do i = 1, mu
                i2 = modulo(i, nu) + 1
                self%tri(:, nt + 1) = [at(i, j), at(i2, j), at(i2, j2)]
                self%tri(:, nt + 2) = [at(i, j), at(i2, j2), at(i, j2)]
                nt = nt + 2
            end do
        end do
        if (allocated(self%edge)) deallocate(self%edge)
        allocate(self%edge(2, nu * nv * 2))
        ne = 0
        if (every(1) > 0) then                              ! lines of constant i, along j
            do i = 1, nu
                if (.not. shown_line(i, nu, every(1), cu)) cycle
                do j = 1, mv
                    ne = ne + 1
                    self%edge(:, ne) = [at(i, j), at(i, modulo(j, nv) + 1)]
                end do
            end do
        end if
        if (every(2) > 0) then                              ! lines of constant j, along i
            do j = 1, nv
                if (.not. shown_line(j, nv, every(2), cv)) cycle
                do i = 1, mu
                    ne = ne + 1
                    self%edge(:, ne) = [at(i, j), at(modulo(i, nu) + 1, j)]
                end do
            end do
        end if
        self%edge = self%edge(:, 1:ne)
        call drop_colors(self)
        call make_polyhedron(self)

    contains

        pure integer function at(i_, j_)
            integer, intent(in) :: i_, j_
            at = i_ + (j_ - 1) * nu
        end function at

        !> Line k of n drawn, every `step` (and the last, if the grid is open).
        pure logical function shown_line(k, n, step, closed)
            integer, intent(in) :: k, n, step
            logical, intent(in) :: closed
            shown_line = modulo(k - 1, step) == 0 .or. (k == n .and. .not. closed)
        end function shown_line

    end subroutine set_grid

    !> The edges drawn (2, k) (vertex numbers), in place of the faces' (after
    !> set_faces, set_triangles or set_grid).
    subroutine set_edges(self, edges)
        class(Polyhedron), intent(inout) :: self
        integer, intent(in) :: edges(:,:)
        self%edge = edges
        call make_polyhedron(self)
    end subroutine set_edges

    !> Move the vertices (3, n), keeping the faces and edges (as many as
    !> before; e.g. a surface changing in time).
    subroutine set_vertices(self, vertices)
        class(Polyhedron), intent(inout) :: self
        real(wp), intent(in) :: vertices(:,:)
        if (size(vertices, 2) /= size(self%v, 2)) then
            write(*, '(a)') 'vkframes: Polyhedron '//self%name()//': set_vertices needs as many vertices as before'
            return
        end if
        self%v = vertices
        call make_polyhedron(self)
    end subroutine set_vertices

    !> The number of vertices.
    pure integer function num_vertices(self)
        class(Polyhedron), intent(in) :: self
        num_vertices = size(self%v, 2)
    end function num_vertices

    !> The number of triangles drawn (polygons: as fans).
    pure integer function num_triangles(self)
        class(Polyhedron), intent(in) :: self
        num_triangles = size(self%tri, 2)
    end function num_triangles

    !> The number of edges.
    pure integer function num_edges(self)
        class(Polyhedron), intent(in) :: self
        num_edges = size(self%edge, 2)
    end function num_edges

    !> A color per vertex (4, n) or (3, n) ([r, g, b(, a)], 0 to 1), for the
    !> faces, blended across them (in place of the face color; the opacity
    !> still applies). Dropped when the faces are set again.
    subroutine set_vertex_colors(self, colors)
        class(Polyhedron), intent(inout) :: self
        real(wp), intent(in) :: colors(:,:)
        integer :: i
        if (size(colors, 2) /= size(self%v, 2) .or. size(colors, 1) < 3) then
            write(*, '(a)') 'vkframes: Polyhedron '//self%name()//': set_vertex_colors needs a color per vertex'
            return
        end if
        if (allocated(self%vertex_colors_)) deallocate(self%vertex_colors_)
        allocate(self%vertex_colors_(4, size(colors, 2)))
        do i = 1, size(colors, 2)
            call set_rgba(self%vertex_colors_(:, i), colors(:, i))
        end do
        call make_polyhedron(self)
    end subroutine set_vertex_colors

    !> The face color again, for every vertex.
    subroutine remove_vertex_colors(self)
        class(Polyhedron), intent(inout) :: self
        call drop_colors(self)
        call make_polyhedron(self)
    end subroutine remove_vertex_colors

    !> Whether the faces have a color per vertex.
    pure logical function has_vertex_colors(self)
        class(Polyhedron), intent(in) :: self
        has_vertex_colors = allocated(self%vertex_colors_)
    end function has_vertex_colors

    !> The faces' color (the alpha times the opacity).
    subroutine set_face_color(self, color)
        class(Polyhedron), intent(inout) :: self
        real(wp), intent(in) :: color(:)                    !! [r, g, b] or [r, g, b, a] (0 to 1)
        call set_rgba(self%face_color_, color)
        call make_polyhedron(self)
    end subroutine set_face_color

    !> The faces' color [r, g, b, a].
    pure function face_color(self) result(c)
        class(Polyhedron), intent(in) :: self
        real(wp) :: c(4)
        c = self%face_color_
    end function face_color

    !> The edges' color.
    subroutine poly_set_line_color(self, color)
        class(Polyhedron), intent(inout) :: self
        real(wp), intent(in) :: color(:)                    !! [r, g, b] or [r, g, b, a] (0 to 1)
        call set_rgba(self%line_color_, color)
        call make_polyhedron(self)
    end subroutine poly_set_line_color

    !> The edges' color [r, g, b, a].
    pure function poly_line_color(self) result(c)
        class(Polyhedron), intent(in) :: self
        real(wp) :: c(4)
        c = self%line_color_
    end function poly_line_color

    !> The vertices' color (POLY_VERTICES).
    subroutine set_point_color(self, color)
        class(Polyhedron), intent(inout) :: self
        real(wp), intent(in) :: color(:)                    !! [r, g, b] or [r, g, b, a] (0 to 1)
        call set_rgba(self%point_color_, color)
        call make_polyhedron(self)
    end subroutine set_point_color

    !> The vertices' color [r, g, b, a].
    pure function point_color(self) result(c)
        class(Polyhedron), intent(in) :: self
        real(wp) :: c(4)
        c = self%point_color_
    end function point_color

    !> The faces' opacity (0 to 1; default 1, opaque): below 1 translucent.
    subroutine poly_set_opacity(self, opacity)
        class(Polyhedron), intent(inout) :: self
        real(wp), intent(in) :: opacity
        self%opacity_ = max(0.0_wp, min(1.0_wp, opacity))
        call make_polyhedron(self)
    end subroutine poly_set_opacity

    !> The faces' opacity.
    pure real(wp) function poly_opacity(self)
        class(Polyhedron), intent(in) :: self
        poly_opacity = self%opacity_
    end function poly_opacity

    !> The edges' width (window units; default 1).
    subroutine poly_set_line_width(self, width)
        class(Polyhedron), intent(inout) :: self
        real(wp), intent(in) :: width
        self%width_ = max(0.0_wp, width)
        call make_polyhedron(self)
    end subroutine poly_set_line_width

    !> The edges' width (window units).
    pure real(wp) function poly_line_width(self)
        class(Polyhedron), intent(in) :: self
        poly_line_width = self%width_
    end function poly_line_width

    !> The vertices' size (window units; default 6).
    subroutine set_point_size(self, size_)
        class(Polyhedron), intent(inout) :: self
        real(wp), intent(in) :: size_
        self%point_size_ = max(0.0_wp, size_)
        call make_polyhedron(self)
    end subroutine set_point_size

    !> The vertices' size (window units).
    pure real(wp) function point_size(self)
        class(Polyhedron), intent(in) :: self
        point_size = self%point_size_
    end function point_size

    !> What to draw: an ior of POLY_FACES, POLY_EDGES, POLY_VERTICES (default
    !> faces and edges).
    subroutine poly_set_draw_mode(self, mode)
        class(Polyhedron), intent(inout) :: self
        integer, intent(in) :: mode
        self%mode = mode
        call make_polyhedron(self)
    end subroutine poly_set_draw_mode

    !> What is drawn: an ior of POLY_FACES, POLY_EDGES, POLY_VERTICES.
    pure integer function poly_draw_mode(self)
        class(Polyhedron), intent(in) :: self
        poly_draw_mode = self%mode
    end function poly_draw_mode

    !> Faces lit by the scene's lights (the default), or unlit (their color
    !> as is, transparent as a cone's sides: writing no depth).
    subroutine set_lit(self, lit)
        class(Polyhedron), intent(inout) :: self
        logical, intent(in) :: lit
        self%lit_ = lit
        call make_polyhedron(self)
    end subroutine set_lit

    !> Whether the faces are lit.
    pure logical function lit(self)
        class(Polyhedron), intent(in) :: self
        lit = self%lit_
    end function lit

    !> Shading smooth across the vertices (normals averaged over the faces
    !> around each: a curved surface, e.g. a grid's), or flat faces (the
    !> default: a polyhedron's).
    subroutine set_smooth(self, smooth)
        class(Polyhedron), intent(inout) :: self
        logical, intent(in) :: smooth
        self%smooth_ = smooth
        call make_polyhedron(self)
    end subroutine set_smooth

    !> Whether the shading is smooth.
    pure logical function smooth(self)
        class(Polyhedron), intent(in) :: self
        smooth = self%smooth_
    end function smooth

    !> Both sides of the faces drawn and lit (the default: open surfaces), or
    !> only their fronts (a closed surface, its faces counterclockwise seen
    !> from outside: translucent, its far side isn't drawn through it).
    subroutine set_two_sided(self, two_sided)
        class(Polyhedron), intent(inout) :: self
        logical, intent(in) :: two_sided
        self%two_sided_ = two_sided
        call make_polyhedron(self)
    end subroutine set_two_sided

    !> Whether both sides are drawn.
    pure logical function two_sided(self)
        class(Polyhedron), intent(in) :: self
        two_sided = self%two_sided_
    end function two_sided

    !> What kind of frame this is: 'Polyhedron'.
    function poly_frame_info(self) result(info)
        class(Polyhedron), intent(in) :: self
        character(len=:), allocatable :: info
        info = 'Polyhedron'
    end function poly_frame_info

    !> The bounding sphere: about the vertices' box's center.
    subroutine poly_bound(self, center, radius)
        class(Polyhedron), intent(in) :: self
        real(wp), intent(out) :: center(3), radius          !! the bounding sphere, frame coordinates (scene units)
        call vertices_sphere(self%v, center, radius)
    end subroutine poly_bound

    subroutine poly_build_contents(self, viewer, group)
        class(Polyhedron), intent(inout) :: self
        type(c_ptr), intent(in) :: viewer                   !! a window's viewer (null: headless)
        type(c_ptr), intent(in) :: group                    !! the frame's contents group
        self%nodes%contents = group
        call make_polyhedron(self)
        self%nodes%shown = c_associated(viewer)
    end subroutine poly_build_contents

    !> No colors per vertex.
    subroutine drop_colors(self)
        type(Polyhedron), intent(inout) :: self
        if (allocated(self%vertex_colors_)) deallocate(self%vertex_colors_)
    end subroutine drop_colors

    !> The polyhedron's nodes, made again (if shown).
    subroutine make_polyhedron(self)
        class(Polyhedron), intent(inout) :: self
        type(c_ptr) :: group
        real(wp) :: rgba(4), sphere(4)
        real(wp), allocatable :: v(:,:), normals(:,:), colors(:,:), segments(:,:)
        integer, allocatable :: idx(:,:)
        integer :: i, k, m
        if (.not. c_associated(self%nodes%contents)) return
        group = vkf_switch_create()
        if (iand(self%mode, POLY_FACES) /= 0 .and. size(self%tri, 2) > 0) then
            rgba = self%face_color_
            if (allocated(self%vertex_colors_)) rgba = [1.0_wp, 1.0_wp, 1.0_wp, 1.0_wp]
            rgba(4) = rgba(4) * self%opacity_
            call vertices_sphere(self%v, sphere(1:3), sphere(4))
            if (.not. self%lit_) then
                if (allocated(self%vertex_colors_)) then
                    call add(group, surface_node(self%v, self%tri, rgba, colors=self%vertex_colors_))
                else
                    call add(group, surface_node(self%v, self%tri, rgba))
                end if
            else if (self%smooth_) then                          ! shared vertices, normals averaged
                normals = vertex_normals(self%v, self%tri)
                if (allocated(self%vertex_colors_)) then
                    call add(group, surface_node(self%v, self%tri, rgba, normals, self%vertex_colors_, self%two_sided_, &
                                                 sphere))
                else
                    call add(group, surface_node(self%v, self%tri, rgba, normals, two_sided=self%two_sided_, &
                                                 sphere=sphere))
                end if
            else                                                ! flat: each triangle its own vertices
                m = size(self%tri, 2)
                allocate(v(3, 3 * m), normals(3, 3 * m), idx(3, m))
                do k = 1, m
                    do i = 1, 3
                        v(:, 3 * (k - 1) + i) = self%v(:, self%tri(i, k))
                    end do
                    normals(:, 3 * k - 2) = unit_normal(v(:, 3 * k - 2), v(:, 3 * k - 1), v(:, 3 * k))
                    normals(:, 3 * k - 1) = normals(:, 3 * k - 2)
                    normals(:, 3 * k) = normals(:, 3 * k - 2)
                    idx(:, k) = [3 * k - 2, 3 * k - 1, 3 * k]
                end do
                if (allocated(self%vertex_colors_)) then
                    allocate(colors(4, 3 * m))
                    do k = 1, m
                        do i = 1, 3
                            colors(:, 3 * (k - 1) + i) = self%vertex_colors_(:, self%tri(i, k))
                        end do
                    end do
                    call add(group, surface_node(v, idx, rgba, normals, colors, self%two_sided_, sphere))
                else
                    call add(group, surface_node(v, idx, rgba, normals, two_sided=self%two_sided_, sphere=sphere))
                end if
            end if
        end if
        if (iand(self%mode, POLY_EDGES) /= 0 .and. size(self%edge, 2) > 0) then
            allocate(segments(3, 2 * size(self%edge, 2)))
            do k = 1, size(self%edge, 2)
                segments(:, 2 * k - 1) = self%v(:, self%edge(1, k))
                segments(:, 2 * k) = self%v(:, self%edge(2, k))
            end do
            call add(group, line_node(segments, self%line_color_, self%width_, .true.))
        end if
        if (iand(self%mode, POLY_VERTICES) /= 0 .and. size(self%v, 2) > 0) &
            call add(group, marker_node(self%v, self%point_color_, self%point_size_, MARKER_DISK, 0.0_wp, c_null_ptr))
        call self%nodes%replace(group)
    end subroutine make_polyhedron

    !> Edges (2, k) of vertex numbers (1 to n), each once whichever its
    !> direction (sorted by their smaller vertex, then the larger).
    function unique_edges(pairs, n) result(edges)
        integer, intent(in) :: pairs(:,:)
        integer, intent(in) :: n                            !! the number of vertices
        integer, allocatable :: edges(:,:)
        integer(int64), allocatable :: keys(:)
        integer :: k, m
        allocate(keys(size(pairs, 2)))
        keys = int(min(pairs(1, :), pairs(2, :)), int64) * (int(n, int64) + 1) + int(max(pairs(1, :), pairs(2, :)), int64)
        call sort_keys(keys)
        allocate(edges(2, size(keys)))
        m = 0
        do k = 1, size(keys)
            if (k > 1) then
                if (keys(k) == keys(k - 1)) cycle
            end if
            m = m + 1
            edges(:, m) = [int(keys(k) / (int(n, int64) + 1)), int(modulo(keys(k), int(n, int64) + 1))]
        end do
        edges = edges(:, 1:m)
    end function unique_edges

    !> Sort keys, increasing (a bottom-up merge sort).
    pure subroutine sort_keys(a)
        integer(int64), intent(inout) :: a(:)
        integer(int64), allocatable :: b(:)
        integer :: n, width, lo, mid, hi, i, j, k
        n = size(a)
        allocate(b(n))
        width = 1
        do while (width < n)
            do lo = 1, n, 2 * width
                mid = min(lo + width, n + 1)
                hi = min(lo + 2 * width, n + 1)
                i = lo
                j = mid
                do k = lo, hi - 1
                    if (i < mid .and. (j >= hi .or. a(min(i, n)) <= a(min(j, n)))) then
                        b(k) = a(i)
                        i = i + 1
                    else
                        b(k) = a(j)
                        j = j + 1
                    end if
                end do
            end do
            a = b
            width = 2 * width
        end do
    end subroutine sort_keys

    !> Normals per vertex: the faces' around it, weighted by their areas.
    pure function vertex_normals(v, tri) result(normals)
        real(wp), intent(in) :: v(:,:)
        integer, intent(in) :: tri(:,:)
        real(wp) :: normals(3, size(v, 2)), f(3), len_
        integer :: k, i
        normals = 0.0_wp
        do k = 1, size(tri, 2)
            f = cross(v(:, tri(2, k)) - v(:, tri(1, k)), v(:, tri(3, k)) - v(:, tri(1, k)))   ! (twice the area)
            do i = 1, 3
                normals(:, tri(i, k)) = normals(:, tri(i, k)) + f
            end do
        end do
        do i = 1, size(v, 2)
            len_ = norm2(normals(:, i))
            if (len_ > 0.0_wp) then
                normals(:, i) = normals(:, i) / len_
            else
                normals(:, i) = [0.0_wp, 0.0_wp, 1.0_wp]
            end if
        end do
    end function vertex_normals

    !> A triangle's unit normal (counterclockwise: its front), or +z if it has
    !> no area.
    pure function unit_normal(a, b, c) result(n)
        real(wp), intent(in) :: a(3), b(3), c(3)
        real(wp) :: n(3), len_
        n = cross(b - a, c - a)
        len_ = norm2(n)
        if (len_ > 0.0_wp) then
            n = n / len_
        else
            n = [0.0_wp, 0.0_wp, 1.0_wp]
        end if
    end function unit_normal

    !> A sphere around points v (3, n): their box's center, out to the
    !> farthest (zero for none).
    pure subroutine vertices_sphere(v, center, radius)
        real(wp), intent(in) :: v(:,:)
        real(wp), intent(out) :: center(3), radius
        integer :: i
        center = 0.0_wp
        radius = 0.0_wp
        if (size(v, 2) == 0) return
        center = 0.5_wp * (minval(v, dim=2) + maxval(v, dim=2))
        do i = 1, size(v, 2)
            radius = max(radius, norm2(v(:, i) - center))
        end do
    end subroutine vertices_sphere

end module vkf_shapes
