!> Trajectory artists: what draws a Trajectory, in a DrawableTrajectory frame, as
!> OpenFrames' TrajectoryArtist, CurveArtist, SegmentArtist, MarkerArtist and
!> DrawableTrajectory, and RibbonArtist (not in OpenFrames: a translucent
!> surface swept between two points at each trajectory point, e.g. an orbit's
!> plane from the origin to the positions).
!>
!> A DrawableTrajectory is a frame (its points are in its coordinates) holding
!> artists. Each frame update, an artist draws what changed in its trajectory:
!> appended points are written into the GPU buffers in place, and only the
!> buffers written to are uploaded (see vkf_line_chain); a cleared trajectory
!> is redrawn into the same buffers. The last point drawn is always rewritten,
!> since `set_optional` changes it.
!>
!> Trace mode (`set_trace_mode`) draws only the part of the trajectory up to
!> the current time: the points already passed (a change of the draw ranges, no
!> upload); a curve adds a two-point tail from the last of them to the point
!> interpolated at the current time (the only data uploaded each frame). The
!> part passed is from the first point to the current time, in the
!> trajectory's time direction (times may decrease: e.g. a trajectory
!> propagated backwards, shown with time running backwards).
!>
!> Point colors (not in OpenFrames): a CurveArtist draws a trajectory with
!> colors (Trajectory%add_color, or add's `color`) in them, each segment
!> blended from its first point's color to its second's (one color: that
!> color); without colors, or with `set_point_colors(.false.)`, in its own
!> color. Its line buffers hold colors only then.
!>
!> Artists and their trajectories are not owned: they must stay alive while
!> drawn.
!>
!> Picking (WindowProxy%pick): a DrawableTrajectory picks what its artists
!> draw, as drawn (in trace mode, up to the current time): a curve's segments
!> (its chunks skipped by their bounds when far from the cursor), with the
!> point and the time there (interpolated); a segment artist's segments; a
!> marker artist's start, end and intermediate markers, with their times.
!> Lines are reached within the tolerance plus half their width, markers
!> plus half their size. Each artist gives its best hit: the closest to the
!> cursor (and of those, the nearest).
module vkf_artists

    use, intrinsic :: iso_c_binding, only: c_ptr, c_null_ptr, c_associated, c_int, c_float, c_f_pointer
    use vkf_math, only: wp
    use vkf_trajectory, only: Trajectory, DataSource, position_sources, SOURCE_ZERO, SOURCE_POSOPT, &
                              TIME_OUT_OF_RANGE, TIME_NOT_FOUND
    use vkf_frames, only: ReferenceFrame, PickHit, add_pick_hit, PICK_CURVE, PICK_SEGMENT, PICK_MARKER
    use vkf_pick, only: PickCamera, point_pick, segment_pick, sphere_near, wants
    use vkf_layer, only: vkf_line_create, vkf_line_create_colored, vkf_line_colors, vkf_line_points_changed, &
                         vkf_ribbon_create, &
                         vkf_line_set_range, &
                         vkf_node_add_child, vkf_node_remove_child, vkf_node_delete, vkf_switch_create, &
                         vkf_compile_node, vkf_viewer_any, line_set_style, line_set_pattern, vkf_image_read, vkf_image_delete
    use vkf_line_chain, only: LineChain, CHAIN_STRIP, CHAIN_SEGMENTS, CHAIN_MARKERS, CHAIN_RIBBON, split_points, &
                              split_distances, &
                              line_buffer, check

    implicit none

    private

    !> The base of the trajectory artists (OpenFrames' TrajectoryArtist).
    type, abstract, public :: TrajectoryArtist
        private
        type(Trajectory), pointer :: traj => null()
        logical :: trace = .false.
        logical :: pickable_ = .true.               !! picked (WindowProxy%pick)
        ! what was drawn
        integer :: seen_generation = -1, seen_revision = -1
    contains
        procedure :: set_trajectory, get_trajectory
        procedure :: set_trace_mode, trace_mode
        procedure :: set_pickable => artist_set_pickable, pickable => artist_pickable
        procedure :: release => artist_release
        procedure(artist_build), deferred :: build
        procedure(artist_update), deferred :: update
    end type TrajectoryArtist

    abstract interface
        !> Create the artist's render nodes under `group`. `viewer` (may be
        !> null) compiles the nodes it creates later.
        subroutine artist_build(self, viewer, group)
            import :: TrajectoryArtist, c_ptr
            implicit none
            class(TrajectoryArtist), intent(inout) :: self
            type(c_ptr), intent(in) :: viewer, group
        end subroutine artist_build

        !> Bring the drawing up to date with the trajectory at simulation time `t`.
        subroutine artist_update(self, t)
            import :: TrajectoryArtist, wp
            implicit none
            class(TrajectoryArtist), intent(inout) :: self
            real(wp), intent(in) :: t
        end subroutine artist_update
    end interface

    !> Draws a trajectory's points connected by a line (OpenFrames' CurveArtist).
    !> The x, y and z of the points come from data sources (default: the
    !> trajectory's positions). The line is in the artist's color, or in the
    !> trajectory's point colors if it has any (see the module documentation).
    type, extends(TrajectoryArtist), public :: CurveArtist
        private
        type(DataSource) :: sources_(3)
        logical :: default_data = .true.
        real(wp) :: color_(4) = [1.0_wp, 1.0_wp, 1.0_wp, 1.0_wp]
        real(wp) :: width_ = 1.0_wp
        integer :: pattern_factor = 1
        integer :: pattern_ = int(z'FFFF')
        logical :: lod_ = .true.                    !! level of detail (see set_level_of_detail)
        logical :: point_colors_ = .true.           !! the trajectory's colors, if any (see set_point_colors)
        ! rendering
        type(c_ptr) :: group = c_null_ptr           !! the chain's group (to make it again, colored or not)
        logical :: compile = .false.                !! (chunks made later are compiled)
        type(LineChain) :: chain
        integer :: n_colored = 0                    !! points written with the trajectory's own colors
        type(c_ptr) :: tail = c_null_ptr
        integer :: n_written = 0                    !! points written to the chain
        integer :: n_drawn = 0                      !! points drawn from it
        logical :: tail_shown = .false.
        real(wp) :: tail_time = huge(1.0_wp)        !! the time the tail was drawn for
    contains
        procedure :: set_trajectory => curve_set_trajectory
        procedure :: set_x_data, set_y_data, set_z_data, data_sources
        procedure :: set_color => curve_set_color, color => curve_color
        procedure :: set_width => curve_set_width, width => curve_width
        procedure :: set_pattern => curve_set_pattern, pattern => curve_pattern
        procedure :: set_level_of_detail, level_of_detail, lod_error
        procedure :: set_point_colors, point_colors, drawn_color
        procedure :: build => curve_build
        procedure :: update => curve_update
        procedure :: release => curve_release
        procedure :: num_chunks => curve_num_chunks, num_drawn => curve_num_drawn
        procedure :: drawn_point, drawn_distance, trace_end
    end type CurveArtist

    interface CurveArtist
        module procedure new_curve_artist
    end interface CurveArtist

    !> Draws a line segment at every `stride`-th point of a trajectory
    !> (OpenFrames' SegmentArtist), from the point given by the start data
    !> sources to the one given by the end data sources: e.g. radius vectors
    !> (the origin to the positions), or from the positions to points stored as
    !> an optional (a target). Default (an addition to OpenFrames, which draws
    !> nothing until the sources are set): from the frame's origin to the
    !> trajectory's positions.
    !>
    !> Relative ends (`set_end_relative`, not in OpenFrames): the end sources
    !> give an offset from the start, so a vector stored per point (a thrust,
    !> a velocity) is drawn from each position, at a length set by the end
    !> sources' scale (`set_end_scale`), which can be changed without touching
    !> the trajectory (only this artist's segments are written again). Where
    !> the vector is zero (no thrust) nothing is drawn.
    type, extends(TrajectoryArtist), public :: SegmentArtist
        private
        type(DataSource) :: start_(3), end_(3)
        logical :: default_data = .true.
        logical :: end_relative_ = .false.          !! the end sources: an offset from the start
        integer :: stride_ = 1
        real(wp) :: color_(4) = [1.0_wp, 1.0_wp, 1.0_wp, 1.0_wp]
        real(wp) :: width_ = 1.0_wp
        integer :: pattern_factor = 1
        integer :: pattern_ = int(z'FFFF')
        ! rendering
        type(LineChain) :: chain
        integer :: n_written = 0                    !! segments written to the chain
        integer :: n_drawn = 0                      !! segments drawn from it
    contains
        procedure :: set_trajectory => segment_set_trajectory
        procedure :: set_start_x_data, set_start_y_data, set_start_z_data
        procedure :: set_end_x_data, set_end_y_data, set_end_z_data
        procedure :: set_start_data, set_end_data
        procedure :: start_data_sources, end_data_sources
        procedure :: set_end_relative, end_relative, set_end_scale
        procedure :: set_stride, stride
        procedure :: set_color => segment_set_color, color => segment_color
        procedure :: set_width => segment_set_width, width => segment_width
        procedure :: set_pattern => segment_set_pattern, pattern => segment_pattern
        procedure :: build => segment_build
        procedure :: update => segment_update
        procedure :: release => segment_release
        procedure :: num_chunks => segment_num_chunks, num_drawn => segment_num_drawn
        procedure :: drawn_segment
    end type SegmentArtist

    interface SegmentArtist
        module procedure new_segment_artist
    end interface SegmentArtist

    !> Draws a translucent surface swept between two points at each point of
    !> a trajectory (not in OpenFrames): from the point given by the start data
    !> sources to the one given by the end data sources, with a quad between
    !> each point's pair and the next's. Default: from the frame's origin to the
    !> positions, sweeping out an orbit's plane (a fan, as the radius vectors of
    !> a SegmentArtist would fill it); other sources give e.g. a curtain from
    !> the positions down to a ground track stored as an optional. Unlit, seen
    !> from both sides, depth tested but writing no depth (as the equatorial
    !> plane of a RadialPlane), drawn to double precision as the curves (its
    !> edge on a CurveArtist's line through the same points).
    !>
    !> Trace mode draws the surface up to the current time: the quads passed
    !> (a draw range, no upload) and one from the last point passed to the
    !> point interpolated at the time. Point colors: a trajectory with colors
    !> colors the surface, each point's pair in its color, with the artist's
    !> opacity (its alpha times theirs); see set_point_colors. A trajectory
    !> going round several times covers the surface again each time, so it
    !> gets more opaque: trace one revolution, or lower the alpha.
    type, extends(TrajectoryArtist), public :: RibbonArtist
        private
        type(DataSource) :: start_(3), end_(3)
        logical :: default_data = .true.
        real(wp) :: color_(4) = [1.0_wp, 1.0_wp, 1.0_wp, 0.25_wp]
        logical :: point_colors_ = .true.           !! the trajectory's colors, if any
        ! rendering: trajectory point i is chain points 2i-1 (start) and 2i (end)
        type(c_ptr) :: group = c_null_ptr
        logical :: compile = .false.
        type(LineChain) :: chain
        type(c_ptr) :: tail = c_null_ptr            !! the quad to the point at the current time (trace mode)
        integer :: n_written = 0                    !! trajectory points written to the chain
        integer :: n_drawn = 0                      !! trajectory points drawn from it
        integer :: n_colored = 0                    !! points written with the trajectory's own colors
        logical :: tail_shown = .false.
        real(wp) :: tail_time = huge(1.0_wp)
    contains
        procedure :: set_trajectory => ribbon_set_trajectory
        procedure :: set_start_x_data => ribbon_set_start_x_data, set_start_y_data => ribbon_set_start_y_data
        procedure :: set_start_z_data => ribbon_set_start_z_data
        procedure :: set_end_x_data => ribbon_set_end_x_data, set_end_y_data => ribbon_set_end_y_data
        procedure :: set_end_z_data => ribbon_set_end_z_data
        procedure :: start_data_sources => ribbon_start_data_sources, end_data_sources => ribbon_end_data_sources
        procedure :: set_color => ribbon_set_color, color => ribbon_color
        procedure :: set_point_colors => ribbon_set_point_colors, point_colors => ribbon_point_colors
        procedure :: build => ribbon_build
        procedure :: update => ribbon_update
        procedure :: release => ribbon_release
        procedure :: num_chunks => ribbon_num_chunks, num_drawn => ribbon_num_drawn
        procedure :: drawn_pair, drawn_color => ribbon_drawn_color, trace_end => ribbon_trace_end
    end type RibbonArtist

    interface RibbonArtist
        module procedure new_ribbon_artist
    end interface RibbonArtist

    ! which markers a MarkerArtist draws (OpenFrames' DrawnMarkers; combine with ior)
    integer, parameter, public :: MARKER_START = 1, MARKER_INTERMEDIATE = 2, MARKER_END = 4
    ! how intermediate markers are spaced (OpenFrames' IntermediateType)
    integer, parameter, public :: INTERMEDIATE_TIME = 1      !! every `spacing` time units
    integer, parameter, public :: INTERMEDIATE_DISTANCE = 2  !! every `spacing` of arc length
    integer, parameter, public :: INTERMEDIATE_DATA = 3      !! every `spacing`-th data point
    ! marker shapes (OpenFrames' disk, image, and its Demos/Shaders/Marker_*.frag)
    integer, parameter, public :: MARKER_DISK = 0, MARKER_CIRCLE = 1, MARKER_SQUARE = 2, MARKER_PLUS = 3, &
                                  MARKER_X = 4, MARKER_TARGET = 5, MARKER_ROSE = 6, MARKER_CIRCLE_PULSE = 7, &
                                  MARKER_DISK_PULSE = 8, MARKER_SPIRAL = 9, MARKER_IMAGE = 10

    !> Draws markers at a trajectory's points (OpenFrames' MarkerArtist): at
    !> its start and end, and intermediate markers spaced by time, arc length
    !> or data points, counted from the start or from the end. Markers keep
    !> their size on screen (window units), or with auto-attenuation shrink
    !> when the camera is farther than the trajectory's size. In trace mode
    !> (an addition), the markers passed are drawn: the end marker (at the
    !> trajectory's end) only once the time reaches the end.
    type, extends(TrajectoryArtist), public :: MarkerArtist
        private
        type(DataSource) :: sources_(3)
        logical :: default_data = .true.
        integer :: markers_ = ior(MARKER_START, MARKER_END)
        integer :: intermediate_type_ = INTERMEDIATE_DATA
        real(wp) :: spacing_ = 1.0_wp
        integer :: direction_ = MARKER_START
        real(wp) :: start_color(4) = [1.0_wp, 0.0_wp, 0.0_wp, 1.0_wp]
        real(wp) :: end_color(4) = [1.0_wp, 0.0_wp, 0.0_wp, 1.0_wp]
        real(wp) :: intermediate_color(4) = [1.0_wp, 1.0_wp, 1.0_wp, 1.0_wp]
        real(wp) :: size_ = 10.0_wp
        integer :: shape_ = MARKER_DISK
        character(len=:), allocatable :: image_file
        type(c_ptr) :: image = c_null_ptr
        logical :: attenuate = .false.
        ! rendering
        type(c_ptr) :: group = c_null_ptr
        logical :: shown = .false.                  !! built for a window (marker nodes made later are compiled)
        type(LineChain) :: start_node, end_node, inter
        logical :: drawn = .false.                  !! drawn at least once since built
        integer :: n_seen = 0                       !! points seen (for the bounds)
        real(wp) :: lo(3) = 0.0_wp, hi(3) = 0.0_wp  !! bounds of the points seen
        real(wp) :: radius = 0.0_wp                 !! attenuation radius applied
        ! intermediate markers: in trajectory order, and where to continue
        real(wp), allocatable :: inter_xyz(:,:), inter_t(:)
        integer :: n_inter = 0, n_inter_written = 0, n_inter_drawn = 0
        real(wp) :: next_time = 0.0_wp
        integer :: next_index = 0
        real(wp) :: arc = 0.0_wp, prev_arc = 0.0_wp, target = 0.0_wp, prev_point(3) = 0.0_wp
        logical :: start_shown = .false., end_shown = .false.
    contains
        procedure :: set_trajectory => marker_set_trajectory
        procedure :: set_x_data => marker_set_x_data, set_y_data => marker_set_y_data
        procedure :: set_z_data => marker_set_z_data, data_sources => marker_data_sources
        procedure :: set_markers, markers
        procedure :: set_marker_color, marker_color
        procedure :: set_marker_size, marker_size
        procedure :: set_marker_shape, marker_shape
        procedure :: set_marker_image, marker_image_file
        procedure :: set_auto_attenuate, auto_attenuate
        procedure :: set_intermediate_type, intermediate_type
        procedure :: set_intermediate_spacing, intermediate_spacing
        procedure :: set_intermediate_direction, intermediate_direction
        procedure :: build => marker_build
        procedure :: update => marker_update
        procedure :: release => marker_release
        procedure :: num_intermediate, drawn_marker
    end type MarkerArtist

    interface MarkerArtist
        module procedure new_marker_artist
    end interface MarkerArtist

    type :: ArtistPointer
        class(TrajectoryArtist), pointer :: p => null()
        type(c_ptr) :: group = c_null_ptr           !! the artist's node (null until built)
    end type ArtistPointer

    !> A frame for drawing trajectories (OpenFrames' DrawableTrajectory): it
    !> holds artists, which draw in its coordinates.
    type, extends(ReferenceFrame), public :: DrawableTrajectory
        private
        type(ArtistPointer), allocatable :: artists(:)
        integer :: n_artists = 0
        type(c_ptr) :: contents_group = c_null_ptr
        logical :: shown = .false.                  !! built for a window (artists added later are compiled)
    contains
        procedure :: add_artist, remove_artist, remove_all_artists, num_artists, artist
        procedure :: frame_info => drawable_frame_info
        procedure :: build_contents => drawable_build_contents
        procedure :: update => drawable_update
        procedure :: pick => drawable_pick
    end type DrawableTrajectory

    interface DrawableTrajectory
        module procedure new_drawable_trajectory
    end interface DrawableTrajectory

contains

    ! ==================================================================
    ! TrajectoryArtist

    !> Draw `traj` (it must stay alive while drawn).
    subroutine set_trajectory(self, traj)
        class(TrajectoryArtist), intent(inout) :: self
        type(Trajectory), intent(in), target :: traj
        self%traj => traj
        self%seen_generation = -1                     ! redraw
    end subroutine set_trajectory

    function get_trajectory(self) result(traj)
        class(TrajectoryArtist), intent(in) :: self
        type(Trajectory), pointer :: traj
        traj => self%traj
    end function get_trajectory

    !> Whether `pick` tests what this artist draws (default true; e.g. false for
    !> a spacecraft's marker beside its orbit's curve).
    subroutine artist_set_pickable(self, pickable)
        class(TrajectoryArtist), intent(inout) :: self
        logical, intent(in) :: pickable
        self%pickable_ = pickable
    end subroutine artist_set_pickable

    pure logical function artist_pickable(self)
        class(TrajectoryArtist), intent(in) :: self
        artist_pickable = self%pickable_
    end function artist_pickable

    !> Draw only the part up to the current time (see the module documentation).
    subroutine set_trace_mode(self, enabled)
        class(TrajectoryArtist), intent(inout) :: self
        logical, intent(in) :: enabled
        self%trace = enabled
    end subroutine set_trace_mode

    pure logical function trace_mode(self)
        class(TrajectoryArtist), intent(in) :: self
        trace_mode = self%trace
    end function trace_mode

    !> Forget the render nodes (after the artist's group was removed from the
    !> scene): build starts again.
    subroutine artist_release(self)
        class(TrajectoryArtist), intent(inout) :: self
        self%seen_generation = -1
    end subroutine artist_release

    !> Whether the trajectory changed since last drawn; `redraw` if everything
    !> must be drawn again (cleared, or a new trajectory or data sources).
    logical function data_changed(self, redraw) result(changed)
        class(TrajectoryArtist), intent(inout) :: self
        logical, intent(out) :: redraw
        redraw = .false.
        changed = .false.
        if (.not. associated(self%traj)) return
        if (self%traj%generation() /= self%seen_generation) then
            redraw = .true.
            self%seen_generation = self%traj%generation()
            self%seen_revision = -1
        end if
        changed = self%traj%revision() /= self%seen_revision
        self%seen_revision = self%traj%revision()
    end function data_changed

    !> Number of points up to time t, from the first point in the trajectory's
    !> time direction (at most n).
    integer function points_passed(traj, t, n) result(k)
        type(Trajectory), intent(in) :: traj
        real(wp), intent(in) :: t
        integer, intent(in) :: n
        integer :: status, index
        status = traj%time_index(t, index)
        select case (status)
        case (TIME_OUT_OF_RANGE)
            k = merge(0, n, index < 1)
        case (TIME_NOT_FOUND)
            k = n
        case default                                    ! found or at a bound: index is the last point passed
            k = index
        end select
        k = max(0, min(n, k))
    end function points_passed

    !> Number of points the data sources give: 0 for invalid sources or no
    !> trajectory, huge() if all are zero.
    integer function source_points(traj, sources) result(n)
        type(Trajectory), pointer, intent(in) :: traj
        type(DataSource), intent(in) :: sources(3)
        n = 0
        if (.not. associated(traj)) return
        if (.not. traj%verify_data(sources)) return
        n = traj%num_points(sources)
    end function source_points

    !> The sources are x, y, z of the position, unscaled.
    pure logical function is_position_xyz(s)
        type(DataSource), intent(in) :: s(3)
        integer :: k
        is_position_xyz = .true.
        do k = 1, 3
            if (s(k)%src /= SOURCE_POSOPT .or. s(k)%opt /= 0 .or. s(k)%element /= k .or. s(k)%scale /= 1.0_wp) &
                is_position_xyz = .false.
        end do
    end function is_position_xyz

    !> Color [r, g, b] (alpha unchanged) or [r, g, b, a] into rgba.
    pure subroutine set_rgba(rgba, color)
        real(wp), intent(inout) :: rgba(4)
        real(wp), intent(in) :: color(:)
        integer :: n
        n = min(4, size(color))
        rgba(1:n) = color(1:n)
    end subroutine set_rgba

    ! ==================================================================
    ! CurveArtist

    !> A curve artist for `traj` (optional), drawn in color `color` ([r, g, b]
    !> or [r, g, b, a]; default white) and `width` (window units; default 1).
    function new_curve_artist(traj, color, width) result(a)
        type(Trajectory), intent(in), target, optional :: traj
        real(wp), intent(in), optional :: color(:), width
        type(CurveArtist) :: a
        a%sources_ = position_sources(3)
        if (present(traj)) call a%set_trajectory(traj)
        if (present(color)) call a%set_color(color)
        if (present(width)) a%width_ = width
    end function new_curve_artist

    !> Draw `traj`; with the default data sources, its positions (per its DOF).
    subroutine curve_set_trajectory(self, traj)
        class(CurveArtist), intent(inout) :: self
        type(Trajectory), intent(in), target :: traj
        self%traj => traj
        if (self%default_data) self%sources_ = position_sources(traj%dof())
        self%seen_generation = -1
    end subroutine curve_set_trajectory

    !> Where the x (y, z) of the points come from. Returns whether the
    !> trajectory has those data.
    logical function set_x_data(self, src) result(valid)
        class(CurveArtist), intent(inout) :: self
        type(DataSource), intent(in) :: src
        valid = set_curve_source(self, 1, src)
    end function set_x_data

    logical function set_y_data(self, src) result(valid)
        class(CurveArtist), intent(inout) :: self
        type(DataSource), intent(in) :: src
        valid = set_curve_source(self, 2, src)
    end function set_y_data

    logical function set_z_data(self, src) result(valid)
        class(CurveArtist), intent(inout) :: self
        type(DataSource), intent(in) :: src
        valid = set_curve_source(self, 3, src)
    end function set_z_data

    logical function set_curve_source(self, k, src) result(valid)
        class(CurveArtist), intent(inout) :: self
        integer, intent(in) :: k
        type(DataSource), intent(in) :: src
        self%sources_(k) = src
        self%default_data = .false.
        self%seen_generation = -1
        valid = .true.
        if (associated(self%traj)) valid = self%traj%verify_data(self%sources_)
    end function set_curve_source

    pure function data_sources(self) result(s)
        class(CurveArtist), intent(in) :: self
        type(DataSource) :: s(3)
        s = self%sources_
    end function data_sources

    !> The line's color: [r, g, b] (alpha unchanged) or [r, g, b, a], 0 to 1
    !> (with the trajectory's point colors drawn: for points without one).
    subroutine curve_set_color(self, color)
        class(CurveArtist), intent(inout) :: self
        real(wp), intent(in) :: color(:)
        call set_rgba(self%color_, color)
        call curve_style(self)
        if (self%chain%is_colored()) self%seen_generation = -1    ! (points without colors redrawn)
    end subroutine curve_set_color

    pure function curve_color(self) result(c)
        class(CurveArtist), intent(in) :: self
        real(wp) :: c(4)
        c = self%color_
    end function curve_color

    !> The line's width in window units (logical pixels).
    subroutine curve_set_width(self, width)
        class(CurveArtist), intent(inout) :: self
        real(wp), intent(in) :: width
        self%width_ = width
        call curve_style(self)
    end subroutine curve_set_width

    pure real(wp) function curve_width(self)
        class(CurveArtist), intent(in) :: self
        curve_width = self%width_
    end function curve_width

    !> A dash pattern, as OpenGL's line stipple (OpenFrames' setPattern): each
    !> bit of the 16-bit `pattern` (bit 0 first; z'FFFF', the default, is
    !> solid) covers `factor` window units, along the whole curve (the dashes
    !> run on across its points). The dashes' length on screen is kept within
    !> 0.7 to 1.4 times the pattern's (see shaders/line.vert).
    subroutine curve_set_pattern(self, factor, pattern)
        class(CurveArtist), intent(inout) :: self
        integer, intent(in) :: factor, pattern
        self%pattern_factor = max(1, factor)
        self%pattern_ = iand(pattern, int(z'FFFF'))
        call curve_style(self)
    end subroutine curve_set_pattern

    !> Level of detail (default on): seen from far away, the curve is drawn
    !> from every 2nd, 4th, ... point (and its last), as many as can be
    !> skipped while staying within half a pixel of all of them, chosen per
    !> view and per chunk of points. Off: every point is drawn.
    subroutine set_level_of_detail(self, enabled)
        class(CurveArtist), intent(inout) :: self
        logical, intent(in) :: enabled
        self%lod_ = enabled
        call self%chain%set_lod(enabled)
    end subroutine set_level_of_detail

    pure logical function level_of_detail(self)
        class(CurveArtist), intent(in) :: self
        level_of_detail = self%lod_
    end function level_of_detail

    !> The error bounds of chunk j of the curve's points (for checks): how far
    !> the points skipped can be from the segments drawn at stride 2^k, k = 1,
    !> 2, ... (scene units).
    function lod_error(self, j) result(error)
        class(CurveArtist), intent(in) :: self
        integer, intent(in) :: j
        real(wp), allocatable :: error(:)
        error = real(self%chain%lod_error(j), wp)
    end function lod_error

    !> Whether a trajectory with colors is drawn in them (default true): each
    !> segment blended from its first point's color to its second's; points
    !> without a color yet (colors arriving after positions) in the artist's
    !> color. False: all in the artist's color. With level of detail, a color
    !> lasting fewer points than are skipped may not show from far away.
    subroutine set_point_colors(self, enabled)
        class(CurveArtist), intent(inout) :: self
        logical, intent(in) :: enabled
        self%point_colors_ = enabled
    end subroutine set_point_colors

    pure logical function point_colors(self)
        class(CurveArtist), intent(in) :: self
        point_colors = self%point_colors_
    end function point_colors

    !> The color of point i (1 to num_drawn) as drawn: read back from the line
    !> buffers.
    function drawn_color(self, i) result(rgba)
        class(CurveArtist), intent(in) :: self
        integer, intent(in) :: i
        real(wp) :: rgba(4)
        if (self%chain%is_colored()) then
            rgba = self%chain%point_color(i)
        else
            rgba = self%color_
        end if
    end function drawn_color

    pure subroutine curve_pattern(self, factor, bits)
        class(CurveArtist), intent(in) :: self
        integer, intent(out) :: factor, bits
        factor = self%pattern_factor
        bits = self%pattern_
    end subroutine curve_pattern

    !> Number of line buffers the curve is drawn with (0 until built).
    pure integer function curve_num_chunks(self)
        class(CurveArtist), intent(in) :: self
        curve_num_chunks = self%chain%num_chunks()
    end function curve_num_chunks

    !> Number of points drawn now (all of them, or in trace mode those passed).
    pure integer function curve_num_drawn(self)
        class(CurveArtist), intent(in) :: self
        curve_num_drawn = self%n_drawn
    end function curve_num_drawn

    !> Point i (1 to num_drawn) as drawn: read back from the line buffers.
    function drawn_point(self, i) result(v)
        class(CurveArtist), intent(in) :: self
        integer, intent(in) :: i
        real(wp) :: v(3)
        v = self%chain%point(i)
    end function drawn_point

    !> The distance along the curve at point i (1 to num_drawn), as drawn (for
    !> the dash pattern): read back from the line buffers.
    function drawn_distance(self, i) result(s)
        class(CurveArtist), intent(in) :: self
        integer, intent(in) :: i
        real(wp) :: s
        s = self%chain%distance(i)
    end function drawn_distance

    !> In trace mode, the end of the line (the point at the current time) as
    !> drawn; false if no tail is drawn.
    logical function trace_end(self, v)
        class(CurveArtist), intent(in) :: self
        real(wp), intent(out) :: v(3)
        real(c_float), dimension(:,:,:), pointer :: p
        v = 0.0_wp
        trace_end = self%tail_shown
        if (.not. trace_end) return
        call line_buffer(self%tail, p)
        if (associated(p)) v = real(p(1:3, 1, 2), wp) + real(p(1:3, 2, 2), wp)
    end function trace_end

    subroutine curve_build(self, viewer, group)
        class(CurveArtist), intent(inout) :: self
        type(c_ptr), intent(in) :: viewer, group
        self%group = group
        self%compile = .false.
        call init_chain(self)
        call self%chain%reserve(curve_points(self))       ! compiled with the group
        self%compile = c_associated(viewer)
        call self%chain%set_compile(self%compile)
        self%tail = vkf_line_create_colored(2_c_int)      ! (white unless the points' colors are drawn)
        if (c_associated(self%tail)) call check(vkf_node_add_child(group, self%tail))
        call curve_style(self)
        self%seen_generation = -1
    end subroutine curve_build

    !> The chain, colored if the trajectory's colors are drawn.
    subroutine init_chain(self)
        type(CurveArtist), intent(inout) :: self
        call self%chain%init(self%group, CHAIN_STRIP, colored=uses_colors(self))
        call self%chain%set_lod(self%lod_)
        call self%chain%set_compile(self%compile)
        self%n_colored = 0
    end subroutine init_chain

    !> Whether the trajectory's colors are drawn: wanted, and it has some.
    logical function uses_colors(self)
        type(CurveArtist), intent(in) :: self
        uses_colors = self%point_colors_ .and. associated(self%traj)
        if (uses_colors) uses_colors = self%traj%num_colors() > 0
    end function uses_colors

    subroutine curve_release(self)
        class(CurveArtist), intent(inout) :: self
        call self%chain%release()
        if (c_associated(self%tail)) call vkf_node_delete(self%tail)
        self%tail = c_null_ptr
        self%n_written = 0
        self%n_drawn = 0
        self%n_colored = 0
        self%group = c_null_ptr
        self%tail_shown = .false.
        call artist_release(self)
    end subroutine curve_release

    subroutine curve_update(self, t)
        class(CurveArtist), intent(inout) :: self
        real(wp), intent(in) :: t
        integer :: n, i1
        logical :: changed, redraw
        if (.not. self%chain%is_built()) return
        n = curve_points(self)

        ! the trajectory's colors drawn or not (its first colors, or the
        ! setting, changed): the chain made again, with or without colors
        if (uses_colors(self) .neqv. self%chain%is_colored()) then
            call self%chain%detach()
            call init_chain(self)
            call curve_style(self)
            self%seen_generation = -1
        end if

        ! new points, or a redraw (cleared, new trajectory or data sources)
        changed = data_changed(self, redraw)
        if (redraw .or. .not. associated(self%traj)) then
            self%n_written = 0
            self%n_colored = 0
        end if
        if (changed) then
            i1 = max(1, min(self%n_written, n))          ! from the last point written (set_optional)
            if (self%chain%is_colored()) i1 = max(1, min(i1, self%n_colored))   ! (and colors added since)
            call self%chain%reserve(n)
            if (n >= i1) call write_curve(self, i1, n)
            self%n_written = n
        end if

        if (.not. self%trace) then
            self%n_drawn = self%n_written
            call self%chain%draw(self%n_drawn)
            if (c_associated(self%tail)) call check(vkf_line_set_range(self%tail, 1_c_int, 0_c_int))
            self%tail_shown = .false.
            self%tail_time = huge(1.0_wp)
            return
        end if
        ! trace mode: the points passed, and a tail to the point at time t
        if (.not. changed .and. t == self%tail_time) return
        self%n_drawn = 0
        if (associated(self%traj)) self%n_drawn = points_passed(self%traj, t, self%n_written)
        call self%chain%draw(self%n_drawn)
        call update_tail(self, t, self%n_drawn)
        self%tail_time = t
    end subroutine curve_update

    !> Number of points to draw (1 if all sources are zero: a point at the origin).
    integer function curve_points(self) result(n)
        type(CurveArtist), intent(in) :: self
        n = source_points(self%traj, self%sources_)
        if (all(self%sources_%src == SOURCE_ZERO) .and. n > 0) n = min(1, self%traj%num_times())
    end function curve_points

    !> Write points i1 .. i2 into the chain (and upload the chunks written to).
    subroutine write_curve(self, i1, i2)
        type(CurveArtist), intent(inout) :: self
        integer, intent(in) :: i1, i2
        real(c_float), dimension(:,:,:), pointer :: p
        real(wp), dimension(:,:), pointer :: pos
        real(wp), allocatable :: v(:,:)
        integer :: j, lo, hi
        do j = 1, self%chain%num_chunks()
            call self%chain%slice(j, i1, i2, lo, hi, p)
            if (.not. associated(p)) cycle
            if (is_position_xyz(self%sources_)) then        ! the usual case: straight from the positions
                call self%traj%positions(pos)
                call split_points(pos(:, lo:hi), p)
            else                                            ! other sources: gathered a range at a time
                allocate(v(3, hi - lo + 1))
                call self%traj%points(lo, hi, self%sources_, v)
                call split_points(v, p)
                deallocate(v)
            end if
        end do
        if (self%chain%is_colored()) call write_colors(self, i1, i2)
        if (is_position_xyz(self%sources_)) then           ! distances from the positions in double
            call self%traj%positions(pos)
            call self%chain%written(i1, i2, pos)
        else
            call self%chain%written(i1, i2)
        end if
    end subroutine write_curve

    !> The colors of points i1 .. i2 into the chain: the trajectory's, and the
    !> artist's color for points without one yet.
    subroutine write_colors(self, i1, i2)
        type(CurveArtist), intent(inout) :: self
        integer, intent(in) :: i1, i2
        real(c_float), dimension(:,:), pointer :: c, col
        integer :: j, lo, hi, m
        call self%traj%colors(col)
        m = self%traj%num_colors()
        do j = 1, self%chain%num_chunks()
            call self%chain%color_slice(j, i1, i2, lo, hi, c)
            if (.not. associated(c)) cycle
            if (m >= lo) c(:, 1:min(hi, m)-lo+1) = col(:, lo:min(hi, m))
            if (hi > m) c(:, max(lo, m+1)-lo+1:) = spread(real(self%color_, c_float), 2, hi - max(lo, m+1) + 1)
        end do
        self%n_colored = min(m, i2)
    end subroutine write_colors

    !> Color of point i as drawn by a colored chain: the trajectory's, or the
    !> artist's if it has none yet.
    function point_rgba(self, i) result(rgba)
        type(CurveArtist), intent(in) :: self
        integer, intent(in) :: i
        real(wp) :: rgba(4)
        if (i <= self%traj%num_colors()) then
            rgba = self%traj%color(i)
        else
            rgba = self%color_
        end if
    end function point_rgba

    !> The tail: from point k to the point interpolated at time t (none if
    !> before the first point or at/after the last).
    subroutine update_tail(self, t, k)
        type(CurveArtist), intent(inout) :: self
        real(wp), intent(in) :: t
        integer, intent(in) :: k
        real(c_float), dimension(:,:,:), pointer :: p
        real(wp) :: t0, t1, frac, xyz(3, 2)
        self%tail_shown = .false.
        if (.not. c_associated(self%tail)) return
        if (k < 1 .or. k >= self%n_written) then
            call check(vkf_line_set_range(self%tail, 1_c_int, 0_c_int))
            return
        end if
        t0 = self%traj%time(k)
        t1 = self%traj%time(k + 1)
        frac = 0.0_wp
        if (t1 /= t0) frac = max(0.0_wp, min(1.0_wp, (t - t0) / (t1 - t0)))
        xyz(:, 1) = self%traj%point(k, self%sources_)
        xyz(:, 2) = xyz(:, 1) + frac * (self%traj%point(k + 1, self%sources_) - xyz(:, 1))
        call line_buffer(self%tail, p)
        if (.not. associated(p)) return
        call split_points(xyz, p(:, :, 1:2))
        call split_distances(self%chain%distance(k) + [0.0_wp, norm2(xyz(:, 2) - xyz(:, 1))], p(4, :, 1:2))
        call tail_colors(self, k, frac)
        call check(vkf_line_points_changed(self%tail, 2_c_int))
        self%tail_shown = .true.
    end subroutine update_tail

    !> The tail's colors: from point k's to the color at frac along to point
    !> k + 1's (white, showing the style's color, if the chain has none).
    subroutine tail_colors(self, k, frac)
        type(CurveArtist), intent(inout) :: self
        integer, intent(in) :: k
        real(wp), intent(in) :: frac
        real(c_float), dimension(:,:), pointer :: c
        real(wp) :: c0(4), c1(4)
        integer(c_int) :: capacity
        type(c_ptr) :: data
        data = vkf_line_colors(self%tail, capacity)
        if (.not. c_associated(data) .or. capacity < 2) return
        call c_f_pointer(data, c, [4, 2])
        if (self%chain%is_colored()) then
            c0 = point_rgba(self, k)
            c1 = point_rgba(self, k + 1)
            c(:, 1) = real(c0, c_float)
            c(:, 2) = real(c0 + frac * (c1 - c0), c_float)
        else
            c = 1.0_c_float
        end if
    end subroutine tail_colors

    !> The style: the artist's color, or white if the points' colors are drawn
    !> (the shader multiplies them by it).
    subroutine curve_style(self)
        type(CurveArtist), intent(inout) :: self
        real(wp) :: rgba(4)
        rgba = self%color_
        if (self%chain%is_colored()) rgba = 1.0_wp
        call self%chain%set_style(rgba, self%width_)
        call self%chain%set_pattern(self%pattern_factor, self%pattern_)
        if (c_associated(self%tail)) then
            call check(line_set_style(self%tail, real(rgba, c_float), real(self%width_, c_float)))
            call check(line_set_pattern(self%tail, self%pattern_factor, self%pattern_))
        end if
    end subroutine curve_style

    ! ==================================================================
    ! SegmentArtist

    !> A segment artist for `traj` (optional), drawn in color `color` ([r, g,
    !> b] or [r, g, b, a]; default white) and `width` (window units; default
    !> 1), at every `stride`-th point (default 1).
    function new_segment_artist(traj, color, width, stride) result(a)
        type(Trajectory), intent(in), target, optional :: traj
        real(wp), intent(in), optional :: color(:), width
        integer, intent(in), optional :: stride
        type(SegmentArtist) :: a
        a%end_ = position_sources(3)
        if (present(traj)) call a%set_trajectory(traj)
        if (present(color)) call a%set_color(color)
        if (present(width)) a%width_ = width
        if (present(stride)) call a%set_stride(stride)
    end function new_segment_artist

    !> Draw `traj`; with the default data sources, from the origin to its
    !> positions (per its DOF).
    subroutine segment_set_trajectory(self, traj)
        class(SegmentArtist), intent(inout) :: self
        type(Trajectory), intent(in), target :: traj
        self%traj => traj
        if (self%default_data) self%end_ = position_sources(traj%dof())
        self%seen_generation = -1
    end subroutine segment_set_trajectory

    !> Where the x (y, z) of the segments' start (end) points come from.
    !> Returns whether the trajectory has the data of all sources.
    logical function set_start_x_data(self, src) result(valid)
        class(SegmentArtist), intent(inout) :: self
        type(DataSource), intent(in) :: src
        valid = set_segment_source(self, 1, 1, src)
    end function set_start_x_data

    logical function set_start_y_data(self, src) result(valid)
        class(SegmentArtist), intent(inout) :: self
        type(DataSource), intent(in) :: src
        valid = set_segment_source(self, 1, 2, src)
    end function set_start_y_data

    logical function set_start_z_data(self, src) result(valid)
        class(SegmentArtist), intent(inout) :: self
        type(DataSource), intent(in) :: src
        valid = set_segment_source(self, 1, 3, src)
    end function set_start_z_data

    logical function set_end_x_data(self, src) result(valid)
        class(SegmentArtist), intent(inout) :: self
        type(DataSource), intent(in) :: src
        valid = set_segment_source(self, 2, 1, src)
    end function set_end_x_data

    logical function set_end_y_data(self, src) result(valid)
        class(SegmentArtist), intent(inout) :: self
        type(DataSource), intent(in) :: src
        valid = set_segment_source(self, 2, 2, src)
    end function set_end_y_data

    logical function set_end_z_data(self, src) result(valid)
        class(SegmentArtist), intent(inout) :: self
        type(DataSource), intent(in) :: src
        valid = set_segment_source(self, 2, 3, src)
    end function set_end_z_data

    !> Set component k of the start (side 1) or end (side 2) sources.
    logical function set_segment_source(self, side, k, src) result(valid)
        class(SegmentArtist), intent(inout) :: self
        integer, intent(in) :: side, k
        type(DataSource), intent(in) :: src
        if (side == 1) then
            self%start_(k) = src
        else
            self%end_(k) = src
        end if
        self%default_data = .false.
        self%seen_generation = -1
        valid = .true.
        if (associated(self%traj)) valid = self%traj%verify_data(self%start_) .and. self%traj%verify_data(self%end_)
    end function set_segment_source

    pure function start_data_sources(self) result(s)
        class(SegmentArtist), intent(in) :: self
        type(DataSource) :: s(3)
        s = self%start_
    end function start_data_sources

    pure function end_data_sources(self) result(s)
        class(SegmentArtist), intent(in) :: self
        type(DataSource) :: s(3)
        s = self%end_
    end function end_data_sources

    !> All three start (end) sources at once, e.g. position_sources(3) or
    !> optional_sources(1). Returns whether the trajectory has their data.
    logical function set_start_data(self, src) result(valid)
        class(SegmentArtist), intent(inout) :: self
        type(DataSource), intent(in) :: src(3)
        integer :: k
        do k = 1, 3
            valid = set_segment_source(self, 1, k, src(k))
        end do
    end function set_start_data

    logical function set_end_data(self, src) result(valid)
        class(SegmentArtist), intent(inout) :: self
        type(DataSource), intent(in) :: src(3)
        integer :: k
        do k = 1, 3
            valid = set_segment_source(self, 2, k, src(k))
        end do
    end function set_end_data

    !> Relative ends (default false): each segment ends at its start plus the
    !> end sources' values (a vector drawn from each point), not at them.
    subroutine set_end_relative(self, relative)
        class(SegmentArtist), intent(inout) :: self
        logical, intent(in) :: relative
        if (relative .eqv. self%end_relative_) return
        self%end_relative_ = relative
        self%seen_generation = -1                   ! (all segments written again)
    end subroutine set_end_relative

    pure logical function end_relative(self)
        class(SegmentArtist), intent(in) :: self
        end_relative = self%end_relative_
    end function end_relative

    !> Set the scale of all three end sources (with relative ends: the length
    !> drawn per unit of the stored vector, e.g. km per newton). Only this
    !> artist's segments are written again.
    subroutine set_end_scale(self, scale)
        class(SegmentArtist), intent(inout) :: self
        real(wp), intent(in) :: scale
        if (all(self%end_%scale == scale)) return
        self%end_%scale = scale
        self%default_data = .false.
        self%seen_generation = -1
    end subroutine set_end_scale

    !> Draw a segment at points 1, 1 + stride, 1 + 2 stride, ... (stride >= 1).
    subroutine set_stride(self, stride)
        class(SegmentArtist), intent(inout) :: self
        integer, intent(in) :: stride
        if (max(1, stride) == self%stride_) return
        self%stride_ = max(1, stride)
        self%seen_generation = -1
    end subroutine set_stride

    pure integer function stride(self)
        class(SegmentArtist), intent(in) :: self
        stride = self%stride_
    end function stride

    subroutine segment_set_color(self, color)
        class(SegmentArtist), intent(inout) :: self
        real(wp), intent(in) :: color(:)
        call set_rgba(self%color_, color)
        call self%chain%set_style(self%color_, self%width_)
    end subroutine segment_set_color

    pure function segment_color(self) result(c)
        class(SegmentArtist), intent(in) :: self
        real(wp) :: c(4)
        c = self%color_
    end function segment_color

    subroutine segment_set_width(self, width)
        class(SegmentArtist), intent(inout) :: self
        real(wp), intent(in) :: width
        self%width_ = width
        call self%chain%set_style(self%color_, self%width_)
    end subroutine segment_set_width

    pure real(wp) function segment_width(self)
        class(SegmentArtist), intent(in) :: self
        segment_width = self%width_
    end function segment_width

    !> A dash pattern (see CurveArtist%set_pattern), starting again at the
    !> start of each segment.
    subroutine segment_set_pattern(self, factor, pattern)
        class(SegmentArtist), intent(inout) :: self
        integer, intent(in) :: factor, pattern
        self%pattern_factor = max(1, factor)
        self%pattern_ = iand(pattern, int(z'FFFF'))
        call self%chain%set_pattern(self%pattern_factor, self%pattern_)
    end subroutine segment_set_pattern

    pure subroutine segment_pattern(self, factor, bits)
        class(SegmentArtist), intent(in) :: self
        integer, intent(out) :: factor, bits
        factor = self%pattern_factor
        bits = self%pattern_
    end subroutine segment_pattern

    pure integer function segment_num_chunks(self)
        class(SegmentArtist), intent(in) :: self
        segment_num_chunks = self%chain%num_chunks()
    end function segment_num_chunks

    !> Number of segments drawn now (all of them, or in trace mode those at
    !> the points passed).
    pure integer function segment_num_drawn(self)
        class(SegmentArtist), intent(in) :: self
        segment_num_drawn = self%n_drawn
    end function segment_num_drawn

    !> Segment s (1 to num_drawn; at point 1 + (s-1) stride) as drawn: read back
    !> from the line buffers.
    subroutine drawn_segment(self, s, a, b)
        class(SegmentArtist), intent(in) :: self
        integer, intent(in) :: s
        real(wp), intent(out) :: a(3), b(3)
        a = self%chain%point(2 * s - 1)
        b = self%chain%point(2 * s)
    end subroutine drawn_segment

    subroutine segment_build(self, viewer, group)
        class(SegmentArtist), intent(inout) :: self
        type(c_ptr), intent(in) :: viewer, group
        call self%chain%init(group, CHAIN_SEGMENTS)
        call self%chain%set_style(self%color_, self%width_)
        call self%chain%set_pattern(self%pattern_factor, self%pattern_)
        call self%chain%reserve(2 * segment_count(self))  ! compiled with the group
        call self%chain%set_compile(c_associated(viewer))
        self%seen_generation = -1
    end subroutine segment_build

    subroutine segment_release(self)
        class(SegmentArtist), intent(inout) :: self
        call self%chain%release()
        self%n_written = 0
        self%n_drawn = 0
        call artist_release(self)
    end subroutine segment_release

    subroutine segment_update(self, t)
        class(SegmentArtist), intent(inout) :: self
        real(wp), intent(in) :: t
        integer :: m, s1, k
        logical :: changed, redraw
        if (.not. self%chain%is_built()) return
        m = segment_count(self)

        changed = data_changed(self, redraw)
        if (redraw .or. .not. associated(self%traj)) self%n_written = 0
        if (changed) then
            s1 = max(1, min(self%n_written, m))          ! from the last segment written (set_optional)
            call self%chain%reserve(2 * m)
            if (m >= s1) call write_segments(self, s1, m)
            self%n_written = m
        end if

        self%n_drawn = self%n_written
        if (self%trace .and. associated(self%traj)) then  ! the segments at the points passed
            k = points_passed(self%traj, t, self%traj%num_times())
            self%n_drawn = 0
            if (k >= 1) self%n_drawn = min(self%n_written, (k - 1) / self%stride_ + 1)
        end if
        call self%chain%draw(2 * self%n_drawn)
    end subroutine segment_update

    !> Number of segments to draw (none if all sources are zero).
    integer function segment_count(self) result(m)
        type(SegmentArtist), intent(in) :: self
        integer :: n
        m = 0
        if (all(self%start_%src == SOURCE_ZERO) .and. all(self%end_%src == SOURCE_ZERO)) return
        n = min(source_points(self%traj, self%start_), source_points(self%traj, self%end_))
        if (n > 0) m = (n - 1) / self%stride_ + 1
    end function segment_count

    !> Write segments s1 .. s2 into the chain (and upload the chunks written to).
    subroutine write_segments(self, s1, s2)
        type(SegmentArtist), intent(inout) :: self
        integer, intent(in) :: s1, s2
        real(c_float), dimension(:,:,:), pointer :: p
        real(wp), allocatable :: v(:,:), w(:,:)
        integer :: j, lo, hi, sa, sb, ia, ib
        do j = 1, self%chain%num_chunks()
            call self%chain%slice(j, 2 * s1 - 1, 2 * s2, lo, hi, p)   ! chunks hold whole segments
            if (.not. associated(p)) cycle
            ! segments sa .. sb: their start points (odd chain points) and end points (even)
            sa = (lo + 1) / 2
            sb = hi / 2
            ia = 1 + (sa - 1) * self%stride_
            ib = 1 + (sb - 1) * self%stride_
            allocate(v(3, sb - sa + 1), w(3, sb - sa + 1))
            call self%traj%points(ia, ib, self%start_, v, self%stride_)
            call split_points(v, p(:, :, 2*sa-lo:2*sb-lo:2))
            call self%traj%points(ia, ib, self%end_, w, self%stride_)
            if (self%end_relative_) w = v + w        ! (no offset: exactly the start, a segment drawn as nothing)
            call split_points(w, p(:, :, 2*sa-lo+1:2*sb-lo+1:2))
            deallocate(v, w)
        end do
        call self%chain%written(2 * s1 - 1, 2 * s2)
    end subroutine write_segments

    ! ==================================================================
    ! RibbonArtist

    !> A ribbon artist for `traj` (optional), of color `color` ([r, g, b], alpha
    !> unchanged, or [r, g, b, a]; default white, alpha 0.25), from the frame's
    !> origin to the positions (see set_start_x_data, ... for others).
    function new_ribbon_artist(traj, color) result(a)
        type(Trajectory), intent(in), target, optional :: traj
        real(wp), intent(in), optional :: color(:)
        type(RibbonArtist) :: a
        a%end_ = position_sources(3)
        if (present(traj)) call a%set_trajectory(traj)
        if (present(color)) call set_rgba(a%color_, color)
    end function new_ribbon_artist

    !> Draw `traj`; with the default data sources, from the origin to its
    !> positions (per its DOF).
    subroutine ribbon_set_trajectory(self, traj)
        class(RibbonArtist), intent(inout) :: self
        type(Trajectory), intent(in), target :: traj
        self%traj => traj
        if (self%default_data) self%end_ = position_sources(traj%dof())
        self%seen_generation = -1
    end subroutine ribbon_set_trajectory

    !> Where the x (y, z) of the surface's start (end) points come from.
    !> Returns whether the trajectory has the data of all sources.
    logical function ribbon_set_start_x_data(self, src) result(valid)
        class(RibbonArtist), intent(inout) :: self
        type(DataSource), intent(in) :: src
        valid = set_ribbon_source(self, 1, 1, src)
    end function ribbon_set_start_x_data

    logical function ribbon_set_start_y_data(self, src) result(valid)
        class(RibbonArtist), intent(inout) :: self
        type(DataSource), intent(in) :: src
        valid = set_ribbon_source(self, 1, 2, src)
    end function ribbon_set_start_y_data

    logical function ribbon_set_start_z_data(self, src) result(valid)
        class(RibbonArtist), intent(inout) :: self
        type(DataSource), intent(in) :: src
        valid = set_ribbon_source(self, 1, 3, src)
    end function ribbon_set_start_z_data

    logical function ribbon_set_end_x_data(self, src) result(valid)
        class(RibbonArtist), intent(inout) :: self
        type(DataSource), intent(in) :: src
        valid = set_ribbon_source(self, 2, 1, src)
    end function ribbon_set_end_x_data

    logical function ribbon_set_end_y_data(self, src) result(valid)
        class(RibbonArtist), intent(inout) :: self
        type(DataSource), intent(in) :: src
        valid = set_ribbon_source(self, 2, 2, src)
    end function ribbon_set_end_y_data

    logical function ribbon_set_end_z_data(self, src) result(valid)
        class(RibbonArtist), intent(inout) :: self
        type(DataSource), intent(in) :: src
        valid = set_ribbon_source(self, 2, 3, src)
    end function ribbon_set_end_z_data

    !> Set component k of the start (side 1) or end (side 2) sources.
    logical function set_ribbon_source(self, side, k, src) result(valid)
        class(RibbonArtist), intent(inout) :: self
        integer, intent(in) :: side, k
        type(DataSource), intent(in) :: src
        if (side == 1) then
            self%start_(k) = src
        else
            self%end_(k) = src
        end if
        self%default_data = .false.
        self%seen_generation = -1
        valid = .true.
        if (associated(self%traj)) valid = self%traj%verify_data(self%start_) .and. self%traj%verify_data(self%end_)
    end function set_ribbon_source

    pure function ribbon_start_data_sources(self) result(s)
        class(RibbonArtist), intent(in) :: self
        type(DataSource) :: s(3)
        s = self%start_
    end function ribbon_start_data_sources

    pure function ribbon_end_data_sources(self) result(s)
        class(RibbonArtist), intent(in) :: self
        type(DataSource) :: s(3)
        s = self%end_
    end function ribbon_end_data_sources

    !> The surface's color: [r, g, b] (alpha unchanged) or [r, g, b, a]; its
    !> alpha is the surface's opacity (also with the trajectory's colors).
    subroutine ribbon_set_color(self, color)
        class(RibbonArtist), intent(inout) :: self
        real(wp), intent(in) :: color(:)
        call set_rgba(self%color_, color)
        call ribbon_style(self)
        if (self%chain%is_colored()) self%seen_generation = -1    ! (points without colors redrawn)
    end subroutine ribbon_set_color

    pure function ribbon_color(self) result(c)
        class(RibbonArtist), intent(in) :: self
        real(wp) :: c(4)
        c = self%color_
    end function ribbon_color

    !> Whether a trajectory with colors colors the surface (default true):
    !> each point's pair in its color (points without one yet in the artist's),
    !> times the artist's alpha. False: all in the artist's color.
    subroutine ribbon_set_point_colors(self, enabled)
        class(RibbonArtist), intent(inout) :: self
        logical, intent(in) :: enabled
        self%point_colors_ = enabled
    end subroutine ribbon_set_point_colors

    pure logical function ribbon_point_colors(self)
        class(RibbonArtist), intent(in) :: self
        ribbon_point_colors = self%point_colors_
    end function ribbon_point_colors

    pure integer function ribbon_num_chunks(self)
        class(RibbonArtist), intent(in) :: self
        ribbon_num_chunks = self%chain%num_chunks()
    end function ribbon_num_chunks

    !> Number of trajectory points the surface is drawn through now (all, or
    !> in trace mode those passed).
    pure integer function ribbon_num_drawn(self)
        class(RibbonArtist), intent(in) :: self
        ribbon_num_drawn = self%n_drawn
    end function ribbon_num_drawn

    !> The pair at trajectory point i (1 to num_drawn) as drawn, its start a
    !> and end b: read back from the buffers.
    subroutine drawn_pair(self, i, a, b)
        class(RibbonArtist), intent(in) :: self
        integer, intent(in) :: i
        real(wp), intent(out) :: a(3), b(3)
        a = self%chain%point(2 * i - 1)
        b = self%chain%point(2 * i)
    end subroutine drawn_pair

    !> The color of the pair at trajectory point i as drawn (rgba: the
    !> trajectory's color times the artist's alpha, or the artist's color).
    function ribbon_drawn_color(self, i) result(rgba)
        class(RibbonArtist), intent(in) :: self
        integer, intent(in) :: i
        real(wp) :: rgba(4)
        rgba = self%color_
        if (.not. self%chain%is_colored()) return
        rgba = self%chain%point_color(2 * i - 1)
        rgba(4) = rgba(4) * self%color_(4)
    end function ribbon_drawn_color

    !> In trace mode, the pair at the current time (the tail's end) as drawn;
    !> false if no tail is drawn.
    logical function ribbon_trace_end(self, a, b)
        class(RibbonArtist), intent(in) :: self
        real(wp), intent(out) :: a(3), b(3)
        real(c_float), dimension(:,:,:), pointer :: p
        a = 0.0_wp
        b = 0.0_wp
        ribbon_trace_end = self%tail_shown
        if (.not. ribbon_trace_end) return
        call line_buffer(self%tail, p)
        if (.not. associated(p)) return
        a = real(p(1:3, 1, 3), wp) + real(p(1:3, 2, 3), wp)
        b = real(p(1:3, 1, 4), wp) + real(p(1:3, 2, 4), wp)
    end function ribbon_trace_end

    subroutine ribbon_build(self, viewer, group)
        class(RibbonArtist), intent(inout) :: self
        type(c_ptr), intent(in) :: viewer, group
        self%group = group
        self%compile = .false.
        call init_ribbon_chain(self)
        call self%chain%reserve(2 * ribbon_points(self))  ! compiled with the group
        self%compile = c_associated(viewer)
        call self%chain%set_compile(self%compile)
        self%tail = vkf_ribbon_create(4_c_int, 1_c_int)   ! (white unless the points' colors are drawn)
        if (c_associated(self%tail)) call check(vkf_node_add_child(group, self%tail))
        call ribbon_style(self)
        self%seen_generation = -1
    end subroutine ribbon_build

    !> The chain, colored if the trajectory's colors are drawn.
    subroutine init_ribbon_chain(self)
        type(RibbonArtist), intent(inout) :: self
        logical :: colored
        colored = self%point_colors_ .and. associated(self%traj)
        if (colored) colored = self%traj%num_colors() > 0
        call self%chain%init(self%group, CHAIN_RIBBON, colored=colored)
        call self%chain%set_compile(self%compile)
        self%n_colored = 0
    end subroutine init_ribbon_chain

    !> Whether the trajectory's colors are drawn: wanted, and it has some.
    logical function ribbon_uses_colors(self) result(uses)
        type(RibbonArtist), intent(in) :: self
        uses = self%point_colors_ .and. associated(self%traj)
        if (uses) uses = self%traj%num_colors() > 0
    end function ribbon_uses_colors

    subroutine ribbon_release(self)
        class(RibbonArtist), intent(inout) :: self
        call self%chain%release()
        if (c_associated(self%tail)) call vkf_node_delete(self%tail)
        self%tail = c_null_ptr
        self%group = c_null_ptr
        self%n_written = 0
        self%n_drawn = 0
        self%n_colored = 0
        self%tail_shown = .false.
        call artist_release(self)
    end subroutine ribbon_release

    subroutine ribbon_update(self, t)
        class(RibbonArtist), intent(inout) :: self
        real(wp), intent(in) :: t
        integer :: n, i1
        logical :: changed, redraw
        if (.not. self%chain%is_built()) return
        n = ribbon_points(self)

        ! the trajectory's colors drawn or not: the chain made again
        if (ribbon_uses_colors(self) .neqv. self%chain%is_colored()) then
            call self%chain%detach()
            call init_ribbon_chain(self)
            call ribbon_style(self)
            self%seen_generation = -1
        end if

        ! new points, or a redraw (cleared, new trajectory or data sources)
        changed = data_changed(self, redraw)
        if (redraw .or. .not. associated(self%traj)) then
            self%n_written = 0
            self%n_colored = 0
        end if
        if (changed) then
            i1 = max(1, min(self%n_written, n))          ! from the last point written (set_optional)
            if (self%chain%is_colored()) i1 = max(1, min(i1, self%n_colored))   ! (and colors added since)
            call self%chain%reserve(2 * n)
            if (n >= i1) call write_ribbon(self, i1, n)
            self%n_written = n
        end if

        if (.not. self%trace) then
            self%n_drawn = self%n_written
            call self%chain%draw(2 * self%n_drawn)
            if (c_associated(self%tail)) call check(vkf_line_set_range(self%tail, 1_c_int, 0_c_int))
            self%tail_shown = .false.
            self%tail_time = huge(1.0_wp)
            return
        end if
        ! trace mode: the quads passed, and one to the pair at time t
        if (.not. changed .and. t == self%tail_time) return
        self%n_drawn = 0
        if (associated(self%traj)) self%n_drawn = points_passed(self%traj, t, self%n_written)
        call self%chain%draw(2 * self%n_drawn)
        call update_ribbon_tail(self, t, self%n_drawn)
        self%tail_time = t
    end subroutine ribbon_update

    !> Number of trajectory points to draw the surface through (none if all
    !> sources are zero).
    integer function ribbon_points(self) result(n)
        type(RibbonArtist), intent(in) :: self
        n = 0
        if (all(self%start_%src == SOURCE_ZERO) .and. all(self%end_%src == SOURCE_ZERO)) return
        n = min(source_points(self%traj, self%start_), source_points(self%traj, self%end_))
    end function ribbon_points

    !> Write the pairs of trajectory points i1 .. i2 into the chain, with their
    !> colors (and upload the chunks written to).
    subroutine write_ribbon(self, i1, i2)
        type(RibbonArtist), intent(inout) :: self
        integer, intent(in) :: i1, i2
        real(c_float), dimension(:,:,:), pointer :: p
        real(c_float), dimension(:,:), pointer :: c, col
        real(c_float) :: own(4)
        real(wp), allocatable :: v(:,:)
        integer :: j, lo, hi, ia, ib, m, k
        do j = 1, self%chain%num_chunks()
            call self%chain%slice(j, 2 * i1 - 1, 2 * i2, lo, hi, p)  ! chunks hold whole pairs
            if (.not. associated(p)) cycle
            ia = (lo + 1) / 2                             ! the trajectory points of the pairs
            ib = hi / 2
            allocate(v(3, ib - ia + 1))
            call self%traj%points(ia, ib, self%start_, v)
            call split_points(v, p(:, :, 1:hi-lo+1:2))
            call self%traj%points(ia, ib, self%end_, v)
            call split_points(v, p(:, :, 2:hi-lo+1:2))
            deallocate(v)
        end do
        if (self%chain%is_colored()) then                 ! each pair in its point's color
            call self%traj%colors(col)
            m = self%traj%num_colors()
            own = real(self%color_, c_float)
            own(4) = 1.0_c_float                          ! (the style has the alpha)
            do j = 1, self%chain%num_chunks()
                call self%chain%color_slice(j, 2 * i1 - 1, 2 * i2, lo, hi, c)
                if (.not. associated(c)) cycle
                do k = (lo + 1) / 2, hi / 2
                    if (k <= m) then
                        c(:, 2*k-lo:2*k-lo+1) = spread(col(:, k), 2, 2)
                    else
                        c(:, 2*k-lo:2*k-lo+1) = spread(own, 2, 2)
                    end if
                end do
            end do
            self%n_colored = min(m, i2)
        end if
        call self%chain%written(2 * i1 - 1, 2 * i2)
    end subroutine write_ribbon

    !> The tail: the quad from the pair at point k to the pair interpolated at
    !> time t (none if before the first point or at/after the last).
    subroutine update_ribbon_tail(self, t, k)
        type(RibbonArtist), intent(inout) :: self
        real(wp), intent(in) :: t
        integer, intent(in) :: k
        real(c_float), dimension(:,:,:), pointer :: p
        real(c_float), dimension(:,:), pointer :: c
        real(wp) :: t0, t1, frac, xyz(3, 4), c0(4), c1(4)
        integer(c_int) :: capacity
        type(c_ptr) :: data
        self%tail_shown = .false.
        if (.not. c_associated(self%tail)) return
        if (k < 1 .or. k >= self%n_written) then
            call check(vkf_line_set_range(self%tail, 1_c_int, 0_c_int))
            return
        end if
        t0 = self%traj%time(k)
        t1 = self%traj%time(k + 1)
        frac = 0.0_wp
        if (t1 /= t0) frac = max(0.0_wp, min(1.0_wp, (t - t0) / (t1 - t0)))
        xyz(:, 1) = self%traj%point(k, self%start_)
        xyz(:, 2) = self%traj%point(k, self%end_)
        xyz(:, 3) = xyz(:, 1) + frac * (self%traj%point(k + 1, self%start_) - xyz(:, 1))
        xyz(:, 4) = xyz(:, 2) + frac * (self%traj%point(k + 1, self%end_) - xyz(:, 2))
        call line_buffer(self%tail, p)
        if (.not. associated(p)) return
        call split_points(xyz, p(:, :, 1:4))
        data = vkf_line_colors(self%tail, capacity)
        if (c_associated(data) .and. capacity >= 4) then
            call c_f_pointer(data, c, [4, 4])
            if (self%chain%is_colored()) then
                c0 = ribbon_point_rgba(self, k)
                c1 = ribbon_point_rgba(self, k + 1)
                c(:, 1:2) = spread(real(c0, c_float), 2, 2)
                c(:, 3:4) = spread(real(c0 + frac * (c1 - c0), c_float), 2, 2)
            else
                c = 1.0_c_float
            end if
        end if
        call check(vkf_line_points_changed(self%tail, 4_c_int))
        self%tail_shown = .true.
    end subroutine update_ribbon_tail

    !> Color of point i in a colored chain: the trajectory's, or the artist's
    !> (opaque: the style has the alpha) if it has none yet.
    function ribbon_point_rgba(self, i) result(rgba)
        type(RibbonArtist), intent(in) :: self
        integer, intent(in) :: i
        real(wp) :: rgba(4)
        if (i <= self%traj%num_colors()) then
            rgba = self%traj%color(i)
        else
            rgba = [self%color_(1:3), 1.0_wp]
        end if
    end function ribbon_point_rgba

    !> The style: the artist's color, or white with its alpha if the points'
    !> colors are drawn (the shader multiplies them by it).
    subroutine ribbon_style(self)
        type(RibbonArtist), intent(inout) :: self
        real(wp) :: rgba(4)
        rgba = self%color_
        if (self%chain%is_colored()) rgba(1:3) = 1.0_wp
        call self%chain%set_style(rgba, 1.0_wp)
        if (c_associated(self%tail)) call check(line_set_style(self%tail, real(rgba, c_float), 1.0_c_float))
    end subroutine ribbon_style

    ! ==================================================================
    ! MarkerArtist

    !> A marker artist for `traj` (optional; without one, or with all data
    !> sources zero, a start marker at the origin: in a DrawableTrajectory that
    !> follows a trajectory, or is a child of a frame that does, it marks the
    !> current position, as in OpenFrames), drawing `markers` (an ior
    !> of MARKER_START, MARKER_INTERMEDIATE, MARKER_END; default start and end)
    !> of `size` window units (default 10) in `color` (all markers; default red
    !> start and end, white intermediate).
    function new_marker_artist(traj, markers, size, color) result(a)
        type(Trajectory), intent(in), target, optional :: traj
        integer, intent(in), optional :: markers
        real(wp), intent(in), optional :: size, color(:)
        type(MarkerArtist) :: a
        a%sources_ = position_sources(3)
        a%image_file = ''
        if (present(traj)) call a%set_trajectory(traj)
        if (present(markers)) a%markers_ = markers
        if (present(size)) a%size_ = size
        if (present(color)) call a%set_marker_color(ior(ior(MARKER_START, MARKER_INTERMEDIATE), MARKER_END), color)
    end function new_marker_artist

    !> Draw `traj`; with the default data sources, its positions (per its DOF).
    subroutine marker_set_trajectory(self, traj)
        class(MarkerArtist), intent(inout) :: self
        type(Trajectory), intent(in), target :: traj
        self%traj => traj
        if (self%default_data) self%sources_ = position_sources(traj%dof())
        self%seen_generation = -1
    end subroutine marker_set_trajectory

    !> Where the x (y, z) of the markers come from. Returns whether the
    !> trajectory has those data.
    logical function marker_set_x_data(self, src) result(valid)
        class(MarkerArtist), intent(inout) :: self
        type(DataSource), intent(in) :: src
        valid = set_marker_source(self, 1, src)
    end function marker_set_x_data

    logical function marker_set_y_data(self, src) result(valid)
        class(MarkerArtist), intent(inout) :: self
        type(DataSource), intent(in) :: src
        valid = set_marker_source(self, 2, src)
    end function marker_set_y_data

    logical function marker_set_z_data(self, src) result(valid)
        class(MarkerArtist), intent(inout) :: self
        type(DataSource), intent(in) :: src
        valid = set_marker_source(self, 3, src)
    end function marker_set_z_data

    logical function set_marker_source(self, k, src) result(valid)
        class(MarkerArtist), intent(inout) :: self
        integer, intent(in) :: k
        type(DataSource), intent(in) :: src
        self%sources_(k) = src
        self%default_data = .false.
        call redraw_markers(self)
        valid = .true.
        if (associated(self%traj)) valid = self%traj%verify_data(self%sources_)
    end function set_marker_source

    pure function marker_data_sources(self) result(s)
        class(MarkerArtist), intent(in) :: self
        type(DataSource) :: s(3)
        s = self%sources_
    end function marker_data_sources

    !> Which markers to draw: an ior of MARKER_START, MARKER_INTERMEDIATE and
    !> MARKER_END (0 for none).
    subroutine set_markers(self, markers)
        class(MarkerArtist), intent(inout) :: self
        integer, intent(in) :: markers
        if (markers == self%markers_) return
        self%markers_ = markers
        call redraw_markers(self)
    end subroutine set_markers

    pure integer function markers(self)
        class(MarkerArtist), intent(in) :: self
        markers = self%markers_
    end function markers

    !> The color of the markers in `which` (an ior of MARKER_* flags): [r, g,
    !> b] (alpha unchanged) or [r, g, b, a].
    subroutine set_marker_color(self, which, color)
        class(MarkerArtist), intent(inout) :: self
        integer, intent(in) :: which
        real(wp), intent(in) :: color(:)
        if (iand(which, MARKER_START) /= 0) call set_rgba(self%start_color, color)
        if (iand(which, MARKER_END) /= 0) call set_rgba(self%end_color, color)
        if (iand(which, MARKER_INTERMEDIATE) /= 0) call set_rgba(self%intermediate_color, color)
        call marker_style(self)
    end subroutine set_marker_color

    !> The color of the start, intermediate or end markers (`which`: one MARKER_* flag).
    pure function marker_color(self, which) result(c)
        class(MarkerArtist), intent(in) :: self
        integer, intent(in) :: which
        real(wp) :: c(4)
        select case (which)
        case (MARKER_START)
            c = self%start_color
        case (MARKER_END)
            c = self%end_color
        case default
            c = self%intermediate_color
        end select
    end function marker_color

    !> Marker size in window units (logical pixels; > 0).
    subroutine set_marker_size(self, size)
        class(MarkerArtist), intent(inout) :: self
        real(wp), intent(in) :: size
        if (size <= 0.0_wp) return
        self%size_ = size
        call marker_style(self)
    end subroutine set_marker_size

    pure real(wp) function marker_size(self)
        class(MarkerArtist), intent(in) :: self
        marker_size = self%size_
    end function marker_size

    !> Marker shape: a MARKER_* shape constant (MARKER_DISK to MARKER_SPIRAL;
    !> MARKER_IMAGE after set_marker_image). These are OpenFrames' disk and its
    !> marker shaders (Demos/Shaders/Marker_*.frag), built in.
    subroutine set_marker_shape(self, shape)
        class(MarkerArtist), intent(inout) :: self
        integer, intent(in) :: shape
        self%shape_ = max(MARKER_DISK, min(MARKER_IMAGE, shape))
        call marker_style(self)
    end subroutine set_marker_shape

    pure integer function marker_shape(self)
        class(MarkerArtist), intent(in) :: self
        marker_shape = self%shape_
    end function marker_shape

    !> Draw the markers as an image file (tinted by the marker color; its
    !> transparent parts left out), or as a disk again with ''. Returns false
    !> if the image cannot be read (the markers are then unchanged).
    logical function set_marker_image(self, file) result(ok)
        class(MarkerArtist), intent(inout) :: self
        character(len=*), intent(in) :: file
        type(c_ptr) :: image
        ok = .true.
        if (len_trim(file) == 0) then
            image = c_null_ptr
            self%shape_ = MARKER_DISK
        else
            image = vkf_image_read(trim(file), len_trim(file, kind=c_int))
            ok = c_associated(image)
            if (.not. ok) return
            self%shape_ = MARKER_IMAGE
        end if
        if (c_associated(self%image)) call vkf_image_delete(self%image)
        self%image = image
        self%image_file = trim(file)
        call rebuild_markers(self)                    ! the image is part of the marker nodes
    end function set_marker_image

    function marker_image_file(self) result(file)
        class(MarkerArtist), intent(in) :: self
        character(len=:), allocatable :: file
        file = self%image_file
    end function marker_image_file

    !> Shrink markers farther from the camera than the trajectory's size
    !> (OpenFrames' auto-attenuation; see shaders/marker.vert).
    subroutine set_auto_attenuate(self, attenuate)
        class(MarkerArtist), intent(inout) :: self
        logical, intent(in) :: attenuate
        self%attenuate = attenuate
        call marker_style(self)
    end subroutine set_auto_attenuate

    pure logical function auto_attenuate(self)
        class(MarkerArtist), intent(in) :: self
        auto_attenuate = self%attenuate
    end function auto_attenuate

    !> How intermediate markers are spaced: INTERMEDIATE_TIME, _DISTANCE or _DATA.
    subroutine set_intermediate_type(self, type)
        class(MarkerArtist), intent(inout) :: self
        integer, intent(in) :: type
        if (type == self%intermediate_type_) return
        self%intermediate_type_ = type
        call redraw_markers(self)
    end subroutine set_intermediate_type

    pure integer function intermediate_type(self)
        class(MarkerArtist), intent(in) :: self
        intermediate_type = self%intermediate_type_
    end function intermediate_type

    !> Spacing of intermediate markers (> 0): time units, arc length, or a
    !> number of data points (rounded down, at least 1).
    subroutine set_intermediate_spacing(self, spacing)
        class(MarkerArtist), intent(inout) :: self
        real(wp), intent(in) :: spacing
        if (spacing <= 0.0_wp .or. spacing == self%spacing_) return
        self%spacing_ = spacing
        call redraw_markers(self)
    end subroutine set_intermediate_spacing

    pure real(wp) function intermediate_spacing(self)
        class(MarkerArtist), intent(in) :: self
        intermediate_spacing = self%spacing_
    end function intermediate_spacing

    !> Count intermediate markers from the start (MARKER_START, default) or
    !> from the end (MARKER_END; recomputed whenever points are added).
    subroutine set_intermediate_direction(self, direction)
        class(MarkerArtist), intent(inout) :: self
        integer, intent(in) :: direction
        if (direction == self%direction_) return
        self%direction_ = merge(MARKER_END, MARKER_START, direction == MARKER_END)
        call redraw_markers(self)
    end subroutine set_intermediate_direction

    pure integer function intermediate_direction(self)
        class(MarkerArtist), intent(in) :: self
        intermediate_direction = self%direction_
    end function intermediate_direction

    !> Number of intermediate markers drawn now.
    pure integer function num_intermediate(self)
        class(MarkerArtist), intent(in) :: self
        num_intermediate = self%n_inter_drawn
    end function num_intermediate

    !> Where marker `which` (MARKER_START, MARKER_END, or MARKER_INTERMEDIATE
    !> number i) is drawn, read back from its buffer; false if not drawn.
    logical function drawn_marker(self, which, v, i) result(shown)
        class(MarkerArtist), intent(in) :: self
        integer, intent(in) :: which
        real(wp), intent(out) :: v(3)
        integer, intent(in), optional :: i
        v = 0.0_wp
        select case (which)
        case (MARKER_START)
            shown = self%start_shown
            if (shown) v = self%start_node%point(1)
        case (MARKER_END)
            shown = self%end_shown
            if (shown) v = self%end_node%point(1)
        case default
            shown = .false.
            if (.not. present(i)) return
            shown = i >= 1 .and. i <= self%n_inter_drawn
            if (shown) v = self%inter%point(i)
        end select
    end function drawn_marker

    subroutine marker_build(self, viewer, group)
        class(MarkerArtist), intent(inout) :: self
        type(c_ptr), intent(in) :: viewer, group
        self%group = group
        self%shown = c_associated(viewer)
        call build_marker_nodes(self, compile=.false.)
    end subroutine marker_build

    !> The start, end and intermediate marker nodes, in the artist's group
    !> (compiled now if it is already shown).
    subroutine build_marker_nodes(self, compile)
        type(MarkerArtist), intent(inout) :: self
        logical, intent(in) :: compile
        call self%start_node%init(self%group, CHAIN_MARKERS, self%image)
        call self%end_node%init(self%group, CHAIN_MARKERS, self%image)
        call self%inter%init(self%group, CHAIN_MARKERS, self%image)
        call self%start_node%set_compile(compile .and. self%shown)
        call self%end_node%set_compile(compile .and. self%shown)
        call self%inter%set_compile(compile .and. self%shown)
        call self%start_node%reserve(1)
        call self%end_node%reserve(1)
        call self%inter%reserve(1)
        call self%start_node%set_compile(self%shown)
        call self%end_node%set_compile(self%shown)
        call self%inter%set_compile(self%shown)
        call marker_style(self)
        call redraw_markers(self)
    end subroutine build_marker_nodes

    !> New marker nodes (for a new image), in place of the old ones.
    subroutine rebuild_markers(self)
        type(MarkerArtist), intent(inout) :: self
        if (.not. c_associated(self%group)) return
        call self%start_node%detach()
        call self%end_node%detach()
        call self%inter%detach()
        call build_marker_nodes(self, compile=.true.)
    end subroutine rebuild_markers

    !> Compute and write everything again at the next update.
    subroutine redraw_markers(self)
        type(MarkerArtist), intent(inout) :: self
        self%seen_generation = -1
        self%drawn = .false.
    end subroutine redraw_markers

    subroutine marker_release(self)
        class(MarkerArtist), intent(inout) :: self
        call self%start_node%release()
        call self%end_node%release()
        call self%inter%release()
        self%group = c_null_ptr
        self%start_shown = .false.
        self%end_shown = .false.
        self%n_inter_drawn = 0
        call redraw_markers(self)
        call artist_release(self)
    end subroutine marker_release

    subroutine marker_style(self)
        type(MarkerArtist), intent(inout) :: self
        real(wp) :: r
        r = 0.0_wp
        if (self%attenuate) r = self%radius
        call self%start_node%set_marker_style(self%start_color, self%size_, self%shape_, r)
        call self%end_node%set_marker_style(self%end_color, self%size_, self%shape_, r)
        call self%inter%set_marker_style(self%intermediate_color, self%size_, self%shape_, r)
    end subroutine marker_style

    subroutine marker_update(self, t)
        class(MarkerArtist), intent(inout) :: self
        real(wp), intent(in) :: t
        logical :: changed, redraw, zero
        integer :: n, k
        if (.not. c_associated(self%group)) return
        zero = all(self%sources_%src == SOURCE_ZERO) .or. .not. associated(self%traj)   ! (a marker at the origin)
        if (zero) then
            n = 1                                         ! a marker at the origin
        else
            n = source_points(self%traj, self%sources_)
        end if

        changed = data_changed(self, redraw)
        if (.not. self%drawn) then
            changed = .true.
            redraw = .true.
            self%drawn = .true.
        end if
        if (changed) then
            if (redraw .or. n < self%n_seen) call reset_markers(self)
            call update_bounds(self, n, zero)
            call write_marker(self%start_node, marker_point(self, 1, zero))
            if (n > 1) call write_marker(self%end_node, marker_point(self, n, zero))
            if (iand(self%markers_, MARKER_INTERMEDIATE) /= 0 .and. .not. zero .and. n > 2) call add_intermediates(self, n)
        end if

        ! what is drawn: everything, or in trace mode what was passed
        k = n
        if (self%trace .and. .not. zero) k = points_passed(self%traj, t, n)
        self%start_shown = iand(self%markers_, MARKER_START) /= 0 .and. n >= 1 .and. k >= 1
        call self%start_node%draw(merge(1, 0, self%start_shown))
        self%end_shown = iand(self%markers_, MARKER_END) /= 0 .and. n > 1 .and. k >= n   ! (trace: once reached)
        call self%end_node%draw(merge(1, 0, self%end_shown))
        self%n_inter_drawn = self%n_inter_written
        if (self%trace .and. self%n_inter_written > 0) self%n_inter_drawn = markers_passed(self, t)
        call self%inter%draw(self%n_inter_drawn)
    end subroutine marker_update

    function marker_point(self, i, zero) result(v)
        type(MarkerArtist), intent(in) :: self
        integer, intent(in) :: i
        logical, intent(in) :: zero
        real(wp) :: v(3)
        v = 0.0_wp
        if (.not. zero) v = self%traj%point(i, self%sources_)
    end function marker_point

    !> The point at time t, interpolated between points k and k + 1.
    function point_at_time(traj, sources, t, k) result(v)
        type(Trajectory), intent(in) :: traj
        type(DataSource), intent(in) :: sources(3)
        real(wp), intent(in) :: t
        integer, intent(in) :: k
        real(wp) :: v(3), t0, t1, frac
        v = traj%point(k, sources)
        if (k >= traj%num_times()) return
        t0 = traj%time(k)
        t1 = traj%time(k + 1)
        frac = 0.0_wp
        if (t1 /= t0) frac = max(0.0_wp, min(1.0_wp, (t - t0) / (t1 - t0)))
        v = v + frac * (traj%point(k + 1, sources) - v)
    end function point_at_time

    !> A single marker's point (in a chain of one marker), uploaded.
    subroutine write_marker(chain, v)
        type(LineChain), intent(inout) :: chain
        real(wp), intent(in) :: v(3)
        real(c_float), dimension(:,:,:), pointer :: p
        integer :: lo, hi
        if (chain%num_chunks() < 1) return
        call chain%slice(1, 1, 1, lo, hi, p)
        if (.not. associated(p)) return
        call split_points(reshape(v, [3, 1]), p)
        call chain%written(1, 1)
    end subroutine write_marker

    !> Start the intermediate markers and the bounds again.
    subroutine reset_markers(self)
        type(MarkerArtist), intent(inout) :: self
        self%n_seen = 0
        self%n_inter = 0
        self%n_inter_written = 0
        self%next_index = 0                           ! (0: not started)
    end subroutine reset_markers

    !> The bounds of the points (for the attenuation radius: OpenFrames uses
    !> the artist's bounding sphere).
    subroutine update_bounds(self, n, zero)
        type(MarkerArtist), intent(inout) :: self
        integer, intent(in) :: n
        logical, intent(in) :: zero
        real(wp), allocatable :: v(:,:)
        real(wp) :: r
        if (zero) then
            self%lo = 0.0_wp
            self%hi = 0.0_wp
        else if (n > self%n_seen) then                      ! the new points, a range at a time
            allocate(v(3, n - self%n_seen))
            call self%traj%points(self%n_seen + 1, n, self%sources_, v)
            if (self%n_seen == 0) then
                self%lo = v(:, 1)
                self%hi = v(:, 1)
            end if
            self%lo = min(self%lo, minval(v, dim=2))
            self%hi = max(self%hi, maxval(v, dim=2))
        end if
        self%n_seen = n
        r = 0.5_wp * norm2(self%hi - self%lo)
        if (r < 1.0e-8_wp) r = 0.0_wp                 ! too small: no attenuation (as OpenFrames)
        if (r /= self%radius) then
            self%radius = r
            if (self%attenuate) call marker_style(self)
        end if
    end subroutine update_bounds

    !> Compute the intermediate markers not computed yet (all of them when
    !> counted from the end), and write them.
    subroutine add_intermediates(self, n)
        type(MarkerArtist), intent(inout) :: self
        integer, intent(in) :: n
        integer :: first
        if (self%direction_ == MARKER_END) then          ! everything changes when points are added
            self%n_inter = 0
            self%n_inter_written = 0
            self%next_index = 0
        end if
        select case (self%intermediate_type_)
        case (INTERMEDIATE_TIME)
            call intermediates_by_time(self, n)
        case (INTERMEDIATE_DISTANCE)
            call intermediates_by_distance(self, n)
        case default
            call intermediates_by_data(self, n)
        end select
        if (self%direction_ == MARKER_END) then          ! in trajectory order
            self%inter_xyz(:, 1:self%n_inter) = self%inter_xyz(:, self%n_inter:1:-1)
            self%inter_t(1:self%n_inter) = self%inter_t(self%n_inter:1:-1)
        end if
        first = self%n_inter_written + 1
        if (self%n_inter >= first) call write_intermediates(self, first, self%n_inter)
        self%n_inter_written = self%n_inter
    end subroutine add_intermediates

    !> Index of the j-th point counted in the intermediate markers' direction.
    pure integer function counted(self, j, n)
        type(MarkerArtist), intent(in) :: self
        integer, intent(in) :: j, n
        counted = merge(n + 1 - j, j, self%direction_ == MARKER_END)
    end function counted

    !> Every `spacing` time units after the first time, before the last
    !> (counted from the start or the end).
    subroutine intermediates_by_time(self, n)
        type(MarkerArtist), intent(inout) :: self
        integer, intent(in) :: n
        real(wp) :: t_first, t_last, step, tm
        integer :: status, index
        t_first = self%traj%time(counted(self, 1, n))
        t_last = self%traj%time(counted(self, n, n))
        step = sign(self%spacing_, t_last - t_first)
        if (self%next_index == 0) then
            self%next_time = t_first + step
            self%next_index = 1
        end if
        do while (sign(1.0_wp, step) * (t_last - self%next_time) > 0.0_wp)
            tm = self%next_time
            status = self%traj%time_index(tm, index)
            index = max(1, min(n - 1, index))
            call push_intermediate(self, point_at_time(self%traj, self%sources_, tm, index), tm)
            self%next_time = self%next_time + step
        end do
    end subroutine intermediates_by_time

    !> Every int(spacing)-th point after the first, before the last.
    subroutine intermediates_by_data(self, n)
        type(MarkerArtist), intent(inout) :: self
        integer, intent(in) :: n
        integer :: step, i
        step = max(1, int(self%spacing_))
        if (self%next_index == 0) self%next_index = 1 + step
        do while (self%next_index < n)
            i = counted(self, self%next_index, n)
            call push_intermediate(self, self%traj%point(i, self%sources_), self%traj%time(i))
            self%next_index = self%next_index + step
        end do
    end subroutine intermediates_by_data

    !> Every `spacing` of arc length (the points joined by straight lines).
    subroutine intermediates_by_distance(self, n)
        type(MarkerArtist), intent(inout) :: self
        integer, intent(in) :: n
        real(wp) :: p(3), frac, t0, t1
        integer :: j
        if (self%next_index == 0) then
            self%next_index = 2
            self%arc = 0.0_wp
            self%prev_arc = 0.0_wp
            self%target = self%spacing_
            self%prev_point = self%traj%point(counted(self, 1, n), self%sources_)
        end if
        do j = self%next_index, n
            p = self%traj%point(counted(self, j, n), self%sources_)
            self%arc = self%arc + norm2(p - self%prev_point)
            t0 = self%traj%time(counted(self, j - 1, n))
            t1 = self%traj%time(counted(self, j, n))
            do while (self%arc >= self%target)
                frac = (self%target - self%prev_arc) / (self%arc - self%prev_arc)
                call push_intermediate(self, self%prev_point + frac * (p - self%prev_point), t0 + frac * (t1 - t0))
                self%target = self%target + self%spacing_
            end do
            self%prev_arc = self%arc
            self%prev_point = p
        end do
        self%next_index = n + 1
    end subroutine intermediates_by_distance

    subroutine push_intermediate(self, v, t)
        type(MarkerArtist), intent(inout) :: self
        real(wp), intent(in) :: v(3), t
        real(wp), allocatable :: xyz(:,:), times(:)
        integer :: m
        if (.not. allocated(self%inter_xyz)) allocate(self%inter_xyz(3, 64), self%inter_t(64))
        m = size(self%inter_t)
        if (self%n_inter == m) then
            allocate(xyz(3, 2 * m), times(2 * m))
            xyz(:, 1:m) = self%inter_xyz
            times(1:m) = self%inter_t
            call move_alloc(xyz, self%inter_xyz)
            call move_alloc(times, self%inter_t)
        end if
        self%n_inter = self%n_inter + 1
        self%inter_xyz(:, self%n_inter) = v
        self%inter_t(self%n_inter) = t
    end subroutine push_intermediate

    subroutine write_intermediates(self, i1, i2)
        type(MarkerArtist), intent(inout) :: self
        integer, intent(in) :: i1, i2
        real(c_float), dimension(:,:,:), pointer :: p
        integer :: j, lo, hi
        call self%inter%reserve(i2)
        do j = 1, self%inter%num_chunks()
            call self%inter%slice(j, i1, i2, lo, hi, p)
            if (.not. associated(p)) cycle
            call split_points(self%inter_xyz(:, lo:hi), p)
        end do
        call self%inter%written(i1, i2)
    end subroutine write_intermediates

    !> Number of intermediate markers up to time t (in trajectory order: in the
    !> trajectory's time direction), by bisection.
    integer function markers_passed(self, t) result(k)
        type(MarkerArtist), intent(in) :: self
        real(wp), intent(in) :: t
        real(wp) :: dir
        integer :: low, high, mid
        dir = 1.0_wp
        if (self%traj%num_times() > 1) dir = sign(1.0_wp, self%traj%time(self%traj%num_times()) - self%traj%time(1))
        low = 0                                           ! markers 1 .. low passed
        high = self%n_inter_written + 1                   ! markers high .. not passed
        do while (high - low > 1)
            mid = (low + high) / 2
            if (dir * (self%inter_t(mid) - t) <= 0.0_wp) then
                low = mid
            else
                high = mid
            end if
        end do
        k = low
    end function markers_passed

    ! ==================================================================
    ! DrawableTrajectory

    !> A drawable trajectory frame named `name` (decorations of color `color`).
    function new_drawable_trajectory(name, color) result(d)
        character(len=*), intent(in) :: name
        real(wp), intent(in), optional :: color(:)
        type(DrawableTrajectory) :: d
        d%ReferenceFrame = ReferenceFrame(name, color)
    end function new_drawable_trajectory

    !> Add an artist (it must stay alive while added). Artists added after the
    !> frame is shown are built and compiled at the next update.
    subroutine add_artist(self, a)
        class(DrawableTrajectory), intent(inout) :: self
        class(TrajectoryArtist), intent(in), target :: a
        type(ArtistPointer), allocatable :: tmp(:)
        integer :: k
        do k = 1, self%n_artists
            if (associated(self%artists(k)%p, a)) return
        end do
        if (.not. allocated(self%artists)) allocate(self%artists(4))
        if (self%n_artists == size(self%artists)) then
            allocate(tmp(2 * self%n_artists))
            tmp(1:self%n_artists) = self%artists(1:self%n_artists)
            call move_alloc(tmp, self%artists)
        end if
        self%n_artists = self%n_artists + 1
        self%artists(self%n_artists)%p => a
        self%artists(self%n_artists)%group = c_null_ptr
    end subroutine add_artist

    !> Remove an artist (it stops being drawn; it can be added again, also to
    !> another frame).
    subroutine remove_artist(self, a)
        class(DrawableTrajectory), intent(inout) :: self
        class(TrajectoryArtist), intent(in), target :: a
        integer :: k
        do k = 1, self%n_artists
            if (associated(self%artists(k)%p, a)) then
                if (c_associated(self%artists(k)%group)) then
                    call check(vkf_node_remove_child(self%contents_group, self%artists(k)%group))
                    call vkf_node_delete(self%artists(k)%group)
                    call self%artists(k)%p%release()
                end if
                self%artists(k:self%n_artists-1) = self%artists(k+1:self%n_artists)
                self%n_artists = self%n_artists - 1
                return
            end if
        end do
    end subroutine remove_artist

    subroutine remove_all_artists(self)
        class(DrawableTrajectory), intent(inout) :: self
        do while (self%n_artists > 0)
            call self%remove_artist(self%artists(self%n_artists)%p)
        end do
    end subroutine remove_all_artists

    pure integer function num_artists(self)
        class(DrawableTrajectory), intent(in) :: self
        num_artists = self%n_artists
    end function num_artists

    !> Artist i (1 to num_artists).
    function artist(self, i) result(a)
        class(DrawableTrajectory), intent(in) :: self
        integer, intent(in) :: i
        class(TrajectoryArtist), pointer :: a
        a => self%artists(i)%p
    end function artist

    !> Picking: what the artists draw (see the module documentation), then
    !> the frame's origin (if it shows axes or its name label).
    subroutine drawable_pick(self, cam, to_root, hits, n)
        class(DrawableTrajectory), intent(in), target :: self
        type(PickCamera), intent(in) :: cam
        real(wp), intent(in) :: to_root(4,4)
        type(PickHit), allocatable, intent(inout) :: hits(:)
        integer, intent(inout) :: n
        integer :: k
        if (self%contents_shown()) then
            do k = 1, self%n_artists
                if (.not. self%artists(k)%p%pickable_) cycle
                select type (a => self%artists(k)%p)
                class is (CurveArtist)
                    if (wants(cam, PICK_CURVE)) call curve_pick(a, self, cam, to_root, hits, n)
                class is (SegmentArtist)
                    if (wants(cam, PICK_SEGMENT)) call segment_artist_pick(a, self, cam, to_root, hits, n)
                class is (MarkerArtist)
                    if (wants(cam, PICK_MARKER)) call marker_pick(a, self, cam, to_root, hits, n)
                end select
            end do
        end if
        call self%ReferenceFrame%pick(cam, to_root, hits, n)
    end subroutine drawable_pick

    !> A candidate closer to the cursor than the best so far (or as close, and nearer).
    pure logical function better(pixels, depth, best_pixels, best_depth)
        real(wp), intent(in) :: pixels, depth, best_pixels, best_depth
        better = pixels < best_pixels - 0.5_wp .or. (pixels <= best_pixels + 0.5_wp .and. depth < best_depth)
    end function better

    !> A curve's best hit: on the segments drawn (and the trace tail).
    subroutine curve_pick(a, frame, cam, m, hits, n)
        class(CurveArtist), intent(in), target :: a
        class(ReferenceFrame), intent(in), target :: frame
        type(PickCamera), intent(in) :: cam
        real(wp), intent(in) :: m(4,4)
        type(PickHit), allocatable, intent(inout) :: hits(:)
        integer, intent(inout) :: n
        real(wp), dimension(:,:), pointer :: pos
        real(wp) :: extra, lo(3), hi(3), c(3), p0(3), p1(3), px, s, depth, best_px, best_depth, best_s, t0, t1, frac
        integer :: j, first, count, i1, i2, i, best_i, nd
        logical :: xyz, tail
        type(PickHit) :: h
        if (.not. associated(a%traj) .or. .not. a%chain%is_built()) return
        nd = a%n_drawn
        if (nd < 1) return
        extra = 0.5_wp * a%width_ * cam%pixel_ratio
        xyz = is_position_xyz(a%sources_)
        if (xyz) call a%traj%positions(pos)
        best_px = huge(1.0_wp)
        best_depth = huge(1.0_wp)
        best_i = 0
        best_s = 0.0_wp
        tail = .false.
        if (nd == 1) then                                       ! (a single point)
            if (point_pick(cam, root_point(1), px, depth, extra)) then
                best_px = px
                best_depth = depth
                best_i = 1
            end if
        end if
        do j = 1, a%chain%num_chunks()                          ! the segments drawn, chunk by chunk
            call a%chain%chunk_bounds(j, first, count, lo, hi)
            i1 = first
            i2 = min(first + count - 1, nd)
            if (count < 2 .or. i2 <= i1) cycle
            c = matmul(m(1:3, 1:3), 0.5_wp * (lo + hi)) + m(1:3, 4)
            if (.not. sphere_near(cam, c, 0.5_wp * norm2(hi - lo), extra)) cycle       ! (a chunk far off: skipped)
            p0 = root_point(i1)
            do i = i1, i2 - 1
                p1 = root_point(i + 1)
                if (segment_pick(cam, p0, p1, px, s, depth, extra)) then
                    if (better(px, depth, best_px, best_depth)) then
                        best_px = px
                        best_depth = depth
                        best_i = i
                        best_s = s
                    end if
                end if
                p0 = p1
            end do
        end do
        ! trace mode: the tail from the last point passed to the point at the time drawn
        if (a%trace .and. a%tail_shown .and. nd < a%traj%num_times()) then
            t0 = a%traj%time(nd)
            t1 = a%traj%time(nd + 1)
            frac = 0.0_wp
            if (t1 /= t0) frac = max(0.0_wp, min(1.0_wp, (a%tail_time - t0) / (t1 - t0)))
            p0 = root_point(nd)
            p1 = p0 + frac * (root_point(nd + 1) - p0)
            if (segment_pick(cam, p0, p1, px, s, depth, extra)) then
                if (better(px, depth, best_px, best_depth)) then
                    best_px = px
                    best_depth = depth
                    best_i = nd
                    best_s = s * frac
                    tail = .true.
                end if
            end if
        end if
        if (best_i == 0) return
        h%kind = PICK_CURVE
        h%frame => frame
        h%artist => a
        h%trajectory => a%traj
        h%index = best_i
        h%local = local_point(best_i)
        h%time = a%traj%time(best_i)
        if (best_i < a%traj%num_times() .and. (best_i < nd .or. tail)) then
            h%local = h%local + best_s * (local_point(best_i + 1) - h%local)
            h%time = h%time + best_s * (a%traj%time(best_i + 1) - h%time)
        end if
        h%position = matmul(m(1:3, 1:3), h%local) + m(1:3, 4)
        h%distance = best_depth
        h%pixels = best_px
        call add_pick_hit(hits, n, h)

    contains

        function local_point(i) result(v)
            integer, intent(in) :: i
            real(wp) :: v(3)
            if (xyz) then
                v = pos(:, i)
            else
                v = a%traj%point(i, a%sources_)
            end if
        end function local_point

        function root_point(i) result(v)
            integer, intent(in) :: i
            real(wp) :: v(3)
            v = matmul(m(1:3, 1:3), local_point(i)) + m(1:3, 4)
        end function root_point

    end subroutine curve_pick

    !> A segment artist's best hit: on the segments drawn (as drawn: relative
    !> ends included; a segment of no length isn't drawn).
    subroutine segment_artist_pick(a, frame, cam, m, hits, n)
        class(SegmentArtist), intent(in), target :: a
        class(ReferenceFrame), intent(in), target :: frame
        type(PickCamera), intent(in) :: cam
        real(wp), intent(in) :: m(4,4)
        type(PickHit), allocatable, intent(inout) :: hits(:)
        integer, intent(inout) :: n
        real(wp) :: extra, a0(3), b0(3), px, s, depth, best_px, best_depth, best_s
        integer :: k, best_k
        type(PickHit) :: h
        if (.not. associated(a%traj) .or. a%n_drawn < 1) return
        extra = 0.5_wp * a%width_ * cam%pixel_ratio
        best_px = huge(1.0_wp)
        best_depth = huge(1.0_wp)
        best_k = 0
        best_s = 0.0_wp
        do k = 1, a%n_drawn
            call a%drawn_segment(k, a0, b0)
            if (all(a0 == b0)) cycle
            if (segment_pick(cam, matmul(m(1:3, 1:3), a0) + m(1:3, 4), matmul(m(1:3, 1:3), b0) + m(1:3, 4), &
                             px, s, depth, extra)) then
                if (better(px, depth, best_px, best_depth)) then
                    best_px = px
                    best_depth = depth
                    best_k = k
                    best_s = s
                end if
            end if
        end do
        if (best_k == 0) return
        call a%drawn_segment(best_k, a0, b0)
        h%kind = PICK_SEGMENT
        h%frame => frame
        h%artist => a
        h%trajectory => a%traj
        h%index = best_k
        h%time = a%traj%time(min(a%traj%num_times(), 1 + (best_k - 1) * a%stride_))
        h%local = a0 + best_s * (b0 - a0)
        h%position = matmul(m(1:3, 1:3), h%local) + m(1:3, 4)
        h%distance = best_depth
        h%pixels = best_px
        call add_pick_hit(hits, n, h)
    end subroutine segment_artist_pick

    !> A marker artist's best hit: its start, end and intermediate markers drawn.
    subroutine marker_pick(a, frame, cam, m, hits, n)
        class(MarkerArtist), intent(in), target :: a
        class(ReferenceFrame), intent(in), target :: frame
        type(PickCamera), intent(in) :: cam
        real(wp), intent(in) :: m(4,4)
        type(PickHit), allocatable, intent(inout) :: hits(:)
        integer, intent(inout) :: n
        real(wp) :: extra, v(3), px, depth, best_px, best_depth, best_v(3)
        integer :: k, which, best_which, best_k, npts
        type(PickHit) :: h
        extra = 0.5_wp * a%size_ * cam%pixel_ratio
        best_px = huge(1.0_wp)
        best_depth = huge(1.0_wp)
        best_which = 0
        best_k = 0
        best_v = 0.0_wp
        do which = 1, 3                                         ! start, end, the intermediate ones
            select case (which)
            case (1)
                if (a%drawn_marker(MARKER_START, v)) call consider(MARKER_START, 0)
            case (2)
                if (a%drawn_marker(MARKER_END, v)) call consider(MARKER_END, 0)
            case (3)
                do k = 1, a%n_inter_drawn
                    if (a%drawn_marker(MARKER_INTERMEDIATE, v, k)) call consider(MARKER_INTERMEDIATE, k)
                end do
            end select
        end do
        if (best_which == 0) return
        h%kind = PICK_MARKER
        h%frame => frame
        h%artist => a
        h%marker = best_which
        npts = 0
        if (associated(a%traj)) then
            h%trajectory => a%traj
            npts = source_points(a%traj, a%sources_)
        end if
        select case (best_which)
        case (MARKER_START)
            h%index = min(1, npts)
            if (npts > 0) h%time = a%traj%time(1)
        case (MARKER_END)
            h%index = npts
            if (npts > 0) h%time = a%traj%time(npts)
        case default
            h%index = best_k
            h%time = a%inter_t(best_k)
        end select
        h%local = best_v
        h%position = matmul(m(1:3, 1:3), best_v) + m(1:3, 4)
        h%distance = best_depth
        h%pixels = best_px
        call add_pick_hit(hits, n, h)

    contains

        subroutine consider(kind, k)
            integer, intent(in) :: kind, k
            if (.not. point_pick(cam, matmul(m(1:3, 1:3), v) + m(1:3, 4), px, depth, extra)) return
            if (.not. better(px, depth, best_px, best_depth)) return
            best_px = px
            best_depth = depth
            best_which = kind
            best_k = k
            best_v = v
        end subroutine consider

    end subroutine marker_pick

    function drawable_frame_info(self) result(info)
        class(DrawableTrajectory), intent(in) :: self
        character(len=:), allocatable :: info
        info = 'DrawableTrajectory'
    end function drawable_frame_info

    subroutine drawable_build_contents(self, viewer, group)
        class(DrawableTrajectory), intent(inout) :: self
        type(c_ptr), intent(in) :: viewer, group
        integer :: k
        self%shown = c_associated(viewer)
        self%contents_group = group
        do k = 1, self%n_artists
            call build_artist(self, k, compile=.false.)
        end do
    end subroutine drawable_build_contents

    !> Update the frame (and its descendants), then its artists.
    recursive subroutine drawable_update(self, t)
        class(DrawableTrajectory), intent(inout) :: self
        real(wp), intent(in) :: t
        integer :: k
        call self%ReferenceFrame%update(t)
        do k = 1, self%n_artists
            if (.not. c_associated(self%artists(k)%group) .and. c_associated(self%contents_group)) &
                call build_artist(self, k, compile=.true.)
            if (c_associated(self%artists(k)%group)) call self%artists(k)%p%update(t)
        end do
    end subroutine drawable_update

    !> Create artist k's node (compiled now if the frame is already shown).
    subroutine build_artist(self, k, compile)
        type(DrawableTrajectory), intent(inout) :: self
        integer, intent(in) :: k
        logical, intent(in) :: compile
        type(c_ptr) :: group
        group = vkf_switch_create()
        if (.not. c_associated(group)) return
        ! (built for a window that exists now, if shown; compiled for the windows running)
        call self%artists(k)%p%build(merge(vkf_viewer_any(), c_null_ptr, self%shown), group)
        if (compile .and. self%shown) call check(vkf_compile_node(group))
        call check(vkf_node_add_child(self%contents_group, group))
        self%artists(k)%group = group
    end subroutine build_artist

end module vkf_artists
