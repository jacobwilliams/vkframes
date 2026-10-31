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
!> the current time: the points whose times are at most the current time,
!> whatever order they were added in (a change of the draw ranges, no
!> upload): for increasing times its first points, for decreasing ones (a
!> trajectory propagated backwards) its last. A curve adds a two-point tail
!> from the latest of them to the point interpolated at the current time
!> (the only data uploaded each frame). So a trajectory propagated forwards
!> and one propagated backwards to the same state trace one history as time
!> runs (example/trace_directions.f90); with time running backwards, the
!> history shrinks back. (A highlight's arrows still run in the order the
!> points were added: the propagation's direction.)
!>
!> Point colors (not in OpenFrames): a CurveArtist draws a trajectory with
!> colors (Trajectory%add_color, or add's `color`) in them, each segment
!> blended from its first point's color to its second's (one color: that
!> color); without colors, or with `set_point_colors(.false.)`, in its own
!> color. Its line buffers hold colors only then.
!>
!> Trails (not in OpenFrames): `CurveArtist%set_trail(past, future, fade)`
!> draws only the part of the curve within `past` before the current time
!> and `future` after it, each fading out over the last `fade` of its window
!> toward its cutoff; `set_trail_style(TRAIL_PAST or TRAIL_FUTURE, ...)` gives
!> each part its own color (or translucency) and dash pattern (e.g. a solid
!> past and a dashed future). The GPU fades and cuts the line by its distance
!> along it (shaders/line.vert): each frame only the distances at the current
!> time and at the window's edges are uploaded (a few floats per chunk), and
!> the range of points drawn changed. It replaces trace mode while set.
!>
!> Highlight (not in OpenFrames; e.g. a trajectory selected by the program or
!> by picking): `set_highlight` on a curve or segment artist, or on a
!> DrawableTrajectory for all of them. HIGHLIGHT_PULSE: the line widens and
!> brightens towards the highlight color, and back, over and over;
!> HIGHLIGHT_ARROWS: a translucent band of the highlight color around it, with
!> chevrons moving along it from its first point to its last (a trajectory's
!> initial time to its final; a segment's start to its end). Both together:
!> HIGHLIGHT_PULSE + HIGHLIGHT_ARROWS. The GPU animates them (shaders/
!> line.vert), so nothing is uploaded or computed each frame. In trace mode,
!> only the part drawn is highlighted.
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
    use vkf_math, only: wp, set_rgba
    use vkf_trajectory, only: Trajectory, DataSource, position_sources, SOURCE_ZERO, SOURCE_POSOPT, &
                              TIME_OUT_OF_RANGE, TIME_NOT_FOUND
    use vkf_frames, only: ReferenceFrame, PickHit, add_pick_hit, PICK_CURVE, PICK_SEGMENT, PICK_MARKER
    use vkf_pick, only: PickCamera, point_pick, segment_pick, sphere_near, wants
    use vkf_layer, only: check_status, vkf_line_create, vkf_line_create_colored, vkf_line_colors, vkf_line_points_changed, &
                         vkf_ribbon_create, &
                         vkf_line_set_range, &
                         vkf_node_add_child, vkf_node_remove_child, vkf_node_delete, vkf_switch_create, &
                         vkf_compile_node, vkf_viewer_any, line_set_style, vkf_image_read, vkf_image_delete
    use vkf_line_chain, only: LineChain, CHAIN_STRIP, CHAIN_SEGMENTS, CHAIN_MARKERS, CHAIN_RIBBON, split_points, &
                              split_distances, &
                              line_buffer

    implicit none

    private

    !> The base of the trajectory artists (OpenFrames' TrajectoryArtist).
    type, abstract, public :: TrajectoryArtist
        private
        type(Trajectory), pointer :: traj => null()  !! the trajectory drawn (not owned; null if none)
        logical :: trace = .false.                  !! trace mode: drawn up to the current time
        logical :: pickable_ = .true.               !! picked (WindowProxy%pick)
        ! what was drawn
        integer :: seen_generation = -1, seen_revision = -1  !! trajectory generation and revision drawn (-1: none)
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

    !> how a curve's or segments' dash pattern is laid out (set_dash_mode)
    integer, parameter, public :: DASH_SCENE = 0    !! anchored to the line (sizes in scene units; the default)
    integer, parameter, public :: DASH_SCREEN = 1   !! screen-locked (a constant length in pixels; OpenGL's stipple)
    integer, parameter, public :: DASH_FIXED = 2    !! a fixed length of the line, as paint on it (curves)

    !> the parts of a curve's trail (set_trail_style)
    integer, parameter, public :: TRAIL_PAST = 1, TRAIL_FUTURE = 2

    !> A curve's trail (see the module documentation and set_trail).
    type :: TrailStyle
        logical :: on = .false.
        real(wp) :: past = 0.0_wp, future = 0.0_wp  !! the window (s before and after the current time; huge: no limit)
        real(wp) :: fade = 0.25_wp                  !! of each part's window, faded toward its cutoff (0 to 1)
        real(wp) :: color(4, 2) = 1.0_wp            !! each part's color (if `replace`) [r, g, b, a]
        logical :: replace(2) = .false.             !! each part in its own color (else the line's)
        real(wp) :: opacity(2) = 1.0_wp             !! each part's alpha multiplied by it
        integer :: factor(2) = 1, pattern(2) = int(z'FFFF')  !! each part's dash pattern
    end type TrailStyle

    !> highlights of curve and segment artists (set_highlight; combine by adding)
    integer, parameter, public :: HIGHLIGHT_NONE = 0     !! not highlighted
    integer, parameter, public :: HIGHLIGHT_PULSE = 1    !! the line widens and brightens, and back, over and over
    integer, parameter, public :: HIGHLIGHT_ARROWS = 2   !! a band around the line, with arrows moving along it

    !> A line artist's highlight (see the module documentation and set_highlight).
    type :: HighlightStyle
        integer :: mode = HIGHLIGHT_NONE            !! HIGHLIGHT_* (added)
        real(wp) :: color(4) = [1.0_wp, 1.0_wp, 1.0_wp, 1.0_wp]  !! [r, g, b, a] (default white)
        real(wp) :: arrow_spacing = 40.0_wp         !! between arrows (window units)
        real(wp) :: arrow_speed = 50.0_wp           !! window units per second
        real(wp) :: band_width = 12.0_wp            !! (window units)
        real(wp) :: pulse_period = 1.5_wp           !! (s)
    end type HighlightStyle

    !> The base of the artists drawing a trajectory's points through one chain
    !> of buffers (CurveArtist: a line, a chain point per trajectory point;
    !> RibbonArtist: a surface, a pair of chain points per trajectory point):
    !> the chain and how much of it is written and drawn, the trace tail, the
    !> trajectory's colors, and the update (chain_update: the points written
    !> as they change, then drawn all or, in trace mode, those passed and the
    !> tail to the point at the current time). Each extension says what its
    !> chain is (chain_kind), how many points it draws (points), its tail's
    !> node (new_tail), its style (apply_style), how its points are written
    !> (write_points) and its tail drawn (draw_tail).
    type, abstract, extends(TrajectoryArtist) :: ChainArtist
        private
        logical :: point_colors_ = .true.           !! the trajectory's colors, if any (see set_point_colors)
        ! rendering
        type(c_ptr) :: group = c_null_ptr           !! the chain's group (to make it again, colored or not)
        logical :: compile = .false.                !! (chunks made later are compiled)
        type(LineChain) :: chain                    !! the buffers
        type(c_ptr) :: tail = c_null_ptr            !! to the point at the current time (trace mode)
        integer :: n_written = 0                    !! trajectory points written to the chain
        integer :: n_drawn = 0                      !! trajectory points drawn from it
        integer :: first_drawn_ = 1                 !! the first of them (trace mode, decreasing times: later)
        integer :: n_colored = 0                    !! points written with the trajectory's own colors
        logical :: tail_shown = .false.             !! the tail is drawn
        real(wp) :: tail_time = huge(1.0_wp)        !! the time the tail was drawn for
    contains
        procedure :: set_point_colors, point_colors
        procedure :: num_chunks, num_drawn, first_drawn
        procedure :: build => chain_build
        procedure :: update => chain_update
        procedure :: release => chain_release
        procedure :: init_chain => chain_init
        procedure(chain_kind_interface), deferred :: chain_kind
        procedure(chain_points_interface), deferred :: points
        procedure(new_tail_interface), deferred :: new_tail
        procedure(apply_style_interface), deferred :: apply_style
        procedure(write_points_interface), deferred :: write_points
        procedure(draw_tail_interface), deferred :: draw_tail
    end type ChainArtist

    abstract interface
        !> The kind of chain (LineChain's CHAIN_STRIP or CHAIN_RIBBON).
        pure integer function chain_kind_interface(self)
            import :: ChainArtist
            implicit none
            class(ChainArtist), intent(in) :: self
        end function chain_kind_interface

        !> The number of trajectory points to draw (0 if none).
        integer function chain_points_interface(self) result(n)
            import :: ChainArtist
            implicit none
            class(ChainArtist), intent(in) :: self
        end function chain_points_interface

        !> The trace tail's node (null on failure), made with the chain.
        function new_tail_interface(self) result(node)
            import :: ChainArtist, c_ptr
            implicit none
            class(ChainArtist), intent(in) :: self
            type(c_ptr) :: node
        end function new_tail_interface

        !> The style (the artist's color, width, ...) given to the chain and the tail.
        subroutine apply_style_interface(self)
            import :: ChainArtist
            implicit none
            class(ChainArtist), intent(inout) :: self
        end subroutine apply_style_interface

        !> Write trajectory points i1 .. i2 into the chain (and upload the chunks written to).
        subroutine write_points_interface(self, i1, i2)
            import :: ChainArtist
            implicit none
            class(ChainArtist), intent(inout) :: self
            integer, intent(in) :: i1, i2                   !! the trajectory points
        end subroutine write_points_interface

        !> The trace tail: from point k (the latest passed) to the point
        !> interpolated at time t toward point k2 (the next in time); none if
        !> either isn't a point written.
        subroutine draw_tail_interface(self, t, k, k2)
            import :: ChainArtist, wp
            implicit none
            class(ChainArtist), intent(inout) :: self
            real(wp), intent(in) :: t                       !! the current time
            integer, intent(in) :: k, k2                    !! the tail's ends (0: none)
        end subroutine draw_tail_interface
    end interface

    !> Draws a trajectory's points connected by a line (OpenFrames' CurveArtist).
    !> The x, y and z of the points come from data sources (default: the
    !> trajectory's positions). The line is in the artist's color, or in the
    !> trajectory's point colors if it has any (see the module documentation).
    type, extends(ChainArtist), public :: CurveArtist
        private
        type(DataSource) :: sources_(3)             !! x, y, z data sources (default: the positions)
        logical :: default_data = .true.            !! sources not set: the positions, per the DOF
        real(wp) :: color_(4) = [1.0_wp, 1.0_wp, 1.0_wp, 1.0_wp]  !! line color [r, g, b, a] (default white)
        real(wp) :: width_ = 1.0_wp                 !! line width (window units)
        integer :: pattern_factor = 1               !! window units per pattern bit (>= 1)
        integer :: pattern_ = int(z'FFFF')          !! 16-bit dash pattern (z'FFFF': solid)
        logical :: lod_ = .true.                    !! level of detail (see set_level_of_detail)
        type(HighlightStyle) :: highlight_          !! (see set_highlight)
        type(TrailStyle) :: trail_                  !! (see set_trail)
        ! rendering (the chain: a line strip; the tail: a line of two points)
        integer :: tail_from = 0, tail_to = 0       !! the tail: from point tail_from toward tail_to
    contains
        procedure :: set_trajectory => curve_set_trajectory
        procedure :: set_x_data, set_y_data, set_z_data, data_sources
        procedure :: set_color => curve_set_color, color => curve_color
        procedure :: set_width => curve_set_width, width => curve_width
        procedure :: set_pattern => curve_set_pattern, pattern => curve_pattern
        procedure :: set_highlight => curve_set_highlight, highlight => curve_highlight
        procedure :: set_trail, remove_trail, has_trail, set_trail_style
        procedure :: set_dash_mode => curve_set_dash_mode, dash_mode => curve_dash_mode
        procedure :: set_level_of_detail, level_of_detail, lod_error
        procedure :: drawn_color
        procedure :: update => curve_update
        procedure :: drawn_point, drawn_distance, trace_end
        procedure :: init_chain => curve_init_chain
        procedure :: chain_kind => curve_chain_kind
        procedure :: points => curve_points
        procedure :: new_tail => curve_new_tail
        procedure :: apply_style => curve_style
        procedure :: write_points => write_curve
        procedure :: draw_tail => update_tail
    end type CurveArtist

    !> The CurveArtist constructor: `CurveArtist(...)` makes one (see `new_curve_artist` for its
    !> arguments).
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
        type(DataSource) :: start_(3), end_(3)      !! start and end points' x, y, z data sources
        logical :: default_data = .true.            !! sources not set: the origin to the positions, per the DOF
        logical :: end_relative_ = .false.          !! the end sources: an offset from the start
        integer :: stride_ = 1                      !! a segment at every stride_-th point (>= 1)
        real(wp) :: color_(4) = [1.0_wp, 1.0_wp, 1.0_wp, 1.0_wp]  !! segment color [r, g, b, a] (default white)
        real(wp) :: width_ = 1.0_wp                 !! segment width (window units)
        integer :: pattern_factor = 1               !! window units per pattern bit (>= 1)
        integer :: pattern_ = int(z'FFFF')          !! 16-bit dash pattern (z'FFFF': solid)
        type(HighlightStyle) :: highlight_          !! (see set_highlight)
        ! rendering
        type(LineChain) :: chain                    !! the line buffers (a pair of points per segment)
        integer :: n_written = 0                    !! segments written to the chain
        integer :: n_drawn = 0                      !! segments drawn from it
        integer :: first_drawn_ = 1                 !! the first of them (trace mode, decreasing times: later)
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
        procedure :: set_highlight => segment_set_highlight, highlight => segment_highlight
        procedure :: set_dash_mode => segment_set_dash_mode, dash_mode => segment_dash_mode
        procedure :: build => segment_build
        procedure :: update => segment_update
        procedure :: release => segment_release
        procedure :: num_chunks => segment_num_chunks, num_drawn => segment_num_drawn
        procedure :: first_drawn => segment_first_drawn
        procedure :: drawn_segment
    end type SegmentArtist

    !> The SegmentArtist constructor: `SegmentArtist(...)` makes one (see `new_segment_artist` for its
    !> arguments).
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
    type, extends(ChainArtist), public :: RibbonArtist
        private
        type(DataSource) :: start_(3), end_(3)      !! start and end points' x, y, z data sources
        logical :: default_data = .true.            !! sources not set: the origin to the positions, per the DOF
        real(wp) :: color_(4) = [1.0_wp, 1.0_wp, 1.0_wp, 0.25_wp]  !! surface color (default white, alpha 0.25)
        ! rendering: the chain a surface, trajectory point i its points 2i-1 (start) and 2i (end); the tail a quad
    contains
        procedure :: set_trajectory => ribbon_set_trajectory
        procedure :: set_start_x_data => ribbon_set_start_x_data, set_start_y_data => ribbon_set_start_y_data
        procedure :: set_start_z_data => ribbon_set_start_z_data
        procedure :: set_end_x_data => ribbon_set_end_x_data, set_end_y_data => ribbon_set_end_y_data
        procedure :: set_end_z_data => ribbon_set_end_z_data
        procedure :: start_data_sources => ribbon_start_data_sources, end_data_sources => ribbon_end_data_sources
        procedure :: set_color => ribbon_set_color, color => ribbon_color
        procedure :: drawn_pair, drawn_color => ribbon_drawn_color, trace_end => ribbon_trace_end
        procedure :: chain_kind => ribbon_chain_kind
        procedure :: points => ribbon_points
        procedure :: new_tail => ribbon_new_tail
        procedure :: apply_style => ribbon_style
        procedure :: write_points => write_ribbon
        procedure :: draw_tail => update_ribbon_tail
    end type RibbonArtist

    !> The RibbonArtist constructor: `RibbonArtist(...)` makes one (see `new_ribbon_artist` for its
    !> arguments).
    interface RibbonArtist
        module procedure new_ribbon_artist
    end interface RibbonArtist

    !> which markers a MarkerArtist draws (OpenFrames' DrawnMarkers; combine with ior)
    integer, parameter, public :: MARKER_START = 1, MARKER_INTERMEDIATE = 2, MARKER_END = 4
    !> how intermediate markers are spaced (OpenFrames' IntermediateType)
    integer, parameter, public :: INTERMEDIATE_TIME = 1      !! every `spacing` time units
    integer, parameter, public :: INTERMEDIATE_DISTANCE = 2  !! every `spacing` of arc length
    integer, parameter, public :: INTERMEDIATE_DATA = 3      !! every `spacing`-th data point
    !> marker shapes (OpenFrames' disk, image, and its Demos/Shaders/Marker_*.frag)
    integer, parameter, public :: MARKER_DISK = 0, MARKER_CIRCLE = 1, MARKER_SQUARE = 2, MARKER_PLUS = 3, &
                                  MARKER_X = 4, MARKER_TARGET = 5, MARKER_ROSE = 6, MARKER_CIRCLE_PULSE = 7, &
                                  MARKER_DISK_PULSE = 8, MARKER_SPIRAL = 9, MARKER_IMAGE = 10

    !> Draws markers at a trajectory's points (OpenFrames' MarkerArtist): at
    !> its start and end, and intermediate markers spaced by time, arc length
    !> or data points, counted from the start or from the end. Markers keep
    !> their size on screen (window units), or with auto-attenuation shrink
    !> when the camera is farther than the trajectory's size. In trace mode
    !> (an addition), the markers passed are drawn: the end marker (at the
    !> trajectory's end) only once the time reaches the end. Point colors (an
    !> addition, off by default: set_point_colors): each marker in the
    !> trajectory's color there (see set_point_colors).
    type, extends(TrajectoryArtist), public :: MarkerArtist
        private
        type(DataSource) :: sources_(3)             !! x, y, z data sources (default: the positions)
        logical :: default_data = .true.            !! sources not set: the positions, per the DOF
        integer :: markers_ = ior(MARKER_START, MARKER_END)  !! markers drawn (an ior of MARKER_* flags)
        integer :: intermediate_type_ = INTERMEDIATE_DATA  !! INTERMEDIATE_TIME, _DISTANCE or _DATA
        real(wp) :: spacing_ = 1.0_wp               !! intermediate spacing: time units, arc length or data points
        integer :: direction_ = MARKER_START        !! intermediate markers counted from MARKER_START or MARKER_END
        real(wp) :: start_color(4) = [1.0_wp, 0.0_wp, 0.0_wp, 1.0_wp]  !! start marker color [r, g, b, a] (red)
        real(wp) :: end_color(4) = [1.0_wp, 0.0_wp, 0.0_wp, 1.0_wp]  !! end marker color (red)
        real(wp) :: intermediate_color(4) = [1.0_wp, 1.0_wp, 1.0_wp, 1.0_wp]  !! intermediate marker color (white)
        real(wp) :: size_ = 10.0_wp                 !! marker size (window units)
        integer :: shape_ = MARKER_DISK             !! marker shape: a MARKER_* shape constant
        character(len=:), allocatable :: image_file  !! the marker image file ('': none)
        type(c_ptr) :: image = c_null_ptr           !! the image read from it (owned; null: none)
        logical :: attenuate = .true.               !! auto-attenuation (see set_auto_attenuate; on by default)
        logical :: point_colors_ = .false.          !! the trajectory's colors, if any (see set_point_colors)
        ! rendering
        type(c_ptr) :: group = c_null_ptr           !! the artist's group (null until built)
        logical :: shown = .false.                  !! built for a window (marker nodes made later are compiled)
        integer :: n_colors_seen = 0                !! the trajectory's colors when the markers were written
        type(LineChain) :: start_node, end_node, inter  !! start, end and intermediate marker buffers
        logical :: drawn = .false.                  !! drawn at least once since built
        integer :: n_seen = 0                       !! points seen (for the bounds)
        real(wp) :: lo(3) = 0.0_wp, hi(3) = 0.0_wp  !! bounds of the points seen
        real(wp) :: radius = 0.0_wp                 !! attenuation radius applied
        ! intermediate markers: in trajectory order, and where to continue
        real(wp), allocatable :: inter_xyz(:,:), inter_t(:)  !! intermediate markers' points and times (1:n_inter)
        real(wp), allocatable :: inter_rgba(:,:)    !! (4, :) their colors (with point colors)
        integer :: n_inter = 0, n_inter_written = 0, n_inter_drawn = 0  !! intermediate markers computed, written, drawn
        integer :: inter_first = 1                  !! the first intermediate marker drawn (trace, decreasing times: later)
        real(wp) :: next_time = 0.0_wp              !! time of the next marker (INTERMEDIATE_TIME)
        integer :: next_index = 0                   !! where to continue (0: not started)
        real(wp) :: arc = 0.0_wp, prev_arc = 0.0_wp, target = 0.0_wp, prev_point(3) = 0.0_wp
        !! arc length (INTERMEDIATE_DISTANCE): so far, at prev_point, and of the next marker; the last point
        logical :: start_shown = .false., end_shown = .false.  !! the start and end markers are drawn
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
        procedure :: set_point_colors => marker_set_point_colors, point_colors => marker_point_colors
        procedure :: drawn_color => marker_drawn_color
        procedure :: set_intermediate_type, intermediate_type
        procedure :: set_intermediate_spacing, intermediate_spacing
        procedure :: set_intermediate_direction, intermediate_direction
        procedure :: build => marker_build
        procedure :: update => marker_update
        procedure :: release => marker_release
        procedure :: num_intermediate, drawn_marker
    end type MarkerArtist

    !> The MarkerArtist constructor: `MarkerArtist(...)` makes one (see `new_marker_artist` for its
    !> arguments).
    interface MarkerArtist
        module procedure new_marker_artist
    end interface MarkerArtist

    !> One of a DrawableTrajectory's artists: a pointer to it (Fortran has no
    !> arrays of pointers) and the node it is drawn under.
    type :: ArtistPointer
        class(TrajectoryArtist), pointer :: p => null()  !! the artist (not owned)
        type(c_ptr) :: group = c_null_ptr           !! the artist's node (null until built)
    end type ArtistPointer

    !> A frame for drawing trajectories (OpenFrames' DrawableTrajectory): it
    !> holds artists, which draw in its coordinates.
    type, extends(ReferenceFrame), public :: DrawableTrajectory
        private
        type(ArtistPointer), allocatable :: artists(:)  !! the artists (1:n_artists; not owned)
        integer :: n_artists = 0                    !! number of artists
        type(c_ptr) :: contents_group = c_null_ptr  !! the artists' group (null until built)
        logical :: shown = .false.                  !! built for a window (artists added later are compiled)
    contains
        procedure :: add_artist, remove_artist, remove_all_artists, num_artists, artist
        procedure :: set_highlight => drawable_set_highlight
        procedure :: frame_info => drawable_frame_info
        procedure :: build_contents => drawable_build_contents
        procedure :: update => drawable_update
        procedure :: pick => drawable_pick
    end type DrawableTrajectory

    !> The DrawableTrajectory constructor: `DrawableTrajectory(...)` makes one (see `new_drawable_trajectory` for its
    !> arguments).
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

    !> The trajectory drawn (null if none).
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

    !> Whether `pick` tests what this artist draws.
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

    !> Whether only the part up to the current time is drawn.
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

    !> The points of 1 .. n whose times are at most t (trace mode): i1 .. i2
    !> (none: i2 < i1), the first points for increasing times, the last for
    !> decreasing ones (by bisection).
    subroutine trace_range(traj, t, n, i1, i2)
        type(Trajectory), intent(in) :: traj
        real(wp), intent(in) :: t
        integer, intent(in) :: n
        integer, intent(out) :: i1, i2
        integer :: low, high, mid
        i1 = 1
        i2 = 0
        if (n < 1) return
        low = 0
        high = n + 1
        if (.not. decreasing_times(traj, n)) then       ! points 1 .. low have times <= t
            do while (high - low > 1)
                mid = (low + high) / 2
                if (traj%time(mid) <= t) then
                    low = mid
                else
                    high = mid
                end if
            end do
            i2 = low
        else                                            ! points high .. n have times <= t
            do while (high - low > 1)
                mid = (low + high) / 2
                if (traj%time(mid) <= t) then
                    high = mid
                else
                    low = mid
                end if
            end do
            i1 = high
            i2 = n
        end if
    end subroutine trace_range

    !> Whether points 1 .. n have decreasing times (a trajectory propagated backwards).
    logical function decreasing_times(traj, n)
        type(Trajectory), intent(in) :: traj
        integer, intent(in) :: n
        decreasing_times = .false.
        if (n > 1) decreasing_times = traj%time(n) < traj%time(1)
    end function decreasing_times

    !> Trace mode's tail, for the points passed i1 .. i2 of the n written: from
    !> the latest passed (k) toward the next in time (k2: i2 + 1, or i1 - 1 for
    !> decreasing times); none (0, 0) without a trajectory.
    subroutine tail_ends(traj, n, i1, i2, k, k2)
        type(Trajectory), pointer, intent(in) :: traj      !! the trajectory (null: none)
        integer, intent(in) :: n                            !! the points written
        integer, intent(in) :: i1, i2                       !! the points passed
        integer, intent(out) :: k, k2                       !! the tail's ends
        k = 0
        k2 = 0
        if (.not. associated(traj)) return
        if (decreasing_times(traj, n)) then
            k = i1
            k2 = i1 - 1
        else
            k = i2
            k2 = i2 + 1
        end if
    end subroutine tail_ends

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

    ! ==================================================================
    ! ChainArtist

    !> Whether a trajectory with colors is drawn in them (default true); points
    !> without a color yet (colors arriving after positions) in the artist's
    !> color. A curve: each segment blended from its first point's color to its
    !> second's (with level of detail, a color lasting fewer points than are
    !> skipped may not show from far away); a surface: each point's pair in its
    !> color, times the artist's alpha. False: all in the artist's color.
    subroutine set_point_colors(self, enabled)
        class(ChainArtist), intent(inout) :: self
        logical, intent(in) :: enabled                      !! true: the trajectory's colors
        self%point_colors_ = enabled
    end subroutine set_point_colors

    !> Whether a trajectory's colors are drawn.
    pure logical function point_colors(self)
        class(ChainArtist), intent(in) :: self
        point_colors = self%point_colors_
    end function point_colors

    !> Number of buffers the artist is drawn with (0 until built).
    pure integer function num_chunks(self)
        class(ChainArtist), intent(in) :: self
        num_chunks = self%chain%num_chunks()
    end function num_chunks

    !> Number of trajectory points drawn now (all of them, or in trace mode
    !> those passed).
    pure integer function num_drawn(self)
        class(ChainArtist), intent(in) :: self
        num_drawn = self%n_drawn
    end function num_drawn

    !> The first point drawn (1, or in trace mode of decreasing times the
    !> earliest point passed): points first_drawn .. first_drawn + num_drawn - 1.
    pure integer function first_drawn(self)
        class(ChainArtist), intent(in) :: self
        first_drawn = self%first_drawn_
    end function first_drawn

    !> Chain points per trajectory point: 1, or 2 for a surface (a pair each).
    pure integer function chain_points_per_point(self) result(m)
        class(ChainArtist), intent(in) :: self
        m = merge(2, 1, self%chain_kind() == CHAIN_RIBBON)
    end function chain_points_per_point

    !> Create the chain of buffers (sized for the points now) and the trace
    !> tail under `group`.
    subroutine chain_build(self, viewer, group)
        class(ChainArtist), intent(inout) :: self
        type(c_ptr), intent(in) :: viewer                   !! compiles what is made later (null: nothing to)
        type(c_ptr), intent(in) :: group                    !! the artist's group
        self%group = group
        self%compile = .false.
        call self%init_chain()
        call self%chain%reserve(chain_points_per_point(self) * self%points())   ! compiled with the group
        self%compile = c_associated(viewer)
        call self%chain%set_compile(self%compile)
        self%tail = self%new_tail()                         ! (white unless the points' colors are drawn)
        if (c_associated(self%tail)) call check_status(vkf_node_add_child(group, self%tail), 'trajectory artists')
        call self%apply_style()
        self%seen_generation = -1
    end subroutine chain_build

    !> The chain, colored if the trajectory's colors are drawn.
    subroutine chain_init(self)
        class(ChainArtist), intent(inout) :: self
        call self%chain%init(self%group, self%chain_kind(), colored=uses_colors(self))
        call self%chain%set_compile(self%compile)
        self%n_colored = 0
    end subroutine chain_init

    !> Whether the trajectory's colors are drawn: wanted, and it has some.
    logical function uses_colors(self)
        class(ChainArtist), intent(in) :: self
        uses_colors = self%point_colors_ .and. associated(self%traj)
        if (uses_colors) uses_colors = self%traj%num_colors() > 0
    end function uses_colors

    !> Forget the render nodes: the chain and the tail (see artist_release).
    subroutine chain_release(self)
        class(ChainArtist), intent(inout) :: self
        call self%chain%release()
        if (c_associated(self%tail)) call vkf_node_delete(self%tail)
        self%tail = c_null_ptr
        self%group = c_null_ptr
        self%n_written = 0
        self%n_drawn = 0
        self%n_colored = 0
        self%tail_shown = .false.
        call artist_release(self)
    end subroutine chain_release

    !> Write the points added (all of them on a redraw), and draw them all or,
    !> in trace mode, those passed and the tail to the point at time t.
    subroutine chain_update(self, t)
        class(ChainArtist), intent(inout) :: self
        real(wp), intent(in) :: t                           !! the current time
        logical :: changed
        if (.not. write_changed(self, changed)) return
        call draw_written(self, t, changed)
    end subroutine chain_update

    !> The points written as the trajectory changed: the chain made again if
    !> its colors are drawn now or no longer (its first colors, or the
    !> setting), then the points new since the last written (and the last one
    !> again: set_optional; and those whose colors came since), or all of them
    !> on a redraw (cleared, a new trajectory or data sources). False if the
    !> chain isn't built.
    logical function write_changed(self, changed) result(built)
        class(ChainArtist), intent(inout) :: self
        logical, intent(out) :: changed                     !! points written (or the chain emptied)
        integer :: n, i1
        logical :: redraw
        changed = .false.
        built = self%chain%is_built()
        if (.not. built) return
        n = self%points()
        if (uses_colors(self) .neqv. self%chain%is_colored()) then
            call self%chain%detach()
            call self%init_chain()
            call self%apply_style()
            self%seen_generation = -1
        end if
        changed = data_changed(self, redraw)
        if (redraw .or. .not. associated(self%traj)) then
            self%n_written = 0
            self%n_colored = 0
        end if
        if (changed) then
            i1 = max(1, min(self%n_written, n))          ! from the last point written (set_optional)
            if (self%chain%is_colored()) i1 = max(1, min(i1, self%n_colored))   ! (and colors added since)
            call self%chain%reserve(chain_points_per_point(self) * n)
            if (n >= i1) call self%write_points(i1, n)
            self%n_written = n
        end if
    end function write_changed

    !> Draw the points written: all of them (the tail hidden) or, in trace
    !> mode, those passed by time t (times up to it) and the tail from the
    !> latest of them to the point at t (unchanged if nothing `changed` and t
    !> is the time it was drawn for).
    subroutine draw_written(self, t, changed)
        class(ChainArtist), intent(inout) :: self
        real(wp), intent(in) :: t                           !! the current time
        logical, intent(in) :: changed                      !! points were written (write_changed)
        integer :: m, i1, i2, k, k2
        m = chain_points_per_point(self)
        if (.not. self%trace) then
            self%n_drawn = self%n_written
            self%first_drawn_ = 1
            call self%chain%draw(m * self%n_drawn)
            if (c_associated(self%tail)) call check_status(vkf_line_set_range(self%tail, 1_c_int, 0_c_int), 'trajectory artists')
            self%tail_shown = .false.
            self%tail_time = huge(1.0_wp)
            return
        end if
        if (.not. changed .and. t == self%tail_time) return
        i1 = 1
        i2 = 0
        if (associated(self%traj)) call trace_range(self%traj, t, self%n_written, i1, i2)
        self%first_drawn_ = i1
        self%n_drawn = max(0, i2 - i1 + 1)
        call self%chain%draw_range(m * (i1 - 1) + 1, m * i2)
        call tail_ends(self%traj, self%n_written, i1, i2, k, k2)
        call self%draw_tail(t, k, k2)
        self%tail_time = t
    end subroutine draw_written

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

    !> Where the y of the points come from (see set_x_data).
    logical function set_y_data(self, src) result(valid)
        class(CurveArtist), intent(inout) :: self
        type(DataSource), intent(in) :: src
        valid = set_curve_source(self, 2, src)
    end function set_y_data

    !> Where the z of the points come from (see set_x_data).
    logical function set_z_data(self, src) result(valid)
        class(CurveArtist), intent(inout) :: self
        type(DataSource), intent(in) :: src
        valid = set_curve_source(self, 3, src)
    end function set_z_data

    !> Set data source k (1: x, 2: y, 3: z); returns whether the trajectory has the data.
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

    !> The x, y and z data sources.
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

    !> The line's color [r, g, b, a].
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

    !> The line's width (window units).
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

    !> Highlight the curve (see the module documentation): `mode` a sum of
    !> HIGHLIGHT_PULSE and HIGHLIGHT_ARROWS (HIGHLIGHT_NONE: not highlighted),
    !> in `color` ([r, g, b] or [r, g, b, a]; default white). The other
    !> settings are kept when not given.
    subroutine curve_set_highlight(self, mode, color, arrow_spacing, arrow_speed, band_width, pulse_period)
        class(CurveArtist), intent(inout) :: self
        integer, intent(in) :: mode                         !! HIGHLIGHT_* (added)
        real(wp), intent(in), optional :: color(:)          !! [r, g, b] or [r, g, b, a] (0 to 1)
        real(wp), intent(in), optional :: arrow_spacing     !! between arrows (window units; default 40)
        real(wp), intent(in), optional :: arrow_speed       !! (window units per second; default 50)
        real(wp), intent(in), optional :: band_width        !! of the arrows' band (window units; default 12)
        real(wp), intent(in), optional :: pulse_period      !! (s; default 1.5)
        call update_highlight(self%highlight_, mode, color, arrow_spacing, arrow_speed, band_width, pulse_period)
        call curve_style(self)
    end subroutine curve_set_highlight

    !> The curve's highlight: HIGHLIGHT_* (added; HIGHLIGHT_NONE: not highlighted).
    pure integer function curve_highlight(self) result(mode)
        class(CurveArtist), intent(in) :: self
        mode = self%highlight_%mode
    end function curve_highlight

    !> A trail (see the module documentation): only the part within `past`
    !> before the current time and `future` after it (s, >= 0; 0: none of
    !> it; huge(1.0_wp): no limit), each fading out over the last `fade` (0
    !> to 1, default 0.25) of its window toward its cutoff. Replaces trace
    !> mode while set.
    subroutine set_trail(self, past, future, fade)
        class(CurveArtist), intent(inout) :: self
        real(wp), intent(in) :: past, future                !! s before and after the current time
        real(wp), intent(in), optional :: fade              !! of each part's window (default 0.25)
        self%trail_%on = .true.
        self%trail_%past = max(0.0_wp, past)
        self%trail_%future = max(0.0_wp, future)
        if (present(fade)) self%trail_%fade = max(0.0_wp, min(1.0_wp, fade))
        self%tail_time = huge(1.0_wp)                       ! (drawn again at the next update)
    end subroutine set_trail

    !> How the dash pattern (and a trail's) is laid out: DASH_SCENE (the
    !> default) anchored to the line: a dash stays put on it as the view
    !> changes, its size in scene units (one per line and view, from 0.7 to
    !> 1.4 times the pattern's on screen, changing by doubling as it zooms);
    !> DASH_SCREEN screen-locked, as OpenGL's line stipple (OpenFrames'):
    !> every dash the pattern's length in pixels, at any zoom and depth,
    !> sliding along the line as the view changes; DASH_FIXED a fixed
    !> length of the line, as paint on it: never changing, so zooming in the
    !> dashes grow on screen, and out they shrink (under a pixel, blending
    !> into a fainter solid line); `length` (scene units, for the whole
    !> pattern: the other patterns, a trail's future's and the arrows', in
    !> proportion), else the pattern's length on screen where the line is
    !> first drawn. DASH_SCREEN and DASH_FIXED cost projecting the points
    !> drawn on the CPU, per view and frame.
    subroutine curve_set_dash_mode(self, mode, length)
        class(CurveArtist), intent(inout) :: self
        integer, intent(in) :: mode                         !! DASH_SCENE, DASH_SCREEN or DASH_FIXED
        real(wp), intent(in), optional :: length            !! (DASH_FIXED) the pattern's length, scene units
        real(wp) :: l
        l = 0.0_wp
        if (present(length)) l = max(0.0_wp, length)
        call self%chain%set_dash_mode(mode, l)
        if (c_associated(self%tail)) call self%chain%style_node(self%tail)
    end subroutine curve_set_dash_mode

    !> The dash pattern's layout: DASH_SCENE, DASH_SCREEN or DASH_FIXED.
    pure integer function curve_dash_mode(self) result(mode)
        class(CurveArtist), intent(in) :: self
        mode = self%chain%dash_mode_of()
    end function curve_dash_mode

    !> No trail: the whole curve (or trace mode, if set) again.
    subroutine remove_trail(self)
        class(CurveArtist), intent(inout) :: self
        real(c_float) :: none(20)
        none = 0.0_c_float                                  ! (direction 0: no trail)
        self%trail_%on = .false.
        call self%chain%set_trail(none)
        self%tail_time = huge(1.0_wp)
    end subroutine remove_trail

    !> Whether the curve has a trail (set_trail).
    pure logical function has_trail(self)
        class(CurveArtist), intent(in) :: self
        has_trail = self%trail_%on
    end function has_trail

    !> The style of a trail's part, `which` TRAIL_PAST or TRAIL_FUTURE:
    !> `color` ([r, g, b] or [r, g, b, a]: drawn in it instead of the line's
    !> colors), `opacity` (its alpha multiplied by it: translucency, default
    !> 1), a dash pattern (`factor` and 16 `pattern` bits, as set_pattern;
    !> default solid). Settings not given are kept.
    subroutine set_trail_style(self, which, color, opacity, factor, pattern)
        class(CurveArtist), intent(inout) :: self
        integer, intent(in) :: which                        !! TRAIL_PAST or TRAIL_FUTURE
        real(wp), intent(in), optional :: color(:)          !! [r, g, b] or [r, g, b, a] (0 to 1)
        real(wp), intent(in), optional :: opacity           !! 0 to 1
        integer, intent(in), optional :: factor, pattern    !! window units per bit (>= 1), 16 bits
        integer :: k
        k = merge(1, 2, which == TRAIL_PAST)
        if (present(color)) then
            call set_rgba(self%trail_%color(:, k), color)
            self%trail_%replace(k) = .true.
        end if
        if (present(opacity)) self%trail_%opacity(k) = max(0.0_wp, min(1.0_wp, opacity))
        if (present(factor)) self%trail_%factor(k) = max(1, factor)
        if (present(pattern)) self%trail_%pattern(k) = iand(pattern, int(z'FFFF'))
        self%tail_time = huge(1.0_wp)
    end subroutine set_trail_style

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

    !> Whether level of detail is on.
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

    !> The dash pattern: its `factor` and 16 pattern `bits`.
    pure subroutine curve_pattern(self, factor, bits)
        class(CurveArtist), intent(in) :: self
        integer, intent(out) :: factor, bits
        factor = self%pattern_factor
        bits = self%pattern_
    end subroutine curve_pattern

    !> Point i (of the trajectory's: first_drawn to first_drawn + num_drawn - 1
    !> drawn) as written: read back from the line buffers.
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
        real(wp), intent(out) :: v(3)               !! the tail's end (frame coordinates)
        real(c_float), dimension(:,:,:), pointer :: p
        v = 0.0_wp
        trace_end = self%tail_shown
        if (.not. trace_end) return
        call line_buffer(self%tail, p)
        if (associated(p)) v = real(p(1:3, 1, 2), wp) + real(p(1:3, 2, 2), wp)
    end function trace_end

    !> Write the points added (all of them on a redraw), and draw them all, or
    !> in trace mode those passed and the tail to the point at time t, or with
    !> a trail the window about time t.
    subroutine curve_update(self, t)
        class(CurveArtist), intent(inout) :: self
        real(wp), intent(in) :: t                           !! the current time
        logical :: changed
        if (.not. write_changed(self, changed)) return
        if (self%trail_%on) then                           ! a trail: the window about time t
            if (.not. changed .and. t == self%tail_time) return
            call draw_trail(self, t)
            self%tail_time = t
            return
        end if
        call draw_written(self, t, changed)
    end subroutine curve_update

    !> The chain (ChainArtist's), with the curve's level of detail.
    subroutine curve_init_chain(self)
        class(CurveArtist), intent(inout) :: self
        call chain_init(self)
        call self%chain%set_lod(self%lod_)
    end subroutine curve_init_chain

    !> The chain: a line strip.
    pure integer function curve_chain_kind(self)
        class(CurveArtist), intent(in) :: self
        curve_chain_kind = CHAIN_STRIP
    end function curve_chain_kind

    !> The trace tail: a line of two points (white unless the points' colors are drawn).
    function curve_new_tail(self) result(node)
        class(CurveArtist), intent(in) :: self
        type(c_ptr) :: node
        node = vkf_line_create_colored(2_c_int)
    end function curve_new_tail

    !> Number of points to draw (1 if all sources are zero: a point at the origin).
    integer function curve_points(self) result(n)
        class(CurveArtist), intent(in) :: self
        n = source_points(self%traj, self%sources_)
        if (all(self%sources_%src == SOURCE_ZERO) .and. n > 0) n = min(1, self%traj%num_times())
    end function curve_points

    !> Write points i1 .. i2 into the chain (and upload the chunks written to).
    subroutine write_curve(self, i1, i2)
        class(CurveArtist), intent(inout) :: self
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

    !> A trail at time t: the points within its window (and the one beyond
    !> each end: the GPU cuts the segments at the cutoffs) drawn, and the
    !> distances along the curve at t and at the window's edges given to the
    !> shader (see vkf_line_set_trail), as distances from t's in the
    !> direction of time; times beyond the trajectory's are extrapolated from
    !> its end segments.
    subroutine draw_trail(self, t)
        type(CurveArtist), intent(inout) :: self
        real(wp), intent(in) :: t
        real(wp), parameter :: unlimited = 0.5_wp * huge(1.0_wp)
        real(c_float), parameter :: far = 1.0e30_c_float
        real(c_float) :: values(20), hi
        real(wp) :: sgn, lo_key, hi_key, s_now, fade
        integer :: n, i1, i2, k
        call check_status(vkf_line_set_range(self%tail, 1_c_int, 0_c_int), 'trajectory artists')
        self%tail_shown = .false.
        n = self%n_written
        self%n_drawn = 0
        self%first_drawn_ = 1
        values = 0.0_c_float
        if (.not. associated(self%traj) .or. n < 2) then
            call self%chain%draw_range(1, 0)
            call self%chain%set_trail(values)
            return
        end if
        sgn = merge(-1.0_wp, 1.0_wp, decreasing_times(self%traj, n))   ! (keys sgn * time increase with the index)
        lo_key = sgn * t - merge(huge(1.0_wp), self%trail_%past, self%trail_%past >= unlimited)
        hi_key = sgn * t + merge(huge(1.0_wp), self%trail_%future, self%trail_%future >= unlimited)
        if (sgn < 0.0_wp) then                              ! (the past is at the larger keys)
            lo_key = sgn * t - merge(huge(1.0_wp), self%trail_%future, self%trail_%future >= unlimited)
            hi_key = sgn * t + merge(huge(1.0_wp), self%trail_%past, self%trail_%past >= unlimited)
        end if
        if (hi_key < key(1) .or. lo_key > key(n)) then     ! (the window misses the trajectory)
            call self%chain%draw_range(1, 0)
            call self%chain%set_trail(values)
            return
        end if
        i1 = max(1, keys_up_to(lo_key, .true.))                   ! the last point before the window, or the first
        i2 = min(n, keys_up_to(hi_key, .false.) + 1)                 ! the first after it, or the last
        self%first_drawn_ = i1
        self%n_drawn = i2 - i1 + 1
        call self%chain%draw_range(i1, i2)
        s_now = s_at(t)
        hi = real(s_now, c_float)
        fade = 1.0_wp - self%trail_%fade
        values(1:4) = [hi, real(s_now - real(hi, wp), c_float), real(sgn, c_float), &
                       real(merge(1, 0, self%trail_%replace(1)) + merge(2, 0, self%trail_%replace(2)), c_float)]
        if (self%trail_%past >= unlimited) then
            values(5:6) = -far
        else
            values(5) = real(sgn * (s_at(t - self%trail_%past) - s_now), c_float)
            values(6) = real(sgn * (s_at(t - fade * self%trail_%past) - s_now), c_float)
        end if
        if (self%trail_%future >= unlimited) then
            values(7:8) = far
        else
            values(7) = real(sgn * (s_at(t + fade * self%trail_%future) - s_now), c_float)
            values(8) = real(sgn * (s_at(t + self%trail_%future) - s_now), c_float)
        end if
        do k = 1, 2                                         ! each part's color (or the line's: then its alpha only)
            values(9 + 4 * (k - 1):12 + 4 * (k - 1)) = real(self%trail_%color(:, k), c_float)
            values(12 + 4 * (k - 1)) = real(self%trail_%opacity(k) * merge(self%trail_%color(4, k), 1.0_wp, &
                                                                           self%trail_%replace(k)), c_float)
        end do
        values(17:20) = real([self%trail_%pattern(1), self%trail_%factor(1), self%trail_%pattern(2), &
                              self%trail_%factor(2)], c_float)
        call self%chain%set_trail(values)

    contains

        !> Point i's key: its time, increasing with the index.
        real(wp) function key(i)
            integer, intent(in) :: i
            key = sgn * self%traj%time(i)
        end function key

        !> The number of points with keys below x, or at most x if `inclusive`
        !> (bisection: the keys increase).
        integer function keys_up_to(x, inclusive) result(m)
            real(wp), intent(in) :: x                       !! the key
            logical, intent(in) :: inclusive                !! keys equal to x counted
            integer :: low, high, mid
            low = 0
            high = n + 1
            do while (high - low > 1)
                mid = (low + high) / 2
                if (key(mid) < x .or. (inclusive .and. key(mid) == x)) then
                    low = mid
                else
                    high = mid
                end if
            end do
            m = low
        end function keys_up_to

        !> The distance along the curve at time tt: interpolated between the
        !> points about it (extrapolated beyond the ends).
        real(wp) function s_at(tt)
            real(wp), intent(in) :: tt
            integer :: j
            real(wp) :: t0, t1
            j = max(1, min(n - 1, keys_up_to(sgn * tt, .true.)))
            t0 = self%traj%time(j)
            t1 = self%traj%time(j + 1)
            s_at = self%chain%distance(j)
            if (t1 /= t0) s_at = s_at + (tt - t0) / (t1 - t0) * (self%chain%distance(j + 1) - s_at)
        end function s_at

    end subroutine draw_trail

    !> The tail: from point k (the latest passed) to the point interpolated at
    !> time t toward point k2 (the next in time: k + 1, or k - 1 for decreasing
    !> times); none if either isn't a point written (before the start, or
    !> everything passed).
    subroutine update_tail(self, t, k, k2)
        class(CurveArtist), intent(inout) :: self
        real(wp), intent(in) :: t
        integer, intent(in) :: k, k2
        real(c_float), dimension(:,:,:), pointer :: p
        real(wp) :: frac, xyz(3, 2), along
        self%tail_shown = .false.
        if (.not. c_associated(self%tail)) return
        if (min(k, k2) < 1 .or. max(k, k2) > self%n_written) then
            call check_status(vkf_line_set_range(self%tail, 1_c_int, 0_c_int), 'trajectory artists')
            return
        end if
        frac = self%traj%time_fraction(k, k2, t)
        xyz(:, 1) = self%traj%point(k, self%sources_)
        xyz(:, 2) = self%traj%point_between(k, k2, t, self%sources_)
        call line_buffer(self%tail, p)
        if (.not. associated(p)) return
        call split_points(xyz, p(:, :, 1:2))
        along = merge(1.0_wp, -1.0_wp, k2 > k) * norm2(xyz(:, 2) - xyz(:, 1))   ! (the distance along: in point order)
        call split_distances(self%chain%distance(k) + [0.0_wp, along], p(4, :, 1:2))
        self%tail_from = k
        self%tail_to = k2
        call tail_colors(self, k, k2, frac)
        call check_status(vkf_line_points_changed(self%tail, 2_c_int), 'trajectory artists')
        self%tail_shown = .true.
    end subroutine update_tail

    !> The tail's colors: from point k's to the color at frac along to point
    !> k2's (white, showing the style's color, if the chain has none).
    subroutine tail_colors(self, k, k2, frac)
        type(CurveArtist), intent(inout) :: self
        integer, intent(in) :: k, k2
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
            c1 = point_rgba(self, k2)
            c(:, 1) = real(c0, c_float)
            c(:, 2) = real(c0 + frac * (c1 - c0), c_float)
        else
            c = 1.0_c_float
        end if
    end subroutine tail_colors

    !> The style: the artist's color, or white if the points' colors are drawn
    !> (the shader multiplies them by it).
    subroutine curve_style(self)
        class(CurveArtist), intent(inout) :: self
        real(wp) :: rgba(4)
        rgba = self%color_
        if (self%chain%is_colored()) rgba = 1.0_wp
        call self%chain%set_style(rgba, self%width_)
        call self%chain%set_pattern(self%pattern_factor, self%pattern_)
        call apply_highlight(self%highlight_, self%chain)
        if (c_associated(self%tail)) call self%chain%style_node(self%tail)
    end subroutine curve_style

    !> Change the settings given (see CurveArtist%set_highlight).
    subroutine update_highlight(h, mode, color, arrow_spacing, arrow_speed, band_width, pulse_period)
        type(HighlightStyle), intent(inout) :: h
        integer, intent(in) :: mode
        real(wp), intent(in), optional :: color(:), arrow_spacing, arrow_speed, band_width, pulse_period
        h%mode = iand(mode, HIGHLIGHT_PULSE + HIGHLIGHT_ARROWS)
        if (present(color)) call set_rgba(h%color, color)
        if (present(arrow_spacing)) h%arrow_spacing = max(arrow_spacing, 1.0_wp)
        if (present(arrow_speed)) h%arrow_speed = arrow_speed
        if (present(band_width)) h%band_width = max(band_width, 0.0_wp)
        if (present(pulse_period)) h%pulse_period = max(pulse_period, 0.05_wp)
    end subroutine update_highlight

    !> Give the highlight `h` to the line buffers `chain`.
    subroutine apply_highlight(h, chain)
        type(HighlightStyle), intent(in) :: h
        type(LineChain), intent(inout) :: chain
        call chain%set_highlight(h%mode, h%color, h%arrow_spacing, h%arrow_speed, h%band_width, h%pulse_period)
    end subroutine apply_highlight

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

    !> Where the y of the segments' start points come from (see set_start_x_data).
    logical function set_start_y_data(self, src) result(valid)
        class(SegmentArtist), intent(inout) :: self
        type(DataSource), intent(in) :: src
        valid = set_segment_source(self, 1, 2, src)
    end function set_start_y_data

    !> Where the z of the segments' start points come from (see set_start_x_data).
    logical function set_start_z_data(self, src) result(valid)
        class(SegmentArtist), intent(inout) :: self
        type(DataSource), intent(in) :: src
        valid = set_segment_source(self, 1, 3, src)
    end function set_start_z_data

    !> Where the x of the segments' end points come from (see set_start_x_data).
    logical function set_end_x_data(self, src) result(valid)
        class(SegmentArtist), intent(inout) :: self
        type(DataSource), intent(in) :: src
        valid = set_segment_source(self, 2, 1, src)
    end function set_end_x_data

    !> Where the y of the segments' end points come from (see set_start_x_data).
    logical function set_end_y_data(self, src) result(valid)
        class(SegmentArtist), intent(inout) :: self
        type(DataSource), intent(in) :: src
        valid = set_segment_source(self, 2, 2, src)
    end function set_end_y_data

    !> Where the z of the segments' end points come from (see set_start_x_data).
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

    !> The x, y and z data sources of the segments' start points.
    pure function start_data_sources(self) result(s)
        class(SegmentArtist), intent(in) :: self
        type(DataSource) :: s(3)
        s = self%start_
    end function start_data_sources

    !> The x, y and z data sources of the segments' end points.
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

    !> All three end sources at once (see set_start_data).
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

    !> Whether the ends are relative (see set_end_relative).
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

    !> The stride: a segment at every stride-th point.
    pure integer function stride(self)
        class(SegmentArtist), intent(in) :: self
        stride = self%stride_
    end function stride

    !> The segments' color: [r, g, b] (alpha unchanged) or [r, g, b, a], 0 to 1.
    subroutine segment_set_color(self, color)
        class(SegmentArtist), intent(inout) :: self
        real(wp), intent(in) :: color(:)
        call set_rgba(self%color_, color)
        call self%chain%set_style(self%color_, self%width_)
    end subroutine segment_set_color

    !> The segments' color [r, g, b, a].
    pure function segment_color(self) result(c)
        class(SegmentArtist), intent(in) :: self
        real(wp) :: c(4)
        c = self%color_
    end function segment_color

    !> The segments' width in window units (logical pixels).
    subroutine segment_set_width(self, width)
        class(SegmentArtist), intent(inout) :: self
        real(wp), intent(in) :: width
        self%width_ = width
        call self%chain%set_style(self%color_, self%width_)
    end subroutine segment_set_width

    !> The segments' width (window units).
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

    !> The dash pattern's layout (see CurveArtist%set_dash_mode): screen-locked
    !> dashes start again at each segment's start, as GL_LINES; DASH_FIXED
    !> (strips only) is taken as DASH_SCENE.
    subroutine segment_set_dash_mode(self, mode)
        class(SegmentArtist), intent(inout) :: self
        integer, intent(in) :: mode                         !! DASH_SCENE or DASH_SCREEN
        call self%chain%set_dash_mode(merge(DASH_SCENE, mode, mode == DASH_FIXED), 0.0_wp)
    end subroutine segment_set_dash_mode

    !> The dash pattern's layout: DASH_SCENE or DASH_SCREEN.
    pure integer function segment_dash_mode(self) result(mode)
        class(SegmentArtist), intent(in) :: self
        mode = self%chain%dash_mode_of()
    end function segment_dash_mode

    !> Highlight the segments (see CurveArtist%set_highlight): the arrows run
    !> along each segment from its start to its end.
    subroutine segment_set_highlight(self, mode, color, arrow_spacing, arrow_speed, band_width, pulse_period)
        class(SegmentArtist), intent(inout) :: self
        integer, intent(in) :: mode                         !! HIGHLIGHT_* (added)
        real(wp), intent(in), optional :: color(:)          !! [r, g, b] or [r, g, b, a] (0 to 1)
        real(wp), intent(in), optional :: arrow_spacing     !! between arrows (window units; default 40)
        real(wp), intent(in), optional :: arrow_speed       !! (window units per second; default 50)
        real(wp), intent(in), optional :: band_width        !! of the arrows' band (window units; default 12)
        real(wp), intent(in), optional :: pulse_period      !! (s; default 1.5)
        call update_highlight(self%highlight_, mode, color, arrow_spacing, arrow_speed, band_width, pulse_period)
        call apply_highlight(self%highlight_, self%chain)
    end subroutine segment_set_highlight

    !> The segments' highlight: HIGHLIGHT_* (added; HIGHLIGHT_NONE: not highlighted).
    pure integer function segment_highlight(self) result(mode)
        class(SegmentArtist), intent(in) :: self
        mode = self%highlight_%mode
    end function segment_highlight

    !> The dash pattern: its `factor` and 16 pattern `bits`.
    pure subroutine segment_pattern(self, factor, bits)
        class(SegmentArtist), intent(in) :: self
        integer, intent(out) :: factor, bits
        factor = self%pattern_factor
        bits = self%pattern_
    end subroutine segment_pattern

    !> Number of line buffers the segments are drawn with (0 until built).
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

    !> The first segment drawn (1, or in trace mode of decreasing times the
    !> earliest passed): segments first_drawn .. first_drawn + num_drawn - 1.
    pure integer function segment_first_drawn(self)
        class(SegmentArtist), intent(in) :: self
        segment_first_drawn = self%first_drawn_
    end function segment_first_drawn

    !> Segment s (at point 1 + (s-1) stride; first_drawn to first_drawn +
    !> num_drawn - 1 drawn) as written: read back from the line buffers.
    subroutine drawn_segment(self, s, a, b)
        class(SegmentArtist), intent(in) :: self
        integer, intent(in) :: s
        real(wp), intent(out) :: a(3)               !! the segment's start (frame coordinates)
        real(wp), intent(out) :: b(3)               !! its end
        a = self%chain%point(2 * s - 1)
        b = self%chain%point(2 * s)
    end subroutine drawn_segment

    !> Create the chain of line buffers (sized for the segments now) under `group`.
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

    !> Forget the render nodes (see artist_release).
    subroutine segment_release(self)
        class(SegmentArtist), intent(inout) :: self
        call self%chain%release()
        self%n_written = 0
        self%n_drawn = 0
        call artist_release(self)
    end subroutine segment_release

    !> Write the segments added (all of them on a redraw), and draw them all or,
    !> in trace mode, those at the points passed at time t.
    subroutine segment_update(self, t)
        class(SegmentArtist), intent(inout) :: self
        real(wp), intent(in) :: t
        integer :: m, s1, s2, i1, i2
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
        self%first_drawn_ = 1
        if (self%trace .and. associated(self%traj)) then  ! the segments at the points passed (times up to t)
            call trace_range(self%traj, t, self%traj%num_times(), i1, i2)
            s1 = (i1 - 1 + self%stride_ - 1) / self%stride_ + 1
            s2 = 0
            if (i2 >= 1) s2 = min(self%n_written, (i2 - 1) / self%stride_ + 1)
            self%first_drawn_ = s1
            self%n_drawn = max(0, s2 - s1 + 1)
        end if
        call self%chain%draw_range(2 * self%first_drawn_ - 1, 2 * (self%first_drawn_ + self%n_drawn - 1))
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

    !> Where the y of the surface's start points come from (see set_start_x_data).
    logical function ribbon_set_start_y_data(self, src) result(valid)
        class(RibbonArtist), intent(inout) :: self
        type(DataSource), intent(in) :: src
        valid = set_ribbon_source(self, 1, 2, src)
    end function ribbon_set_start_y_data

    !> Where the z of the surface's start points come from (see set_start_x_data).
    logical function ribbon_set_start_z_data(self, src) result(valid)
        class(RibbonArtist), intent(inout) :: self
        type(DataSource), intent(in) :: src
        valid = set_ribbon_source(self, 1, 3, src)
    end function ribbon_set_start_z_data

    !> Where the x of the surface's end points come from (see set_start_x_data).
    logical function ribbon_set_end_x_data(self, src) result(valid)
        class(RibbonArtist), intent(inout) :: self
        type(DataSource), intent(in) :: src
        valid = set_ribbon_source(self, 2, 1, src)
    end function ribbon_set_end_x_data

    !> Where the y of the surface's end points come from (see set_start_x_data).
    logical function ribbon_set_end_y_data(self, src) result(valid)
        class(RibbonArtist), intent(inout) :: self
        type(DataSource), intent(in) :: src
        valid = set_ribbon_source(self, 2, 2, src)
    end function ribbon_set_end_y_data

    !> Where the z of the surface's end points come from (see set_start_x_data).
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

    !> The x, y and z data sources of the surface's start points.
    pure function ribbon_start_data_sources(self) result(s)
        class(RibbonArtist), intent(in) :: self
        type(DataSource) :: s(3)
        s = self%start_
    end function ribbon_start_data_sources

    !> The x, y and z data sources of the surface's end points.
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

    !> The surface's color [r, g, b, a].
    pure function ribbon_color(self) result(c)
        class(RibbonArtist), intent(in) :: self
        real(wp) :: c(4)
        c = self%color_
    end function ribbon_color

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
        real(wp), intent(out) :: a(3)               !! the pair's start (frame coordinates)
        real(wp), intent(out) :: b(3)               !! its end
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

    !> Number of trajectory points to draw the surface through (none if all
    !> sources are zero).
    integer function ribbon_points(self) result(n)
        class(RibbonArtist), intent(in) :: self
        n = 0
        if (all(self%start_%src == SOURCE_ZERO) .and. all(self%end_%src == SOURCE_ZERO)) return
        n = min(source_points(self%traj, self%start_), source_points(self%traj, self%end_))
    end function ribbon_points

    !> The chain: a surface (a pair of points per trajectory point).
    pure integer function ribbon_chain_kind(self)
        class(RibbonArtist), intent(in) :: self
        ribbon_chain_kind = CHAIN_RIBBON
    end function ribbon_chain_kind

    !> The trace tail: a quad (white unless the points' colors are drawn).
    function ribbon_new_tail(self) result(node)
        class(RibbonArtist), intent(in) :: self
        type(c_ptr) :: node
        node = vkf_ribbon_create(4_c_int, 1_c_int)
    end function ribbon_new_tail

    !> Write the pairs of trajectory points i1 .. i2 into the chain, with their
    !> colors (and upload the chunks written to).
    subroutine write_ribbon(self, i1, i2)
        class(RibbonArtist), intent(inout) :: self
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
    !> time t toward point k2 (as the curve's tail: none if either isn't a
    !> point written).
    subroutine update_ribbon_tail(self, t, k, k2)
        class(RibbonArtist), intent(inout) :: self
        real(wp), intent(in) :: t
        integer, intent(in) :: k, k2
        real(c_float), dimension(:,:,:), pointer :: p
        real(c_float), dimension(:,:), pointer :: c
        real(wp) :: frac, xyz(3, 4), c0(4), c1(4)
        integer(c_int) :: capacity
        type(c_ptr) :: data
        self%tail_shown = .false.
        if (.not. c_associated(self%tail)) return
        if (min(k, k2) < 1 .or. max(k, k2) > self%n_written) then
            call check_status(vkf_line_set_range(self%tail, 1_c_int, 0_c_int), 'trajectory artists')
            return
        end if
        frac = self%traj%time_fraction(k, k2, t)
        xyz(:, 1) = self%traj%point(k, self%start_)
        xyz(:, 2) = self%traj%point(k, self%end_)
        xyz(:, 3) = self%traj%point_between(k, k2, t, self%start_)
        xyz(:, 4) = self%traj%point_between(k, k2, t, self%end_)
        call line_buffer(self%tail, p)
        if (.not. associated(p)) return
        call split_points(xyz, p(:, :, 1:4))
        data = vkf_line_colors(self%tail, capacity)
        if (c_associated(data) .and. capacity >= 4) then
            call c_f_pointer(data, c, [4, 4])
            if (self%chain%is_colored()) then
                c0 = ribbon_point_rgba(self, k)
                c1 = ribbon_point_rgba(self, k2)
                c(:, 1:2) = spread(real(c0, c_float), 2, 2)
                c(:, 3:4) = spread(real(c0 + frac * (c1 - c0), c_float), 2, 2)
            else
                c = 1.0_c_float
            end if
        end if
        call check_status(vkf_line_points_changed(self%tail, 4_c_int), 'trajectory artists')
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
        class(RibbonArtist), intent(inout) :: self
        real(wp) :: rgba(4)
        rgba = self%color_
        if (self%chain%is_colored()) rgba(1:3) = 1.0_wp
        call self%chain%set_style(rgba, 1.0_wp)
        if (c_associated(self%tail)) &
            call check_status(line_set_style(self%tail, real(rgba, c_float), 1.0_c_float), 'trajectory artists')
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

    !> Where the y of the markers come from (see set_x_data).
    logical function marker_set_y_data(self, src) result(valid)
        class(MarkerArtist), intent(inout) :: self
        type(DataSource), intent(in) :: src
        valid = set_marker_source(self, 2, src)
    end function marker_set_y_data

    !> Where the z of the markers come from (see set_x_data).
    logical function marker_set_z_data(self, src) result(valid)
        class(MarkerArtist), intent(inout) :: self
        type(DataSource), intent(in) :: src
        valid = set_marker_source(self, 3, src)
    end function marker_set_z_data

    !> Set data source k (1: x, 2: y, 3: z), redrawing the markers; returns whether
    !> the trajectory has the data.
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

    !> The x, y and z data sources.
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

    !> Which markers are drawn (an ior of MARKER_* flags).
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

    !> Marker size (window units).
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

    !> Marker shape (a MARKER_* shape constant).
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

    !> The marker image file ('' for none).
    function marker_image_file(self) result(file)
        class(MarkerArtist), intent(in) :: self
        character(len=:), allocatable :: file
        file = self%image_file
    end function marker_image_file

    !> Shrink markers farther from the camera than the trajectory's size
    !> (OpenFrames' auto-attenuation; see shaders/marker.vert). On by default
    !> (OpenFrames': off).
    subroutine set_auto_attenuate(self, attenuate)
        class(MarkerArtist), intent(inout) :: self
        logical, intent(in) :: attenuate
        self%attenuate = attenuate
        call marker_style(self)
    end subroutine set_auto_attenuate

    !> Whether the markers are auto-attenuated.
    pure logical function auto_attenuate(self)
        class(MarkerArtist), intent(in) :: self
        auto_attenuate = self%attenuate
    end function auto_attenuate

    !> How intermediate markers are spaced: INTERMEDIATE_TIME, _DISTANCE or _DATA.
    !> Point colors (default off): with the trajectory's colors (Trajectory%
    !> add_color, or add's `color`), each marker is drawn in the color at its
    !> point (intermediate markers between points: blended between theirs, by
    !> time), points without a color yet in the marker's own color
    !> (set_marker_color). Off, or without colors: the markers' own colors.
    subroutine marker_set_point_colors(self, enabled)
        class(MarkerArtist), intent(inout) :: self
        logical, intent(in) :: enabled
        if (enabled .eqv. self%point_colors_) return
        self%point_colors_ = enabled
        call rebuild_markers(self)
    end subroutine marker_set_point_colors

    !> Whether the trajectory's colors are drawn (set_point_colors).
    pure logical function marker_point_colors(self)
        class(MarkerArtist), intent(in) :: self
        marker_point_colors = self%point_colors_
    end function marker_point_colors

    !> The color marker k of `which` (one MARKER_* flag; k: the intermediate
    !> marker's number) is drawn in: read back from its buffer with point
    !> colors, else the marker's own color.
    function marker_drawn_color(self, which, k) result(rgba)
        class(MarkerArtist), intent(in) :: self
        integer, intent(in) :: which
        integer, intent(in), optional :: k
        real(wp) :: rgba(4)
        integer :: i
        i = 1
        if (present(k)) i = k
        select case (which)
        case (MARKER_START)
            rgba = self%start_color
            if (self%start_node%is_colored()) rgba = self%start_node%point_color(1)
        case (MARKER_END)
            rgba = self%end_color
            if (self%end_node%is_colored()) rgba = self%end_node%point_color(1)
        case default
            rgba = self%intermediate_color
            if (self%inter%is_colored()) rgba = self%inter%point_color(i)
        end select
    end function marker_drawn_color

    !> Whether the markers are drawn in the trajectory's colors: wanted, and it has some.
    logical function marker_uses_colors(self)
        type(MarkerArtist), intent(in) :: self
        marker_uses_colors = self%point_colors_ .and. associated(self%traj)
        if (marker_uses_colors) marker_uses_colors = self%traj%num_colors() > 0
    end function marker_uses_colors

    !> The trajectory's color at time t (between points i and i + 1: blended),
    !> `own` for points without one.
    function color_at(self, t, own) result(rgba)
        type(MarkerArtist), intent(in) :: self
        real(wp), intent(in) :: t, own(4)
        real(wp) :: rgba(4), c1(4), t0, t1
        integer :: status, i, n
        n = self%traj%num_times()
        status = self%traj%time_index(t, i)
        i = max(1, min(n, i))
        rgba = point_rgba_or(self%traj, i, own)
        if (i >= n) return
        t0 = self%traj%time(i)
        t1 = self%traj%time(i + 1)
        if (t1 == t0) return
        c1 = point_rgba_or(self%traj, i + 1, own)
        rgba = rgba + max(0.0_wp, min(1.0_wp, (t - t0) / (t1 - t0))) * (c1 - rgba)
    end function color_at

    !> Color of point i: the trajectory's, or `own` if it has none yet.
    pure function point_rgba_or(traj, i, own) result(rgba)
        type(Trajectory), intent(in) :: traj
        integer, intent(in) :: i
        real(wp), intent(in) :: own(4)
        real(wp) :: rgba(4)
        rgba = own
        if (i >= 1 .and. i <= traj%num_colors()) rgba = traj%color(i)
    end function point_rgba_or

    subroutine set_intermediate_type(self, type)
        class(MarkerArtist), intent(inout) :: self
        integer, intent(in) :: type
        if (type == self%intermediate_type_) return
        self%intermediate_type_ = type
        call redraw_markers(self)
    end subroutine set_intermediate_type

    !> How intermediate markers are spaced (INTERMEDIATE_*).
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

    !> Spacing of intermediate markers.
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

    !> Where intermediate markers are counted from (MARKER_START or MARKER_END).
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
        real(wp), intent(out) :: v(3)               !! where it is drawn (frame coordinates)
        integer, intent(in), optional :: i          !! the intermediate marker's number (1 to num_intermediate)
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
            shown = i >= self%inter_first .and. i <= self%inter_first + self%n_inter_drawn - 1
            if (shown) v = self%inter%point(i)
        end select
    end function drawn_marker

    !> Create the marker nodes under `group` (see artist_build).
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
        logical :: colored
        colored = marker_uses_colors(self)
        call self%start_node%init(self%group, CHAIN_MARKERS, self%image, colored=colored)
        call self%end_node%init(self%group, CHAIN_MARKERS, self%image, colored=colored)
        call self%inter%init(self%group, CHAIN_MARKERS, self%image, colored=colored)
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

    !> Forget the render nodes (see artist_release).
    subroutine marker_release(self)
        class(MarkerArtist), intent(inout) :: self
        call self%start_node%release()
        call self%end_node%release()
        call self%inter%release()
        self%group = c_null_ptr
        self%start_shown = .false.
        self%end_shown = .false.
        self%n_inter_drawn = 0
        self%inter_first = 1
        call redraw_markers(self)
        call artist_release(self)
    end subroutine marker_release

    !> Apply the colors, size, shape and attenuation radius (0: off) to the marker nodes.
    subroutine marker_style(self)
        type(MarkerArtist), intent(inout) :: self
        real(wp) :: r
        r = 0.0_wp
        if (self%attenuate) r = self%radius
        call self%start_node%set_marker_style(style_color(self%start_node, self%start_color), self%size_, self%shape_, r)
        call self%end_node%set_marker_style(style_color(self%end_node, self%end_color), self%size_, self%shape_, r)
        call self%inter%set_marker_style(style_color(self%inter, self%intermediate_color), self%size_, self%shape_, r)
    contains
        !> The style's color: the marker's, or white if each marker has its own (the shader multiplies them).
        function style_color(chain, own) result(c)
            type(LineChain), intent(in) :: chain
            real(wp), intent(in) :: own(4)
            real(wp) :: c(4)
            c = own
            if (chain%is_colored()) c = 1.0_wp
        end function style_color
    end subroutine marker_style

    !> Write the start, end and new intermediate markers if the trajectory changed,
    !> and draw them all or, in trace mode, those passed at time t.
    subroutine marker_update(self, t)
        class(MarkerArtist), intent(inout) :: self
        real(wp), intent(in) :: t
        logical :: changed, redraw, zero, passed_start, passed_end
        integer :: n
        if (.not. c_associated(self%group)) return
        zero = all(self%sources_%src == SOURCE_ZERO) .or. .not. associated(self%traj)   ! (a marker at the origin)
        if (zero) then
            n = 1                                         ! a marker at the origin
        else
            n = source_points(self%traj, self%sources_)
        end if

        ! the trajectory's colors drawn or not (its first colors, or the setting,
        ! changed): the marker nodes made again, with or without colors
        if (marker_uses_colors(self) .neqv. self%inter%is_colored()) call rebuild_markers(self)
        changed = data_changed(self, redraw)
        if (.not. self%drawn) then
            changed = .true.
            redraw = .true.
            self%drawn = .true.
        end if
        if (changed) then
            if (self%inter%is_colored()) then                 ! (colors added or changed: all written again)
                if (self%traj%num_colors() /= self%n_colors_seen) redraw = .true.
                self%n_colors_seen = self%traj%num_colors()
            end if
            if (redraw .or. n < self%n_seen) call reset_markers(self)
            call update_bounds(self, n, zero)
            if (n >= 1) call write_marker(self%start_node, marker_point(self, 1, zero), &   ! (none: nothing drawn)
                                          end_rgba(self, 1, self%start_color))
            if (n > 1) call write_marker(self%end_node, marker_point(self, n, zero), end_rgba(self, n, self%end_color))
            if (iand(self%markers_, MARKER_INTERMEDIATE) /= 0 .and. .not. zero .and. n > 2) call add_intermediates(self, n)
        end if

        ! what is drawn: everything, or in trace mode the markers whose times
        ! have passed (the start and end markers at their points' times)
        passed_start = .true.
        passed_end = .true.
        if (self%trace .and. .not. zero .and. n >= 1) then
            passed_start = self%traj%time(1) <= t
            passed_end = self%traj%time(n) <= t
        end if
        self%start_shown = iand(self%markers_, MARKER_START) /= 0 .and. n >= 1 .and. passed_start
        call self%start_node%draw(merge(1, 0, self%start_shown))
        self%end_shown = iand(self%markers_, MARKER_END) /= 0 .and. n > 1 .and. passed_end
        call self%end_node%draw(merge(1, 0, self%end_shown))
        self%inter_first = 1
        self%n_inter_drawn = self%n_inter_written
        if (self%trace .and. self%n_inter_written > 0) call markers_passed(self, t, self%inter_first, self%n_inter_drawn)
        call self%inter%draw_range(self%inter_first, self%inter_first + self%n_inter_drawn - 1)
    end subroutine marker_update

    !> The color of point i for a start or end marker: the trajectory's (its
    !> own if none), with point colors.
    function end_rgba(self, i, own) result(rgba)
        type(MarkerArtist), intent(in) :: self
        integer, intent(in) :: i
        real(wp), intent(in) :: own(4)
        real(wp) :: rgba(4)
        rgba = own
        if (self%inter%is_colored()) rgba = point_rgba_or(self%traj, i, own)
    end function end_rgba

    !> Point i from the data sources (the origin if `zero`).
    function marker_point(self, i, zero) result(v)
        type(MarkerArtist), intent(in) :: self
        integer, intent(in) :: i
        logical, intent(in) :: zero                 !! all sources zero, or no trajectory: the origin
        real(wp) :: v(3)
        v = 0.0_wp
        if (.not. zero) v = self%traj%point(i, self%sources_)
    end function marker_point

    !> A single marker's point (in a chain of one marker), and its color (a
    !> colored chain), uploaded.
    subroutine write_marker(chain, v, rgba)
        type(LineChain), intent(inout) :: chain
        real(wp), intent(in) :: v(3), rgba(4)
        real(c_float), dimension(:,:,:), pointer :: p
        real(c_float), dimension(:,:), pointer :: c
        integer :: lo, hi
        if (chain%num_chunks() < 1) return
        call chain%slice(1, 1, 1, lo, hi, p)
        if (.not. associated(p)) return
        call split_points(reshape(v, [3, 1]), p)
        call chain%color_slice(1, 1, 1, lo, hi, c)
        if (associated(c)) c(:, 1) = real(rgba, c_float)
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
        logical, intent(in) :: zero                 !! all sources zero, or no trajectory: a marker at the origin
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
            if (self%inter%is_colored()) self%inter_rgba(:, 1:self%n_inter) = self%inter_rgba(:, self%n_inter:1:-1)
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
            call push_intermediate(self, self%traj%point_between(index, index + 1, tm, self%sources_), tm)
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

    !> Append an intermediate marker at `v`, time `t` (the arrays grown as needed).
    subroutine push_intermediate(self, v, t)
        type(MarkerArtist), intent(inout) :: self
        real(wp), intent(in) :: v(3), t
        real(wp), allocatable :: xyz(:,:), times(:), rgba(:,:)
        integer :: m
        if (.not. allocated(self%inter_xyz)) allocate(self%inter_xyz(3, 64), self%inter_t(64), self%inter_rgba(4, 64))
        m = size(self%inter_t)
        if (self%n_inter == m) then
            allocate(xyz(3, 2 * m), times(2 * m), rgba(4, 2 * m))
            xyz(:, 1:m) = self%inter_xyz
            times(1:m) = self%inter_t
            rgba(:, 1:m) = self%inter_rgba
            call move_alloc(xyz, self%inter_xyz)
            call move_alloc(times, self%inter_t)
            call move_alloc(rgba, self%inter_rgba)
        end if
        self%n_inter = self%n_inter + 1
        self%inter_xyz(:, self%n_inter) = v
        self%inter_t(self%n_inter) = t
        if (self%inter%is_colored()) self%inter_rgba(:, self%n_inter) = color_at(self, t, self%intermediate_color)
    end subroutine push_intermediate

    !> Write intermediate markers i1 .. i2 into their chain, with their colors
    !> (with point colors), and upload the chunks written to.
    subroutine write_intermediates(self, i1, i2)
        type(MarkerArtist), intent(inout) :: self
        integer, intent(in) :: i1, i2
        real(c_float), dimension(:,:,:), pointer :: p
        real(c_float), dimension(:,:), pointer :: c
        integer :: j, lo, hi
        call self%inter%reserve(i2)
        do j = 1, self%inter%num_chunks()
            call self%inter%slice(j, i1, i2, lo, hi, p)
            if (.not. associated(p)) cycle
            call split_points(self%inter_xyz(:, lo:hi), p)
            call self%inter%color_slice(j, i1, i2, lo, hi, c)
            if (associated(c)) c = real(self%inter_rgba(:, lo:hi), c_float)
        end do
        call self%inter%written(i1, i2)
    end subroutine write_intermediates

    !> The intermediate markers (in trajectory order) whose times are at most
    !> t: `count` from `first` (the first ones for increasing times, the last
    !> for decreasing), by bisection.
    subroutine markers_passed(self, t, first, count)
        type(MarkerArtist), intent(in) :: self
        real(wp), intent(in) :: t
        integer, intent(out) :: first, count
        integer :: low, high, mid, m
        m = self%n_inter_written
        low = 0
        high = m + 1
        if (.not. decreasing_times(self%traj, self%traj%num_times())) then   ! markers 1 .. low passed
            do while (high - low > 1)
                mid = (low + high) / 2
                if (self%inter_t(mid) <= t) then
                    low = mid
                else
                    high = mid
                end if
            end do
            first = 1
            count = low
        else                                              ! markers high .. m passed
            do while (high - low > 1)
                mid = (low + high) / 2
                if (self%inter_t(mid) <= t) then
                    high = mid
                else
                    low = mid
                end if
            end do
            first = high
            count = m - high + 1
        end if
    end subroutine markers_passed

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
                    call check_status(vkf_node_remove_child(self%contents_group, self%artists(k)%group), 'trajectory artists')
                    call vkf_node_delete(self%artists(k)%group)
                    call self%artists(k)%p%release()
                end if
                self%artists(k:self%n_artists-1) = self%artists(k+1:self%n_artists)
                self%n_artists = self%n_artists - 1
                return
            end if
        end do
    end subroutine remove_artist

    !> Remove all the artists.
    subroutine remove_all_artists(self)
        class(DrawableTrajectory), intent(inout) :: self
        do while (self%n_artists > 0)
            call self%remove_artist(self%artists(self%n_artists)%p)
        end do
    end subroutine remove_all_artists

    !> Number of artists.
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

    !> Highlight all the frame's curve and segment artists (see
    !> CurveArtist%set_highlight; HIGHLIGHT_NONE: none of them).
    subroutine drawable_set_highlight(self, mode, color, arrow_spacing, arrow_speed, band_width, pulse_period)
        class(DrawableTrajectory), intent(inout) :: self
        integer, intent(in) :: mode                         !! HIGHLIGHT_* (added)
        real(wp), intent(in), optional :: color(:)          !! [r, g, b] or [r, g, b, a] (0 to 1)
        real(wp), intent(in), optional :: arrow_spacing     !! between arrows (window units; default 40)
        real(wp), intent(in), optional :: arrow_speed       !! (window units per second; default 50)
        real(wp), intent(in), optional :: band_width        !! of the arrows' band (window units; default 12)
        real(wp), intent(in), optional :: pulse_period      !! (s; default 1.5)
        integer :: k
        do k = 1, self%n_artists
            select type (a => self%artists(k)%p)
            class is (CurveArtist)
                call a%set_highlight(mode, color, arrow_spacing, arrow_speed, band_width, pulse_period)
            class is (SegmentArtist)
                call a%set_highlight(mode, color, arrow_spacing, arrow_speed, band_width, pulse_period)
            end select
        end do
    end subroutine drawable_set_highlight

    !> Picks what the artists draw (see the module documentation), then
    !> the frame's origin (if it shows axes or its name label).
    subroutine drawable_pick(self, cam, to_root, hits, n)
        class(DrawableTrajectory), intent(in), target :: self
        type(PickCamera), intent(in) :: cam
        real(wp), intent(in) :: to_root(4,4)        !! the frame's transform to the root frame
        type(PickHit), allocatable, intent(inout) :: hits(:)  !! hits are added to hits(1:n)
        integer, intent(inout) :: n                 !! number of hits
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
        real(wp), intent(in) :: pixels              !! the candidate's distance from the cursor (pixels)
        real(wp), intent(in) :: depth               !! its depth
        real(wp), intent(in) :: best_pixels         !! the best hit's so far (pixels)
        real(wp), intent(in) :: best_depth          !! the best hit's depth
        better = pixels < best_pixels - 0.5_wp .or. (pixels <= best_pixels + 0.5_wp .and. depth < best_depth)
    end function better

    !> A curve's best hit: on the segments drawn (and the trace tail).
    subroutine curve_pick(a, frame, cam, m, hits, n)
        class(CurveArtist), intent(in), target :: a
        class(ReferenceFrame), intent(in), target :: frame  !! the frame drawing it (for the hit)
        type(PickCamera), intent(in) :: cam
        real(wp), intent(in) :: m(4,4)              !! the frame's transform to the root frame
        type(PickHit), allocatable, intent(inout) :: hits(:)  !! hits are added to hits(1:n)
        integer, intent(inout) :: n                 !! number of hits
        real(wp), dimension(:,:), pointer :: pos
        real(wp) :: extra, lo(3), hi(3), c(3), p0(3), p1(3), px, s, depth, best_px, best_depth, best_s, t0, t1, frac
        integer :: j, first, count, i1, i2, i, best_i, best_j, nd, d1, d2
        logical :: xyz
        type(PickHit) :: h
        if (.not. associated(a%traj) .or. .not. a%chain%is_built()) return
        nd = a%n_drawn
        if (nd < 1) return
        d1 = a%first_drawn_                                     ! (the points drawn: d1 .. d2)
        d2 = d1 + nd - 1
        extra = 0.5_wp * a%width_ * cam%pixel_ratio
        xyz = is_position_xyz(a%sources_)
        if (xyz) call a%traj%positions(pos)
        best_px = huge(1.0_wp)
        best_depth = huge(1.0_wp)
        best_i = 0
        best_j = 0                                              ! (the hit is between points best_i and best_j)
        best_s = 0.0_wp
        if (nd == 1) then                                       ! (a single point)
            if (point_pick(cam, root_point(d1), px, depth, extra)) then
                best_px = px
                best_depth = depth
                best_i = d1
            end if
        end if
        do j = 1, a%chain%num_chunks()                          ! the segments drawn, chunk by chunk
            call a%chain%chunk_bounds(j, first, count, lo, hi)
            i1 = max(first, d1)
            i2 = min(first + count - 1, d2)
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
                        best_j = i + 1
                        best_s = s
                    end if
                end if
                p0 = p1
            end do
        end do
        ! trace mode: the tail from the latest point passed to the point at the time drawn
        if (a%trace .and. a%tail_shown) then
            t0 = a%traj%time(a%tail_from)
            t1 = a%traj%time(a%tail_to)
            frac = 0.0_wp
            if (t1 /= t0) frac = max(0.0_wp, min(1.0_wp, (a%tail_time - t0) / (t1 - t0)))
            p0 = root_point(a%tail_from)
            p1 = p0 + frac * (root_point(a%tail_to) - p0)
            if (segment_pick(cam, p0, p1, px, s, depth, extra)) then
                if (better(px, depth, best_px, best_depth)) then
                    best_px = px
                    best_depth = depth
                    best_i = a%tail_from
                    best_j = a%tail_to
                    best_s = s * frac
                end if
            end if
        end if
        if (best_i == 0) return
        h%kind = PICK_CURVE
        h%frame => frame
        h%artist => a
        h%trajectory => a%traj
        h%index = best_i
        if (best_j > 0) h%index = min(best_i, best_j)           ! (the point the hit is after, in point order)
        h%local = local_point(best_i)
        h%time = a%traj%time(best_i)
        if (best_j > 0) then
            h%local = h%local + best_s * (local_point(best_j) - h%local)
            h%time = h%time + best_s * (a%traj%time(best_j) - h%time)
        end if
        h%position = matmul(m(1:3, 1:3), h%local) + m(1:3, 4)
        h%distance = best_depth
        h%pixels = best_px
        call add_pick_hit(hits, n, h)

    contains

        !> Point i in the frame's coordinates (straight from the positions if they are the sources).
        function local_point(i) result(v)
            integer, intent(in) :: i
            real(wp) :: v(3)
            if (xyz) then
                v = pos(:, i)
            else
                v = a%traj%point(i, a%sources_)
            end if
        end function local_point

        !> Point i in the root frame.
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
        class(ReferenceFrame), intent(in), target :: frame  !! the frame drawing it (for the hit)
        type(PickCamera), intent(in) :: cam
        real(wp), intent(in) :: m(4,4)              !! the frame's transform to the root frame
        type(PickHit), allocatable, intent(inout) :: hits(:)  !! hits are added to hits(1:n)
        integer, intent(inout) :: n                 !! number of hits
        real(wp) :: extra, a0(3), b0(3), px, s, depth, best_px, best_depth, best_s
        integer :: k, best_k
        type(PickHit) :: h
        if (.not. associated(a%traj) .or. a%n_drawn < 1) return
        extra = 0.5_wp * a%width_ * cam%pixel_ratio
        best_px = huge(1.0_wp)
        best_depth = huge(1.0_wp)
        best_k = 0
        best_s = 0.0_wp
        do k = a%first_drawn_, a%first_drawn_ + a%n_drawn - 1
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
        class(ReferenceFrame), intent(in), target :: frame  !! the frame drawing it (for the hit)
        type(PickCamera), intent(in) :: cam
        real(wp), intent(in) :: m(4,4)              !! the frame's transform to the root frame
        type(PickHit), allocatable, intent(inout) :: hits(:)  !! hits are added to hits(1:n)
        integer, intent(inout) :: n                 !! number of hits
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
                do k = a%inter_first, a%inter_first + a%n_inter_drawn - 1
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

        !> Take the marker at `v` (`kind`, intermediate number k) as the best hit if picked and better.
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

    !> What kind of frame this is: 'DrawableTrajectory'.
    function drawable_frame_info(self) result(info)
        class(DrawableTrajectory), intent(in) :: self
        character(len=:), allocatable :: info
        info = 'DrawableTrajectory'
    end function drawable_frame_info

    !> Build the artists' nodes under `group` (`viewer`, may be null: those made
    !> later are compiled if not null).
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
        if (compile .and. self%shown) call check_status(vkf_compile_node(group), 'trajectory artists')
        call check_status(vkf_node_add_child(self%contents_group, group), 'trajectory artists')
        self%artists(k)%group = group
    end subroutine build_artist

end module vkf_artists
