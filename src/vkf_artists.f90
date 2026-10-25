!> Trajectory artists: what draws a Trajectory, in a DrawableTrajectory frame, as
!> OpenFrames' TrajectoryArtist, CurveArtist, SegmentArtist and
!> DrawableTrajectory.
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
!> Artists and their trajectories are not owned: they must stay alive while
!> drawn.
module vkf_artists

    use, intrinsic :: iso_c_binding, only: c_ptr, c_null_ptr, c_associated, c_int, c_float
    use vkf_math, only: wp
    use vkf_trajectory, only: Trajectory, DataSource, position_sources, SOURCE_ZERO, SOURCE_POSOPT, &
                              TIME_OUT_OF_RANGE, TIME_NOT_FOUND
    use vkf_frames, only: ReferenceFrame
    use vkf_layer, only: vkf_line_create, vkf_line_points_changed, vkf_line_set_range, &
                         vkf_node_add_child, vkf_node_remove_child, vkf_node_delete, vkf_switch_create, &
                         vkf_viewer_compile_node, line_set_style
    use vkf_line_chain, only: LineChain, split_points, line_buffer, check

    implicit none

    private

    !> The base of the trajectory artists (OpenFrames' TrajectoryArtist).
    type, abstract, public :: TrajectoryArtist
        private
        type(Trajectory), pointer :: traj => null()
        logical :: trace = .false.
        ! what was drawn
        integer :: seen_generation = -1, seen_revision = -1
    contains
        procedure :: set_trajectory, get_trajectory
        procedure :: set_trace_mode, trace_mode
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
    !> trajectory's positions).
    type, extends(TrajectoryArtist), public :: CurveArtist
        private
        type(DataSource) :: sources_(3)
        logical :: default_data = .true.
        real(wp) :: color_(4) = [1.0_wp, 1.0_wp, 1.0_wp, 1.0_wp]
        real(wp) :: width_ = 1.0_wp
        integer :: pattern_factor = 1
        integer :: pattern_ = int(z'FFFF')
        ! rendering
        type(LineChain) :: chain
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
        procedure :: build => curve_build
        procedure :: update => curve_update
        procedure :: release => curve_release
        procedure :: num_chunks => curve_num_chunks, num_drawn => curve_num_drawn
        procedure :: drawn_point, trace_end
    end type CurveArtist

    interface CurveArtist
        module procedure new_curve_artist
    end interface CurveArtist

    !> Draws a line segment at every `stride`-th point of a trajectory
    !> (OpenFrames' SegmentArtist), from the point given by the start data
    !> sources to the one given by the end data sources: e.g. radius vectors
    !> (the origin to the positions), or from the positions to points stored as
    !> an optional (the tip of a velocity vector, a target). Default (an
    !> addition to OpenFrames, which draws nothing until the sources are set):
    !> from the frame's origin to the trajectory's positions.
    type, extends(TrajectoryArtist), public :: SegmentArtist
        private
        type(DataSource) :: start_(3), end_(3)
        logical :: default_data = .true.
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
        procedure :: start_data_sources, end_data_sources
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
        type(c_ptr) :: viewer = c_null_ptr, contents_group = c_null_ptr
    contains
        procedure :: add_artist, remove_artist, remove_all_artists, num_artists, artist
        procedure :: frame_info => drawable_frame_info
        procedure :: build_contents => drawable_build_contents
        procedure :: update => drawable_update
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

    !> The line's color: [r, g, b] (alpha unchanged) or [r, g, b, a], 0 to 1.
    subroutine curve_set_color(self, color)
        class(CurveArtist), intent(inout) :: self
        real(wp), intent(in) :: color(:)
        call set_rgba(self%color_, color)
        call curve_style(self)
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
    !> bit of the 16-bit `pattern` covers `factor` pixels. Kept for later; lines
    !> are drawn solid for now.
    subroutine curve_set_pattern(self, factor, pattern)
        class(CurveArtist), intent(inout) :: self
        integer, intent(in) :: factor, pattern
        self%pattern_factor = factor
        self%pattern_ = iand(pattern, int(z'FFFF'))
    end subroutine curve_set_pattern

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
        if (associated(p)) v = real(p(:, 1, 2), wp) + real(p(:, 2, 2), wp)
    end function trace_end

    subroutine curve_build(self, viewer, group)
        class(CurveArtist), intent(inout) :: self
        type(c_ptr), intent(in) :: viewer, group
        call self%chain%init(group, segments=.false.)
        call self%chain%set_style(self%color_, self%width_)
        call self%chain%reserve(curve_points(self))       ! compiled with the group
        call self%chain%set_viewer(viewer)
        self%tail = vkf_line_create(2_c_int)
        if (c_associated(self%tail)) call check(vkf_node_add_child(group, self%tail))
        call curve_style(self)
        self%seen_generation = -1
    end subroutine curve_build

    subroutine curve_release(self)
        class(CurveArtist), intent(inout) :: self
        call self%chain%release()
        if (c_associated(self%tail)) call vkf_node_delete(self%tail)
        self%tail = c_null_ptr
        self%n_written = 0
        self%n_drawn = 0
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

        ! new points, or a redraw (cleared, new trajectory or data sources)
        changed = data_changed(self, redraw)
        if (redraw .or. .not. associated(self%traj)) self%n_written = 0
        if (changed) then
            i1 = max(1, min(self%n_written, n))          ! from the last point written (set_optional)
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
        real(wp) :: v(3, 1)
        integer :: j, lo, hi, i
        do j = 1, self%chain%num_chunks()
            call self%chain%slice(j, i1, i2, lo, hi, p)
            if (.not. associated(p)) cycle
            if (is_position_xyz(self%sources_)) then        ! the usual case: straight from the positions
                call self%traj%positions(pos)
                call split_points(pos(:, lo:hi), p)
            else
                do i = lo, hi
                    v(:, 1) = self%traj%point(i, self%sources_)
                    call split_points(v, p(:, :, i-lo+1:i-lo+1))
                end do
            end if
            call self%chain%changed(j, hi)
        end do
    end subroutine write_curve

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
        call check(vkf_line_points_changed(self%tail, 2_c_int))
        self%tail_shown = .true.
    end subroutine update_tail

    subroutine curve_style(self)
        type(CurveArtist), intent(inout) :: self
        call self%chain%set_style(self%color_, self%width_)
        if (c_associated(self%tail)) &
            call check(line_set_style(self%tail, real(self%color_, c_float), real(self%width_, c_float)))
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

    !> A dash pattern (see CurveArtist%set_pattern; drawn solid for now).
    subroutine segment_set_pattern(self, factor, pattern)
        class(SegmentArtist), intent(inout) :: self
        integer, intent(in) :: factor, pattern
        self%pattern_factor = factor
        self%pattern_ = iand(pattern, int(z'FFFF'))
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
        call self%chain%init(group, segments=.true.)
        call self%chain%set_style(self%color_, self%width_)
        call self%chain%reserve(2 * segment_count(self))  ! compiled with the group
        call self%chain%set_viewer(viewer)
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
        real(wp) :: v(3, 2)
        integer :: j, lo, hi, s, i
        do j = 1, self%chain%num_chunks()
            call self%chain%slice(j, 2 * s1 - 1, 2 * s2, lo, hi, p)   ! chunks hold whole segments
            if (.not. associated(p)) cycle
            do s = (lo + 1) / 2, hi / 2
                i = 1 + (s - 1) * self%stride_
                v(:, 1) = self%traj%point(i, self%start_)
                v(:, 2) = self%traj%point(i, self%end_)
                call split_points(v, p(:, :, 2*s-lo:2*s-lo+1))
            end do
            call self%chain%changed(j, hi)
        end do
    end subroutine write_segments

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

    function drawable_frame_info(self) result(info)
        class(DrawableTrajectory), intent(in) :: self
        character(len=:), allocatable :: info
        info = 'DrawableTrajectory'
    end function drawable_frame_info

    subroutine drawable_build_contents(self, viewer, group)
        class(DrawableTrajectory), intent(inout) :: self
        type(c_ptr), intent(in) :: viewer, group
        integer :: k
        self%viewer = viewer
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
        call self%artists(k)%p%build(self%viewer, group)
        if (compile .and. c_associated(self%viewer)) call check(vkf_viewer_compile_node(self%viewer, group))
        call check(vkf_node_add_child(self%contents_group, group))
        self%artists(k)%group = group
    end subroutine build_artist

end module vkf_artists
