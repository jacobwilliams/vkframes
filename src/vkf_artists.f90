!> Trajectory artists: what draws a Trajectory, in a DrawableTrajectory frame, as
!> OpenFrames' TrajectoryArtist, CurveArtist and DrawableTrajectory.
!>
!> A DrawableTrajectory is a frame (its points are in its coordinates) holding
!> artists. Each frame update, an artist draws what changed in its trajectory.
!>
!> A curve is drawn as a chain of line buffers ("chunks"; consecutive chunks
!> share their end point), filled in place with (high, low) float pairs for
!> double precision (see src/vsg/vkf_layer.cpp: EyeRelative). Appended points
!> go into the last chunks, so only those are uploaded again; a chain that is
!> full gets a new chunk (as large as the chain so far, up to `max_chunk`
!> points), so nothing is ever copied or reallocated. A cleared trajectory is
!> redrawn into the same chunks.
!>
!> Trace mode (`set_trace_mode`) draws only the part of the trajectory up to
!> the current time: the points already passed (a change of the draw ranges, no
!> upload), plus a two-point tail from the last of them to the point
!> interpolated at the current time (the only data uploaded each frame). The
!> part passed is from the first point to the current time, in the
!> trajectory's time direction (times may decrease).
!>
!> Artists and their trajectories are not owned: they must stay alive while
!> drawn.
module vkf_artists

    use, intrinsic :: iso_c_binding, only: c_ptr, c_null_ptr, c_associated, c_int, c_float, c_f_pointer
    use vkf_math, only: wp
    use vkf_trajectory, only: Trajectory, DataSource, position_sources, SOURCE_ZERO, SOURCE_POSOPT, &
                              TIME_OUT_OF_RANGE, TIME_NOT_FOUND
    use vkf_frames, only: ReferenceFrame
    use vkf_layer, only: vkf_line_create, vkf_line_points, vkf_line_points_changed, vkf_line_set_range, &
                         vkf_node_add_child, vkf_node_remove_child, vkf_switch_create, &
                         vkf_viewer_compile_node, line_set_style

    implicit none

    private

    integer, parameter :: min_chunk = 64                !! points in the smallest chunk
    integer, parameter, public :: max_chunk = 65536     !! points in a chunk made for appending (1.5 MB)

    !> The base of the trajectory artists (OpenFrames' TrajectoryArtist).
    type, abstract, public :: TrajectoryArtist
        private
        type(Trajectory), pointer :: traj => null()
        logical :: trace = .false.
    contains
        procedure :: set_trajectory, get_trajectory
        procedure :: set_trace_mode, trace_mode
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

    !> A line buffer holding points first .. first+capacity-1 of a curve.
    type :: LineChunk
        type(c_ptr) :: line = c_null_ptr
        integer :: first = 1, capacity = 0
    end type LineChunk

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
        type(c_ptr) :: viewer = c_null_ptr, group = c_null_ptr, tail = c_null_ptr
        type(LineChunk), allocatable :: chunks(:)
        integer :: n_chunks = 0
        integer :: n_written = 0                    !! points written to the chunks
        integer :: n_drawn = 0                      !! points drawn from them
        logical :: tail_shown = .false.
        integer :: seen_generation = -1, seen_revision = -1
        real(wp) :: tail_time = huge(1.0_wp)        !! the time the tail was drawn for
    contains
        procedure :: set_trajectory => curve_set_trajectory
        procedure :: set_x_data, set_y_data, set_z_data, data_sources
        procedure :: set_color, color, set_width, width
        procedure :: set_pattern, pattern
        procedure :: build => curve_build
        procedure :: update => curve_update
        procedure :: num_chunks, num_drawn, drawn_point, trace_end
    end type CurveArtist

    interface CurveArtist
        module procedure new_curve_artist
    end interface CurveArtist

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
        self%seen_generation = -1                     ! redraw
    end subroutine curve_set_trajectory

    !> Where the x (y, z) of the points come from. Returns whether the
    !> trajectory has those data.
    logical function set_x_data(self, src) result(valid)
        class(CurveArtist), intent(inout) :: self
        type(DataSource), intent(in) :: src
        valid = set_source(self, 1, src)
    end function set_x_data

    logical function set_y_data(self, src) result(valid)
        class(CurveArtist), intent(inout) :: self
        type(DataSource), intent(in) :: src
        valid = set_source(self, 2, src)
    end function set_y_data

    logical function set_z_data(self, src) result(valid)
        class(CurveArtist), intent(inout) :: self
        type(DataSource), intent(in) :: src
        valid = set_source(self, 3, src)
    end function set_z_data

    logical function set_source(self, k, src) result(valid)
        class(CurveArtist), intent(inout) :: self
        integer, intent(in) :: k
        type(DataSource), intent(in) :: src
        self%sources_(k) = src
        self%default_data = .false.
        self%seen_generation = -1                     ! redraw
        valid = .true.
        if (associated(self%traj)) valid = self%traj%verify_data(self%sources_)
    end function set_source

    pure function data_sources(self) result(s)
        class(CurveArtist), intent(in) :: self
        type(DataSource) :: s(3)
        s = self%sources_
    end function data_sources

    !> The line's color: [r, g, b] (alpha unchanged) or [r, g, b, a], 0 to 1.
    subroutine set_color(self, color)
        class(CurveArtist), intent(inout) :: self
        real(wp), intent(in) :: color(:)
        integer :: n
        n = min(4, size(color))
        self%color_(1:n) = color(1:n)
        call apply_style(self)
    end subroutine set_color

    pure function color(self) result(c)
        class(CurveArtist), intent(in) :: self
        real(wp) :: c(4)
        c = self%color_
    end function color

    !> The line's width in window units (logical pixels).
    subroutine set_width(self, width)
        class(CurveArtist), intent(inout) :: self
        real(wp), intent(in) :: width
        self%width_ = width
        call apply_style(self)
    end subroutine set_width

    pure real(wp) function width(self)
        class(CurveArtist), intent(in) :: self
        width = self%width_
    end function width

    !> A dash pattern, as OpenGL's line stipple (OpenFrames' setPattern): each
    !> bit of the 16-bit `pattern` covers `factor` pixels. Kept for later; lines
    !> are drawn solid for now.
    subroutine set_pattern(self, factor, pattern)
        class(CurveArtist), intent(inout) :: self
        integer, intent(in) :: factor, pattern
        self%pattern_factor = factor
        self%pattern_ = iand(pattern, int(z'FFFF'))
    end subroutine set_pattern

    pure subroutine pattern(self, factor, bits)
        class(CurveArtist), intent(in) :: self
        integer, intent(out) :: factor, bits
        factor = self%pattern_factor
        bits = self%pattern_
    end subroutine pattern

    !> Number of line buffers the curve is drawn with (0 until built).
    pure integer function num_chunks(self)
        class(CurveArtist), intent(in) :: self
        num_chunks = self%n_chunks
    end function num_chunks

    !> Number of points drawn now (all of them, or in trace mode those passed).
    pure integer function num_drawn(self)
        class(CurveArtist), intent(in) :: self
        num_drawn = self%n_drawn
    end function num_drawn

    !> Point i (1 to num_drawn) as drawn: read back from the line buffers.
    function drawn_point(self, i) result(v)
        class(CurveArtist), intent(in) :: self
        integer, intent(in) :: i
        real(wp) :: v(3)
        real(c_float), dimension(:,:,:), pointer :: p
        integer :: j
        v = 0.0_wp
        do j = 1, self%n_chunks
            associate (c => self%chunks(j))
                if (i < c%first .or. i > c%first + c%capacity - 1) cycle
                call buffer(c%line, p)
                if (associated(p)) v = real(p(:, 1, i-c%first+1), wp) + real(p(:, 2, i-c%first+1), wp)
                return
            end associate
        end do
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
        call buffer(self%tail, p)
        if (associated(p)) v = real(p(:, 1, 2), wp) + real(p(:, 2, 2), wp)
    end function trace_end

    subroutine curve_build(self, viewer, group)
        class(CurveArtist), intent(inout) :: self
        type(c_ptr), intent(in) :: viewer, group
        self%group = group
        self%viewer = c_null_ptr                      ! compiled with the group
        call ensure_capacity(self, point_count(self))
        self%viewer = viewer
        self%tail = vkf_line_create(2_c_int)
        if (c_associated(self%tail)) call check(vkf_node_add_child(group, self%tail))
        call apply_style(self)
        self%seen_generation = -1
    end subroutine curve_build

    subroutine curve_update(self, t)
        class(CurveArtist), intent(inout) :: self
        real(wp), intent(in) :: t
        integer :: n, i1
        logical :: changed
        if (.not. c_associated(self%group)) return
        n = point_count(self)

        ! new points, or a redraw (cleared, new trajectory or data sources)
        changed = .false.
        if (associated(self%traj)) then
            if (self%traj%generation() /= self%seen_generation) then
                self%n_written = 0
                self%seen_generation = self%traj%generation()
                self%seen_revision = -1
            end if
            if (self%traj%revision() /= self%seen_revision) then
                ! from the last point written: set_optional may have changed it
                i1 = max(1, min(self%n_written, n))
                call ensure_capacity(self, n)
                if (n >= i1) call write_points(self, i1, n)
                self%n_written = n
                self%seen_revision = self%traj%revision()
                changed = .true.
            end if
        else
            self%n_written = 0
        end if

        if (.not. self%trace) then
            call set_ranges(self, self%n_written)
            if (c_associated(self%tail)) call check(vkf_line_set_range(self%tail, 1_c_int, 0_c_int))
            self%tail_shown = .false.
            self%tail_time = huge(1.0_wp)
            return
        end if
        ! trace mode: the points passed, and a tail to the point at time t
        if (.not. changed .and. t == self%tail_time) return
        n = points_passed(self, t, self%n_written)
        call set_ranges(self, n)
        call update_tail(self, t, n)
        self%tail_time = t
    end subroutine curve_update

    !> Number of points the data sources give (0 for invalid sources or no
    !> trajectory; 1 if all sources are zero: a point at the origin).
    integer function point_count(self) result(n)
        type(CurveArtist), intent(in) :: self
        n = 0
        if (.not. associated(self%traj)) return
        if (.not. self%traj%verify_data(self%sources_)) return
        n = self%traj%num_points(self%sources_)
        if (all(self%sources_%src == SOURCE_ZERO)) n = min(1, self%traj%num_times())
    end function point_count

    !> Number of points up to time t, from the first point in the trajectory's
    !> time direction.
    integer function points_passed(self, t, n) result(k)
        type(CurveArtist), intent(in) :: self
        real(wp), intent(in) :: t
        integer, intent(in) :: n
        integer :: status, index
        k = 0
        if (.not. associated(self%traj)) return
        status = self%traj%time_index(t, index)
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

    !> Draw points 1 .. k of the curve.
    subroutine set_ranges(self, k)
        type(CurveArtist), intent(inout) :: self
        integer, intent(in) :: k
        integer :: j, m
        self%n_drawn = k
        do j = 1, self%n_chunks
            m = max(0, min(self%chunks(j)%capacity, k - self%chunks(j)%first + 1))
            if (m == 1) m = 0                                 ! (only the shared point: nothing to draw)
            call check(vkf_line_set_range(self%chunks(j)%line, 1_c_int, int(m, c_int)))
        end do
    end subroutine set_ranges

    !> The tail: from point k to the point interpolated at time t (none if
    !> before the first point or at/after the last).
    subroutine update_tail(self, t, k)
        type(CurveArtist), intent(inout) :: self
        real(wp), intent(in) :: t
        integer, intent(in) :: k
        real(c_float), dimension(:,:,:), pointer :: p
        real(wp) :: t0, t1, frac, xyz(3, 2)
        integer :: i
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
        call buffer(self%tail, p)
        if (.not. associated(p)) return
        do i = 1, 2
            p(:, 1, i) = real(xyz(:, i), c_float)
            p(:, 2, i) = real(xyz(:, i) - real(p(:, 1, i), wp), c_float)
        end do
        call check(vkf_line_points_changed(self%tail, 2_c_int))
        self%tail_shown = .true.
    end subroutine update_tail

    !> Chunks for at least n points (at least one chunk).
    subroutine ensure_capacity(self, n)
        type(CurveArtist), intent(inout) :: self
        integer, intent(in) :: n
        type(LineChunk), allocatable :: tmp(:)
        type(LineChunk) :: c
        integer :: covered
        covered = 0
        if (self%n_chunks > 0) covered = self%chunks(self%n_chunks)%first + self%chunks(self%n_chunks)%capacity - 1
        do while (self%n_chunks == 0 .or. covered < n)
            if (self%n_chunks == 0) then
                c%first = 1
                c%capacity = max(min_chunk, n)                 ! the data so far in one chunk
            else
                c%first = covered                              ! sharing the previous chunk's last point
                c%capacity = max(n - covered + 1, min(max_chunk, max(min_chunk, covered)))
            end if
            c%line = vkf_line_create(int(c%capacity, c_int))
            if (.not. c_associated(c%line)) return
            call check(line_set_style(c%line, real(self%color_, c_float), real(self%width_, c_float)))
            if (c_associated(self%viewer)) call check(vkf_viewer_compile_node(self%viewer, c%line))
            call check(vkf_node_add_child(self%group, c%line))
            if (.not. allocated(self%chunks)) allocate(self%chunks(4))
            if (self%n_chunks == size(self%chunks)) then
                allocate(tmp(2 * self%n_chunks))
                tmp(1:self%n_chunks) = self%chunks(1:self%n_chunks)
                call move_alloc(tmp, self%chunks)
            end if
            self%n_chunks = self%n_chunks + 1
            self%chunks(self%n_chunks) = c
            covered = c%first + c%capacity - 1
        end do
    end subroutine ensure_capacity

    !> Write points i1 .. i2 into the chunks holding them, and upload those.
    subroutine write_points(self, i1, i2)
        type(CurveArtist), intent(inout) :: self
        integer, intent(in) :: i1, i2
        real(c_float), dimension(:,:,:), pointer :: p        ! (xyz, high/low, point)
        integer :: j, lo, hi
        do j = 1, self%n_chunks
            associate (c => self%chunks(j))
                lo = max(i1, c%first)
                hi = min(i2, c%first + c%capacity - 1)
                if (lo > hi) cycle
                call buffer(c%line, p)
                if (.not. associated(p)) cycle
                call fill(self, p(:, :, lo-c%first+1:hi-c%first+1), lo, hi)
                call check(vkf_line_points_changed(c%line, int(hi - c%first + 1, c_int)))
            end associate
        end do
    end subroutine write_points

    !> Points i1 .. i2 of the trajectory, split into (high, low) floats.
    subroutine fill(self, p, i1, i2)
        type(CurveArtist), intent(in) :: self
        real(c_float), intent(out) :: p(:,:,:)
        integer, intent(in) :: i1, i2
        real(wp), dimension(:,:), pointer :: pos
        real(wp) :: v(3)
        integer :: i
        if (is_position_xyz(self%sources_)) then          ! the usual case: straight from the positions
            call self%traj%positions(pos)
            p(:, 1, :) = real(pos(:, i1:i2), c_float)
            p(:, 2, :) = real(pos(:, i1:i2) - real(p(:, 1, :), wp), c_float)
            return
        end if
        do i = i1, i2
            v = self%traj%point(i, self%sources_)
            p(:, 1, i-i1+1) = real(v, c_float)
            p(:, 2, i-i1+1) = real(v - real(p(:, 1, i-i1+1), wp), c_float)
        end do
    end subroutine fill

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

    !> The point buffer of `line`: (xyz, high/low, point).
    subroutine buffer(line, p)
        type(c_ptr), intent(in) :: line
        real(c_float), dimension(:,:,:), pointer, intent(out) :: p
        integer(c_int) :: capacity
        type(c_ptr) :: data
        p => null()
        data = vkf_line_points(line, capacity)
        if (c_associated(data)) call c_f_pointer(data, p, [3, 2, int(capacity)])
    end subroutine buffer

    subroutine apply_style(self)
        class(CurveArtist), intent(inout) :: self
        integer :: j
        do j = 1, self%n_chunks
            call check(line_set_style(self%chunks(j)%line, real(self%color_, c_float), real(self%width_, c_float)))
        end do
        if (c_associated(self%tail)) &
            call check(line_set_style(self%tail, real(self%color_, c_float), real(self%width_, c_float)))
    end subroutine apply_style

    subroutine check(status)
        integer(c_int), intent(in) :: status
        if (status /= 0) write(*, '(a)') 'vkframes: a rendering-layer call failed (vkf_artists)'
    end subroutine check

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

    !> Remove an artist (it stops being drawn).
    subroutine remove_artist(self, a)
        class(DrawableTrajectory), intent(inout) :: self
        class(TrajectoryArtist), intent(in), target :: a
        integer :: k
        do k = 1, self%n_artists
            if (associated(self%artists(k)%p, a)) then
                if (c_associated(self%artists(k)%group)) &
                    call check(vkf_node_remove_child(self%contents_group, self%artists(k)%group))
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
