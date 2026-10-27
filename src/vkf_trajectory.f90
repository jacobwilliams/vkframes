!> Trajectories: times with positions, attitudes and optional data vectors, as
!> in OpenFrames' Trajectory.
!>
!> Data are appended: each time may get one position (with `num_optionals`
!> optional vectors), one attitude and one color, so there may be fewer
!> positions, attitudes or colors than times (data can arrive separately).
!> Colors are optional (not in OpenFrames): a CurveArtist draws a trajectory
!> with colors in them, blended along each segment between its points'
!> colors (see CurveArtist%set_point_colors). No sorting is done; times
!> are expected to be monotonic, increasing or decreasing. Indices are 1-based.
!>
!> Differences from OpenFrames:
!> * Storage grows by doubling, positions, attitudes and colors separately, so
!>   a trajectory without attitudes (colors) holds no attitude (color) memory.
!>   `reserve` sets it for a known size; `compact` frees what's unused once a
!>   trajectory is complete (one copy of each array; `storage_bytes` says
!>   what is held).
!> * Instead of subscriber callbacks, a trajectory counts its changes
!>   (`revision`) and clears (`generation`): a reader such as an artist compares
!>   them with what it last saw and updates only what changed (e.g. uploads only
!>   the points appended since).
!> * No locking: vkframes updates and renders on one thread.
module vkf_trajectory

    use, intrinsic :: iso_c_binding, only: c_float
    use, intrinsic :: iso_fortran_env, only: int64
    use vkf_math, only: wp

    implicit none

    private

    ! where a component of a DataSource comes from (OpenFrames' SourceType)
    integer, parameter, public :: SOURCE_ZERO = 0       !! 0
    integer, parameter, public :: SOURCE_TIME = 1       !! the time
    integer, parameter, public :: SOURCE_POSOPT = 2     !! an element of the position or of an optional
    integer, parameter, public :: SOURCE_ATTITUDE = 3   !! an element of the attitude quaternion

    ! results of Trajectory%time_index (as OpenFrames' getTimeIndex)
    integer, parameter, public :: TIME_NOT_FOUND = -2   !! the search did not converge
    integer, parameter, public :: TIME_OUT_OF_RANGE = -1 !! index = 0 (before the first time) or num_times + 1 (after the last)
    integer, parameter, public :: TIME_AT_BOUND = 0     !! index = 1 or num_times (at the first or last time)
    ! a positive result: found, with times(index) <= t < times(index+1) (for increasing times)

    !> Where one component (x, y or z) of a point is taken from, as OpenFrames'
    !> Trajectory::DataSource: `src` is a SOURCE_* constant; for SOURCE_POSOPT,
    !> `element` (1 to dof) of the position (`opt` = 0) or of optional `opt`;
    !> for SOURCE_ATTITUDE, `element` (1 to 4) of the quaternion. The value is
    !> multiplied by `scale`.
    type, public :: DataSource
        integer :: src = SOURCE_ZERO
        integer :: element = 1
        integer :: opt = 0
        real(wp) :: scale = 1.0_wp
    end type DataSource

    !> A trajectory (see the module documentation).
    type, public :: Trajectory
        private
        integer :: dof_ = 3                                 !! degrees of freedom of positions and optionals (2 or 3)
        integer :: nopt_ = 0                                !! optionals per position
        integer :: n_times = 0, n_pos = 0, n_att = 0, n_col = 0
        real(wp), allocatable :: t(:)                       !! (capacity)
        real(wp), allocatable :: posopt(:,:,:)              !! (3, 0:nopt, capacity): position, then optionals
        real(wp), allocatable :: att(:,:)                   !! (4, capacity): quaternions [x, y, z, w]
        real(c_float), allocatable :: col(:,:)              !! (4, capacity): colors [r, g, b, a] (as drawn)
        integer :: revision_ = 0                            !! incremented on every change
        integer :: generation_ = 0                          !! incremented when the data are cleared
    contains
        procedure :: reserve, compact, storage_bytes
        procedure :: clear
        procedure :: set_dof, dof
        procedure :: set_num_optionals, num_optionals
        procedure :: num_times, num_positions, num_attitudes, num_colors, is_empty
        procedure :: add_time, add_position, add_attitude, add_color, set_optional
        generic :: add => add_point, add_points
        procedure, private :: add_point, add_points
        procedure :: time, position, attitude, optional, color
        procedure :: times, positions, attitudes, colors
        procedure :: time_range, time_distance, time_index
        procedure :: point, points, verify_data, num_points
        procedure :: revision, generation
    end type Trajectory

    interface Trajectory
        module procedure new_trajectory
    end interface Trajectory

    interface DataSource
        module procedure new_data_source
    end interface DataSource

    public :: position_sources, optional_sources

contains

    ! ==================================================================
    ! construction and sizes

    !> A trajectory with `dof` (2 or 3, default 3) degrees of freedom and
    !> `nopt` (default 0) optional vectors per position.
    function new_trajectory(dof, nopt) result(traj)
        integer, intent(in), optional :: dof, nopt
        type(Trajectory) :: traj
        if (present(dof)) traj%dof_ = max(2, min(3, dof))
        if (present(nopt)) traj%nopt_ = max(0, nopt)
    end function new_trajectory

    !> A data source (see `DataSource`); `element` defaults to 1, `opt` to 0,
    !> `scale` to 1.
    pure function new_data_source(src, element, opt, scale) result(s)
        integer, intent(in) :: src
        integer, intent(in), optional :: element, opt
        real(wp), intent(in), optional :: scale
        type(DataSource) :: s
        s%src = src
        if (present(element)) s%element = element
        if (present(opt)) s%opt = opt
        if (present(scale)) s%scale = scale
    end function new_data_source

    !> The sources of a trajectory's positions: x, y, (z) of the position, and
    !> zero for components beyond `dof`.
    pure function position_sources(dof) result(s)
        integer, intent(in) :: dof
        type(DataSource) :: s(3)
        integer :: k
        do k = 1, 3
            if (k <= dof) s(k) = DataSource(SOURCE_POSOPT, element=k)
        end do
    end function position_sources

    !> The sources of optional `opt`'s x, y, (z) (zero beyond `dof`, default
    !> 3), each times `scale` (default 1): e.g. a thrust vector stored per
    !> point, for SegmentArtist%set_end_data with relative ends.
    pure function optional_sources(opt, dof, scale) result(s)
        integer, intent(in) :: opt
        integer, intent(in), optional :: dof
        real(wp), intent(in), optional :: scale
        type(DataSource) :: s(3)
        integer :: k, d
        d = 3
        if (present(dof)) d = dof
        do k = 1, min(3, d)
            s(k) = DataSource(SOURCE_POSOPT, element=k, opt=opt, scale=scale)
        end do
    end function optional_sources

    !> Reserve memory for `n` points: times, positions (with their optionals)
    !> unless `positions` is false, and attitudes and colors only if
    !> `attitudes` / `colors` is true. Optional: storage grows as needed, but
    !> reserving avoids reallocations for a known size. It never shrinks the
    !> storage (see `compact`).
    subroutine reserve(self, n, positions, attitudes, colors)
        class(Trajectory), intent(inout) :: self
        integer, intent(in) :: n
        logical, intent(in), optional :: positions, attitudes, colors
        logical :: pos, att, col
        pos = .true.
        att = .false.
        col = .false.
        if (present(positions)) pos = positions
        if (present(attitudes)) att = attitudes
        if (present(colors)) col = colors
        call grow_times(self, n, exact=.true.)
        if (pos) call grow_positions(self, n, exact=.true.)
        if (att) call grow_attitudes(self, n, exact=.true.)
        if (col) call grow_colors(self, n, exact=.true.)
    end subroutine reserve

    !> Free the storage not used: each array is reallocated to the points it
    !> holds (one copy, an array at a time), and arrays holding none are
    !> deallocated. For a trajectory that is complete: growing doubles the
    !> storage, so up to about half of it can be unused. The data and what is
    !> drawn of them don't change; pointers from `times`, `positions`, ...
    !> must be fetched again. Points can still be added (storage grows again).
    subroutine compact(self)
        class(Trajectory), intent(inout) :: self
        real(wp), allocatable :: t(:), posopt(:,:,:), att(:,:)
        real(c_float), allocatable :: col(:,:)
        if (allocated(self%t)) then
            if (size(self%t) /= self%n_times) then
                allocate(t(self%n_times))
                t = self%t(1:self%n_times)
                call move_alloc(t, self%t)
            end if
            if (self%n_times == 0) deallocate(self%t)
        end if
        if (allocated(self%posopt)) then
            if (size(self%posopt, 3) /= self%n_pos) then
                allocate(posopt(3, 0:self%nopt_, self%n_pos))
                posopt = self%posopt(:, :, 1:self%n_pos)
                call move_alloc(posopt, self%posopt)
            end if
            if (self%n_pos == 0) deallocate(self%posopt)
        end if
        if (allocated(self%att)) then
            if (size(self%att, 2) /= self%n_att) then
                allocate(att(4, self%n_att))
                att = self%att(:, 1:self%n_att)
                call move_alloc(att, self%att)
            end if
            if (self%n_att == 0) deallocate(self%att)
        end if
        if (allocated(self%col)) then
            if (size(self%col, 2) /= self%n_col) then
                allocate(col(4, self%n_col))
                col = self%col(:, 1:self%n_col)
                call move_alloc(col, self%col)
            end if
            if (self%n_col == 0) deallocate(self%col)
        end if
    end subroutine compact

    !> The memory the trajectory's data take (bytes), used or not: its
    !> storage's capacity (see `reserve`, `compact`).
    pure integer(int64) function storage_bytes(self) result(b)
        class(Trajectory), intent(in) :: self
        b = 0_int64
        if (allocated(self%t)) b = b + storage_size(self%t, int64) / 8 * size(self%t, kind=int64)
        if (allocated(self%posopt)) b = b + storage_size(self%posopt, int64) / 8 * size(self%posopt, kind=int64)
        if (allocated(self%att)) b = b + storage_size(self%att, int64) / 8 * size(self%att, kind=int64)
        if (allocated(self%col)) b = b + storage_size(self%col, int64) / 8 * size(self%col, kind=int64)
    end function storage_bytes

    !> Remove all points (keeping the memory).
    subroutine clear(self)
        class(Trajectory), intent(inout) :: self
        self%n_times = 0
        self%n_pos = 0
        self%n_att = 0
        self%n_col = 0
        self%generation_ = self%generation_ + 1
        self%revision_ = self%revision_ + 1
    end subroutine clear

    !> Set the degrees of freedom (2 or 3) of positions and optionals; clears the trajectory.
    subroutine set_dof(self, dof)
        class(Trajectory), intent(inout) :: self
        integer, intent(in) :: dof
        self%dof_ = max(2, min(3, dof))
        call self%clear()
    end subroutine set_dof

    pure integer function dof(self)
        class(Trajectory), intent(in) :: self
        dof = self%dof_
    end function dof

    !> Set the number of optional vectors per position; clears the trajectory.
    subroutine set_num_optionals(self, nopt)
        class(Trajectory), intent(inout) :: self
        integer, intent(in) :: nopt
        if (nopt /= self%nopt_ .and. allocated(self%posopt)) deallocate(self%posopt)
        self%nopt_ = max(0, nopt)
        call self%clear()
    end subroutine set_num_optionals

    pure integer function num_optionals(self)
        class(Trajectory), intent(in) :: self
        num_optionals = self%nopt_
    end function num_optionals

    pure integer function num_times(self)
        class(Trajectory), intent(in) :: self
        num_times = self%n_times
    end function num_times

    pure integer function num_positions(self)
        class(Trajectory), intent(in) :: self
        num_positions = self%n_pos
    end function num_positions

    pure integer function num_attitudes(self)
        class(Trajectory), intent(in) :: self
        num_attitudes = self%n_att
    end function num_attitudes

    pure integer function num_colors(self)
        class(Trajectory), intent(in) :: self
        num_colors = self%n_col
    end function num_colors

    pure logical function is_empty(self)
        class(Trajectory), intent(in) :: self
        is_empty = self%n_times == 0
    end function is_empty

    !> Number of changes so far (any added, set or cleared data).
    pure integer function revision(self)
        class(Trajectory), intent(in) :: self
        revision = self%revision_
    end function revision

    !> Number of times the data were cleared (by `clear`, `set_dof` or
    !> `set_num_optionals`): while it is unchanged, data were only appended.
    pure integer function generation(self)
        class(Trajectory), intent(in) :: self
        generation = self%generation_
    end function generation

    ! ==================================================================
    ! adding data

    !> Add a time at the end of the time list.
    subroutine add_time(self, t)
        class(Trajectory), intent(inout) :: self
        real(wp), intent(in) :: t
        call grow_times(self, self%n_times + 1)
        self%n_times = self%n_times + 1
        self%t(self%n_times) = t
        self%revision_ = self%revision_ + 1
    end subroutine add_time

    !> Add a position (the first `dof` elements of `pos` are used) for the next
    !> time that has none. `ok` is false if every time already has a position.
    subroutine add_position(self, pos, ok)
        class(Trajectory), intent(inout) :: self
        real(wp), intent(in) :: pos(:)
        logical, intent(out), optional :: ok
        integer :: d
        if (present(ok)) ok = .false.
        if (self%n_pos >= self%n_times) return
        call grow_positions(self, self%n_pos + 1)
        self%n_pos = self%n_pos + 1
        d = min(self%dof_, size(pos))
        self%posopt(:, :, self%n_pos) = 0.0_wp
        self%posopt(1:d, 0, self%n_pos) = pos(1:d)
        self%revision_ = self%revision_ + 1
        if (present(ok)) ok = .true.
    end subroutine add_position

    !> Add an attitude quaternion [x, y, z, w] for the next time that has none.
    !> `ok` is false if every time already has an attitude.
    subroutine add_attitude(self, q, ok)
        class(Trajectory), intent(inout) :: self
        real(wp), intent(in) :: q(4)
        logical, intent(out), optional :: ok
        if (present(ok)) ok = .false.
        if (self%n_att >= self%n_times) return
        call grow_attitudes(self, self%n_att + 1)
        self%n_att = self%n_att + 1
        self%att(:, self%n_att) = q
        self%revision_ = self%revision_ + 1
        if (present(ok)) ok = .true.
    end subroutine add_attitude

    !> Add a color ([r, g, b], alpha 1, or [r, g, b, a], 0 to 1) for the next
    !> time that has none. `ok` is false if every time already has a color.
    subroutine add_color(self, color, ok)
        class(Trajectory), intent(inout) :: self
        real(wp), intent(in) :: color(:)
        logical, intent(out), optional :: ok
        if (present(ok)) ok = .false.
        if (self%n_col >= self%n_times) return
        call grow_colors(self, self%n_col + 1)
        self%n_col = self%n_col + 1
        call set_rgba(self%col(:, self%n_col), color)
        self%revision_ = self%revision_ + 1
        if (present(ok)) ok = .true.
    end subroutine add_color

    !> rgba from [r, g, b] (alpha 1) or [r, g, b, a].
    pure subroutine set_rgba(rgba, color)
        real(c_float), intent(out) :: rgba(4)
        real(wp), intent(in) :: color(:)
        integer :: n
        n = min(4, size(color))
        rgba = 1.0_c_float
        rgba(1:n) = real(color(1:n), c_float)
    end subroutine set_rgba

    !> Set optional vector `index` (1 to num_optionals) of the most recently
    !> added position. `ok` is false if there is no position or no such optional.
    subroutine set_optional(self, index, v, ok)
        class(Trajectory), intent(inout) :: self
        integer, intent(in) :: index
        real(wp), intent(in) :: v(:)
        logical, intent(out), optional :: ok
        integer :: d
        if (present(ok)) ok = .false.
        if (index < 1 .or. index > self%nopt_ .or. self%n_pos == 0) return
        d = min(self%dof_, size(v))
        self%posopt(1:d, index, self%n_pos) = v(1:d)
        self%revision_ = self%revision_ + 1
        if (present(ok)) ok = .true.
    end subroutine set_optional

    !> Add a point: a time with an optional position, attitude and color
    !> ([r, g, b] or [r, g, b, a]).
    subroutine add_point(self, t, pos, att, color)
        class(Trajectory), intent(inout) :: self
        real(wp), intent(in) :: t
        real(wp), intent(in), optional :: pos(:), att(4), color(:)
        call self%add_time(t)
        if (present(pos)) call self%add_position(pos)
        if (present(att)) call self%add_attitude(att)
        if (present(color)) call self%add_color(color)
    end subroutine add_point

    !> Add points in bulk: times `t(n)`, with optional positions `pos(dof, n)`,
    !> attitudes `att(4, n)` and colors `color(3 or 4, n)`. Times, positions,
    !> attitudes and colors must all have been complete before (as many of each
    !> as times) for the given ones to line up with `t`.
    subroutine add_points(self, t, pos, att, color)
        class(Trajectory), intent(inout) :: self
        real(wp), intent(in) :: t(:)
        real(wp), intent(in), optional :: pos(:,:), att(:,:), color(:,:)
        integer :: n, i0, d
        n = size(t)
        i0 = self%n_times
        call grow_times(self, i0 + n)
        self%t(i0+1:i0+n) = t
        self%n_times = i0 + n
        if (present(pos)) then
            n = min(size(pos, 2), self%n_times - self%n_pos)
            d = min(self%dof_, size(pos, 1))
            call grow_positions(self, self%n_pos + n)
            self%posopt(:, :, self%n_pos+1:self%n_pos+n) = 0.0_wp
            self%posopt(1:d, 0, self%n_pos+1:self%n_pos+n) = pos(1:d, 1:n)
            self%n_pos = self%n_pos + n
        end if
        if (present(att)) then
            n = min(size(att, 2), self%n_times - self%n_att)
            call grow_attitudes(self, self%n_att + n)
            self%att(:, self%n_att+1:self%n_att+n) = att(:, 1:n)
            self%n_att = self%n_att + n
        end if
        if (present(color)) then
            n = min(size(color, 2), self%n_times - self%n_col)
            d = min(4, size(color, 1))
            call grow_colors(self, self%n_col + n)
            self%col(:, self%n_col+1:self%n_col+n) = 1.0_c_float
            self%col(1:d, self%n_col+1:self%n_col+n) = real(color(1:d, 1:n), c_float)
            self%n_col = self%n_col + n
        end if
        self%revision_ = self%revision_ + 1
    end subroutine add_points

    ! ==================================================================
    ! reading data

    pure real(wp) function time(self, i)
        class(Trajectory), intent(in) :: self
        integer, intent(in) :: i
        time = self%t(i)
    end function time

    !> Position i (z = 0 for a 2-DOF trajectory).
    pure function position(self, i) result(p)
        class(Trajectory), intent(in) :: self
        integer, intent(in) :: i
        real(wp) :: p(3)
        p = self%posopt(:, 0, i)
    end function position

    pure function attitude(self, i) result(q)
        class(Trajectory), intent(in) :: self
        integer, intent(in) :: i
        real(wp) :: q(4)
        q = self%att(:, i)
    end function attitude

    !> Color i ([r, g, b, a]).
    pure function color(self, i) result(c)
        class(Trajectory), intent(in) :: self
        integer, intent(in) :: i
        real(wp) :: c(4)
        c = real(self%col(:, i), wp)
    end function color

    !> Optional vector k (1 to num_optionals) of position i.
    pure function optional(self, i, k) result(v)
        class(Trajectory), intent(in) :: self
        integer, intent(in) :: i, k
        real(wp) :: v(3)
        v = self%posopt(:, k, i)
    end function optional

    !> Pointer to the times (num_times), without copying.
    subroutine times(self, t)
        class(Trajectory), intent(in), target :: self
        real(wp), dimension(:), pointer, intent(out) :: t
        if (self%n_times == 0) then
            t => null()
        else
            t => self%t(1:self%n_times)
        end if
    end subroutine times

    !> Pointer to the positions (3, num_positions), without copying.
    subroutine positions(self, p)
        class(Trajectory), intent(in), target :: self
        real(wp), dimension(:,:), pointer, intent(out) :: p
        if (self%n_pos == 0) then
            p => null()
        else
            p => self%posopt(:, 0, 1:self%n_pos)
        end if
    end subroutine positions

    !> Pointer to the attitudes (4, num_attitudes), without copying.
    subroutine attitudes(self, q)
        class(Trajectory), intent(in), target :: self
        real(wp), dimension(:,:), pointer, intent(out) :: q
        if (self%n_att == 0) then
            q => null()
        else
            q => self%att(:, 1:self%n_att)
        end if
    end subroutine attitudes

    !> Pointer to the colors (4, num_colors; single precision, as drawn),
    !> without copying.
    subroutine colors(self, c)
        class(Trajectory), intent(in), target :: self
        real(c_float), dimension(:,:), pointer, intent(out) :: c
        if (self%n_col == 0) then
            c => null()
        else
            c => self%col(:, 1:self%n_col)
        end if
    end subroutine colors

    !> The first and last times, ordered (t0 <= tf); false if there are no times.
    logical function time_range(self, t0, tf)
        class(Trajectory), intent(in) :: self
        real(wp), intent(out) :: t0, tf
        time_range = self%n_times > 0
        if (.not. time_range) then
            t0 = 0.0_wp
            tf = 0.0_wp
            return
        end if
        t0 = min(self%t(1), self%t(self%n_times))
        tf = max(self%t(1), self%t(self%n_times))
    end function time_range

    !> The "distance" from time t to the time range [t0, tf]: > 0 outside it (to
    !> the nearer end), 0 at its ends, < 0 inside; huge() without times.
    !> (OpenFrames' getTimeDistance.)
    pure real(wp) function time_distance(self, t)
        class(Trajectory), intent(in) :: self
        real(wp), intent(in) :: t
        real(wp) :: t0, tf
        if (self%n_times == 0) then
            time_distance = huge(1.0_wp)
            return
        end if
        t0 = min(self%t(1), self%t(self%n_times))
        tf = max(self%t(1), self%t(self%n_times))
        if (t <= t0) then
            time_distance = t0 - t
        else if (t >= tf) then
            time_distance = t - tf
        else
            time_distance = (t - t0) * (t - tf)
        end if
    end function time_distance

    !> Find the interval of the time list containing t, by bisection (times
    !> increasing or decreasing). Returns a TIME_* code, or the number of
    !> iterations (> 0) when found, with times(index) <= t < times(index+1) for
    !> increasing times (>= and > for decreasing). See the TIME_* constants for
    !> `index` in the other cases.
    integer function time_index(self, t, index) result(status)
        class(Trajectory), intent(in) :: self
        real(wp), intent(in) :: t
        integer, intent(out) :: index
        integer :: low, high, mid, iter, n
        real(wp) :: dir, dist
        n = self%n_times
        if (n == 0) then
            index = 0
            status = TIME_OUT_OF_RANGE
            return
        end if
        dir = merge(1.0_wp, -1.0_wp, self%t(1) <= self%t(n))
        dist = self%time_distance(t)
        if (dist > 0.0_wp) then
            index = merge(0, n + 1, dir * t < dir * self%t(1))
            status = TIME_OUT_OF_RANGE
            return
        else if (dist == 0.0_wp) then
            index = merge(1, n, t == self%t(1))
            status = TIME_AT_BOUND
            return
        end if
        low = 1
        high = n
        do iter = 1, 64
            mid = low + (high - low) / 2
            if (dir * t < dir * self%t(mid)) then
                high = mid
            else
                low = mid
            end if
            if (high - low <= 1) then
                index = low
                status = iter
                return
            end if
        end do
        index = low
        status = TIME_NOT_FOUND
    end function time_index

    !> Point i with components from `sources` (OpenFrames' getPoint). No checks:
    !> use `verify_data` and `num_points` first.
    pure function point(self, i, sources) result(v)
        class(Trajectory), intent(in) :: self
        integer, intent(in) :: i
        type(DataSource), intent(in) :: sources(3)
        real(wp) :: v(3)
        integer :: k
        do k = 1, 3
            select case (sources(k)%src)
            case (SOURCE_TIME)
                v(k) = sources(k)%scale * self%t(i)
            case (SOURCE_POSOPT)
                v(k) = sources(k)%scale * self%posopt(sources(k)%element, sources(k)%opt, i)
            case (SOURCE_ATTITUDE)
                v(k) = sources(k)%scale * self%att(sources(k)%element, i)
            case default
                v(k) = 0.0_wp
            end select
        end do
    end function point

    !> Points i1, i1 + stride, ... (up to i2) with components from `sources`, into
    !> v(3, :) (as many columns as points; `point` for a range, a whole array
    !> slice per component). No checks: use `verify_data` and `num_points` first.
    pure subroutine points(self, i1, i2, sources, v, stride)
        class(Trajectory), intent(in) :: self
        integer, intent(in) :: i1, i2
        type(DataSource), intent(in) :: sources(3)
        real(wp), intent(out) :: v(:,:)
        integer, intent(in), optional :: stride
        integer :: k, s
        s = 1
        if (present(stride)) s = stride
        do k = 1, 3
            associate (src => sources(k))
                select case (src%src)
                case (SOURCE_TIME)
                    v(k, :) = src%scale * self%t(i1:i2:s)
                case (SOURCE_POSOPT)
                    v(k, :) = src%scale * self%posopt(src%element, src%opt, i1:i2:s)
                case (SOURCE_ATTITUDE)
                    v(k, :) = src%scale * self%att(src%element, i1:i2:s)
                case default
                    v(k, :) = 0.0_wp
                end select
            end associate
        end do
    end subroutine points

    !> True if the trajectory has the data the sources refer to (elements and
    !> optionals in range).
    pure logical function verify_data(self, sources)
        class(Trajectory), intent(in) :: self
        type(DataSource), intent(in) :: sources(3)
        integer :: k
        verify_data = .true.
        do k = 1, 3
            select case (sources(k)%src)
            case (SOURCE_ZERO, SOURCE_TIME)
            case (SOURCE_POSOPT)
                if (sources(k)%element < 1 .or. sources(k)%element > self%dof_ .or. &
                    sources(k)%opt < 0 .or. sources(k)%opt > self%nopt_) verify_data = .false.
            case (SOURCE_ATTITUDE)
                if (sources(k)%element < 1 .or. sources(k)%element > 4) verify_data = .false.
            case default
                verify_data = .false.
            end select
        end do
    end function verify_data

    !> Number of points available with these sources: the number of times,
    !> limited by the positions or attitudes they use; huge() if all are zero.
    pure integer function num_points(self, sources)
        class(Trajectory), intent(in) :: self
        type(DataSource), intent(in) :: sources(3)
        integer :: k
        if (all(sources%src == SOURCE_ZERO)) then
            num_points = huge(0)
            return
        end if
        num_points = self%n_times
        do k = 1, 3
            if (sources(k)%src == SOURCE_POSOPT) num_points = min(num_points, self%n_pos)
            if (sources(k)%src == SOURCE_ATTITUDE) num_points = min(num_points, self%n_att)
        end do
    end function num_points

    ! ==================================================================
    ! storage (amortized doubling)

    !> The capacity to grow to for `needed` points: twice the current, or what is
    !> needed and a quarter more (so that appending after a large bulk add
    !> doesn't copy everything at once); `exact` (reserve): what is needed.
    pure integer function new_capacity(current, needed, exact)
        integer, intent(in) :: current, needed
        logical, intent(in), optional :: exact
        new_capacity = max(needed + needed / 4, 2 * current, 16)
        if (present(exact)) then
            if (exact) new_capacity = needed
        end if
    end function new_capacity

    subroutine grow_times(self, n, exact)
        type(Trajectory), intent(inout) :: self
        integer, intent(in) :: n
        logical, intent(in), optional :: exact
        real(wp), allocatable :: tmp(:)
        integer :: cap
        cap = 0
        if (allocated(self%t)) cap = size(self%t)
        if (n <= cap) return
        allocate(tmp(new_capacity(cap, n, exact)))
        if (self%n_times > 0) tmp(1:self%n_times) = self%t(1:self%n_times)
        call move_alloc(tmp, self%t)
    end subroutine grow_times

    subroutine grow_positions(self, n, exact)
        type(Trajectory), intent(inout) :: self
        integer, intent(in) :: n
        logical, intent(in), optional :: exact
        real(wp), allocatable :: tmp(:,:,:)
        integer :: cap
        cap = 0
        if (allocated(self%posopt)) cap = size(self%posopt, 3)
        if (n <= cap) return
        allocate(tmp(3, 0:self%nopt_, new_capacity(cap, n, exact)))
        if (self%n_pos > 0) tmp(:, :, 1:self%n_pos) = self%posopt(:, :, 1:self%n_pos)
        call move_alloc(tmp, self%posopt)
    end subroutine grow_positions

    subroutine grow_attitudes(self, n, exact)
        type(Trajectory), intent(inout) :: self
        integer, intent(in) :: n
        logical, intent(in), optional :: exact
        real(wp), allocatable :: tmp(:,:)
        integer :: cap
        cap = 0
        if (allocated(self%att)) cap = size(self%att, 2)
        if (n <= cap) return
        allocate(tmp(4, new_capacity(cap, n, exact)))
        if (self%n_att > 0) tmp(:, 1:self%n_att) = self%att(:, 1:self%n_att)
        call move_alloc(tmp, self%att)
    end subroutine grow_attitudes

    subroutine grow_colors(self, n, exact)
        type(Trajectory), intent(inout) :: self
        integer, intent(in) :: n
        logical, intent(in), optional :: exact
        real(c_float), allocatable :: tmp(:,:)
        integer :: cap
        cap = 0
        if (allocated(self%col)) cap = size(self%col, 2)
        if (n <= cap) return
        allocate(tmp(4, new_capacity(cap, n, exact)))
        if (self%n_col > 0) tmp(:, 1:self%n_col) = self%col(:, 1:self%n_col)
        call move_alloc(tmp, self%col)
    end subroutine grow_colors

end module vkf_trajectory
