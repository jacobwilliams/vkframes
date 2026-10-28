!> Following trajectories: the position and attitude of a frame from one or more
!> trajectories at the current time, as OpenFrames' TrajectoryFollower.
!>
!> With several trajectories, the one followed at time t is the one whose time
!> range contains t, or the closest one. If t is outside all of them, FOLLOW_LOOP
!> wraps it into their combined time range and FOLLOW_LIMIT holds the ends.
!> Positions are interpolated linearly, attitudes by slerp.
!>
!> The time is either the global simulation time plus an offset
!> (`set_offset_time`, the default with offset 0) or a fixed time (`set_time`).
!> The trajectories are not owned: they must stay alive while followed.
module vkf_follower

    use vkf_math, only: wp, quat_slerp
    use vkf_trajectory, only: Trajectory, DataSource, position_sources, SOURCE_ZERO, &
                              TIME_OUT_OF_RANGE, TIME_NOT_FOUND

    implicit none

    private

    !> follow modes (OpenFrames' FollowMode)
    integer, parameter, public :: FOLLOW_LOOP = 0      !! wrap the time into the trajectories' time range
    integer, parameter, public :: FOLLOW_LIMIT = 1     !! hold the first/last point outside the time range

    !> what to follow (OpenFrames' FollowData), combined with ior
    integer, parameter, public :: FOLLOW_POSITION = 1  !! follow the position
    integer, parameter, public :: FOLLOW_ATTITUDE = 2  !! follow the attitude

    !> A pointer to a followed trajectory (for an array of them).
    type :: TrajectoryPointer
        type(Trajectory), pointer :: p => null()    !! the trajectory (not owned)
    end type TrajectoryPointer

    !> Follows trajectories (see the module documentation).
    type, public :: TrajectoryFollower
        private
        type(TrajectoryPointer), allocatable :: trajs(:)    !! (capacity): the followed trajectories
        integer :: n = 0                            !! number of followed trajectories
        integer :: current = 0                      !! index (in trajs) of the trajectory last followed
        integer :: mode_ = FOLLOW_LOOP              !! FOLLOW_LOOP or FOLLOW_LIMIT
        integer :: data_ = FOLLOW_POSITION + FOLLOW_ATTITUDE    !! what is followed (FOLLOW_* combined with ior)
        type(DataSource) :: sources_(3)             !! where the followed position's x, y, z come from
        logical :: default_data = .true.            !! (true: sources_ are the trajectories' positions)
        logical :: follow_time = .true.             !! (true: simulation time + time_value; false: time_value)
        real(wp) :: time_value = 0.0_wp             !! time offset or fixed time (s)
        real(wp) :: last_time_ = 0.0_wp             !! time (after looping) at the last update (s)
    contains
        procedure :: set_trajectory, add_trajectory, remove_trajectory, remove_all
        procedure :: num_trajectories
        procedure :: set_follow_type, follow_mode, follow_data
        procedure :: set_x_data, set_y_data, set_z_data, data_sources
        procedure :: set_default_data, using_default_data, data_valid
        procedure :: set_time, set_offset_time, is_following_time
        procedure :: last_time, last_trajectory
        procedure :: update
    end type TrajectoryFollower

contains

    ! ==================================================================
    ! trajectories

    !> Follow only `traj` (none if it is not associated).
    subroutine set_trajectory(self, traj)
        class(TrajectoryFollower), intent(inout) :: self
        type(Trajectory), pointer, intent(in) :: traj
        call self%remove_all()
        if (associated(traj)) call self%add_trajectory(traj)
    end subroutine set_trajectory

    !> Also follow `traj` (no effect if it is already followed).
    subroutine add_trajectory(self, traj)
        class(TrajectoryFollower), intent(inout) :: self
        type(Trajectory), pointer, intent(in) :: traj
        type(TrajectoryPointer), allocatable :: tmp(:)
        integer :: k
        if (.not. associated(traj)) return
        do k = 1, self%n
            if (associated(self%trajs(k)%p, traj)) return
        end do
        if (.not. allocated(self%trajs)) allocate(self%trajs(4))
        if (self%n == size(self%trajs)) then
            allocate(tmp(2 * self%n))
            tmp(1:self%n) = self%trajs(1:self%n)
            call move_alloc(tmp, self%trajs)
        end if
        self%n = self%n + 1
        self%trajs(self%n)%p => traj
        if (self%default_data) call self%set_default_data()
    end subroutine add_trajectory

    !> Stop following `traj`.
    subroutine remove_trajectory(self, traj)
        class(TrajectoryFollower), intent(inout) :: self
        type(Trajectory), pointer, intent(in) :: traj
        integer :: k
        do k = 1, self%n
            if (associated(self%trajs(k)%p, traj)) then
                self%trajs(k:self%n-1) = self%trajs(k+1:self%n)
                self%trajs(self%n)%p => null()
                self%n = self%n - 1
                if (self%current >= k) self%current = max(0, self%current - 1)
                exit
            end if
        end do
    end subroutine remove_trajectory

    !> Stop following all trajectories.
    subroutine remove_all(self)
        class(TrajectoryFollower), intent(inout) :: self
        integer :: k
        do k = 1, self%n
            self%trajs(k)%p => null()
        end do
        self%n = 0
        self%current = 0
    end subroutine remove_all

    !> Number of trajectories followed.
    pure integer function num_trajectories(self)
        class(TrajectoryFollower), intent(in) :: self
        num_trajectories = self%n
    end function num_trajectories

    ! ==================================================================
    ! settings

    !> What to follow (FOLLOW_POSITION and/or FOLLOW_ATTITUDE, combined with
    !> ior) and how (FOLLOW_LOOP or FOLLOW_LIMIT).
    subroutine set_follow_type(self, data, mode)
        class(TrajectoryFollower), intent(inout) :: self
        integer, intent(in) :: data     !! FOLLOW_POSITION and/or FOLLOW_ATTITUDE (ior)
        integer, intent(in) :: mode     !! FOLLOW_LOOP or FOLLOW_LIMIT
        self%data_ = data
        self%mode_ = mode
    end subroutine set_follow_type

    !> How time outside the trajectories is handled: FOLLOW_LOOP or FOLLOW_LIMIT.
    pure integer function follow_mode(self)
        class(TrajectoryFollower), intent(in) :: self
        follow_mode = self%mode_
    end function follow_mode

    !> What is followed: FOLLOW_POSITION and/or FOLLOW_ATTITUDE (combined with ior).
    pure integer function follow_data(self)
        class(TrajectoryFollower), intent(in) :: self
        follow_data = self%data_
    end function follow_data

    !> Where the x, y and z of the followed position come from (default: the
    !> trajectories' positions). Returns whether all followed trajectories have
    !> those data.
    logical function set_x_data(self, src) result(valid)
        class(TrajectoryFollower), intent(inout) :: self
        type(DataSource), intent(in) :: src
        self%sources_(1) = src
        self%default_data = .false.
        valid = self%data_valid()
    end function set_x_data

    !> Where the y of the followed position comes from (see `set_x_data`).
    logical function set_y_data(self, src) result(valid)
        class(TrajectoryFollower), intent(inout) :: self
        type(DataSource), intent(in) :: src
        self%sources_(2) = src
        self%default_data = .false.
        valid = self%data_valid()
    end function set_y_data

    !> Where the z of the followed position comes from (see `set_x_data`).
    logical function set_z_data(self, src) result(valid)
        class(TrajectoryFollower), intent(inout) :: self
        type(DataSource), intent(in) :: src
        self%sources_(3) = src
        self%default_data = .false.
        valid = self%data_valid()
    end function set_z_data

    !> Where the followed position's x, y and z come from.
    pure function data_sources(self) result(s)
        class(TrajectoryFollower), intent(in) :: self
        type(DataSource) :: s(3)
        s = self%sources_
    end function data_sources

    !> Use the trajectories' positions (per the first trajectory's DOF).
    subroutine set_default_data(self)
        class(TrajectoryFollower), intent(inout) :: self
        if (self%n > 0) then
            self%sources_ = position_sources(self%trajs(1)%p%dof())
        else
            self%sources_ = position_sources(0)
        end if
        self%default_data = .true.
    end subroutine set_default_data

    !> True if the followed position is the trajectories' positions (`set_default_data`).
    pure logical function using_default_data(self)
        class(TrajectoryFollower), intent(in) :: self
        using_default_data = self%default_data
    end function using_default_data

    !> True if every followed trajectory has the data the position sources use.
    pure logical function data_valid(self)
        class(TrajectoryFollower), intent(in) :: self
        integer :: k
        data_valid = .true.
        do k = 1, self%n
            if (.not. self%trajs(k)%p%verify_data(self%sources_)) data_valid = .false.
        end do
    end function data_valid

    !> Follow at a fixed time (independent of the simulation time).
    subroutine set_time(self, t)
        class(TrajectoryFollower), intent(inout) :: self
        real(wp), intent(in) :: t           !! the time (s)
        self%time_value = t
        self%follow_time = .false.
    end subroutine set_time

    !> Follow at the simulation time plus `offset` (the default, offset 0).
    subroutine set_offset_time(self, offset)
        class(TrajectoryFollower), intent(inout) :: self
        real(wp), intent(in) :: offset      !! added to the simulation time (s)
        self%time_value = offset
        self%follow_time = .true.
    end subroutine set_offset_time

    !> True if following the simulation time plus an offset, false for a fixed time.
    pure logical function is_following_time(self)
        class(TrajectoryFollower), intent(in) :: self
        is_following_time = self%follow_time
    end function is_following_time

    !> The time (after looping) at the last update.
    pure real(wp) function last_time(self)
        class(TrajectoryFollower), intent(in) :: self
        last_time = self%last_time_
    end function last_time

    !> The trajectory followed at the last update (null if none).
    function last_trajectory(self) result(traj)
        class(TrajectoryFollower), intent(in) :: self
        type(Trajectory), pointer :: traj
        traj => null()
        if (self%current >= 1 .and. self%current <= self%n) traj => self%trajs(self%current)%p
    end function last_trajectory

    ! ==================================================================
    ! following

    !> The position and attitude at simulation time `sim_time`. `has_position`
    !> and `has_attitude` say whether each could be computed (and is being
    !> followed); otherwise `position` / `attitude` are left unchanged.
    subroutine update(self, sim_time, position, attitude, has_position, has_attitude)
        class(TrajectoryFollower), intent(inout) :: self
        real(wp), intent(in) :: sim_time            !! simulation time (s)
        real(wp), intent(inout) :: position(3)      !! the followed position (in the trajectories' frame)
        real(wp), intent(inout) :: attitude(4)      !! the followed attitude (quaternion [x, y, z, w])
        logical, intent(out) :: has_position, has_attitude
        type(Trajectory), pointer :: traj
        real(wp) :: t
        integer :: status, i
        has_position = .false.
        has_attitude = .false.
        if (self%n == 0) return
        if (self%follow_time) then
            t = sim_time + self%time_value
        else
            t = self%time_value
        end if
        t = wrapped_time(self, t)
        self%last_time_ = t
        self%current = chosen_trajectory(self, t)
        traj => self%trajs(self%current)%p
        status = traj%time_index(t, i)                      ! (once, for the position and the attitude)
        if (iand(self%data_, FOLLOW_POSITION) /= 0 .and. self%data_valid()) &
            call interpolate_position(traj, self%sources_, t, status, i, position, has_position)
        if (iand(self%data_, FOLLOW_ATTITUDE) /= 0) &
            call interpolate_attitude(traj, t, status, i, attitude, has_attitude)
    end subroutine update

    !> For FOLLOW_LOOP, t wrapped into the combined time range of the trajectories.
    real(wp) function wrapped_time(self, t) result(tw)
        type(TrajectoryFollower), intent(in) :: self
        real(wp), intent(in) :: t
        real(wp) :: t0, tf, a, b
        integer :: k
        tw = t
        if (self%mode_ == FOLLOW_LIMIT) return
        t0 = huge(1.0_wp)
        tf = -huge(1.0_wp)
        do k = 1, self%n
            if (.not. self%trajs(k)%p%time_range(a, b)) cycle
            t0 = min(t0, a)
            tf = max(tf, b)
        end do
        if (t0 == huge(1.0_wp)) return
        if (tf - t0 <= 8.0_wp * tiny(1.0_wp)) then
            tw = t0
        else
            tw = t0 + modulo(t - t0, tf - t0)     ! t - floor((t - t0)/(tf - t0)) * (tf - t0), in real arithmetic
        end if
    end function wrapped_time

    !> The trajectory to follow at time t: the current one if it contains t,
    !> else the first that does, else the closest.
    integer function chosen_trajectory(self, t) result(k)
        type(TrajectoryFollower), intent(in) :: self
        real(wp), intent(in) :: t
        real(wp) :: dist, min_dist
        integer :: j
        k = 1
        if (self%n == 1) return
        if (self%current >= 1 .and. self%current <= self%n) then
            if (self%trajs(self%current)%p%time_distance(t) <= 0.0_wp) then
                k = self%current
                return
            end if
        end if
        min_dist = huge(1.0_wp)
        do j = 1, self%n
            dist = self%trajs(j)%p%time_distance(t)
            if (dist <= 0.0_wp) then
                k = j
                return
            else if (dist < min_dist) then
                min_dist = dist
                k = j
            end if
        end do
    end function chosen_trajectory

    !> Linear interpolation of the position (from `sources`) at time t; the
    !> first/last point outside the time range. `status` and `index`: the
    !> trajectory's time_index for t.
    subroutine interpolate_position(traj, sources, t, status, index, position, ok)
        type(Trajectory), intent(in) :: traj
        type(DataSource), intent(in) :: sources(3)
        real(wp), intent(in) :: t
        integer, intent(in) :: status, index
        real(wp), intent(inout) :: position(3)
        logical, intent(out) :: ok                  !! false if there is no position (position unchanged)
        integer :: n, i
        real(wp) :: frac
        ok = .false.
        n = traj%num_points(sources)
        if (n == 0) return
        if (all(sources%src == SOURCE_ZERO)) then
            position = 0.0_wp
            ok = .true.
            return
        end if
        i = index
        if (status == TIME_NOT_FOUND) return
        if (status == TIME_OUT_OF_RANGE) then
            i = merge(1, n, i < 1)
            position = traj%point(i, sources)
        else if (i > n) then
            position = traj%point(n, sources)            ! fewer points than times: last available
        else
            position = traj%point(i, sources)
            if (i + 1 <= n) then
                if (traj%time(i + 1) /= traj%time(i)) then
                    frac = (t - traj%time(i)) / (traj%time(i + 1) - traj%time(i))
                    position = position + (traj%point(i + 1, sources) - position) * frac
                end if
            end if
        end if
        ok = .true.
    end subroutine interpolate_position

    !> Slerp of the attitude at time t; the first/last attitude outside the time
    !> range. `status` and `index`: the trajectory's time_index for t.
    subroutine interpolate_attitude(traj, t, status, index, attitude, ok)
        type(Trajectory), intent(in) :: traj
        real(wp), intent(in) :: t
        integer, intent(in) :: status, index
        real(wp), intent(inout) :: attitude(4)      !! quaternion [x, y, z, w]
        logical, intent(out) :: ok                  !! false if there is no attitude (attitude unchanged)
        integer :: n, i
        real(wp) :: frac
        ok = .false.
        n = traj%num_attitudes()
        if (n == 0) return
        i = index
        if (status == TIME_NOT_FOUND) return
        if (status == TIME_OUT_OF_RANGE) then
            i = merge(1, n, i < 1)
            attitude = traj%attitude(i)
        else if (i > n) then
            attitude = traj%attitude(n)
        else
            attitude = traj%attitude(i)
            if (i + 1 <= n) then
                if (traj%time(i + 1) /= traj%time(i)) then
                    frac = (t - traj%time(i)) / (traj%time(i + 1) - traj%time(i))
                    attitude = quat_slerp(frac, attitude, traj%attitude(i + 1))
                end if
            end if
        end if
        ok = .true.
    end subroutine interpolate_attitude

end module vkf_follower
