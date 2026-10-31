!> The simulation clock: simulation time from wall-clock time, with a time
!> scale, pause and a time range, as in OpenFrames' WindowProxy.
!>
!> While running, time = offset + (wall - wall_ref) * scale, limited to the
!> time range. Setting the time, pausing and changing the scale restart the
!> count from the current time (offset = time, wall_ref = now). A clock can be
!> synchronized to another one (`synchronize`), whose time it then reports and
!> whose settings it changes.
!>
!> Wall time is in seconds from `system_clock`, unless given explicitly with the
!> optional `wall` arguments (for tests, or for driving the clock from a host).
module vkf_clock

    use, intrinsic :: iso_fortran_env, only: int64
    use vkf_math, only: wp

    implicit none

    private

    !> A simulation clock (see the module documentation).
    type, public :: SimulationClock
        private
        real(wp) :: time_ = 0.0_wp          !! the current simulation time (s)
        real(wp) :: offset = 0.0_wp         !! simulation time at wall_ref (s)
        real(wp) :: wall_ref = 0.0_wp       !! wall time (s) at which counting restarted
        real(wp) :: scale_ = 1.0_wp         !! simulation seconds per wall second
        real(wp) :: t_min = -huge(1.0_wp)   !! start of the time range (s; default unlimited)
        real(wp) :: t_max = huge(1.0_wp)    !! end of the time range (s; default unlimited)
        logical :: paused_ = .false.        !! true while paused
        logical :: started = .false.        !! false until counting first started (set_time or update)
        type(SimulationClock), pointer :: sync => null()    !! the clock this one is synchronized to (not owned)
    contains
        procedure :: update
        procedure :: time, set_time
        procedure :: set_time_range, time_range
        procedure :: pause, is_paused
        procedure :: set_time_scale, time_scale
        procedure :: synchronize, synchronized_to
    end type SimulationClock

    public :: wall_time

contains

    !> Wall-clock time in seconds (system_clock), for the clock's default.
    real(wp) function wall_time()
        integer(int64) :: count, rate
        call system_clock(count, rate)
        wall_time = real(count, wp) / real(rate, wp)
    end function wall_time

    !> The wall time (s): `wall` if present, else `wall_time()`.
    real(wp) function now(wall)
        real(wp), intent(in), optional :: wall      !! wall time (s)
        if (present(wall)) then
            now = wall
        else
            now = wall_time()
        end if
    end function now

    !> Advance the clock to the current wall time (call once per frame) and
    !> return the simulation time.
    real(wp) function update(self, wall) result(t)
        class(SimulationClock), intent(inout) :: self
        real(wp), intent(in), optional :: wall      !! wall time (s; default: wall_time())
        if (associated(self%sync)) then
            self%time_ = self%sync%time()
        else
            if (.not. self%started) call self%set_time(self%time_, wall)
            if (.not. self%paused_) &
                self%time_ = max(self%t_min, min(self%t_max, self%offset + (now(wall) - self%wall_ref) * self%scale_))
        end if
        t = self%time_
    end function update

    !> The simulation time at the last `update` (or as last set).
    recursive real(wp) function time(self)
        class(SimulationClock), intent(in) :: self
        if (associated(self%sync)) then
            time = self%sync%time()
        else
            time = self%time_
        end if
    end function time

    !> Set the simulation time (limited to the time range) and restart counting from it.
    recursive subroutine set_time(self, t, wall)
        class(SimulationClock), intent(inout) :: self
        real(wp), intent(in) :: t                   !! simulation time (s)
        real(wp), intent(in), optional :: wall      !! wall time (s; default: wall_time())
        if (associated(self%sync)) then
            call self%sync%set_time(t, wall)
            return
        end if
        self%time_ = max(self%t_min, min(self%t_max, t))
        self%offset = self%time_
        self%wall_ref = now(wall)
        self%started = .true.
    end subroutine set_time

    !> Limit the simulation time to [t_min, t_max] (ignored if t_max < t_min).
    recursive subroutine set_time_range(self, t_min, t_max)
        class(SimulationClock), intent(inout) :: self
        real(wp), intent(in) :: t_min, t_max        !! the time range (s)
        if (associated(self%sync)) then
            call self%sync%set_time_range(t_min, t_max)
        else if (t_max >= t_min) then
            self%t_min = t_min
            self%t_max = t_max
        end if
    end subroutine set_time_range

    !> The time range the simulation time is limited to (s; +-huge() if unlimited).
    recursive subroutine time_range(self, t_min, t_max)
        class(SimulationClock), intent(in) :: self
        real(wp), intent(out) :: t_min, t_max       !! the time range (s)
        if (associated(self%sync)) then
            call self%sync%time_range(t_min, t_max)
        else
            t_min = self%t_min
            t_max = self%t_max
        end if
    end subroutine time_range

    !> Pause (true) or resume (false) the simulation time.
    recursive subroutine pause(self, paused, wall)
        class(SimulationClock), intent(inout) :: self
        logical, intent(in) :: paused
        real(wp), intent(in), optional :: wall      !! wall time (s; default: wall_time())
        if (associated(self%sync)) then
            call self%sync%pause(paused, wall)
            return
        end if
        self%paused_ = paused
        call self%set_time(self%time_, wall)
    end subroutine pause

    !> True if the simulation time is paused.
    recursive logical function is_paused(self)
        class(SimulationClock), intent(in) :: self
        if (associated(self%sync)) then
            is_paused = self%sync%is_paused()
        else
            is_paused = self%paused_
        end if
    end function is_paused

    !> Simulation seconds per wall-clock second (may be 0 or negative).
    recursive subroutine set_time_scale(self, scale, wall)
        class(SimulationClock), intent(inout) :: self
        real(wp), intent(in) :: scale               !! simulation seconds per wall-clock second
        real(wp), intent(in), optional :: wall      !! wall time (s; default: wall_time())
        if (associated(self%sync)) then
            call self%sync%set_time_scale(scale, wall)
            return
        end if
        self%scale_ = scale
        call self%set_time(self%time_, wall)
    end subroutine set_time_scale

    !> Simulation seconds per wall-clock second.
    recursive real(wp) function time_scale(self)
        class(SimulationClock), intent(in) :: self
        if (associated(self%sync)) then
            time_scale = self%sync%time_scale()
        else
            time_scale = self%scale_
        end if
    end function time_scale

    !> Follow `master`'s time (it must stay alive while synchronized), or stop
    !> following (master not associated, or this clock itself), keeping the
    !> master's current time and settings. Returns false, and changes nothing,
    !> if this would make a cycle.
    recursive logical function synchronize(self, master, wall) result(ok)
        class(SimulationClock), intent(inout), target :: self
        type(SimulationClock), pointer, intent(in) :: master    !! the clock to follow (null: stop following)
        real(wp), intent(in), optional :: wall      !! wall time (s; default: wall_time())
        type(SimulationClock), pointer :: c
        real(wp) :: t
        ok = .true.
        if (.not. associated(master)) then
            if (associated(self%sync)) then
                t = self%sync%time()
                self%scale_ = self%sync%time_scale()
                self%paused_ = self%sync%is_paused()
                call self%sync%time_range(self%t_min, self%t_max)
                self%sync => null()
                call self%set_time(t, wall)
            end if
            return
        end if
        if (same_clock(master, self)) then
            ok = self%synchronize(null(), wall)
            return
        end if
        c => master
        do while (associated(c))
            if (same_clock(c, self)) then
                ok = .false.
                return
            end if
            c => c%sync
        end do
        self%sync => master
    end function synchronize

    !> The clock this one is synchronized to (null if none).
    function synchronized_to(self) result(master)
        class(SimulationClock), intent(in) :: self
        type(SimulationClock), pointer :: master
        master => self%sync
    end function synchronized_to

    !> True if `a` points to the clock `b`.
    logical function same_clock(a, b)
        type(SimulationClock), pointer, intent(in) :: a
        class(SimulationClock), intent(in), target :: b
        select type (b)
        type is (SimulationClock)
            same_clock = associated(a, b)
        class default
            same_clock = .false.
        end select
    end function same_clock

end module vkf_clock
