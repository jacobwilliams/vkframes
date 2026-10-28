!> Relative motion near a target in a circular orbit: the Clohessy-Wiltshire
!> (Hill) equations, in the target's LVLH frame (Hill's): R radial (away from
!> the Earth), I in-track (along the velocity: V-bar), C cross-track (along
!> the orbit's angular momentum). A state is [R, I, C, vR, vI, vC] (m, m/s);
!> n is the target's mean motion (rad/s).
!>
!> * `cw_state`: the closed-form solution, the state a time t later.
!> * `propagate_plan`: a plan, the initial state and impulsive burns (Δv in
!>   LVLH at given times), propagated piecewise between the burns: samples
!>   every `dt`, and at each burn (the state just before it).
!> * `radial_hops`: the burns of a series of radial hops along V-bar (each
!>   half an orbit: a radial Δv moves the chaser -4 vR / n in-track and back
!>   to V-bar, where the opposite Δv stops it).
module cw

    use, intrinsic :: iso_fortran_env, only: wp => real64

    implicit none

    private

    public :: cw_state, propagate_plan, radial_hops, sort_burns

    !> An impulsive burn: its time and Δv (LVLH).
    type, public :: Burn
        real(wp) :: t = 0.0_wp                  !! s from the start
        real(wp) :: dv(3) = 0.0_wp              !! [R, I, C] (m/s)
    end type Burn

contains

    !> The state `t` after state `s0` (Clohessy-Wiltshire, closed form).
    pure function cw_state(n, s0, t) result(s)
        real(wp), intent(in) :: n                   !! mean motion (rad/s)
        real(wp), intent(in) :: s0(6)               !! [R, I, C, vR, vI, vC]
        real(wp), intent(in) :: t                   !! s
        real(wp) :: s(6), c, sn, nt
        nt = n * t
        c = cos(nt)
        sn = sin(nt)
        associate (x => s0(1), y => s0(2), z => s0(3), vx => s0(4), vy => s0(5), vz => s0(6))
            s(1) = (4.0_wp - 3.0_wp * c) * x + sn / n * vx + 2.0_wp * (1.0_wp - c) / n * vy
            s(2) = 6.0_wp * (sn - nt) * x + y - 2.0_wp * (1.0_wp - c) / n * vx + (4.0_wp * sn - 3.0_wp * nt) / n * vy
            s(3) = c * z + sn / n * vz
            s(4) = 3.0_wp * n * sn * x + c * vx + 2.0_wp * sn * vy
            s(5) = -6.0_wp * n * (1.0_wp - c) * x - 2.0_wp * sn * vx + (4.0_wp * c - 3.0_wp) * vy
            s(6) = -n * sn * z + c * vz
        end associate
    end function cw_state

    !> The burns in time order (insertion sort: a plan has a few).
    pure subroutine sort_burns(burns)
        type(Burn), intent(inout) :: burns(:)
        type(Burn) :: b
        integer :: i, j
        do i = 2, size(burns)
            b = burns(i)
            j = i - 1
            do while (j >= 1)
                if (burns(j)%t <= b%t) exit
                burns(j + 1) = burns(j)
                j = j - 1
            end do
            burns(j + 1) = b
        end do
    end subroutine sort_burns

    !> The plan (`s0` at time 0, then `burns`, in time order) from 0 to
    !> `t_end`: `t(m)` and `states(6, m)`, every `dt` and at each burn's time
    !> (the state just before the burn; the burn applied after it). `at_burn(k)`:
    !> the state just before burn k (its position: where the burn is).
    subroutine propagate_plan(n, s0, burns, t_end, dt, t, states, at_burn)
        real(wp), intent(in) :: n, s0(6), t_end, dt
        type(Burn), intent(in) :: burns(:)          !! in time order
        real(wp), allocatable, intent(out) :: t(:), states(:,:)
        real(wp), intent(out) :: at_burn(6, size(burns))
        real(wp) :: s(6), t0, target, tnext
        integer :: m, k, cap
        cap = int(t_end / dt) + 2 * size(burns) + 4
        allocate(t(cap), states(6, cap))
        m = 0
        s = s0
        t0 = 0.0_wp                                 ! the time of state s
        call keep(t0, s)
        k = 1                                       ! the next burn
        tnext = dt
        do
            target = min(tnext, t_end)              ! the next sample
            do while (k <= size(burns))             ! the burns up to it, at their times
                if (burns(k)%t > target) exit
                if (burns(k)%t > t0) then
                    s = cw_state(n, s, burns(k)%t - t0)
                    t0 = burns(k)%t
                    call keep(t0, s)
                end if
                at_burn(:, k) = s
                s(4:6) = s(4:6) + burns(k)%dv
                k = k + 1
            end do
            if (target > t0) then
                s = cw_state(n, s, target - t0)
                t0 = target
                call keep(t0, s)
            end if
            if (target >= t_end) exit
            tnext = tnext + dt
        end do
        do while (k <= size(burns))                 ! (after the end: not applied)
            at_burn(:, k) = s
            k = k + 1
        end do
        t = t(1:m)
        states = states(:, 1:m)

    contains

        subroutine keep(tk_, sk)
            real(wp), intent(in) :: tk_, sk(6)
            m = m + 1
            t(m) = tk_
            states(:, m) = sk
        end subroutine keep

    end subroutine propagate_plan

    !> The burns of radial hops along V-bar from in-track position y0 through
    !> the positions `stops` (m), half an orbit each, starting at time 0: a
    !> radial Δv -n dy / 4 at each hop's start, and the opposite at its end
    !> (combined with the next hop's start).
    pure function radial_hops(n, y0, stops) result(burns)
        real(wp), intent(in) :: n, y0, stops(:)
        type(Burn) :: burns(size(stops) + 1)
        real(wp) :: dv_prev, dv, y, half
        integer :: k
        half = acos(-1.0_wp) / n
        y = y0
        dv_prev = 0.0_wp
        do k = 1, size(stops)
            dv = -n * (stops(k) - y) / 4.0_wp
            burns(k)%t = (k - 1) * half
            burns(k)%dv = [dv - dv_prev, 0.0_wp, 0.0_wp]
            dv_prev = -dv                             ! (the radial velocity at the hop's end: -dv)
            y = stops(k)
        end do
        burns(size(stops) + 1)%t = size(stops) * half
        burns(size(stops) + 1)%dv = [-dv_prev, 0.0_wp, 0.0_wp]
    end function radial_hops

end module cw
