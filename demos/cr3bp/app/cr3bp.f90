!> The circular restricted three-body problem (CR3BP), in the usual
!> nondimensional units: the distance between the primaries is 1, their
!> total mass 1, and their mean motion 1 (time unit: 1 / mean motion). In the
!> frame rotating with them, about their barycenter, the larger primary (mass
!> 1 - mu) is fixed at (-mu, 0, 0) and the smaller (mass mu) at (1 - mu, 0, 0).
!> A state is [x, y, z, vx, vy, vz] in that frame.
!>
!> * `eom`: the equations of motion (with the Coriolis and centrifugal terms).
!> * `jacobi`: the Jacobi constant, C = 2 Omega - v^2, conserved along a
!>   trajectory; `pseudo_potential` gives Omega.
!> * `propagate`: an adaptive Dormand-Prince 5(4) integrator, keeping every
!>   step (and at most `max_step` apart, for a smooth curve), stopping at a
!>   primary's surface (an impact) or, optionally, beyond an escape distance.
!> * `lagrange_points`: L1 to L5.
!> * `sample_states`: a cloud of states of one Jacobi constant: positions
!>   drawn at random where they are allowed (2 Omega >= C: inside the zero
!>   velocity surface), each with the speed that constant gives and a random
!>   direction.
module cr3bp

    use, intrinsic :: iso_fortran_env, only: wp => real64

    implicit none

    private

    public :: eom, jacobi, pseudo_potential, propagate, lagrange_points, sample_states

    !> why `propagate` stopped
    integer, parameter, public :: PROPAGATED = 0     !! the whole time
    integer, parameter, public :: IMPACT_1 = 1       !! at the larger primary's surface
    integer, parameter, public :: IMPACT_2 = 2       !! at the smaller primary's surface
    integer, parameter, public :: STEP_FAILED = 3    !! the step size fell below the smallest
    integer, parameter, public :: ESCAPED = 4        !! beyond the escape distance from the barycenter

contains

    !> The state's derivative [v, a] in the rotating frame.
    pure function eom(mu, s) result(ds)
        real(wp), intent(in) :: mu              !! the mass ratio m2 / (m1 + m2)
        real(wp), intent(in) :: s(6)            !! [x, y, z, vx, vy, vz]
        real(wp) :: ds(6), r1, r2, a1, a2
        r1 = norm2([s(1) + mu, s(2), s(3)])
        r2 = norm2([s(1) - 1.0_wp + mu, s(2), s(3)])
        a1 = (1.0_wp - mu) / r1**3
        a2 = mu / r2**3
        ds(1:3) = s(4:6)
        ds(4) = s(1) + 2.0_wp * s(5) - a1 * (s(1) + mu) - a2 * (s(1) - 1.0_wp + mu)
        ds(5) = s(2) - 2.0_wp * s(4) - (a1 + a2) * s(2)
        ds(6) = -(a1 + a2) * s(3)
    end function eom

    !> The pseudo-potential Omega = (x^2 + y^2) / 2 + (1 - mu) / r1 + mu / r2.
    pure real(wp) function pseudo_potential(mu, r) result(omega)
        real(wp), intent(in) :: mu, r(3)
        omega = 0.5_wp * (r(1)**2 + r(2)**2) + (1.0_wp - mu) / norm2([r(1) + mu, r(2), r(3)]) + &
                mu / norm2([r(1) - 1.0_wp + mu, r(2), r(3)])
    end function pseudo_potential

    !> The Jacobi constant C = 2 Omega - v^2.
    pure real(wp) function jacobi(mu, s) result(c)
        real(wp), intent(in) :: mu, s(6)
        c = 2.0_wp * pseudo_potential(mu, s(1:3)) - dot_product(s(4:6), s(4:6))
    end function jacobi

    !> Propagate `s0` from time 0 to `tf` (negative: backwards): `t(n)` and
    !> `states(6, n)` (the start included), steps at most `max_step` long
    !> (> 0), to a relative and absolute tolerance `tol`. Stops at the
    !> primaries' radii `radius(2)`, and beyond `escape` from the barycenter
    !> (if given); `outcome` says why it stopped.
    subroutine propagate(mu, s0, tf, max_step, tol, radius, t, states, outcome, escape)
        real(wp), intent(in) :: mu, s0(6), tf, max_step, tol
        real(wp), intent(in) :: radius(2)                   !! the primaries' radii (nondimensional)
        real(wp), allocatable, intent(out) :: t(:), states(:,:)
        integer, intent(out) :: outcome                     !! PROPAGATED, IMPACT_1, IMPACT_2, ESCAPED or STEP_FAILED
        real(wp), intent(in), optional :: escape            !! the escape distance (nondimensional; default: none)
        ! Dormand-Prince 5(4) coefficients
        real(wp), parameter :: c2 = 1.0_wp / 5, c3 = 3.0_wp / 10, c4 = 4.0_wp / 5, c5 = 8.0_wp / 9
        real(wp), parameter :: a21 = 1.0_wp / 5
        real(wp), parameter :: a31 = 3.0_wp / 40, a32 = 9.0_wp / 40
        real(wp), parameter :: a41 = 44.0_wp / 45, a42 = -56.0_wp / 15, a43 = 32.0_wp / 9
        real(wp), parameter :: a51 = 19372.0_wp / 6561, a52 = -25360.0_wp / 2187, a53 = 64448.0_wp / 6561, &
                               a54 = -212.0_wp / 729
        real(wp), parameter :: a61 = 9017.0_wp / 3168, a62 = -355.0_wp / 33, a63 = 46732.0_wp / 5247, &
                               a64 = 49.0_wp / 176, a65 = -5103.0_wp / 18656
        real(wp), parameter :: b1 = 35.0_wp / 384, b3 = 500.0_wp / 1113, b4 = 125.0_wp / 192, &
                               b5 = -2187.0_wp / 6784, b6 = 11.0_wp / 84
        real(wp), parameter :: e1 = 71.0_wp / 57600, e3 = -71.0_wp / 16695, e4 = 71.0_wp / 1920, &
                               e5 = -17253.0_wp / 339200, e6 = 22.0_wp / 525, e7 = -1.0_wp / 40
        real(wp) :: k1(6), k2(6), k3(6), k4(6), k5(6), k6(6), k7(6), s(6), y(6), err(6)
        real(wp) :: time, h, dir, e, h_min
        integer :: n

        allocate(t(1024), states(6, 1024))
        n = 1
        t(1) = 0.0_wp
        states(:, 1) = s0
        outcome = PROPAGATED
        if (tf == 0.0_wp) then
            call shrink()
            return
        end if
        dir = sign(1.0_wp, tf)
        time = 0.0_wp
        s = s0
        h = min(max_step, 1.0e-3_wp)
        h_min = 1.0e-12_wp * max(1.0_wp, abs(tf))
        k1 = eom(mu, s)
        do while (dir * (tf - time) > 0.0_wp)
            h = min(h, max_step, dir * (tf - time))
            k2 = eom(mu, s + dir * h * a21 * k1)
            k3 = eom(mu, s + dir * h * (a31 * k1 + a32 * k2))
            k4 = eom(mu, s + dir * h * (a41 * k1 + a42 * k2 + a43 * k3))
            k5 = eom(mu, s + dir * h * (a51 * k1 + a52 * k2 + a53 * k3 + a54 * k4))
            k6 = eom(mu, s + dir * h * (a61 * k1 + a62 * k2 + a63 * k3 + a64 * k4 + a65 * k5))
            y = s + dir * h * (b1 * k1 + b3 * k3 + b4 * k4 + b5 * k5 + b6 * k6)
            k7 = eom(mu, y)
            err = dir * h * (e1 * k1 + e3 * k3 + e4 * k4 + e5 * k5 + e6 * k6 + e7 * k7)
            e = maxval(abs(err) / (tol * (1.0_wp + max(abs(s), abs(y)))))
            if (e <= 1.0_wp) then                       ! accepted: kept
                time = time + dir * h
                s = y
                k1 = k7                                 ! (first same as last)
                call keep(time, s)
                if (norm2([s(1) + mu, s(2), s(3)]) <= radius(1)) then
                    outcome = IMPACT_1
                    exit
                end if
                if (norm2([s(1) - 1.0_wp + mu, s(2), s(3)]) <= radius(2)) then
                    outcome = IMPACT_2
                    exit
                end if
                if (present(escape)) then
                    if (norm2(s(1:3)) > escape) then
                        outcome = ESCAPED
                        exit
                    end if
                end if
            end if
            h = h * min(5.0_wp, max(0.2_wp, 0.9_wp * max(e, 1.0e-10_wp)**(-0.2_wp)))
            if (h < h_min) then
                outcome = STEP_FAILED
                exit
            end if
        end do
        call shrink()

    contains

        subroutine keep(tk, sk)
            real(wp), intent(in) :: tk, sk(6)
            real(wp), allocatable :: t2(:), s2(:,:)
            if (n == size(t)) then
                allocate(t2(2 * n), s2(6, 2 * n))
                t2(1:n) = t
                s2(:, 1:n) = states
                call move_alloc(t2, t)
                call move_alloc(s2, states)
            end if
            n = n + 1
            t(n) = tk
            states(:, n) = sk
        end subroutine keep

        subroutine shrink()
            t = t(1:n)
            states = states(:, 1:n)
        end subroutine shrink

    end subroutine propagate

    !> The Lagrange points L1 to L5, `l(3, 5)`: the collinear ones by Newton's
    !> method on the x axis, the triangular ones at the equilateral points.
    pure function lagrange_points(mu) result(l)
        real(wp), intent(in) :: mu
        real(wp) :: l(3, 5), x, guess(3), h
        integer :: k, i
        h = (mu / 3.0_wp)**(1.0_wp / 3.0_wp)
        guess = [1.0_wp - mu - h, 1.0_wp - mu + h, -1.0_wp - 5.0_wp * mu / 12.0_wp]
        l = 0.0_wp
        do k = 1, 3
            x = guess(k)
            do i = 1, 50
                x = x - f(x) / df(x)
            end do
            l(1, k) = x
        end do
        l(:, 4) = [0.5_wp - mu, sqrt(3.0_wp) / 2.0_wp, 0.0_wp]
        l(:, 5) = [0.5_wp - mu, -sqrt(3.0_wp) / 2.0_wp, 0.0_wp]

    contains

        !> dOmega/dx on the x axis.
        pure real(wp) function f(x)
            real(wp), intent(in) :: x
            f = x - (1.0_wp - mu) * (x + mu) / abs(x + mu)**3 - mu * (x - 1.0_wp + mu) / abs(x - 1.0_wp + mu)**3
        end function f

        pure real(wp) function df(x)
            real(wp), intent(in) :: x
            df = 1.0_wp + 2.0_wp * (1.0_wp - mu) / abs(x + mu)**3 + 2.0_wp * mu / abs(x - 1.0_wp + mu)**3
        end function df

    end function lagrange_points

    !> `n` states of Jacobi constant `c` (fewer, `n_found`, if few positions
    !> are allowed): positions at random in [-extent, extent] (x and y; z too
    !> unless `planar`) where 2 Omega >= c and outside `exclude` of each
    !> primary, velocities of speed sqrt(2 Omega - c) in a random direction (in
    !> the plane if `planar`). Uses the intrinsic random numbers (seed them
    !> first for the same cloud again).
    subroutine sample_states(mu, c, n, extent, planar, exclude, states, n_found)
        real(wp), intent(in) :: mu, c, extent
        integer, intent(in) :: n
        logical, intent(in) :: planar
        real(wp), intent(in) :: exclude(2)                  !! distances from the primaries left out
        real(wp), intent(out) :: states(6, n)
        integer, intent(out) :: n_found
        integer, parameter :: tries_per_state = 200
        real(wp) :: r(3), u(3), v2, theta, z
        integer :: tries
        n_found = 0
        tries = 0
        do while (n_found < n .and. tries < tries_per_state * n)
            tries = tries + 1
            call random_number(r)
            r = extent * (2.0_wp * r - 1.0_wp)
            if (planar) r(3) = 0.0_wp
            if (norm2([r(1) + mu, r(2), r(3)]) < exclude(1)) cycle
            if (norm2([r(1) - 1.0_wp + mu, r(2), r(3)]) < exclude(2)) cycle
            v2 = 2.0_wp * pseudo_potential(mu, r) - c
            if (v2 < 0.0_wp) cycle                              ! (outside the zero velocity surface)
            call random_number(u)
            theta = 2.0_wp * acos(-1.0_wp) * u(1)
            if (planar) then
                u = [cos(theta), sin(theta), 0.0_wp]
            else                                                ! (uniform on the sphere)
                z = 2.0_wp * u(2) - 1.0_wp
                u = [sqrt(1.0_wp - z**2) * cos(theta), sqrt(1.0_wp - z**2) * sin(theta), z]
            end if
            n_found = n_found + 1
            states(:, n_found) = [r, sqrt(v2) * u]
        end do
    end subroutine sample_states

end module cr3bp
