!> The circular restricted three-body problem (Earth-Moon) for the halo
!> demo: the equations of motion with the state transition matrix, an
!> adaptive Dormand-Prince 5(4) integrator, halo orbits by differential
!> correction (from Richardson's approximation for a given amplitude, about
!> L1, L2 or L3; and continuation in the Jacobi constant), the monodromy
!> matrix's eigenvectors and the zero-velocity curves (see main.f90).
module halo_cr3bp

    use vkframes, only: wp

    implicit none

    private

    public :: eom, eom_stm, propagate, propagate_to_crossing, correct_halo, correct_halo_jacobi, continue_halo, &
              monodromy_vectors, monodromy_eigenvalues, jacobi, zero_velocity_curves, collinear_point, richardson_halo, halo_from_amplitude

    real(wp), parameter, public :: mu = 0.01215058560962404_wp     !! Earth-Moon mass ratio
    real(wp), parameter, public :: lu = 384400.0_wp                !! length unit (km)
    real(wp), parameter, public :: tu_days = 4.342479_wp           !! time unit (days)
    real(wp), parameter :: tol = 1.0e-12_wp                         !! integration tolerance

    abstract interface
        !> The derivative of state s (autonomous).
        pure function rhs(s) result(ds)
            import :: wp
            implicit none
            real(wp), intent(in) :: s(:)
            real(wp) :: ds(size(s))
        end function rhs
    end interface

contains

    !> The CR3BP equations of motion, rotating frame, nondimensional: s =
    !> [x, y, z, vx, vy, vz]; the Earth at (-mu, 0, 0), the Moon at (1 - mu, 0, 0).
    pure function eom(s) result(ds)
        real(wp), intent(in) :: s(:)
        real(wp) :: ds(size(s))
        real(wp) :: r1, r2
        associate (x => s(1), y => s(2), z => s(3))
            r1 = norm2([x + mu, y, z])
            r2 = norm2([x - 1.0_wp + mu, y, z])
            ds(1:3) = s(4:6)
            ds(4) = 2.0_wp * s(5) + x - (1.0_wp - mu) * (x + mu) / r1**3 - mu * (x - 1.0_wp + mu) / r2**3
            ds(5) = -2.0_wp * s(4) + y - (1.0_wp - mu) * y / r1**3 - mu * y / r2**3
            ds(6) = -(1.0_wp - mu) * z / r1**3 - mu * z / r2**3
        end associate
    end function eom

    !> The Jacobi constant of state s: 2 U - v^2.
    pure real(wp) function jacobi(s)
        real(wp), intent(in) :: s(6)
        jacobi = two_u(s(1), s(2), s(3)) - sum(s(4:6)**2)
    end function jacobi

    !> Twice the pseudo-potential at (x, y, z).
    pure real(wp) function two_u(x, y, z)
        real(wp), intent(in) :: x, y, z
        two_u = x**2 + y**2 + 2.0_wp * ((1.0_wp - mu) / norm2([x + mu, y, z]) + mu / norm2([x - 1.0_wp + mu, y, z]))
    end function two_u

    !> The zero-velocity curves of Jacobi constant c in the x-y plane, within
    !> |x|, |y| <= extent: segments (points 1-2, 3-4, ...; z = 0) by marching
    !> squares on an n x n grid (the cells' centers resolving the saddles).
    function zero_velocity_curves(c, extent, n) result(segments)
        real(wp), intent(in) :: c, extent
        integer, intent(in) :: n
        real(wp), allocatable :: segments(:,:)
        real(wp) :: f(0:n, 0:n), h, x, y, e(2, 4), corner(4), center
        logical :: cut(4)
        integer :: i, j, k, m, a, b
        h = 2.0_wp * extent / n
        do j = 0, n
            do i = 0, n
                f(i, j) = two_u(-extent + i * h, -extent + j * h, 0.0_wp) - c
            end do
        end do
        allocate(segments(3, 4 * n * n / 8 + 16))
        m = 0
        do j = 0, n - 1
            y = -extent + j * h
            do i = 0, n - 1
                x = -extent + i * h
                corner = [f(i, j), f(i + 1, j), f(i + 1, j + 1), f(i, j + 1)]     ! counterclockwise
                do k = 1, 4                                 ! edge k from corner k to the next
                    a = k
                    b = modulo(k, 4) + 1
                    cut(k) = (corner(a) > 0.0_wp) .neqv. (corner(b) > 0.0_wp)
                    if (cut(k)) e(:, k) = lerp(a, b)
                end do
                select case (count(cut))
                case (2)
                    call keep(pack([1, 2, 3, 4], cut))
                case (4)                                    ! a saddle: the center's sign joins two corners
                    center = two_u(x + 0.5_wp * h, y + 0.5_wp * h, 0.0_wp) - c
                    if ((center > 0.0_wp) .eqv. (corner(1) > 0.0_wp)) then
                        call keep([1, 2])                   ! corners 2 and 4 cut off
                        call keep([3, 4])
                    else
                        call keep([4, 1])                   ! corners 1 and 3 cut off
                        call keep([2, 3])
                    end if
                end select
            end do
        end do
        segments = segments(:, 1:m)

    contains

        !> The crossing on the edge from corner p to q (linear).
        function lerp(p, q) result(pt)
            integer, intent(in) :: p, q
            real(wp) :: pt(2), xy(2, 4)
            xy = reshape([x, y, x + h, y, x + h, y + h, x, y + h], [2, 4])
            pt = xy(:, p) + corner(p) / (corner(p) - corner(q)) * (xy(:, q) - xy(:, p))
        end function lerp

        subroutine keep(edges)
            integer, intent(in) :: edges(2)
            real(wp), allocatable :: tmp(:,:)
            if (m + 2 > size(segments, 2)) then
                allocate(tmp(3, 2 * size(segments, 2)))
                tmp(:, 1:m) = segments(:, 1:m)
                call move_alloc(tmp, segments)
            end if
            segments(:, m + 1) = [e(:, edges(1)), 0.0_wp]
            segments(:, m + 2) = [e(:, edges(2)), 0.0_wp]
            m = m + 2
        end subroutine keep

    end function zero_velocity_curves

    !> The state and its state transition matrix Phi (s(7:42), by columns):
    !> d Phi / dt = A Phi.
    pure function eom_stm(s) result(ds)
        real(wp), intent(in) :: s(:)
        real(wp) :: ds(size(s))
        real(wp) :: a(6, 6), r1, r2, p1, p2, dx1, dx2
        ds(1:6) = eom(s(1:6))
        associate (y => s(2), z => s(3))
            dx1 = s(1) + mu
            dx2 = s(1) - 1.0_wp + mu
            r1 = norm2([dx1, y, z])
            r2 = norm2([dx2, y, z])
            p1 = 3.0_wp * (1.0_wp - mu) / r1**5
            p2 = 3.0_wp * mu / r2**5
            a = 0.0_wp
            a(1, 4) = 1.0_wp
            a(2, 5) = 1.0_wp
            a(3, 6) = 1.0_wp
            a(4, 1) = 1.0_wp - (1.0_wp - mu) / r1**3 - mu / r2**3 + p1 * dx1**2 + p2 * dx2**2
            a(5, 2) = 1.0_wp - (1.0_wp - mu) / r1**3 - mu / r2**3 + (p1 + p2) * y**2
            a(6, 3) = -(1.0_wp - mu) / r1**3 - mu / r2**3 + (p1 + p2) * z**2
            a(4, 2) = p1 * dx1 * y + p2 * dx2 * y
            a(4, 3) = p1 * dx1 * z + p2 * dx2 * z
            a(5, 3) = (p1 + p2) * y * z
            a(5, 1) = a(4, 2)
            a(6, 1) = a(4, 3)
            a(6, 2) = a(5, 3)
            a(4, 5) = 2.0_wp
            a(5, 4) = -2.0_wp
        end associate
        ds(7:42) = reshape(matmul(a, reshape(s(7:42), [6, 6])), [36])
    end function eom_stm

    !> Propagate s from time 0 by dt (either sign): Dormand-Prince 5(4),
    !> adaptive steps.
    subroutine propagate(f, s, dt)
        procedure(rhs) :: f
        real(wp), intent(inout) :: s(:)
        real(wp), intent(in) :: dt
        real(wp), parameter :: c2 = 1.0_wp/5, c3 = 3.0_wp/10, c4 = 4.0_wp/5, c5 = 8.0_wp/9
        real(wp), parameter :: a21 = 1.0_wp/5, a31 = 3.0_wp/40, a32 = 9.0_wp/40, a41 = 44.0_wp/45, &
            a42 = -56.0_wp/15, a43 = 32.0_wp/9, a51 = 19372.0_wp/6561, a52 = -25360.0_wp/2187, &
            a53 = 64448.0_wp/6561, a54 = -212.0_wp/729, a61 = 9017.0_wp/3168, a62 = -355.0_wp/33, &
            a63 = 46732.0_wp/5247, a64 = 49.0_wp/176, a65 = -5103.0_wp/18656, &
            b1 = 35.0_wp/384, b3 = 500.0_wp/1113, b4 = 125.0_wp/192, b5 = -2187.0_wp/6784, b6 = 11.0_wp/84, &
            e1 = 71.0_wp/57600, e3 = -71.0_wp/16695, e4 = 71.0_wp/1920, e5 = -17253.0_wp/339200, &
            e6 = 22.0_wp/525, e7 = -1.0_wp/40
        real(wp), dimension(size(s)) :: k1, k2, k3, k4, k5, k6, k7, y, err
        real(wp) :: t, h, sgn, r
        if (dt == 0.0_wp) return
        sgn = sign(1.0_wp, dt)
        t = 0.0_wp
        h = sgn * min(0.01_wp, abs(dt))
        k1 = f(s)
        do while (sgn * (dt - t) > 0.0_wp)
            if (sgn * (t + h - dt) > 0.0_wp) h = dt - t
            k2 = f(s + h * a21 * k1)
            k3 = f(s + h * (a31 * k1 + a32 * k2))
            k4 = f(s + h * (a41 * k1 + a42 * k2 + a43 * k3))
            k5 = f(s + h * (a51 * k1 + a52 * k2 + a53 * k3 + a54 * k4))
            k6 = f(s + h * (a61 * k1 + a62 * k2 + a63 * k3 + a64 * k4 + a65 * k5))
            y = s + h * (b1 * k1 + b3 * k3 + b4 * k4 + b5 * k5 + b6 * k6)
            k7 = f(y)
            err = h * (e1 * k1 + e3 * k3 + e4 * k4 + e5 * k5 + e6 * k6 + e7 * k7)
            r = maxval(abs(err) / (tol * (1.0_wp + abs(s))))
            if (r <= 1.0_wp) then
                t = t + h
                s = y
                k1 = k7
            end if
            h = h * min(5.0_wp, max(0.2_wp, 0.9_wp * r**(-0.2_wp)))
            if (abs(h) < 1.0e-14_wp) h = sgn * 1.0e-14_wp
        end do
    end subroutine propagate

    !> Propagate s (with its STM, if 42 long) to its next crossing of y = 0
    !> after `t_min`; t: the time of it.
    subroutine propagate_to_crossing(f, s, t_min, t)
        procedure(rhs) :: f
        real(wp), intent(inout) :: s(:)
        real(wp), intent(in) :: t_min
        real(wp), intent(out) :: t
        real(wp), parameter :: dt = 0.01_wp
        real(wp) :: prev(size(s)), d(size(s))
        integer :: k
        call propagate(f, s, t_min)
        t = t_min
        do
            prev = s
            call propagate(f, s, dt)
            t = t + dt
            if (sign(1.0_wp, s(2)) /= sign(1.0_wp, prev(2))) exit
            if (t > 20.0_wp .or. any(s /= s)) return       ! (none: a guess gone astray; NaN)
        end do
        do k = 1, 8                                      ! Newton on the time: y = 0
            d = f(s)
            if (abs(s(2)) < 1.0e-14_wp) exit
            t = t - s(2) / d(2)
            call propagate(f, s, -s(2) / d(2))
        end do
    end subroutine propagate_to_crossing

    !> A halo orbit through (x0, 0, z0) perpendicular to the x-z plane: x0 and
    !> vy0 corrected (z0 kept) until the next crossing is perpendicular too.
    !> period: its period; ok: converged.
    subroutine correct_halo(x0, z0, vy0, period, ok)
        real(wp), intent(inout) :: x0, vy0
        real(wp), intent(in) :: z0
        real(wp), intent(out) :: period
        logical, intent(out), optional :: ok
        real(wp) :: s(42), phi(6, 6), acc(6), m(2, 2), rhs_(2), d(2), t, det
        integer :: iter
        logical :: done
        done = .false.
        do iter = 1, 30
            s = initial(x0, z0, vy0)
            call propagate_to_crossing(eom_stm, s, 0.5_wp, t)
            if (any(s /= s) .or. t > 20.0_wp) exit
            done = abs(s(4)) < 1.0e-11_wp .and. abs(s(6)) < 1.0e-11_wp
            if (done) exit
            phi = reshape(s(7:42), [6, 6])
            acc = eom(s(1:6))
            m(:, 1) = [phi(4, 1), phi(6, 1)] - acc([4, 6]) / s(5) * phi(2, 1)
            m(:, 2) = [phi(4, 5), phi(6, 5)] - acc([4, 6]) / s(5) * phi(2, 5)
            rhs_ = -[s(4), s(6)]
            det = m(1, 1) * m(2, 2) - m(1, 2) * m(2, 1)
            d = [m(2, 2) * rhs_(1) - m(1, 2) * rhs_(2), m(1, 1) * rhs_(2) - m(2, 1) * rhs_(1)] / det
            x0 = x0 + d(1)
            vy0 = vy0 + d(2)
        end do
        period = 2.0_wp * t
        if (present(ok)) ok = done
    end subroutine correct_halo

    !> The x of collinear Lagrange point `lib` (1, 2 or 3; rotating frame),
    !> and gamma, its distance from the nearer primary (the Moon for L1 and
    !> L2, the Earth for L3): Newton's method on the quintic.
    subroutine collinear_point(lib, x, gamma)
        integer, intent(in) :: lib
        real(wp), intent(out) :: x, gamma
        real(wp) :: g, f, df
        integer :: k
        g = (mu / 3.0_wp)**(1.0_wp / 3.0_wp)            ! (L1, L2: Hill's radius)
        if (lib == 3) g = 1.0_wp - 7.0_wp / 12.0_wp * mu
        do k = 1, 50
            select case (lib)
            case (1)                                    ! x = 1 - mu - g
                f = (1.0_wp - mu) / (1.0_wp - g)**2 - mu / g**2 - (1.0_wp - mu - g)
                df = 2.0_wp * (1.0_wp - mu) / (1.0_wp - g)**3 + 2.0_wp * mu / g**3 + 1.0_wp
            case (2)                                    ! x = 1 - mu + g
                f = (1.0_wp - mu) / (1.0_wp + g)**2 + mu / g**2 - (1.0_wp - mu + g)
                df = -2.0_wp * (1.0_wp - mu) / (1.0_wp + g)**3 - 2.0_wp * mu / g**3 - 1.0_wp
            case default                                ! x = -mu - g
                f = (1.0_wp - mu) / g**2 + mu / (1.0_wp + g)**2 - (mu + g)
                df = -2.0_wp * (1.0_wp - mu) / g**3 - 2.0_wp * mu / (1.0_wp + g)**3 - 1.0_wp
            end select
            if (abs(f) < 1.0e-15_wp) exit
            g = g - f / df
        end do
        gamma = g
        select case (lib)
        case (1)
            x = 1.0_wp - mu - g
        case (2)
            x = 1.0_wp - mu + g
        case default
            x = -mu - g
        end select
    end subroutine collinear_point

    !> Richardson's third-order approximation of a halo orbit about collinear
    !> point `lib` (1, 2 or 3) of z amplitude `az` (km), family `n` (1:
    !> northern, 3: southern), at tau1 = 0 (its crossing of the x-z plane,
    !> perpendicular to it): rv, the state (rotating frame, normalized), and
    !> period, the approximation's. ok: feasible (false below the smallest
    !> halo's amplitude). Ported from the Fortran Astrodynamics Toolkit's
    !> halo_orbit_module (halo_to_rv; J. Williams), after D. L. Richardson,
    !> "Analytic Construction of Periodic Orbits About the Collinear Points",
    !> Celestial Mechanics 22 (1980).
    subroutine richardson_halo(lib, az, n, rv, period, ok)
        integer, intent(in) :: lib, n
        real(wp), intent(in) :: az
        real(wp), intent(out) :: rv(6), period
        logical, intent(out) :: ok
        real(wp) :: xl, gl, a_z, c2, c3, c4, lambda, k, delta, d1, d2, d3, a21, a23, b21, s1, a22, a24, b22, d21, &
                    s2, a1, a2, l1, l2, az2, term, ax, ax2, w, a31, a32, b31, b32, d31, d32, dn
        rv = 0.0_wp
        period = 0.0_wp
        call collinear_point(lib, xl, gl)
        a_z = az / (lu * gl)                            ! (in units of gamma)
        c2 = c_n(2)
        c3 = c_n(3)
        c4 = c_n(4)
        lambda = sqrt((-c2 + 2.0_wp + sqrt(9.0_wp * c2**2 - 8.0_wp * c2)) / 2.0_wp)
        k = 2.0_wp * lambda / (lambda**2 + 1.0_wp - c2)
        delta = lambda**2 - c2
        d1 = 16.0_wp * lambda**4 + 4.0_wp * lambda**2 * (c2 - 2.0_wp) - 2.0_wp * c2**2 + c2 + 1.0_wp
        d2 = 81.0_wp * lambda**4 + 9.0_wp * lambda**2 * (c2 - 2.0_wp) - 2.0_wp * c2**2 + c2 + 1.0_wp
        d3 = 2.0_wp * lambda * (lambda * (1.0_wp + k**2) - 2.0_wp * k)
        a21 = 3.0_wp * c3 * (k**2 - 2.0_wp) / 4.0_wp / (1.0_wp + 2.0_wp * c2)
        a23 = -3.0_wp * lambda * c3 * (3.0_wp * k**3 * lambda - 6.0_wp * k * (k - lambda) + 4.0_wp) / 4.0_wp / k / d1
        b21 = -3.0_wp * c3 * lambda * (3.0_wp * lambda * k - 4.0_wp) / 2.0_wp / d1
        s1 = ((3.0_wp / 2.0_wp) * c3 * (2.0_wp * a21 * (k**2 - 2.0_wp) - a23 * (k**2 + 2.0_wp) - 2.0_wp * k * b21) - &
              (3.0_wp / 8.0_wp) * c4 * (3.0_wp * k**4 - 8.0_wp * k**2 + 8.0_wp)) / d3
        a22 = 3.0_wp * c3 / 4.0_wp / (1.0_wp + 2.0_wp * c2)
        a24 = -3.0_wp * c3 * lambda * (2.0_wp + 3.0_wp * lambda * k) / 4.0_wp / k / d1
        b22 = 3.0_wp * lambda * c3 / d1
        d21 = -c3 / 2.0_wp / lambda**2
        s2 = ((3.0_wp / 2.0_wp) * c3 * (2.0_wp * a22 * (k**2 - 2.0_wp) + a24 * (k**2 + 2.0_wp) + 2.0_wp * k * b22 + &
              5.0_wp * d21) + (3.0_wp / 8.0_wp) * c4 * (12.0_wp - k**2)) / d3
        a1 = -(3.0_wp / 2.0_wp) * c3 * (2.0_wp * a21 + a23 + 5.0_wp * d21) - (3.0_wp / 8.0_wp) * c4 * (12.0_wp - k**2)
        a2 = (3.0_wp / 2.0_wp) * c3 * (a24 - 2.0_wp * a22) + (9.0_wp / 8.0_wp) * c4
        l1 = 2.0_wp * s1 * lambda**2 + a1
        l2 = 2.0_wp * s2 * lambda**2 + a2
        az2 = a_z**2
        ok = l1 /= 0.0_wp
        if (ok) then
            term = (-delta - l2 * az2) / l1
            ok = term >= 0.0_wp
        end if
        if (.not. ok) return
        ax = sqrt(term)                                 ! (equation 18)
        ax2 = ax**2
        w = 1.0_wp + s1 * ax2 + s2 * az2                ! (the frequency's correction)
        a31 = -9.0_wp * lambda * (c3 * (k * a23 - b21) + k * c4 * (1.0_wp + k**2 / 4.0_wp)) / d2 + &
              (9.0_wp * lambda**2 + 1.0_wp - c2) * (3.0_wp * c3 * (2.0_wp * a23 - k * b21) + &
              c4 * (2.0_wp + 3.0_wp * k**2)) / 2.0_wp / d2
        a32 = -9.0_wp * lambda * (4.0_wp * c3 * (k * a24 - b22) + k * c4) / 4.0_wp / d2 - &
              3.0_wp * (9.0_wp * lambda**2 + 1.0_wp - c2) * (c3 * (k * b22 + d21 - 2.0_wp * a24) - c4) / 2.0_wp / d2
        b31 = (3.0_wp * lambda * (3.0_wp * c3 * (k * b21 - 2.0_wp * a23) - c4 * (2.0_wp + 3.0_wp * k**2)) + &
               (9.0_wp * lambda**2 + 1.0_wp + 2.0_wp * c2) * (12.0_wp * c3 * (k * a23 - b21) + &
               3.0_wp * k * c4 * (4.0_wp + k**2)) / 8.0_wp) / d2
        b32 = (3.0_wp * lambda * (3.0_wp * c3 * (k * b22 + d21 - 2.0_wp * a24) - 3.0_wp * c4) + &
               (9.0_wp * lambda**2 + 1.0_wp + 2.0_wp * c2) * (12.0_wp * c3 * (k * a24 - b22) + 3.0_wp * c4 * k) / &
               8.0_wp) / d2
        d31 = 3.0_wp * (4.0_wp * c3 * a24 + c4) / 64.0_wp / lambda**2
        d32 = 3.0_wp * (4.0_wp * c3 * (a23 - d21) + c4 * (4.0_wp + k**2)) / 64.0_wp / lambda**2
        dn = real(2 - n, wp)                            ! (equation 21)
        period = 2.0_wp * acos(-1.0_wp) / (lambda * w)
        ! equations 20a, 20b, 20c and their derivatives, at tau1 = 0
        rv(1) = a21 * ax2 + a22 * az2 - ax + (a23 * ax2 - a24 * az2) + (a31 * ax**3 - a32 * ax * az2)
        rv(2) = 0.0_wp
        rv(3) = dn * (a_z + d21 * ax * a_z * (1.0_wp - 3.0_wp) + (d32 * a_z * ax2 - d31 * a_z**3))
        rv(4) = 0.0_wp
        rv(5) = k * ax + 2.0_wp * (b21 * ax2 - b22 * az2) + 3.0_wp * (b31 * ax**3 - b32 * ax * az2)
        rv(6) = 0.0_wp
        ! from Richardson's scale, about the point, to the rotating frame's
        rv(1:3) = rv(1:3) * gl
        rv(4:6) = rv(4:6) * gl * lambda * w
        rv(1) = rv(1) + xl

    contains

        !> Richardson's c_n (equations 8a and 8b).
        pure real(wp) function c_n(m)
            integer, intent(in) :: m
            select case (lib)
            case (1)
                c_n = (mu + (-1)**m * (1.0_wp - mu) * (gl / (1.0_wp - gl))**(m + 1)) / gl**3
            case (2)
                c_n = ((-1)**m * (mu + (1.0_wp - mu) * (gl / (1.0_wp + gl))**(m + 1))) / gl**3
            case default
                c_n = (1.0_wp - mu + mu * (gl / (1.0_wp + gl))**(m + 1)) / gl**3
            end select
        end function c_n

    end subroutine richardson_halo

    !> A halo orbit (northern) about collinear point `lib` of z amplitude about
    !> `az` (km): Richardson's approximation, corrected (z0 kept; as the
    !> toolkit's halo_to_rv_diffcorr, here with the state transition
    !> matrix's partials). ok: feasible, and converged.
    subroutine halo_from_amplitude(lib, az, x0, z0, vy0, period, ok)
        integer, intent(in) :: lib
        real(wp), intent(in) :: az
        real(wp), intent(out) :: x0, z0, vy0, period
        logical, intent(out) :: ok
        real(wp) :: rv(6)
        call richardson_halo(lib, az, 1, rv, period, ok)
        x0 = rv(1)
        z0 = rv(3)
        vy0 = rv(5)
        if (.not. ok) return
        call correct_halo(x0, z0, vy0, period, ok)
    end subroutine halo_from_amplitude

    !> A halo orbit as correct_halo's, of Jacobi constant c: x0, z0 and vy0
    !> corrected together (Newton's method, the steps limited), the third
    !> condition C(x0, 0, z0, 0, vy0, 0) = c. ok: converged, still a halo
    !> (z0 > 0; else the inputs are left as they were).
    subroutine correct_halo_jacobi(x0, z0, vy0, c, period, ok)
        real(wp), intent(inout) :: x0, z0, vy0
        real(wp), intent(in) :: c
        real(wp), intent(out) :: period
        logical, intent(out) :: ok
        real(wp) :: s(42), phi(6, 6), acc(6), m(3, 3), f(3), d(3), t, r1, r2, p(3)
        integer :: iter, j
        integer, parameter :: params(3) = [1, 3, 5]          ! x0, z0, vy0
        p = [x0, z0, vy0]
        ok = .false.
        period = 0.0_wp
        do iter = 1, 25
            s = initial(p(1), p(2), p(3))
            call propagate_to_crossing(eom_stm, s, 0.5_wp, t)
            f = [s(4), s(6), jacobi([p(1), 0.0_wp, p(2), 0.0_wp, p(3), 0.0_wp]) - c]
            if (all(abs(f) < 1.0e-11_wp)) then
                ok = p(2) > 1.0e-3_wp
                exit
            end if
            phi = reshape(s(7:42), [6, 6])
            acc = eom(s(1:6))
            do j = 1, 3
                m(1:2, j) = [phi(4, params(j)), phi(6, params(j))] - acc([4, 6]) / s(5) * phi(2, params(j))
            end do
            r1 = norm2([p(1) + mu, 0.0_wp, p(2)])
            r2 = norm2([p(1) - 1.0_wp + mu, 0.0_wp, p(2)])
            m(3, :) = [2.0_wp * p(1) - 2.0_wp * (1.0_wp - mu) * (p(1) + mu) / r1**3 - 2.0_wp * mu * (p(1) - 1.0_wp + mu) / r2**3, &
                       -2.0_wp * (1.0_wp - mu) * p(2) / r1**3 - 2.0_wp * mu * p(2) / r2**3, -2.0_wp * p(3)]
            d = -solve(m, f)
            if (norm2(d) > 0.01_wp) d = d * 0.01_wp / norm2(d)
            p = p + d
        end do
        if (.not. ok) return
        x0 = p(1)
        z0 = p(2)
        vy0 = p(3)
        period = 2.0_wp * t
    end subroutine correct_halo_jacobi

    !> From a halo orbit (x0, z0, vy0) to the one of Jacobi constant c, by
    !> continuation (steps in c of at most 0.002, each corrected). ok: all
    !> the way (else the last found is kept: c_reached).
    subroutine continue_halo(x0, z0, vy0, c, period, c_reached, ok)
        real(wp), intent(inout) :: x0, z0, vy0
        real(wp), intent(in) :: c
        real(wp), intent(inout) :: period, c_reached
        logical, intent(out) :: ok
        real(wp) :: c0, target, p
        integer :: n, k
        c0 = jacobi([x0, 0.0_wp, z0, 0.0_wp, vy0, 0.0_wp])
        n = max(1, ceiling(abs(c - c0) / 0.002_wp))
        c_reached = c0
        do k = 1, n
            target = c0 + (c - c0) * k / n
            call correct_halo_jacobi(x0, z0, vy0, target, p, ok)
            if (.not. ok) return
            period = p
            c_reached = target
        end do
    end subroutine continue_halo

    !> The state at (x0, 0, z0) moving at vy0, with the identity STM.
    pure function initial(x0, z0, vy0) result(s)
        real(wp), intent(in) :: x0, z0, vy0
        real(wp) :: s(42)
        integer :: i
        s = 0.0_wp
        s(1:6) = [x0, 0.0_wp, z0, 0.0_wp, vy0, 0.0_wp]
        do i = 1, 6
            s(6 + 6 * (i - 1) + i) = 1.0_wp
        end do
    end function initial

    !> The eigenvalues of a monodromy matrix m (a symplectic matrix's: in
    !> reciprocal pairs, with a pair of 1s), from the traces of m and m^2.
    !> Ported from the Fortran Astrodynamics Toolkit's halo_orbit_module
    !> (compute_monodromy_matrix_eigenvalues; J. Williams), after J. S.
    !> Parker and R. L. Anderson, "Low-Energy Lunar Trajectory Design" (2014).
    pure function monodromy_eigenvalues(m) result(lambda)
        real(wp), intent(in) :: m(6, 6)
        complex(wp) :: lambda(6)
        real(wp) :: alpha, beta
        complex(wp) :: p, q, a, b, c
        integer :: i
        alpha = 2.0_wp
        do i = 1, 6
            alpha = alpha - m(i, i)
        end do
        beta = alpha**2 / 2.0_wp + 1.0_wp
        block
            real(wp) :: m2(6, 6)
            m2 = matmul(m, m)
            do i = 1, 6
                beta = beta - m2(i, i) / 2.0_wp
            end do
        end block
        a = sqrt(cmplx(alpha**2 - 4.0_wp * beta + 8.0_wp, 0.0_wp, wp))
        p = (alpha + a) / 2.0_wp
        q = (alpha - a) / 2.0_wp
        b = sqrt(p * p - 4.0_wp)
        c = sqrt(q * q - 4.0_wp)
        lambda = [(-p + b) / 2.0_wp, (-p - b) / 2.0_wp, (-q + c) / 2.0_wp, (-q - c) / 2.0_wp, &
                  (1.0_wp, 0.0_wp), (1.0_wp, 0.0_wp)]
    end function monodromy_eigenvalues

    !> The unstable and stable eigenvectors of the monodromy matrix m, and
    !> the unstable eigenvalue (the largest real one; 1 if none is above 1:
    !> a stable orbit): inverse iteration at each eigenvalue (shifted a hair,
    !> so m - shift is invertible), robust also for weakly unstable orbits
    !> (power iteration would mix in the eigenvalues on the unit circle).
    subroutine monodromy_vectors(m, unstable, stable, lambda_u)
        real(wp), intent(in) :: m(6, 6)
        real(wp), intent(out) :: unstable(6), stable(6), lambda_u
        complex(wp) :: lambda(6)
        real(wp) :: a(6, 6), shift
        integer :: i, k
        lambda = monodromy_eigenvalues(m)
        lambda_u = 1.0_wp
        do i = 1, 6
            if (abs(aimag(lambda(i))) < 1.0e-6_wp * abs(lambda(i))) lambda_u = max(lambda_u, abs(real(lambda(i), wp)))
        end do
        unstable = 1.0_wp
        stable = 1.0_wp
        shift = lambda_u * (1.0_wp + 1.0e-9_wp)
        a = m
        do i = 1, 6
            a(i, i) = a(i, i) - shift
        end do
        do k = 1, 4
            unstable = solve(a, unstable)
            unstable = unstable / norm2(unstable)
        end do
        shift = (1.0_wp / lambda_u) * (1.0_wp + 1.0e-9_wp)
        a = m
        do i = 1, 6
            a(i, i) = a(i, i) - shift
        end do
        do k = 1, 4
            stable = solve(a, stable)
            stable = stable / norm2(stable)
        end do
    end subroutine monodromy_vectors

    !> x solving a x = b (n x n; Gaussian elimination, partial pivoting).
    pure function solve(a, b) result(x)
        real(wp), intent(in) :: a(:,:), b(:)
        real(wp) :: x(size(b)), m(size(b), size(b) + 1), row(size(b) + 1)
        integer :: i, j, p, n
        n = size(b)
        m(:, 1:n) = a
        m(:, n + 1) = b
        do i = 1, n
            p = i - 1 + maxloc(abs(m(i:n, i)), dim=1)
            row = m(i, :)
            m(i, :) = m(p, :)
            m(p, :) = row
            do j = i + 1, n
                m(j, :) = m(j, :) - m(j, i) / m(i, i) * m(i, :)
            end do
        end do
        do i = n, 1, -1
            x(i) = (m(i, n + 1) - dot_product(m(i, i+1:n), x(i+1:n))) / m(i, i)
        end do
    end function solve

end module halo_cr3bp
