!> The invariant manifolds of a halo orbit in the Earth-Moon circular
!> restricted three-body problem (CR3BP), drawn as Polyhedron surfaces: the
!> tubes of trajectories on the Earth's side that leave the orbit (unstable,
!> orange) and that arrive at it (stable, cyan), in the rotating frame (the
!> Earth and Moon fixed on the x axis); and the zero-velocity curves at the
!> orbit's Jacobi constant (in the x-y plane: the edges of the region the
!> orbit's energy can't reach, open at L1), a Polyhedron of edges only.
!>
!> The halo orbit (an L1 northern halo) is found by differential correction
!> from a published initial guess: from the x-z plane, perpendicular to it,
!> half a period to its next crossing, the corrections to x and vy making
!> that crossing perpendicular too. Its monodromy matrix (the state
!> transition matrix over a period) has a large real eigenvalue and its
!> inverse; their eigenvectors (by power iteration), carried along the
!> orbit by the state transition matrix, give the directions of the
!> manifolds at each of 72 points. Each tube: those points displaced 50 km
!> along them toward the Earth (- toward the Moon), propagated 4.5 time
!> units (about 20 days) forward (unstable) or backward (stable), sampled at
!> common times: a grid of points (around the orbit by along the time), a
!> Polyhedron (set_grid), closed around. Trajectories that reach the Earth
!> or the Moon stop there.
!>
!> The surfaces: lit, smooth, translucent, a color per vertex fading out
!> along the time (set_vertex_colors), with every 6th trajectory also drawn
!> as a line (a CurveArtist, pickable). Hover over one: it's drawn thicker;
!> click it: it pulses, with arrows moving along it in its direction of
!> motion (set_highlight), a readout at the point clicked (the time there,
!> from leaving the orbit or before arriving at it, and the distance from
!> the Earth), and its details at the bottom left (its place around the
!> orbit, its closest approach to the Earth, or its impact, and its Jacobi
!> constant with its change along it: the integration's error).
!>
!> The Jacobi constant: a slider in a GUI panel (top right; Dear ImGui,
!> WindowProxy%gui) picks another orbit of the family (3.03 to 3.17, from
!> large halos to small ones near their bifurcation from the planar
!> Lyapunov orbits): it's found by continuation from the one shown (steps
!> in C of at most 0.002, each corrected with C held: x0, z0 and vy0
!> together), then its manifolds and zero-velocity curves are computed and
!> drawn again (the latest value asked for, once the last is done; the
!> panel says how long it took). The manifolds' trajectories are propagated
!> in parallel with OpenMP when built with it: `FPM_FFLAGS="-O3 -fopenmp"
!> FPM_LDFLAGS=-fopenmp pixi run example halo_manifolds` (about 0.04 s on one
!> thread, 0.004 s on ten, on an Apple M-series).
!>
!> The zero-velocity curves: 2 U(x, y, 0) = C (U the pseudo-potential, C
!> the Jacobi constant), traced by marching squares on a 600 x 600 grid of
!> the plane, as segments.
!>
!> Keys: 1 and 2 each tube shown or hidden, z the zero-velocity curves, e
!> the trajectory lines on or off, l lit or unlit, s smooth or flat, c
!> colors fading or not, + / - more or less opaque, space resets the view.
!>
!> Arguments (optional): number of frames to run, then a file to capture the
!> last frame to; with `--demo-pick`, a trajectory is picked at the start
!> (for a capture), and with `--jacobi=C` the orbit of Jacobi constant C at
!> the start: `pixi run example halo_manifolds -- 60 halo.png --demo-pick`
module halo_cr3bp

    use vkframes, only: wp

    implicit none

    private

    public :: eom, eom_stm, propagate, propagate_to_crossing, correct_halo, correct_halo_jacobi, continue_halo, &
              monodromy_vectors, jacobi, zero_velocity_curves

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
    !> period: its period.
    subroutine correct_halo(x0, z0, vy0, period)
        real(wp), intent(inout) :: x0, vy0
        real(wp), intent(in) :: z0
        real(wp), intent(out) :: period
        real(wp) :: s(42), phi(6, 6), acc(6), m(2, 2), rhs_(2), d(2), t, det
        integer :: iter
        do iter = 1, 30
            s = initial(x0, z0, vy0)
            call propagate_to_crossing(eom_stm, s, 0.5_wp, t)
            if (abs(s(4)) < 1.0e-11_wp .and. abs(s(6)) < 1.0e-11_wp) exit
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
    end subroutine correct_halo

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

    !> The unstable and stable eigenvectors of the monodromy matrix m (power
    !> iteration on m and its inverse: their eigenvalues dominate by far).
    subroutine monodromy_vectors(m, unstable, stable)
        real(wp), intent(in) :: m(6, 6)
        real(wp), intent(out) :: unstable(6), stable(6)
        integer :: k
        unstable = 1.0_wp
        stable = 1.0_wp
        do k = 1, 60
            unstable = matmul(m, unstable)
            unstable = unstable / norm2(unstable)
            stable = solve(m, stable)
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

module halo_manifolds_keys

    use vkframes

    implicit none

    private

    integer, parameter, public :: n_tubes = 2
    integer, parameter, public :: n_lines = 12          !! trajectories drawn as lines (pickable), per tube

    !> What the readout says about a trajectory drawn as a line.
    type, public :: LineInfo
        integer :: index = 0                            !! its number around the orbit (1 to n_around)
        integer :: n_around = 1
        real(wp) :: closest = 0.0_wp                    !! its closest approach to the Earth's center (km)
        real(wp) :: closest_time = 0.0_wp               !! then (days)
        real(wp) :: jacobi = 0.0_wp                     !! its Jacobi constant at the orbit
        real(wp) :: drift = 0.0_wp                      !! its change along the trajectory (integration error)
        logical :: impact = .false.                     !! it reaches the Earth's surface (at closest_time)
    end type LineInfo

    !> The keys and the mouse (see the program's documentation).
    type, extends(WindowEventHandler), public :: ManifoldKeys
        type(Polyhedron), pointer :: tubes(:) => null()
        type(Polyhedron), pointer :: curves => null()  !! the zero-velocity curves
        type(CurveArtist), pointer :: lines(:,:) => null()          !! (n_lines, n_tubes)
        type(DrawableTrajectory), pointer :: line_frames(:) => null()  !! each tube's lines
        type(LineInfo) :: info(n_lines, n_tubes)
        character(len=24) :: names(n_tubes) = ''
        real(wp) :: line_colors(4, n_tubes) = 1.0_wp
        type(ReferenceFrame), pointer :: selection => null()        !! moved to the point picked; its label: the readout
        type(DrawableTrajectory), pointer :: ring => null()         !! the ring there
        type(HUDText), pointer :: details => null()                 !! the trajectory picked, in detail
        real(wp) :: earth(3) = 0.0_wp, re = 1.0_wp      !! the Earth's center (km) and radius
        real(wp), allocatable :: colors(:,:,:)          !! each tube's vertex colors (4, n, tube)
        logical :: shown(n_tubes) = .true., curves_shown = .true.
        logical :: edges = .true., lit = .true., smooth = .true., fading = .true.
        real(wp) :: opacity = 0.6_wp
        integer :: hovered(2) = 0, selected(2) = 0      !! (line, tube), 0: none
        ! the Jacobi constant's panel: the value asked for (the slider; the
        ! program computes the orbit and its manifolds again when it changes),
        ! and the orbit's
        real(wp) :: c_requested = 0.0_wp, c_min = 3.03_wp, c_max = 3.17_wp
        real(wp) :: c = 0.0_wp, period_days = 0.0_wp, az = 0.0_wp, seconds = 0.0_wp, continued = 0.0_wp
        integer :: threads = 1
        character(len=80) :: status = ''
    contains
        procedure :: key_press => manifold_key
        procedure :: mouse_motion => manifold_motion
        procedure :: button_press => manifold_press
        procedure :: before_draw => manifold_panel
        procedure :: apply
        procedure :: select => select_line
        procedure :: clear_selection
    end type ManifoldKeys

    real(wp), parameter :: line_width = 1.2_wp, hover_width = 3.0_wp, selected_width = 2.5_wp

contains

    !> The settings, on every tube.
    subroutine apply(self)
        class(ManifoldKeys), intent(inout) :: self
        integer :: k
        if (associated(self%curves)) call self%curves%show_contents(self%curves_shown)
        do k = 1, size(self%tubes)
            call self%tubes(k)%show_contents(self%shown(k))
            call self%line_frames(k)%show_contents(self%shown(k) .and. self%edges)
            call self%tubes(k)%set_lit(self%lit)
            call self%tubes(k)%set_smooth(self%smooth)
            call self%tubes(k)%set_opacity(self%opacity)
            if (self%fading) then
                call self%tubes(k)%set_vertex_colors(self%colors(:, :, k))
            else
                call self%tubes(k)%remove_vertex_colors()
            end if
        end do
    end subroutine apply

    subroutine manifold_key(self, window, key, modifiers, handled)
        class(ManifoldKeys), intent(inout) :: self
        class(WindowProxy), intent(inout) :: window
        integer, intent(in) :: key, modifiers
        logical, intent(inout) :: handled
        handled = .true.
        select case (key)
        case (iachar('1'):iachar('0') + n_tubes)
            self%shown(key - iachar('0')) = .not. self%shown(key - iachar('0'))
        case (iachar('z'))
            self%curves_shown = .not. self%curves_shown
        case (iachar('e'))
            self%edges = .not. self%edges
        case (iachar('l'))
            self%lit = .not. self%lit
        case (iachar('s'))
            self%smooth = .not. self%smooth
        case (iachar('c'))
            self%fading = .not. self%fading
        case (iachar('+'), iachar('='))
            self%opacity = min(1.0_wp, self%opacity + 0.1_wp)
        case (iachar('-'))
            self%opacity = max(0.1_wp, self%opacity - 0.1_wp)
        case default
            handled = .false.
            return
        end select
        call self%apply()
    end subroutine manifold_key

    !> Each frame: the Jacobi constant's panel (top right).
    subroutine manifold_panel(self, window, t)
        class(ManifoldKeys), intent(inout) :: self
        class(WindowProxy), intent(inout) :: window
        real(wp), intent(in) :: t
        type(Gui) :: g
        character(len=80) :: text
        real(wp) :: value
        g = window%gui()
        call g%begin_panel('Halo orbit', position=[0.99_wp, 0.92_wp], pivot=[1.0_wp, 0.0_wp])
        value = self%c_requested
        if (g%slider('Jacobi constant', value, self%c_min, self%c_max)) self%c_requested = value
        write(text, '(a,f9.6,a,f6.2,a,i0,a)') 'C = ', self%c, ', period ', self%period_days, ' days, Az ', &
            nint(self%az), ' km'
        call g%text(trim(text))
        write(text, '(a,f6.3,a,f6.3,a,i0,a)') 'orbit continued in ', self%continued, ' s, manifolds in ', &
            self%seconds, ' s (', self%threads, ' threads)'
        call g%text(trim(text))
        if (len_trim(self%status) > 0) call g%text(trim(self%status))
        call g%end_panel()
    end subroutine manifold_panel

    !> No trajectory selected (e.g. computed again).
    subroutine clear_selection(self)
        class(ManifoldKeys), intent(inout) :: self
        if (self%selected(1) > 0) call self%lines(self%selected(1), self%selected(2))%set_highlight(HIGHLIGHT_NONE)
        if (self%hovered(1) > 0) call restyle(self, self%hovered, hovered=.false.)
        self%selected = 0
        self%hovered = 0
        call self%ring%show_contents(.false.)
        call self%selection%show_name_label(.false.)
        call self%details%set_text('click a trajectory line for its details')
    end subroutine clear_selection

    !> Hover: the trajectory under the pointer drawn thicker (not while dragging).
    subroutine manifold_motion(self, window, x, y, buttons)
        class(ManifoldKeys), intent(inout) :: self
        class(WindowProxy), intent(inout) :: window
        real(wp), intent(in) :: x, y
        logical, intent(in) :: buttons(3)
        type(PickHit), allocatable :: hits(:)
        integer :: lk(2)
        if (any(buttons)) return
        call window%pick(x, y, hits, kinds=[PICK_CURVE], nearest=.true.)
        lk = 0
        if (size(hits) > 0) lk = line_of(self, hits(1))
        if (all(lk == self%hovered)) return
        call restyle(self, self%hovered, hovered=.false.)
        call restyle(self, lk, hovered=.true.)
        self%hovered = lk
    end subroutine manifold_motion

    subroutine manifold_press(self, window, x, y, button)
        class(ManifoldKeys), intent(inout) :: self
        class(WindowProxy), intent(inout) :: window
        real(wp), intent(in) :: x, y
        integer, intent(in) :: button
        if (button == MOUSE_LEFT) call self%select(window, x, y)
    end subroutine manifold_press

    !> Select the trajectory at (x, y) (or none): highlighted (pulsing, arrows
    !> in its direction of motion), a readout at the point picked, and its
    !> details in the HUD.
    subroutine select_line(self, window, x, y)
        class(ManifoldKeys), intent(inout) :: self
        class(WindowProxy), intent(inout) :: window
        real(wp), intent(in) :: x, y
        type(PickHit), allocatable :: hits(:)
        character(len=160) :: line1, line2, line3
        character(len=120) :: label
        real(wp) :: at(2), r, days
        integer :: lk(2), previous(2)
        logical :: unstable
        call window%pick(x, y, hits, kinds=[PICK_CURVE], nearest=.true.)
        lk = 0
        if (size(hits) > 0) lk = line_of(self, hits(1))
        previous = self%selected
        self%selected = lk
        if (previous(1) > 0) call self%lines(previous(1), previous(2))%set_highlight(HIGHLIGHT_NONE)
        call restyle(self, previous, hovered=.false.)
        if (lk(1) == 0) then
            call self%ring%show_contents(.false.)
            call self%selection%show_name_label(.false.)
            call self%details%set_text('click a trajectory line for its details')
            return
        end if
        call self%lines(lk(1), lk(2))%set_highlight(ior(HIGHLIGHT_PULSE, HIGHLIGHT_ARROWS), color=[1.0_wp, 1.0_wp, 1.0_wp])
        call restyle(self, lk, hovered=.false.)
        associate (h => hits(1), info => self%info(lk(1), lk(2)))
            unstable = lk(2) == 1
            r = norm2(h%position - self%earth)
            days = abs(h%time)
            ! the readout beside the point (the side with room)
            write(label, '(a,f5.1,a,a,a,i0,a)') 't = ', days, ' d ', trim(merge('after leaving ', 'before arrival', unstable)), &
                ', ', nint(r / 1000.0_wp), ' thousand km from the Earth'
            call self%selection%set_color(self%line_colors(1:3, lk(2)))
            if (window%screen_position(h%position, at)) then
                if (at(1) > 0.6_wp) then
                    call self%selection%set_label_alignment(ALIGN_RIGHT_CENTER)
                    label = trim(label)//'    '
                else
                    call self%selection%set_label_alignment(ALIGN_LEFT_CENTER)
                    label = '    '//trim(label)
                end if
            end if
            call self%selection%set_name(trim(label))
            call self%selection%show_name_label(.true.)
            call self%selection%set_position(h%position)
            call self%ring%show_contents(.true.)
            ! the details
            write(line1, '(a,i0,a,i0,a,f4.2,a)') trim(self%names(lk(2)))//' manifold, trajectory ', info%index, &
                ' of ', info%n_around, ' (orbit phase ', real(info%index - 1, wp) / info%n_around, ')'
            if (info%impact) then
                write(line2, '(a,f5.1,a)') 'reaches the Earth''s surface ', abs(info%closest_time), &
                    ' days '//trim(merge('after leaving the orbit', 'before arriving at it  ', unstable))
            else
                write(line2, '(a,i0,a,f5.1,a)') 'closest to the Earth: ', nint(info%closest - self%re), &
                    ' km altitude, ', abs(info%closest_time), &
                    ' days '//trim(merge('after leaving the orbit', 'before arriving at it  ', unstable))
            end if
            write(line3, '(a,f9.6,a,es8.1,a)') 'Jacobi constant ', info%jacobi, ' (change along it: ', info%drift, ')'
            call self%details%set_text(trim(line1)//new_line('a')//trim(line2)//new_line('a')//trim(line3))
        end associate
    end subroutine select_line

    !> A line's width and color: selected, hovered or neither.
    subroutine restyle(self, lk, hovered)
        type(ManifoldKeys), intent(inout) :: self
        integer, intent(in) :: lk(2)
        logical, intent(in) :: hovered
        if (lk(1) == 0) return
        if (hovered) then
            call self%lines(lk(1), lk(2))%set_width(hover_width)
        else if (all(lk == self%selected)) then
            call self%lines(lk(1), lk(2))%set_width(selected_width)
        else
            call self%lines(lk(1), lk(2))%set_width(line_width)
        end if
    end subroutine restyle

    !> The (line, tube) a hit is on, or 0.
    function line_of(self, hit) result(lk)
        type(ManifoldKeys), intent(in) :: self
        type(PickHit), intent(in) :: hit
        integer :: lk(2), l, k
        type(Trajectory), pointer :: traj
        lk = 0
        if (.not. associated(hit%trajectory)) return
        do k = 1, n_tubes
            do l = 1, n_lines
                traj => self%lines(l, k)%get_trajectory()
                if (associated(hit%trajectory, traj)) then
                    lk = [l, k]
                    return
                end if
            end do
        end do
    end function line_of

end module halo_manifolds_keys

program halo_manifolds

    use vkframes
    use halo_cr3bp
    use halo_manifolds_keys
    !$ use omp_lib, only: omp_get_max_threads

    implicit none

    integer, parameter :: n_around = 72                 !! points around the orbit (the tubes' circumference)
    integer, parameter :: n_along = 240                 !! samples along each trajectory
    integer, parameter :: n_orbit = 4 * n_around        !! points along the orbit drawn
    real(wp), parameter :: span = 4.5_wp                !! each trajectory's time (time units)
    real(wp), parameter :: offset = 50.0_wp / lu        !! the displacement off the orbit (50 km)
    real(wp), parameter :: re = 6378.137_wp, rm = 1737.4_wp     !! km
    character(len=*), parameter :: names(n_tubes) = [character(len=24) :: 'Unstable', 'Stable']
    integer, parameter :: branch(n_tubes) = [1, 2]      !! each tube's eigenvector: 1 unstable, 2 stable
    real(wp), parameter :: side(n_tubes) = [-1.0_wp, -1.0_wp]      !! its side: + the Moon's, - the Earth's
    real(wp), parameter :: direction(n_tubes) = [1.0_wp, -1.0_wp]  !! its time: forward (unstable), backward
    real(wp), parameter :: tube_colors(3, n_tubes) = reshape([1.0_wp, 0.65_wp, 0.15_wp, 0.2_wp, 0.75_wp, 1.0_wp], &
                                                             [3, n_tubes])

    type(ReferenceFrame), target :: rotating, sun
    type(Sphere), target :: earth, moon
    type(Trajectory), target :: halo
    type(CurveArtist), target :: halo_curve
    type(DrawableTrajectory), target :: halo_frame
    type(Polyhedron), target :: tubes(n_tubes), zvc
    type(Trajectory), target :: paths(n_lines, n_tubes)
    type(CurveArtist), target :: lines(n_lines, n_tubes)
    type(DrawableTrajectory), target :: line_frames(n_tubes), ring_frame
    type(MarkerArtist), target :: ring
    type(ReferenceFrame), target :: selection
    type(HUDText), target :: details
    type(FrameManager), target :: scene
    type(WindowProxy), target :: window
    type(View), target :: view_
    type(HUDText), target :: legend
    type(ManifoldKeys), target :: keys
    character(len=1024) :: arg, file
    real(wp) :: x0, z0, vy0, period, c_reached, c_start
    real(wp) :: orbit(6, n_orbit + 1), dirs(6, n_around, 2), times(n_along)
    real(wp) :: grid(3, n_around, n_along, n_tubes)
    integer :: last(n_around, n_tubes)                  !! each trajectory's samples drawn (up to an impact)
    integer :: j, k, l, frames, frame
    integer(8) :: tick, tock, rate
    logical :: running, demo, ok

    frames = huge(frames)
    file = ''
    if (command_argument_count() >= 1) then
        call get_command_argument(1, arg)
        read(arg, *) frames
    end if
    if (command_argument_count() >= 2) call get_command_argument(2, file)
    demo = .false.
    c_start = 0.0_wp
    do j = 3, command_argument_count()
        call get_command_argument(j, arg)
        if (trim(arg) == '--demo-pick') demo = .true.
        if (arg(1:9) == '--jacobi=') read(arg(10:), *) c_start
    end do
    keys%threads = 1
    !$ keys%threads = omp_get_max_threads()
    times = [(span * (j - 1) / (n_along - 1) * tu_days, j = 1, n_along)]

    ! the first halo orbit: from a published initial guess
    x0 = 0.82575887090385_wp
    z0 = 0.08_wp
    vy0 = 0.19369724986446_wp
    call correct_halo(x0, z0, vy0, period)
    write(*, '(a,f12.9,a,f6.3,a,f12.9,a,f9.6,a,f6.2,a)') 'halo orbit: x0 = ', x0, ', z0 = ', z0, ', vy0 = ', vy0, &
        ', period ', period, ' (', period * tu_days, ' days)'

    ! the scene: the rotating frame, the Earth and the Moon on its x axis; the
    ! orbit, the tubes (and their lines) and the zero-velocity curves (filled
    ! in by compute)
    rotating = ReferenceFrame('Rotating frame')
    call rotating%show_decorations(.false.)
    earth = Sphere('Earth', radius=re)
    call earth%set_texture_map('2k_earth_daymap.jpg')
    call earth%set_position([-mu * lu, 0.0_wp, 0.0_wp])
    call earth%show_axes(NO_AXES)
    call earth%show_axes_labels(NO_AXES)
    moon = Sphere('Moon', radius=rm)
    call moon%set_texture_map('lroc_color_2k.jpg')
    call moon%set_position([(1.0_wp - mu) * lu, 0.0_wp, 0.0_wp])
    call moon%show_axes(NO_AXES)
    call moon%show_axes_labels(NO_AXES)
    halo = Trajectory()
    halo_curve = CurveArtist(halo, color=[1.0_wp, 1.0_wp, 1.0_wp], width=2.5_wp)
    halo_frame = DrawableTrajectory('L1 halo orbit')
    call halo_frame%show_decorations(.false.)
    call halo_frame%add_artist(halo_curve)
    call halo_frame%set_pickable(.false.)
    zvc = Polyhedron('Zero-velocity curves', color=[0.85_wp, 0.85_wp, 0.85_wp, 0.8_wp])
    call zvc%set_draw_mode(POLY_EDGES)
    call zvc%set_line_width(1.5_wp)
    call zvc%show_decorations(.false.)
    call zvc%set_pickable(.false.)
    sun = ReferenceFrame('Sun')
    call sun%set_position([-0.4_wp, -1.0_wp, 0.6_wp] * 1.5e8_wp)
    call sun%set_light_source_enabled(.true.)
    call sun%set_light_diffuse([1.0_wp, 1.0_wp, 1.0_wp])
    call sun%set_light_ambient([0.25_wp, 0.25_wp, 0.25_wp])
    call sun%show_decorations(.false.)
    if (.not. (rotating%add_child(earth) .and. rotating%add_child(moon) .and. rotating%add_child(halo_frame) .and. &
               rotating%add_child(sun) .and. rotating%add_child(zvc))) error stop 'add_child failed'
    allocate(keys%colors(4, n_around * n_along, n_tubes))
    do k = 1, n_tubes
        line_frames(k) = DrawableTrajectory(trim(names(k))//' trajectories')
        call line_frames(k)%show_decorations(.false.)
        do l = 1, n_lines
            paths(l, k) = Trajectory()
            lines(l, k) = CurveArtist(paths(l, k), color=[tube_colors(:, k), 0.7_wp], width=1.2_wp)
            call line_frames(k)%add_artist(lines(l, k))
        end do
        tubes(k) = Polyhedron(trim(names(k)), color=tube_colors(:, k))
        call tubes(k)%set_draw_mode(POLY_FACES)
        call tubes(k)%show_decorations(.false.)
        call tubes(k)%set_pickable(.false.)
        keys%names(k) = names(k)
        keys%line_colors(:, k) = [tube_colors(:, k), 1.0_wp]
        if (.not. (rotating%add_child(tubes(k)) .and. rotating%add_child(line_frames(k)))) error stop 'add_child failed'
    end do

    ! the selection: a ring (a marker at the frame's origin) and a readout,
    ! moved to the point picked
    selection = ReferenceFrame('Selection')
    call selection%show_decorations(.false.)
    ring = MarkerArtist(markers=MARKER_START, size=22.0_wp, color=[1.0_wp, 1.0_wp, 1.0_wp])
    call ring%set_marker_shape(MARKER_CIRCLE)
    ring_frame = DrawableTrajectory('Ring')
    call ring_frame%show_decorations(.false.)
    call ring_frame%add_artist(ring)
    call ring_frame%show_contents(.false.)
    call ring_frame%set_pickable(.false.)
    call selection%set_pickable(.false.)
    call selection%set_label_size(15)
    call selection%set_name_label_on_top(.true.)
    if (.not. (selection%add_child(ring_frame) .and. rotating%add_child(selection))) error stop 'add_child failed'

    keys%tubes => tubes
    keys%curves => zvc
    keys%lines => lines
    keys%line_frames => line_frames
    keys%selection => selection
    keys%ring => ring_frame
    keys%details => details
    keys%earth = [-mu * lu, 0.0_wp, 0.0_wp]
    keys%re = re
    call compute()
    keys%c_requested = keys%c
    if (c_start > 0.0_wp) keys%c_requested = c_start   ! (--jacobi=C: computed in the first frame)
    scene = FrameManager(rotating)

    if (.not. window%open(1200, 800, 'vkframes: halo orbit manifolds')) error stop 'could not open a window'
    call window%set_scene(scene)
    call window%set_background_color([0.0_wp, 0.0_wp, 0.02_wp])
    view_ = View(rotating, rotating)
    call view_%set_default_view_parameters(eye=[0.2_wp, -2.0_wp, 1.35_wp] * lu, center=[0.25_wp, 0.0_wp, 0.0_wp] * lu, &
                                           up=[0.0_wp, 0.0_wp, 1.0_wp])
    call window%add_view(view_)
    legend = HUDText('Earth-Moon L1 halo orbit (white), its manifolds on the Earth side:  1 unstable (orange)   ' // &
                     '2 stable (cyan)   z: zero-velocity curves (gray)' // &
                     new_line('a') // 'click a line: its details (arrows: its motion)    e: trajectory lines    ' // &
                     'l: lit / unlit    s: smooth / flat    c: fading colors / plain    + / -: opacity', &
                     position=[0.01_wp, 0.99_wp], alignment=ALIGN_LEFT_TOP, size=14.0_wp, color=[0.75_wp, 0.75_wp, 0.75_wp])
    call window%add_hud_text(legend)
    details = HUDText('click a trajectory line for its details', position=[0.01_wp, 0.01_wp], &
                      alignment=ALIGN_LEFT_BOTTOM, size=15.0_wp, color=[0.9_wp, 0.9_wp, 0.9_wp])
    call window%add_hud_text(details)
    call window%set_event_handler(keys)

    running = .true.
    frame = 0
    do while (running .and. frame < frames)
        running = window%frame()
        frame = frame + 1
        if (demo .and. frame == 2) call demo_pick()
        ! the slider moved: the orbit of that Jacobi constant (continued from
        ! this one), and its manifolds (the latest value asked for, once the
        ! last is done)
        if (abs(keys%c_requested - keys%c) > 1.0e-9_wp) then
            call system_clock(tick, rate)
            call continue_halo(x0, z0, vy0, keys%c_requested, period, c_reached, ok)
            call system_clock(tock)
            keys%continued = real(tock - tick, wp) / rate
            keys%status = ''
            if (.not. ok) then
                write(keys%status, '(a,f9.6)') 'no halo orbit found beyond C = ', c_reached
                keys%c_requested = c_reached
            end if
            call keys%clear_selection()
            call compute()
        end if
    end do
    if (len_trim(file) > 0 .and. running) then
        if (window%capture(trim(file))) write(*, '(a)') 'captured '//trim(file)
    end if
    call window%close()

contains

    !> The orbit (x0, z0, vy0), its manifolds and zero-velocity curves,
    !> computed (the trajectories in parallel) and drawn.
    subroutine compute()
        integer(8) :: t0, t1, rate
        call system_clock(t0, rate)
        call compute_orbit()
        call compute_tubes()
        call system_clock(t1)
        keys%seconds = real(t1 - t0, wp) / rate
        write(*, '(a,f9.6,a,f6.2,a,i0,a,f6.3,a,f6.3,a,i0,a)') 'C = ', keys%c, ': period ', keys%period_days, ' days, Az ', &
            nint(keys%az), ' km; continued in ', keys%continued, ' s, manifolds in ', keys%seconds, ' s (', &
            keys%threads, ' threads)'
        call draw()
    end subroutine compute

    !> The orbit (n_orbit + 1 states over its period) and the manifolds'
    !> directions at n_around points: the monodromy matrix's eigenvectors
    !> carried along it by the state transition matrix, each a unit
    !> position, + toward the Moon at the first point.
    subroutine compute_orbit()
        real(wp) :: s(42), vu(6), vs(6), phi(6, 6)
        integer :: i, k_, sgn
        s = initial_state()
        call propagate(eom_stm, s, period)
        call monodromy_vectors(reshape(s(7:42), [6, 6]), vu, vs)
        s = initial_state()
        do i = 1, n_orbit + 1
            orbit(:, i) = s(1:6)
            if (modulo(i - 1, 4) == 0 .and. i <= n_orbit) then
                phi = reshape(s(7:42), [6, 6])
                dirs(:, (i - 1) / 4 + 1, 1) = matmul(phi, vu)
                dirs(:, (i - 1) / 4 + 1, 2) = matmul(phi, vs)
            end if
            if (i <= n_orbit) call propagate(eom_stm, s, period / n_orbit)
        end do
        do k_ = 1, 2
            sgn = merge(1, -1, dirs(1, 1, k_) > 0.0_wp)
            do i = 1, n_around
                dirs(:, i, k_) = sgn * dirs(:, i, k_) / norm2(dirs(1:3, i, k_))
            end do
        end do
        keys%c = jacobi(orbit(:, 1))
        keys%period_days = period * tu_days
        keys%az = maxval(abs(orbit(3, :))) * lu
    end subroutine compute_orbit

    !> The tubes: each trajectory propagated (in parallel: they're
    !> independent), unstable forward, stable backward, toward the Earth,
    !> sampled at common times (stopping at the Earth or the Moon); the
    !> lines' details.
    subroutine compute_tubes()
        real(wp) :: w(6), r
        integer :: i, j_, k_, l_
        logical :: stopped
        !$omp parallel do collapse(2) schedule(dynamic) private(i, j_, k_, l_, w, r, stopped)
        do k_ = 1, n_tubes
            do i = 1, n_around
                w = orbit(:, 4 * (i - 1) + 1) + side(k_) * offset * dirs(:, i, branch(k_))
                l_ = 0
                if (modulo(i - 1, n_around / n_lines) == 0) l_ = (i - 1) / (n_around / n_lines) + 1
                if (l_ > 0) keys%info(l_, k_) = LineInfo(index=i, n_around=n_around, closest=huge(1.0_wp), jacobi=jacobi(w))
                stopped = .false.
                last(i, k_) = n_along
                do j_ = 1, n_along
                    if (j_ > 1 .and. .not. stopped) then
                        call propagate(eom, w, direction(k_) * span / (n_along - 1))
                        stopped = norm2(w(1:3) - [-mu, 0.0_wp, 0.0_wp]) < re / lu .or. &
                                  norm2(w(1:3) - [1.0_wp - mu, 0.0_wp, 0.0_wp]) < rm / lu
                        if (stopped) last(i, k_) = j_
                        if (l_ > 0 .and. (j_ == n_along .or. stopped)) &
                            keys%info(l_, k_)%drift = jacobi(w) - keys%info(l_, k_)%jacobi
                    end if
                    grid(:, i, j_, k_) = w(1:3) * lu
                    keys%colors(:, i + (j_ - 1) * n_around, k_) = &
                        [tube_colors(:, k_), 1.0_wp - 0.85_wp * real(j_ - 1, wp) / (n_along - 1)]
                    if (l_ > 0 .and. j_ <= last(i, k_)) then
                        r = norm2(w(1:3) - [-mu, 0.0_wp, 0.0_wp]) * lu
                        if (r < keys%info(l_, k_)%closest) then
                            keys%info(l_, k_)%closest = r
                            keys%info(l_, k_)%closest_time = direction(k_) * times(j_)
                        end if
                    end if
                end do
                if (l_ > 0) keys%info(l_, k_)%impact = norm2(w(1:3) - [-mu, 0.0_wp, 0.0_wp]) < re / lu
            end do
        end do
        !$omp end parallel do
    end subroutine compute_tubes

    !> The orbit, the tubes, their lines (points in increasing time: the
    !> stable ones' toward the orbit, so the highlight's arrows show the
    !> motion; times in days) and the zero-velocity curves, drawn again.
    subroutine draw()
        real(wp), allocatable :: segments(:,:)
        integer, allocatable :: pairs(:,:)
        integer :: i, j_, k_, l_
        call halo%clear()
        do i = 1, n_orbit + 1
            call halo%add(period * (i - 1) / n_orbit * tu_days, pos=orbit(1:3, i) * lu)
        end do
        do k_ = 1, n_tubes
            call tubes(k_)%set_grid(grid(:, :, :, k_), closed_u=.true., lines=[0, 0])
            do l_ = 1, n_lines
                i = (l_ - 1) * (n_around / n_lines) + 1
                call paths(l_, k_)%clear()
                if (direction(k_) > 0.0_wp) then
                    do j_ = 1, last(i, k_)
                        call paths(l_, k_)%add(times(j_), pos=grid(:, i, j_, k_))
                    end do
                else
                    do j_ = last(i, k_), 1, -1
                        call paths(l_, k_)%add(-times(j_), pos=grid(:, i, j_, k_))
                    end do
                end if
            end do
        end do
        segments = zero_velocity_curves(keys%c, 1.4_wp, 600) * lu
        pairs = reshape([(i, i = 1, size(segments, 2))], [2, size(segments, 2) / 2])
        call zvc%set_triangles(segments, reshape([integer ::], [3, 0]))
        call zvc%set_edges(pairs)
        call keys%apply()                               ! (the tubes' colors and settings again)
    end subroutine draw

    !> (--demo-pick, for a capture) Select a point 3/5 along the 4th unstable
    !> trajectory, where it is on the screen.
    subroutine demo_pick()
        real(wp) :: at(2), p(3)
        p = paths(4, 1)%position(3 * paths(4, 1)%num_positions() / 5)
        if (window%screen_position(p, at)) call keys%select(window, 2.0_wp * at(1) - 1.0_wp, 2.0_wp * at(2) - 1.0_wp)
    end subroutine demo_pick

    !> The halo orbit's initial state, with the identity STM.
    function initial_state() result(s_)
        real(wp) :: s_(42)
        integer :: c
        s_ = 0.0_wp
        s_(1:6) = [x0, 0.0_wp, z0, 0.0_wp, vy0, 0.0_wp]
        do c = 1, 6
            s_(6 + 6 * (c - 1) + c) = 1.0_wp
        end do
    end function initial_state

end program halo_manifolds
