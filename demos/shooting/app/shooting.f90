!> A multiple-shooting solver for an Earth-to-Mars transfer, recording each
!> iteration so it can be played back: the kind of iteration a spacecraft
!> trajectory optimization program goes through as it makes a trajectory of
!> separately propagated segments continuous.
!>
!> The problem: heliocentric two-body motion (AU, days), the Earth on a
!> circular orbit of 1 AU in the ecliptic, Mars on a circular orbit of
!> 1.524 AU inclined 1.85 degrees; the spacecraft leaves the Earth at t = 0
!> and reaches Mars at t = tof. The trajectory is n segments of equal
!> duration, each propagated (fixed-step RK4, with its state transition
!> matrix) from its own initial state, its node. The unknowns: the departure
!> velocity (its position is the Earth's) and the other nodes' states, 6 n
!> - 3 of them; the constraints, as many: each segment's end state equal to
!> the next node's (the gaps), and the last segment's end position at Mars.
!>
!> Newton's method on the constraints, the Jacobian from the state
!> transition matrices: a step fraction `step` of the Newton step while the
!> largest position gap is above 1e-3 AU (damped, as a program does far
!> from a solution), full steps after (converging quadratically). The
!> initial guess: nodes with the radius and longitude interpolated from the
!> Earth's to Mars's, at the circular speed, with random noise of size
!> `noise` (0 to 1: up to 0.1 AU and 30% of the speed) from a seed.
module shooting

    use vkf_math, only: wp, pi

    implicit none

    private

    real(wp), parameter, public :: mu_sun = 2.9591220828559115e-4_wp    !! AU^3/day^2
    real(wp), parameter, public :: au_km = 1.495978707e8_wp             !! km per AU
    real(wp), parameter, public :: day_s = 86400.0_wp                   !! s per day
    real(wp), parameter, public :: r_mars = 1.5237_wp                   !! AU
    real(wp), parameter, public :: tof = 230.0_wp                       !! days
    integer, parameter, public :: max_segments = 16, max_iterations = 80
    integer, parameter, public :: n_steps = 240                         !! RK4 steps per segment
    integer, parameter, public :: n_samples = n_steps / 2 + 1           !! points drawn per segment
    real(wp), parameter :: deg = pi / 180.0_wp
    real(wp), parameter :: inc_mars = 1.85_wp * deg, node_mars = 49.6_wp * deg
    real(wp), parameter :: lon_arrival = 160.0_wp * deg                 !! Mars's longitude at arrival (the transfer angle)
    real(wp), parameter :: full_step_gap = 1.0e-3_wp                    !! AU: full Newton steps below it
    real(wp), parameter :: tol_position = 1.0e-9_wp, tol_velocity = 1.0e-9_wp   !! AU, AU/day

    !> One iteration of the solver: its nodes, the segments propagated from
    !> them, the gaps, the departure and arrival speeds.
    type, public :: Iteration
        integer :: n = 0                                    !! segments
        real(wp), allocatable :: nodes(:,:)                 !! (6, n): each segment's initial state
        real(wp), allocatable :: arcs(:,:,:)                !! (3, n_samples, n): each segment's positions
        real(wp), allocatable :: ends(:,:)                  !! (6, n): each segment's end state
        real(wp), allocatable :: gap_position(:)            !! (n): to the next node (the last: to Mars), AU
        real(wp), allocatable :: gap_velocity(:)            !! (n): to the next node (the last: 0), AU/day
        real(wp) :: max_position = 0.0_wp, max_velocity = 0.0_wp
        real(wp) :: v_departure = 0.0_wp                    !! relative to the Earth (AU/day)
        real(wp) :: v_arrival = 0.0_wp                      !! relative to Mars (AU/day)
        real(wp) :: step = 0.0_wp                           !! the step fraction that led here (0: the guess)
    end type Iteration

    !> A solve: its iterations (1: the initial guess) and whether it converged.
    type, public :: Solution
        integer :: n = 0                                    !! segments
        type(Iteration), allocatable :: iterations(:)
        integer :: count = 0                                !! iterations recorded
        logical :: converged = .false.
    end type Solution

    public :: solve, earth_state, mars_state, segment_times

contains

    !> The Earth's state (AU, AU/day) at time t (days).
    pure function earth_state(t) result(x)
        real(wp), intent(in) :: t
        real(wp) :: x(6), n, u
        n = sqrt(mu_sun)
        u = n * t
        x = [cos(u), sin(u), 0.0_wp, -n * sin(u), n * cos(u), 0.0_wp]
    end function earth_state

    !> Mars's state (AU, AU/day) at time t (days): at longitude lon_arrival at tof.
    pure function mars_state(t) result(x)
        real(wp), intent(in) :: t
        real(wp) :: x(6), n, u, p(3), v(3), u_arrival
        n = sqrt(mu_sun / r_mars**3)
        u_arrival = lon_arrival - node_mars                 ! (argument of latitude, near the node)
        u = u_arrival + n * (t - tof)
        p = r_mars * [cos(u), sin(u), 0.0_wp]
        v = r_mars * n * [-sin(u), cos(u), 0.0_wp]
        x(1:3) = orbit_to_ecliptic(p)
        x(4:6) = orbit_to_ecliptic(v)
    end function mars_state

    !> Mars's orbit plane to the ecliptic: inclined by inc_mars about its node.
    pure function orbit_to_ecliptic(p) result(q)
        real(wp), intent(in) :: p(3)
        real(wp) :: q(3), y, z
        y = p(2) * cos(inc_mars) - p(3) * sin(inc_mars)
        z = p(2) * sin(inc_mars) + p(3) * cos(inc_mars)
        q = [p(1) * cos(node_mars) - y * sin(node_mars), p(1) * sin(node_mars) + y * cos(node_mars), z]
    end function orbit_to_ecliptic

    !> Segment k's start and end times (days) of n.
    pure subroutine segment_times(n, k, t0, t1)
        integer, intent(in) :: n, k
        real(wp), intent(out) :: t0, t1
        t0 = tof * real(k - 1, wp) / real(n, wp)
        t1 = tof * real(k, wp) / real(n, wp)
    end subroutine segment_times

    !> Solve with n segments (2 to max_segments), step fraction `step` (0 to
    !> 1) while far from a solution, an initial guess perturbed by `noise`
    !> (0 to 1) from `seed`; every iteration recorded.
    subroutine solve(n, step, noise, seed, sol)
        integer, intent(in) :: n, seed
        real(wp), intent(in) :: step, noise
        type(Solution), intent(out) :: sol
        real(wp), allocatable :: nodes(:,:), stm(:,:,:), jac(:,:), c(:), dz(:)
        real(wp) :: alpha
        integer :: m, it
        logical :: ok
        sol%n = max(2, min(max_segments, n))
        m = 6 * sol%n - 3
        allocate(sol%iterations(max_iterations + 1))
        allocate(nodes(6, sol%n), stm(6, 6, sol%n), jac(m, m), c(m), dz(m))
        call initial_guess(sol%n, noise, seed, nodes)
        alpha = 0.0_wp
        do it = 1, max_iterations + 1
            call evaluate(sol%n, nodes, sol%iterations(it), stm)
            sol%iterations(it)%step = alpha
            sol%count = it
            if (.not. (sol%iterations(it)%max_position < huge(1.0_wp))) exit          ! (diverged)
            if (sol%iterations(it)%max_position < tol_position .and. &
                sol%iterations(it)%max_velocity < tol_velocity) then
                sol%converged = .true.
                exit
            end if
            if (it > max_iterations) exit
            call constraints(sol%n, nodes, sol%iterations(it), stm, c, jac)
            call linear_solve(jac, -c, dz, ok)
            if (.not. ok) exit
            alpha = merge(1.0_wp, max(0.01_wp, min(1.0_wp, step)), sol%iterations(it)%max_position < full_step_gap)
            nodes(4:6, 1) = nodes(4:6, 1) + alpha * dz(1:3)
            nodes(:, 2:sol%n) = nodes(:, 2:sol%n) + alpha * reshape(dz(4:m), [6, sol%n - 1])
        end do
    end subroutine solve

    !> Nodes between the Earth's departure state and Mars's arrival position:
    !> radius and longitude interpolated, z too, at the circular speed along
    !> the longitude; noise from the seed; the first at the Earth.
    subroutine initial_guess(n, noise, seed, nodes)
        integer, intent(in) :: n, seed
        real(wp), intent(in) :: noise
        real(wp), intent(out) :: nodes(:,:)
        real(wp) :: f, r, lon, z, v, x_mars(6), e(6), rnd(6)
        integer :: k, s
        integer, allocatable :: seeds(:)
        call random_seed(size=s)
        allocate(seeds(s))
        seeds = seed + 37 * [(k, k = 1, s)]
        call random_seed(put=seeds)
        x_mars = mars_state(tof)
        e = earth_state(0.0_wp)
        do k = 1, n
            f = real(k - 1, wp) / real(n, wp)
            r = 1.0_wp + f * (norm2(x_mars(1:3)) - 1.0_wp)
            lon = f * lon_arrival
            z = f * x_mars(3)
            v = sqrt(mu_sun / r)
            nodes(:, k) = [r * cos(lon), r * sin(lon), z, -v * sin(lon), v * cos(lon), 0.0_wp]
            call random_number(rnd)
            rnd = 2.0_wp * rnd - 1.0_wp
            nodes(1:3, k) = nodes(1:3, k) + noise * 0.1_wp * rnd(1:3)
            nodes(4:6, k) = nodes(4:6, k) + noise * 0.3_wp * v * rnd(4:6)
        end do
        nodes(1:3, 1) = e(1:3)                              ! (the departure position: fixed)
    end subroutine initial_guess

    !> Propagate each segment from its node: positions, end states, state
    !> transition matrices; the gaps and speeds.
    subroutine evaluate(n, nodes, iter, stm)
        integer, intent(in) :: n
        real(wp), intent(in) :: nodes(:,:)
        type(Iteration), intent(out) :: iter
        real(wp), intent(out) :: stm(:,:,:)
        real(wp) :: t0, t1, x_mars(6), e(6)
        integer :: k
        iter%n = n
        iter%nodes = nodes
        allocate(iter%arcs(3, n_samples, n), iter%ends(6, n), iter%gap_position(n), iter%gap_velocity(n))
        do k = 1, n
            call segment_times(n, k, t0, t1)
            call propagate(nodes(:, k), t1 - t0, iter%arcs(:, :, k), iter%ends(:, k), stm(:, :, k))
        end do
        do k = 1, n - 1
            iter%gap_position(k) = norm2(iter%ends(1:3, k) - nodes(1:3, k + 1))
            iter%gap_velocity(k) = norm2(iter%ends(4:6, k) - nodes(4:6, k + 1))
        end do
        x_mars = mars_state(tof)
        iter%gap_position(n) = norm2(iter%ends(1:3, n) - x_mars(1:3))
        iter%gap_velocity(n) = 0.0_wp
        iter%max_position = maxval(iter%gap_position)
        iter%max_velocity = maxval(iter%gap_velocity)
        e = earth_state(0.0_wp)
        iter%v_departure = norm2(nodes(4:6, 1) - e(4:6))
        iter%v_arrival = norm2(iter%ends(4:6, n) - x_mars(4:6))
    end subroutine evaluate

    !> The constraints (the gaps, and the miss at Mars) and their Jacobian in
    !> the unknowns [v_1, x_2, ..., x_n].
    subroutine constraints(n, nodes, iter, stm, c, jac)
        integer, intent(in) :: n
        real(wp), intent(in) :: nodes(:,:), stm(:,:,:)
        type(Iteration), intent(in) :: iter
        real(wp), intent(out) :: c(:), jac(:,:)
        real(wp) :: x_mars(6)
        integer :: k, row, col
        integer :: i
        jac = 0.0_wp
        do k = 1, n - 1
            row = 6 * (k - 1)
            c(row + 1:row + 6) = iter%ends(:, k) - nodes(:, k + 1)
            if (k == 1) then
                jac(row + 1:row + 6, 1:3) = stm(:, 4:6, 1)              ! (only the departure velocity is free)
            else
                col = 3 + 6 * (k - 2)
                jac(row + 1:row + 6, col + 1:col + 6) = stm(:, :, k)
            end if
            col = 3 + 6 * (k - 1)
            do i = 1, 6
                jac(row + i, col + i) = -1.0_wp
            end do
        end do
        row = 6 * (n - 1)
        x_mars = mars_state(tof)
        c(row + 1:row + 3) = iter%ends(1:3, n) - x_mars(1:3)
        col = 3 + 6 * (n - 2)
        jac(row + 1:row + 3, col + 1:col + 6) = stm(1:3, :, n)
    end subroutine constraints

    !> Propagate state x0 for dt days (RK4, n_steps steps) with its state
    !> transition matrix: the positions every other step, the end state.
    subroutine propagate(x0, dt, arc, x1, phi)
        real(wp), intent(in) :: x0(6), dt
        real(wp), intent(out) :: arc(:,:), x1(6), phi(6,6)
        real(wp) :: y(42), k1(42), k2(42), k3(42), k4(42), h
        integer :: i, j
        h = dt / n_steps
        y(1:6) = x0
        y(7:42) = 0.0_wp
        do i = 1, 6
            y(6 + 6 * (i - 1) + i) = 1.0_wp                 ! (the identity, column-major)
        end do
        arc(:, 1) = x0(1:3)
        j = 1
        do i = 1, n_steps
            k1 = derivatives(y)
            k2 = derivatives(y + 0.5_wp * h * k1)
            k3 = derivatives(y + 0.5_wp * h * k2)
            k4 = derivatives(y + h * k3)
            y = y + h / 6.0_wp * (k1 + 2.0_wp * k2 + 2.0_wp * k3 + k4)
            if (mod(i, 2) == 0) then
                j = j + 1
                arc(:, j) = y(1:3)
            end if
        end do
        x1 = y(1:6)
        phi = reshape(y(7:42), [6, 6])
    end subroutine propagate

    !> Two-body motion with its state transition matrix: d/dt [x, Phi] = [f(x), A Phi].
    pure function derivatives(y) result(dy)
        real(wp), intent(in) :: y(42)
        real(wp) :: dy(42), r(3), rn, g(3,3), phi(6,6), a(6,6)
        integer :: i
        r = y(1:3)
        rn = norm2(r)
        dy(1:3) = y(4:6)
        dy(4:6) = -mu_sun * r / rn**3
        g = 0.0_wp
        do i = 1, 3
            g(:, i) = 3.0_wp * r * r(i) / rn**2
            g(i, i) = g(i, i) - 1.0_wp
        end do
        g = mu_sun / rn**3 * g
        a = 0.0_wp
        do i = 1, 3
            a(i, 3 + i) = 1.0_wp
        end do
        a(4:6, 1:3) = g
        phi = reshape(y(7:42), [6, 6])
        dy(7:42) = reshape(matmul(a, phi), [36])
    end function derivatives

    !> Solve a x = b (Gaussian elimination, partial pivoting); ok false if singular.
    subroutine linear_solve(a, b, x, ok)
        real(wp), intent(in) :: a(:,:), b(:)
        real(wp), intent(out) :: x(:)
        logical, intent(out) :: ok
        real(wp), allocatable :: m(:,:), row(:)
        real(wp) :: f
        integer :: n, i, j, p
        n = size(b)
        m = a
        x = b
        ok = .false.
        do i = 1, n
            p = maxloc(abs(m(i:n, i)), dim=1) + i - 1
            if (abs(m(p, i)) <= tiny(1.0_wp)) return
            if (p /= i) then
                row = m(i, :)
                m(i, :) = m(p, :)
                m(p, :) = row
                f = x(i)
                x(i) = x(p)
                x(p) = f
            end if
            do j = i + 1, n
                f = m(j, i) / m(i, i)
                m(j, i:n) = m(j, i:n) - f * m(i, i:n)
                x(j) = x(j) - f * x(i)
            end do
        end do
        do i = n, 1, -1
            x(i) = (x(i) - dot_product(m(i, i + 1:n), x(i + 1:n))) / m(i, i)
        end do
        ok = all(abs(x) < huge(1.0_wp))
    end subroutine linear_solve

end module shooting
