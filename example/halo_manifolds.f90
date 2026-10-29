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
!> (for a capture): `pixi run example halo_manifolds -- 60 halo.png --demo-pick`
module halo_cr3bp

    use vkframes, only: wp

    implicit none

    private

    public :: eom, eom_stm, propagate, propagate_to_crossing, correct_halo, monodromy_vectors, jacobi, &
              zero_velocity_curves

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

    !> x solving a x = b (Gaussian elimination, partial pivoting).
    pure function solve(a, b) result(x)
        real(wp), intent(in) :: a(6, 6), b(6)
        real(wp) :: x(6), m(6, 7), row(7)
        integer :: i, j, p
        m(:, 1:6) = a
        m(:, 7) = b
        do i = 1, 6
            p = i - 1 + maxloc(abs(m(i:6, i)), dim=1)
            row = m(i, :)
            m(i, :) = m(p, :)
            m(p, :) = row
            do j = i + 1, 6
                m(j, :) = m(j, :) - m(j, i) / m(i, i) * m(i, :)
            end do
        end do
        do i = 6, 1, -1
            x(i) = (m(i, 7) - dot_product(m(i, i+1:6), x(i+1:6))) / m(i, i)
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
    contains
        procedure :: key_press => manifold_key
        procedure :: mouse_motion => manifold_motion
        procedure :: button_press => manifold_press
        procedure :: apply
        procedure :: select => select_line
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

    implicit none

    integer, parameter :: n_around = 72                 !! points around the orbit (the tubes' circumference)
    integer, parameter :: n_along = 240                 !! samples along each trajectory
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
    real(wp) :: x0, z0, vy0, period, s(42), vu(6), vs(6), phi(6, 6), dir(6), w(6), dt, fade
    real(wp), allocatable :: grid(:,:,:,:), orbit(:,:), dirs(:,:,:), segments(:,:)
    real(wp) :: c
    integer, allocatable :: pairs(:,:)
    real(wp) :: r, times(n_along)
    integer :: i, j, k, l, frames, frame, sgn, n_orbit, last
    logical :: running, stopped, demo

    frames = huge(frames)
    file = ''
    if (command_argument_count() >= 1) then
        call get_command_argument(1, arg)
        read(arg, *) frames
    end if
    if (command_argument_count() >= 2) call get_command_argument(2, file)
    demo = .false.
    if (command_argument_count() >= 3) then
        call get_command_argument(3, arg)
        demo = trim(arg) == '--demo-pick'
    end if

    ! the halo orbit, and its monodromy matrix's eigenvectors
    x0 = 0.82575887090385_wp
    z0 = 0.08_wp
    vy0 = 0.19369724986446_wp
    call correct_halo(x0, z0, vy0, period)
    write(*, '(a,f12.9,a,f6.3,a,f12.9,a,f9.6,a,f6.2,a)') 'halo orbit: x0 = ', x0, ', z0 = ', z0, ', vy0 = ', vy0, &
        ', period ', period, ' (', period * tu_days, ' days)'
    s = initial_state()
    call propagate(eom_stm, s, period)
    call monodromy_vectors(reshape(s(7:42), [6, 6]), vu, vs)

    ! the orbit, and the eigenvectors carried along it (at n_around points)
    n_orbit = 4 * n_around
    allocate(orbit(6, n_orbit + 1), dirs(6, n_around, 2))
    s = initial_state()
    dt = period / n_orbit
    do i = 1, n_orbit + 1
        orbit(:, i) = s(1:6)
        if (modulo(i - 1, 4) == 0 .and. i <= n_orbit) then
            phi = reshape(s(7:42), [6, 6])
            dirs(:, (i - 1) / 4 + 1, 1) = matmul(phi, vu)
            dirs(:, (i - 1) / 4 + 1, 2) = matmul(phi, vs)
        end if
        if (i <= n_orbit) call propagate(eom_stm, s, dt)
    end do
    do k = 1, 2                                         ! each a unit position, + toward the Moon at the start
        sgn = merge(1, -1, dirs(1, 1, k) > 0.0_wp)
        do i = 1, n_around
            dirs(:, i, k) = sgn * dirs(:, i, k) / norm2(dirs(1:3, i, k))
        end do
    end do

    ! the tubes: unstable forward, stable backward; toward the Earth. Every
    ! n_around / n_lines-th trajectory also a line (a CurveArtist: pickable),
    ! its points in increasing time (the stable ones' toward the orbit: the
    ! highlight's arrows in the direction of motion), times in days
    allocate(grid(3, n_around, n_along, n_tubes))
    allocate(keys%colors(4, n_around * n_along, n_tubes))
    times = [(direction(1) * span * (j - 1) / (n_along - 1) * tu_days, j = 1, n_along)]
    do k = 1, n_tubes
        line_frames(k) = DrawableTrajectory(trim(names(k))//' trajectories')
        call line_frames(k)%show_decorations(.false.)
        do i = 1, n_around
            dir = dirs(:, i, branch(k))
            w = orbit(:, 4 * (i - 1) + 1) + side(k) * offset * dir
            stopped = .false.
            last = n_along
            l = 0
            if (modulo(i - 1, n_around / n_lines) == 0) l = (i - 1) / (n_around / n_lines) + 1
            if (l > 0) then
                keys%info(l, k) = LineInfo(index=i, n_around=n_around, closest=huge(1.0_wp), jacobi=jacobi(w))
            end if
            do j = 1, n_along
                if (j > 1 .and. .not. stopped) then
                    call propagate(eom, w, direction(k) * span / (n_along - 1))
                    stopped = norm2(w(1:3) - [-mu, 0.0_wp, 0.0_wp]) < re / lu .or. &
                              norm2(w(1:3) - [1.0_wp - mu, 0.0_wp, 0.0_wp]) < rm / lu
                    if (stopped) last = j
                    if (l > 0 .and. (j == n_along .or. stopped)) keys%info(l, k)%drift = jacobi(w) - keys%info(l, k)%jacobi
                end if
                grid(:, i, j, k) = w(1:3) * lu
                fade = 1.0_wp - 0.85_wp * real(j - 1, wp) / (n_along - 1)
                keys%colors(:, i + (j - 1) * n_around, k) = [tube_colors(:, k), fade]
                if (l > 0 .and. j <= last) then
                    r = norm2(w(1:3) - [-mu, 0.0_wp, 0.0_wp]) * lu
                    if (r < keys%info(l, k)%closest) then
                        keys%info(l, k)%closest = r
                        keys%info(l, k)%closest_time = times(j)
                    end if
                end if
            end do
            if (l > 0) then
                keys%info(l, k)%impact = norm2(w(1:3) - [-mu, 0.0_wp, 0.0_wp]) < re / lu
                paths(l, k) = Trajectory()
                if (direction(k) > 0.0_wp) then
                    do j = 1, last
                        call paths(l, k)%add(times(j), pos=grid(:, i, j, k))
                    end do
                else
                    do j = last, 1, -1
                        call paths(l, k)%add(-times(j), pos=grid(:, i, j, k))
                    end do
                end if
                lines(l, k) = CurveArtist(paths(l, k), color=[tube_colors(:, k), 0.7_wp], width=1.2_wp)
                call line_frames(k)%add_artist(lines(l, k))
            end if
        end do
        tubes(k) = Polyhedron(trim(names(k)), color=tube_colors(:, k))
        call tubes(k)%set_grid(grid(:, :, :, k), closed_u=.true., lines=[0, 0])
        call tubes(k)%set_draw_mode(POLY_FACES)
        call tubes(k)%show_decorations(.false.)
        call tubes(k)%set_pickable(.false.)
        keys%names(k) = names(k)
        keys%line_colors(:, k) = [tube_colors(:, k), 1.0_wp]
    end do

    ! the zero-velocity curves at the orbit's Jacobi constant: edges only
    c = jacobi(orbit(:, 1))
    write(*, '(a,f10.6)') 'Jacobi constant: ', c
    segments = zero_velocity_curves(c, 1.4_wp, 600) * lu
    pairs = reshape([(i, i = 1, size(segments, 2))], [2, size(segments, 2) / 2])
    zvc = Polyhedron('Zero-velocity curves', color=[0.85_wp, 0.85_wp, 0.85_wp, 0.8_wp])
    call zvc%set_triangles(segments, reshape([integer ::], [3, 0]))
    call zvc%set_edges(pairs)
    call zvc%set_draw_mode(POLY_EDGES)
    call zvc%set_line_width(1.5_wp)
    call zvc%show_decorations(.false.)

    ! the scene: the rotating frame, the Earth and the Moon on its x axis
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
    do i = 1, n_orbit + 1
        call halo%add(period * (i - 1) / n_orbit, pos=orbit(1:3, i) * lu)
    end do
    halo_curve = CurveArtist(halo, color=[1.0_wp, 1.0_wp, 1.0_wp], width=2.5_wp)
    halo_frame = DrawableTrajectory('L1 halo orbit')
    call halo_frame%show_decorations(.false.)
    call halo_frame%add_artist(halo_curve)
    sun = ReferenceFrame('Sun')
    call sun%set_position([-0.4_wp, -1.0_wp, 0.6_wp] * 1.5e8_wp)
    call sun%set_light_source_enabled(.true.)
    call sun%set_light_diffuse([1.0_wp, 1.0_wp, 1.0_wp])
    call sun%set_light_ambient([0.25_wp, 0.25_wp, 0.25_wp])
    call sun%show_decorations(.false.)
    if (.not. (rotating%add_child(earth) .and. rotating%add_child(moon) .and. rotating%add_child(halo_frame) .and. &
               rotating%add_child(sun) .and. rotating%add_child(zvc))) error stop 'add_child failed'
    do k = 1, n_tubes
        if (.not. (rotating%add_child(tubes(k)) .and. rotating%add_child(line_frames(k)))) error stop 'add_child failed'
    end do
    call halo_frame%set_pickable(.false.)
    call zvc%set_pickable(.false.)

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
    call keys%apply()
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
                     'l: lit / unlit    ' // &
                     's: smooth / flat    ' // &
                     'c: fading colors / plain    + / -: opacity', &
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
    end do
    if (len_trim(file) > 0 .and. running) then
        if (window%capture(trim(file))) write(*, '(a)') 'captured '//trim(file)
    end if
    call window%close()

contains

    !> (--demo-pick, for a capture) Select a point 3/5 along the 4th unstable
    !> trajectory, where it is on the screen.
    subroutine demo_pick()
        real(wp) :: at(2), p(3)
        p = paths(4, 1)%position(3 * paths(4, 1)%num_positions() / 5)
        if (window%screen_position(p, at)) call keys%select(window, 2.0_wp * at(1) - 1.0_wp, 2.0_wp * at(2) - 1.0_wp)
    end subroutine demo_pick

    !> The corrected halo orbit's initial state, with the identity STM.
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
