!> Two-body motion about the Earth, for the trajectory design demo: a state
!> (position, km; velocity, km/s) propagated exactly with universal
!> variables (any conic: ellipse, parabola, hyperbola), and its orbital
!> elements.
!>
!> The trajectory is sampled uniformly in the universal anomaly chi (for an
!> ellipse, the eccentric anomaly times sqrt(a)), not in time: the points are
!> spread evenly along the orbit, however eccentric (uniform times bunch them
!> at apoapsis and leave periapsis jagged). The time at each point follows
!> from Kepler's equation in universal form; the end (`duration`) is found by
!> Newton's method on it, safeguarded by bisection (the time grows with chi).
module kepler

    use vkframes, only: wp, pi

    implicit none

    private

    real(wp), parameter, public :: mu = 398600.4418_wp     !! the Earth's gravitational parameter (km^3/s^2)
    real(wp), parameter, public :: earth_radius = 6378.137_wp  !! km

    !> Orbital elements (angles in degrees; a < 0 for a hyperbola).
    type, public :: Elements
        real(wp) :: a = 0.0_wp                             !! semi-major axis (km)
        real(wp) :: e = 0.0_wp                              !! eccentricity
        real(wp) :: i = 0.0_wp                              !! inclination
        real(wp) :: raan = 0.0_wp                           !! right ascension of the ascending node
        real(wp) :: argp = 0.0_wp                           !! argument of periapsis
        real(wp) :: nu = 0.0_wp                             !! true anomaly
        real(wp) :: rp = 0.0_wp                             !! periapsis radius (km)
        real(wp) :: ra = 0.0_wp                             !! apoapsis radius (km; huge() if not bound)
        real(wp) :: period = 0.0_wp                         !! s (huge() if not bound)
        real(wp) :: energy = 0.0_wp                         !! specific energy (km^2/s^2)
        real(wp) :: h(3) = 0.0_wp                           !! specific angular momentum (km^2/s)
    end type Elements

    public :: propagate, elements_of, state_at

contains

    !> The Stumpff functions C(z) and S(z).
    pure subroutine stumpff(z, c, s)
        real(wp), intent(in) :: z
        real(wp), intent(out) :: c, s
        real(wp) :: sz
        if (z > 1.0e-6_wp) then
            sz = sqrt(z)
            c = (1.0_wp - cos(sz)) / z
            s = (sz - sin(sz)) / (sz * z)
        else if (z < -1.0e-6_wp) then
            sz = sqrt(-z)
            c = (cosh(sz) - 1.0_wp) / (-z)
            s = (sinh(sz) - sz) / (sz * (-z))
        else                                                ! (series near 0)
            c = 0.5_wp - z / 24.0_wp + z * z / 720.0_wp
            s = 1.0_wp / 6.0_wp - z / 120.0_wp + z * z / 5040.0_wp
        end if
    end subroutine stumpff

    !> The state at universal anomaly `chi` from (r0, v0): position, velocity
    !> and the time since (s).
    pure subroutine state_at(r0, v0, chi, r, v, t)
        real(wp), intent(in) :: r0(3), v0(3), chi
        real(wp), intent(out) :: r(3), v(3), t
        real(wp) :: rn0, sigma, alpha, z, c, s, f, g, fdot, gdot, rn, smu
        smu = sqrt(mu)
        rn0 = norm2(r0)
        sigma = dot_product(r0, v0) / smu
        alpha = 2.0_wp / rn0 - dot_product(v0, v0) / mu
        z = alpha * chi * chi
        call stumpff(z, c, s)
        t = (sigma * chi * chi * c + (1.0_wp - alpha * rn0) * chi**3 * s + rn0 * chi) / smu
        f = 1.0_wp - chi * chi / rn0 * c
        g = t - chi**3 * s / smu
        r = f * r0 + g * v0
        rn = chi * chi * c + sigma * chi * (1.0_wp - z * s) + rn0 * (1.0_wp - z * c)
        fdot = smu / (rn * rn0) * chi * (z * s - 1.0_wp)
        gdot = 1.0_wp - chi * chi / rn * c
        v = fdot * r0 + gdot * v0
    end subroutine state_at

    !> The universal anomaly at time `duration` (s) after (r0, v0).
    function chi_at(r0, v0, duration) result(chi)
        real(wp), intent(in) :: r0(3), v0(3), duration
        real(wp) :: chi
        real(wp) :: lo, hi, t, r(3), v(3), alpha, step
        integer :: k
        alpha = 2.0_wp / norm2(r0) - dot_product(v0, v0) / mu
        ! a bracket: chi from 0 up until the time passes the duration
        lo = 0.0_wp
        hi = sqrt(mu) * abs(alpha) * duration
        if (hi <= 0.0_wp) hi = sqrt(mu) * duration / norm2(r0)
        hi = max(hi, 1.0e-3_wp)
        do k = 1, 200
            call state_at(r0, v0, hi, r, v, t)
            if (t >= duration) exit
            lo = hi
            hi = 2.0_wp * hi
        end do
        ! Newton (dt/dchi = |r| / sqrt(mu)), kept within the bracket
        chi = 0.5_wp * (lo + hi)
        do k = 1, 100
            call state_at(r0, v0, chi, r, v, t)
            if (abs(t - duration) <= 1.0e-9_wp * max(1.0_wp, duration)) return
            if (t < duration) then
                lo = chi
            else
                hi = chi
            end if
            step = (duration - t) * sqrt(mu) / norm2(r)
            chi = chi + step
            if (chi <= lo .or. chi >= hi) chi = 0.5_wp * (lo + hi)
        end do
    end function chi_at

    !> The trajectory from (r0, v0) for `duration` s: `n` points (times t,
    !> positions r) evenly along it (see the module documentation), cut where
    !> it first goes below the Earth's surface (`n` then fewer, `impact` true).
    subroutine propagate(r0, v0, duration, n, t, r, impact)
        real(wp), intent(in) :: r0(3), v0(3), duration
        integer, intent(inout) :: n                         !! points wanted; points given
        real(wp), intent(out) :: t(:), r(:,:)               !! (n), (3, n)
        logical, intent(out) :: impact
        real(wp) :: chi_end, v(3)
        integer :: k
        impact = .false.
        n = min(n, size(t), size(r, 2))
        chi_end = chi_at(r0, v0, duration)
        do k = 1, n
            call state_at(r0, v0, chi_end * (k - 1) / (n - 1), r(:, k), v, t(k))
            if (k > 1 .and. norm2(r(:, k)) < earth_radius) then
                impact = .true.
                n = k
                return
            end if
        end do
    end subroutine propagate

    !> The orbital elements of a state (position, km; velocity, km/s).
    function elements_of(r, v) result(el)
        real(wp), intent(in) :: r(3), v(3)
        type(Elements) :: el
        real(wp) :: rn, h(3), node(3), ev(3), nn
        rn = norm2(r)
        h = [r(2) * v(3) - r(3) * v(2), r(3) * v(1) - r(1) * v(3), r(1) * v(2) - r(2) * v(1)]
        el%h = h
        el%energy = 0.5_wp * dot_product(v, v) - mu / rn
        ev = ((dot_product(v, v) - mu / rn) * r - dot_product(r, v) * v) / mu
        el%e = norm2(ev)
        if (abs(el%energy) > 1.0e-12_wp) then
            el%a = -mu / (2.0_wp * el%energy)
        else
            el%a = huge(1.0_wp)
        end if
        el%rp = dot_product(h, h) / mu / (1.0_wp + el%e)
        el%ra = huge(1.0_wp)
        el%period = huge(1.0_wp)
        if (el%e < 1.0_wp .and. el%a > 0.0_wp) then
            el%ra = el%a * (1.0_wp + el%e)
            el%period = 2.0_wp * pi * sqrt(el%a**3 / mu)
        end if
        if (norm2(h) <= 0.0_wp) return                      ! (radial: no plane)
        el%i = acos(max(-1.0_wp, min(1.0_wp, h(3) / norm2(h)))) * 180.0_wp / pi
        node = [-h(2), h(1), 0.0_wp]
        nn = norm2(node)
        if (nn > 1.0e-9_wp) then
            el%raan = modulo(atan2(node(2), node(1)) * 180.0_wp / pi, 360.0_wp)
            if (el%e > 1.0e-9_wp) el%argp = angle_between(node, ev, h)
        else if (el%e > 1.0e-9_wp) then                     ! (equatorial: from x)
            el%argp = angle_between([1.0_wp, 0.0_wp, 0.0_wp], ev, h)
        end if
        if (el%e > 1.0e-9_wp) then
            el%nu = angle_between(ev, r, h)
        else if (nn > 1.0e-9_wp) then                       ! (circular: from the node)
            el%nu = angle_between(node, r, h)
        else
            el%nu = angle_between([1.0_wp, 0.0_wp, 0.0_wp], r, h)
        end if
    end function elements_of

    !> The angle from a to b about the normal n (degrees, 0 to 360).
    pure real(wp) function angle_between(a, b, n) result(angle)
        real(wp), intent(in) :: a(3), b(3), n(3)
        real(wp) :: c(3)
        c = [a(2) * b(3) - a(3) * b(2), a(3) * b(1) - a(1) * b(3), a(1) * b(2) - a(2) * b(1)]
        angle = atan2(sign(norm2(c), dot_product(c, n)), dot_product(a, b)) * 180.0_wp / pi
        angle = modulo(angle, 360.0_wp)
    end function angle_between

end module kepler
