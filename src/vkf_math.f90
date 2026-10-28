!> Math for vkframes: the working precision, quaternions and 4x4 transforms.
!>
!> Quaternions are `q(4) = [x, y, z, w]` (vector part first, as OpenFrames and
!> OpenSceneGraph store them), unit length for rotations. The rotation of q
!> takes vectors in a frame to its parent frame: v_parent = rotate(q, v_local).
!> Matrices are 4x4, column-major, acting on column vectors.
module vkf_math

    use, intrinsic :: iso_fortran_env, only: real64

    implicit none

    private

    integer, parameter, public :: wp = real64           !! working precision
    real(wp), parameter, public :: pi = acos(-1.0_wp)   !! pi

    real(wp), parameter, public :: quat_identity(4) = [0.0_wp, 0.0_wp, 0.0_wp, 1.0_wp]   !! the identity rotation

    public :: quat_from_axis_angle, quat_multiply, quat_conjugate, quat_normalize
    public :: quat_rotate, quat_to_matrix, quat_from_matrix, quat_slerp, quat_from_vectors
    public :: identity_matrix, translation_matrix, frame_matrix, rigid_inverse, cross

contains

    !> The rotation by `angle` (radians) about `axis` (need not be unit length).
    pure function quat_from_axis_angle(axis, angle) result(q)
        real(wp), intent(in) :: axis(3)
        real(wp), intent(in) :: angle       !! (radians)
        real(wp) :: q(4)
        real(wp) :: n
        n = norm2(axis)
        if (n == 0.0_wp) then
            q = quat_identity
        else
            q(1:3) = axis / n * sin(0.5_wp * angle)
            q(4) = cos(0.5_wp * angle)
        end if
    end function quat_from_axis_angle

    !> The product a*b: rotating by b, then by a.
    pure function quat_multiply(a, b) result(q)
        real(wp), intent(in) :: a(4), b(4)
        real(wp) :: q(4)
        q(1) = a(4) * b(1) + a(1) * b(4) + a(2) * b(3) - a(3) * b(2)
        q(2) = a(4) * b(2) - a(1) * b(3) + a(2) * b(4) + a(3) * b(1)
        q(3) = a(4) * b(3) + a(1) * b(2) - a(2) * b(1) + a(3) * b(4)
        q(4) = a(4) * b(4) - a(1) * b(1) - a(2) * b(2) - a(3) * b(3)
    end function quat_multiply

    !> The conjugate of q: the inverse rotation, for a unit quaternion.
    pure function quat_conjugate(q) result(c)
        real(wp), intent(in) :: q(4)
        real(wp) :: c(4)
        c = [-q(1:3), q(4)]
    end function quat_conjugate

    !> q / |q| (the identity for a zero quaternion).
    pure function quat_normalize(q) result(u)
        real(wp), intent(in) :: q(4)
        real(wp) :: u(4)
        real(wp) :: n
        n = norm2(q)
        if (n == 0.0_wp) then
            u = quat_identity
        else
            u = q / n
        end if
    end function quat_normalize

    !> The vector v rotated by the unit quaternion q.
    pure function quat_rotate(q, v) result(r)
        real(wp), intent(in) :: q(4), v(3)
        real(wp) :: r(3)
        real(wp) :: t(3)
        ! r = v + 2 w (u x v) + 2 u x (u x v), with u = q(1:3), w = q(4)
        t = 2.0_wp * cross(q(1:3), v)
        r = v + q(4) * t + cross(q(1:3), t)
    end function quat_rotate

    !> The 3x3 rotation matrix of the unit quaternion q (columns: the rotated axes).
    pure function quat_to_matrix(q) result(m)
        real(wp), intent(in) :: q(4)
        real(wp) :: m(3,3)
        real(wp) :: x, y, z, w
        x = q(1); y = q(2); z = q(3); w = q(4)
        m(1,1) = 1.0_wp - 2.0_wp * (y*y + z*z)
        m(2,1) = 2.0_wp * (x*y + z*w)
        m(3,1) = 2.0_wp * (x*z - y*w)
        m(1,2) = 2.0_wp * (x*y - z*w)
        m(2,2) = 1.0_wp - 2.0_wp * (x*x + z*z)
        m(3,2) = 2.0_wp * (y*z + x*w)
        m(1,3) = 2.0_wp * (x*z + y*w)
        m(2,3) = 2.0_wp * (y*z - x*w)
        m(3,3) = 1.0_wp - 2.0_wp * (x*x + y*y)
    end function quat_to_matrix

    !> The unit quaternion of a 3x3 rotation matrix (columns: the rotated
    !> axes; the inverse of quat_to_matrix), by Shepperd's method (from the
    !> largest of w and the vector components, for accuracy).
    pure function quat_from_matrix(m) result(q)
        real(wp), intent(in) :: m(3,3)
        real(wp) :: q(4)
        real(wp) :: t, s
        t = m(1,1) + m(2,2) + m(3,3)
        if (t >= max(m(1,1), m(2,2), m(3,3))) then
            s = 2.0_wp * sqrt(1.0_wp + t)
            q = [(m(3,2) - m(2,3)) / s, (m(1,3) - m(3,1)) / s, (m(2,1) - m(1,2)) / s, 0.25_wp * s]
        else if (m(1,1) >= m(2,2) .and. m(1,1) >= m(3,3)) then
            s = 2.0_wp * sqrt(1.0_wp + m(1,1) - m(2,2) - m(3,3))
            q = [0.25_wp * s, (m(1,2) + m(2,1)) / s, (m(1,3) + m(3,1)) / s, (m(3,2) - m(2,3)) / s]
        else if (m(2,2) >= m(3,3)) then
            s = 2.0_wp * sqrt(1.0_wp - m(1,1) + m(2,2) - m(3,3))
            q = [(m(1,2) + m(2,1)) / s, 0.25_wp * s, (m(2,3) + m(3,2)) / s, (m(1,3) - m(3,1)) / s]
        else
            s = 2.0_wp * sqrt(1.0_wp - m(1,1) - m(2,2) + m(3,3))
            q = [(m(1,3) + m(3,1)) / s, (m(2,3) + m(3,2)) / s, 0.25_wp * s, (m(2,1) - m(1,2)) / s]
        end if
        q = q / norm2(q)
    end function quat_from_matrix

    !> Spherical linear interpolation from `a` (t = 0) to `b` (t = 1), along the
    !> shorter arc; linear when the two are nearly equal. Same as OpenSceneGraph's
    !> osg::Quat::slerp, which OpenFrames' TrajectoryFollower uses.
    pure function quat_slerp(t, a, b) result(q)
        real(wp), intent(in) :: t           !! the fraction of the way (0 to 1)
        real(wp), intent(in) :: a(4), b(4)
        real(wp) :: q(4)
        real(wp), parameter :: epsilon = 0.00001_wp
        real(wp) :: cos_omega, omega, sin_omega, scale_a, scale_b, to(4)
        cos_omega = dot_product(a, b)
        to = b
        if (cos_omega < 0.0_wp) then
            cos_omega = -cos_omega
            to = -b
        end if
        if (1.0_wp - cos_omega > epsilon) then
            omega = acos(cos_omega)
            sin_omega = sin(omega)
            scale_a = sin((1.0_wp - t) * omega) / sin_omega
            scale_b = sin(t * omega) / sin_omega
        else
            scale_a = 1.0_wp - t
            scale_b = t
        end if
        q = scale_a * a + scale_b * to
    end function quat_slerp

    !> The shortest rotation taking the direction of `from` to that of `to`
    !> (OpenSceneGraph's osg::Quat::makeRotate(from, to)); a half turn about a
    !> perpendicular axis if they are opposite, the identity if either is zero.
    pure function quat_from_vectors(from, to) result(q)
        real(wp), intent(in) :: from(3), to(3)
        real(wp) :: q(4)
        real(wp), parameter :: eps = 1.0e-12_wp
        real(wp) :: u(3), v(3), d, axis(3)
        q = quat_identity
        if (norm2(from) == 0.0_wp .or. norm2(to) == 0.0_wp) return
        u = from / norm2(from)
        v = to / norm2(to)
        d = dot_product(u, v)
        if (d >= 1.0_wp - eps) return
        if (d <= -1.0_wp + eps) then
            axis = cross(u, [1.0_wp, 0.0_wp, 0.0_wp])
            if (norm2(axis) < 1.0e-6_wp) axis = cross(u, [0.0_wp, 1.0_wp, 0.0_wp])
            q = [axis / norm2(axis), 0.0_wp]
            return
        end if
        q = quat_normalize([cross(u, v), 1.0_wp + d])
    end function quat_from_vectors

    !> The inverse of a rigid transform (rotation and translation): [R' -R't].
    pure function rigid_inverse(m) result(inv)
        real(wp), intent(in) :: m(4,4)
        real(wp) :: inv(4,4)
        inv = 0.0_wp
        inv(1:3, 1:3) = transpose(m(1:3, 1:3))
        inv(1:3, 4) = -matmul(inv(1:3, 1:3), m(1:3, 4))
        inv(4, 4) = 1.0_wp
    end function rigid_inverse

    !> The 4x4 identity matrix.
    pure function identity_matrix() result(m)
        real(wp) :: m(4,4)
        integer :: k
        m = 0.0_wp
        do k = 1, 4
            m(k, k) = 1.0_wp
        end do
    end function identity_matrix

    !> The 4x4 transform translating by `t`.
    pure function translation_matrix(t) result(m)
        real(wp), intent(in) :: t(3)
        real(wp) :: m(4,4)
        m = identity_matrix()
        m(1:3, 4) = t
    end function translation_matrix

    !> A frame's local-to-parent transform, as OpenFrames' FrameTransform builds
    !> it: translate(position) * rotate(attitude) * scale(scale) * translate(-pivot).
    pure function frame_matrix(position, attitude, scale, pivot) result(m)
        real(wp), intent(in) :: position(3)             !! origin in the parent frame
        real(wp), intent(in) :: attitude(4)             !! quaternion [x, y, z, w]
        real(wp), intent(in), optional :: scale(3)      !! scale along each local axis (default 1)
        real(wp), intent(in), optional :: pivot(3)      !! local point placed at `position` (default 0)
        real(wp) :: m(4,4)
        real(wp) :: r(3,3), s(3), p(3)
        integer :: k
        s = 1.0_wp
        p = 0.0_wp
        if (present(scale)) s = scale
        if (present(pivot)) p = pivot
        r = quat_to_matrix(attitude)
        m = 0.0_wp
        do k = 1, 3
            m(1:3, k) = r(:, k) * s(k)
        end do
        m(1:3, 4) = position - matmul(m(1:3, 1:3), p)
        m(4, 4) = 1.0_wp
    end function frame_matrix

    !> The cross product a x b.
    pure function cross(a, b) result(c)
        real(wp), intent(in) :: a(3), b(3)
        real(wp) :: c(3)
        c = [a(2) * b(3) - a(3) * b(2), a(3) * b(1) - a(1) * b(3), a(1) * b(2) - a(2) * b(1)]
    end function cross

end module vkf_math
