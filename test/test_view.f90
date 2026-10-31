!> Checks View geometry (no window): world-to-view transforms for relative and
!> absolute views and for views between frames (direct and azimuth-elevation),
!> and the default camera distance (OpenFrames' resetView formula);
!> camera_between (the ends, the center halfway, the distance geometric, the
!> orientation turned halfway, up kept perpendicular).
program test_view

    use vkframes
    use vkf_checks, only: check, report

    implicit none

    type(Sphere), target :: earth
    type(ReferenceFrame), target :: craft
    type(View) :: v
    real(wp) :: m(4,4), p(4), eye(3), center(3), up(3), dist
    real(wp), parameter :: tol = 1.0e-9_wp, z(3) = [0.0_wp, 0.0_wp, 1.0_wp]
    logical :: ok

    earth = Sphere('Earth', radius=2.0_wp)
    craft = ReferenceFrame('Craft')
    ok = earth%add_child(craft)
    call craft%set_position([10.0_wp, 0.0_wp, 0.0_wp])
    call craft%set_attitude(quat_from_axis_angle(z, 0.5_wp * pi))     ! craft x axis = root y axis

    ! relative view: body-fixed (the root's x axis is the craft's -y)
    v = View(earth, craft)
    m = v%world_to_view()
    p = matmul(m, [10.0_wp, 0.0_wp, 0.0_wp, 1.0_wp])
    call check(all(abs(p(1:3)) < tol), 'relative: the craft origin is the view origin')
    p = matmul(m, [11.0_wp, 0.0_wp, 0.0_wp, 1.0_wp])
    call check(all(abs(p(1:3) - [0.0_wp, -1.0_wp, 0.0_wp]) < tol), 'relative: rotated with the craft')

    ! absolute view: translation only
    v = View(earth, craft, frame_type=ABSOLUTE_FRAME)
    m = v%world_to_view()
    p = matmul(m, [11.0_wp, 0.0_wp, 0.0_wp, 1.0_wp])
    call check(all(abs(p(1:3) - [1.0_wp, 0.0_wp, 0.0_wp]) < tol), 'absolute: root axes at the craft origin')

    ! view between frames: from the craft toward the Earth (at -x from it) on +y
    v = View(earth, craft, lookat=earth, frame_type=ABSOLUTE_FRAME, rotation_type=VIEW_DIRECT)
    m = v%world_to_view()
    p = matmul(m, [0.0_wp, 0.0_wp, 0.0_wp, 1.0_wp])
    call check(all(abs(p(1:3) - [0.0_wp, 10.0_wp, 0.0_wp]) < tol), 'direct: the look-at frame on +y')
    v = View(earth, craft, lookat=earth, frame_type=ABSOLUTE_FRAME)
    m = v%world_to_view()
    p = matmul(m, [0.0_wp, 0.0_wp, 0.0_wp, 1.0_wp])
    call check(all(abs(p(1:3) - [0.0_wp, 10.0_wp, 0.0_wp]) < tol), 'azel: the look-at frame on +y')
    p = matmul(m, [10.0_wp, 0.0_wp, 1.0_wp, 1.0_wp])
    call check(all(abs(p(1:3) - [0.0_wp, 0.0_wp, 1.0_wp]) < tol), 'azel keeps z up')
    call craft%set_position([10.0_wp, 0.0_wp, 10.0_wp])               ! above: elevation -45 degrees
    m = v%world_to_view()
    p = matmul(m, [0.0_wp, 0.0_wp, 0.0_wp, 1.0_wp])
    call check(all(abs(p(1:3) - [0.0_wp, sqrt(200.0_wp), 0.0_wp]) < tol), 'azel: elevation too')

    ! default camera: the Earth's bound (radius 2) fills the vertical field of view
    v = View(earth, earth)
    call v%home(2.0_wp, eye, center, up)                            ! aspect 2: vertical field is limiting
    dist = 2.0_wp / sin(22.5_wp * pi / 180.0_wp)
    call check(all(abs(center) < tol) .and. all(abs(eye - [0.0_wp, -dist, 0.0_wp]) < tol) .and. &
               all(up == z), 'home: radius / sin(fovy/2) along -y, z up')
    call v%set_default_view_distance(5.0_wp)
    call v%home(2.0_wp, eye, center, up)
    call check(abs(eye(2) + 5.0_wp / sin(22.5_wp * pi / 180.0_wp)) < tol, 'home: default view distance')
    call v%set_default_view_parameters([1.0_wp, 2.0_wp, 3.0_wp], [0.0_wp, 0.0_wp, 0.0_wp], z)
    call v%home(2.0_wp, eye, center, up)
    call check(all(eye == [1.0_wp, 2.0_wp, 3.0_wp]), 'home: default view parameters')

    call check_camera_between()

    call report('View')

contains

    !> From a camera 10 away along -y, looking at the origin with z up, to one
    !> 1000 away along +x, looking at [0, 50, 0]: half way, the center is
    !> halfway, 100 away (10 x 1000, square root), and the view direction
    !> turned halfway from +y to -x (45 degrees, about z) with z still up.
    subroutine check_camera_between()
        real(wp) :: a(3,3), b(3,3), c(3,3), f(3)
        a = reshape([0.0_wp, -10.0_wp, 0.0_wp, 0.0_wp, 0.0_wp, 0.0_wp, 0.0_wp, 0.0_wp, 1.0_wp], [3, 3])
        b = reshape([1000.0_wp, 50.0_wp, 0.0_wp, 0.0_wp, 50.0_wp, 0.0_wp, 0.0_wp, 0.0_wp, 1.0_wp], [3, 3])
        c = camera_between(0.0_wp, a, b)
        call check(maxval(abs(c - a)) < 1.0e-9_wp, 'camera_between: the start')
        c = camera_between(1.0_wp, a, b)
        call check(maxval(abs(c - b)) < 1.0e-9_wp, 'camera_between: the end')
        c = camera_between(0.5_wp, a, b)
        f = c(:, 2) - c(:, 1)
        call check(maxval(abs(c(:, 2) - [0.0_wp, 25.0_wp, 0.0_wp])) < 1.0e-9_wp, 'camera_between: the center halfway')
        call check(abs(norm2(f) - 100.0_wp) < 1.0e-9_wp, 'camera_between: the distance geometric')
        call check(abs(dot_product(f / norm2(f), [-1.0_wp, 1.0_wp, 0.0_wp] / sqrt(2.0_wp)) - 1.0_wp) < 1.0e-9_wp, &
                   'camera_between: the direction turned halfway')
        call check(maxval(abs(c(:, 3) - z)) < 1.0e-9_wp, 'camera_between: up kept')
    end subroutine check_camera_between

end program test_view
