!> Checks View geometry (no window): world-to-view transforms for relative and
!> absolute views and for views between frames (direct and azimuth-elevation),
!> and the default camera distance (OpenFrames' resetView formula).
program test_view

    use vkframes

    implicit none

    type(Sphere), target :: earth
    type(ReferenceFrame), target :: craft
    type(View) :: v
    real(wp) :: m(4,4), p(4), eye(3), center(3), up(3), dist
    real(wp), parameter :: tol = 1.0e-9_wp, z(3) = [0.0_wp, 0.0_wp, 1.0_wp]
    logical :: ok
    integer :: failures

    failures = 0
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

    if (failures > 0) then
        write(*, '(a,i0,a)') 'FAIL: ', failures, ' checks failed'
        error stop 1
    end if
    write(*, '(a)') 'PASS: View'

contains

    subroutine check(ok, what)
        logical, intent(in) :: ok
        character(len=*), intent(in) :: what
        if (.not. ok) then
            write(*, '(a)') '  failed: '//what
            failures = failures + 1
        end if
    end subroutine check

end program test_view
