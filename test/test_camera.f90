!> Checks a window's camera (in a window of its own): get_camera and
!> set_camera in the view frame's coordinates and in the root's; flights
!> (set_camera with a duration, and changes of view with a view transition)
!> pass between the two cameras and end at the new one; a mouse press ends a
!> flight; View%set_camera gives a view its camera for when it is shown; the
!> zoom speed scales a scroll step's zoom (a half step as a step at half the
!> speed).
program test_camera

    use vkframes

    implicit none

    real(wp), parameter :: tol = 1.0e-6_wp
    type(ReferenceFrame), target :: root, craft
    type(FrameManager), target :: scene
    type(WindowProxy) :: window
    type(View), target :: view_a, view_b
    real(wp) :: eye(3), center(3), up(3), cam_b(3,3), target(3,3), mid(3), z(3)
    integer :: failures, n

    failures = 0
    root = ReferenceFrame('Root')
    craft = ReferenceFrame('Craft')
    call craft%set_position([100.0_wp, 0.0_wp, 0.0_wp])
    call craft%set_attitude(quat_from_axis_angle([0.0_wp, 0.0_wp, 1.0_wp], 0.5_wp * pi))
    if (.not. root%add_child(craft)) error stop 'add_child failed'
    scene = FrameManager(root)
    if (.not. window%open(320, 240, 'vkframes: camera test')) then
        write(*, '(a)') 'FAIL: could not open a window'
        error stop 1
    end if
    call window%set_scene(scene)
    view_a = View(root, root)
    view_b = View(root, craft)                              ! (a turned frame, away from the root's origin)
    call window%add_view(view_a)
    call window%add_view(view_b)
    call frame()

    ! set and get, in the view frame and in the root frame
    call window%set_camera([0.0_wp, -20.0_wp, 5.0_wp], [1.0_wp, 0.0_wp, 0.0_wp], [0.0_wp, 0.0_wp, 1.0_wp])
    call frame()
    call window%get_camera(eye, center, up)
    call check(near(eye, [0.0_wp, -20.0_wp, 5.0_wp]) .and. near(center, [1.0_wp, 0.0_wp, 0.0_wp]), 'set and get')
    call window%select_view(2)
    call frame()
    call window%get_camera(eye, center, up)
    cam_b = reshape([eye, center, up], [3, 3])              ! (view b's camera after its reset)
    call window%get_camera(eye, center, up, in_root=.true.)
    call check(near(center, [100.0_wp, 0.0_wp, 0.0_wp] + rotate_z(cam_b(:, 2))), 'in the root frame: moved and turned')
    call window%set_camera(eye, center, up, in_root=.true.)
    call frame()
    call window%get_camera(eye, center, up)
    call check(near(eye, cam_b(:, 1)) .and. near(center, cam_b(:, 2)) .and. near(up, cam_b(:, 3)), &
               'set in the root frame: the same camera in the view frame')

    ! a flight within the view: between the two cameras, then at the new one
    target = reshape([0.0_wp, -400.0_wp, 0.0_wp, 0.0_wp, 0.0_wp, 0.0_wp, 0.0_wp, 0.0_wp, 1.0_wp], [3, 3])
    call window%set_camera(target(:, 1), target(:, 2), target(:, 3), duration=0.3_wp)
    call check(window%in_transition(), 'set_camera with a duration: flying')
    call frame()
    call window%get_camera(eye, center, up)
    mid = eye
    n = fly_to_end()
    call window%get_camera(eye, center, up)
    call check(.not. window%in_transition() .and. near(eye, target(:, 1)) .and. near(center, target(:, 2)), &
               'flight: ends at the new camera')
    call check(n > 2 .and. norm2(mid) > norm2(cam_b(:, 1) - cam_b(:, 2)) * 0.999_wp .and. norm2(mid) < 400.0_wp, &
               'flight: in between on the way')

    ! a mouse press ends a flight where it is
    call window%set_camera(cam_b(:, 1), cam_b(:, 2), cam_b(:, 3), duration=5.0_wp)
    call frame()
    call window%mouse(MOUSE_PRESS, 10, 10, 256, MOUSE_LEFT)
    call window%mouse(MOUSE_RELEASE, 10, 10, 0, MOUSE_LEFT)
    call frame()
    call check(.not. window%in_transition(), 'a mouse press ends a flight')

    ! changes of view with a view transition: fly to the other view's camera
    call window%set_camera(cam_b(:, 1), cam_b(:, 2), cam_b(:, 3))
    call frame()
    call window%set_view_transition(0.3_wp)
    call check(window%view_transition() == 0.3_wp, 'view transition')
    call window%select_view(1)
    call frame()
    call check(window%in_transition(), 'change of view: flying')
    n = fly_to_end()
    call window%get_camera(eye, center, up)
    call check(near(eye, [0.0_wp, -20.0_wp, 5.0_wp]) .and. near(center, [1.0_wp, 0.0_wp, 0.0_wp]), &
               'change of view: ends at its kept camera')
    call window%next_view(duration=0.0_wp)                  ! (a cut, whatever the transition)
    call frame()
    call check(.not. window%in_transition(), 'duration 0: a cut')

    ! View%set_camera: a view not shown gets its camera when it is
    call view_a%set_camera([5.0_wp, 5.0_wp, 5.0_wp], [0.0_wp, 0.0_wp, 0.0_wp], [0.0_wp, 0.0_wp, 1.0_wp])
    call window%set_view_transition(0.0_wp)
    call window%select_view(1)
    call frame()
    call window%get_camera(eye, center, up)
    call check(near(eye, [5.0_wp, 5.0_wp, 5.0_wp]), 'View%set_camera: when shown')

    ! the zoom speed: a scroll step's zoom scaled by it (and a half step = a full step at half the speed)
    call check(window%zoom_speed() == 1.0_wp .and. window%camera_throw(), 'zoom speed and throw: VSG''s defaults')
    call window%mouse(MOUSE_MOVE, 160, 120, 0, 0)                   ! (the pointer in the window: the trackball zooms)
    call frame()
    z(1) = zoom_ratio(1.0_wp)
    call window%set_zoom_speed(0.5_wp)
    z(2) = zoom_ratio(1.0_wp)
    call window%set_zoom_speed(1.0_wp)
    z(3) = zoom_ratio(0.5_wp)
    call check(z(1) /= 1.0_wp .and. abs(log(z(2)) / log(z(1)) - 0.5_wp) < 0.05_wp, &
               'zoom speed 0.5: half the zoom per scroll step')
    call check(abs(z(3) - z(2)) < 1.0e-9_wp, 'a half scroll step: as a step at half the speed')
    call window%set_camera_throw(.false.)
    call check(.not. window%camera_throw(), 'camera throw off')

    call window%close()
    if (failures > 0) then
        write(*, '(a,i0,a)') 'FAIL: ', failures, ' checks failed'
        error stop 1
    end if
    write(*, '(a)') 'PASS: camera (get, set, in the root frame), flights, view transitions'

contains

    !> The camera's distance after a scroll of `dy` steps, over the distance before.
    real(wp) function zoom_ratio(dy) result(ratio)
        real(wp), intent(in) :: dy
        real(wp) :: d0
        call window%get_camera(eye, center, up)
        d0 = norm2(eye - center)
        call window%scroll(dy)
        call frame()
        call frame()
        call window%get_camera(eye, center, up)
        ratio = norm2(eye - center) / d0
    end function zoom_ratio

    subroutine frame()
        if (.not. window%frame()) then
            write(*, '(a)') 'FAIL: the window closed'
            error stop 1
        end if
    end subroutine frame

    !> Frames until the flight ends (at most 10 s); how many.
    integer function fly_to_end() result(n)
        n = 0
        do while (window%in_transition() .and. n < 2000)
            call frame()
            n = n + 1
        end do
    end function fly_to_end

    pure function rotate_z(v) result(w)                     ! (the craft's attitude: 90 degrees about z)
        real(wp), intent(in) :: v(3)
        real(wp) :: w(3)
        w = [-v(2), v(1), v(3)]
    end function rotate_z

    logical function near(a, b)
        real(wp), intent(in) :: a(:), b(:)
        near = maxval(abs(a - b)) < tol * max(1.0_wp, maxval(abs(b)))
    end function near

    subroutine check(ok, what)
        logical, intent(in) :: ok
        character(len=*), intent(in) :: what
        if (.not. ok) then
            write(*, '(a)') 'FAIL: '//what
            failures = failures + 1
        end if
    end subroutine check

end program test_camera
