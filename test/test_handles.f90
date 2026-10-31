!> A window's handler that records the handles' drags and the mouse buttons,
!> and paths for time handles: a line and a circle.
module test_handles_recorder

    use vkframes

    implicit none

    private

    type, extends(WindowEventHandler), public :: DragRecorder
        integer :: starts = 0, moves = 0, ends = 0          !! handle_drag's stages
        integer :: button_presses = 0, button_releases = 0
        class(DragHandle), pointer :: last => null()        !! the handle last told about
    contains
        procedure :: handle_drag => recorder_handle_drag
        procedure :: button_press => recorder_button_press
        procedure :: button_release => recorder_button_release
    end type DragRecorder

    !> The line x = t (y, z = 0).
    type, extends(TimePath), public :: LinePath
    contains
        procedure :: position => line_position
    end type LinePath

    !> The unit circle about z, at angle t (radians): a revolution every 2 pi.
    type, extends(TimePath), public :: CirclePath
    contains
        procedure :: position => circle_position
    end type CirclePath

contains

    !> The line's point at time t.
    function line_position(self, t) result(p)
        class(LinePath), intent(in) :: self
        real(wp), intent(in) :: t                           !! the time
        real(wp) :: p(3)
        p = [t, 0.0_wp, 0.0_wp]
    end function line_position

    !> The circle's point at time t.
    function circle_position(self, t) result(p)
        class(CirclePath), intent(in) :: self
        real(wp), intent(in) :: t                           !! the time (the angle, radians)
        real(wp) :: p(3)
        p = [cos(t), sin(t), 0.0_wp]
    end function circle_position

    !> A drag: its stage counted, the handle kept.
    subroutine recorder_handle_drag(self, window, handle, stage)
        class(DragRecorder), intent(inout) :: self
        class(WindowProxy), intent(inout) :: window         !! the window
        class(DragHandle), intent(inout), target :: handle  !! the handle dragged
        integer, intent(in) :: stage                        !! DRAG_START, DRAG_MOVE or DRAG_END
        select case (stage)
        case (DRAG_START)
            self%starts = self%starts + 1
        case (DRAG_MOVE)
            self%moves = self%moves + 1
        case (DRAG_END)
            self%ends = self%ends + 1
        end select
        self%last => handle
    end subroutine recorder_handle_drag

    !> A button press given to the handler: counted.
    subroutine recorder_button_press(self, window, x, y, button)
        class(DragRecorder), intent(inout) :: self
        class(WindowProxy), intent(inout) :: window         !! the window
        real(wp), intent(in) :: x, y                        !! where (-1 to 1, y up)
        integer, intent(in) :: button                       !! MOUSE_LEFT, MOUSE_MIDDLE or MOUSE_RIGHT
        self%button_presses = self%button_presses + 1
    end subroutine recorder_button_press

    !> A button release given to the handler: counted.
    subroutine recorder_button_release(self, window, x, y, button)
        class(DragRecorder), intent(inout) :: self
        class(WindowProxy), intent(inout) :: window         !! the window
        real(wp), intent(in) :: x, y                        !! where (-1 to 1, y up)
        integer, intent(in) :: button                       !! MOUSE_LEFT, MOUSE_MIDDLE or MOUSE_RIGHT
        self%button_releases = self%button_releases + 1
    end subroutine recorder_button_release

end module test_handles_recorder

!> Checks handles (vkf_handles): the cursor's ray against planes, spheres and
!> lines (vkf_pick); a VectorHandle's constraints, dragged with given rays;
!> its pick; and, in a window, the mouse hovering, grabbing, dragging and
!> releasing it (events forwarded as a host does), the camera not moving.
program test_handles

    use, intrinsic :: iso_c_binding, only: c_int
    use vkframes
    use vkf_pick, only: PickCamera, pick_camera
    use vkf_layer, only: vkf_viewer_size
    use test_handles_recorder, only: DragRecorder, LinePath, CirclePath

    implicit none

    integer, parameter :: MASK_LEFT = 256                   ! (VSG's BUTTON_MASK_1)
    real(wp), parameter :: tol = 1.0e-9_wp
    type(ReferenceFrame), target :: root
    type(VectorHandle), target :: arrow
    type(FrameManager), target :: scene
    type(WindowProxy) :: window
    type(View), target :: front
    type(DragRecorder), target :: rec
    type(PickHit), allocatable :: hits(:)
    integer :: failures

    failures = 0
    call check_rays()

    root = ReferenceFrame('Root')
    arrow = VectorHandle('Arrow', vector=[1.0_wp, 0.0_wp, 0.0_wp], color=[1.0_wp, 0.8_wp, 0.2_wp], scale=2.0_wp)
    call arrow%set_position([1.0_wp, 0.0_wp, 0.0_wp])      ! (its tip at [3, 0, 0] in the root)
    if (.not. root%add_child(arrow)) error stop 'add_child failed'
    call check(near3(arrow%tip(), [2.0_wp, 0.0_wp, 0.0_wp]), 'the tip: the display scale times the vector')
    call check_constraints()
    call check_pick()
    call check_time_handles()
    call check_window()

    if (failures > 0) then
        write(*, '(a,i0,a)') 'FAIL: ', failures, ' check(s)'
        error stop 1
    end if
    write(*, '(a)') 'PASS: handles (the cursor''s ray, VectorHandle''s constraints and pick, TimeHandle''s drags ' // &
                    'and pick, TrajectoryPath, dragged with the mouse)'

contains

    ! (each call first, then its results checked: an expression's operands may be evaluated in any order)
    !> ray_plane, ray_sphere and ray_line: where they meet, and where they miss.
    subroutine check_rays()
        real(wp) :: t, p(3), near, far, s
        logical :: hit
        hit = ray_plane([0.0_wp, 0.0_wp, 5.0_wp], [0.0_wp, 0.0_wp, -1.0_wp], [0.0_wp, 0.0_wp, 1.0_wp], &
                        [0.0_wp, 0.0_wp, 2.0_wp], t, p)
        call check(hit .and. abs(t - 4.0_wp) < tol .and. near3(p, [0.0_wp, 0.0_wp, 1.0_wp]), 'ray_plane: the point met')
        call check(.not. ray_plane([0.0_wp, 0.0_wp, 5.0_wp], [1.0_wp, 0.0_wp, 0.0_wp], [0.0_wp, 0.0_wp, 1.0_wp], &
                                   [0.0_wp, 0.0_wp, 1.0_wp], t, p), 'ray_plane: parallel')
        call check(.not. ray_plane([0.0_wp, 0.0_wp, 5.0_wp], [0.0_wp, 0.0_wp, 1.0_wp], [0.0_wp, 0.0_wp, 1.0_wp], &
                                   [0.0_wp, 0.0_wp, 1.0_wp], t, p), 'ray_plane: behind')
        hit = ray_sphere([-5.0_wp, 0.0_wp, 0.0_wp], [1.0_wp, 0.0_wp, 0.0_wp], [0.0_wp, 0.0_wp, 0.0_wp], 2.0_wp, near, far)
        call check(hit .and. abs(near - 3.0_wp) < tol .and. abs(far - 7.0_wp) < tol, 'ray_sphere: entering and leaving')
        call check(.not. ray_sphere([-5.0_wp, 3.0_wp, 0.0_wp], [1.0_wp, 0.0_wp, 0.0_wp], [0.0_wp, 0.0_wp, 0.0_wp], &
                                    2.0_wp, near, far), 'ray_sphere: missed')
        hit = ray_line([0.0_wp, -5.0_wp, 1.0_wp], [0.0_wp, 1.0_wp, 0.0_wp], [0.0_wp, 0.0_wp, 0.0_wp], &
                       [0.0_wp, 0.0_wp, 3.0_wp], s, p)
        call check(hit .and. abs(s - 1.0_wp) < tol .and. near3(p, [0.0_wp, 0.0_wp, 1.0_wp]), &
                   'ray_line: the closest point of the line')
    end subroutine check_rays

    !> Grab the arrow's tip ([3, 0, 0] in the root) with `constraint`, by the
    !> ray (o0, d0) through it, drag it with the ray (o1, d1): its vector then.
    function dragged_vector(constraint, axis, o0, d0, o1, d1) result(v)
        integer, intent(in) :: constraint                   !! HANDLE_*
        real(wp), intent(in) :: axis(3)                     !! the constraint's normal or direction
        real(wp), intent(in) :: o0(3), d0(3)                !! the ray grabbing it (origin, unit direction)
        real(wp), intent(in) :: o1(3), d1(3)                !! the ray dragging it
        real(wp) :: v(3)
        call arrow%set_vector([1.0_wp, 0.0_wp, 0.0_wp])
        call arrow%set_constraint(constraint, axis)
        v = huge(1.0_wp)
        if (.not. arrow%grab(o0, d0)) return
        if (.not. arrow%drag(o1, d1)) return
        call arrow%release()
        v = arrow%vector()
    end function dragged_vector

    !> Each constraint dragged with given rays, the far side of a sphere, the
    !> grab's offset, and a handle that isn't draggable.
    subroutine check_constraints()
        real(wp), parameter :: x(3) = [1.0_wp, 0.0_wp, 0.0_wp], y(3) = [0.0_wp, 1.0_wp, 0.0_wp], z(3) = [0.0_wp, 0.0_wp, 1.0_wp]
        call check(near3(dragged_vector(HANDLE_FREE, z, [3.0_wp, -5.0_wp, 0.0_wp], y, [3.0_wp, -5.0_wp, 1.0_wp], y), &
                         [1.0_wp, 0.0_wp, 0.5_wp]), 'HANDLE_FREE: in the plane facing the camera')
        call check(near3(dragged_vector(HANDLE_PLANE, z, [3.0_wp, 0.0_wp, 5.0_wp], -z, [3.5_wp, 0.5_wp, 5.0_wp], -z), &
                         [1.25_wp, 0.25_wp, 0.0_wp]), 'HANDLE_PLANE: in the plane given')
        call check(near3(dragged_vector(HANDLE_LINE, y, [6.0_wp, 0.0_wp, 0.0_wp], -x, [6.0_wp, 1.0_wp, 0.0_wp], -x), &
                         [1.0_wp, 0.5_wp, 0.0_wp]), 'HANDLE_LINE: along the line given')
        call check(near3(dragged_vector(HANDLE_RADIAL, z, [3.0_wp, -5.0_wp, 0.0_wp], y, [4.0_wp, -5.0_wp, 0.3_wp], y), &
                         [1.5_wp, 0.0_wp, 0.0_wp]), 'HANDLE_RADIAL: the length only')
        call check(near3(dragged_vector(HANDLE_SPHERE, z, [6.0_wp, 0.0_wp, 0.0_wp], -x, [6.0_wp, 0.0_wp, 1.0_wp], -x), &
                         [0.5_wp * sqrt(3.0_wp), 0.0_wp, 0.5_wp]), 'HANDLE_SPHERE: the direction only, the near side')
        call check(near3(dragged_vector(HANDLE_SPHERE, z, [-4.0_wp, 0.0_wp, 0.0_wp], x, [-4.0_wp, 0.0_wp, 1.0_wp], x), &
                         [0.5_wp * sqrt(3.0_wp), 0.0_wp, 0.5_wp]), 'HANDLE_SPHERE: grabbed on the far side, kept there')
        ! the grab's offset kept: grabbed off the tip, the tip moves as the cursor does
        call check(near3(dragged_vector(HANDLE_PLANE, z, [3.2_wp, 0.0_wp, 5.0_wp], -z, [3.7_wp, 0.0_wp, 5.0_wp], -z), &
                         [1.25_wp, 0.0_wp, 0.0_wp]), 'the offset from the cursor kept')
        call arrow%set_draggable(.false.)
        call check(.not. arrow%grab([3.0_wp, -5.0_wp, 0.0_wp], y), 'not draggable: not grabbed')
        call arrow%set_draggable(.true.)
        call arrow%set_constraint(HANDLE_FREE)
        call arrow%set_vector([1.0_wp, 0.0_wp, 0.0_wp])
    end subroutine check_constraints

    !> The arrow picked at its tip (a PICK_HANDLE hit), not at its tail.
    subroutine check_pick()
        type(PickCamera) :: cam
        type(PickHit), allocatable :: found(:)
        integer :: n
        ! a camera looking at the tip ([3, 0, 0]), the cursor at the center
        cam = pick_camera([3.0_wp, -10.0_wp, 0.0_wp], [3.0_wp, 0.0_wp, 0.0_wp], [0.0_wp, 0.0_wp, 1.0_wp], .true., &
                          30.0_wp, 1.0_wp, [-1.0_wp, 1.0_wp, -1.0_wp, 1.0_wp], 400, 400, 0.0_wp, 0.0_wp, 4.0_wp)
        allocate(found(4))
        n = 0
        call arrow%pick(cam, arrow%world_matrix(), found, n)
        call check(n == 1, 'picked at its tip')
        if (n == 1) call check(found(1)%kind == PICK_HANDLE .and. associated(found(1)%frame, arrow) .and. &
                               near3(found(1)%position, [3.0_wp, 0.0_wp, 0.0_wp]), 'a PICK_HANDLE hit, at the tip')
        ! the cursor off the arrow (the tail: [1, 0, 0])
        cam%x = -2.0_wp / (10.0_wp * tan(15.0_wp * pi / 180.0_wp))
        n = 0
        call arrow%pick(cam, arrow%world_matrix(), found, n)
        call check(n == 0, 'not picked at its tail')
    end subroutine check_pick

    !> TimeHandles: along a line (dragged to a time, kept within its range),
    !> along a circle of three revolutions under a moved parent (the drag
    !> stays on the handle's revolution), picked at its marker; and
    !> TrajectoryPath's interpolation.
    subroutine check_time_handles()
        type(LinePath), target :: line
        type(CirclePath), target :: circle
        type(ReferenceFrame), target :: base, raised
        type(TimeHandle), target :: on_line, on_circle
        type(Trajectory), target :: traj
        type(TrajectoryPath) :: along
        type(PickCamera) :: cam
        type(PickHit), allocatable :: found(:)
        real(wp), parameter :: y(3) = [0.0_wp, 1.0_wp, 0.0_wp], down(3) = [0.0_wp, 0.0_wp, -1.0_wp]
        real(wp), parameter :: two_pi = 2.0_wp * pi
        real(wp) :: t0, t1
        integer :: n

        base = ReferenceFrame('Base')
        on_line = TimeHandle('On a line', line, time=3.0_wp, t_min=0.0_wp, t_max=10.0_wp)
        if (.not. base%add_child(on_line)) error stop 'add_child failed'
        call check(near3(on_line%position(), [3.0_wp, 0.0_wp, 0.0_wp]), 'TimeHandle: placed at its time''s point')
        call check(on_line%grab([3.0_wp, -5.0_wp, 0.0_wp], y), 'TimeHandle: grabbed')
        call check(on_line%drag([7.5_wp, -5.0_wp, 0.0_wp], y), 'TimeHandle: dragged')
        call check(abs(on_line%time() - 7.5_wp) < 1.0e-6_wp .and. near3(on_line%position(), &
                   [on_line%time(), 0.0_wp, 0.0_wp]), 'TimeHandle: the time under the cursor, the frame there')
        call check(on_line%drag([15.0_wp, -5.0_wp, 0.0_wp], y) .and. abs(on_line%time() - 10.0_wp) < 1.0e-6_wp, &
                   'TimeHandle: kept within its time range')
        call on_line%release()
        call on_line%set_time_range(0.0_wp, 4.0_wp)
        call on_line%time_range(t0, t1)
        call check(t0 == 0.0_wp .and. t1 == 4.0_wp .and. on_line%time() == 4.0_wp, 'TimeHandle: a new range clamps it')

        ! a circle of three revolutions, its frame 1 above the root
        raised = ReferenceFrame('Raised')
        call raised%set_position([0.0_wp, 0.0_wp, 1.0_wp])
        on_circle = TimeHandle('On a circle', circle, time=two_pi + 0.5_wp, t_min=0.0_wp, t_max=3.0_wp * two_pi)
        if (.not. (base%add_child(raised) .and. raised%add_child(on_circle))) error stop 'add_child failed'
        call check(on_circle%grab([cos(0.5_wp), sin(0.5_wp), 5.0_wp], down), 'TimeHandle on a circle: grabbed')
        call check(on_circle%drag([cos(0.8_wp), sin(0.8_wp), 5.0_wp], down), 'TimeHandle on a circle: dragged')
        call check(abs(on_circle%time() - (two_pi + 0.8_wp)) < 1.0e-5_wp, &
                   'TimeHandle on a circle: the same revolution (not another one at the same point)')
        call on_circle%release()

        ! picked at its marker: a camera above the circle's point
        cam = pick_camera([cos(0.8_wp), sin(0.8_wp), 11.0_wp], [cos(0.8_wp), sin(0.8_wp), 1.0_wp], y, .true., &
                          30.0_wp, 1.0_wp, [-1.0_wp, 1.0_wp, -1.0_wp, 1.0_wp], 400, 400, 0.0_wp, 0.0_wp, 4.0_wp)
        allocate(found(4))
        n = 0
        call on_circle%pick(cam, on_circle%world_matrix(), found, n)
        call check(n == 1, 'TimeHandle: picked at its marker')
        if (n == 1) call check(found(1)%kind == PICK_HANDLE .and. abs(found(1)%time - on_circle%time()) < 1.0e-12_wp, &
                               'TimeHandle: a PICK_HANDLE hit, with its time')

        ! TrajectoryPath: linear between the points, the ends outside
        traj = Trajectory()
        call traj%add(0.0_wp, pos=[0.0_wp, 0.0_wp, 0.0_wp])
        call traj%add(1.0_wp, pos=[1.0_wp, 0.0_wp, 0.0_wp])
        call traj%add(2.0_wp, pos=[1.0_wp, 1.0_wp, 0.0_wp])
        along = TrajectoryPath(traj)
        call check(near3(along%position(1.5_wp), [1.0_wp, 0.5_wp, 0.0_wp]) .and. &
                   near3(along%position(5.0_wp), [1.0_wp, 1.0_wp, 0.0_wp]) .and. &
                   near3(along%position(-1.0_wp), [0.0_wp, 0.0_wp, 0.0_wp]), 'TrajectoryPath: interpolated, ends kept')
    end subroutine check_time_handles

    !> In a window: the cursor's ray, and the arrow hovered, grabbed, dragged and
    !> released with forwarded mouse events; the camera unchanged; a press
    !> elsewhere the handler's.
    subroutine check_window()
        integer(c_int) :: width, height
        real(wp) :: at(2), before(3,3), after(3,3), v0(3), v1(3)
        integer :: px, py
        scene = FrameManager(root)
        if (.not. window%open(320, 240, 'vkframes: handles test')) then
            write(*, '(a)') 'FAIL: could not open a window'
            error stop 1
        end if
        call window%set_scene(scene)
        front = View(root, root)
        call front%set_default_view_parameters(eye=[2.0_wp, -8.0_wp, 0.0_wp], center=[2.0_wp, 0.0_wp, 0.0_wp], &
                                              up=[0.0_wp, 0.0_wp, 1.0_wp])
        call window%add_view(front)
        call window%set_event_handler(rec)
        call check(window%frame(), 'first frame')
        call check(vkf_viewer_size(window%viewer(), width, height) == 0 .and. width > 0 .and. height > 0, 'window size')
        call check(window%screen_position([3.0_wp, 0.0_wp, 0.0_wp], at), 'the tip on the screen')
        px = nint(at(1) * width)
        py = nint((1.0_wp - at(2)) * height)

        ! the cursor's ray through the tip
        block
            real(wp) :: origin(3), dir(3), t, p(3)
            logical :: ok
            ok = window%cursor_ray(2.0_wp * at(1) - 1.0_wp, 2.0_wp * at(2) - 1.0_wp, origin, dir)
            call check(ok, 'cursor_ray')
            ok = ray_plane(origin, dir, [0.0_wp, 0.0_wp, 0.0_wp], [0.0_wp, 1.0_wp, 0.0_wp], t, p)
            call check(ok .and. norm2(p - [3.0_wp, 0.0_wp, 0.0_wp]) < 1.0e-6_wp, 'the cursor''s ray meets the tip')
        end block

        ! hovered, then grabbed and dragged to the right: the camera stays
        call window%mouse(MOUSE_MOVE, px, py, 0, 0)
        call check(window%frame(), 'frame')
        call check(arrow%hovered(), 'hovered under the cursor')
        call window%pick(2.0_wp * at(1) - 1.0_wp, 2.0_wp * at(2) - 1.0_wp, hits, kinds=[PICK_HANDLE], nearest=.true.)
        call check(size(hits) == 1, 'picked in the window')
        call window%get_camera(before(:, 1), before(:, 2), before(:, 3))
        v0 = arrow%vector()
        call window%mouse(MOUSE_PRESS, px, py, MASK_LEFT, MOUSE_LEFT)
        call check(window%frame(), 'frame')
        call check(arrow%dragged() .and. rec%starts == 1 .and. associated(rec%last, arrow), 'grabbed: DRAG_START')
        call window%mouse(MOUSE_MOVE, px + width / 8, py, MASK_LEFT, 0)
        call check(window%frame(), 'frame')
        v1 = arrow%vector()
        call check(rec%moves >= 1 .and. v1(1) > v0(1) + 0.1_wp, 'dragged to the right: DRAG_MOVE, the vector longer')
        call check(abs(v1(3) - v0(3)) < 1.0e-6_wp, 'in the plane facing the camera (moved sideways: z unchanged)')
        call window%mouse(MOUSE_RELEASE, px + width / 8, py, 0, MOUSE_LEFT)
        call check(window%frame(), 'frame')
        call check(.not. arrow%dragged() .and. rec%ends == 1, 'released: DRAG_END')
        call window%get_camera(after(:, 1), after(:, 2), after(:, 3))
        call check(all(abs(after - before) < 1.0e-9_wp), 'the camera didn''t move')
        call check(rec%button_presses == 0 .and. rec%button_releases == 0, 'the grab''s press and release not the handler''s')

        ! off the handle, a press is the handler's (and the camera's)
        call window%mouse(MOUSE_MOVE, 2, 2, 0, 0)
        call check(window%frame(), 'frame')
        call check(.not. arrow%hovered(), 'not hovered off it')
        call window%mouse(MOUSE_PRESS, 2, 2, MASK_LEFT, MOUSE_LEFT)
        call window%mouse(MOUSE_RELEASE, 2, 2, 0, MOUSE_LEFT)
        call check(window%frame(), 'frame')
        call check(rec%button_presses == 1 .and. rec%starts == 1, 'a press elsewhere: the handler''s, no drag')
        call window%close()
    end subroutine check_window

    !> The points are the same, within 1e-9.
    logical function near3(a, b)
        real(wp), intent(in) :: a(3), b(3)                  !! the points
        near3 = norm2(a - b) < 1.0e-9_wp
    end function near3

    !> A check: counted and reported if it failed.
    subroutine check(ok, what)
        logical, intent(in) :: ok                           !! passed
        character(len=*), intent(in) :: what                !! what is checked
        if (ok) return
        failures = failures + 1
        write(*, '(a)') 'FAIL: '//what
    end subroutine check

end program test_handles
