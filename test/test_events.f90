!> A program's handler of a window's keyboard and mouse events: a
!> WindowEventHandler recording what it is given.
module test_events_recorder

    use vkframes

    implicit none

    private

    type, extends(WindowEventHandler), public :: Recorder
        integer :: presses = 0, releases = 0, motions = 0, button_presses = 0, button_releases = 0
        ! the motions since `motions` was reset (the real pointer's too, if it moves over the window)
        real(wp) :: motion_xy(2, 64) = 0.0_wp
        logical :: motion_buttons(3, 64) = .false.
        integer :: key = 0, modifiers = 0, button = 0
        real(wp) :: x = huge(1.0_wp), y = huge(1.0_wp)
        logical :: buttons(3) = .false.
        integer :: swallow = -1                             !! a key to say handled
        integer :: draws = 0                                !! before_draw calls
        real(wp) :: draw_time = -1.0_wp
        logical :: on_screen = .false.                      !! the root's origin on the screen, in before_draw
        real(wp) :: at(2) = -1.0_wp
        integer :: close_on = -1                            !! a key to close the window on
    contains
        procedure :: key_press => recorder_key_press
        procedure :: key_release => recorder_key_release
        procedure :: mouse_motion => recorder_mouse_motion
        procedure :: button_press => recorder_button_press
        procedure :: button_release => recorder_button_release
        procedure :: before_draw => recorder_before_draw
    end type Recorder

contains

    subroutine recorder_key_press(self, window, key, modifiers, handled)
        class(Recorder), intent(inout) :: self
        class(WindowProxy), intent(inout) :: window
        integer, intent(in) :: key, modifiers
        logical, intent(inout) :: handled
        self%presses = self%presses + 1
        self%key = key
        self%modifiers = modifiers
        handled = key == self%swallow
        if (key == self%close_on) call window%close()
    end subroutine recorder_key_press

    subroutine recorder_key_release(self, window, key, modifiers)
        class(Recorder), intent(inout) :: self
        class(WindowProxy), intent(inout) :: window
        integer, intent(in) :: key, modifiers
        self%releases = self%releases + 1
        self%key = key
        self%modifiers = modifiers
    end subroutine recorder_key_release

    subroutine recorder_mouse_motion(self, window, x, y, buttons)
        class(Recorder), intent(inout) :: self
        class(WindowProxy), intent(inout) :: window
        real(wp), intent(in) :: x, y
        logical, intent(in) :: buttons(3)
        self%motions = self%motions + 1
        self%x = x
        self%y = y
        self%buttons = buttons
        if (self%motions <= size(self%motion_xy, 2)) then
            self%motion_xy(:, self%motions) = [x, y]
            self%motion_buttons(:, self%motions) = buttons
        end if
    end subroutine recorder_mouse_motion

    subroutine recorder_button_press(self, window, x, y, button)
        class(Recorder), intent(inout) :: self
        class(WindowProxy), intent(inout) :: window
        real(wp), intent(in) :: x, y
        integer, intent(in) :: button
        self%button_presses = self%button_presses + 1
        self%x = x
        self%y = y
        self%button = button
    end subroutine recorder_button_press

    subroutine recorder_button_release(self, window, x, y, button)
        class(Recorder), intent(inout) :: self
        class(WindowProxy), intent(inout) :: window
        real(wp), intent(in) :: x, y
        integer, intent(in) :: button
        self%button_releases = self%button_releases + 1
        self%x = x
        self%y = y
        self%button = button
    end subroutine recorder_button_release

    subroutine recorder_before_draw(self, window, t)
        class(Recorder), intent(inout) :: self
        class(WindowProxy), intent(inout) :: window
        real(wp), intent(in) :: t
        self%draws = self%draws + 1
        self%draw_time = t
        self%on_screen = window%screen_position([0.0_wp, 0.0_wp, 0.0_wp], self%at)
    end subroutine recorder_before_draw

end module test_events_recorder

!> Checks a window's keyboard and mouse events (WindowEventHandler, OpenFrames'
!> callbacks) and the built-in keys (v, V, the space bar), with events given
!> as a host forwards them (WindowProxy%key, %mouse), in a window of its own.
program test_events

    use, intrinsic :: iso_c_binding, only: c_int
    use vkframes
    use vkf_layer, only: vkf_viewer_size
    use test_events_recorder, only: Recorder

    implicit none

    integer, parameter :: MASK_LEFT = 256                   ! (VSG's BUTTON_MASK_1)
    type(ReferenceFrame), target :: root, craft
    type(FrameManager), target :: scene
    type(WindowProxy) :: window
    type(View), target :: view_a, view_b
    type(Recorder), target :: rec
    integer(c_int) :: width, height
    integer :: failures

    failures = 0
    root = ReferenceFrame('Root')
    craft = ReferenceFrame('Craft')
    call craft%set_position([1.0_wp, 0.0_wp, 0.0_wp])
    if (.not. root%add_child(craft)) error stop 'add_child failed'
    scene = FrameManager(root)
    if (.not. window%open(320, 240, 'vkframes: events test')) then
        write(*, '(a)') 'FAIL: could not open a window'
        error stop 1
    end if
    call window%set_scene(scene)
    view_a = View(root, root)
    view_b = View(root, craft)
    call window%add_view(view_a)
    call window%add_view(view_b)
    call check(window%frame(), 'first frame')
    call check(vkf_viewer_size(window%viewer(), width, height) == 0 .and. width > 0 .and. height > 0, 'window size')

    ! the built-in keys, without a handler: v, V (shift), the space bar
    call press(iachar('v'), iachar('v'), 0)
    call check(associated(window%current_view(), view_b), 'v: the next view')
    call press(iachar('v'), iachar('V'), MOD_SHIFT)
    call check(associated(window%current_view(), view_a), 'V: the previous view')
    call press(iachar('v'), iachar('v'), MOD_CONTROL)
    call check(associated(window%current_view(), view_a), 'control-v: not a built-in key')

    ! the handler: keys, with the built-in keys still acting
    call window%set_event_handler(rec)
    call press(iachar('v'), iachar('v'), 0)
    call check(rec%presses == 1 .and. rec%releases == 1 .and. rec%key == iachar('v') .and. rec%modifiers == 0, &
               'the handler is given the key press and release')
    call check(associated(window%current_view(), view_b), 'with a handler, v still shows the next view')
    call press(KEY_LEFT, KEY_LEFT, MOD_ALT)
    call check(rec%key == KEY_LEFT .and. rec%modifiers == MOD_ALT, 'a special key, with its modifiers')

    ! a key the handler says it handled: no built-in action; built-in keys off
    rec%swallow = iachar('v')
    call press(iachar('v'), iachar('v'), 0)
    call check(associated(window%current_view(), view_b), 'handled: no built-in action')
    rec%swallow = -1
    call window%set_default_keys(.false.)
    call press(iachar('v'), iachar('v'), 0)
    call check(associated(window%current_view(), view_b) .and. rec%key == iachar('v'), 'built-in keys off: only the handler')
    call window%set_default_keys(.true.)

    ! the mouse: positions -1 to 1 across the window, y up
    call window%mouse(MOUSE_PRESS, 0, 0, MASK_LEFT, MOUSE_LEFT)
    call check(window%frame(), 'frame')
    call check(rec%button_presses == 1 .and. rec%button == MOUSE_LEFT .and. &
               near(rec%x, -1.0_wp) .and. near(rec%y, 1.0_wp), 'button press at the top left: (-1, 1)')
    ! (the real pointer over the window can add motions: the ones forwarded are looked for among them)
    rec%motions = 0
    call window%mouse(MOUSE_MOVE, int(width) / 2, int(height) / 2, MASK_LEFT, 0)
    call check(window%frame(), 'frame')
    call check(saw_motion(0.0_wp, 0.0_wp, [.true., .false., .false.]), &
               'dragging at the center: (0, 0), the left button held')
    rec%motions = 0
    call window%mouse(MOUSE_RELEASE, int(width), int(height), 0, MOUSE_LEFT)
    call window%mouse(MOUSE_MOVE, int(width), int(height), 0, 0)
    call check(window%frame(), 'frame')
    call check(rec%button_releases == 1 .and. saw_motion(1.0_wp, -1.0_wp, [.false., .false., .false.]), &
               'release and move at the bottom right: (1, -1), no buttons')

    ! cleared: no more events to the handler
    call window%clear_event_handler()
    call press(iachar('x'), iachar('x'), 0)
    call check(rec%key /= iachar('x') .and. .not. associated(window%event_handler()), 'cleared handler')

    ! before_draw: once a frame, at the frame's time, with the camera placed
    ! (the default view looks at the root's origin: the window's center)
    call window%set_event_handler(rec)
    call window%select_view(1)
    call window%pause_time(.true.)
    call window%set_time(12.5_wp)
    rec%draws = 0
    call check(window%frame() .and. window%frame(), 'frames')
    call check(rec%draws == 2 .and. rec%draw_time == 12.5_wp, 'before_draw: each frame, at its time')
    call check(rec%on_screen .and. all(abs(rec%at - 0.5_wp) < 1.0e-6_wp), 'before_draw: screen_position, camera placed')

    ! the handler closes the window: the frame reports it closed
    call window%set_event_handler(rec)
    rec%close_on = KEY_ESCAPE
    call window%key(.true., KEY_ESCAPE, KEY_ESCAPE, 0)
    call check(.not. window%frame(), 'closed by the handler: frame() is false')
    call check(.not. window%is_open(), 'closed by the handler')

    if (failures > 0) then
        write(*, '(a,i0,a)') 'FAIL: ', failures, ' checks failed'
        error stop 1
    end if
    write(*, '(a)') 'PASS: window events (WindowEventHandler), built-in keys'

contains

    !> A motion to (x, y) with these buttons held was given to the handler
    !> since its motions were counted from 0.
    logical function saw_motion(x, y, buttons) result(saw)
        real(wp), intent(in) :: x, y
        logical, intent(in) :: buttons(3)
        integer :: k
        saw = .false.
        do k = 1, min(rec%motions, size(rec%motion_xy, 2))
            if (near(rec%motion_xy(1, k), x) .and. near(rec%motion_xy(2, k), y) .and. &
                all(rec%motion_buttons(:, k) .eqv. buttons)) saw = .true.
        end do
    end function saw_motion

    !> A key pressed and released, then a frame (the events passed on).
    subroutine press(symbol, modified, modifier)
        integer, intent(in) :: symbol, modified, modifier
        logical :: running
        call window%key(.true., symbol, modified, modifier)
        call window%key(.false., symbol, modified, modifier)
        running = window%frame()
        call check(running, 'frame')
    end subroutine press

    logical function near(a, b)
        real(wp), intent(in) :: a, b
        near = abs(a - b) < 0.02_wp
    end function near

    subroutine check(ok, what)
        logical, intent(in) :: ok
        character(len=*), intent(in) :: what
        if (.not. ok) then
            write(*, '(a)') 'FAIL: '//what
            failures = failures + 1
        end if
    end subroutine check

end program test_events
