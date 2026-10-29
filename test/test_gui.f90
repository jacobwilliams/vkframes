!> A handler counting the button presses it is given (those not taken by a
!> GUI panel).
module test_gui_presses

    use vkframes

    implicit none

    private

    type, extends(WindowEventHandler), public :: Presses
        integer :: count = 0
    contains
        procedure :: button_press => presses_button_press
    end type Presses

contains

    subroutine presses_button_press(self, window, x, y, button)
        class(Presses), intent(inout) :: self
        class(WindowProxy), intent(inout) :: window
        real(wp), intent(in) :: x, y
        integer, intent(in) :: button
        self%count = self%count + 1
    end subroutine presses_button_press

end module test_gui_presses

!> Checks GUI panels (vkf_gui, in windows of their own), with mouse events as
!> a host forwards them: a button in a panel at the window's center is
!> clicked (the panel takes the click: the program's handler doesn't get it,
!> and `button` is then true once); a click away from the panel goes to the
!> handler; a second window has a GUI of its own (its own ImGui context), and
!> both close.
program test_gui

    use, intrinsic :: iso_c_binding, only: c_int
    use vkframes
    use vkf_layer, only: vkf_viewer_size
    use test_gui_presses, only: Presses

    implicit none

    integer, parameter :: MASK_LEFT = 256                   ! (VSG's BUTTON_MASK_1)
    type(ReferenceFrame), target :: root
    type(FrameManager), target :: scene
    type(WindowProxy) :: w1, w2
    type(Presses), target :: handler
    type(Gui) :: g1, g2
    integer(c_int) :: width, height
    integer :: failures, clicks
    logical :: checked
    real(wp) :: amount = 1.0e4_wp
    integer :: choice = 2

    failures = 0
    root = ReferenceFrame('Root')
    scene = FrameManager(root)
    if (.not. (w1%open(400, 300, 'vkframes: GUI test') .and. w2%open(300, 200, 'vkframes: GUI test 2'))) then
        write(*, '(a)') 'FAIL: could not open the windows'
        error stop 1
    end if
    call w1%set_scene(scene)
    call w2%set_scene(scene)
    call w1%set_event_handler(handler)
    clicks = 0
    checked = .false.
    call frames(3)
    g1 = w1%gui()
    g2 = w2%gui()
    call check(g1%is_valid() .and. g2%is_valid(), 'a GUI per window')
    if (vkf_viewer_size(w1%viewer(), width, height) /= 0) error stop 'window size'

    ! a click on the button at the window's center: taken by the panel
    call w1%mouse(MOUSE_MOVE, int(width) / 2, int(height) / 2, 0, 0)
    call frames(2)
    call w1%mouse(MOUSE_PRESS, int(width) / 2, int(height) / 2, MASK_LEFT, MOUSE_LEFT)
    call frames(2)
    call w1%mouse(MOUSE_RELEASE, int(width) / 2, int(height) / 2, 0, MOUSE_LEFT)
    call frames(3)
    call check(clicks == 1, 'the button: clicked once')
    call check(handler%count == 0, 'the click taken by the panel: not the handler''s')

    ! a click away from the panel: the handler's
    call w1%mouse(MOUSE_MOVE, 4, 4, 0, 0)
    call frames(2)
    call w1%mouse(MOUSE_PRESS, 4, 4, MASK_LEFT, MOUSE_LEFT)
    call w1%mouse(MOUSE_RELEASE, 4, 4, 0, MOUSE_LEFT)
    call frames(2)
    call check(handler%count == 1 .and. clicks == 1, 'a click away from the panel: the handler''s')

    call check(choice == 2 .and. abs(amount - 1.0e4_wp) < 1.0e-9_wp, 'an untouched combo box and slider: unchanged')

    ! resized: the panel (kept at the center) still takes a click there (ImGui's
    ! display size the framebuffer's, not the resize event's)
    call w1%resize(int(width) * 3 / 2, int(height) * 3 / 2)
    call frames(3)
    if (vkf_viewer_size(w1%viewer(), width, height) /= 0) error stop 'window size'
    call w1%mouse(MOUSE_MOVE, int(width) / 2, int(height) / 2, 0, 0)
    call frames(2)
    call w1%mouse(MOUSE_PRESS, int(width) / 2, int(height) / 2, MASK_LEFT, MOUSE_LEFT)
    call frames(2)
    call w1%mouse(MOUSE_RELEASE, int(width) / 2, int(height) / 2, 0, MOUSE_LEFT)
    call frames(3)
    call check(clicks == 2, 'resized: the button clicked again')

    ! a click on the panel with no move to it first (the pointer last moved
    ! elsewhere: as after a window zoomed from its title bar on macOS, no moves
    ! come until the pointer moves): the click's own position counts
    call w1%mouse(MOUSE_MOVE, 4, 4, 0, 0)
    call frames(2)
    call w1%mouse(MOUSE_PRESS, int(width) / 2, int(height) / 2, MASK_LEFT, MOUSE_LEFT)
    call frames(2)
    call w1%mouse(MOUSE_RELEASE, int(width) / 2, int(height) / 2, 0, MOUSE_LEFT)
    call frames(3)
    call check(clicks == 3 .and. handler%count == 1, 'a click with no move first: the panel''s')

    ! the same with the left button numbered 0 (as VSG's own window on macOS gives it)
    call w1%mouse(MOUSE_MOVE, 4, 4, 0, 0)
    call frames(2)
    call w1%mouse(MOUSE_PRESS, int(width) / 2, int(height) / 2, MASK_LEFT, 0)
    call frames(2)
    call w1%mouse(MOUSE_RELEASE, int(width) / 2, int(height) / 2, 0, 0)
    call frames(3)
    call check(clicks == 4 .and. handler%count == 1, 'a click of button 0 (macOS''s left) with no move first: the panel''s')

    call w2%close()
    call w1%close()
    if (failures > 0) then
        write(*, '(a,i0,a)') 'FAIL: ', failures, ' checks failed'
        error stop 1
    end if
    write(*, '(a)') 'PASS: GUI panels (a button clicked, also after a resize and with no move first; events taken; ' // &
                    'a GUI per window; a slider, a combo box)'

contains

    !> n frames of both windows, each describing its panels first.
    subroutine frames(n)
        integer, intent(in) :: n
        type(Gui) :: g
        integer :: i
        do i = 1, n
            g = w1%gui()
            call g%begin_panel('##test', position=[0.5_wp, 0.5_wp], pivot=[0.5_wp, 0.5_wp], follow=.true.)
            if (g%button('press me')) clicks = clicks + 1
            call g%end_panel()
            g = w2%gui()
            call g%begin_panel('Second window')
            if (g%checkbox('a checkbox', checked)) continue
            if (g%slider('a slider', amount, 0.0_wp, 5.0e4_wp, format='%.0f km')) continue
            if (g%combo('a menu', choice, [character(len=8) :: 'L1', 'L2', 'L3'])) continue
            call g%end_panel()
            if (.not. (w1%frame() .and. w2%frame())) then
                write(*, '(a)') 'FAIL: a window closed'
                error stop 1
            end if
        end do
    end subroutine frames

    subroutine check(ok, what)
        logical, intent(in) :: ok
        character(len=*), intent(in) :: what
        if (.not. ok) then
            write(*, '(a)') 'FAIL: '//what
            failures = failures + 1
        end if
    end subroutine check

end program test_gui
