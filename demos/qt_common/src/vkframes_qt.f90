!> What the vkframes demos in qt-fortran GUIs share (vkframes itself doesn't
!> depend on Qt): Qt's mouse, wheel and key events turned into VSG's for a
!> WindowProxy rendering into a QWindow (`forward_mouse`, `forward_wheel`,
!> `forward_key`; the conversions `vsg_button`, `vsg_mask`, `vsg_key`), and
!> the window's native surface going away (`watch_surface`).
!>
!> Keys: VSG's key symbols are X11 keysyms. Letters are lower case without
!> Shift (the modified symbol upper case with it); other printable keys are
!> their character (the modified symbol the character typed, e.g. '!' for
!> Shift+1); the keypad's keys (Qt_KeypadModifier) are VSG's KEY_KP_*; the
!> editing, function (F1 to F20), modifier and lock keys their keysyms. On
!> macOS Qt reports Command as Control and Control as Meta, as VSG's own
!> macOS window does.
module vkframes_qt

    use qt
    use vkframes, only: wp, WindowProxy, MOUSE_MOVE, MOUSE_PRESS, MOUSE_RELEASE

    implicit none

    private

    public :: vsg_button, vsg_mask, vsg_key
    public :: forward_mouse, forward_wheel, forward_key
    public :: watch_surface

    ! VSG's modifier bits (vsg::KeyModifier)
    integer, parameter :: MODKEY_SHIFT = 1, MODKEY_CONTROL = 4, MODKEY_ALT = 8, MODKEY_META = 128

contains

    !> VSG's button number (1 left, 2 middle, 3 right; 0 other) for a Qt button.
    pure integer function vsg_button(qt_button)
        integer, intent(in) :: qt_button
        select case (qt_button)
        case (Qt_LeftButton)
            vsg_button = 1
        case (Qt_MiddleButton)
            vsg_button = 2
        case (Qt_RightButton)
            vsg_button = 3
        case default
            vsg_button = 0
        end select
    end function vsg_button

    !> VSG's mask of the buttons held (256 left, 512 middle, 1024 right) for Qt's.
    pure integer function vsg_mask(qt_buttons)
        integer, intent(in) :: qt_buttons
        vsg_mask = 0
        if (iand(qt_buttons, Qt_LeftButton) /= 0) vsg_mask = ior(vsg_mask, 256)
        if (iand(qt_buttons, Qt_MiddleButton) /= 0) vsg_mask = ior(vsg_mask, 512)
        if (iand(qt_buttons, Qt_RightButton) /= 0) vsg_mask = ior(vsg_mask, 1024)
    end function vsg_mask

    !> VSG's key symbol (without modifiers), modified symbol and modifier bits
    !> for a Qt key event; symbol 0 for a key VSG has no symbol for.
    subroutine vsg_key(event, symbol, modified_symbol, modifier)
        type(QKeyEvent), intent(in) :: event
        integer, intent(out) :: symbol, modified_symbol, modifier
        character(len=:), allocatable :: text
        integer :: key, mods
        key = event%key()
        mods = event%modifiers()
        modifier = 0
        if (iand(mods, Qt_ShiftModifier) /= 0) modifier = ior(modifier, MODKEY_SHIFT)
        if (iand(mods, Qt_ControlModifier) /= 0) modifier = ior(modifier, MODKEY_CONTROL)
        if (iand(mods, Qt_AltModifier) /= 0) modifier = ior(modifier, MODKEY_ALT)
        if (iand(mods, Qt_MetaModifier) /= 0) modifier = ior(modifier, MODKEY_META)
        if (iand(mods, Qt_KeypadModifier) /= 0) then
            symbol = keypad_symbol(key)
            if (symbol /= 0) then
                modified_symbol = symbol
                return
            end if
        end if
        symbol = special_symbol(key)
        if (symbol /= 0) then
            modified_symbol = symbol
            return
        end if
        if (key >= 32 .and. key < 127) then                 ! a printable character (Qt's key: its code, letters upper case)
            symbol = key
            if (key >= Qt_Key_A .and. key <= Qt_Key_Z) symbol = key - Qt_Key_A + iachar('a')
            modified_symbol = symbol
            text = event%text()
            if (len(text) == 1) then
                if (iachar(text) >= 32 .and. iachar(text) < 127) modified_symbol = iachar(text)
            end if
            if (key >= Qt_Key_A .and. key <= Qt_Key_Z .and. iand(modifier, MODKEY_SHIFT) /= 0) modified_symbol = key
            return
        end if
        symbol = 0
        modified_symbol = 0
    end subroutine vsg_key

    !> VSG's keysym of a non-printing Qt key (0 if none).
    pure integer function special_symbol(key) result(symbol)
        integer, intent(in) :: key
        select case (key)
        case (Qt_Key_Escape)
            symbol = int(z'FF1B')
        case (Qt_Key_Tab, Qt_Key_Backtab)                  ! (Backtab: Shift+Tab)
            symbol = int(z'FF09')
        case (Qt_Key_Backspace)
            symbol = int(z'FF08')
        case (Qt_Key_Return)
            symbol = int(z'FF0D')
        case (Qt_Key_Enter)
            symbol = int(z'FF8D')
        case (Qt_Key_Insert)
            symbol = int(z'FF63')
        case (Qt_Key_Delete)
            symbol = int(z'FFFF')
        case (Qt_Key_Pause)
            symbol = int(z'FF13')
        case (Qt_Key_Print)
            symbol = int(z'FF61')
        case (Qt_Key_SysReq)
            symbol = int(z'FF15')
        case (Qt_Key_Clear)
            symbol = int(z'FF0B')
        case (Qt_Key_Home)
            symbol = int(z'FF50')
        case (Qt_Key_End)
            symbol = int(z'FF57')
        case (Qt_Key_Left)
            symbol = int(z'FF51')
        case (Qt_Key_Up)
            symbol = int(z'FF52')
        case (Qt_Key_Right)
            symbol = int(z'FF53')
        case (Qt_Key_Down)
            symbol = int(z'FF54')
        case (Qt_Key_PageUp)
            symbol = int(z'FF55')
        case (Qt_Key_PageDown)
            symbol = int(z'FF56')
        case (Qt_Key_Shift)
            symbol = int(z'FFE1')
        case (Qt_Key_Control)
            symbol = int(z'FFE3')
        case (Qt_Key_Meta)
            symbol = int(z'FFE7')
        case (Qt_Key_Alt)
            symbol = int(z'FFE9')
        case (Qt_Key_CapsLock)
            symbol = int(z'FFE5')
        case (Qt_Key_NumLock)
            symbol = int(z'FF7F')
        case (Qt_Key_ScrollLock)
            symbol = int(z'FF14')
        case (Qt_Key_Menu)
            symbol = int(z'FF67')
        case (Qt_Key_Help)
            symbol = int(z'FF6A')
        case (Qt_Key_F1:Qt_Key_F20)                         ! (consecutive in both)
            symbol = int(z'FFBE') + (key - Qt_Key_F1)
        case default
            symbol = 0
        end select
    end function special_symbol

    !> VSG's keysym of a keypad key (0 if none: e.g. its arrows, as the others).
    pure integer function keypad_symbol(key) result(symbol)
        integer, intent(in) :: key
        select case (key)
        case (Qt_Key_0:Qt_Key_9)
            symbol = int(z'FFB0') + (key - Qt_Key_0)
        case (Qt_Key_Asterisk)
            symbol = int(z'FFAA')
        case (Qt_Key_Plus)
            symbol = int(z'FFAB')
        case (Qt_Key_Minus)
            symbol = int(z'FFAD')
        case (Qt_Key_Period)
            symbol = int(z'FFAE')
        case (Qt_Key_Slash)
            symbol = int(z'FFAF')
        case (Qt_Key_Equal)
            symbol = int(z'FFBD')
        case (Qt_Key_Enter)
            symbol = int(z'FF8D')
        case default
            symbol = 0
        end select
    end function keypad_symbol

    !> A Qt mouse event of `window` to `proxy` (kind MOUSE_MOVE, MOUSE_PRESS
    !> or MOUSE_RELEASE), in framebuffer pixels; accepted.
    subroutine forward_mouse(proxy, window, kind, event)
        type(WindowProxy), intent(inout) :: proxy
        type(QWindow), intent(in) :: window
        integer, intent(in) :: kind
        type(QMouseEvent), intent(in) :: event
        real(wp) :: dpr
        dpr = window%devicePixelRatio()
        call proxy%mouse(kind, nint(event%x() * dpr), nint(event%y() * dpr), vsg_mask(event%buttons()), &
                         vsg_button(event%button()))
        call event%accept()
    end subroutine forward_mouse

    !> A Qt wheel event to `proxy`, in wheel notches (a trackpad's are
    !> fractions); accepted (ignored if it has no vertical part).
    subroutine forward_wheel(proxy, event)
        type(WindowProxy), intent(inout) :: proxy
        type(QWheelEvent), intent(in) :: event
        if (event%angleDeltaY() == 0) return
        call proxy%scroll(event%angleDeltaY() / 120.0_wp)
        call event%accept()
    end subroutine forward_wheel

    !> A Qt key press (`pressed`) or release to `proxy`, as VSG's key symbols;
    !> accepted, unless VSG has no symbol for the key.
    subroutine forward_key(proxy, pressed, event)
        type(WindowProxy), intent(inout) :: proxy
        logical, intent(in) :: pressed
        type(QKeyEvent), intent(in) :: event
        integer :: symbol, modified_symbol, modifier
        call vsg_key(event, symbol, modified_symbol, modifier)
        if (symbol == 0) return
        call proxy%key(pressed, symbol, modified_symbol, modifier)
        call event%accept()
    end subroutine forward_key

    !> Close `proxy` when the window `events` filters is about to lose its
    !> native surface (Qt destroying it: re-parenting, `destroy`, the window
    !> deleted), while the surface still exists. The host opens it again on
    !> the window's next expose (as at first), with the proxy's views, HUD
    !> texts and scene as before.
    subroutine watch_surface(events, proxy)
        type(QtfEventFilter), intent(in) :: events
        type(WindowProxy), intent(inout), target :: proxy
        call events%surfaceAboutToBeDestroyed%connect(on_surface_destroyed, proxy)
    end subroutine watch_surface

    !> The surface is going: stop rendering into it (waiting for the GPU).
    subroutine on_surface_destroyed(data)
        class(*), intent(inout), optional :: data
        select type (data)
        type is (WindowProxy)
            call data%close()
        end select
    end subroutine on_surface_destroyed

end module vkframes_qt
