!> GUI panels in a window (not in OpenFrames): Dear ImGui panels, buttons,
!> checkboxes and sliders, drawn flat on top of the scene, through vsgImGui.
!>
!> A window's Gui (`WindowProxy%gui()`, made on first use) is described
!> immediate-mode, each frame (e.g. in the event handler's `before_draw`, or
!> in the frame loop before `frame()`): panels (`begin_panel` ... `end_panel`)
!> holding text, buttons, checkboxes, sliders, combo boxes (drop-down
!> menus), separators. A widget's call gives what the user did with it at the
!> frame before: `button` is true once after a click; `checkbox`, `slider`
!> and `combo` change the value given (and are true then). What isn't described in a frame isn't shown in it.
!>
!> A panel is placed where ImGui puts it (and can be moved), or at a position
!> (0 to 1 across the window, y up, as HUD text), at first or every frame
!> (`follow`): e.g. next to a picked object, kept at its screen position
!> (WindowProxy%screen_position). A title starting with '##' gives no title
!> bar (a callout). Clicks, drags and keys over a panel go to it: the camera,
!> the event handler and picking don't get them. The GUI is scaled by the
!> window's pixel ratio. A Gui is a handle (copies are the same GUI), valid
!> while its window is open.
module vkf_gui

    use, intrinsic :: iso_c_binding, only: c_ptr, c_null_ptr, c_associated, c_int, c_double
    use vkf_math, only: wp
    use vkf_layer, only: vkf_gui_begin, vkf_gui_end, vkf_gui_text, vkf_gui_button, vkf_gui_checkbox, vkf_gui_slider, &
                         vkf_gui_separator, vkf_gui_same_line, vkf_gui_combo

    implicit none

    private

    !> A window's GUI panels (see the module documentation).
    type, public :: Gui
        type(c_ptr) :: handle = c_null_ptr      !! the window's GUI in the C++ layer (null: none)
    contains
        procedure :: begin_panel, end_panel
        procedure :: text, button, checkbox, slider, combo, separator, same_line
        procedure :: is_valid
    end type Gui

contains

    !> False if the window has no GUI (not open).
    pure logical function is_valid(self)
        class(Gui), intent(in) :: self
        is_valid = c_associated(self%handle)
    end function is_valid

    !> A panel titled `title` ('##name': no title bar), with the widgets up to
    !> `end_panel`. `position` ([x, y], 0 to 1 across the window, y up; default:
    !> where ImGui puts it) is where its point `pivot` is ([0, 0], the default:
    !> its top left; [0.5, 1]: the middle of its bottom); with `follow`, every
    !> frame (and it can't be moved), else at first.
    subroutine begin_panel(self, title, position, pivot, follow)
        class(Gui), intent(inout) :: self
        character(len=*), intent(in) :: title
        real(wp), intent(in), optional :: position(2)   !! [x, y] (0 to 1 across the window, y up)
        real(wp), intent(in), optional :: pivot(2)      !! the panel's point at `position` (0 to 1, from its top left)
        logical, intent(in), optional :: follow         !! place it every frame (default false: at first)
        real(wp) :: p(2), q(2)
        integer(c_int) :: f
        p = -1.0_wp
        q = 0.0_wp
        f = 0
        if (present(position)) p = position
        if (present(pivot)) q = pivot
        if (present(follow)) f = merge(1_c_int, 0_c_int, follow)
        call vkf_gui_begin(self%handle, title, len(title, kind=c_int), real(p(1), c_double), real(p(2), c_double), &
                           real(q(1), c_double), real(q(2), c_double), f)
    end subroutine begin_panel

    !> End the panel started by `begin_panel`.
    subroutine end_panel(self)
        class(Gui), intent(inout) :: self
        call vkf_gui_end(self%handle)
    end subroutine end_panel

    !> A line of text.
    subroutine text(self, line)
        class(Gui), intent(inout) :: self
        character(len=*), intent(in) :: line
        call vkf_gui_text(self%handle, line, len(line, kind=c_int))
    end subroutine text

    !> A button: true once after it was clicked.
    logical function button(self, label)
        class(Gui), intent(inout) :: self
        character(len=*), intent(in) :: label
        button = vkf_gui_button(self%handle, label, len(label, kind=c_int)) /= 0
    end function button

    !> A checkbox showing `value`: true (and `value` changed) after the user
    !> changed it.
    logical function checkbox(self, label, value) result(changed)
        class(Gui), intent(inout) :: self
        character(len=*), intent(in) :: label
        logical, intent(inout) :: value
        integer(c_int) :: v
        v = merge(1_c_int, 0_c_int, value)
        changed = vkf_gui_checkbox(self%handle, label, len(label, kind=c_int), v) /= 0
        value = v /= 0
    end function checkbox

    !> A slider showing `value`, from `low` to `high`: true (and `value`
    !> changed) after the user moved it. `format`: how the value is shown
    !> (printf's, e.g. '%.0f km'; default '%.4g').
    logical function slider(self, label, value, low, high, format) result(changed)
        class(Gui), intent(inout) :: self
        character(len=*), intent(in) :: label
        real(wp), intent(inout) :: value
        real(wp), intent(in) :: low, high
        character(len=*), intent(in), optional :: format
        real(c_double) :: v
        v = value
        if (present(format)) then
            changed = vkf_gui_slider(self%handle, label, len(label, kind=c_int), v, real(low, c_double), &
                                     real(high, c_double), format, len(format, kind=c_int)) /= 0
        else
            changed = vkf_gui_slider(self%handle, label, len(label, kind=c_int), v, real(low, c_double), &
                                     real(high, c_double), '', 0_c_int) /= 0
        end if
        value = v
    end function slider

    !> A combo box (a drop-down menu) of `items` (trailing blanks trimmed),
    !> showing item `index` (1-based): true (and `index` changed) after the
    !> user chose another.
    logical function combo(self, label, index, items) result(changed)
        class(Gui), intent(inout) :: self
        character(len=*), intent(in) :: label
        integer, intent(inout) :: index
        character(len=*), intent(in) :: items(:)
        character(len=:), allocatable :: all
        integer(c_int) :: i
        integer :: k
        all = ''
        do k = 1, size(items)
            all = all//trim(items(k))
            if (k < size(items)) all = all//new_line('a')
        end do
        i = int(index - 1, c_int)
        changed = vkf_gui_combo(self%handle, label, len(label, kind=c_int), i, all, len(all, kind=c_int)) /= 0
        index = i + 1
    end function combo

    !> A horizontal separator line.
    subroutine separator(self)
        class(Gui), intent(inout) :: self
        call vkf_gui_separator(self%handle)
    end subroutine separator

    !> The next widget on the same line as the last.
    subroutine same_line(self)
        class(Gui), intent(inout) :: self
        call vkf_gui_same_line(self%handle)
    end subroutine same_line

end module vkf_gui
