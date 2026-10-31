!> HUD text: text drawn on top of the scene at a fixed place in a window, as
!> OpenFrames' HUD text (one per RenderRectangle there; any number here).
!>
!> Its position is (x, y) from 0 to 1 across the window, left to right and
!> bottom to top, and the text's alignment (an ALIGN_* constant) says which
!> point of the text is there; its size is in window units (logical pixels).
!> Everything can change at any time, also while shown: the text (e.g. a
!> clock; up to 256 characters, or twice the text's length when first shown
!> if more: longer ones are cut), color, position, size and alignment in
!> place, and the font (the text is made again). Moved each frame to an
!> object's place on the screen (WindowProxy%screen_position), it is a
!> callout that follows the object. A HUDText can be shown in several
!> windows (`WindowProxy%add_hud_text`); it is not owned by them and must
!> stay alive while shown.
module vkf_hud

    use vkf_math, only: wp
    use vkf_frames, only: ALIGN_LEFT_BOTTOM

    implicit none

    private

    !> HUD text (see the module documentation).
    type, public :: HUDText
        private
        character(len=:), allocatable :: text_       !! the text
        real(wp) :: position_(2) = [0.01_wp, 0.01_wp]   !! [x, y] (0 to 1 across the window, y up)
        integer :: alignment_ = ALIGN_LEFT_BOTTOM    !! the point of the text at `position_` (ALIGN_*)
        real(wp) :: size_ = 20.0_wp                  !! character height (window units)
        real(wp) :: color_(4) = [1.0_wp, 1.0_wp, 1.0_wp, 1.0_wp]   !! [r, g, b, a] (0 to 1)
        character(len=:), allocatable :: font_       !! font file (unallocated: the default font)
        logical :: shown = .true.                    !! false while hidden
        integer :: revision_ = 0                     !! incremented by each change
    contains
        procedure :: set_text, text
        procedure :: set_position, position
        procedure :: set_alignment, alignment
        procedure :: set_size, size
        procedure :: set_color, color
        procedure :: set_font, font
        procedure :: show, is_shown
        procedure :: revision
    end type HUDText

    !> The HUDText constructor: `HUDText(...)` makes one (see `new_hud_text` for its
    !> arguments).
    interface HUDText
        module procedure new_hud_text
    end interface HUDText

contains

    !> HUD text `text`, optionally at `position` ([x, y], 0 to 1; default near
    !> the bottom left), with `alignment` (default ALIGN_LEFT_BOTTOM), `size`
    !> (window units; default 20), `color` ([r, g, b] or [r, g, b, a]; default
    !> white) and `font` (default: the default font).
    function new_hud_text(text, position, alignment, size, color, font) result(h)
        character(len=*), intent(in) :: text
        real(wp), intent(in), optional :: position(2)   !! [x, y] (0 to 1 across the window, y up)
        real(wp), intent(in), optional :: size          !! character height (window units)
        real(wp), intent(in), optional :: color(:)      !! [r, g, b] or [r, g, b, a] (0 to 1)
        integer, intent(in), optional :: alignment      !! an ALIGN_* constant
        character(len=*), intent(in), optional :: font  !! font file
        type(HUDText) :: h
        h%text_ = text
        if (present(position)) h%position_ = position
        if (present(alignment)) h%alignment_ = alignment
        if (present(size)) h%size_ = size
        if (present(color)) call h%set_color(color)
        if (present(font)) h%font_ = font
    end function new_hud_text

    !> Set the text (up to 256 characters, or twice the first text's length; see the module documentation).
    subroutine set_text(self, text)
        class(HUDText), intent(inout) :: self
        character(len=*), intent(in) :: text
        self%text_ = text
        self%revision_ = self%revision_ + 1
    end subroutine set_text

    !> The text (as shown: '' while hidden).
    function text(self) result(t)
        class(HUDText), intent(in) :: self
        character(len=:), allocatable :: t
        t = ''
        if (allocated(self%text_) .and. self%shown) t = self%text_
    end function text

    !> Set where the text is.
    subroutine set_position(self, position)
        class(HUDText), intent(inout) :: self
        real(wp), intent(in) :: position(2)     !! [x, y] (0 to 1 across the window, y up)
        self%position_ = position
        self%revision_ = self%revision_ + 1
    end subroutine set_position

    !> Where the text is: [x, y] (0 to 1 across the window, y up).
    pure function position(self) result(p)
        class(HUDText), intent(in) :: self
        real(wp) :: p(2)
        p = self%position_
    end function position

    !> Set which point of the text is at its position.
    subroutine set_alignment(self, alignment)
        class(HUDText), intent(inout) :: self
        integer, intent(in) :: alignment        !! an ALIGN_* constant
        self%alignment_ = alignment
        self%revision_ = self%revision_ + 1
    end subroutine set_alignment

    !> Which point of the text is at its position (an ALIGN_* constant).
    pure integer function alignment(self)
        class(HUDText), intent(in) :: self
        alignment = self%alignment_
    end function alignment

    !> Set the size of the text.
    subroutine set_size(self, size)
        class(HUDText), intent(inout) :: self
        real(wp), intent(in) :: size            !! character height (window units)
        self%size_ = size
        self%revision_ = self%revision_ + 1
    end subroutine set_size

    !> The size of the text: character height (window units).
    pure real(wp) function size(self)
        class(HUDText), intent(in) :: self
        size = self%size_
    end function size

    !> [r, g, b] (alpha unchanged) or [r, g, b, a], 0 to 1.
    subroutine set_color(self, color)
        class(HUDText), intent(inout) :: self
        real(wp), intent(in) :: color(:)
        integer :: n
        n = min(4, ubound(color, 1))
        self%color_(1:n) = color(1:n)
        self%revision_ = self%revision_ + 1
    end subroutine set_color

    !> The color: [r, g, b, a] (0 to 1).
    pure function color(self) result(c)
        class(HUDText), intent(in) :: self
        real(wp) :: c(4)
        c = self%color_
    end function color

    !> Set the font (the text is made again when next drawn).
    subroutine set_font(self, font)
        class(HUDText), intent(inout) :: self
        character(len=*), intent(in) :: font    !! font file
        self%font_ = font
        self%revision_ = self%revision_ + 1
    end subroutine set_font

    !> The font file ('' for the default font).
    function font(self) result(f)
        class(HUDText), intent(in) :: self
        character(len=:), allocatable :: f
        f = ''
        if (allocated(self%font_)) f = self%font_
    end function font

    !> Show (true) or hide the text (OpenFrames' enable HUD text).
    subroutine show(self, shown)
        class(HUDText), intent(inout) :: self
        logical, intent(in) :: shown
        self%shown = shown
        self%revision_ = self%revision_ + 1
    end subroutine show

    !> True if the text is shown.
    pure logical function is_shown(self)
        class(HUDText), intent(in) :: self
        is_shown = self%shown
    end function is_shown

    !> Number of changes so far (for the windows drawing the text to update it).
    pure integer function revision(self)
        class(HUDText), intent(in) :: self
        revision = self%revision_
    end function revision

end module vkf_hud
