!> Checks font lookup (no window): the default font (DejaVu, installed by pixi
!> in the environment's fonts directory), a missing font, an explicit path.
program test_text

    use vkf_text, only: find_font, default_font
    use vkframes, only: HUDText, wp, ALIGN_LEFT_BOTTOM, ALIGN_RIGHT_TOP

    implicit none

    character(len=:), allocatable :: path
    type(HUDText) :: hud, plain
    integer :: rev
    logical :: exists
    integer :: failures

    failures = 0
    path = find_font(default_font)
    write(*, '(a)') 'default font: '//path
    call check(len(path) > 0, 'the default font is found')
    inquire(file=path, exist=exists)
    call check(exists, 'the path names a file')
    call check(find_font('no-such-font.ttf') == '', 'a missing font gives ''''')
    if (len(path) > 0) call check(find_font(path) == path, 'an explicit path is used as is')

    ! HUD text settings and its change counter
    hud = HUDText('time', position=[0.5_wp, 1.0_wp], alignment=ALIGN_RIGHT_TOP, size=12.0_wp)
    call check(hud%text() == 'time' .and. all(hud%position() == [0.5_wp, 1.0_wp]) .and. &
               hud%alignment() == ALIGN_RIGHT_TOP .and. hud%size() == 12.0_wp, 'HUD text settings')
    plain = HUDText('x')
    call check(all(plain%color() == 1.0_wp) .and. plain%alignment() == ALIGN_LEFT_BOTTOM, 'HUD text defaults')
    rev = hud%revision()
    call hud%set_text('t = 1 s')
    call check(hud%revision() > rev .and. hud%text() == 't = 1 s', 'set_text counts a change')
    call hud%show(.false.)
    call check(hud%text() == '' .and. .not. hud%is_shown(), 'hidden: no text shown')

    if (failures > 0) then
        write(*, '(a,i0,a)') 'FAIL: ', failures, ' checks failed'
        error stop 1
    end if
    write(*, '(a)') 'PASS: fonts, HUD text'

contains

    subroutine check(ok, what)
        logical, intent(in) :: ok
        character(len=*), intent(in) :: what
        if (.not. ok) then
            write(*, '(a)') '  failed: '//what
            failures = failures + 1
        end if
    end subroutine check

end program test_text
