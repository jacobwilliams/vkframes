!> Checks font lookup (no window): the default font (DejaVu, installed by pixi
!> in the environment's fonts directory), a missing font, an explicit path.
program test_text

    use vkf_text, only: find_font, default_font

    implicit none

    character(len=:), allocatable :: path
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

    if (failures > 0) then
        write(*, '(a,i0,a)') 'FAIL: ', failures, ' checks failed'
        error stop 1
    end if
    write(*, '(a)') 'PASS: fonts'

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
