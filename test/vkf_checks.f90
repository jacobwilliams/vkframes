!> What the tests share: their checks counted (`check`: a failed one is
!> reported as it happens) and the result reported at the end (`report`:
!> PASS, or FAIL and an error stop).
module vkf_checks

    implicit none

    private

    integer, public, save :: failures = 0                   !! the checks failed so far

    public :: check, report

contains

    !> A check: counted and reported if it failed.
    subroutine check(ok, what)
        logical, intent(in) :: ok                           !! passed
        character(len=*), intent(in) :: what                !! what is checked
        if (ok) return
        failures = failures + 1
        write(*, '(a)') 'FAIL: '//what
    end subroutine check

    !> The test's result: PASS and what was tested, or FAIL with the number of
    !> checks failed (and an error stop, so the run fails).
    subroutine report(tested)
        character(len=*), intent(in) :: tested              !! what the test tests (the PASS line)
        if (failures > 0) then
            write(*, '(a,i0,a)') 'FAIL: ', failures, ' check(s) failed'
            error stop 1
        end if
        write(*, '(a)') 'PASS: '//tested
    end subroutine report

end module vkf_checks
