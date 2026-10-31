!> Checks SimulationClock with explicit wall times: scale, pause, time range,
!> synchronization.
program test_clock

    use vkframes
    use vkf_checks, only: check, report

    implicit none

    type(SimulationClock), target :: c, d, e
    type(SimulationClock), pointer :: pc, pd, pe
    real(wp) :: t, t_min, t_max

    pc => c
    pd => d
    pe => e

    call c%set_time(100.0_wp, wall=0.0_wp)
    t = c%update(wall=2.0_wp)
    call check(t == 102.0_wp, 'scale 1: 2 s later')
    call c%set_time_scale(10.0_wp, wall=2.0_wp)
    t = c%update(wall=3.0_wp)
    call check(t == 112.0_wp .and. c%time_scale() == 10.0_wp, 'scale 10 from the current time')
    call c%pause(.true., wall=3.0_wp)
    t = c%update(wall=10.0_wp)
    call check(t == 112.0_wp .and. c%is_paused(), 'paused')
    call c%pause(.false., wall=10.0_wp)
    t = c%update(wall=11.0_wp)
    call check(t == 122.0_wp, 'resumed from the paused time')
    call c%set_time_range(0.0_wp, 125.0_wp)
    t = c%update(wall=12.0_wp)
    call check(t == 125.0_wp, 'limited to the time range')
    call c%time_range(t_min, t_max)
    call check(t_min == 0.0_wp .and. t_max == 125.0_wp, 'time_range')
    call c%set_time(-5.0_wp, wall=12.0_wp)
    call check(c%time() == 0.0_wp, 'set_time limited to the range')
    call c%set_time_scale(-1.0_wp, wall=12.0_wp)
    call c%set_time(50.0_wp, wall=12.0_wp)
    t = c%update(wall=14.0_wp)
    call check(t == 48.0_wp, 'negative scale runs backwards')

    ! synchronization
    call check(d%synchronize(pc), 'synchronize')
    call check(d%time() == c%time() .and. associated(d%synchronized_to(), pc), 'synchronized time')
    call d%set_time(60.0_wp, wall=14.0_wp)
    call check(c%time() == 60.0_wp, 'setting the time of a synchronized clock sets the master')
    call check(e%synchronize(pd), 'chain')
    call check(.not. c%synchronize(pe), 'cycle refused')
    call check(.not. associated(c%synchronized_to()), 'cycle left the master unsynchronized')
    call check(d%synchronize(null()), 'unsynchronize')
    call check(d%time() == 60.0_wp .and. d%time_scale() == -1.0_wp, 'unsynchronized keeps time and scale')

    call report('SimulationClock')

end program test_clock
