!> Checks TrajectoryFollower: interpolation (linear position, slerp attitude),
!> FOLLOW_LOOP / FOLLOW_LIMIT, choosing among trajectories, fixed and offset
!> times, data sources.
program test_follower

    use vkframes

    implicit none

    type(Trajectory), target :: a, b
    type(Trajectory), pointer :: pa, pb
    type(TrajectoryFollower) :: f
    real(wp) :: pos(3), att(4)
    real(wp), parameter :: z(3) = [0.0_wp, 0.0_wp, 1.0_wp], tol = 1.0e-12_wp
    logical :: hp, ha
    integer :: failures

    failures = 0
    a = Trajectory()
    call a%add(0.0_wp, pos=[0.0_wp, 0.0_wp, 0.0_wp], att=quat_identity)
    call a%add(10.0_wp, pos=[10.0_wp, 20.0_wp, 30.0_wp], att=quat_from_axis_angle(z, 0.5_wp * pi))
    b = Trajectory()
    call b%add(20.0_wp, pos=[100.0_wp, 0.0_wp, 0.0_wp])
    call b%add(30.0_wp, pos=[200.0_wp, 0.0_wp, 0.0_wp])
    pa => a
    pb => b

    call f%set_trajectory(pa)
    call f%update(5.0_wp, pos, att, hp, ha)
    call check(hp .and. ha, 'position and attitude computed')
    call check(all(abs(pos - [5.0_wp, 10.0_wp, 15.0_wp]) < tol), 'linear position')
    call check(all(abs(att - quat_from_axis_angle(z, 0.25_wp * pi)) < tol), 'slerp: halfway is 45 degrees')

    call f%update(15.0_wp, pos, att, hp, ha)
    call check(all(abs(pos - [5.0_wp, 10.0_wp, 15.0_wp]) < tol) .and. f%last_time() == 5.0_wp, 'LOOP: 15 wraps to 5')
    call f%update(-2.5_wp, pos, att, hp, ha)
    call check(abs(f%last_time() - 7.5_wp) < tol, 'LOOP: -2.5 wraps to 7.5')

    call f%set_follow_type(FOLLOW_POSITION, FOLLOW_LIMIT)
    att = 0.0_wp
    call f%update(15.0_wp, pos, att, hp, ha)
    call check(all(pos == [10.0_wp, 20.0_wp, 30.0_wp]), 'LIMIT: last point after the end')
    call check(.not. ha .and. all(att == 0.0_wp), 'position only: attitude untouched')
    call f%update(-5.0_wp, pos, att, hp, ha)
    call check(all(pos == 0.0_wp), 'LIMIT: first point before the start')

    ! two trajectories, [0, 10] and [20, 30]
    call f%add_trajectory(pb)
    call check(f%num_trajectories() == 2, 'two trajectories')
    call f%update(25.0_wp, pos, att, hp, ha)
    call check(all(abs(pos - [150.0_wp, 0.0_wp, 0.0_wp]) < tol) .and. associated(f%last_trajectory(), pb), &
               'the trajectory containing the time')
    call f%update(17.0_wp, pos, att, hp, ha)
    call check(all(pos == [100.0_wp, 0.0_wp, 0.0_wp]), 'in the gap: the closest trajectory (start of b)')
    call f%update(12.0_wp, pos, att, hp, ha)
    call check(all(pos == [10.0_wp, 20.0_wp, 30.0_wp]), 'in the gap: the closest trajectory (end of a)')
    call f%set_follow_type(FOLLOW_POSITION, FOLLOW_LOOP)
    call f%update(35.0_wp, pos, att, hp, ha)
    call check(abs(f%last_time() - 5.0_wp) < tol .and. associated(f%last_trajectory(), pa), &
               'LOOP over the combined range [0, 30]')
    call f%remove_trajectory(pb)
    call check(f%num_trajectories() == 1, 'remove_trajectory')

    ! fixed and offset times
    call f%set_time(2.0_wp)
    call f%update(100.0_wp, pos, att, hp, ha)
    call check(all(abs(pos - [2.0_wp, 4.0_wp, 6.0_wp]) < tol) .and. .not. f%is_following_time(), 'fixed time')
    call f%set_offset_time(1.0_wp)
    call f%update(3.0_wp, pos, att, hp, ha)
    call check(all(abs(pos - [4.0_wp, 8.0_wp, 12.0_wp]) < tol) .and. f%is_following_time(), 'offset time')

    ! data sources: x from the time (scaled), z from position y
    call check(f%set_x_data(DataSource(SOURCE_TIME, scale=2.0_wp)), 'set_x_data valid')
    call check(f%set_z_data(DataSource(SOURCE_POSOPT, element=2)), 'set_z_data valid')
    call f%update(3.0_wp, pos, att, hp, ha)
    call check(all(abs(pos - [8.0_wp, 8.0_wp, 8.0_wp]) < tol) .and. .not. f%using_default_data(), 'custom data sources')
    call check(.not. f%set_y_data(DataSource(SOURCE_POSOPT, element=1, opt=1)), 'invalid source (no optionals)')
    call f%set_default_data()
    call f%update(3.0_wp, pos, att, hp, ha)
    call check(all(abs(pos - [4.0_wp, 8.0_wp, 12.0_wp]) < tol), 'default data sources again')

    if (failures > 0) then
        write(*, '(a,i0,a)') 'FAIL: ', failures, ' checks failed'
        error stop 1
    end if
    write(*, '(a)') 'PASS: TrajectoryFollower'

contains

    subroutine check(ok, what)
        logical, intent(in) :: ok
        character(len=*), intent(in) :: what
        if (.not. ok) then
            write(*, '(a)') '  failed: '//what
            failures = failures + 1
        end if
    end subroutine check

end program test_follower
