!> Checks Trajectory: data with fewer positions/attitudes than times, optionals,
!> time search (increasing and decreasing times), data sources, growth, change
!> counters.
program test_trajectory

    use vkframes

    implicit none

    type(Trajectory), target :: traj, down, big, flat
    type(DataSource) :: src(3)
    real(wp), pointer :: p(:,:)
    real(wp), allocatable :: t(:), pos(:,:)
    real(wp) :: t0, tf
    real(wp), allocatable :: v(:,:)
    logical :: ok
    integer :: i, index, status, failures, rev, gen

    failures = 0

    ! empty
    traj = Trajectory()
    call check(traj%is_empty() .and. traj%num_times() == 0, 'empty')
    call check(traj%time_index(1.0_wp, index) == TIME_OUT_OF_RANGE, 'empty: time_index')
    call check(traj%time_distance(1.0_wp) == huge(1.0_wp), 'empty: time_distance')
    call check(.not. traj%time_range(t0, tf), 'empty: no time range')

    ! 10 times, 8 positions, 5 attitudes
    do i = 0, 9
        call traj%add_time(real(i, wp))
        if (i < 8) call traj%add_position([real(i, wp), 2.0_wp * i, 3.0_wp * i])
        if (i < 5) call traj%add_attitude(quat_identity)
    end do
    call check(traj%num_times() == 10 .and. traj%num_positions() == 8 .and. traj%num_attitudes() == 5, 'counts')
    call check(all(traj%position(4) == [3.0_wp, 6.0_wp, 9.0_wp]), 'position(4)')
    call check(traj%num_points(position_sources(3)) == 8, 'num_points: positions')
    src = [DataSource(SOURCE_TIME), DataSource(SOURCE_ATTITUDE, element=4), DataSource(SOURCE_ZERO)]
    call check(traj%num_points(src) == 5, 'num_points: attitudes')
    src = DataSource(SOURCE_ZERO)
    call check(traj%num_points(src) == huge(0), 'num_points: all zero')
    call check(traj%time_range(t0, tf) .and. t0 == 0.0_wp .and. tf == 9.0_wp, 'time_range')

    ! a position for each time at most
    call traj%add_position([1.0_wp, 1.0_wp, 1.0_wp], ok)
    call traj%add_position([1.0_wp, 1.0_wp, 1.0_wp], ok)
    call check(ok .and. traj%num_positions() == 10, 'positions up to the number of times')
    call traj%add_position([1.0_wp, 1.0_wp, 1.0_wp], ok)
    call check(.not. ok .and. traj%num_positions() == 10, 'no more positions than times')

    ! time search, increasing
    status = traj%time_index(3.5_wp, index)
    call check(status > 0 .and. index == 4, 'time_index inside: times(4) = 3 <= 3.5 < 4')
    status = traj%time_index(3.0_wp, index)
    call check(status > 0 .and. index == 4, 'time_index at a time: lower bound')
    call check(traj%time_index(0.0_wp, index) == TIME_AT_BOUND .and. index == 1, 'time_index at the first time')
    call check(traj%time_index(9.0_wp, index) == TIME_AT_BOUND .and. index == 10, 'time_index at the last time')
    call check(traj%time_index(-1.0_wp, index) == TIME_OUT_OF_RANGE .and. index == 0, 'time_index before')
    call check(traj%time_index(10.0_wp, index) == TIME_OUT_OF_RANGE .and. index == 11, 'time_index after')
    call check(traj%time_distance(4.0_wp) < 0.0_wp .and. traj%time_distance(12.0_wp) == 3.0_wp, 'time_distance')

    ! time search, decreasing: 9, 8, ..., 0
    down = Trajectory()
    do i = 9, 0, -1
        call down%add(real(i, wp), pos=[real(i, wp), 0.0_wp, 0.0_wp])
    end do
    status = down%time_index(3.5_wp, index)
    call check(status > 0 .and. index == 6, 'decreasing times: times(6) = 4 >= 3.5 > 3')
    call check(down%time_index(10.0_wp, index) == TIME_OUT_OF_RANGE .and. index == 0, 'decreasing: before the first')

    ! optionals and data sources
    traj = Trajectory(nopt=2)
    call traj%add(1.0_wp, pos=[1.0_wp, 2.0_wp, 3.0_wp])
    call traj%set_optional(2, [7.0_wp, 8.0_wp, 9.0_wp], ok)
    call check(ok .and. all(traj%optional(1, 2) == [7.0_wp, 8.0_wp, 9.0_wp]), 'set_optional')
    call traj%set_optional(3, [0.0_wp, 0.0_wp, 0.0_wp], ok)
    call check(.not. ok, 'set_optional: no optional 3')
    src = [DataSource(SOURCE_POSOPT, element=2, opt=2), DataSource(SOURCE_TIME, scale=10.0_wp), &
           DataSource(SOURCE_POSOPT, element=1)]
    call check(traj%verify_data(src), 'verify_data')
    call check(all(traj%point(1, src) == [8.0_wp, 10.0_wp, 1.0_wp]), 'point from sources')
    ! points for a range, with a stride: as `point` for each
    call traj%add(2.0_wp, pos=[4.0_wp, 5.0_wp, 6.0_wp])
    call traj%set_optional(2, [1.0_wp, 2.0_wp, 3.0_wp])
    call traj%add(3.0_wp, pos=[7.0_wp, 8.0_wp, 9.0_wp])
    call traj%set_optional(2, [4.0_wp, 5.0_wp, 6.0_wp])
    allocate(v(3, 3))
    call traj%points(1, 3, src, v)
    call check(all(v(:, 1) == traj%point(1, src)) .and. all(v(:, 2) == traj%point(2, src)) .and. &
               all(v(:, 3) == traj%point(3, src)), 'points: a range from sources')
    deallocate(v)
    allocate(v(3, 2))
    call traj%points(1, 3, src, v, stride=2)
    call check(all(v(:, 1) == traj%point(1, src)) .and. all(v(:, 2) == traj%point(3, src)), 'points: every 2nd')
    src(1)%opt = 3
    call check(.not. traj%verify_data(src), 'verify_data: no optional 3')

    ! 2 degrees of freedom
    flat = Trajectory(dof=2)
    call flat%add(0.0_wp, pos=[1.0_wp, 2.0_wp, 3.0_wp])
    call check(all(flat%position(1) == [1.0_wp, 2.0_wp, 0.0_wp]), '2 DOF: z = 0')
    call check(.not. flat%verify_data(position_sources(3)), '2 DOF: no z element')

    ! growth, bulk and one at a time, and change counters
    big = Trajectory()
    call big%reserve(10)
    allocate(t(100000), pos(3, 100000))
    do i = 1, size(t)
        t(i) = real(i, wp)
        pos(:, i) = [real(i, wp), -real(i, wp), 0.5_wp * i]
    end do
    gen = big%generation()
    rev = big%revision()
    call big%add(t(1:50000), pos=pos(:, 1:50000))
    do i = 50001, size(t)
        call big%add(t(i), pos=pos(:, i))
    end do
    call check(big%num_times() == 100000 .and. big%num_positions() == 100000, 'growth: counts')
    call big%positions(p)
    call check(all(p == pos), 'growth: all positions kept')
    call check(big%revision() > rev .and. big%generation() == gen, 'appending changes revision, not generation')
    status = big%time_index(77777.25_wp, index)
    call check(status > 0 .and. index == 77777 .and. status <= 20, 'time_index in 100000 points')
    call big%clear()
    call check(big%is_empty() .and. big%generation() == gen + 1, 'clear changes generation')

    if (failures > 0) then
        write(*, '(a,i0,a)') 'FAIL: ', failures, ' checks failed'
        error stop 1
    end if
    write(*, '(a)') 'PASS: Trajectory'

contains

    subroutine check(ok, what)
        logical, intent(in) :: ok
        character(len=*), intent(in) :: what
        if (.not. ok) then
            write(*, '(a)') '  failed: '//what
            failures = failures + 1
        end if
    end subroutine check

end program test_trajectory
