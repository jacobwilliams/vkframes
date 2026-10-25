!> Checks ReferenceFrame: the tree (cycles refused, several parents, detach),
!> frame_string, transforms, following a trajectory, defaults.
program test_frames

    use vkframes

    implicit none

    type(ReferenceFrame), target :: root, a, b, c
    type(Trajectory), target :: traj
    type(TrajectoryFollower), pointer :: follower
    real(wp) :: m(4,4), p(4)
    real(wp), parameter :: z(3) = [0.0_wp, 0.0_wp, 1.0_wp], tol = 1.0e-12_wp
    character(len=:), allocatable :: str
    character(len=*), parameter :: nl = new_line('a')
    integer :: failures

    failures = 0
    root = ReferenceFrame('Root')
    a = ReferenceFrame('A', [1.0_wp, 0.0_wp, 0.0_wp])
    b = ReferenceFrame('B')
    c = ReferenceFrame('C')

    ! defaults (as OpenFrames)
    call check(all(root%color() == [1.0_wp, 1.0_wp, 1.0_wp, 0.9_wp]), 'default color: white, 90% opaque')
    call check(all(a%color() == [1.0_wp, 0.0_wp, 0.0_wp, 0.9_wp]), 'color [r, g, b] keeps alpha')
    call check(root%axis_label(X_AXIS) == 'X' .and. root%label_size() == 30, 'default labels')
    call check(root%axes_shown() == NO_AXES .and. root%name_label_shown(), 'default decorations')
    call root%set_z_label('Pole')
    call check(root%axis_label(Z_AXIS) == 'Pole', 'set_z_label')

    ! the tree
    call check(root%add_child(a) .and. a%add_child(b) .and. root%add_child(c), 'add children')
    call check(root%add_child(a) .and. root%num_children() == 2, 'adding again: true, no duplicate')
    call check(.not. b%add_child(root), 'an ancestor cannot become a child')
    call check(.not. a%add_child(a), 'a frame cannot be its own child')
    call check(c%add_child(b) .and. b%num_parents() == 2, 'a frame with two parents')
    call check(associated(a%child(1), b) .and. associated(b%parent(1), a) .and. b%parent_index(c) == 2, &
               'child and parent pointers')

    str = root%frame_string()
    call check(str == ' Root (ReferenceFrame)'//nl// &
                      ' |-->A (ReferenceFrame)'//nl// &
                      ' |---|-->B (ReferenceFrame)'//nl// &
                      ' |-->C (ReferenceFrame)'//nl// &
                      ' |---|-->B (ReferenceFrame)'//nl, 'frame_string (OpenFrames format)')
    if (failures > 0) write(*, '(a)') str

    call check(c%remove_child(b) .and. b%num_parents() == 1 .and. c%num_children() == 0, 'remove_child')
    call check(.not. c%remove_child(b), 'remove_child: not a child')

    ! transforms: A at (1, 0, 0) turned 90 degrees about z, B at (1, 0, 0) in A
    call a%set_position([1.0_wp, 0.0_wp, 0.0_wp])
    call a%set_attitude(quat_from_axis_angle(z, 0.5_wp * pi))
    call b%set_position([1.0_wp, 0.0_wp, 0.0_wp])
    m = b%world_matrix()
    p = matmul(m, [0.0_wp, 0.0_wp, 0.0_wp, 1.0_wp])
    call check(all(abs(p(1:3) - [1.0_wp, 1.0_wp, 0.0_wp]) < tol), 'world position through a rotated parent')

    ! following a trajectory, through the tree's update
    traj = Trajectory()
    call traj%add(0.0_wp, pos=[0.0_wp, 0.0_wp, 0.0_wp])
    call traj%add(10.0_wp, pos=[10.0_wp, 0.0_wp, 0.0_wp])
    call b%follow(traj, data=FOLLOW_POSITION)
    call check(b%is_following(), 'following')
    call root%update(4.0_wp)
    call check(all(abs(b%position() - [4.0_wp, 0.0_wp, 0.0_wp]) < tol), 'update moves the follower')
    follower => b%follower()
    call follower%set_offset_time(1.0_wp)
    call root%update(4.0_wp)
    call check(all(abs(b%position() - [5.0_wp, 0.0_wp, 0.0_wp]) < tol), 'follower settings through follower()')
    call b%unfollow()
    call root%update(8.0_wp)
    call check(all(abs(b%position() - [5.0_wp, 0.0_wp, 0.0_wp]) < tol), 'unfollow keeps the last position')

    ! detach
    call check(c%add_child(b), 're-add')
    call b%detach()
    call check(b%num_parents() == 0 .and. a%num_children() == 0 .and. c%num_children() == 0, 'detach')

    if (failures > 0) then
        write(*, '(a,i0,a)') 'FAIL: ', failures, ' checks failed'
        error stop 1
    end if
    write(*, '(a)') 'PASS: ReferenceFrame'

contains

    subroutine check(ok, what)
        logical, intent(in) :: ok
        character(len=*), intent(in) :: what
        if (.not. ok) then
            write(*, '(a)') '  failed: '//what
            failures = failures + 1
        end if
    end subroutine check

end program test_frames
