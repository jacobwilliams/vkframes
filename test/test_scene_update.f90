!> Checks that a scene shown in several windows is updated once per
!> simulation time (FrameManager%update, with each window's count of the
!> updates it has seen), headless: two windows with synchronized clocks render
!> the same time and update the scene once per round of frames, also while the
!> time is paused (for data added meanwhile); a window left alone (the other
!> one closed) updates it every frame; a different time always updates it.
program test_scene_update

    use vkframes

    implicit none

    type(ReferenceFrame), target :: root
    type(ReferenceFrame), target :: craft
    type(Trajectory), target :: traj
    type(FrameManager) :: scene
    type(SimulationClock), target :: master, other
    type(SimulationClock), pointer :: pm
    real(wp) :: t1, t2
    integer :: seen_a, seen_b, failures

    failures = 0
    traj = Trajectory()
    call traj%add(0.0_wp, pos=[0.0_wp, 0.0_wp, 0.0_wp])
    call traj%add(10.0_wp, pos=[10.0_wp, 0.0_wp, 0.0_wp])
    root = ReferenceFrame('Root')
    craft = ReferenceFrame('Craft')
    call craft%follow(traj)
    if (.not. root%add_child(craft)) error stop 'add_child failed'
    scene = FrameManager(root)
    seen_a = -1                                     ! (as a new window's)
    seen_b = -1

    ! two windows, B's clock synchronized to A's: the same time in both
    pm => master
    call check(other%synchronize(pm), 'synchronize')
    call master%set_time(2.0_wp, wall=100.0_wp)
    t1 = master%update(wall=101.0_wp)               ! window A's frame, then B's
    t2 = other%update(wall=101.5_wp)
    call check(t1 == 3.0_wp .and. t2 == t1, 'synchronized windows render the same time')
    call scene%update(t1, seen_a)
    call check(scene%num_updates() == 1 .and. all(craft%position() == [3.0_wp, 0.0_wp, 0.0_wp]), &
               'window A updates the scene')
    call scene%update(t2, seen_b)
    call check(scene%num_updates() == 1, 'window B, same time: not updated again')

    ! the next round: a new time, one update
    t1 = master%update(wall=102.0_wp)
    t2 = other%update(wall=102.0_wp)
    call scene%update(t1, seen_a)
    call scene%update(t2, seen_b)
    call check(scene%num_updates() == 2 .and. all(craft%position() == [4.0_wp, 0.0_wp, 0.0_wp]), &
               'next round: one update, at the new time')

    ! paused: the same time every round, still one update per round (new data)
    call master%pause(.true., wall=102.0_wp)
    call traj%clear()
    call traj%add(0.0_wp, pos=[0.0_wp, 0.0_wp, 0.0_wp])
    call traj%add(10.0_wp, pos=[0.0_wp, 20.0_wp, 0.0_wp])
    t1 = master%update(wall=103.0_wp)
    t2 = other%update(wall=103.0_wp)
    call check(t1 == 4.0_wp .and. t2 == t1, 'paused: the same time')
    call scene%update(t1, seen_a)
    call scene%update(t2, seen_b)
    call check(scene%num_updates() == 3 .and. all(craft%position() == [0.0_wp, 8.0_wp, 0.0_wp]), &
               'paused: one update per round, with the new data')
    call scene%update(t1, seen_a)
    call scene%update(t2, seen_b)
    call check(scene%num_updates() == 4, 'paused: the next round updates once again')

    ! window A gone: B alone updates the scene every frame
    call scene%update(t2, seen_b)
    call scene%update(t2, seen_b)
    call check(scene%num_updates() == 6, 'a window alone updates every frame')

    ! a different time always updates
    call scene%update(t2 + 1.0_wp, seen_a)
    call scene%update(t2, seen_b)
    call check(scene%num_updates() == 8, 'different times: each updates')

    if (failures > 0) then
        write(*, '(a,i0,a)') 'FAIL: ', failures, ' checks failed'
        error stop 1
    end if
    write(*, '(a)') 'PASS: scene updated once per time across windows'

contains

    subroutine check(ok, what)
        logical, intent(in) :: ok
        character(len=*), intent(in) :: what
        if (.not. ok) then
            write(*, '(a)') '  failed: '//what
            failures = failures + 1
        end if
    end subroutine check

end program test_scene_update
