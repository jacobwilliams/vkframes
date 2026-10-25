!> One scene in two windows: the Earth and a spacecraft following an orbit,
!> seen from the Earth in one window and from the spacecraft toward the Earth
!> in the other. The second window's clock is synchronized to the first's, so
!> both show the same time. (A scene shown in several windows is drawn from the
!> same GPU data: vkframes' windows share one Vulkan device.)
!>
!> Arguments (optional): number of frames to run, then a file prefix to capture
!> both windows' last frames to (prefix_1.png, prefix_2.png).
program two_windows

    use vkframes

    implicit none

    real(wp), parameter :: re = 6378.137_wp, r_orbit = 1.6_wp * re, inc = 30.0_wp * pi / 180.0_wp
    real(wp), parameter :: period = 600.0_wp
    integer, parameter :: n = 361

    type(Sphere), target :: earth, craft
    type(Trajectory), target :: orbit
    type(FrameManager), target :: scene
    type(WindowProxy), target :: w1, w2
    type(View), target :: earth_view, nadir_view
    type(SimulationClock), pointer :: master, clock2
    character(len=1024) :: arg, prefix
    real(wp) :: t, u
    integer :: max_frames, frame, k
    logical :: running

    max_frames = huge(max_frames)
    prefix = ''
    if (command_argument_count() >= 1) then
        call get_command_argument(1, arg)
        read(arg, *) max_frames
    end if
    if (command_argument_count() >= 2) call get_command_argument(2, prefix)

    ! the scene
    earth = Sphere('Earth', radius=re, color=[0.2_wp, 0.4_wp, 1.0_wp])
    craft = Sphere('Spacecraft', radius=0.05_wp * re, color=[1.0_wp, 0.3_wp, 0.2_wp])
    orbit = Trajectory()
    do k = 1, n
        t = period * real(k - 1, wp) / real(n - 1, wp)
        u = 2.0_wp * pi * t / period
        call orbit%add(t, pos=r_orbit * [cos(u), sin(u) * cos(inc), sin(u) * sin(inc)])
    end do
    call craft%follow(orbit)
    if (.not. earth%add_child(craft)) error stop 'add_child failed'
    scene = FrameManager(earth)

    ! two windows on the same scene, with different views
    earth_view = View(earth, earth)
    nadir_view = View(earth, craft, lookat=earth, frame_type=ABSOLUTE_FRAME)
    call nadir_view%set_default_view_distance(0.3_wp * re)
    if (.not. w1%open(700, 500, 'vkframes: the Earth')) error stop 'could not open window 1'
    if (.not. w2%open(700, 500, 'vkframes: from the spacecraft')) error stop 'could not open window 2'
    call w1%set_scene(scene)
    call w2%set_scene(scene)
    call w1%add_view(earth_view)
    call w2%add_view(nadir_view)

    ! one time for both
    master => w1%clock()
    clock2 => w2%clock()
    if (.not. clock2%synchronize(master)) error stop 'could not synchronize the clocks'

    frame = 0
    running = .true.
    do while (running .and. frame < max_frames)
        running = w1%frame()
        running = w2%frame() .and. running
        frame = frame + 1
    end do
    write(*, '(a,i0,a,f8.2,a)') 'rendered ', frame, ' frames (t = ', w2%time(), ' s in both windows)'

    if (len_trim(prefix) > 0 .and. running) then
        if (w1%capture(trim(prefix)//'_1.png')) write(*, '(a)') 'captured '//trim(prefix)//'_1.png'
        if (w2%capture(trim(prefix)//'_2.png')) write(*, '(a)') 'captured '//trim(prefix)//'_2.png'
    end if
    call w2%close()
    call w1%close()

end program two_windows
