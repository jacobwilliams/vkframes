!> Changing a scene while it is shown: after 30 frames a Moon is added to the
!> tree, the Earth's axes are moved (from its center, longer), HUD text is
!> added and a starry sky is set; after 60 frames the Moon is removed and the
!> names of the brightest stars are shown.
!>
!> Arguments (optional): a file prefix to capture frames 50 and 80 to
!> (prefix_50.png, prefix_80.png).
program live_changes

    use vkframes

    implicit none

    real(wp), parameter :: re = 6378.137_wp

    type(ReferenceFrame), target :: root
    type(Sphere), target :: earth, moon
    type(FrameManager), target :: scene
    type(WindowProxy) :: window
    type(View), target :: v
    type(HUDText), target :: note
    type(SkySphere), target :: sky
    character(len=1024) :: prefix
    character(len=16) :: num
    integer :: frame
    logical :: running

    prefix = ''
    if (command_argument_count() >= 1) call get_command_argument(1, prefix)

    root = ReferenceFrame('Root')
    call root%show_decorations(.false.)
    earth = Sphere('Earth', radius=re, color=[0.6_wp, 0.8_wp, 1.0_wp])
    call earth%set_texture_map('2k_earth_daymap.jpg')
    moon = Sphere('Moon', radius=0.27_wp * re, color=[0.8_wp, 0.8_wp, 0.8_wp])
    call moon%set_position([3.0_wp * re, 1.0_wp * re, 0.5_wp * re])
    if (.not. root%add_child(earth)) error stop 'add_child failed'
    scene = FrameManager(root)
    note = HUDText('added while running', position=[0.02_wp, 0.97_wp], alignment=ALIGN_LEFT_TOP, size=20.0_wp)

    if (.not. window%open(900, 700, 'vkframes: live changes')) error stop 'could not open a window'
    call window%set_scene(scene)
    v = View(root, root)
    call v%set_default_view_parameters(eye=[2.0_wp, -8.0_wp, 2.5_wp] * re, center=[1.0_wp, 0.0_wp, 0.0_wp] * re, &
                                       up=[0.0_wp, 0.0_wp, 1.0_wp])
    call window%add_view(v)

    frame = 0
    running = .true.
    do while (running .and. frame < 80)
        frame = frame + 1
        if (frame == 30) then
            if (.not. root%add_child(moon)) error stop 'add_child failed'
            call earth%move_x_axis([0.0_wp, 0.0_wp, 0.0_wp], 2.0_wp * re, body_radius=0.02_wp * re, head_radius=0.06_wp * re)
            call earth%move_y_axis([0.0_wp, 0.0_wp, 0.0_wp], 2.0_wp * re, body_radius=0.02_wp * re, head_radius=0.06_wp * re)
            call earth%move_z_axis([0.0_wp, 0.0_wp, 0.0_wp], 2.0_wp * re, body_radius=0.02_wp * re, head_radius=0.06_wp * re)
            call window%add_hud_text(note)
            if (.not. sky%set_star_data('Stars_HYGv3.txt', max_mag=6.0_wp)) write(*, '(a)') 'no star catalog'
            call window%set_sky(sky)
        end if
        if (frame == 60) then
            if (.not. root%remove_child(moon)) error stop 'remove_child failed'
            call note%set_text('the Moon was removed')
            if (.not. sky%set_star_names(.true., max_mag=2.5_wp)) write(*, '(a)') 'could not show the star names'
        end if
        running = window%frame()
        if (len_trim(prefix) > 0 .and. (frame == 50 .or. frame == 80) .and. running) then
            write(num, '(i0)') frame
            if (window%capture(trim(prefix)//'_'//trim(num)//'.png')) &
                write(*, '(a)') 'captured '//trim(prefix)//'_'//trim(num)//'.png'
        end if
    end do
    call window%close()

end program live_changes
