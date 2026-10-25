!> Precision check: an Earth-sized planet (meters), a 7000 km circular orbit
!> drawn as a line, and a 10 m spacecraft at one of the line's points, seen
!> from 30 m away. With eye-relative rendering the line passes through the
!> spacecraft's center; in plain float it would be off by about a meter.
!>
!> Arguments (optional): number of frames to run, then a file to capture the
!> last frame to. Without them, runs until the window is closed.
program precision

    use, intrinsic :: iso_c_binding
    use vkf_layer

    implicit none

    integer, parameter :: wp = c_double
    real(wp), parameter :: pi = acos(-1.0_wp)
    real(wp), parameter :: r_earth = 6378137.0_wp       !! m
    real(wp), parameter :: r_orbit = 7000.0e3_wp        !! m
    real(wp), parameter :: inc = 51.6_wp * pi / 180.0_wp
    real(wp), parameter :: sc_radius = 5.0_wp           !! m (a 10 m spacecraft)
    integer, parameter :: n_orbit = 3601
    integer, parameter :: i_sc = 400                    !! the orbit point the spacecraft is at

    character(len=*), parameter :: title = 'vkframes: precision'
    type(c_ptr) :: viewer, earth, spacecraft, sc_transform, orbit_line
    real(wp) :: orbit_xyz(3, n_orbit), m(4,4), sc(3), radial(3), along(3), eye(3)
    integer :: max_frames, frame, status, i, k
    character(len=256) :: arg, capture_file

    max_frames = huge(max_frames)
    capture_file = ''
    if (command_argument_count() >= 1) then
        call get_command_argument(1, arg)
        read(arg, *) max_frames
    end if
    if (command_argument_count() >= 2) call get_command_argument(2, capture_file)

    do i = 1, n_orbit
        orbit_xyz(:, i) = orbit_position(2.0_wp * pi * real(i - 1, wp) / real(n_orbit - 1, wp))
    end do
    sc = orbit_xyz(:, i_sc)
    radial = sc / norm2(sc)
    along = orbit_xyz(:, i_sc + 1) - orbit_xyz(:, i_sc - 1)
    along = along / norm2(along)
    eye = sc - 30.0_wp * along + 6.0_wp * radial

    viewer = vkf_viewer_create(1000_c_int, 700_c_int, title, len(title, kind=c_int))
    if (.not. c_associated(viewer)) error stop 'could not create the viewer'
    call check(vkf_viewer_look_at(viewer, eye, sc, radial))
    call check(vkf_viewer_perspective(viewer, 40.0_wp, 0.1_wp, 1.0e8_wp))

    earth = vkf_sphere_create(r_earth, [0.2_c_float, 0.4_c_float, 1.0_c_float, 1.0_c_float])
    call check(vkf_viewer_add_node(viewer, earth))
    call vkf_node_delete(earth)

    orbit_line = vkf_line_create(viewer, int(n_orbit, c_int))
    if (.not. c_associated(orbit_line)) error stop 'could not create the orbit line'
    call check(line_set_style(orbit_line, [1.0_c_float, 0.85_c_float, 0.2_c_float, 1.0_c_float], 2.0_c_float))
    call check(line_set_points(orbit_line, orbit_xyz))
    call check(vkf_viewer_add_node(viewer, orbit_line))

    sc_transform = vkf_transform_create()
    m = 0.0_wp
    do k = 1, 4
        m(k, k) = 1.0_wp
    end do
    m(1:3, 4) = sc
    call check(vkf_transform_set_matrix(sc_transform, m))
    spacecraft = vkf_sphere_create(sc_radius, [1.0_c_float, 0.25_c_float, 0.2_c_float, 1.0_c_float])
    call check(vkf_node_add_child(sc_transform, spacecraft))
    call vkf_node_delete(spacecraft)
    call check(vkf_viewer_add_node(viewer, sc_transform))

    frame = 0
    status = 1
    do while (frame < max_frames)
        status = vkf_viewer_frame(viewer)
        if (status < 0) error stop 'frame failed'
        if (status == 0) exit
        frame = frame + 1
    end do
    write(*, '(a,i0,a)') 'rendered ', frame, ' frames'

    if (len_trim(capture_file) > 0 .and. status == 1) then
        call check(vkf_viewer_capture(viewer, trim(capture_file), len_trim(capture_file, kind=c_int)))
        write(*, '(a)') 'captured '//trim(capture_file)
    end if

    call vkf_node_delete(sc_transform)
    call vkf_node_delete(orbit_line)
    call vkf_viewer_delete(viewer)

contains

    subroutine check(status)
        integer(c_int), intent(in) :: status
        if (status /= 0) error stop 'vkframes layer call failed'
    end subroutine check

    !> Position on the circular orbit at argument of latitude `u`.
    pure function orbit_position(u) result(r)
        real(wp), intent(in) :: u
        real(wp) :: r(3)
        r = r_orbit * [cos(u), sin(u) * cos(inc), sin(u) * sin(inc)]
    end function orbit_position

end program precision
