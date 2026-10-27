!> A planet, an inclined elliptical orbit drawn as a thick line, a dashed
!> circle in the equatorial plane, and a
!> spacecraft moved along the orbit by Fortran every frame (Kepler's equation).
!>
!> Arguments (optional): number of frames to run, then a file to capture the
!> last frame to (e.g. orbit.png). Without them, runs until the window is closed.
program orbit

    use, intrinsic :: iso_c_binding
    use vkf_layer
    use vkf_geometry, only: sphere_create

    implicit none

    integer, parameter :: wp = c_double
    real(wp), parameter :: pi = acos(-1.0_wp)

    ! orbit (units of planet radii)
    real(wp), parameter :: sma = 2.5_wp                 !! semi-major axis
    real(wp), parameter :: ecc = 0.4_wp                 !! eccentricity
    real(wp), parameter :: inc = 30.0_wp * pi / 180.0_wp  !! inclination
    real(wp), parameter :: raan = 20.0_wp * pi / 180.0_wp !! right ascension of the ascending node
    real(wp), parameter :: argp = 60.0_wp * pi / 180.0_wp !! argument of periapsis
    real(wp), parameter :: period = 8.0_wp              !! seconds per orbit (wall clock, at 60 frames/s)
    integer, parameter :: n_orbit = 361                 !! points on the orbit line
    integer, parameter :: n_equator = 181

    character(len=*), parameter :: title = 'vkframes: orbit'
    type(c_ptr) :: viewer, planet, spacecraft, sc_transform, orbit_line, equator_line
    real(wp) :: orbit_xyz(3, n_orbit), equator_xyz(3, n_equator), m(4,4)
    integer :: max_frames, frame, status, i
    character(len=256) :: arg, capture_file

    max_frames = huge(max_frames)
    capture_file = ''
    if (command_argument_count() >= 1) then
        call get_command_argument(1, arg)
        read(arg, *) max_frames
    end if
    if (command_argument_count() >= 2) call get_command_argument(2, capture_file)

    viewer = vkf_viewer_create(1000_c_int, 700_c_int, title, len(title, kind=c_int), 0_c_long_long, 1.0_c_double, 1_c_int)
    if (.not. c_associated(viewer)) error stop 'could not create the viewer'
    call check(vkf_viewer_look_at(viewer, [0.0_wp, -14.0_wp, 6.0_wp], [0.0_wp, 0.0_wp, 0.0_wp], [0.0_wp, 0.0_wp, 1.0_wp]))

    planet = sphere_create(1.0_wp, [0.2_c_float, 0.4_c_float, 1.0_c_float, 1.0_c_float])
    call check(vkf_viewer_add_node(viewer, planet))
    call vkf_node_delete(planet)

    ! the orbit line, sampled uniformly in eccentric anomaly
    do i = 1, n_orbit
        orbit_xyz(:, i) = orbit_position(2.0_wp * pi * real(i - 1, wp) / real(n_orbit - 1, wp))
    end do
    orbit_line = vkf_line_create(int(n_orbit, c_int))
    if (.not. c_associated(orbit_line)) error stop 'could not create the orbit line'
    call check(line_set_style(orbit_line, [1.0_c_float, 0.85_c_float, 0.2_c_float, 1.0_c_float], 4.0_c_float))
    call check(line_set_points(orbit_line, orbit_xyz))
    call check(vkf_viewer_add_node(viewer, orbit_line))

    ! a thin dashed circle in the equatorial plane (dashes and gaps of 8 window units)
    do i = 1, n_equator
        associate (t => 2.0_wp * pi * real(i - 1, wp) / real(n_equator - 1, wp))
            equator_xyz(:, i) = 1.6_wp * [cos(t), sin(t), 0.0_wp]
        end associate
    end do
    equator_line = vkf_line_create(int(n_equator, c_int))
    if (.not. c_associated(equator_line)) error stop 'could not create the equator line'
    call check(line_set_style(equator_line, [0.3_c_float, 1.0_c_float, 0.4_c_float, 1.0_c_float], 1.5_c_float))
    call check(line_set_pattern(equator_line, 1, int(z'00FF')))
    call check(line_set_points(equator_line, equator_xyz))
    call check(vkf_viewer_add_node(viewer, equator_line))

    ! the spacecraft: a small sphere under a transform
    sc_transform = vkf_transform_create()
    spacecraft = sphere_create(0.08_wp, [1.0_c_float, 0.25_c_float, 0.2_c_float, 1.0_c_float])
    call check(vkf_node_add_child(sc_transform, spacecraft))
    call vkf_node_delete(spacecraft)
    call check(vkf_viewer_add_node(viewer, sc_transform))

    frame = 0
    do while (frame < max_frames)
        m = translation(orbit_position(eccentric_anomaly(2.0_wp * pi * real(frame, wp) / (60.0_wp * period))))
        call check(vkf_transform_set_matrix(sc_transform, m))
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
    call vkf_node_delete(equator_line)
    call vkf_viewer_delete(viewer)

contains

    subroutine check(status)
        integer(c_int), intent(in) :: status
        if (status /= 0) error stop 'vkframes layer call failed'
    end subroutine check

    !> Position on the orbit at eccentric anomaly `ea`.
    function orbit_position(ea) result(r)
        real(wp), intent(in) :: ea
        real(wp) :: r(3)
        real(wp) :: p(3)
        p = [sma * (cos(ea) - ecc), sma * sqrt(1.0_wp - ecc**2) * sin(ea), 0.0_wp]
        r = matmul(rot3(raan), matmul(rot1(inc), matmul(rot3(argp), p)))
    end function orbit_position

    !> Eccentric anomaly for mean anomaly `ma` (Newton's method on Kepler's equation).
    function eccentric_anomaly(ma) result(ea)
        real(wp), intent(in) :: ma
        real(wp) :: ea
        integer :: k
        ea = ma
        do k = 1, 20
            ea = ea - (ea - ecc * sin(ea) - ma) / (1.0_wp - ecc * cos(ea))
        end do
    end function eccentric_anomaly

    !> Rotation about z by angle `a` (active).
    pure function rot3(a) result(r)
        real(wp), intent(in) :: a
        real(wp) :: r(3,3)
        r = reshape([cos(a), sin(a), 0.0_wp, -sin(a), cos(a), 0.0_wp, 0.0_wp, 0.0_wp, 1.0_wp], [3, 3])
    end function rot3

    !> Rotation about x by angle `a` (active).
    pure function rot1(a) result(r)
        real(wp), intent(in) :: a
        real(wp) :: r(3,3)
        r = reshape([1.0_wp, 0.0_wp, 0.0_wp, 0.0_wp, cos(a), sin(a), 0.0_wp, -sin(a), cos(a)], [3, 3])
    end function rot1

    pure function translation(t) result(m)
        real(wp), intent(in) :: t(3)
        real(wp) :: m(4,4)
        integer :: k
        m = 0.0_wp
        do k = 1, 4
            m(k, k) = 1.0_wp
        end do
        m(1:3, 4) = t
    end function translation

end program orbit
