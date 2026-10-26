!> A Rocky map (optional; the `rocky` pixi environment), drawn in kilometers
!> as vkframes' scenes usually are, seen from orbit and close to the surface:
!> the Earth's terrain from ReadyMap's tile services (imagery and elevation),
!> or the Moon (Rocky's "moon" profile) with the LRO WAC mosaic from NASA's
!> Moon Trek (over the network).
!>
!> Arguments (optional): earth or moon, the number of frames to run, then a
!> file prefix to capture the last frame of each view to (prefix_orbit.png,
!> prefix_low.png):
!>   pixi run -e rocky example rocky_map -- moon 600 moon
program rocky_map

    use, intrinsic :: iso_c_binding
    use vkf_layer

    implicit none

    integer, parameter :: wp = c_double
    real(wp), parameter :: re = 6378.137_wp                         !! km
    character(len=*), parameter :: title = 'vkframes: Rocky map'
    character(len=*), parameter :: imagery = 'https://readymap.org/readymap/tiles/1.0.0/7/'
    character(len=*), parameter :: elevation = 'https://readymap.org/readymap/tiles/1.0.0/116/'
    character(len=*), parameter :: moon_imagery = &
        'https://trek.nasa.gov/tiles/Moon/EQ/LRO_WAC_Mosaic_Global_303ppd_v02/1.0.0/default/default028mm/{z}/{y}/{x}.jpg'
    real(wp), parameter :: rm = 1737.4_wp                           !! km

    type(c_ptr) :: viewer, map
    integer :: max_frames, frame, status, k
    character(len=256) :: arg, prefix, body
    real(wp) :: radius
    real(wp) :: eye(3, 2), center(3, 2)
    character(len=5), parameter :: names(2) = ['orbit', 'low  ']

    if (vkf_rocky_available() == 0) then
        write(*, '(a)') 'vkframes was built without Rocky: run with `pixi run -e rocky ...` after `pixi run -e rocky rocky-build`'
        stop
    end if
    body = 'earth'
    max_frames = 600
    prefix = ''
    if (command_argument_count() >= 1) call get_command_argument(1, body)
    if (command_argument_count() >= 2) then
        call get_command_argument(2, arg)
        read(arg, *) max_frames
    end if
    if (command_argument_count() >= 3) call get_command_argument(3, prefix)

    viewer = vkf_viewer_create(1000_c_int, 700_c_int, title, len(title, kind=c_int), 0_c_long_long, 1.0_c_double)
    if (.not. c_associated(viewer)) error stop 'could not create the viewer'

    ! the map, in km (1e-3 scene units per meter)
    if (body == 'moon') then
        radius = rm
        map = vkf_rocky_map_create(viewer, 'moon', 4_c_int, 1.0e-3_c_double)
        if (.not. c_associated(map)) error stop 'could not create the map'
        call check(vkf_rocky_add_image(map, 2_c_int, moon_imagery, len(moon_imagery, kind=c_int)))
    else
        radius = re
        map = vkf_rocky_map_create(viewer, '', 0_c_int, 1.0e-3_c_double)
        if (.not. c_associated(map)) error stop 'could not create the map'
        call check(vkf_rocky_add_image(map, 0_c_int, imagery, len(imagery, kind=c_int)))
        call check(vkf_rocky_add_elevation(map, 0_c_int, elevation, len(elevation, kind=c_int), 0_c_int))
    end if
    call check(vkf_viewer_add_node(viewer, map))

    ! from orbit (3 radii), then low: over the Alps, or Tycho on the Moon
    eye(:, 1) = 3.0_wp * radius * [0.6_wp, -0.6_wp, 0.5_wp]
    center(:, 1) = 0.0_wp
    if (body == 'moon') then
        center(:, 2) = geodetic(-43.3_wp, -11.2_wp, 0.0_wp)
        eye(:, 2) = geodetic(-45.5_wp, -11.8_wp, 40.0_wp)
    else
        center(:, 2) = geodetic(46.5_wp, 8.0_wp, 0.0_wp)
        eye(:, 2) = geodetic(45.8_wp, 7.6_wp, 25.0_wp)
    end if

    do k = 1, 2
        call check(vkf_viewer_look_at(viewer, eye(:, k), center(:, k), merge([0.0_wp, 0.0_wp, 1.0_wp], unit(eye(:, k)), k == 1)))
        call check(vkf_viewer_perspective(viewer, 30.0_wp, 1.0e-3_wp * norm2(eye(:, k) - center(:, k)), 10.0_wp * radius))
        frame = 0
        do while (frame < max_frames)
            status = vkf_viewer_frame(viewer)
            if (status < 0) error stop 'frame failed'
            if (status == 0) exit
            frame = frame + 1
        end do
        if (status == 0) exit
        if (len_trim(prefix) > 0) then
            arg = trim(prefix)//'_'//trim(names(k))//'.png'
            call check(vkf_viewer_capture(viewer, trim(arg), len_trim(arg, kind=c_int)))
            write(*, '(a)') 'captured '//trim(arg)
        end if
    end do

    call vkf_node_delete(map)
    call vkf_viewer_delete(viewer)

contains

    subroutine check(status)
        integer(c_int), intent(in) :: status
        if (status /= 0) error stop 'vkframes layer call failed'
    end subroutine check

    !> A point at latitude and longitude (degrees) and height (km) on a sphere of the body's radius.
    function geodetic(lat, lon, h) result(r)
        real(wp), intent(in) :: lat, lon, h
        real(wp) :: r(3)
        real(wp), parameter :: deg = acos(-1.0_wp) / 180.0_wp
        r = (radius + h) * [cos(lat * deg) * cos(lon * deg), cos(lat * deg) * sin(lon * deg), sin(lat * deg)]
    end function geodetic

    pure function unit(v) result(u)
        real(wp), intent(in) :: v(3)
        real(wp) :: u(3)
        u = v / norm2(v)
    end function unit

end program rocky_map
