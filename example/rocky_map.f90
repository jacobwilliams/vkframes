!> Maps and terrain with Rocky (optional; the `rocky` pixi environment): a
!> TerrainBody lit by the Sun (a frame with a light source), seen from orbit
!> and close to the surface, in kilometers as vkframes' scenes usually are:
!> the Earth from ReadyMap's tile services (imagery and elevation), or the
!> Moon (Rocky's "moon" profile) with the LRO WAC mosaic from NASA's Moon Trek
!> and LOLA's elevation (data/moon_lola_ldem16.tif, from
!> `pixi run -e rocky moon-data`; without it, imagery only).
!>
!> Arguments (optional): earth or moon, the number of frames to show each view,
!> then a file prefix to capture the last frame of each view to
!> (prefix_orbit.png, prefix_low.png):
!>   pixi run -e rocky example rocky_map -- moon 600 moon
program rocky_map

    use vkframes

    implicit none

    real(wp), parameter :: deg = pi / 180.0_wp
    real(wp), parameter :: au = 1.495978707e8_wp                    !! km
    character(len=*), parameter :: readymap_imagery = 'https://readymap.org/readymap/tiles/1.0.0/7/'
    character(len=*), parameter :: readymap_elevation = 'https://readymap.org/readymap/tiles/1.0.0/116/'
    character(len=*), parameter :: moon_imagery = &
        'https://trek.nasa.gov/tiles/Moon/EQ/LRO_WAC_Mosaic_Global_303ppd_v02/1.0.0/default/default028mm/{z}/{y}/{x}.jpg'
    !> the Moon's elevation (`pixi run -e rocky moon-data [-- --resolution 64]`): the finest found
    character(len=*), parameter :: moon_elevation(3) = [character(len=20) :: 'moon_lola_ldem64.tif', &
                                                        'moon_lola_ldem16.tif', 'moon_lola_ldem4.tif']

    type(ReferenceFrame), target :: root, sun
    type(TerrainBody), target :: body
    type(FrameManager), target :: scene
    type(WindowProxy) :: window
    type(View), target :: views(2)
    character(len=256) :: arg, prefix, name
    character(len=5), parameter :: view_names(2) = ['orbit', 'low  ']
    real(wp) :: r, lat, lon, h, sun_lon
    integer :: frames, frame, k
    logical :: running

    if (.not. rocky_available()) then
        write(*, '(a)') 'vkframes was built without Rocky: run with `pixi run -e rocky ...` after `pixi run -e rocky rocky-build`'
        stop
    end if
    name = 'earth'
    frames = 600
    prefix = ''
    if (command_argument_count() >= 1) call get_command_argument(1, name)
    if (command_argument_count() >= 2) then
        call get_command_argument(2, arg)
        read(arg, *) frames
    end if
    if (command_argument_count() >= 3) call get_command_argument(3, prefix)

    root = ReferenceFrame('Root')
    call root%show_axes(NO_AXES)
    call root%show_axes_labels(NO_AXES)
    call root%show_name_label(.false.)

    ! the body, and a spot to look at closely (latitude, longitude, height in km)
    if (name == 'moon') then
        body = TerrainBody('Moon', profile='moon', color=[0.5_wp, 0.5_wp, 0.5_wp])
        call body%add_image_layer(moon_imagery, LAYER_GEODETIC)
        do k = 1, size(moon_elevation)
            if (len(find_data_file(trim(moon_elevation(k)))) == 0) cycle
            call body%add_elevation_layer(trim(moon_elevation(k)), LAYER_GDAL)
            exit
        end do
        if (k > size(moon_elevation)) write(*, '(a)') 'no Moon elevation (pixi run -e rocky moon-data): imagery only'
        lat = -43.3_wp; lon = -11.2_wp; h = 40.0_wp                ! Tycho
        sun_lon = lon + 75.0_wp                                     ! low over Tycho, from the east
    else
        body = TerrainBody('Earth', color=[0.1_wp, 0.2_wp, 0.4_wp])
        call body%add_image_layer(readymap_imagery)
        call body%add_elevation_layer(readymap_elevation)
        lat = 46.5_wp; lon = 8.0_wp; h = 25.0_wp                    ! the Alps
        sun_lon = lon + 60.0_wp
    end if
    call body%show_axes(NO_AXES)
    call body%show_axes_labels(NO_AXES)
    call body%show_name_label(.false.)
    r = body%body_radius()

    ! the Sun, in the body's equatorial plane
    sun = ReferenceFrame('Sun')
    call sun%set_position(au * [cos(sun_lon * deg), sin(sun_lon * deg), 0.0_wp])
    call sun%set_light_source_enabled(.true.)
    call sun%set_light_diffuse([1.0_wp, 1.0_wp, 1.0_wp])
    call sun%set_light_ambient([0.03_wp, 0.03_wp, 0.03_wp])
    call sun%show_axes(NO_AXES)
    call sun%show_axes_labels(NO_AXES)
    call sun%show_name_label(.false.)

    if (.not. (root%add_child(body) .and. root%add_child(sun))) error stop 'add_child failed'
    scene = FrameManager(root)

    if (.not. window%open(1000, 700, 'vkframes: Rocky map')) error stop 'could not open a window'
    call window%set_scene(scene)
    ! from orbit (3 radii), and close to the surface (looking at the spot from the south)
    views(1) = View(root, body)
    call views(1)%set_default_view_parameters(eye=3.0_wp * r * unit(surface(lat, lon - 25.0_wp, 0.0_wp)), &
                                              center=[0.0_wp, 0.0_wp, 0.0_wp], up=[0.0_wp, 0.0_wp, 1.0_wp])
    views(2) = View(root, body)
    call views(2)%set_default_view_parameters(eye=surface(lat - 2.0_wp, lon - 0.8_wp, h), center=surface(lat, lon, 0.0_wp), &
                                              up=unit(surface(lat, lon, 0.0_wp)))
    do k = 1, 2
        call window%add_view(views(k))
    end do

    running = .true.
    do k = 1, 2
        call window%select_view(k)
        frame = 0
        do while (running .and. frame < frames)
            running = window%frame()
            frame = frame + 1
        end do
        if (.not. running) exit
        if (len_trim(prefix) > 0) then
            arg = trim(prefix)//'_'//trim(view_names(k))//'.png'
            if (window%capture(trim(arg))) write(*, '(a)') 'captured '//trim(arg)
        end if
    end do
    call window%close()

contains

    !> A point at latitude and longitude (degrees) and height (km) above the body's sphere.
    function surface(lat, lon, h) result(p)
        real(wp), intent(in) :: lat, lon, h
        real(wp) :: p(3)
        p = (r + h) * [cos(lat * deg) * cos(lon * deg), cos(lat * deg) * sin(lon * deg), sin(lat * deg)]
    end function surface

    pure function unit(v) result(u)
        real(wp), intent(in) :: v(3)
        real(wp) :: u(3)
        u = v / norm2(v)
    end function unit

end program rocky_map
