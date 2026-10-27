!> Orbit planes (RibbonArtist; not in OpenFrames): each orbit sweeps out a
!> translucent surface from the Earth's center to the spacecraft, in its
!> orbit's color, drawn beside the orbit's line (a CurveArtist) in the same
!> DrawableTrajectory.
!>
!> * A low orbit (inclined 28.5 degrees) and a geostationary orbit, drawn
!>   whole.
!> * A Molniya orbit traced up to the current time, the plane swept as the
!>   spacecraft (a marker) moves: the quads passed, and one to the
!>   spacecraft's position at the current time.
!>
!> Arguments (optional): number of frames to run, then a file to capture the
!> last frame to: `pixi run example orbit_planes -- 300 planes.png`
program orbit_planes

    use vkframes

    implicit none

    real(wp), parameter :: mu = 398600.4418_wp                  !! km^3/s^2
    real(wp), parameter :: re = 6378.137_wp                     !! km
    real(wp), parameter :: deg = pi / 180.0_wp
    integer, parameter :: n_orbits = 3, n_points = 1000
    character(len=*), parameter :: names(n_orbits) = [character(len=8) :: 'LEO', 'GEO', 'Molniya']
    real(wp), parameter :: elements(5, n_orbits) = reshape([ &      ! a (km), e, i, node, perigee (degrees)
        7000.0_wp, 0.0_wp, 28.5_wp, 20.0_wp, 0.0_wp, &
        42164.0_wp, 0.0_wp, 0.0_wp, 0.0_wp, 0.0_wp, &
        26600.0_wp, 0.74_wp, 63.4_wp, 250.0_wp, 270.0_wp], [5, n_orbits])
    real(wp), parameter :: colors(3, n_orbits) = reshape([ &
        1.0_wp, 0.8_wp, 0.2_wp, &
        0.3_wp, 0.7_wp, 1.0_wp, &
        1.0_wp, 0.35_wp, 0.6_wp], [3, n_orbits])

    type(ReferenceFrame), target :: root
    type(Sphere), target :: earth
    type(Trajectory), target :: orbits(n_orbits)
    type(DrawableTrajectory), target :: orbit_frames(n_orbits), craft
    type(CurveArtist), target :: curves(n_orbits)
    type(RibbonArtist), target :: planes(n_orbits)
    type(MarkerArtist), target :: marker
    type(FrameManager), target :: scene
    type(WindowProxy), target :: window
    type(View), target :: earth_view
    character(len=1024) :: arg, file
    real(wp) :: period, t
    integer :: i, k, frames, frame
    logical :: running

    frames = huge(frames)
    file = ''
    if (command_argument_count() >= 1) then
        call get_command_argument(1, arg)
        read(arg, *) frames
    end if
    if (command_argument_count() >= 2) call get_command_argument(2, file)

    root = ReferenceFrame('Inertial')
    call hide_decorations(root)
    earth = Sphere('Earth', radius=re)
    call earth%set_texture_map('2k_earth_daymap.jpg')                    ! (data/, on VKFRAMES_DATA_PATH)
    call hide_decorations(earth)
    if (.not. root%add_child(earth)) error stop 'add_child failed'

    ! each orbit (one period): its line, and its plane from the Earth's center
    do i = 1, n_orbits
        period = 2.0_wp * pi * sqrt(elements(1, i)**3 / mu)
        orbits(i) = Trajectory()
        call orbits(i)%reserve(n_points)
        do k = 1, n_points
            t = period * real(k - 1, wp) / real(n_points - 1, wp)
            call orbits(i)%add(t, pos=orbit_position(t, elements(:, i)))
        end do
        curves(i) = CurveArtist(orbits(i), color=colors(:, i), width=2.0_wp)
        planes(i) = RibbonArtist(orbits(i), color=[colors(:, i), 0.2_wp])
        orbit_frames(i) = DrawableTrajectory(trim(names(i)))
        call hide_decorations(orbit_frames(i))
        call orbit_frames(i)%add_artist(planes(i))
        call orbit_frames(i)%add_artist(curves(i))
        if (.not. root%add_child(orbit_frames(i))) error stop 'add_child failed'
    end do

    ! the Molniya orbit traced, with a marker at the spacecraft
    call curves(3)%set_trace_mode(.true.)
    call planes(3)%set_trace_mode(.true.)
    marker = MarkerArtist(markers=MARKER_START, size=14.0_wp, color=colors(:, 3))
    craft = DrawableTrajectory('Molniya spacecraft')
    call hide_decorations(craft)
    call craft%add_artist(marker)
    call craft%follow(orbits(3), data=FOLLOW_POSITION)
    if (.not. root%add_child(craft)) error stop 'add_child failed'
    scene = FrameManager(root)

    if (.not. window%open(1100, 800, 'vkframes: orbit planes')) error stop 'could not open a window'
    call window%set_scene(scene)
    call window%set_background_color([0.0_wp, 0.0_wp, 0.02_wp])
    call window%set_time_scale(1200.0_wp)                                ! 20 minutes a second
    call window%set_time(3.0_wp * 3600.0_wp)
    earth_view = View(root, earth)
    call earth_view%set_default_view_parameters(eye=[95000.0_wp, -80000.0_wp, 55000.0_wp], &
                                                center=[0.0_wp, 0.0_wp, 0.0_wp], up=[0.0_wp, 0.0_wp, 1.0_wp])
    call window%add_view(earth_view)

    running = .true.
    frame = 0
    do while (running .and. frame < frames)
        running = window%frame()
        frame = frame + 1
    end do
    if (running .and. len_trim(file) > 0) then
        if (window%capture(trim(file))) write(*, '(a)') 'captured '//trim(file)
    end if
    call window%close()

contains

    subroutine hide_decorations(frame)
        class(ReferenceFrame), intent(inout) :: frame
        call frame%show_axes(NO_AXES)
        call frame%show_axes_labels(NO_AXES)
        call frame%show_name_label(.false.)
    end subroutine hide_decorations

    !> Position at time t (km) on the orbit of elements e: semi-major axis,
    !> eccentricity, inclination, ascending node, argument of perigee (degrees),
    !> at perigee at t = 0 (Kepler's equation by Newton iteration).
    pure function orbit_position(t, e) result(r)
        real(wp), intent(in) :: t, e(5)
        real(wp) :: r(3)
        real(wp) :: m, ea, x, y, ci, si, co, so, cw, sw
        integer :: k
        m = sqrt(mu / e(1)**3) * t
        ea = m
        do k = 1, 20
            ea = ea - (ea - e(2) * sin(ea) - m) / (1.0_wp - e(2) * cos(ea))
        end do
        x = e(1) * (cos(ea) - e(2))
        y = e(1) * sqrt(1.0_wp - e(2)**2) * sin(ea)
        ci = cos(e(3) * deg)
        si = sin(e(3) * deg)
        co = cos(e(4) * deg)
        so = sin(e(4) * deg)
        cw = cos(e(5) * deg)
        sw = sin(e(5) * deg)
        r = [(co * cw - so * sw * ci) * x + (-co * sw - so * cw * ci) * y, &
             (so * cw + co * sw * ci) * x + (-so * sw + co * cw * ci) * y, &
             (sw * si) * x + (cw * si) * y]
    end function orbit_position

end program orbit_planes
