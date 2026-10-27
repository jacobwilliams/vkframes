!> Trajectories with a color per point (Trajectory's `color`, drawn by
!> CurveArtist; not in OpenFrames): each segment is blended from its first
!> point's color to its second's, so points of one color give a line of that
!> color and a change of color a smooth transition.
!>
!> * A Molniya orbit colored by altitude: a smooth gradient from blue at
!>   perigee through green and yellow to red at apogee.
!> * A low orbit colored by sunlight: yellow while sunlit, dark blue while in
!>   the Earth's umbra (the arcs line up with the Earth's ShadowCones, shown),
!>   traced up to the current time (the tail's colors interpolated too).
!>
!> Arguments (optional): number of frames to run, then a file to capture the
!> last frame to: `pixi run example colored_orbits -- 300 colored.png`
program colored_orbits

    use vkframes

    implicit none

    real(wp), parameter :: mu = 398600.4418_wp                  !! km^3/s^2
    real(wp), parameter :: re = 6378.137_wp, rs = 695700.0_wp   !! km
    real(wp), parameter :: au = 1.495978707e8_wp                !! km
    real(wp), parameter :: deg = pi / 180.0_wp
    integer, parameter :: n_molniya = 2000, n_leo = 1000, leo_orbits = 10
    real(wp), parameter :: sun_dir(3) = [-0.6_wp, -0.8_wp, 0.0_wp]  !! toward the Sun (unit)

    type(ReferenceFrame), target :: root, sun
    type(Sphere), target :: earth
    type(ShadowCones), target :: shadow
    type(Trajectory), target :: molniya, leo
    type(DrawableTrajectory), target :: molniya_frame, leo_frame
    type(CurveArtist), target :: molniya_curve, leo_curve
    type(FrameManager), target :: scene
    type(WindowProxy), target :: window
    type(View), target :: earth_view
    type(HUDText), target :: legend
    character(len=1024) :: arg, file
    real(wp) :: period, t, p(3), r_min, r_max
    real(wp), allocatable :: times(:), pos(:,:), colors(:,:)
    integer :: k, frames, frame
    logical :: running

    frames = huge(frames)
    file = ''
    if (command_argument_count() >= 1) then
        call get_command_argument(1, arg)
        read(arg, *) frames
    end if
    if (command_argument_count() >= 2) call get_command_argument(2, file)

    ! the Molniya orbit, one period, colored by altitude (added in bulk)
    period = 2.0_wp * pi * sqrt(26600.0_wp**3 / mu)
    allocate(times(n_molniya), pos(3, n_molniya), colors(3, n_molniya))
    do k = 1, n_molniya
        times(k) = period * real(k - 1, wp) / real(n_molniya - 1, wp)
        pos(:, k) = orbit_position(times(k), [26600.0_wp, 0.74_wp, 63.4_wp, 40.0_wp, 270.0_wp])
    end do
    r_min = minval(norm2(pos, dim=1))
    r_max = maxval(norm2(pos, dim=1))
    do k = 1, n_molniya
        colors(:, k) = color_map((norm2(pos(:, k)) - r_min) / (r_max - r_min))
    end do
    molniya = Trajectory()
    call molniya%add(times, pos, color=colors)

    ! the low orbit, ten periods, colored by sunlight (added one point at a time)
    period = 2.0_wp * pi * sqrt(7000.0_wp**3 / mu)
    leo = Trajectory()
    call leo%reserve(n_leo * leo_orbits, attitudes=.false., colors=.true.)
    do k = 1, n_leo * leo_orbits
        t = period * real(k - 1, wp) / real(n_leo, wp)
        p = orbit_position(t, [7000.0_wp, 0.0_wp, 30.0_wp, 60.0_wp, 0.0_wp])
        if (in_umbra(p)) then
            call leo%add(t, pos=p, color=[0.15_wp, 0.25_wp, 0.8_wp])
        else
            call leo%add(t, pos=p, color=[1.0_wp, 0.85_wp, 0.2_wp])
        end if
    end do

    root = ReferenceFrame('Inertial')
    call hide_decorations(root)
    earth = Sphere('Earth', radius=re)
    call earth%set_texture_map('2k_earth_daymap.jpg')                    ! (data/, on VKFRAMES_DATA_PATH)
    call earth%set_night_texture_map('2k_earth_nightmap.jpg')
    call hide_decorations(earth)
    sun = ReferenceFrame('Sun')
    call sun%set_position(au * sun_dir)
    call hide_decorations(sun)
    call sun%set_light_source_enabled(.true.)
    call sun%set_light_diffuse([1.0_wp, 1.0_wp, 1.0_wp])
    call sun%set_light_ambient([0.06_wp, 0.06_wp, 0.06_wp])
    shadow = ShadowCones('Earth shadow', body=earth, light=sun, light_radius=rs)
    call shadow%set_length(8.0_wp * re)
    call shadow%set_regions(SHADOW_UMBRA)
    call shadow%set_region_color(SHADOW_UMBRA, [0.45_wp, 0.5_wp, 0.7_wp, 0.15_wp])

    molniya_curve = CurveArtist(molniya, width=3.0_wp)                    ! (the points' colors)
    molniya_frame = DrawableTrajectory('Molniya')
    call hide_decorations(molniya_frame)
    call molniya_frame%add_artist(molniya_curve)
    leo_curve = CurveArtist(leo, width=3.0_wp)
    call leo_curve%set_trace_mode(.true.)
    leo_frame = DrawableTrajectory('LEO')
    call hide_decorations(leo_frame)
    call leo_frame%add_artist(leo_curve)
    if (.not. (root%add_child(earth) .and. root%add_child(sun) .and. earth%add_child(shadow) .and. &
               root%add_child(molniya_frame) .and. root%add_child(leo_frame))) error stop 'add_child failed'
    scene = FrameManager(root)

    if (.not. window%open(1100, 800, 'vkframes: colored trajectories')) error stop 'could not open a window'
    call window%set_scene(scene)
    call window%set_background_color([0.0_wp, 0.0_wp, 0.02_wp])
    call window%set_time_scale(300.0_wp)                                 ! 5 minutes a second
    call window%set_time(0.8_wp * period)                                ! (most of the low orbit traced)
    earth_view = View(root, earth)
    call earth_view%set_default_view_parameters(eye=[70000.0_wp, -52000.0_wp, 38000.0_wp], &
                                                center=[0.0_wp, 0.0_wp, 6000.0_wp], up=[0.0_wp, 0.0_wp, 1.0_wp])
    call window%add_view(earth_view)
    legend = HUDText('Molniya orbit: altitude (blue at perigee to red at apogee)' // new_line('a') // &
                     "low orbit: sunlit (yellow), in the Earth's umbra (blue)", position=[0.01_wp, 0.01_wp], &
                     alignment=ALIGN_LEFT_BOTTOM, size=18.0_wp, color=[0.8_wp, 0.8_wp, 0.8_wp])
    call window%add_hud_text(legend)

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

    !> A color for x in [0, 1]: blue, cyan, green, yellow, red.
    pure function color_map(x) result(c)
        real(wp), intent(in) :: x
        real(wp) :: c(3)
        real(wp), parameter :: stops(3, 5) = reshape([0.2_wp, 0.3_wp, 1.0_wp, 0.0_wp, 0.9_wp, 1.0_wp, &
                                                       0.2_wp, 1.0_wp, 0.3_wp, 1.0_wp, 0.9_wp, 0.1_wp, &
                                                       1.0_wp, 0.15_wp, 0.1_wp], [3, 5])
        real(wp) :: s
        integer :: i
        s = 4.0_wp * max(0.0_wp, min(1.0_wp, x))
        i = min(4, int(s) + 1)
        s = s - real(i - 1, wp)
        c = stops(:, i) + s * (stops(:, i + 1) - stops(:, i))
    end function color_map

    !> Whether p (km, from the Earth's center) is in the Earth's umbra: behind
    !> the Earth from the Sun, within the cone narrowing at asin((Rs - Re) / AU).
    pure logical function in_umbra(p)
        real(wp), intent(in) :: p(3)
        real(wp) :: along, radial
        along = -dot_product(p, sun_dir)
        radial = norm2(p + along * sun_dir)
        in_umbra = along > 0.0_wp .and. radial < re - along * tan(asin((rs - re) / au))
    end function in_umbra

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

end program colored_orbits
