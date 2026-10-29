!> Fading trails (CurveArtist%set_trail, not in OpenFrames): three spacecraft
!> about the Earth, each drawn only near the current time, a solid trail
!> behind it (the past: 35% of its orbit, fading out over its oldest 30%)
!> and a dashed, translucent one ahead (the future: 15% of its orbit),
!> instead of its whole orbit.
!>
!> The GPU fades and cuts each line by its distance along it: each frame only
!> a few numbers per line change (the distances at the current time and at
!> the trails' ends), not the points.
!>
!> The three dash layouts (set_dash_mode), side by side: the low orbit's
!> dashes are anchored to the line (DASH_SCENE, the default: they stay put
!> on it, sized in scene units, changing by doubling as you zoom); the polar
!> orbit's a fixed length of the line (DASH_FIXED: as paint on it, they
!> never change, growing on screen as you zoom in); the elliptic orbit's
!> screen-locked
!> (DASH_SCREEN, as OpenGL's line stipple: every dash the same number of
!> pixels at any zoom, sliding along the line as the view changes).
!>
!> Each spacecraft's label says which its orbit has.
!>
!> Keys: t trails or the whole orbits, + / - the trails longer / shorter, d
!> the dash layouts passed on to the next orbit (the labels follow), r the
!> time's direction reversed, space resets the view.
!>
!> Arguments (optional): number of frames to run, then a file to capture the
!> last frame to: `pixi run example fading_trails -- 120 trails.png`
module fading_trails_keys

    use vkframes

    implicit none

    private

    integer, parameter, public :: n_craft = 3
    character(len=*), parameter, public :: names(n_craft) = [character(len=10) :: 'Low', 'Polar', 'Elliptic']
    character(len=*), parameter :: mode_names(0:2) = [character(len=24) :: 'anchored dashes', 'screen-locked dashes', &
                                                      'fixed-length dashes' ]   !! (by DASH_SCENE, DASH_SCREEN, DASH_FIXED)

    !> The keys (see the program's documentation).
    type, extends(WindowEventHandler), public :: TrailKeys
        type(CurveArtist), pointer :: curves(:) => null()
        type(Sphere), pointer :: craft(:) => null()             !! the spacecraft (their labels: the dash layout)
        real(wp) :: periods(n_craft) = 1.0_wp           !! each orbit's period (s)
        real(wp) :: scale = 1.0_wp                      !! the trails' length (times their default)
        logical :: on = .true.                          !! trails (else the whole orbits)
        integer :: modes(n_craft) = [DASH_SCENE, DASH_FIXED, DASH_SCREEN]   !! each orbit's dash layout
    contains
        procedure :: key_press => trail_key
        procedure :: apply
    end type TrailKeys

contains

    !> The trails (or none) on every curve: the past 35% of the orbit, the
    !> future 15%, times the scale; each orbit's dash layout, and its
    !> spacecraft's label saying which.
    subroutine apply(self)
        class(TrailKeys), intent(inout) :: self
        integer :: k
        do k = 1, size(self%curves)
            call self%curves(k)%set_dash_mode(self%modes(k))
            if (associated(self%craft)) call self%craft(k)%set_name(trim(names(k))//' ('//trim(mode_names(self%modes(k)))//')')
            if (self%on) then
                call self%curves(k)%set_trail(0.35_wp * self%scale * self%periods(k), 0.15_wp * self%scale * self%periods(k), &
                                              fade=0.3_wp)
            else
                call self%curves(k)%remove_trail()
            end if
        end do
    end subroutine apply

    subroutine trail_key(self, window, key, modifiers, handled)
        class(TrailKeys), intent(inout) :: self
        class(WindowProxy), intent(inout) :: window
        integer, intent(in) :: key, modifiers
        logical, intent(inout) :: handled
        handled = .true.
        select case (key)
        case (iachar('t'))
            self%on = .not. self%on
        case (iachar('+'), iachar('='))
            self%scale = min(2.5_wp, self%scale * 1.25_wp)
        case (iachar('-'))
            self%scale = max(0.1_wp, self%scale / 1.25_wp)
        case (iachar('d'))                              ! (each orbit's dash layout passed on to the next)
            self%modes = cshift(self%modes, -1)
        case (iachar('r'))
            call window%set_time_scale(-window%time_scale())
            return
        case default
            handled = .false.
            return
        end select
        call self%apply()
    end subroutine trail_key

end module fading_trails_keys

program fading_trails

    use vkframes
    use fading_trails_keys

    implicit none

    real(wp), parameter :: mu = 398600.4418_wp, re = 6378.137_wp           !! km^3/s^2, km
    real(wp), parameter :: deg = pi / 180.0_wp
    real(wp), parameter :: hours = 10.0_wp * 3600.0_wp                     !! the trajectories' span (s)
    real(wp), parameter :: a(n_craft) = [7000.0_wp, 9500.0_wp, 16000.0_wp]          !! km
    real(wp), parameter :: e(n_craft) = [0.0_wp, 0.02_wp, 0.5_wp]
    real(wp), parameter :: inc(n_craft) = [28.0_wp, 88.0_wp, 55.0_wp] * deg
    real(wp), parameter :: raan(n_craft) = [0.0_wp, 60.0_wp, 200.0_wp] * deg
    real(wp), parameter :: colors(3, n_craft) = reshape([1.0_wp, 0.6_wp, 0.2_wp, 0.3_wp, 0.85_wp, 1.0_wp, &
                                                         0.6_wp, 1.0_wp, 0.4_wp], [3, n_craft])

    type(Sphere), target :: earth
    type(Trajectory), target :: orbits(n_craft)
    type(CurveArtist), target :: curves(n_craft)
    type(DrawableTrajectory), target :: frames_(n_craft)
    type(Sphere), target :: craft(n_craft)
    type(FrameManager), target :: scene
    type(WindowProxy), target :: window
    type(View), target :: earth_view
    type(HUDText), target :: hud, legend
    type(TrailKeys), target :: keys
    character(len=1024) :: arg, file
    character(len=200) :: text
    real(wp) :: t, period
    integer :: k, i, n, frames, frame
    logical :: running

    frames = huge(frames)
    file = ''
    if (command_argument_count() >= 1) then
        call get_command_argument(1, arg)
        read(arg, *) frames
    end if
    if (command_argument_count() >= 2) call get_command_argument(2, file)

    earth = Sphere('Earth', radius=re)
    call earth%set_texture_map('2k_earth_daymap.jpg')
    call earth%show_decorations(.false.)
    do k = 1, n_craft
        period = 2.0_wp * pi * sqrt(a(k)**3 / mu)
        keys%periods(k) = period
        n = nint(hours / period * 360.0_wp)                 ! (a point per degree of mean anomaly)
        orbits(k) = Trajectory()
        do i = 0, n
            t = hours * i / n
            call orbits(k)%add(t, pos=kepler_position(k, t))
        end do
        curves(k) = CurveArtist(orbits(k), color=colors(:, k), width=2.5_wp)
        call curves(k)%set_trail_style(TRAIL_PAST, opacity=1.0_wp)                          ! solid
        call curves(k)%set_trail_style(TRAIL_FUTURE, opacity=0.6_wp, factor=3, pattern=int(z'00FF'))   ! dashed
        frames_(k) = DrawableTrajectory(trim(names(k))//' orbit')
        call frames_(k)%show_decorations(.false.)
        call frames_(k)%add_artist(curves(k))
        craft(k) = Sphere(trim(names(k)), radius=180.0_wp, color=colors(:, k))     ! (labeled by keys%apply)
        call craft(k)%show_axes(NO_AXES)
        call craft(k)%show_axes_labels(NO_AXES)
        call craft(k)%follow(orbits(k), mode=FOLLOW_LIMIT)
        if (.not. (earth%add_child(frames_(k)) .and. earth%add_child(craft(k)))) error stop 'add_child failed'
    end do
    keys%curves => curves
    keys%craft => craft
    call keys%apply()
    scene = FrameManager(earth)

    if (.not. window%open(1100, 800, 'vkframes: fading trails')) error stop 'could not open a window'
    call window%set_scene(scene)
    call window%set_background_color([0.0_wp, 0.0_wp, 0.02_wp])
    earth_view = View(earth, earth)
    call earth_view%set_default_view_distance(5.0_wp * a(n_craft))
    call window%add_view(earth_view)
    call window%set_camera([1.2_wp, -3.2_wp, 1.6_wp] * a(n_craft), [0.0_wp, 0.0_wp, 0.0_wp], [0.0_wp, 0.0_wp, 1.0_wp], &
                           in_root=.true.)
    hud = HUDText('', position=[0.01_wp, 0.01_wp], alignment=ALIGN_LEFT_BOTTOM, size=15.0_wp, &
                  color=[0.85_wp, 0.85_wp, 0.85_wp])
    call window%add_hud_text(hud)
    legend = HUDText('solid: the past (fading out)    dashed: the future' // new_line('a') // &
                     't: trails / whole orbits    + / -: longer / shorter    d: dash layouts rotated    ' // &
                     'r: reverse time', &
                     position=[0.01_wp, 0.99_wp], alignment=ALIGN_LEFT_TOP, size=14.0_wp, color=[0.7_wp, 0.7_wp, 0.7_wp])
    call window%add_hud_text(legend)
    call window%set_event_handler(keys)
    call window%set_time_range(0.0_wp, hours)
    call window%set_time_scale(keys%periods(1) / 10.0_wp)   ! (the low orbit in 10 s)
    call window%set_time(0.3_wp * hours)

    running = .true.
    frame = 0
    do while (running .and. frame < frames)
        if (keys%on) then
            write(text, '(a,f6.2,a,i0,a,i0,a)') 't = ', window%time() / 3600.0_wp, ' h    trails: the past ', &
                nint(35.0_wp * keys%scale), '% and the future ', nint(15.0_wp * keys%scale), '% of each orbit'
        else
            write(text, '(a,f6.2,a)') 't = ', window%time() / 3600.0_wp, ' h    whole orbits'
        end if
        call hud%set_text(trim(text))
        running = window%frame()
        frame = frame + 1
    end do
    if (running .and. len_trim(file) > 0) then
        if (window%capture(trim(file))) write(*, '(a)') 'captured '//trim(file)
    end if
    call window%close()

contains

    !> Spacecraft k's position (km) at time t: a Kepler orbit (periapsis at t = 0).
    function kepler_position(k, t) result(p)
        integer, intent(in) :: k
        real(wp), intent(in) :: t
        real(wp) :: p(3), m, ea, nu, r, x, y, n_mean
        integer :: j
        n_mean = sqrt(mu / a(k)**3)
        m = n_mean * t
        ea = m
        do j = 1, 30
            ea = ea - (ea - e(k) * sin(ea) - m) / (1.0_wp - e(k) * cos(ea))
        end do
        nu = 2.0_wp * atan2(sqrt(1.0_wp + e(k)) * sin(0.5_wp * ea), sqrt(1.0_wp - e(k)) * cos(0.5_wp * ea))
        r = a(k) * (1.0_wp - e(k) * cos(ea))
        x = r * cos(nu)
        y = r * sin(nu)
        p = [x * cos(raan(k)) - y * cos(inc(k)) * sin(raan(k)), x * sin(raan(k)) + y * cos(inc(k)) * cos(raan(k)), &
             y * sin(inc(k))]
    end function kepler_position

end program fading_trails
