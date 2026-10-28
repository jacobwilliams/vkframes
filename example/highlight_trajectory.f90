!> Highlighting a selected trajectory (CurveArtist%set_highlight, not in
!> OpenFrames): four Earth orbits (a parking orbit, a transfer to
!> geostationary orbit, part of the geostationary orbit, a Molniya orbit);
!> the program selects the next one every few seconds, and a click selects
!> the one under the pointer (WindowProxy%pick).
!>
!> * The selected orbit pulses (wider and brighter, and back) and has arrows
!>   moving along it from its initial time to its final, in a translucent
!>   band: HIGHLIGHT_PULSE + HIGHLIGHT_ARROWS. The GPU animates them: nothing
!>   is uploaded each frame.
!> * m: the highlight's mode (pulse and arrows, pulse, arrows).
!>
!> Arguments (optional): number of frames to run, a file to capture the last
!> frame to, and an orbit to select (1 to 4; the program doesn't cycle):
!> `pixi run example highlight_trajectory -- 300 highlight.png 2`
module highlight_trajectory_handler

    use vkframes

    implicit none

    private

    integer, parameter, public :: n_orbits = 4

    !> Selects the orbit clicked; m changes the highlight's mode.
    type, extends(WindowEventHandler), public :: Selector
        type(CurveArtist), pointer :: curves(:) => null()   !! the orbits' curves
        integer :: selected = 0                             !! the orbit selected (0: none)
        integer :: mode = HIGHLIGHT_PULSE + HIGHLIGHT_ARROWS
        logical :: clicked = .false.                        !! the user selected one (the program stops cycling)
    contains
        procedure :: button_press => selector_press
        procedure :: key_press => selector_key
        procedure :: select => selector_select
    end type Selector

contains

    !> Highlight orbit k only (0: none).
    subroutine selector_select(self, k)
        class(Selector), intent(inout) :: self
        integer, intent(in) :: k
        if (self%selected > 0) call self%curves(self%selected)%set_highlight(HIGHLIGHT_NONE)
        self%selected = k
        if (k > 0) call self%curves(k)%set_highlight(self%mode, color=[1.0_wp, 1.0_wp, 0.85_wp])
    end subroutine selector_select

    subroutine selector_press(self, window, x, y, button)
        class(Selector), intent(inout) :: self
        class(WindowProxy), intent(inout) :: window
        real(wp), intent(in) :: x, y
        integer, intent(in) :: button
        type(PickHit), allocatable :: hits(:)
        integer :: k
        if (button /= MOUSE_LEFT) return
        call window%pick(x, y, hits, kinds=[PICK_CURVE], nearest=.true.)
        if (size(hits) == 0) return
        do k = 1, n_orbits
            select type (a => hits(1)%artist)
            class is (CurveArtist)
                if (associated(a, self%curves(k))) then
                    call self%select(k)
                    self%clicked = .true.
                end if
            end select
        end do
    end subroutine selector_press

    subroutine selector_key(self, window, key, modifiers, handled)
        class(Selector), intent(inout) :: self
        class(WindowProxy), intent(inout) :: window
        integer, intent(in) :: key, modifiers
        logical, intent(inout) :: handled
        if (key /= ichar('m')) return
        select case (self%mode)
        case (HIGHLIGHT_PULSE + HIGHLIGHT_ARROWS)
            self%mode = HIGHLIGHT_PULSE
        case (HIGHLIGHT_PULSE)
            self%mode = HIGHLIGHT_ARROWS
        case default
            self%mode = HIGHLIGHT_PULSE + HIGHLIGHT_ARROWS
        end select
        call self%select(self%selected)
        handled = .true.
    end subroutine selector_key

end module highlight_trajectory_handler

program highlight_trajectory

    use, intrinsic :: iso_fortran_env, only: int64
    use vkframes
    use highlight_trajectory_handler

    implicit none

    real(wp), parameter :: re = 6378.137_wp, mu = 398600.4418_wp     !! km, km^3/s^2
    real(wp), parameter :: deg = pi / 180.0_wp
    real(wp), parameter :: r_leo = 7000.0_wp, r_geo = 42164.0_wp
    real(wp), parameter :: dwell = 6.0_wp                            !! s between selections
    character(len=*), parameter :: names(n_orbits) = [character(len=24) :: 'parking orbit', &
        'transfer orbit', 'geostationary orbit', 'Molniya orbit']
    character(len=*), parameter :: mode_names(3) = [character(len=16) :: 'pulse', 'arrows', 'pulse and arrows']

    type(Sphere), target :: earth
    type(Trajectory), target :: orbits(n_orbits)
    type(CurveArtist), target :: curves(n_orbits)
    type(DrawableTrajectory), target :: drawn
    type(FrameManager), target :: scene
    type(WindowProxy), target :: window
    type(View), target :: earth_view
    type(HUDText), target :: hud
    type(Selector), target :: selector_
    character(len=1024) :: arg, file
    real(wp) :: colors(3, n_orbits), elapsed, next
    integer(int64) :: start, count, rate
    integer :: k, frames, frame, shown, shown_mode, fixed
    logical :: running

    frames = huge(frames)
    file = ''
    if (command_argument_count() >= 1) then
        call get_command_argument(1, arg)
        read(arg, *) frames
    end if
    if (command_argument_count() >= 2) call get_command_argument(2, file)
    fixed = 0
    if (command_argument_count() >= 3) then
        call get_command_argument(3, arg)
        read(arg, *) fixed
    end if

    earth = Sphere('Earth', radius=re)
    call earth%set_texture_map('2k_earth_daymap.jpg')                    ! (data/, on VKFRAMES_DATA_PATH)
    call earth%show_decorations(.false.)

    ! each orbit from true anomaly nu0 to nu1, with Kepler's times
    call kepler_arc(orbits(1), r_leo, 0.0_wp, 28.5_wp * deg, 0.0_wp, 0.0_wp, 360.0_wp * deg)
    call kepler_arc(orbits(2), 0.5_wp * (r_leo + r_geo), (r_geo - r_leo) / (r_geo + r_leo), 28.5_wp * deg, 0.0_wp, &
                    0.0_wp, 180.0_wp * deg)
    call kepler_arc(orbits(3), r_geo, 0.0_wp, 0.0_wp, 180.0_wp * deg, 0.0_wp, 240.0_wp * deg)
    call kepler_arc(orbits(4), 26600.0_wp, 0.74_wp, 63.4_wp * deg, 270.0_wp * deg, 0.0_wp, 360.0_wp * deg)
    colors = reshape([0.3_wp, 0.7_wp, 1.0_wp, 1.0_wp, 0.55_wp, 0.2_wp, 0.4_wp, 0.9_wp, 0.4_wp, &
                      0.9_wp, 0.4_wp, 0.9_wp], [3, n_orbits])
    drawn = DrawableTrajectory('Orbits')
    call drawn%show_decorations(.false.)
    do k = 1, n_orbits
        curves(k) = CurveArtist(orbits(k), color=colors(:, k), width=2.0_wp)
        call drawn%add_artist(curves(k))
    end do
    if (.not. earth%add_child(drawn)) error stop 'add_child failed'
    scene = FrameManager(earth)

    if (.not. window%open(1100, 750, 'vkframes: highlighting a trajectory')) error stop 'could not open a window'
    call window%set_scene(scene)
    call window%set_background_color([0.0_wp, 0.0_wp, 0.02_wp])
    earth_view = View(earth, earth)
    call earth_view%set_default_view_distance(1.6e5_wp)
    call window%add_view(earth_view)
    call window%set_camera([4.0e4_wp, -1.3e5_wp, 6.0e4_wp], [0.0_wp, 0.0_wp, 0.0_wp], [0.0_wp, 0.0_wp, 1.0_wp], &
                           in_root=.true.)
    hud = HUDText('', position=[0.01_wp, 0.01_wp], alignment=ALIGN_LEFT_BOTTOM, size=16.0_wp, &
                  color=[0.85_wp, 0.85_wp, 0.85_wp])
    call window%add_hud_text(hud)
    selector_%curves => curves
    call window%set_event_handler(selector_)
    if (fixed >= 1 .and. fixed <= n_orbits) then
        call selector_%select(fixed)
        selector_%clicked = .true.
    end if

    call system_clock(start, rate)
    next = 0.0_wp
    shown = -1
    shown_mode = -1
    running = .true.
    frame = 0
    do while (running .and. frame < frames)
        call system_clock(count)
        elapsed = real(count - start, wp) / real(rate, wp)
        if (.not. selector_%clicked .and. elapsed >= next) then      ! the program selects the next orbit
            call selector_%select(mod(selector_%selected, n_orbits) + 1)
            next = next + dwell
        end if
        if (selector_%selected /= shown .or. selector_%mode /= shown_mode) then
            shown = selector_%selected
            shown_mode = selector_%mode
            call hud%set_text('selected: '//trim(names(max(shown, 1)))//' ('//trim(mode_names(shown_mode))// &
                              ')   click: select an orbit   m: the highlight')
        end if
        running = window%frame()
        frame = frame + 1
    end do
    if (running .and. len_trim(file) > 0) then
        if (window%capture(trim(file))) write(*, '(a)') 'captured '//trim(file)
    end if
    call window%close()

contains

    !> traj: the orbit of semi-major axis a (km) and eccentricity e, at
    !> inclination inc, argument of periapsis argp, from true anomaly nu0 to
    !> nu1 (rad; nu1 - nu0 at most 2 pi), at times from Kepler's equation
    !> (from 0 at nu0).
    subroutine kepler_arc(traj, a, e, inc, argp, nu0, nu1)
        type(Trajectory), intent(inout) :: traj
        real(wp), intent(in) :: a, e, inc, argp, nu0, nu1
        integer, parameter :: n = 721
        real(wp) :: nu, r, ecc_anomaly, m, m0, m_last, t, p(3), u
        integer :: i
        traj = Trajectory()
        m0 = 0.0_wp
        m_last = 0.0_wp
        do i = 0, n - 1
            nu = nu0 + (nu1 - nu0) * i / real(n - 1, wp)
            r = a * (1.0_wp - e**2) / (1.0_wp + e * cos(nu))
            ecc_anomaly = 2.0_wp * atan2(sqrt(1.0_wp - e) * sin(0.5_wp * nu), sqrt(1.0_wp + e) * cos(0.5_wp * nu))
            m = ecc_anomaly - e * sin(ecc_anomaly)
            if (i == 0) m0 = m
            if (i > 0) m = m + 2.0_wp * pi * real(ceiling((m_last - m) / (2.0_wp * pi)), wp)   ! (unwrapped)
            m_last = m
            t = (m - m0) / sqrt(mu / a**3)
            u = argp + nu
            p = r * [cos(u), sin(u) * cos(inc), sin(u) * sin(inc)]
            call traj%add(t, pos=p)
        end do
    end subroutine kepler_arc

end program highlight_trajectory
