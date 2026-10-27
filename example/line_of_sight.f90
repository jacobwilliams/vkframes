!> Lines of sight between spacecraft in different orbits (CustomLineSegments,
!> as OpenFrames' class of that name): five spacecraft (an ISS-like orbit, a
!> polar sun-synchronous orbit, a GPS-like orbit, a geostationary orbit and a
!> Molniya orbit), each a marker at its current position on its orbit, and a
!> line between every pair of them, recomputed each frame by a callback
!> (LineOfSight, an extension of LineSegmentsCallback): links with a clear line
!> of sight are drawn in the two spacecraft's colors (blended along the line),
!> links the Earth blocks (the line passes within 100 km of its surface: the
!> atmosphere) dashed and dim. Two CustomLineSegments share one callback type:
!> one keeps the visible links, the other the blocked ones. A HUD line counts
!> the visible links.
!>
!> Arguments (optional): number of frames to run, then a file to capture the
!> last frame to: `pixi run example line_of_sight -- 600 los.png`
module line_of_sight_links

    use vkframes

    implicit none

    real(wp), parameter :: re = 6378.137_wp                 !! Earth's radius (km)
    real(wp), parameter :: margin = 100.0_wp                !! above the surface still blocks (km)

    !> Lines of sight between every pair of `craft`: the visible ones, or (with
    !> `blocked`) the ones the Earth blocks; `n_visible` counts the visible ones.
    type, extends(LineSegmentsCallback) :: LineOfSight
        type(FramePointer), allocatable :: craft(:)
        real(wp), allocatable :: colors(:,:)                !! (4, craft): each spacecraft's
        logical :: blocked = .false.
        integer :: n_visible = 0
    contains
        procedure :: compute => line_of_sight_compute
    end type LineOfSight

contains

    !> The links for time t (all frames at their positions for t: this is
    !> called after the tree's update).
    subroutine line_of_sight_compute(self, segments, t)
        class(LineOfSight), intent(inout) :: self
        class(CustomLineSegments), intent(inout) :: segments
        real(wp), intent(in) :: t
        real(wp), allocatable :: p(:,:), a(:,:), b(:,:), ca(:,:), cb(:,:)
        real(wp) :: dim(4)
        integer :: n, i, j, k
        logical :: clear
        n = size(self%craft)
        allocate(p(3, n), a(3, n * (n - 1) / 2), b(3, n * (n - 1) / 2), &
                 ca(4, n * (n - 1) / 2), cb(4, n * (n - 1) / 2))
        do i = 1, n
            p(:, i) = segments%position_of(self%craft(i)%p)
        end do
        k = 0
        self%n_visible = 0
        dim = [0.55_wp, 0.55_wp, 0.6_wp, 0.5_wp]
        do i = 1, n - 1
            do j = i + 1, n
                clear = segment_clears_earth(p(:, i), p(:, j))
                if (clear) self%n_visible = self%n_visible + 1
                if (clear .eqv. self%blocked) cycle
                k = k + 1
                a(:, k) = p(:, i)
                b(:, k) = p(:, j)
                if (self%blocked) then
                    ca(:, k) = dim
                    cb(:, k) = dim
                else
                    ca(:, k) = self%colors(:, i)
                    cb(:, k) = self%colors(:, j)
                end if
            end do
        end do
        call segments%set_segments(a(:, 1:k), b(:, 1:k), ca(:, 1:k), cb(:, 1:k))
    end subroutine line_of_sight_compute

    !> The segment from p to q stays more than `margin` above the Earth (a
    !> sphere at the origin): its closest point to the center is far enough.
    pure logical function segment_clears_earth(p, q) result(clear)
        real(wp), intent(in) :: p(3), q(3)
        real(wp) :: d(3), s
        d = q - p
        s = 0.0_wp
        if (dot_product(d, d) > 0.0_wp) s = max(0.0_wp, min(1.0_wp, -dot_product(p, d) / dot_product(d, d)))
        clear = norm2(p + s * d) > re + margin
    end function segment_clears_earth

end module line_of_sight_links

program line_of_sight

    use vkframes
    use line_of_sight_links

    implicit none

    real(wp), parameter :: mu = 398600.4418_wp                       !! km^3/s^2
    real(wp), parameter :: deg = pi / 180.0_wp
    integer, parameter :: n_craft = 5, n_points = 721
    character(len=12), parameter :: names(n_craft) = [character(len=12) :: 'LEO 51.6', 'SSO', 'GPS', 'GEO', 'Molniya']
    ! orbits: semi-major axis (km), eccentricity, inclination, ascending node,
    ! argument of perigee (degrees); and colors
    real(wp), parameter :: elements(5, n_craft) = reshape([ &
        6778.0_wp, 0.0005_wp, 51.6_wp, 0.0_wp, 0.0_wp, &
        7078.0_wp, 0.001_wp, 98.2_wp, 60.0_wp, 0.0_wp, &
        26560.0_wp, 0.01_wp, 55.0_wp, 140.0_wp, 30.0_wp, &
        42164.0_wp, 0.0_wp, 0.0_wp, 0.0_wp, 0.0_wp, &
        26600.0_wp, 0.74_wp, 63.4_wp, 250.0_wp, 270.0_wp], [5, n_craft])
    real(wp), parameter :: colors(3, n_craft) = reshape([ &
        1.0_wp, 0.85_wp, 0.2_wp,  0.3_wp, 1.0_wp, 0.8_wp,  1.0_wp, 0.45_wp, 0.4_wp, &
        0.6_wp, 0.7_wp, 1.0_wp,  0.9_wp, 0.5_wp, 1.0_wp], [3, n_craft])

    type(ReferenceFrame), target :: root
    type(Sphere), target :: earth
    type(Trajectory), target :: orbits(n_craft)
    type(DrawableTrajectory), target :: orbit_frames(n_craft), craft(n_craft)
    type(CurveArtist), target :: curves(n_craft)
    type(MarkerArtist), target :: markers(n_craft)
    type(CustomLineSegments), target :: visible, blocked
    type(LineOfSight), target :: visible_links, blocked_links
    type(FrameManager), target :: scene
    type(WindowProxy), target :: window
    type(View), target :: earth_view
    type(HUDText), target :: count_text
    character(len=1024) :: arg, file
    character(len=64) :: text
    character(len=8) :: hours
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
    earth = Sphere('Earth', radius=re, color=[0.3_wp, 0.5_wp, 1.0_wp])
    call earth%set_texture_map('2k_earth_daymap.jpg')                    ! (data/, on VKFRAMES_DATA_PATH)
    call hide_decorations(earth)
    if (.not. root%add_child(earth)) error stop 'add_child failed'

    ! the spacecraft: an orbit (one period, drawn), and a marker at the current
    ! position (a marker artist without a trajectory, in a frame following it)
    do i = 1, n_craft
        period = 2.0_wp * pi * sqrt(elements(1, i)**3 / mu)
        orbits(i) = Trajectory()
        call orbits(i)%reserve(n_points)
        do k = 1, n_points
            t = period * real(k - 1, wp) / real(n_points - 1, wp)
            call orbits(i)%add(t, pos=orbit_position(t, elements(:, i)))
        end do
        curves(i) = CurveArtist(orbits(i), color=[colors(:, i), 0.6_wp], width=1.5_wp)
        orbit_frames(i) = DrawableTrajectory(trim(names(i))//' orbit')
        call hide_decorations(orbit_frames(i))
        call orbit_frames(i)%add_artist(curves(i))
        markers(i) = MarkerArtist(markers=MARKER_START, size=14.0_wp, color=colors(:, i))
        craft(i) = DrawableTrajectory(trim(names(i)), color=colors(:, i))
        call craft(i)%show_axes(NO_AXES)
        call craft(i)%show_axes_labels(NO_AXES)
        call craft(i)%add_artist(markers(i))
        call craft(i)%follow(orbits(i), data=FOLLOW_POSITION)
        if (.not. (root%add_child(orbit_frames(i)) .and. root%add_child(craft(i)))) error stop 'add_child failed'
    end do

    ! the lines of sight: visible ones in the spacecraft's colors, blocked ones dashed
    call setup_links(visible_links, blocked=.false.)
    call setup_links(blocked_links, blocked=.true.)
    visible = CustomLineSegments('Visible links', width=2.0_wp)
    call hide_decorations(visible)
    call visible%set_callback(visible_links)
    blocked = CustomLineSegments('Blocked links', width=1.0_wp)
    call hide_decorations(blocked)
    call blocked%set_pattern(2, int(z'0F0F'))
    call blocked%set_callback(blocked_links)
    if (.not. (root%add_child(visible) .and. root%add_child(blocked))) error stop 'add_child failed'
    scene = FrameManager(root)

    if (.not. window%open(1100, 800, 'vkframes: lines of sight')) error stop 'could not open a window'
    call window%set_scene(scene)
    call window%set_background_color([0.0_wp, 0.0_wp, 0.02_wp])
    call window%set_time_scale(600.0_wp)                                 ! 10 minutes a second
    call window%set_time(3.0_wp * 3600.0_wp)                             ! (the spacecraft spread out)
    earth_view = View(root, earth)
    call earth_view%set_default_view_parameters(eye=[90000.0_wp, -70000.0_wp, 50000.0_wp], &
                                                center=[0.0_wp, 0.0_wp, 0.0_wp], up=[0.0_wp, 0.0_wp, 1.0_wp])
    call window%add_view(earth_view)
    count_text = HUDText('', position=[0.01_wp, 0.01_wp], alignment=ALIGN_LEFT_BOTTOM, size=18.0_wp, &
                         color=[0.8_wp, 0.8_wp, 0.8_wp])
    call window%add_hud_text(count_text)

    running = .true.
    frame = 0
    do while (running .and. frame < frames)
        running = window%frame()
        frame = frame + 1
        write(hours, '(f8.2)') window%time() / 3600.0_wp
        write(text, '(a,i0,a,i0,a)') 'clear lines of sight: ', visible_links%n_visible, ' of ', &
            n_craft * (n_craft - 1) / 2, '   t = '//trim(adjustl(hours))//' h'
        call count_text%set_text(trim(text))
    end do
    if (running .and. len_trim(file) > 0) then
        if (window%capture(trim(file))) write(*, '(a)') 'captured '//trim(file)
    end if
    call window%close()

contains

    subroutine setup_links(links, blocked)
        type(LineOfSight), intent(inout) :: links
        logical, intent(in) :: blocked
        integer :: j
        allocate(links%craft(n_craft), links%colors(4, n_craft))
        do j = 1, n_craft
            links%craft(j)%p => craft(j)
            links%colors(:, j) = [colors(:, j), 1.0_wp]
        end do
        links%blocked = blocked
    end subroutine setup_links

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
        do k = 1, 30
            ea = ea - (ea - e(2) * sin(ea) - m) / (1.0_wp - e(2) * cos(ea))
        end do
        x = e(1) * (cos(ea) - e(2))                                      ! in the orbit plane, perigee along x
        y = e(1) * sqrt(1.0_wp - e(2)**2) * sin(ea)
        ci = cos(e(3) * deg); si = sin(e(3) * deg)
        co = cos(e(4) * deg); so = sin(e(4) * deg)
        cw = cos(e(5) * deg); sw = sin(e(5) * deg)
        r = [(co * cw - so * sw * ci) * x + (-co * sw - so * cw * ci) * y, &
             (so * cw + co * sw * ci) * x + (-so * sw + co * cw * ci) * y, &
             (sw * si) * x + (cw * si) * y]
    end function orbit_position

end program line_of_sight
