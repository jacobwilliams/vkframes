!> The halo demo's event handler: keys, picking a trajectory (its
!> highlight, readout and details) and the Jacobi constant's panel (see
!> main.f90).
module halo_manifolds_keys

    use vkframes

    implicit none

    private

    integer, parameter, public :: n_tubes = 2
    integer, parameter, public :: n_lines = 12          !! trajectories drawn as lines (pickable), per tube

    public :: center_on, place_axes

    !> What the readout says about a trajectory drawn as a line.
    type, public :: LineInfo
        integer :: index = 0                            !! its number around the orbit (1 to n_around)
        integer :: n_around = 1
        real(wp) :: closest = 0.0_wp                    !! its closest approach to the Earth's center (km)
        real(wp) :: closest_time = 0.0_wp               !! then (days)
        real(wp) :: jacobi = 0.0_wp                     !! its Jacobi constant at the orbit
        real(wp) :: drift = 0.0_wp                      !! its change along the trajectory (integration error)
        logical :: impact = .false.                     !! it reaches the Earth's surface (at closest_time)
    end type LineInfo

    !> The keys and the mouse (see the program's documentation).
    type, extends(WindowEventHandler), public :: ManifoldKeys
        type(Polyhedron), pointer :: tubes(:) => null()
        type(Polyhedron), pointer :: curves => null()  !! the zero-velocity curves
        type(CurveArtist), pointer :: lines(:,:) => null()          !! (n_lines, n_tubes)
        type(DrawableTrajectory), pointer :: line_frames(:) => null()  !! each tube's lines
        type(LineInfo) :: info(n_lines, n_tubes)
        character(len=24) :: names(n_tubes) = ''
        real(wp) :: line_colors(4, n_tubes) = 1.0_wp
        type(ReferenceFrame), pointer :: selection => null()        !! moved to the point picked; its label: the readout
        type(DrawableTrajectory), pointer :: ring => null()         !! the ring there
        type(HUDText), pointer :: details => null()                 !! the trajectory picked, in detail
        type(CoordinateAxes), pointer :: axes => null()             !! ticked axes at the center (center_on)
        logical :: axes_shown = .true.
        real(wp) :: earth(3) = 0.0_wp, re = 1.0_wp      !! the Earth's center (km) and radius
        real(wp) :: moon(3) = 0.0_wp                    !! the Moon's center (km)
        real(wp) :: lagrange(3) = 0.0_wp                !! the Lagrange point's (km)
        real(wp) :: lu = 1.0_wp                         !! the Earth-Moon distance (km)
        real(wp), allocatable :: colors(:,:,:)          !! each tube's vertex colors (4, n, tube)
        logical :: shown(n_tubes) = .true., curves_shown = .true.
        logical :: edges = .true., lit = .true., smooth = .true., fading = .true.
        real(wp) :: opacity = 0.6_wp
        integer :: hovered(2) = 0, selected(2) = 0      !! (line, tube), 0: none
        ! the panel: the Lagrange point (the menu), the halo's z amplitude
        ! (km) and Jacobi constant asked for (the sliders: the program
        ! computes the orbit and its manifolds again when one changes; each
        ! slider's range the point's family's), and the orbit's
        integer :: point_requested = 1, point = 1
        real(wp) :: az_requested = 0.0_wp, az_min = 1.0_wp, az_max = 1.0_wp
        real(wp) :: c_requested = 0.0_wp, c_min = 3.03_wp, c_max = 3.17_wp
        ! the propagation time (days): asked for (the slider; the manifolds
        ! computed again when it changes), and the manifolds'
        real(wp) :: days_requested = 0.0_wp, days = 0.0_wp, days_min = 2.0_wp, days_max = 120.0_wp
        real(wp) :: c = 0.0_wp, period_days = 0.0_wp, az = 0.0_wp, seconds = 0.0_wp, continued = 0.0_wp
        real(wp) :: lambda_u = 1.0_wp                   !! the orbit's unstable eigenvalue (its monodromy matrix's)
        integer :: threads = 1
        character(len=80) :: status = ''
    contains
        procedure :: key_press => manifold_key
        procedure :: mouse_motion => manifold_motion
        procedure :: button_press => manifold_press
        procedure :: before_draw => manifold_panel
        procedure :: apply
        procedure :: select => select_line
        procedure :: clear_selection
    end type ManifoldKeys

    real(wp), parameter :: line_width = 1.2_wp, hover_width = 3.0_wp, selected_width = 2.5_wp

contains

    !> The settings, on every tube.
    subroutine apply(self)
        class(ManifoldKeys), intent(inout) :: self
        integer :: k
        if (associated(self%curves)) call self%curves%show_contents(self%curves_shown)
        do k = 1, size(self%tubes)
            call self%tubes(k)%show_contents(self%shown(k))
            call self%line_frames(k)%show_contents(self%shown(k) .and. self%edges)
            call self%tubes(k)%set_lit(self%lit)
            call self%tubes(k)%set_smooth(self%smooth)
            call self%tubes(k)%set_opacity(self%opacity)
            if (self%fading) then
                call self%tubes(k)%set_vertex_colors(self%colors(:, :, k))
            else
                call self%tubes(k)%remove_vertex_colors()
            end if
        end do
    end subroutine apply

    subroutine manifold_key(self, window, key, modifiers, handled)
        class(ManifoldKeys), intent(inout) :: self
        class(WindowProxy), intent(inout) :: window
        integer, intent(in) :: key, modifiers
        logical, intent(inout) :: handled
        handled = .true.
        select case (key)
        case (iachar('1'):iachar('0') + n_tubes)
            self%shown(key - iachar('0')) = .not. self%shown(key - iachar('0'))
        case (iachar('z'))
            self%curves_shown = .not. self%curves_shown
        case (iachar('x'))
            self%axes_shown = .not. self%axes_shown
            if (associated(self%axes)) call self%axes%show_contents(self%axes_shown)
            return
        case (iachar('e'))
            self%edges = .not. self%edges
        case (iachar('l'))
            self%lit = .not. self%lit
        case (iachar('s'))
            self%smooth = .not. self%smooth
        case (iachar('c'))
            self%fading = .not. self%fading
        case (iachar('+'), iachar('='))
            self%opacity = min(1.0_wp, self%opacity + 0.1_wp)
        case (iachar('-'))
            self%opacity = max(0.1_wp, self%opacity - 0.1_wp)
        case default
            handled = .false.
            return
        end select
        call self%apply()
    end subroutine manifold_key

    !> Each frame: the Jacobi constant's panel (top right).
    subroutine manifold_panel(self, window, t)
        class(ManifoldKeys), intent(inout) :: self
        class(WindowProxy), intent(inout) :: window
        real(wp), intent(in) :: t
        type(Gui) :: g
        character(len=80) :: text
        real(wp) :: value
        integer :: point
        g = window%gui()
        call g%begin_panel('Halo orbit', position=[0.99_wp, 0.92_wp], pivot=[1.0_wp, 0.0_wp])
        point = self%point_requested
        if (g%combo('Lagrange point', point, [character(len=2) :: 'L1', 'L2', 'L3'])) self%point_requested = point
        value = self%az_requested
        if (g%slider('amplitude Az', value, self%az_min, self%az_max, format='%.0f km')) self%az_requested = value
        value = self%c_requested
        if (g%slider('Jacobi constant', value, self%c_min, self%c_max, format='%.5f')) self%c_requested = value
        value = self%days_requested
        if (g%slider('propagation', value, self%days_min, self%days_max, format='%.1f days')) &
            self%days_requested = value
        write(text, '(a,f9.6,a,f6.2,a,i0,a)') 'C = ', self%c, ', period ', self%period_days, ' days, Az ', &
            nint(self%az), ' km'
        call g%text(trim(text))
        write(text, '(a,es10.3,a,f9.2,a)') 'unstable eigenvalue ', self%lambda_u, ' (stability index ', &
            0.5_wp * (self%lambda_u + 1.0_wp / self%lambda_u), ')'
        call g%text(trim(text))
        write(text, '(a,f6.3,a,f6.3,a,i0,a)') 'orbit continued in ', self%continued, ' s, manifolds in ', &
            self%seconds, ' s (', self%threads, ' threads)'
        call g%text(trim(text))
        if (len_trim(self%status) > 0) call g%text(trim(self%status))
        ! the camera: flown to (and then turning about) the Earth, the Moon,
        ! the Lagrange point or the point picked, looking from where it is
        call g%separator()
        call g%text('center on:')
        call g%same_line()
        if (g%button('Earth')) call center_on(self, window, self%earth, 0.8_wp)
        call g%same_line()
        if (g%button('Moon')) call center_on(self, window, self%moon, 0.5_wp)
        call g%same_line()
        if (g%button('L'//achar(iachar('0') + self%point))) &
            call center_on(self, window, self%lagrange, merge(1.6_wp, 0.6_wp, self%point == 3))
        if (self%selected(1) > 0) then
            call g%same_line()
            if (g%button('the point picked')) call center_on(self, window, self%selection%position(), 0.25_wp)
        end if
        call g%end_panel()
    end subroutine manifold_panel

    !> Fly the camera to `p` (root frame, km): centered on it (the trackball
    !> then turns about it), from the direction it looks from now, `distance`
    !> Earth-Moon distances away; the ticked axes moved there (place_axes).
    subroutine center_on(self, window, p, distance)
        type(ManifoldKeys), intent(in) :: self
        class(WindowProxy), intent(inout) :: window
        real(wp), intent(in) :: p(3), distance
        real(wp) :: eye(3), center(3), up(3), d(3)
        call window%get_camera(eye, center, up, in_root=.true.)
        d = eye - center
        if (norm2(d) <= 0.0_wp) d = [0.0_wp, -1.0_wp, 1.0_wp]
        call window%set_camera(p + distance * self%lu * d / norm2(d), p, up, duration=1.2_wp, in_root=.true.)
        call place_axes(self, p, distance)
    end subroutine center_on

    !> The ticked axes at `p` (root frame, km), sized for a view `distance`
    !> Earth-Moon distances away: major ticks at a round spacing (1, 2 or 5
    !> times a power of ten km) about a quarter of 0.6 of it, minor ones a
    !> fifth of the major, and the axes a whole number of major ticks long
    !> (the nearest to 0.6 of it), so each ends on a major tick.
    subroutine place_axes(self, p, distance)
        type(ManifoldKeys), intent(in) :: self
        real(wp), intent(in) :: p(3), distance
        real(wp) :: length, major, power, f
        if (.not. associated(self%axes)) return
        length = 0.6_wp * distance * self%lu
        major = length / 4.0_wp
        power = 10.0_wp**floor(log10(major))
        f = major / power
        if (f < 1.5_wp) then
            major = power
        else if (f < 3.5_wp) then
            major = 2.0_wp * power
        else if (f < 7.5_wp) then
            major = 5.0_wp * power
        else
            major = 10.0_wp * power
        end if
        length = major * max(1.0_wp, real(nint(length / major), wp))
        call self%axes%set_position(p)
        call self%axes%set_axis_length(length)
        call self%axes%set_tick_spacing(major, major / 5.0_wp)
    end subroutine place_axes

    !> No trajectory selected (e.g. computed again).
    subroutine clear_selection(self)
        class(ManifoldKeys), intent(inout) :: self
        if (self%selected(1) > 0) call self%lines(self%selected(1), self%selected(2))%set_highlight(HIGHLIGHT_NONE)
        if (self%hovered(1) > 0) call restyle(self, self%hovered, hovered=.false.)
        self%selected = 0
        self%hovered = 0
        call self%ring%show_contents(.false.)
        call self%selection%show_name_label(.false.)
        call self%details%set_text('click a trajectory line for its details')
    end subroutine clear_selection

    !> Hover: the trajectory under the pointer drawn thicker (not while dragging).
    subroutine manifold_motion(self, window, x, y, buttons)
        class(ManifoldKeys), intent(inout) :: self
        class(WindowProxy), intent(inout) :: window
        real(wp), intent(in) :: x, y
        logical, intent(in) :: buttons(3)
        type(PickHit), allocatable :: hits(:)
        integer :: lk(2)
        if (any(buttons)) return
        call window%pick(x, y, hits, kinds=[PICK_CURVE], nearest=.true.)
        lk = 0
        if (size(hits) > 0) lk = line_of(self, hits(1))
        if (all(lk == self%hovered)) return
        call restyle(self, self%hovered, hovered=.false.)
        call restyle(self, lk, hovered=.true.)
        self%hovered = lk
    end subroutine manifold_motion

    subroutine manifold_press(self, window, x, y, button)
        class(ManifoldKeys), intent(inout) :: self
        class(WindowProxy), intent(inout) :: window
        real(wp), intent(in) :: x, y
        integer, intent(in) :: button
        if (button == MOUSE_LEFT) call self%select(window, x, y)
    end subroutine manifold_press

    !> Select the trajectory at (x, y) (or none): highlighted (pulsing, arrows
    !> in its direction of motion), a readout at the point picked, and its
    !> details in the HUD.
    subroutine select_line(self, window, x, y)
        class(ManifoldKeys), intent(inout) :: self
        class(WindowProxy), intent(inout) :: window
        real(wp), intent(in) :: x, y
        type(PickHit), allocatable :: hits(:)
        character(len=160) :: line1, line2, line3
        character(len=120) :: label
        real(wp) :: at(2), r, days
        integer :: lk(2), previous(2)
        logical :: unstable
        call window%pick(x, y, hits, kinds=[PICK_CURVE], nearest=.true.)
        lk = 0
        if (size(hits) > 0) lk = line_of(self, hits(1))
        previous = self%selected
        self%selected = lk
        if (previous(1) > 0) call self%lines(previous(1), previous(2))%set_highlight(HIGHLIGHT_NONE)
        call restyle(self, previous, hovered=.false.)
        if (lk(1) == 0) then
            call self%ring%show_contents(.false.)
            call self%selection%show_name_label(.false.)
            call self%details%set_text('click a trajectory line for its details')
            return
        end if
        call self%lines(lk(1), lk(2))%set_highlight(ior(HIGHLIGHT_PULSE, HIGHLIGHT_ARROWS), color=[1.0_wp, 1.0_wp, 1.0_wp])
        call restyle(self, lk, hovered=.false.)
        associate (h => hits(1), info => self%info(lk(1), lk(2)))
            unstable = lk(2) == 1
            r = norm2(h%position - self%earth)
            days = abs(h%time)
            ! the readout beside the point (the side with room)
            write(label, '(a,f5.1,a,a,a,i0,a)') 't = ', days, ' d ', trim(merge('after leaving ', 'before arrival', unstable)), &
                ', ', nint(r / 1000.0_wp), ' thousand km from the Earth'
            call self%selection%set_color(self%line_colors(1:3, lk(2)))
            if (window%screen_position(h%position, at)) then
                if (at(1) > 0.6_wp) then
                    call self%selection%set_label_alignment(ALIGN_RIGHT_CENTER)
                    label = trim(label)//'    '
                else
                    call self%selection%set_label_alignment(ALIGN_LEFT_CENTER)
                    label = '    '//trim(label)
                end if
            end if
            call self%selection%set_name(trim(label))
            call self%selection%show_name_label(.true.)
            call self%selection%set_position(h%position)
            call self%ring%show_contents(.true.)
            ! the details
            write(line1, '(a,i0,a,i0,a,f4.2,a)') trim(self%names(lk(2)))//' manifold, trajectory ', info%index, &
                ' of ', info%n_around, ' (orbit phase ', real(info%index - 1, wp) / info%n_around, ')'
            if (info%impact) then
                write(line2, '(a,f5.1,a)') 'reaches the Earth''s surface ', abs(info%closest_time), &
                    ' days '//trim(merge('after leaving the orbit', 'before arriving at it  ', unstable))
            else
                write(line2, '(a,i0,a,f5.1,a)') 'closest to the Earth: ', nint(info%closest - self%re), &
                    ' km altitude, ', abs(info%closest_time), &
                    ' days '//trim(merge('after leaving the orbit', 'before arriving at it  ', unstable))
            end if
            write(line3, '(a,f9.6,a,es8.1,a)') 'Jacobi constant ', info%jacobi, ' (change along it: ', info%drift, ')'
            call self%details%set_text(trim(line1)//new_line('a')//trim(line2)//new_line('a')//trim(line3))
        end associate
    end subroutine select_line

    !> A line's width and color: selected, hovered or neither.
    subroutine restyle(self, lk, hovered)
        type(ManifoldKeys), intent(inout) :: self
        integer, intent(in) :: lk(2)
        logical, intent(in) :: hovered
        if (lk(1) == 0) return
        if (hovered) then
            call self%lines(lk(1), lk(2))%set_width(hover_width)
        else if (all(lk == self%selected)) then
            call self%lines(lk(1), lk(2))%set_width(selected_width)
        else
            call self%lines(lk(1), lk(2))%set_width(line_width)
        end if
    end subroutine restyle

    !> The (line, tube) a hit is on, or 0.
    function line_of(self, hit) result(lk)
        type(ManifoldKeys), intent(in) :: self
        type(PickHit), intent(in) :: hit
        integer :: lk(2), l, k
        type(Trajectory), pointer :: traj
        lk = 0
        if (.not. associated(hit%trajectory)) return
        do k = 1, n_tubes
            do l = 1, n_lines
                traj => self%lines(l, k)%get_trajectory()
                if (associated(hit%trajectory, traj)) then
                    lk = [l, k]
                    return
                end if
            end do
        end do
    end function line_of

end module halo_manifolds_keys
