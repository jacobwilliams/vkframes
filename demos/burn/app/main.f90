!> A burn designed with the mouse, in a qt-fortran GUI: a spacecraft on an
!> Earth orbit, a delta-v applied at a point of it, and the trajectory after
!> the burn (two-body, kepler in vkframes_qt) computed again as the delta-v is
!> dragged. Two views, side by side:
!>
!> * The orbit (left; Earth-centered inertial axes, km): the Earth, the orbit
!>   before the burn (dashed), the trajectory after it (cyan), each filled in
!>   translucent (RibbonArtist: from the Earth's center to it), where their
!>   planes cross a white line (translucent surfaces don't cut each other:
!>   the line is drawn: from the center through the burn point, which both
!>   orbits pass, as far as both fills reach), with its
!>   periapsis and apoapsis (white dots), the burn point, and the delta-v as a
!>   red line from it (a SegmentArtist: the vector stored at the burn point,
!>   drawn from it at the arrow's scale).
!> * The spacecraft (right): a view of the spacecraft's frame, whose axes are
!>   the burn's directions (the VNB frame): prograde (x, along the velocity),
!>   normal (y, along the orbit's angular momentum) and radial (z: prograde x
!>   normal, in the orbit's plane, outward on a circular orbit). The delta-v
!>   is an arrow there (a VectorHandle in that frame):
!>   hovering its tip lights it and shows its value; a left drag changes it,
!>   and the trajectory, in both views, follows.
!>
!> Each view has its own scene: the trajectories (the same Trajectory
!> objects) are drawn in both by artists of their own, the delta-v as a line
!> in one and as the arrow in the other. The dock has the delta-v as numbers
!> (prograde, normal, radial; typed in, or updated by the drag), how the arrow
!> moves (freely, in the orbit's plane, its size only, or its direction
!> only), where the burn is along the orbit, the arrow's scale, how long the
!> trajectory is drawn, the orbit's elements after the burn, and resets.
!>
!> Run with `pixi run -e qt burn-demo` (from the vkframes root). Options:
!> `--capture file` saves frame `--capture-frame n` (default 120) of the orbit
!> view to file and of the spacecraft view to file_2, `--quit-after
!> seconds`, and, for tests, `--drag-test`: the arrow's tip is dragged with
!> forwarded mouse events, and the results printed.
module burn_demo

    use qt
    use vkframes
    use vkframes_qt, only: connect_view, ViewLink, DemoOptions
    use kepler
    use iso_c_binding

    implicit none

    integer, parameter :: n_points = 1500                   !! points along each trajectory
    real(wp), parameter :: orbit_radius = 8000.0_wp         !! the orbit before the burn: circular (km)
    real(wp), parameter :: inclination = 28.5_wp            !! its inclination (deg)
    real(wp), parameter :: start_dv(3) = [1.2_wp, 0.0_wp, 0.0_wp]      !! km/s (prograde, normal, radial)
    real(wp), parameter :: start_scale = 1500.0_wp          !! the arrow's and line's length per km/s (km)
    real(wp), parameter :: max_time = 48.0_wp * 3600.0_wp   !! the most drawn after the burn (s)
    character(len=36), parameter :: dv_moves(4) = [character(len=36) :: 'Freely (facing the camera)', &
        'In the orbit''s plane (no normal part)', 'Size only (along itself)', 'Direction only (size kept)']
    integer, parameter :: move_constraints(4) = [HANDLE_FREE, HANDLE_PLANE, HANDLE_RADIAL, HANDLE_SPHERE]
    real(wp), parameter :: dv_color(3) = [1.0_wp, 0.2_wp, 0.15_wp]
    real(wp), parameter :: transfer_color(3) = [0.3_wp, 0.85_wp, 1.0_wp]
    real(wp), parameter :: initial_color(4) = [0.75_wp, 0.75_wp, 0.75_wp, 0.7_wp]
    real(wp), parameter :: initial_fill(4) = [1.0_wp, 0.55_wp, 0.1_wp, 0.3_wp]       !! the orbit before the burn, filled
    real(wp), parameter :: transfer_fill(4) = [0.3_wp, 0.85_wp, 1.0_wp, 0.18_wp]    !! the trajectory after it, filled
    real(wp), parameter :: crossing_color(4) = [1.0_wp, 1.0_wp, 1.0_wp, 0.85_wp]    !! where the fills' planes cross

    !> The arrow's drags: the trajectory computed again as it moves; its value next to it.
    type, extends(WindowEventHandler) :: BurnHandler
    contains
        procedure :: handle_drag => burn_handle_drag
        procedure :: before_draw => burn_before_draw
    end type BurnHandler

    !> One of the two views: a window, its proxy and its view.
    type :: ViewWindow
        type(QWindow) :: window
        type(QWidget) :: container
        type(QtfEventFilter) :: events
        type(ViewLink) :: link                              !! the window's events to the proxy (connect_view)
        type(WindowProxy) :: proxy
        type(View) :: view
        integer :: frame = 0                                !! frames drawn
    end type ViewWindow

    !> The GUI and the two scenes.
    type :: app_t
        type(QMainWindow) :: main
        type(ViewWindow) :: orbit_win, craft_win            !! the orbit view (left), the spacecraft view (right)
        ! controls
        type(QDoubleSpinBox) :: dv_box(3), scale_box, hours_box
        type(QSlider) :: burn_slider
        type(QLabel) :: burn_label, elements_label
        type(QComboBox) :: dv_move
        type(QCheckBox) :: one_orbit
        logical :: updating = .false.                       !! the demo sets the controls (not the user)
        type(HUDText) :: hud_burn, hud_elements, hud_craft, callout
        ! the data, drawn in both scenes
        type(Trajectory) :: initial, transfer, apsides, burn_point
        type(Trajectory) :: crossing                        !! where the two orbits' planes cross, within both fills
        ! the orbit view's scene
        type(ReferenceFrame) :: root
        type(Sphere) :: earth
        type(CurveArtist) :: initial_curve, transfer_curve
        type(RibbonArtist) :: initial_plane, transfer_plane !! the orbits filled in (translucent)
        type(MarkerArtist) :: apsis_dots, burn_dot
        type(CurveArtist) :: crossing_line
        type(SegmentArtist) :: dv_line                      !! the delta-v: a red line from the burn point
        type(DrawableTrajectory) :: orbits, burn
        type(FrameManager) :: scene
        ! the spacecraft view's scene
        type(ReferenceFrame) :: root2
        type(Sphere) :: earth2
        type(CurveArtist) :: initial_curve2, transfer_curve2
        type(RibbonArtist) :: initial_plane2, transfer_plane2
        type(CurveArtist) :: crossing_line2
        type(DrawableTrajectory) :: orbits2
        type(Sphere) :: craft                               !! the spacecraft: its axes the burn's directions
        type(VectorHandle) :: dv_handle                     !! the delta-v, in the spacecraft's frame
        type(FrameManager) :: scene2
        type(SkySphere) :: sky
        ! the burn
        real(wp) :: burn_angle = 0.0_wp                     !! where along the orbit (deg from its ascending node)
        real(wp) :: r(3), v(3)                              !! the state at the burn (km, km/s)
        real(wp) :: axes(3, 3)                              !! the burn's directions (columns: prograde, normal, radial)
        real(wp) :: dv(3) = start_dv                        !! the delta-v (km/s: prograde, normal, radial)
        type(Elements) :: el                                !! the orbit after the burn
        logical :: cut = .false.                            !! the trajectory hits the Earth
        ! options
        type(DemoOptions) :: options                        !! --capture, --capture-frame, --quit-after
        logical :: drag_test = .false.
    end type app_t

    type(app_t), target, save :: app
    type(BurnHandler), target, save :: handler

contains

    ! ==================================================================
    ! the scenes

    !> The two scenes: the orbit view's (the Earth, the orbits, the burn point
    !> and the delta-v line) and the spacecraft view's (the Earth, the same
    !> orbits, the spacecraft and the delta-v arrow), and the sky.
    subroutine build_model()
        ! the data: the orbit before the burn, the trajectory after it, its
        ! apsides, and the burn point with the delta-v stored at it (inertial)
        app%initial = Trajectory()
        call app%initial%reserve(n_points)
        app%transfer = Trajectory()
        call app%transfer%reserve(n_points)
        app%apsides = Trajectory()
        app%burn_point = Trajectory(nopt=1)
        app%crossing = Trajectory()

        ! the orbit view
        app%root = ReferenceFrame('ECI')
        call app%root%show_decorations(.false.)
        app%earth = Sphere('Earth', radius=earth_radius)
        call app%earth%set_texture_map('2k_earth_daymap.jpg')
        call app%earth%show_decorations(.false.)
        app%initial_curve = CurveArtist(app%initial, color=initial_color, width=1.5_wp)
        call app%initial_curve%set_pattern(4, int(z'00FF'))
        app%transfer_curve = CurveArtist(app%transfer, color=transfer_color, width=2.5_wp)
        app%apsis_dots = MarkerArtist(app%apsides, markers=ior(MARKER_START, MARKER_END), size=9.0_wp, &
                                      color=[1.0_wp, 1.0_wp, 1.0_wp])
        app%orbits = DrawableTrajectory('Orbits')
        call app%orbits%show_decorations(.false.)
        call orbit_fills(app%initial_plane, app%transfer_plane)
        call app%orbits%add_artist(app%initial_plane)
        call app%orbits%add_artist(app%transfer_plane)
        call app%orbits%add_artist(app%initial_curve)
        call app%orbits%add_artist(app%transfer_curve)
        call app%orbits%add_artist(app%apsis_dots)
        app%crossing_line = CurveArtist(app%crossing, color=crossing_color, width=2.0_wp)
        call app%crossing_line%set_pickable(.false.)
        call app%orbits%add_artist(app%crossing_line)
        ! the burn: a dot, and the delta-v as a line from it (the stored vector, at the scale)
        app%burn_dot = MarkerArtist(app%burn_point, markers=MARKER_START, size=11.0_wp, color=dv_color)
        app%dv_line = SegmentArtist(app%burn_point, color=dv_color, width=3.0_wp)
        if (.not. (app%dv_line%set_start_data(position_sources(3)) .and. &
                   app%dv_line%set_end_data(optional_sources(1, scale=start_scale)))) &
            error stop 'burn demo: the delta-v line''s data'
        call app%dv_line%set_end_relative(.true.)
        app%burn = DrawableTrajectory('Burn')
        call app%burn%show_decorations(.false.)
        call app%burn%add_artist(app%dv_line)
        call app%burn%add_artist(app%burn_dot)
        if (.not. (app%root%add_child(app%earth) .and. app%root%add_child(app%orbits) .and. &
                   app%root%add_child(app%burn))) error stop 'burn demo: could not build the orbit view'
        app%scene = FrameManager(app%root)

        ! the spacecraft view: the same orbits, the spacecraft and the arrow
        app%root2 = ReferenceFrame('ECI')
        call app%root2%show_decorations(.false.)
        app%earth2 = Sphere('Earth', radius=earth_radius)
        call app%earth2%set_texture_map('2k_earth_daymap.jpg')
        call app%earth2%show_decorations(.false.)
        app%initial_curve2 = CurveArtist(app%initial, color=initial_color, width=1.5_wp)
        call app%initial_curve2%set_pattern(4, int(z'00FF'))
        app%transfer_curve2 = CurveArtist(app%transfer, color=transfer_color, width=2.5_wp)
        app%orbits2 = DrawableTrajectory('Orbits')
        call app%orbits2%show_decorations(.false.)
        call orbit_fills(app%initial_plane2, app%transfer_plane2)
        call app%orbits2%add_artist(app%initial_plane2)
        call app%orbits2%add_artist(app%transfer_plane2)
        call app%orbits2%add_artist(app%initial_curve2)
        call app%orbits2%add_artist(app%transfer_curve2)
        app%crossing_line2 = CurveArtist(app%crossing, color=crossing_color, width=2.0_wp)
        call app%crossing_line2%set_pickable(.false.)
        call app%orbits2%add_artist(app%crossing_line2)
        app%craft = Sphere('Spacecraft', radius=60.0_wp, color=[0.9_wp, 0.9_wp, 0.95_wp])
        call app%craft%show_name_label(.false.)
        call app%craft%move_x_axis([0.0_wp, 0.0_wp, 0.0_wp], 1200.0_wp, body_radius=12.0_wp, head_radius=40.0_wp)
        call app%craft%move_y_axis([0.0_wp, 0.0_wp, 0.0_wp], 1200.0_wp, body_radius=12.0_wp, head_radius=40.0_wp)
        call app%craft%move_z_axis([0.0_wp, 0.0_wp, 0.0_wp], 1200.0_wp, body_radius=12.0_wp, head_radius=40.0_wp)
        call app%craft%set_x_label('prograde')
        call app%craft%set_y_label('normal')
        call app%craft%set_z_label('radial')
        call app%craft%show_axes(ALL_AXES)
        call app%craft%show_axes_labels(ALL_AXES)
        call app%craft%set_label_size(14)
        app%dv_handle = VectorHandle('Delta-v', vector=app%dv, color=dv_color, scale=start_scale)
        call app%dv_handle%set_head_size(220.0_wp)
        if (.not. (app%root2%add_child(app%earth2) .and. app%root2%add_child(app%orbits2) .and. &
                   app%root2%add_child(app%craft) .and. app%craft%add_child(app%dv_handle))) &
            error stop 'burn demo: could not build the spacecraft view'
        app%scene2 = FrameManager(app%root2)

        app%sky = SkySphere()
        if (.not. app%sky%set_star_data('Stars_HYGv3.txt', min_mag=-2.0_wp, max_mag=6.0_wp, max_stars=20000)) &
            write(*, '(a)') 'burn demo: no star catalog'
    end subroutine build_model

    !> The orbits filled in, translucent (from the Earth's center to them), in
    !> different colors; not picked (the arrow is).
    subroutine orbit_fills(initial, transfer)
        type(RibbonArtist), intent(out) :: initial          !! the orbit before the burn, filled
        type(RibbonArtist), intent(out) :: transfer         !! the trajectory after it, filled
        initial = RibbonArtist(app%initial, color=initial_fill)
        transfer = RibbonArtist(app%transfer, color=transfer_fill)
        call initial%set_pickable(.false.)
        call transfer%set_pickable(.false.)
    end subroutine orbit_fills

    !> The burn point (burn_angle along the circular orbit): the state there,
    !> the burn's directions, the spacecraft placed and turned to them, and the
    !> orbit before the burn (from that point: one period).
    subroutine place_burn()
        real(wp) :: u, inc, speed, p_hat(3), q_hat(3), t(n_points), r(3, n_points)
        integer :: n
        logical :: cut0, cut1
        u = app%burn_angle * pi / 180.0_wp
        inc = inclination * pi / 180.0_wp
        p_hat = [1.0_wp, 0.0_wp, 0.0_wp]                     ! (the ascending node: along x)
        q_hat = [0.0_wp, cos(inc), sin(inc)]
        speed = sqrt(mu / orbit_radius)
        app%r = orbit_radius * (cos(u) * p_hat + sin(u) * q_hat)
        app%v = speed * (-sin(u) * p_hat + cos(u) * q_hat)
        app%axes = burn_axes(app%r, app%v)
        call app%craft%set_position(app%r)
        call app%craft%set_attitude(quat_from_matrix(app%axes))
        n = n_points
        call propagate(app%r, app%v, 0.0_wp, 2.0_wp * pi * sqrt(orbit_radius**3 / mu), n, t, r, cut0, cut1)
        call app%initial%clear()
        call app%initial%add(t(1:n), pos=r(:, 1:n))
    end subroutine place_burn

    !> The burn's directions at a state, as a matrix's columns (the VNB
    !> frame): prograde (V, along the velocity), normal (N, along the angular
    !> momentum) and radial (B = V x N: in the orbit's plane, outward on a
    !> circular orbit).
    pure function burn_axes(r, v) result(m)
        real(wp), intent(in) :: r(3)                        !! the position (km)
        real(wp), intent(in) :: v(3)                        !! the velocity (km/s)
        real(wp) :: m(3, 3)
        real(wp) :: h(3)
        m(:, 1) = v / norm2(v)
        h = [r(2) * v(3) - r(3) * v(2), r(3) * v(1) - r(1) * v(3), r(1) * v(2) - r(2) * v(1)]
        m(:, 2) = h / norm2(h)
        m(:, 3) = [m(2, 1) * m(3, 2) - m(3, 1) * m(2, 2), m(3, 1) * m(1, 2) - m(1, 1) * m(3, 2), &
                   m(1, 1) * m(2, 2) - m(2, 1) * m(1, 2)]   ! (prograde x normal)
    end function burn_axes

    !> The trajectory after the burn computed again from the state and the
    !> delta-v (one orbit if checked and bound, else the hours drawn), and
    !> everything showing it.
    subroutine recompute()
        real(wp) :: dv_eci(3), v1(3), t(n_points), r(3, n_points), t1, e_dir(3)
        integer :: n
        logical :: cut0
        dv_eci = matmul(app%axes, app%dv)
        v1 = app%v + dv_eci
        app%el = elements_of(app%r, v1)
        if (app%one_orbit%isChecked() .and. app%el%period < huge(1.0_wp)) then
            t1 = min(app%el%period, max_time)
        else
            t1 = app%hours_box%value() * 3600.0_wp
        end if
        n = n_points
        call propagate(app%r, v1, 0.0_wp, t1, n, t, r, cut0, app%cut)
        call app%transfer%clear()
        call app%transfer%add(t(1:n), pos=r(:, 1:n))
        ! the burn point, with the delta-v stored at it (inertial: the line's end, relative)
        call app%burn_point%clear()
        call app%burn_point%add(0.0_wp, pos=app%r)
        call app%burn_point%set_optional(1, dv_eci)
        ! the apsides: along the eccentricity vector
        call app%apsides%clear()
        if (app%el%e > 1.0e-6_wp) then
            e_dir = ((dot_product(v1, v1) - mu / norm2(app%r)) * app%r - dot_product(app%r, v1) * v1) / mu
            e_dir = e_dir / norm2(e_dir)
            call app%apsides%add(0.0_wp, pos=app%el%rp * e_dir)
            if (app%el%ra < huge(1.0_wp)) call app%apsides%add(1.0_wp, pos=-app%el%ra * e_dir)
        end if
        call place_crossing(v1)
        call update_readouts()
    end subroutine recompute

    !> Where the two orbits' planes cross, within both fills (each from the
    !> Earth's center to its orbit): both orbits pass the burn point, so the
    !> planes cross on the line from the center through it; on the burn
    !> point's side to it, on the other to the nearer orbit there (the one
    !> before the burn is a circle; the one after it, at the point opposite the
    !> burn, if it reaches it). None if the planes are the same (no normal
    !> delta-v).
    subroutine place_crossing(v1)
        real(wp), intent(in) :: v1(3)                       !! the velocity after the burn (km/s)
        real(wp) :: n0(3), n1(3), u(3), e_vec(3), p, denom, far, sin_angle
        call app%crossing%clear()
        n0 = cross(app%r, app%v)
        n1 = cross(app%r, v1)
        sin_angle = norm2(cross(n0 / norm2(n0), n1 / norm2(n1)))
        if (sin_angle < 1.0e-4_wp) return                    ! (the same plane: no line)
        u = app%r / norm2(app%r)
        ! the orbit after the burn at the point opposite the burn (r = p / (1 + e . direction))
        p = dot_product(n1, n1) / mu
        e_vec = cross(v1, n1) / mu - u
        denom = 1.0_wp + dot_product(e_vec, -u)
        far = 0.0_wp                                         ! (a hyperbola that doesn't go there: no fill there)
        if (denom > 1.0e-9_wp) far = min(orbit_radius, p / denom)
        call app%crossing%add(0.0_wp, pos=-far * u)
        call app%crossing%add(1.0_wp, pos=app%r)
    end subroutine place_crossing

    !> a x b.
    pure function cross(a, b) result(c)
        real(wp), intent(in) :: a(3)                        !! the first vector
        real(wp), intent(in) :: b(3)                        !! the second vector
        real(wp) :: c(3)
        c = [a(2) * b(3) - a(3) * b(2), a(3) * b(1) - a(1) * b(3), a(1) * b(2) - a(2) * b(1)]
    end function cross

    !> The delta-v and the elements in the HUDs and the dock, and the dock's numbers.
    subroutine update_readouts()
        integer :: k
        call app%hud_burn%set_text('delta-v '//num(norm2(app%dv), 3)//' km/s  (prograde '//num(app%dv(1), 3)// &
                                   ', normal '//num(app%dv(2), 3)//', radial '//num(app%dv(3), 3)//')')
        call app%hud_elements%set_text(elements_text(.false.))
        call app%hud_craft%set_text('delta-v '//num(norm2(app%dv), 3)//' km/s: drag the arrow''s tip')
        call app%elements_label%setText(elements_text(.true.))
        app%updating = .true.
        do k = 1, 3
            call app%dv_box(k)%setValue(real(app%dv(k), c_double))
        end do
        app%updating = .false.
    end subroutine update_readouts

    !> The orbit after the burn as text: one item a line (`lines`), or on two lines.
    function elements_text(lines) result(text)
        logical, intent(in) :: lines                        !! true: one item a line (the dock); false: two lines (the HUD)
        character(len=:), allocatable :: text, sep, mid
        sep = ';  '
        mid = new_line('a')
        if (lines) sep = new_line('a')
        if (lines) mid = sep
        associate (el => app%el)
            if (el%e < 1.0_wp) then
                text = 'after the burn: a '//num(el%a, 1)//' km'//sep//'e '//num(el%e, 4)//sep// &
                       'i '//num(el%i, 2)//' deg'//mid//'periapsis altitude '//num(el%rp - earth_radius, 1)//' km'// &
                       sep//'apoapsis altitude '//num(el%ra - earth_radius, 1)//' km'//sep// &
                       'period '//num(el%period / 3600.0_wp, 2)//' h'
            else
                text = 'after the burn: not bound (e '//num(el%e, 4)//')'//sep//'i '//num(el%i, 2)//' deg'//mid// &
                       'periapsis altitude '//num(el%rp - earth_radius, 1)//' km'//sep//'excess speed '// &
                       num(sqrt(max(0.0_wp, 2.0_wp * el%energy)), 3)//' km/s'
            end if
        end associate
        if (app%cut) text = text//sep//'hits the Earth'
    end function elements_text

    !> x with `decimals` decimals (a leading zero kept: 0.5, -0.5).
    function num(x, decimals) result(text)
        real(wp), intent(in) :: x                           !! the number
        integer, intent(in) :: decimals                     !! decimals shown
        character(len=:), allocatable :: text
        character(len=32) :: buf, fmt
        write(fmt, '(a,i0,a)') '(f32.', decimals, ')'
        write(buf, fmt) x
        text = trim(adjustl(buf))
    end function num

    ! ==================================================================
    ! the arrow

    !> The arrow dragged: the delta-v from it (the spacecraft's frame: its
    !> components), and the trajectory again.
    subroutine burn_handle_drag(self, window, handle, stage)
        class(BurnHandler), intent(inout) :: self
        class(WindowProxy), intent(inout) :: window         !! the window the arrow is dragged in (the spacecraft view)
        class(DragHandle), intent(inout), target :: handle  !! the arrow (app%dv_handle)
        integer, intent(in) :: stage                        !! DRAG_START, DRAG_MOVE or DRAG_END
        if (stage /= DRAG_MOVE) return
        app%dv = app%dv_handle%vector()
        call recompute()
    end subroutine burn_handle_drag

    !> The callout: the delta-v next to the arrow's tip while it's hovered or dragged.
    subroutine burn_before_draw(self, window, t)
        class(BurnHandler), intent(inout) :: self
        class(WindowProxy), intent(inout) :: window         !! the window about to draw (the spacecraft view)
        real(wp), intent(in) :: t                           !! the simulation time (s; paused here)
        real(wp) :: at(2), tip(3)
        if (.not. (app%dv_handle%hovered() .or. app%dv_handle%dragged())) then
            call app%callout%show(.false.)
            return
        end if
        tip = app%r + matmul(app%axes, app%dv_handle%tip())      ! (the tip in the root frame)
        if (.not. window%screen_position(tip, at)) then
            call app%callout%show(.false.)
            return
        end if
        call app%callout%set_text(num(norm2(app%dv), 3)//' km/s')
        call app%callout%set_position(at + [0.012_wp, 0.012_wp])
        call app%callout%show(.true.)
    end subroutine burn_before_draw

    !> The arrow's constraint from the dock (the orbit's plane: normal to the
    !> spacecraft frame's y, the normal direction).
    subroutine apply_constraint()
        call app%dv_handle%set_constraint(move_constraints(app%dv_move%currentIndex() + 1), [0.0_wp, 1.0_wp, 0.0_wp])
    end subroutine apply_constraint

    ! ==================================================================
    ! the windows

    !> A view's window: its QWindow and container, with its events connected.
    subroutine build_window(w)
        type(ViewWindow), intent(inout), target :: w        !! the view
        w%window = QWindow()
        w%container = QWidget_createWindowContainer(w%window)
        call w%container%setMinimumSize(320, 240)
        w%events = QtfEventFilter(w%window)
        call connect_view(w%link, w%events, w%proxy, w%window)   ! (mouse, keys, resizes, the surface)
    end subroutine build_window

    !> The views' cameras, the HUD texts, and both windows.
    subroutine build_windows()
        app%orbit_win%view = View(app%root, app%root)
        call app%orbit_win%view%set_default_view_parameters(eye=[6000.0_wp, -30000.0_wp, 30000.0_wp], &
            center=[-3000.0_wp, 0.0_wp, 0.0_wp], up=[0.0_wp, 0.0_wp, 1.0_wp])
        ! the spacecraft's frame (x prograde, y normal, z radial): from outside
        ! the orbit, above it and a little behind, looking past the spacecraft
        ! toward the Earth, the orbit's normal up
        app%craft_win%view = View(app%root2, app%craft)
        call app%craft_win%view%set_default_view_parameters(eye=[-1500.0_wp, 4000.0_wp, 6500.0_wp], &
            center=[900.0_wp, 0.0_wp, -1800.0_wp], up=[0.0_wp, 1.0_wp, 0.0_wp])
        app%hud_burn = HUDText('', position=[0.01_wp, 0.99_wp], alignment=ALIGN_LEFT_TOP, size=16.0_wp, color=dv_color)
        app%hud_elements = HUDText('', position=[0.01_wp, 0.01_wp], alignment=ALIGN_LEFT_BOTTOM, size=15.0_wp, &
                                   color=transfer_color)
        app%hud_craft = HUDText('', position=[0.01_wp, 0.99_wp], alignment=ALIGN_LEFT_TOP, size=16.0_wp, color=dv_color)
        app%callout = HUDText('', position=[0.5_wp, 0.5_wp], alignment=ALIGN_LEFT_BOTTOM, size=16.0_wp, &
                              color=[1.0_wp, 1.0_wp, 1.0_wp])
        call app%callout%show(.false.)
        call build_window(app%orbit_win)
        call build_window(app%craft_win)
        call app%orbit_win%events%exposeEvent%connect(on_expose_orbit)
        call app%orbit_win%events%updateRequest%connect(on_update_orbit)
        call app%craft_win%events%exposeEvent%connect(on_expose_craft)
        call app%craft_win%events%updateRequest%connect(on_update_craft)
    end subroutine build_windows

    !> Open a view (on its window's first expose) and show its scene.
    subroutine open_view(w, scene, title)
        type(ViewWindow), intent(inout), target :: w        !! the view
        type(FrameManager), intent(inout), target :: scene  !! its scene
        character(len=*), intent(in) :: title               !! its window's title (for the renderer)
        if (.not. w%proxy%open(w%window%width(), w%window%height(), title, native_window=w%window%winId(), &
                               pixel_ratio=w%window%devicePixelRatio())) then
            write(*, '(a)') 'burn demo: could not open the '//title
            return
        end if
        if (w%proxy%num_views() > 0) return                     ! (opened again)
        call w%proxy%set_scene(scene)
        call w%proxy%set_background_color([0.0_wp, 0.0_wp, 0.0_wp])
        call w%proxy%set_sky(app%sky)
        call w%proxy%add_view(w%view)
        call w%proxy%pause_time(.true.)                          ! (the burn is designed, not flown)
    end subroutine open_view

    !> The orbit view's window shown: opened (the first time), its frames started.
    subroutine on_expose_orbit(data)
        class(*), intent(inout), optional :: data           !! the connection's data (unused)
        if (app%orbit_win%proxy%is_open() .or. .not. app%orbit_win%window%isExposed()) return
        call open_view(app%orbit_win, app%scene, 'orbit view')
        if (.not. app%orbit_win%proxy%is_open()) return
        call app%orbit_win%proxy%add_hud_text(app%hud_burn)
        call app%orbit_win%proxy%add_hud_text(app%hud_elements)
        call app%orbit_win%window%requestUpdate()
    end subroutine on_expose_orbit

    !> The spacecraft view's window shown: opened (the first time), with the
    !> arrow's handler, its frames started.
    subroutine on_expose_craft(data)
        class(*), intent(inout), optional :: data           !! the connection's data (unused)
        if (app%craft_win%proxy%is_open() .or. .not. app%craft_win%window%isExposed()) return
        call open_view(app%craft_win, app%scene2, 'spacecraft view')
        if (.not. app%craft_win%proxy%is_open()) return
        call app%craft_win%proxy%set_event_handler(handler)
        call app%craft_win%proxy%add_hud_text(app%hud_craft)
        call app%craft_win%proxy%add_hud_text(app%callout)
        call app%craft_win%window%requestUpdate()
    end subroutine on_expose_craft

    !> Render a frame of the orbit view, ask for the next one.
    subroutine on_update_orbit(data)
        class(*), intent(inout), optional :: data           !! the connection's data (unused)
        if (.not. frame_of(app%orbit_win, 1)) return
        call app%orbit_win%window%requestUpdate()
    end subroutine on_update_orbit

    !> Render a frame of the spacecraft view (and a step of --drag-test), ask for the next one.
    subroutine on_update_craft(data)
        class(*), intent(inout), optional :: data           !! the connection's data (unused)
        if (.not. frame_of(app%craft_win, 2)) return
        if (app%drag_test) call drag_test_step()
        call app%craft_win%window%requestUpdate()
    end subroutine on_update_craft

    !> A view's frame (its size first if its pixel ratio changed), and a
    !> capture (--capture). False if not drawn, or its surface is made again.
    logical function frame_of(w, index) result(drawn)
        type(ViewWindow), intent(inout), target :: w        !! the view
        integer, intent(in) :: index                        !! its number, for the capture's file (1: file, 2: file_2)
        drawn = .false.
        if (w%window%devicePixelRatio() /= w%proxy%pixel_ratio()) &
            call w%proxy%resize(nint(w%window%width() * w%window%devicePixelRatio()), &
                                nint(w%window%height() * w%window%devicePixelRatio()), &
                                pixel_ratio=w%window%devicePixelRatio())
        if (.not. w%proxy%frame()) return
        w%frame = w%frame + 1
        drawn = .not. app%options%after_frame(w%proxy, w%window, w%frame, index)
    end function frame_of

    !> --drag-test: the arrow's tip hovered (frame 10), grabbed (12), dragged
    !> (14 to 30: toward a point beside it, radially outward) and
    !> released (32), with mouse events forwarded as from the spacecraft
    !> view's window; the results printed (34).
    subroutine drag_test_step()
        real(wp), save :: at(2), dv0(3)
        integer, save :: px, py, dx, dy
        real(wp) :: tip(3), aside(2)
        integer :: width, height
        width = nint(app%craft_win%window%width() * app%craft_win%window%devicePixelRatio())
        height = nint(app%craft_win%window%height() * app%craft_win%window%devicePixelRatio())
        select case (app%craft_win%frame)
        case (10)
            tip = app%r + matmul(app%axes, app%dv_handle%tip())
            if (.not. app%craft_win%proxy%screen_position(tip, at)) error stop 'burn demo: the arrow''s tip is off the screen'
            if (.not. app%craft_win%proxy%screen_position(tip + 600.0_wp * app%axes(:, 3), aside)) &
                error stop 'burn demo: off the screen'
            px = nint(at(1) * width)
            py = nint((1.0_wp - at(2)) * height)
            dx = nint((aside(1) - at(1)) * width / 17)
            dy = -nint((aside(2) - at(2)) * height / 17)
            dv0 = app%dv
            call app%craft_win%proxy%mouse(MOUSE_MOVE, px, py, 0, 0)
        case (12)
            write(*, '(a,l1)') 'drag test: hovered ', app%dv_handle%hovered()
            call app%craft_win%proxy%mouse(MOUSE_PRESS, px, py, 256, MOUSE_LEFT)
        case (14:30)
            px = px + dx
            py = py + dy
            call app%craft_win%proxy%mouse(MOUSE_MOVE, px, py, 256, 0)
        case (32)
            call app%craft_win%proxy%mouse(MOUSE_RELEASE, px, py, 0, MOUSE_LEFT)
        case (34)
            write(*, '(a)') 'drag test: delta-v ['//num(dv0(1), 3)//', '//num(dv0(2), 3)//', '//num(dv0(3), 3)// &
                            '] -> ['//num(app%dv(1), 3)//', '//num(app%dv(2), 3)//', '//num(app%dv(3), 3)//'] km/s'
            write(*, '(a,i0,a)') 'drag test: the trajectory after the burn: ', app%transfer%num_positions(), ' points'
            write(*, '(a)') 'drag test: '//elements_text(.false.)
        end select
    end subroutine drag_test_step

    ! ==================================================================
    ! controls

    !> A component of the delta-v typed in: the arrow and the trajectory from it.
    subroutine on_dv(value, data)
        real(c_double), intent(in) :: value                 !! the box's new value (all three are read)
        class(*), intent(inout), optional :: data           !! the connection's data (unused)
        integer :: k
        if (app%updating) return
        do k = 1, 3
            app%dv(k) = app%dv_box(k)%value()
        end do
        call app%dv_handle%set_vector(app%dv)
        call recompute()
    end subroutine on_dv

    !> The burn point moved along the orbit: the spacecraft there, the trajectory again.
    subroutine on_burn_angle(value, data)
        integer, intent(in) :: value                        !! degrees along the orbit (from its ascending node)
        class(*), intent(inout), optional :: data           !! the connection's data (unused)
        app%burn_angle = value
        call app%burn_label%setText(num(app%burn_angle, 0)//' deg from the ascending node')
        call place_burn()
        call recompute()
    end subroutine on_burn_angle

    !> How long the trajectory is drawn (one orbit, or hours): drawn again.
    subroutine on_length(value, data)
        real(c_double), intent(in) :: value                 !! hours (the box is read)
        class(*), intent(inout), optional :: data           !! the connection's data (unused)
        call recompute()
    end subroutine on_length

    subroutine on_one_orbit(checked, data)
        logical, intent(in) :: checked                      !! true: one period after the burn (read by recompute)
        class(*), intent(inout), optional :: data           !! the connection's data (unused)
        call app%hours_box%setEnabled(.not. checked)
        call recompute()
    end subroutine on_one_orbit

    !> The arrow's and the line's scale.
    subroutine on_scale(value, data)
        real(c_double), intent(in) :: value                 !! km per km/s
        class(*), intent(inout), optional :: data           !! the connection's data (unused)
        call app%dv_handle%set_display_scale(real(value, wp))
        call app%dv_line%set_end_scale(real(value, wp))
    end subroutine on_scale

    !> How the arrow moves, chosen.
    subroutine on_move_choice(index, data)
        integer, intent(in) :: index                        !! the choice (the box is read)
        class(*), intent(inout), optional :: data           !! the connection's data (unused)
        call apply_constraint()
    end subroutine on_move_choice

    !> The delta-v as at the start.
    subroutine on_reset_dv(data)
        class(*), intent(inout), optional :: data           !! the connection's data (unused)
        app%dv = start_dv
        call app%dv_handle%set_vector(app%dv)
        call recompute()
    end subroutine on_reset_dv

    !> Both views' cameras reset.
    subroutine on_reset_views(data)
        class(*), intent(inout), optional :: data           !! the connection's data (unused)
        call app%orbit_win%view%reset_view()
        call app%craft_win%view%reset_view()
    end subroutine on_reset_views

    !> The main window closes: close the views (waiting for the GPU) first.
    subroutine on_close(event, data)
        type(QCloseEvent), intent(in) :: event              !! the close (accepted)
        class(*), intent(inout), optional :: data           !! the connection's data (unused)
        call app%craft_win%proxy%close()
        call app%orbit_win%proxy%close()
    end subroutine on_close

    !> The main window: the two views side by side in the middle, the dock
    !> (the delta-v, how the arrow moves, the burn point, the trajectory's
    !> length, its elements) on the left.
    subroutine build_ui()
        type(QDockWidget) :: dock
        type(QWidget) :: panel
        type(QFormLayout) :: form
        type(QSplitter) :: views
        type(QPushButton) :: reset_dv, reset_views
        type(QHBoxLayout) :: dv_row, length_row, reset_row
        character(len=8), parameter :: dv_names(3) = [character(len=8) :: 'prograde', 'normal', 'radial']
        integer :: k

        app%main = QMainWindow()
        call app%main%setWindowTitle('vkframes: a burn (drag the delta-v arrow in the spacecraft view)')
        call app%main%closeEvent%connect(on_close)
        call build_windows()
        views = QSplitter(Qt_Horizontal)
        call views%addWidget(app%orbit_win%container)
        call views%addWidget(app%craft_win%container)
        call views%setSizes([760, 560])
        call app%main%setCentralWidget(views)

        panel = QWidget()
        form = QFormLayout(panel)

        ! the delta-v, as numbers
        dv_row = QHBoxLayout()
        do k = 1, 3
            app%dv_box(k) = QDoubleSpinBox()
            call app%dv_box(k)%setRange(-10.0_c_double, 10.0_c_double)
            call app%dv_box(k)%setDecimals(3)
            call app%dv_box(k)%setSingleStep(0.05_c_double)
            call app%dv_box(k)%setValue(real(app%dv(k), c_double))
            call app%dv_box(k)%setToolTip(trim(dv_names(k))//' (km/s)')
            call app%dv_box(k)%valueChanged%connect(on_dv)
            call dv_row%addWidget(app%dv_box(k))
        end do
        call form%addRow('Delta-v (km/s)', dv_row)
        call form%addRow(QLabel('<small>prograde, normal, radial</small>'))
        app%dv_move = QComboBox()
        call app%dv_move%addItems(dv_moves)
        call app%dv_move%currentIndexChanged%connect(on_move_choice)
        call form%addRow('The arrow moves', app%dv_move)
        app%scale_box = QDoubleSpinBox()
        call app%scale_box%setRange(200.0_c_double, 5000.0_c_double)
        call app%scale_box%setSingleStep(100.0_c_double)
        call app%scale_box%setDecimals(0)
        call app%scale_box%setValue(real(start_scale, c_double))
        call app%scale_box%setSuffix(' km per km/s')
        call app%scale_box%valueChanged%connect(on_scale)
        call form%addRow('Arrow scale', app%scale_box)

        ! where the burn is
        app%burn_slider = QSlider(Qt_Horizontal)
        call app%burn_slider%setRange(0, 359)
        call app%burn_slider%setValue(nint(app%burn_angle))
        call app%burn_slider%valueChanged%connect(on_burn_angle)
        call form%addRow('Burn point', app%burn_slider)
        app%burn_label = QLabel(num(app%burn_angle, 0)//' deg from the ascending node')
        call form%addRow('', app%burn_label)

        ! how long the trajectory after the burn is drawn
        app%one_orbit = QCheckBox('One orbit')
        call app%one_orbit%setChecked(.true.)
        call app%one_orbit%toggled%connect(on_one_orbit)
        app%hours_box = QDoubleSpinBox()
        call app%hours_box%setRange(0.1_c_double, real(max_time / 3600.0_wp, c_double))
        call app%hours_box%setDecimals(1)
        call app%hours_box%setValue(6.0_c_double)
        call app%hours_box%setSuffix(' h')
        call app%hours_box%setEnabled(.false.)
        call app%hours_box%valueChanged%connect(on_length)
        length_row = QHBoxLayout()
        call length_row%addWidget(app%one_orbit)
        call length_row%addWidget(app%hours_box)
        call form%addRow('After the burn', length_row)

        ! the orbit after the burn
        app%elements_label = QLabel('')
        call app%elements_label%setWordWrap(.true.)
        call form%addRow('Orbit', app%elements_label)

        reset_dv = QPushButton('Reset the delta-v')
        call reset_dv%clicked%connect(on_reset_dv)
        reset_views = QPushButton('Reset the views')
        call reset_views%clicked%connect(on_reset_views)
        reset_row = QHBoxLayout()
        call reset_row%addWidget(reset_dv)
        call reset_row%addWidget(reset_views)
        call form%addRow('', reset_row)

        call form%addRow(QLabel('<small>In the spacecraft view (right), drag the red arrow''s tip (left ' // &
                                'button) to change the delta-v: the trajectory follows in both views. ' // &
                                'Elsewhere: left drag rotates, right drag zooms, middle drag pans; space ' // &
                                'resets a view.</small>'))
        call panel%setMaximumWidth(420)
        dock = QDockWidget('Burn', app%main)
        call dock%setWidget(panel)
        call app%main%addDockWidget(Qt_LeftDockWidgetArea, dock)
    end subroutine build_ui

end module burn_demo

!> The burn demo (see the module documentation).
program vkframes_burn_demo

    use qt
    use burn_demo
    implicit none

    type(QApplication) :: qapp
    character(len=1024) :: arg
    integer :: i, ierr

    qapp = QApplication()
    i = 1
    do while (i <= command_argument_count())
        if (app%options%take(i)) cycle                      ! (--capture, --quit-after, ...: vkframes_qt)
        call get_command_argument(i, arg)
        select case (arg)
        case ('--drag-test')
            app%drag_test = .true.
        end select
        i = i + 1
    end do

    call build_model()
    call build_ui()
    call place_burn()
    call recompute()
    call apply_constraint()
    call app%options%start_quit_timer(app%main)
    call app%main%resize(1500, 820)
    call app%main%show()
    ierr = qapp%exec()

end program vkframes_burn_demo
