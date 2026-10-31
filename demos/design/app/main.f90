!> Trajectory design with the mouse, in a qt-fortran GUI: a spacecraft's
!> state (position and velocity at t = 0) drawn as arrows about the Earth
!> (VectorHandle), and the trajectory's start and end as markers on it
!> (TimeHandle), dragged with the mouse; the trajectory (two-body,
!> app/kepler.f90) is computed again at each move, as they are dragged.
!>
!> The scene (Earth-centered inertial axes, km): the Earth, the position
!> vector (yellow, from the Earth's center), the velocity vector (orange, at
!> the position; drawn 1500 s long by default: 1 km/s is 1500 km), the
!> trajectory through that state (cyan) from its start (a blue marker, at
!> or before t = 0: propagated backward) to its end (a green marker),
!> labeled with their times, with the orbit's periapsis and apoapsis (white
!> dots), and while something is dragged, the trajectory before the drag
!> (dashed). A trajectory that hits the Earth ends there (a red cross).
!> Hovering an arrow's tip or a marker lights it (an arrow's value is shown
!> next to it); a left drag moves it: an arrow changes the state, a marker
!> its time along the trajectory (also beyond its ends, propagated as it
!> goes: the TimeHandles' path is the two-body motion through the state,
!> DesignPath), to make the trajectory longer or shorter in time.
!>
!> How each arrow moves (the dock): freely (in the plane facing the camera),
!> in the orbit's plane, along itself (the altitude or the speed only), or
!> about its base (the direction only: the radius or the speed kept). The
!> dock also has the state as numbers (typed in, or updated by the drags),
!> the orbit's elements, the times drawn (one orbit, or from and to, in
!> hours: also set by the markers), the velocity arrow's scale, the view,
!> and a reset.
!>
!> Run with `pixi run -e qt design-demo` (from the vkframes root).
!> Options: `--view n` (1 oblique, 2 top), `--capture file` saves frame
!> `--capture-frame n` (default 120), `--quit-after seconds`, and, for
!> tests, `--drag-test`: the velocity arrow's tip is dragged with forwarded
!> mouse events (its speed changed along itself), then the end marker
!> (back along the trajectory), and the results printed.
module design_demo

    use qt
    use vkframes
    use vkframes_qt, only: connect_view, ViewLink
    use kepler
    use iso_c_binding

    implicit none

    integer, parameter :: n_views = 2
    character(len=12), parameter :: view_names(n_views) = [character(len=12) :: 'Oblique', 'Top']
    integer, parameter :: n_points = 1500                   !! points along the orbit
    character(len=32), parameter :: position_moves(4) = [character(len=32) :: 'Freely (facing the camera)', &
        'In the orbit''s plane', 'Altitude only (along it)', 'Direction only (radius kept)']
    character(len=32), parameter :: velocity_moves(4) = [character(len=32) :: 'Freely (facing the camera)', &
        'In the orbit''s plane', 'Speed only (along it)', 'Direction only (speed kept)']
    integer, parameter :: move_constraints(4) = [HANDLE_FREE, HANDLE_PLANE, HANDLE_RADIAL, HANDLE_SPHERE]
    real(wp), parameter :: start_r(3) = [12000.0_wp, 0.0_wp, 0.0_wp]       !! km (periapsis of an orbit to about 28,000 km)
    real(wp), parameter :: start_v(3) = [0.0_wp, 6.7_wp, 1.2_wp]           !! km/s
    real(wp), parameter :: r_color(3) = [1.0_wp, 0.85_wp, 0.25_wp], v_color(3) = [1.0_wp, 0.5_wp, 0.2_wp]
    real(wp), parameter :: orbit_color(3) = [0.3_wp, 0.85_wp, 1.0_wp]
    real(wp), parameter :: start_color(3) = [0.55_wp, 0.7_wp, 1.0_wp], end_color(3) = [0.45_wp, 1.0_wp, 0.55_wp]
    real(wp), parameter :: max_time = 240.0_wp * 3600.0_wp  !! the times drawn: at most 10 days either way (s)

    !> The trajectory's path for the time handles: the two-body motion through
    !> the state (r, v) at t = 0, at any time (km, s).
    type, extends(TimePath) :: DesignPath
        real(wp) :: r(3) = start_r                          !! the state's position (km)
        real(wp) :: v(3) = start_v                          !! its velocity (km/s)
    contains
        procedure :: position => design_path_position
    end type DesignPath

    !> The arrows' drags: the orbit computed again as they move.
    type, extends(WindowEventHandler) :: DesignHandler
    contains
        procedure :: handle_drag => design_handle_drag
        procedure :: before_draw => design_before_draw
    end type DesignHandler

    !> The GUI and the scene.
    type :: app_t
        ! the window
        type(QMainWindow) :: main
        type(QWindow) :: window
        type(QWidget) :: container
        type(QtfEventFilter) :: events
        type(ViewLink) :: view_link                         !! the window's events to the proxy (connect_view)
        type(WindowProxy) :: proxy
        type(View) :: views(n_views)
        integer :: frame = 0
        ! controls
        type(QDoubleSpinBox) :: r_box(3), v_box(3), t_from, t_to, v_scale
        type(QCheckBox) :: one_orbit, show_before
        type(QComboBox) :: r_move, v_move, view_choice
        type(QLabel) :: elements_label
        logical :: updating = .false.                       !! the demo sets the controls (not the user)
        type(HUDText) :: hud_state, hud_elements, callout
        ! the scene
        type(ReferenceFrame) :: root                        !! Earth-centered inertial (km)
        type(Sphere) :: earth
        type(VectorHandle) :: r_handle, v_handle
        type(TimeHandle) :: start_handle, end_handle        !! the trajectory's ends, dragged along it
        type(DesignPath) :: path                            !! their path (the state's two-body motion)
        type(Trajectory) :: orbit, before, apsides, impact
        type(CurveArtist) :: orbit_curve, before_curve
        type(MarkerArtist) :: apsis_dots, impact_mark
        type(DrawableTrajectory) :: orbit_frame, before_frame
        type(SkySphere) :: sky
        type(FrameManager) :: scene
        ! the state
        real(wp) :: r(3) = start_r, v(3) = start_v
        type(Elements) :: el
        real(wp) :: t0 = 0.0_wp, t1 = 0.0_wp                !! the times drawn (s from the state: t0 <= 0 <= t1)
        logical :: cut_start = .false., cut_end = .false.   !! the trajectory below the surface before its start, after its end
        ! options
        integer :: start_view = 1
        character(len=:), allocatable :: capture
        integer :: capture_frame = 120
        real(wp) :: quit_after = -1.0_wp
        logical :: drag_test = .false.
    end type app_t

    type(app_t), target, save :: app
    type(DesignHandler), target, save :: handler

contains

    ! ==================================================================
    ! the scene

    !> The scene: the Earth, the two arrows, the orbit (and the one before a
    !> drag, its apsides, an impact), and the sky.
    subroutine build_model()
        app%root = ReferenceFrame('ECI')
        call app%root%show_decorations(.false.)

        app%earth = Sphere('Earth', radius=earth_radius)
        call app%earth%set_texture_map('2k_earth_daymap.jpg')
        call app%earth%show_decorations(.false.)

        ! the handles: the position from the Earth's center, the velocity at the position
        app%r_handle = VectorHandle('Position', vector=app%r, color=r_color)
        call app%r_handle%set_head_size(900.0_wp)
        app%v_handle = VectorHandle('Velocity', vector=app%v, color=v_color, scale=1500.0_wp)
        call app%v_handle%set_head_size(900.0_wp)
        call app%v_handle%set_position(app%r)

        ! the trajectory's ends: markers dragged along it, labeled with their times
        app%start_handle = TimeHandle('Start', app%path, time=0.0_wp, t_min=-max_time, t_max=0.0_wp, color=start_color)
        app%end_handle = TimeHandle('End', app%path, time=0.0_wp, t_min=0.0_wp, t_max=max_time, color=end_color)
        call app%start_handle%show_name_label(.true.)
        call app%end_handle%show_name_label(.true.)
        call app%start_handle%set_label_size(15)
        call app%end_handle%set_label_size(15)
        call app%start_handle%set_label_alignment(ALIGN_LEFT_CENTER)
        call app%end_handle%set_label_alignment(ALIGN_LEFT_CENTER)
        call app%end_handle%set_priority(2)                     ! (one orbit: the end on the start, the end first)

        ! the orbit, the one before a drag (dashed), the apsides, an impact
        app%orbit = Trajectory()
        call app%orbit%reserve(n_points)
        app%orbit_curve = CurveArtist(app%orbit, color=orbit_color, width=2.5_wp)
        app%before = Trajectory()
        call app%before%reserve(n_points)
        app%before_curve = CurveArtist(app%before, color=[0.7_wp, 0.7_wp, 0.7_wp, 0.6_wp], width=1.5_wp)
        call app%before_curve%set_pattern(6, int(z'00FF'))
        call app%before_curve%set_pickable(.false.)
        app%apsides = Trajectory()
        app%apsis_dots = MarkerArtist(app%apsides, markers=ior(MARKER_START, MARKER_END), size=9.0_wp, &
                                      color=[1.0_wp, 1.0_wp, 1.0_wp])
        app%impact = Trajectory()
        app%impact_mark = MarkerArtist(app%impact, markers=ior(MARKER_START, MARKER_END), size=16.0_wp, &
                                       color=[1.0_wp, 0.2_wp, 0.2_wp])
        call app%impact_mark%set_marker_shape(MARKER_X)
        app%orbit_frame = DrawableTrajectory('Orbit')
        call app%orbit_frame%show_decorations(.false.)
        call app%orbit_frame%add_artist(app%orbit_curve)
        call app%orbit_frame%add_artist(app%apsis_dots)
        call app%orbit_frame%add_artist(app%impact_mark)
        app%before_frame = DrawableTrajectory('Before the drag')
        call app%before_frame%show_decorations(.false.)
        call app%before_frame%add_artist(app%before_curve)
        call app%before_frame%show_contents(.false.)

        if (.not. (app%root%add_child(app%earth) .and. app%root%add_child(app%orbit_frame) .and. &
                   app%root%add_child(app%before_frame) .and. app%root%add_child(app%r_handle) .and. &
                   app%root%add_child(app%v_handle) .and. app%root%add_child(app%start_handle) .and. &
                   app%root%add_child(app%end_handle))) &
            error stop 'design demo: could not build the scene'
        app%scene = FrameManager(app%root)

        app%sky = SkySphere()
        if (.not. app%sky%set_star_data('Stars_HYGv3.txt', min_mag=-2.0_wp, max_mag=6.0_wp, max_stars=20000)) &
            write(*, '(a)') 'design demo: no star catalog'
    end subroutine build_model

    !> The path's position at time `t` (s from the state).
    function design_path_position(self, t) result(p)
        class(DesignPath), intent(in) :: self
        real(wp), intent(in) :: t                           !! the time (s from the state)
        real(wp) :: p(3)
        p = position_at(self%r, self%v, t)
    end function design_path_position

    !> The trajectory computed again from the state (r, v) and the times drawn
    !> (one orbit from the state, if checked and bound), and everything
    !> showing it.
    subroutine recompute()
        real(wp) :: t(n_points), r(3, n_points), rp(3), ra(3), e_dir(3)
        integer :: n
        app%el = elements_of(app%r, app%v)
        app%path%r = app%r
        app%path%v = app%v
        if (app%one_orbit%isChecked() .and. app%el%period < huge(1.0_wp)) then
            app%t0 = 0.0_wp
            app%t1 = min(app%el%period, max_time)
        end if
        n = n_points
        call propagate(app%r, app%v, app%t0, app%t1, n, t, r, app%cut_start, app%cut_end)
        call app%orbit%clear()
        call app%orbit%add(t(1:n), pos=r(:, 1:n))
        ! the ends' markers there (not draggable below the surface), labeled with their times
        call app%start_handle%set_time_range(merge(t(1), -max_time, app%cut_start), 0.0_wp)
        call app%start_handle%set_time(t(1))
        call app%start_handle%set_name('   start '//num(t(1) / 3600.0_wp, 2)//' h')    ! (spaces: clear of the marker)
        call app%end_handle%set_time_range(0.0_wp, merge(t(n), max_time, app%cut_end))
        call app%end_handle%set_time(t(n))
        call app%end_handle%set_name('   end '//num(t(n) / 3600.0_wp, 2)//' h')
        ! the apsides: along the eccentricity vector
        call app%apsides%clear()
        if (app%el%e > 1.0e-6_wp) then
            e_dir = ((dot_product(app%v, app%v) - mu / norm2(app%r)) * app%r - dot_product(app%r, app%v) * app%v) / mu
            e_dir = e_dir / norm2(e_dir)
            rp = app%el%rp * e_dir
            call app%apsides%add(0.0_wp, pos=rp)
            if (app%el%ra < huge(1.0_wp)) then
                ra = -app%el%ra * e_dir
                call app%apsides%add(1.0_wp, pos=ra)
            end if
        end if
        call app%impact%clear()
        if (app%cut_start) call app%impact%add(t(1), pos=r(:, 1))
        if (app%cut_end) call app%impact%add(t(n), pos=r(:, n))
        call update_readouts()
        call apply_constraints()
    end subroutine recompute

    !> The handles' constraints from the dock (the orbit's plane: its normal now).
    subroutine apply_constraints()
        real(wp) :: h(3)
        h = app%el%h
        if (norm2(h) <= 0.0_wp) h = [0.0_wp, 0.0_wp, 1.0_wp]
        call app%r_handle%set_constraint(move_constraints(app%r_move%currentIndex() + 1), h)
        call app%v_handle%set_constraint(move_constraints(app%v_move%currentIndex() + 1), h)
    end subroutine apply_constraints

    !> The state and elements in the HUD and the dock, and the dock's numbers.
    subroutine update_readouts()
        integer :: k
        call app%hud_state%set_text('r = ['//num(app%r(1), 1)//', '//num(app%r(2), 1)//', '//num(app%r(3), 1)// &
                                    '] km,  v = ['//num(app%v(1), 3)//', '//num(app%v(2), 3)//', '//num(app%v(3), 3)// &
                                    '] km/s,  altitude '//num(norm2(app%r) - earth_radius, 1)//' km,  speed '// &
                                    num(norm2(app%v), 3)//' km/s')
        call app%hud_elements%set_text(elements_text(.false.))
        call app%elements_label%setText(elements_text(.true.))
        app%updating = .true.
        do k = 1, 3
            call app%r_box(k)%setValue(real(app%r(k), c_double))
            call app%v_box(k)%setValue(real(app%v(k), c_double))
        end do
        call app%t_from%setValue(real(app%t0 / 3600.0_wp, c_double))
        call app%t_to%setValue(real(app%t1 / 3600.0_wp, c_double))
        app%updating = .false.
    end subroutine update_readouts

    !> The orbital elements as text: one item a line (`lines`), or on two lines.
    function elements_text(lines) result(text)
        logical, intent(in) :: lines                        !! true: one item a line (the dock); false: two lines (the HUD)
        character(len=:), allocatable :: text, sep, mid
        sep = ';  '
        mid = new_line('a')
        if (lines) sep = new_line('a')
        if (lines) mid = sep
        associate (el => app%el)
            if (el%e < 1.0_wp) then
                text = 'a '//num(el%a, 1)//' km'//sep//'e '//num(el%e, 4)//sep//'i '//num(el%i, 2)//' deg'//sep// &
                       'RAAN '//num(el%raan, 2)//' deg'//sep//'arg. of periapsis '//num(el%argp, 2)//' deg'//mid// &
                       'periapsis altitude '//num(el%rp - earth_radius, 1)//' km'//sep// &
                       'apoapsis altitude '//num(el%ra - earth_radius, 1)//' km'//sep// &
                       'period '//num(el%period / 3600.0_wp, 2)//' h'
            else
                text = 'not bound (e '//num(el%e, 4)//')'//sep//'i '//num(el%i, 2)//' deg'//sep// &
                       'RAAN '//num(el%raan, 2)//' deg'//mid//'periapsis altitude '//num(el%rp - earth_radius, 1)// &
                       ' km'//sep//'excess speed '//num(sqrt(max(0.0_wp, 2.0_wp * el%energy)), 3)//' km/s'
            end if
        end associate
        if (app%cut_end) text = text//sep//'hits the Earth'
        if (app%cut_start) text = text//sep//'from the Earth'
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
    ! the handles

    !> An arrow grabbed, dragged or released: the state from it, the orbit
    !> again (and the orbit before the drag shown while it lasts).
    subroutine design_handle_drag(self, window, handle, stage)
        class(DesignHandler), intent(inout) :: self
        class(WindowProxy), intent(inout) :: window         !! the window the arrow is dragged in
        class(DragHandle), intent(inout), target :: handle  !! the arrow (app%r_handle or app%v_handle)
        integer, intent(in) :: stage                        !! DRAG_START, DRAG_MOVE or DRAG_END
        class(DragHandle), pointer :: h
        real(wp), pointer :: t(:), r(:,:)
        h => handle
        select case (stage)
        case (DRAG_START)
            call app%before%clear()
            call app%orbit%times(t)
            call app%orbit%positions(r)
            if (associated(t)) call app%before%add(t, pos=r)
            call app%before_frame%show_contents(app%show_before%isChecked())
        case (DRAG_MOVE)
            if (associated(h, app%r_handle)) then
                app%r = app%r_handle%vector()
                call app%v_handle%set_position(app%r)
            else if (associated(h, app%v_handle)) then
                app%v = app%v_handle%vector()
            else if (associated(h, app%start_handle) .or. associated(h, app%end_handle)) then
                app%t0 = app%start_handle%time()                   ! (the times drawn from now on, not one orbit)
                app%t1 = app%end_handle%time()
                app%updating = .true.
                call app%one_orbit%setChecked(.false.)
                app%updating = .false.
            end if
            call recompute()
        case (DRAG_END)
            call app%before_frame%show_contents(.false.)
            call apply_constraints()
        end select
    end subroutine design_handle_drag

    !> The callout: the value of the arrow under the cursor or dragged, next to its tip.
    subroutine design_before_draw(self, window, t)
        class(DesignHandler), intent(inout) :: self
        class(WindowProxy), intent(inout) :: window         !! the window about to draw
        real(wp), intent(in) :: t                           !! the simulation time (s; paused here)
        real(wp) :: at(2), tip(3)
        character(len=:), allocatable :: text
        if (app%r_handle%hovered() .or. app%r_handle%dragged()) then
            tip = app%r
            text = 'r '//num(norm2(app%r), 1)//' km (altitude '//num(norm2(app%r) - earth_radius, 1)//' km)'
        else if (app%v_handle%hovered() .or. app%v_handle%dragged()) then
            tip = app%r + app%v_handle%tip()
            text = 'v '//num(norm2(app%v), 3)//' km/s'
        else
            call app%callout%show(.false.)
            return
        end if
        if (.not. window%screen_position(tip, at)) then
            call app%callout%show(.false.)
            return
        end if
        call app%callout%set_text(text)
        call app%callout%set_position(at + [0.012_wp, 0.012_wp])
        call app%callout%show(.true.)
    end subroutine design_before_draw

    ! ==================================================================
    ! the window

    !> The views, the HUD texts, and the QWindow the scene is drawn in, with
    !> its events connected.
    subroutine build_window()
        app%views(1) = View(app%root, app%root)
        call app%views(1)%set_default_view_parameters(eye=[4000.0_wp, -38000.0_wp, 76000.0_wp], &
                                                      center=[-6000.0_wp, 0.0_wp, 0.0_wp], up=[0.0_wp, 0.0_wp, 1.0_wp])
        app%views(2) = View(app%root, app%root)
        call app%views(2)%set_default_view_parameters(eye=[-6000.0_wp, 0.0_wp, 100000.0_wp], &
                                                      center=[-6000.0_wp, 0.0_wp, 0.0_wp], up=[0.0_wp, 1.0_wp, 0.0_wp])
        app%hud_state = HUDText('', position=[0.01_wp, 0.99_wp], alignment=ALIGN_LEFT_TOP, size=16.0_wp, &
                                color=[0.95_wp, 0.95_wp, 0.95_wp])
        app%hud_elements = HUDText('', position=[0.01_wp, 0.01_wp], alignment=ALIGN_LEFT_BOTTOM, size=15.0_wp, &
                                   color=orbit_color)
        app%callout = HUDText('', position=[0.5_wp, 0.5_wp], alignment=ALIGN_LEFT_BOTTOM, size=16.0_wp, &
                              color=[1.0_wp, 1.0_wp, 1.0_wp])
        call app%callout%show(.false.)

        app%window = QWindow()
        app%container = QWidget_createWindowContainer(app%window)
        call app%container%setMinimumSize(320, 240)
        app%events = QtfEventFilter(app%window)
        call app%events%exposeEvent%connect(on_expose)
        call app%events%updateRequest%connect(on_update)
        call connect_view(app%view_link, app%events, app%proxy, app%window)   ! (mouse, keys, resizes, the surface)
    end subroutine build_window

    !> Open the view (on the window's first expose, or again on a new native
    !> window: the views, HUD texts and settings are the proxy's already) and
    !> show the scene.
    subroutine open_view()
        integer :: k
        if (.not. app%proxy%open(app%window%width(), app%window%height(), 'vkframes design', &
                                 native_window=app%window%winId(), pixel_ratio=app%window%devicePixelRatio())) then
            write(*, '(a)') 'design demo: could not open the view'
            return
        end if
        if (app%proxy%num_views() > 0) return                    ! (opened again)
        call app%proxy%set_scene(app%scene)
        call app%proxy%set_event_handler(handler)
        call app%proxy%set_background_color([0.0_wp, 0.0_wp, 0.0_wp])
        call app%proxy%set_sky(app%sky)
        do k = 1, n_views
            call app%proxy%add_view(app%views(k))
        end do
        call app%proxy%select_view(app%view_choice%currentIndex() + 1)
        call app%proxy%add_hud_text(app%hud_state)
        call app%proxy%add_hud_text(app%hud_elements)
        call app%proxy%add_hud_text(app%callout)
        call app%proxy%pause_time(.true.)                        ! (the state is designed, not flown)
    end subroutine open_view

    !> The window shown: the view opened (the first time), the frames started.
    subroutine on_expose(data)
        class(*), intent(inout), optional :: data           !! the connection's data (unused)
        if (app%proxy%is_open() .or. .not. app%window%isExposed()) return
        call open_view()
        if (app%proxy%is_open()) call app%window%requestUpdate()
    end subroutine on_expose

    !> Render a frame, ask for the next one.
    subroutine on_update(data)
        class(*), intent(inout), optional :: data           !! the connection's data (unused)
        if (app%window%devicePixelRatio() /= app%proxy%pixel_ratio()) &
            call app%proxy%resize(nint(app%window%width() * app%window%devicePixelRatio()), &
                                  nint(app%window%height() * app%window%devicePixelRatio()), &
                                  pixel_ratio=app%window%devicePixelRatio())
        if (.not. app%proxy%frame()) return
        app%frame = app%frame + 1
        if (app%drag_test) call drag_test_step()
        if (allocated(app%capture) .and. app%frame == app%capture_frame) then
            if (app%proxy%capture(app%capture)) write(*, '(a)') 'captured '//app%capture
        end if
        call app%window%requestUpdate()
    end subroutine on_update

    !> --drag-test: the velocity arrow's tip hovered (frame 10), grabbed (12),
    !> dragged along itself (14 to 30: its speed only) and released (32); then
    !> the end marker hovered (40), grabbed (42), dragged toward the point a
    !> quarter of the way back along the trajectory (44 to 51) and released
    !> (53): with mouse events forwarded as from the window, the results
    !> printed (34, 55).
    subroutine drag_test_step()
        real(wp), save :: at(2), speed0, end0
        integer, save :: px, py, dx, dy
        real(wp) :: tip(3), ahead(2)
        integer :: width, height
        width = nint(app%window%width() * app%window%devicePixelRatio())
        height = nint(app%window%height() * app%window%devicePixelRatio())
        select case (app%frame)
        case (10)
            call app%v_move%setCurrentIndex(2)                   ! (speed only)
            tip = app%r + app%v_handle%tip()
            if (.not. app%proxy%screen_position(tip, at)) error stop 'design demo: the velocity tip is off the screen'
            px = nint(at(1) * width)
            py = nint((1.0_wp - at(2)) * height)
            if (.not. app%proxy%screen_position(tip + 0.2_wp * app%v_handle%tip(), ahead)) error stop 'design demo: off'
            dx = nint((ahead(1) - at(1)) * width / 32)           ! (17 moves: about 0.1 of the arrow longer)
            dy = -nint((ahead(2) - at(2)) * height / 32)
            speed0 = norm2(app%v)
            call app%proxy%mouse(MOUSE_MOVE, px, py, 0, 0)
        case (12)
            write(*, '(a,l1)') 'drag test: hovered ', app%v_handle%hovered()
            call app%proxy%mouse(MOUSE_PRESS, px, py, 256, MOUSE_LEFT)
        case (14:30)
            px = px + dx
            py = py + dy
            call app%proxy%mouse(MOUSE_MOVE, px, py, 256, 0)
        case (32)
            call app%proxy%mouse(MOUSE_RELEASE, px, py, 0, MOUSE_LEFT)
        case (34)
            write(*, '(a)') 'drag test: speed '//num(speed0, 3)//' -> '//num(norm2(app%v), 3)//' km/s'
            write(*, '(a)') 'drag test: '//elements_text(.false.)
        case (40)
            end0 = app%end_handle%time()
            if (.not. (app%proxy%screen_position(app%end_handle%position(), at) .and. &
                       app%proxy%screen_position(app%path%position(0.75_wp * end0), ahead))) &
                error stop 'design demo: the end marker is off the screen'
            px = nint(at(1) * width)
            py = nint((1.0_wp - at(2)) * height)
            dx = nint((ahead(1) - at(1)) * width / 8)
            dy = -nint((ahead(2) - at(2)) * height / 8)
            call app%proxy%mouse(MOUSE_MOVE, px, py, 0, 0)
        case (42)
            write(*, '(a,l1)') 'drag test: end marker hovered ', app%end_handle%hovered()
            call app%proxy%mouse(MOUSE_PRESS, px, py, 256, MOUSE_LEFT)
        case (44:51)
            px = px + dx
            py = py + dy
            call app%proxy%mouse(MOUSE_MOVE, px, py, 256, 0)
        case (53)
            call app%proxy%mouse(MOUSE_RELEASE, px, py, 0, MOUSE_LEFT)
        case (55)
            write(*, '(a)') 'drag test: end '//num(end0 / 3600.0_wp, 3)//' -> '// &
                            num(app%end_handle%time() / 3600.0_wp, 3)//' h (aimed at '// &
                            num(0.75_wp * end0 / 3600.0_wp, 3)//' h), the trajectory to '// &
                            num(app%t1 / 3600.0_wp, 3)//' h'
        end select
    end subroutine drag_test_step

    ! ==================================================================
    ! controls

    !> A number of the state typed in: the arrows and the orbit from it.
    subroutine on_state(value, data)
        real(c_double), intent(in) :: value                 !! the box's new value (all six are read)
        class(*), intent(inout), optional :: data           !! the connection's data (unused)
        integer :: k
        if (app%updating) return
        do k = 1, 3
            app%r(k) = app%r_box(k)%value()
            app%v(k) = app%v_box(k)%value()
        end do
        call show_state()
    end subroutine on_state

    !> The arrows at the state, and the orbit from it.
    subroutine show_state()
        call app%r_handle%set_vector(app%r)
        call app%v_handle%set_position(app%r)
        call app%v_handle%set_vector(app%v)
        call recompute()
    end subroutine show_state

    !> A time drawn typed in (from or to): the trajectory between them (not one orbit).
    subroutine on_times(value, data)
        real(c_double), intent(in) :: value                 !! hours (both boxes are read)
        class(*), intent(inout), optional :: data           !! the connection's data (unused)
        if (app%updating) return
        app%t0 = app%t_from%value() * 3600.0_wp
        app%t1 = app%t_to%value() * 3600.0_wp
        app%updating = .true.
        call app%one_orbit%setChecked(.false.)
        app%updating = .false.
        call recompute()
    end subroutine on_times

    !> "One orbit" checked or not: the orbit drawn again.
    subroutine on_one_orbit(checked, data)
        logical, intent(in) :: checked                      !! true: one period from the state (read by recompute)
        class(*), intent(inout), optional :: data           !! the connection's data (unused)
        if (app%updating) return
        call recompute()
    end subroutine on_one_orbit

    !> The velocity arrow's scale.
    subroutine on_v_scale(value, data)
        real(c_double), intent(in) :: value                 !! km per km/s
        class(*), intent(inout), optional :: data           !! the connection's data (unused)
        call app%v_handle%set_display_scale(real(value, wp))
    end subroutine on_v_scale

    !> How an arrow moves, chosen: the constraints set again.
    subroutine on_move_choice(index, data)
        integer, intent(in) :: index                        !! the choice (both boxes are read)
        class(*), intent(inout), optional :: data           !! the connection's data (unused)
        call apply_constraints()
    end subroutine on_move_choice

    !> The state as at the start, and its orbit.
    subroutine on_reset_state(data)
        class(*), intent(inout), optional :: data           !! the connection's data (unused)
        app%r = start_r
        app%v = start_v
        call show_state()
    end subroutine on_reset_state

    !> A view chosen: shown.
    subroutine on_view(index, data)
        integer, intent(in) :: index                        !! the view (0-based)
        class(*), intent(inout), optional :: data           !! the connection's data (unused)
        if (app%proxy%is_open()) call app%proxy%select_view(index + 1)
    end subroutine on_view

    !> The view's camera reset.
    subroutine on_reset_view(data)
        class(*), intent(inout), optional :: data           !! the connection's data (unused)
        type(View), pointer :: v
        v => app%proxy%current_view()
        call v%reset_view()
    end subroutine on_reset_view

    !> Quit (--quit-after): the main window closed.
    subroutine on_quit(data)
        class(*), intent(inout), optional :: data           !! the connection's data (unused)
        call app%main%close()
    end subroutine on_quit

    !> The main window closes: close the view (waiting for the GPU) first.
    subroutine on_close(event, data)
        type(QCloseEvent), intent(in) :: event              !! the close (accepted)
        class(*), intent(inout), optional :: data           !! the connection's data (unused)
        call app%proxy%close()
    end subroutine on_close

    !> A spin box for a component: `decimals`, a step and a range.
    function component_box(value, low, high, step, decimals) result(box)
        real(wp), intent(in) :: value                       !! the value shown first
        real(wp), intent(in) :: low                         !! the smallest allowed
        real(wp), intent(in) :: high                        !! the largest allowed
        real(wp), intent(in) :: step                        !! the arrows' step
        integer, intent(in) :: decimals                     !! decimals shown
        type(QDoubleSpinBox) :: box
        box = QDoubleSpinBox()
        call box%setRange(real(low, c_double), real(high, c_double))
        call box%setDecimals(decimals)
        call box%setSingleStep(real(step, c_double))
        call box%setValue(real(value, c_double))
        call box%valueChanged%connect(on_state)
    end function component_box

    !> The main window: the scene in the middle, the dock (the state, how the
    !> arrows move, how long the orbit is drawn, its elements, the view) on the left.
    subroutine build_ui()
        type(QDockWidget) :: dock
        type(QWidget) :: panel
        type(QFormLayout) :: form
        type(QPushButton) :: reset_view, reset_state
        type(QHBoxLayout) :: r_row, v_row, view_row, length_row
        integer :: k

        app%main = QMainWindow()
        call app%main%setWindowTitle('vkframes: trajectory design (drag the position and velocity arrows)')
        call app%main%closeEvent%connect(on_close)
        call build_window()
        call app%main%setCentralWidget(app%container)

        panel = QWidget()
        form = QFormLayout(panel)

        ! the state, as numbers
        r_row = QHBoxLayout()
        v_row = QHBoxLayout()
        do k = 1, 3
            app%r_box(k) = component_box(app%r(k), -200000.0_wp, 200000.0_wp, 100.0_wp, 1)
            app%v_box(k) = component_box(app%v(k), -20.0_wp, 20.0_wp, 0.05_wp, 3)
            call r_row%addWidget(app%r_box(k))
            call v_row%addWidget(app%v_box(k))
        end do
        call form%addRow('Position (km)', r_row)
        call form%addRow('Velocity (km/s)', v_row)
        reset_state = QPushButton('Reset the state')
        call reset_state%clicked%connect(on_reset_state)
        call form%addRow('', reset_state)

        ! how the arrows move
        app%r_move = QComboBox()
        call app%r_move%addItems(position_moves)
        call app%r_move%currentIndexChanged%connect(on_move_choice)
        call form%addRow('Position arrow moves', app%r_move)
        app%v_move = QComboBox()
        call app%v_move%addItems(velocity_moves)
        call app%v_move%currentIndexChanged%connect(on_move_choice)
        call form%addRow('Velocity arrow moves', app%v_move)
        app%v_scale = QDoubleSpinBox()
        call app%v_scale%setRange(100.0_c_double, 5000.0_c_double)
        call app%v_scale%setSingleStep(100.0_c_double)
        call app%v_scale%setDecimals(0)
        call app%v_scale%setValue(1500.0_c_double)
        call app%v_scale%setSuffix(' km per km/s')
        call app%v_scale%valueChanged%connect(on_v_scale)
        call form%addRow('Velocity arrow scale', app%v_scale)

        ! the times drawn (also set by dragging the markers)
        app%one_orbit = QCheckBox('One orbit')
        call app%one_orbit%setChecked(.false.)
        call app%one_orbit%toggled%connect(on_one_orbit)
        app%t_from = QDoubleSpinBox()
        call app%t_from%setRange(real(-max_time / 3600.0_wp, c_double), 0.0_c_double)
        call app%t_from%setDecimals(3)
        call app%t_from%setPrefix('from ')
        call app%t_from%setSuffix(' h')
        call app%t_from%valueChanged%connect(on_times)
        app%t_to = QDoubleSpinBox()
        call app%t_to%setRange(0.0_c_double, real(max_time / 3600.0_wp, c_double))
        call app%t_to%setDecimals(3)
        call app%t_to%setPrefix('to ')
        call app%t_to%setSuffix(' h')
        call app%t_to%valueChanged%connect(on_times)
        length_row = QHBoxLayout()
        call length_row%addWidget(app%one_orbit)
        call length_row%addWidget(app%t_from)
        call length_row%addWidget(app%t_to)
        call form%addRow('Drawn', length_row)
        call form%addRow(QLabel('<small>(hours from the state; or drag the start and end markers along ' // &
                                'the trajectory)</small>'))
        app%show_before = QCheckBox('The orbit before the drag (dashed)')
        call app%show_before%setChecked(.true.)
        call form%addRow('While dragging', app%show_before)

        ! the elements
        app%elements_label = QLabel('')
        call app%elements_label%setWordWrap(.true.)
        call form%addRow('Orbit', app%elements_label)

        ! the view
        app%view_choice = QComboBox()
        call app%view_choice%addItems(view_names)
        call app%view_choice%setCurrentIndex(app%start_view - 1)
        call app%view_choice%currentIndexChanged%connect(on_view)
        reset_view = QPushButton('Reset view')
        call reset_view%clicked%connect(on_reset_view)
        view_row = QHBoxLayout()
        call view_row%addWidget(app%view_choice)
        call view_row%addWidget(reset_view)
        call form%addRow('View', view_row)

        call form%addRow(QLabel('<small>Drag an arrow''s tip (left button) to change the state, or a marker ' // &
                                'to change the start or end time: the trajectory follows. Elsewhere: left drag ' // &
                                'rotates, right drag zooms, middle drag pans; ' // &
                                'space resets the view.</small>'))
        call panel%setMaximumWidth(460)
        dock = QDockWidget('Design', app%main)
        call dock%setWidget(panel)
        call app%main%addDockWidget(Qt_LeftDockWidgetArea, dock)
    end subroutine build_ui

end module design_demo

!> The trajectory design demo (see the module documentation).
program vkframes_design_demo

    use qt
    use design_demo
    implicit none

    type(QApplication) :: qapp
    character(len=1024) :: arg
    integer :: i, ierr

    qapp = QApplication()
    i = 1
    do while (i <= command_argument_count())
        call get_command_argument(i, arg)
        select case (arg)
        case ('--view')
            call get_command_argument(i + 1, arg)
            read(arg, *) app%start_view
            app%start_view = max(1, min(n_views, app%start_view))
            i = i + 1
        case ('--capture')
            call get_command_argument(i + 1, arg)
            app%capture = trim(arg)
            i = i + 1
        case ('--capture-frame')
            call get_command_argument(i + 1, arg)
            read(arg, *) app%capture_frame
            i = i + 1
        case ('--quit-after')
            call get_command_argument(i + 1, arg)
            read(arg, *) app%quit_after
            i = i + 1
        case ('--drag-test')
            app%drag_test = .true.
        end select
        i = i + 1
    end do

    call build_model()
    call build_ui()
    app%el = elements_of(app%r, app%v)
    app%t1 = 0.75_wp * app%el%period                        ! (three quarters of an orbit: the end clear of the state)
    call show_state()
    if (app%quit_after > 0) call QTimer_singleShot(int(1000 * app%quit_after), on_quit)
    call app%main%resize(1400, 860)
    call app%main%show()
    ierr = qapp%exec()

end program vkframes_design_demo
