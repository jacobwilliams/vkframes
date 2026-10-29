!> A trajectory optimization program's iterations, played back, in a
!> qt-fortran GUI: an Earth-to-Mars transfer solved by multiple shooting
!> (app/shooting.f90). The trajectory is a chain of segments, each
!> propagated from its own initial state (its node); the solver (Newton's
!> method on the gaps between them, damped far from the solution) moves the
!> nodes until the chain is one continuous trajectory from the Earth at
!> departure to Mars at arrival. Every iteration is recorded.
!>
!> The scene (heliocentric, ecliptic axes, AU): the Sun, the Earth's and
!> Mars's orbits, the Earth at departure and Mars at arrival, and the
!> iteration shown: each segment a curve in its own color (from its node, a
!> white dot), the gaps red lines from each segment's end to the next node
!> (the last: to Mars), and optionally the previous iteration, faint. A
!> click on a segment (or its row) selects it: arrows run along it.
!>
!> The dock: the iteration (a slider over the recorded iterations, play
!> and pause at a number of iterations per second), the solver's options
!> (the number of segments, the step fraction while far from the solution,
!> the initial guess's noise and seed: solved again when one changes), what
!> is shown, the view, and two tables: the segments of the iteration shown
!> (their times and gaps) and the iterations (their largest gaps and step;
!> a click shows that iteration).
!>
!> Run with `pixi run -e qt shooting-demo` (from the vkframes root).
!> Options: `--segments n` (2 to 16), `--step f` (0.01 to 1), `--noise x` (0
!> to 1), `--seed k`, `--iteration k` (0: the initial guess; -1: the last),
!> `--play`, `--select k` (a segment), `--view n` (1 top, 2 oblique),
!> `--capture file` saves frame `--capture-frame n` (default 120),
!> `--quit-after seconds`.
module shooting_demo

    use qt
    use vkframes
    use shooting
    use iso_c_binding

    implicit none

    integer, parameter :: n_views = 2
    character(len=12), parameter :: view_names(n_views) = [character(len=12) :: 'Top', 'Oblique']
    real(wp), parameter :: palette(3, 8) = reshape([0.35_wp, 0.75_wp, 1.0_wp, 1.0_wp, 0.7_wp, 0.25_wp, &
        0.45_wp, 0.9_wp, 0.45_wp, 0.95_wp, 0.45_wp, 0.85_wp, 1.0_wp, 0.9_wp, 0.35_wp, 0.4_wp, 0.95_wp, 0.9_wp, &
        0.7_wp, 0.55_wp, 1.0_wp, 0.95_wp, 0.6_wp, 0.55_wp], [3, 8])
    real(wp), parameter :: gap_color(3) = [1.0_wp, 0.2_wp, 0.2_wp], ghost_color(4) = [0.6_wp, 0.6_wp, 0.6_wp, 0.35_wp]
    character(len=*), parameter :: segment_headers(4) = [character(len=24) :: 'Segment', 'Days', &
        'Position gap (km)', 'Velocity gap (m/s)']
    character(len=*), parameter :: iteration_headers(4) = [character(len=24) :: 'Iteration', 'Largest gap (km)', &
        'Largest gap (m/s)', 'Step']

    !> Clicks on segments select them.
    type, extends(WindowEventHandler) :: ShootingHandler
        real(wp) :: pressed(2) = 0.0_wp
        logical :: down = .false.
    contains
        procedure :: button_press => shooting_press
        procedure :: button_release => shooting_release
    end type ShootingHandler

    !> The GUI and the scene.
    type :: app_t
        ! the window
        type(QMainWindow) :: main
        type(QWindow) :: window
        type(QWidget) :: container
        type(QtfEventFilter) :: events
        type(WindowProxy) :: proxy
        type(View) :: views(n_views)
        integer :: frame = 0
        ! controls
        type(QSlider) :: iteration_slider
        type(QLabel) :: iteration_label, status_label
        type(QPushButton) :: play
        type(QTimer) :: timer
        type(QSpinBox) :: speed, segments, seed
        type(QDoubleSpinBox) :: step, noise
        type(QCheckBox) :: show_previous, show_nodes, show_gaps, show_orbits
        type(QComboBox) :: view_choice
        type(QTableWidget) :: segment_table, iteration_table
        logical :: updating = .false.                         !! the demo sets the controls (not the user)
        type(HUDText) :: hud_iteration, hud_speeds
        ! the scene
        type(ReferenceFrame) :: root                          !! the Sun's center, ecliptic axes (AU)
        type(Sphere) :: sun, earth, mars
        type(Trajectory) :: earth_orbit, mars_orbit
        type(CurveArtist) :: earth_curve, mars_curve
        type(DrawableTrajectory) :: orbits, segments_frame, previous_frame, nodes_frame, gaps_frame
        type(Trajectory) :: seg(max_segments), prev(max_segments)
        type(CurveArtist) :: seg_curve(max_segments), prev_curve(max_segments)
        type(Trajectory) :: nodes, gaps
        type(MarkerArtist) :: node_dots
        type(SegmentArtist) :: gap_lines
        type(SkySphere) :: sky
        type(FrameManager) :: scene
        ! the solver
        type(Solution) :: sol
        integer :: shown = 1                                  !! the iteration shown (1: the initial guess)
        integer :: selected = 0                               !! the segment selected (0: none)
        ! options
        integer :: start_segments = 8, start_seed = 7, start_iteration = 0, start_view = 2
        real(wp) :: start_step = 0.3_wp, start_noise = 0.3_wp
        logical :: start_play = .false.
        character(len=:), allocatable :: capture
        integer :: capture_frame = 120
        real(wp) :: quit_after = -1.0_wp
    end type app_t

    type(app_t), target, save :: app
    type(ShootingHandler), target, save :: handler

contains

    ! ==================================================================
    ! the scene

    subroutine build_model()
        real(wp) :: t, x(6)
        integer :: k
        app%root = ReferenceFrame('Ecliptic')
        call app%root%show_decorations(.false.)

        app%sun = Sphere('Sun', radius=0.04_wp, color=[1.0_wp, 0.85_wp, 0.35_wp])
        call app%sun%set_material(emission=[1.0_wp, 0.85_wp, 0.35_wp])
        call app%sun%set_light_source_enabled(.true.)
        call app%sun%show_decorations(.false.)
        call app%sun%show_name_label(.true.)

        x = earth_state(0.0_wp)
        app%earth = Sphere('Earth (departure)', radius=0.02_wp, color=[0.35_wp, 0.6_wp, 1.0_wp])
        call app%earth%set_position(x(1:3))
        call app%earth%show_decorations(.false.)
        call app%earth%show_name_label(.true.)
        x = mars_state(tof)
        app%mars = Sphere('Mars (arrival)', radius=0.015_wp, color=[1.0_wp, 0.45_wp, 0.3_wp])
        call app%mars%set_position(x(1:3))
        call app%mars%show_decorations(.false.)
        call app%mars%show_name_label(.true.)

        ! the planets' orbits (one period each, from t = 0)
        app%earth_orbit = Trajectory()
        app%mars_orbit = Trajectory()
        do k = 0, 360
            t = 365.25_wp * k / 360.0_wp
            x = earth_state(t)
            call app%earth_orbit%add(t, pos=x(1:3))
            t = 687.0_wp * k / 360.0_wp
            x = mars_state(t)
            call app%mars_orbit%add(t, pos=x(1:3))
        end do
        app%earth_curve = CurveArtist(app%earth_orbit, color=[0.35_wp, 0.6_wp, 1.0_wp, 0.5_wp], width=1.0_wp)
        app%mars_curve = CurveArtist(app%mars_orbit, color=[1.0_wp, 0.45_wp, 0.3_wp, 0.5_wp], width=1.0_wp)
        app%orbits = DrawableTrajectory('Orbits')
        call app%orbits%show_decorations(.false.)
        call app%orbits%add_artist(app%earth_curve)
        call app%orbits%add_artist(app%mars_curve)

        ! the segments (the iteration shown) and the previous iteration's, faint
        app%segments_frame = DrawableTrajectory('Segments')
        app%previous_frame = DrawableTrajectory('Previous iteration')
        call app%segments_frame%show_decorations(.false.)
        call app%previous_frame%show_decorations(.false.)
        do k = 1, max_segments
            app%seg(k) = Trajectory()
            app%prev(k) = Trajectory()
            call app%seg(k)%reserve(n_samples)
            call app%prev(k)%reserve(n_samples)
            app%seg_curve(k) = CurveArtist(app%seg(k), color=palette(:, mod(k - 1, 8) + 1), width=2.5_wp)
            app%prev_curve(k) = CurveArtist(app%prev(k), color=ghost_color, width=1.0_wp)
            call app%prev_curve(k)%set_pickable(.false.)
            call app%segments_frame%add_artist(app%seg_curve(k))
            call app%previous_frame%add_artist(app%prev_curve(k))
        end do

        ! the nodes (white dots) and the gaps (red, from each segment's end to the next node, or Mars)
        app%nodes = Trajectory()
        app%node_dots = MarkerArtist(app%nodes, markers=ior(ior(MARKER_START, MARKER_INTERMEDIATE), MARKER_END), &
                                     size=8.0_wp, color=[1.0_wp, 1.0_wp, 1.0_wp])
        call app%node_dots%set_pickable(.false.)
        app%nodes_frame = DrawableTrajectory('Nodes')
        call app%nodes_frame%show_decorations(.false.)
        call app%nodes_frame%add_artist(app%node_dots)
        app%gaps = Trajectory()
        call app%gaps%set_num_optionals(1)
        app%gap_lines = SegmentArtist(app%gaps, color=gap_color, width=2.0_wp)
        if (.not. (app%gap_lines%set_start_data(position_sources(3)) .and. &
                   app%gap_lines%set_end_data(optional_sources(1)))) error stop 'shooting demo: the gaps'
        call app%gap_lines%set_pickable(.false.)
        app%gaps_frame = DrawableTrajectory('Gaps')
        call app%gaps_frame%show_decorations(.false.)
        call app%gaps_frame%add_artist(app%gap_lines)

        if (.not. (app%root%add_child(app%sun) .and. app%root%add_child(app%earth) .and. &
                   app%root%add_child(app%mars) .and. app%root%add_child(app%orbits) .and. &
                   app%root%add_child(app%previous_frame) .and. app%root%add_child(app%segments_frame) .and. &
                   app%root%add_child(app%gaps_frame) .and. app%root%add_child(app%nodes_frame))) &
            error stop 'shooting demo: could not build the scene'
        app%scene = FrameManager(app%root)

        app%sky = SkySphere()
        if (.not. app%sky%set_star_data('Stars_HYGv3.txt', min_mag=-2.0_wp, max_mag=6.0_wp, max_stars=20000)) &
            write(*, '(a)') 'shooting demo: no star catalog'
    end subroutine build_model

    !> Solve again with the dock's options (the iteration shown: the first).
    subroutine resolve()
        call solve(app%segments%value(), real(app%step%value(), wp), real(app%noise%value(), wp), &
                   app%seed%value(), app%sol)
        app%selected = min(app%selected, app%sol%n)
        call fill_iteration_table()
        app%updating = .true.
        call app%iteration_slider%setRange(0, app%sol%count - 1)
        call app%iteration_slider%setValue(0)
        app%updating = .false.
        call show_iteration(1)
    end subroutine resolve

    !> Show iteration i (1: the initial guess): its segments, nodes and gaps,
    !> the previous iteration, the segment table and the readouts.
    subroutine show_iteration(i)
        integer, intent(in) :: i
        integer :: k, j
        real(wp) :: t0, t1, x(6)
        app%shown = max(1, min(app%sol%count, i))
        associate (it => app%sol%iterations(app%shown))
            do k = 1, max_segments
                call app%seg(k)%clear()
                call app%prev(k)%clear()
                if (k > it%n) cycle
                call segment_times(it%n, k, t0, t1)
                do j = 1, n_samples
                    call app%seg(k)%add(t0 + (t1 - t0) * (j - 1) / (n_samples - 1), pos=it%arcs(:, j, k))
                    if (app%shown > 1) call app%prev(k)%add(t0 + (t1 - t0) * (j - 1) / (n_samples - 1), &
                                                            pos=app%sol%iterations(app%shown - 1)%arcs(:, j, k))
                end do
            end do
            call app%nodes%clear()
            call app%gaps%clear()
            x = mars_state(tof)
            do k = 1, it%n
                call segment_times(it%n, k, t0, t1)
                call app%nodes%add(t0, pos=it%nodes(1:3, k))
                call app%gaps%add(t1, pos=it%ends(1:3, k))
                if (k < it%n) then
                    call app%gaps%set_optional(1, it%nodes(1:3, k + 1))
                else
                    call app%gaps%set_optional(1, x(1:3))
                end if
            end do
        end associate
        call fill_segment_table()
        call update_readouts()
        call highlight_selected()
    end subroutine show_iteration

    !> Arrows along the selected segment; the others plain.
    subroutine highlight_selected()
        integer :: k
        do k = 1, max_segments
            if (k == app%selected) then
                call app%seg_curve(k)%set_highlight(HIGHLIGHT_ARROWS, color=[1.0_wp, 1.0_wp, 1.0_wp, 0.9_wp])
            else
                call app%seg_curve(k)%set_highlight(HIGHLIGHT_NONE)
            end if
        end do
    end subroutine highlight_selected

    !> The HUD lines and the dock's status.
    subroutine update_readouts()
        character(len=160) :: line
        character(len=:), allocatable :: state
        associate (it => app%sol%iterations(app%shown))
            if (app%shown == 1) then
                state = 'the initial guess'
            else
                write(line, '(a,f4.2)') 'step ', it%step
                state = trim(line)
            end if
            write(line, '(a,i0,a,i0,a)') 'Iteration ', app%shown - 1, ' of ', app%sol%count - 1, ' ('//state//'): '
            line = trim(line)//' largest gap '//sci(it%max_position * au_km)//' km, '// &
                   sci(it%max_velocity * au_km / day_s * 1000.0_wp)//' m/s'
            call app%hud_iteration%set_text(trim(line))
            call app%iteration_label%setText(trim(line))
            write(line, '(a,f0.3,a,f0.3,a)') 'Departure ', it%v_departure * au_km / day_s, &
                ' km/s, arrival ', it%v_arrival * au_km / day_s, ' km/s (relative to the planets)'
            if (app%shown == app%sol%count) then
                if (app%sol%converged) then
                    line = trim(line)//': converged'
                else
                    line = trim(line)//': not converged'
                end if
            end if
            call app%hud_speeds%set_text(trim(line))
            call app%status_label%setText(trim(line))
        end associate
    end subroutine update_readouts

    !> A number in scientific notation (3 significant digits).
    function sci(x) result(text)
        real(wp), intent(in) :: x
        character(len=:), allocatable :: text
        character(len=16) :: buf
        write(buf, '(es10.2)') x
        text = trim(adjustl(buf))
    end function sci

    !> The segment table: the iteration shown's segments.
    subroutine fill_segment_table()
        character(len=32) :: text
        real(wp) :: t0, t1
        integer :: k
        associate (it => app%sol%iterations(app%shown))
            app%updating = .true.
            call app%segment_table%setRowCount(it%n)
            do k = 1, it%n
                call segment_times(it%n, k, t0, t1)
                write(text, '(i0)') k
                call set_cell(app%segment_table, k - 1, 0, trim(text), palette(:, mod(k - 1, 8) + 1))
                write(text, '(f0.1,a,f0.1)') t0, ' - ', t1
                call set_cell(app%segment_table, k - 1, 1, trim(text))
                if (k < it%n) then
                    call set_cell(app%segment_table, k - 1, 2, sci(it%gap_position(k) * au_km))
                    call set_cell(app%segment_table, k - 1, 3, sci(it%gap_velocity(k) * au_km / day_s * 1000.0_wp))
                else
                    call set_cell(app%segment_table, k - 1, 2, sci(it%gap_position(k) * au_km)//' (Mars)')
                    call set_cell(app%segment_table, k - 1, 3, '-')
                end if
            end do
            if (app%selected > 0) call app%segment_table%selectRow(app%selected - 1)
            app%updating = .false.
        end associate
    end subroutine fill_segment_table

    !> The iteration table: every recorded iteration.
    subroutine fill_iteration_table()
        character(len=32) :: text
        integer :: i
        app%updating = .true.
        call app%iteration_table%setRowCount(app%sol%count)
        do i = 1, app%sol%count
            associate (it => app%sol%iterations(i))
                write(text, '(i0)') i - 1
                if (i == 1) text = '0 (guess)'
                call set_cell(app%iteration_table, i - 1, 0, trim(text))
                call set_cell(app%iteration_table, i - 1, 1, sci(it%max_position * au_km))
                call set_cell(app%iteration_table, i - 1, 2, sci(it%max_velocity * au_km / day_s * 1000.0_wp))
                write(text, '(f4.2)') it%step
                if (i == 1) text = '-'
                call set_cell(app%iteration_table, i - 1, 3, trim(text))
            end associate
        end do
        app%updating = .false.
    end subroutine fill_iteration_table

    !> Set a table cell's text (not editable), optionally in a color.
    subroutine set_cell(table, row, column, text, color)
        type(QTableWidget), intent(in) :: table
        integer, intent(in) :: row, column
        character(len=*), intent(in) :: text
        real(wp), intent(in), optional :: color(3)
        type(QTableWidgetItem) :: item
        item = QTableWidgetItem(text)
        if (present(color)) call item%setForeground(QColor(nint(255 * color(1)), nint(255 * color(2)), &
                                                           nint(255 * color(3))))
        call table%setItem(row, column, item)
    end subroutine set_cell

    !> Select segment k (0: none): arrows along it, its row.
    subroutine select_segment(k)
        integer, intent(in) :: k
        app%selected = max(0, min(app%sol%n, k))
        call highlight_selected()
        app%updating = .true.
        if (app%selected > 0) then
            call app%segment_table%selectRow(app%selected - 1)
        else
            call app%segment_table%clearSelection()
        end if
        app%updating = .false.
    end subroutine select_segment

    ! ==================================================================
    ! clicks

    subroutine shooting_press(self, window, x, y, button)
        class(ShootingHandler), intent(inout) :: self
        class(WindowProxy), intent(inout) :: window
        real(wp), intent(in) :: x, y
        integer, intent(in) :: button
        if (button /= MOUSE_LEFT) return
        self%pressed = [x, y]
        self%down = .true.
    end subroutine shooting_press

    !> A click (released where pressed): on a segment, select it; elsewhere, deselect.
    subroutine shooting_release(self, window, x, y, button)
        class(ShootingHandler), intent(inout) :: self
        class(WindowProxy), intent(inout) :: window
        real(wp), intent(in) :: x, y
        integer, intent(in) :: button
        type(PickHit), allocatable :: hits(:)
        integer :: k
        if (button /= MOUSE_LEFT .or. .not. self%down) return
        self%down = .false.
        if (norm2([x, y] - self%pressed) > 0.01_wp) return
        call window%pick(x, y, hits, kinds=[PICK_CURVE], nearest=.true.)
        if (size(hits) > 0) then
            do k = 1, app%sol%n
                if (associated(hits(1)%trajectory, app%seg(k))) then
                    call select_segment(k)
                    return
                end if
            end do
        end if
        call select_segment(0)
    end subroutine shooting_release

    ! ==================================================================
    ! the window

    subroutine build_window()
        app%views(1) = View(app%root, app%root)
        call app%views(1)%set_default_view_parameters(eye=[0.25_wp, 0.4_wp, 4.6_wp], center=[0.25_wp, 0.4_wp, 0.0_wp], &
                                                      up=[0.0_wp, 1.0_wp, 0.0_wp])
        app%views(2) = View(app%root, app%root)
        call app%views(2)%set_default_view_parameters(eye=[1.6_wp, -2.9_wp, 1.9_wp], center=[0.0_wp, 0.35_wp, 0.0_wp], &
                                                      up=[0.0_wp, 0.0_wp, 1.0_wp])
        app%hud_iteration = HUDText('', position=[0.01_wp, 0.99_wp], alignment=ALIGN_LEFT_TOP, size=16.0_wp, &
                                    color=[0.9_wp, 0.9_wp, 0.9_wp])
        app%hud_speeds = HUDText('', position=[0.01_wp, 0.01_wp], alignment=ALIGN_LEFT_BOTTOM, size=16.0_wp, &
                                 color=[1.0_wp, 0.85_wp, 0.35_wp])

        app%window = QWindow()
        app%container = QWidget_createWindowContainer(app%window)
        call app%container%setMinimumSize(320, 240)
        app%events = QtfEventFilter(app%window)
        call app%events%exposeEvent%connect(on_expose)
        call app%events%updateRequest%connect(on_update)
        call app%events%resizeEvent%connect(on_resize)
        call app%events%mousePressEvent%connect(on_press)
        call app%events%mouseReleaseEvent%connect(on_release)
        call app%events%mouseMoveEvent%connect(on_move)
        call app%events%wheelEvent%connect(on_wheel)
        call app%events%keyPressEvent%connect(on_key_press)
        call app%events%keyReleaseEvent%connect(on_key_release)
    end subroutine build_window

    !> Open the view (on the window's first expose) and show the scene.
    subroutine open_view()
        integer :: k
        if (.not. app%proxy%open(app%window%width(), app%window%height(), 'vkframes shooting', &
                                 native_window=app%window%winId(), pixel_ratio=app%window%devicePixelRatio())) then
            write(*, '(a)') 'shooting demo: could not open the view'
            return
        end if
        call app%proxy%set_scene(app%scene)
        call app%proxy%set_event_handler(handler)
        call app%proxy%set_background_color([0.0_wp, 0.0_wp, 0.0_wp])
        call app%proxy%set_sky(app%sky)
        do k = 1, n_views
            call app%proxy%add_view(app%views(k))
        end do
        call app%proxy%select_view(app%view_choice%currentIndex() + 1)
        call app%proxy%add_hud_text(app%hud_iteration)
        call app%proxy%add_hud_text(app%hud_speeds)
        call app%proxy%pause_time(.true.)                        ! (the iterations, not time, are played)
    end subroutine open_view

    subroutine on_expose(data)
        class(*), intent(inout), optional :: data
        if (app%proxy%is_open() .or. .not. app%window%isExposed()) return
        call open_view()
        if (app%proxy%is_open()) call app%window%requestUpdate()
    end subroutine on_expose

    !> Render a frame, ask for the next one.
    subroutine on_update(data)
        class(*), intent(inout), optional :: data
        if (app%window%devicePixelRatio() /= app%proxy%pixel_ratio()) &
            call app%proxy%resize(nint(app%window%width() * app%window%devicePixelRatio()), &
                                  nint(app%window%height() * app%window%devicePixelRatio()), &
                                  pixel_ratio=app%window%devicePixelRatio())
        if (.not. app%proxy%frame()) return
        app%frame = app%frame + 1
        if (allocated(app%capture) .and. app%frame == app%capture_frame) then
            if (app%proxy%capture(app%capture)) write(*, '(a)') 'captured '//app%capture
        end if
        call app%window%requestUpdate()
    end subroutine on_update

    subroutine on_resize(event, data)
        type(QResizeEvent), intent(in) :: event
        class(*), intent(inout), optional :: data
        real(wp) :: dpr
        if (.not. app%proxy%is_open()) return
        dpr = app%window%devicePixelRatio()
        call app%proxy%resize(nint(event%width() * dpr), nint(event%height() * dpr), pixel_ratio=dpr)
        call app%window%requestUpdate()
    end subroutine on_resize

    ! ------------------------------------------------------------------
    ! mouse and keys, forwarded to the trackball (VSG's conventions)

    integer function vsg_button(qt_button)
        integer, intent(in) :: qt_button
        select case (qt_button)
        case (Qt_LeftButton)
            vsg_button = 1
        case (Qt_MiddleButton)
            vsg_button = 2
        case (Qt_RightButton)
            vsg_button = 3
        case default
            vsg_button = 0
        end select
    end function vsg_button

    integer function vsg_mask(qt_buttons)
        integer, intent(in) :: qt_buttons
        vsg_mask = 0
        if (iand(qt_buttons, Qt_LeftButton) /= 0) vsg_mask = ior(vsg_mask, 256)
        if (iand(qt_buttons, Qt_MiddleButton) /= 0) vsg_mask = ior(vsg_mask, 512)
        if (iand(qt_buttons, Qt_RightButton) /= 0) vsg_mask = ior(vsg_mask, 1024)
    end function vsg_mask

    subroutine forward_mouse(kind, event)
        integer, intent(in) :: kind
        type(QMouseEvent), intent(in) :: event
        real(wp) :: dpr
        dpr = app%window%devicePixelRatio()
        call app%proxy%mouse(kind, nint(event%x() * dpr), nint(event%y() * dpr), vsg_mask(event%buttons()), &
                             vsg_button(event%button()))
        call event%accept()
    end subroutine forward_mouse

    subroutine on_move(event, data)
        type(QMouseEvent), intent(in) :: event
        class(*), intent(inout), optional :: data
        call forward_mouse(MOUSE_MOVE, event)
    end subroutine on_move

    subroutine on_press(event, data)
        type(QMouseEvent), intent(in) :: event
        class(*), intent(inout), optional :: data
        call forward_mouse(MOUSE_PRESS, event)
    end subroutine on_press

    subroutine on_release(event, data)
        type(QMouseEvent), intent(in) :: event
        class(*), intent(inout), optional :: data
        call forward_mouse(MOUSE_RELEASE, event)
    end subroutine on_release

    subroutine on_wheel(event, data)
        type(QWheelEvent), intent(in) :: event
        class(*), intent(inout), optional :: data
        if (event%angleDeltaY() == 0) return
        call app%proxy%scroll(event%angleDeltaY() / 120.0_wp)     ! (wheel notches: a trackpad's are fractions)
        call event%accept()
    end subroutine on_wheel

    !> VSG key symbol for a Qt key (ASCII, or the X11-style codes VSG's trackball uses; 0 if unmapped).
    subroutine vsg_key(event, symbol, modified_symbol, modifier)
        type(QKeyEvent), intent(in) :: event
        integer, intent(out) :: symbol, modified_symbol, modifier
        character(len=:), allocatable :: text
        integer :: key, mods
        key = event%key()
        mods = event%modifiers()
        modifier = 0
        if (iand(mods, Qt_ShiftModifier) /= 0) modifier = ior(modifier, 1)
        if (iand(mods, Qt_ControlModifier) /= 0) modifier = ior(modifier, 4)
        if (iand(mods, Qt_AltModifier) /= 0) modifier = ior(modifier, 8)
        if (iand(mods, Qt_MetaModifier) /= 0) modifier = ior(modifier, 128)
        select case (key)
        case (Qt_Key_Left)
            symbol = int(z'FF51')
        case (Qt_Key_Up)
            symbol = int(z'FF52')
        case (Qt_Key_Right)
            symbol = int(z'FF53')
        case (Qt_Key_Down)
            symbol = int(z'FF54')
        case (Qt_Key_Home)
            symbol = int(z'FF50')
        case (Qt_Key_Escape)
            symbol = int(z'FF1B')
        case (Qt_Key_Return)
            symbol = int(z'FF0D')
        case (Qt_Key_Space)
            symbol = 32
        case (Qt_Key_A:Qt_Key_Z)
            symbol = key - Qt_Key_A + iachar('a')
        case default
            symbol = 0
            text = event%text()
            if (len(text) == 1) then
                if (iachar(text) >= 32 .and. iachar(text) < 127) symbol = iachar(text)
            end if
        end select
        modified_symbol = symbol
        if (key >= Qt_Key_A .and. key <= Qt_Key_Z .and. iand(modifier, 1) /= 0) modified_symbol = key
    end subroutine vsg_key

    subroutine forward_key(pressed, event)
        logical, intent(in) :: pressed
        type(QKeyEvent), intent(in) :: event
        integer :: symbol, modified_symbol, modifier
        call vsg_key(event, symbol, modified_symbol, modifier)
        if (symbol == 0) return
        call app%proxy%key(pressed, symbol, modified_symbol, modifier)
        call event%accept()
    end subroutine forward_key

    subroutine on_key_press(event, data)
        type(QKeyEvent), intent(in) :: event
        class(*), intent(inout), optional :: data
        call forward_key(.true., event)
    end subroutine on_key_press

    subroutine on_key_release(event, data)
        type(QKeyEvent), intent(in) :: event
        class(*), intent(inout), optional :: data
        call forward_key(.false., event)
    end subroutine on_key_release

    ! ==================================================================
    ! controls

    subroutine on_iteration(value, data)
        integer, intent(in) :: value
        class(*), intent(inout), optional :: data
        if (app%updating) return
        call show_iteration(value + 1)
    end subroutine on_iteration

    !> Play from the start (or where the slider is) or pause.
    subroutine on_play(data)
        class(*), intent(inout), optional :: data
        if (app%timer%isActive()) then
            call app%timer%stop()
            call app%play%setText('Play')
        else
            if (app%iteration_slider%value() >= app%iteration_slider%maximum()) call app%iteration_slider%setValue(0)
            call app%timer%start(nint(1000.0_wp / app%speed%value()))
            call app%play%setText('Pause')
        end if
    end subroutine on_play

    !> The next iteration (playing); at the last, stop.
    subroutine on_tick(data)
        class(*), intent(inout), optional :: data
        if (app%iteration_slider%value() >= app%iteration_slider%maximum()) then
            call app%timer%stop()
            call app%play%setText('Play')
            return
        end if
        call app%iteration_slider%setValue(app%iteration_slider%value() + 1)
    end subroutine on_tick

    subroutine on_speed(value, data)
        integer, intent(in) :: value
        class(*), intent(inout), optional :: data
        if (app%timer%isActive()) call app%timer%start(nint(1000.0_wp / max(1, value)))
    end subroutine on_speed

    subroutine on_option_int(value, data)
        integer, intent(in) :: value
        class(*), intent(inout), optional :: data
        if (app%updating) return
        call resolve()
    end subroutine on_option_int

    subroutine on_option_real(value, data)
        real(c_double), intent(in) :: value
        class(*), intent(inout), optional :: data
        if (app%updating) return
        call resolve()
    end subroutine on_option_real

    subroutine on_show_previous(checked, data)
        logical, intent(in) :: checked
        class(*), intent(inout), optional :: data
        call app%previous_frame%show_contents(checked)
    end subroutine on_show_previous

    subroutine on_show_nodes(checked, data)
        logical, intent(in) :: checked
        class(*), intent(inout), optional :: data
        call app%nodes_frame%show_contents(checked)
    end subroutine on_show_nodes

    subroutine on_show_gaps(checked, data)
        logical, intent(in) :: checked
        class(*), intent(inout), optional :: data
        call app%gaps_frame%show_contents(checked)
    end subroutine on_show_gaps

    subroutine on_show_orbits(checked, data)
        logical, intent(in) :: checked
        class(*), intent(inout), optional :: data
        call app%orbits%show_contents(checked)
    end subroutine on_show_orbits

    subroutine on_view(index, data)
        integer, intent(in) :: index
        class(*), intent(inout), optional :: data
        if (app%proxy%is_open()) call app%proxy%select_view(index + 1)
    end subroutine on_view

    subroutine on_reset_view(data)
        class(*), intent(inout), optional :: data
        type(View), pointer :: v
        v => app%proxy%current_view()
        call v%reset_view()
    end subroutine on_reset_view

    !> A segment's row clicked: select it.
    subroutine on_segment_row(row, column, data)
        integer, intent(in) :: row, column
        class(*), intent(inout), optional :: data
        if (app%updating) return
        call select_segment(row + 1)
    end subroutine on_segment_row

    !> An iteration's row clicked: show it.
    subroutine on_iteration_row(row, column, data)
        integer, intent(in) :: row, column
        class(*), intent(inout), optional :: data
        if (app%updating) return
        call app%iteration_slider%setValue(row)
    end subroutine on_iteration_row

    subroutine on_quit(data)
        class(*), intent(inout), optional :: data
        call app%main%close()
    end subroutine on_quit

    !> The main window closes: close the view (waiting for the GPU) first.
    subroutine on_close(event, data)
        type(QCloseEvent), intent(in) :: event
        class(*), intent(inout), optional :: data
        call app%timer%stop()
        call app%proxy%close()
    end subroutine on_close

    !> A table: columns with headers, rows selected whole, not editable.
    function new_table(headers) result(table)
        character(len=*), intent(in) :: headers(:)
        type(QTableWidget) :: table
        table = QTableWidget(0, size(headers))
        call table%setHorizontalHeaderLabels(headers)
        call table%setVerticalHeaderVisible(.false.)
        call table%setSelectionBehavior(QAbstractItemView_SelectRows)
        call table%setSelectionMode(QAbstractItemView_SingleSelection)
        call table%setEditTriggers(QAbstractItemView_NoEditTriggers)
        call table%setStretchLastSection(.true.)
        call table%setMinimumSize(380, 150)
    end function new_table

    subroutine build_ui()
        type(QDockWidget) :: dock
        type(QWidget) :: panel
        type(QFormLayout) :: form
        type(QPushButton) :: reset_view
        type(QHBoxLayout) :: play_row, view_row

        app%main = QMainWindow()
        call app%main%setWindowTitle('vkframes: a trajectory optimizer iterating (multiple shooting, Earth to Mars)')
        call app%main%closeEvent%connect(on_close)
        call build_window()
        call app%main%setCentralWidget(app%container)

        panel = QWidget()
        form = QFormLayout(panel)

        ! the iterations
        app%iteration_slider = QSlider(Qt_Horizontal)
        call app%iteration_slider%valueChanged%connect(on_iteration)
        call form%addRow('Iteration', app%iteration_slider)
        app%iteration_label = QLabel('')
        call app%iteration_label%setWordWrap(.true.)
        call form%addRow(app%iteration_label)
        app%play = QPushButton('Play')
        call app%play%clicked%connect(on_play)
        app%speed = QSpinBox()
        call app%speed%setRange(1, 20)
        call app%speed%setValue(3)
        call app%speed%setSuffix(' iterations/s')
        call app%speed%valueChanged%connect(on_speed)
        play_row = QHBoxLayout()
        call play_row%addWidget(app%play)
        call play_row%addWidget(app%speed)
        call form%addRow('', play_row)
        app%timer = QTimer()
        call app%timer%timeout%connect(on_tick)

        ! the solver
        app%segments = QSpinBox()
        call app%segments%setRange(2, max_segments)
        call app%segments%setValue(app%start_segments)
        call app%segments%valueChanged%connect(on_option_int)
        call form%addRow('Segments', app%segments)
        app%step = QDoubleSpinBox()
        call app%step%setRange(0.01_c_double, 1.0_c_double)
        call app%step%setSingleStep(0.05_c_double)
        call app%step%setValue(real(app%start_step, c_double))
        call app%step%valueChanged%connect(on_option_real)
        call form%addRow('Step fraction (far)', app%step)
        app%noise = QDoubleSpinBox()
        call app%noise%setRange(0.0_c_double, 1.0_c_double)
        call app%noise%setSingleStep(0.1_c_double)
        call app%noise%setValue(real(app%start_noise, c_double))
        call app%noise%valueChanged%connect(on_option_real)
        call form%addRow('Initial guess noise', app%noise)
        app%seed = QSpinBox()
        call app%seed%setRange(1, 999)
        call app%seed%setValue(app%start_seed)
        call app%seed%valueChanged%connect(on_option_int)
        call form%addRow('Noise seed', app%seed)
        app%status_label = QLabel('')
        call app%status_label%setWordWrap(.true.)
        call form%addRow(app%status_label)

        ! what is shown, the view
        app%show_previous = QCheckBox('Previous iteration (faint)')
        call app%show_previous%setChecked(.true.)
        call app%show_previous%toggled%connect(on_show_previous)
        call form%addRow('Show', app%show_previous)
        app%show_nodes = QCheckBox('Nodes')
        call app%show_nodes%setChecked(.true.)
        call app%show_nodes%toggled%connect(on_show_nodes)
        call form%addRow('', app%show_nodes)
        app%show_gaps = QCheckBox('Gaps (red)')
        call app%show_gaps%setChecked(.true.)
        call app%show_gaps%toggled%connect(on_show_gaps)
        call form%addRow('', app%show_gaps)
        app%show_orbits = QCheckBox('Planet orbits')
        call app%show_orbits%setChecked(.true.)
        call app%show_orbits%toggled%connect(on_show_orbits)
        call form%addRow('', app%show_orbits)
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

        ! the tables
        app%segment_table = new_table(segment_headers)
        call app%segment_table%cellClicked%connect(on_segment_row)
        call form%addRow(QLabel('Segments of the iteration shown (click: select)'))
        call form%addRow(app%segment_table)
        app%iteration_table = new_table(iteration_headers)
        call app%iteration_table%cellClicked%connect(on_iteration_row)
        call form%addRow(QLabel('Iterations (click: show)'))
        call form%addRow(app%iteration_table)

        call form%addRow(QLabel('<small>Click a segment to select it. Left drag: rotate; right drag: zoom; ' // &
                                'middle drag: pan; space: reset the view</small>'))
        dock = QDockWidget('Solver', app%main)
        call dock%setWidget(panel)
        call app%main%addDockWidget(Qt_LeftDockWidgetArea, dock)
    end subroutine build_ui

end module shooting_demo

!> The multiple-shooting demo (see the module documentation).
program vkframes_shooting_demo

    use qt
    use shooting_demo
    implicit none

    type(QApplication) :: qapp
    character(len=1024) :: arg
    integer :: i, ierr

    qapp = QApplication()
    i = 1
    do while (i <= command_argument_count())
        call get_command_argument(i, arg)
        select case (arg)
        case ('--segments')
            call get_command_argument(i + 1, arg)
            read(arg, *) app%start_segments
            i = i + 1
        case ('--step')
            call get_command_argument(i + 1, arg)
            read(arg, *) app%start_step
            i = i + 1
        case ('--noise')
            call get_command_argument(i + 1, arg)
            read(arg, *) app%start_noise
            i = i + 1
        case ('--seed')
            call get_command_argument(i + 1, arg)
            read(arg, *) app%start_seed
            i = i + 1
        case ('--iteration')
            call get_command_argument(i + 1, arg)
            read(arg, *) app%start_iteration
            i = i + 1
        case ('--select')
            call get_command_argument(i + 1, arg)
            read(arg, *) app%selected
            i = i + 1
        case ('--view')
            call get_command_argument(i + 1, arg)
            read(arg, *) app%start_view
            app%start_view = max(1, min(n_views, app%start_view))
            i = i + 1
        case ('--play')
            app%start_play = .true.
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
        end select
        i = i + 1
    end do

    call build_model()
    call build_ui()
    call resolve()
    if (app%start_iteration < 0) app%start_iteration = app%sol%count - 1
    call app%iteration_slider%setValue(app%start_iteration)
    call select_segment(app%selected)
    if (app%start_play) call on_play()
    if (app%quit_after > 0) call QTimer_singleShot(int(1000 * app%quit_after), on_quit)
    call app%main%resize(1400, 860)
    call app%main%show()
    ierr = qapp%exec()

end program vkframes_shooting_demo
