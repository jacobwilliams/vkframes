!> Rendezvous and proximity operations (RPO) in a qt-fortran GUI: a chaser
!> approaching a target in a circular orbit, in the target's LVLH frame (the
!> target at the origin), under the Clohessy-Wiltshire equations (app/cw.f90:
!> closed form, so a plan is propagated exactly and at once). The scene's
!> axes: x along-track (V-bar), y cross-track, z radial (up, away from the
!> Earth); metres.
!>
!> * The plan: the chaser's initial state and impulsive burns. Its path is
!>   colored by safety: outside the keep-out sphere (cyan), inside it within
!>   the approach corridor (yellow, allowed), inside it outside the corridor
!>   (red: a violation). The keep-out sphere is translucent red (a Sphere
!>   with `set_opacity`: lit, the path and the target seen through it), the
!>   corridor a translucent cone along -V-bar.
!> * Burns: markers on the path with their Δv drawn as vectors (a
!>   SegmentArtist with relative ends, `Δv arrow scale` metres per m/s). A
!>   click on the path (not a drag) adds a burn there; a click on a burn
!>   selects it: a GUI panel next to it (vkf_gui, kept at the burn's place on
!>   the screen in the window handler's before_draw) sets its Δv (radial,
!>   in-track, cross-track) and time, or deletes it; the path is propagated
!>   again as the sliders move.
!> * The dock: presets (radial hops along V-bar, a natural-motion
!>   "football", a drift from below, a start 2 km behind, a football too
!>   close), the target's altitude, the duration, the initial state, the
!>   keep-out radius and corridor angle, the arrows' scale; the burns and the
!>   readout (total Δv, closest approach, keep-out violations); animation (a
!>   chaser marker flying the path, traced), the views (the whole approach;
!>   following the chaser), the zoom speed, video recording.
!>
!> Run with `pixi run -e qt rpo-demo` (from the vkframes root). Options:
!> `--preset k` (1 to 5) starts with preset k; `--select k` selects burn k;
!> `--capture file` saves frame `--capture-frame n` (default 120);
!> `--click t` clicks the path at time t (s) after 30 frames, as the user
!> would; `--quit-after seconds` closes the demo.
module rpo_demo

    use qt
    use vkframes
    use vkframes_qt, only: connect_view, ViewLink
    use vkf_text, only: get_font
    use cw, only: Burn, propagate_plan, radial_hops, sort_burns
    use iso_c_binding

    implicit none

    real(wp), parameter :: mu_earth = 3.986004418e14_wp, r_earth = 6378137.0_wp     !! m^3/s^2, m
    real(wp), parameter :: deg = pi / 180.0_wp
    integer, parameter :: max_burns = 32
    real(wp), parameter :: safe_color(3) = [0.35_wp, 0.85_wp, 1.0_wp], corridor_color(3) = [1.0_wp, 0.85_wp, 0.2_wp]
    real(wp), parameter :: violation_color(3) = [1.0_wp, 0.2_wp, 0.2_wp], burn_color(3) = [1.0_wp, 0.55_wp, 0.15_wp]
    character(len=*), parameter :: preset_names(5) = [character(len=40) :: 'Radial hops along V-bar', &
        'Football (natural motion circumnavigation)', 'Drift from below (R-bar)', 'Start 2 km behind, no burns', &
        'Football too close (violates keep-out)']

    !> Picks on a click (a press and release in one place), and keeps the
    !> selected burn's panel next to it.
    type, extends(WindowEventHandler) :: Planner
        real(wp) :: pressed(2) = 0.0_wp
        logical :: down = .false.
    contains
        procedure :: button_press => planner_press
        procedure :: button_release => planner_release
        procedure :: before_draw => planner_before_draw
    end type Planner

    !> The GUI, the scene and the plan.
    type :: app_t
        type(QMainWindow) :: main
        type(QStackedWidget) :: stack
        type(QLabel) :: startup_message
        type(QWindow) :: window
        type(QWidget) :: container
        type(QtfEventFilter) :: events
        type(ViewLink) :: view_link                         !! the window's events to the proxy (connect_view)
        type(WindowProxy) :: proxy
        type(Planner) :: planner
        integer :: frame = 0
        ! controls
        type(QComboBox) :: preset, view_choice
        type(QDoubleSpinBox) :: altitude, orbits, state(6), keep_out, corridor, arrow_scale, zoom_speed, keep_out_opacity
        type(QCheckBox) :: animate
        type(QPushButton) :: record
        type(QLabel) :: burn_list, readout, orbit_info
        ! the scene (LVLH, metres: x in-track, y cross-track, z radial)
        type(ReferenceFrame) :: root
        type(CoordinateAxes) :: axes
        type(Sphere) :: target
        type(Sphere) :: keep_out_zone                          !! translucent
        type(EllipticCone) :: approach
        type(Trajectory) :: path                               !! the chaser's path (times s; colors by safety)
        type(DrawableTrajectory) :: path_frame
        type(CurveArtist) :: path_curve
        type(MarkerArtist) :: path_start
        type(Trajectory) :: burn_points                        !! the burns (their positions; optional 1: Δv)
        type(SegmentArtist) :: burn_vectors
        type(MarkerArtist) :: burn_dots
        type(Trajectory) :: selection                          !! the selected burn (a pulsing ring)
        type(MarkerArtist) :: selection_ring
        type(ReferenceFrame) :: chaser                         !! flies the path (Animate)
        type(Trajectory) :: chaser_point
        type(DrawableTrajectory) :: chaser_frame
        type(MarkerArtist) :: chaser_dot
        type(FrameManager) :: scene
        type(View) :: overview, follow
        type(HUDText) :: hud_title, hud_help, hud_time
        ! the plan
        type(Burn) :: burns(max_burns)
        integer :: n_burns = 0
        integer :: selected = 0                                !! the selected burn (0: none)
        real(wp), allocatable :: burn_at(:,:)                  !! (6, n_burns): the state at each burn (LVLH)
        logical :: dirty = .false.                             !! the plan changed: propagated at the next frame
        logical :: building = .true.
        ! options
        character(len=:), allocatable :: capture
        integer :: capture_frame = 120, start_preset = 1, select_at_start = 0
        real(wp) :: click_time = -1.0_wp, quit_after = -1.0_wp
        logical :: print_plan = .false.                        !! (--click: the plan printed when propagated)
        logical :: frame_pending = .false.                     !! a preset loaded: the camera framed on its plan
    end type app_t

    type(app_t), target, save :: app

contains

    ! ==================================================================
    ! the plan

    !> The target's mean motion (rad/s) and orbital period (s).
    real(wp) function mean_motion()
        mean_motion = sqrt(mu_earth / (r_earth + 1000.0_wp * app%altitude%value())**3)
    end function mean_motion

    real(wp) function period()
        period = 2.0_wp * pi / mean_motion()
    end function period

    !> LVLH [R, I, C] to the scene's [x, y, z] = [I, C, R].
    pure function to_scene(v) result(p)
        real(wp), intent(in) :: v(3)
        real(wp) :: p(3)
        p = [v(2), v(3), v(1)]
    end function to_scene

    !> The initial state from the controls (LVLH: m, m/s).
    function initial_state() result(s0)
        real(wp) :: s0(6)
        integer :: k
        do k = 1, 6
            s0(k) = app%state(k)%value()
        end do
    end function initial_state

    !> Load preset k: the initial state, the duration and the burns.
    subroutine load_preset(k)
        integer, intent(in) :: k
        real(wp) :: s0(6), n, orbits
        type(Burn), allocatable :: b(:)
        integer :: j
        n = mean_motion()
        orbits = 1.5_wp
        allocate(b(0))
        select case (k)
        case (1)                                            ! hops: 2 km, 1 km, 300 m behind, then held
            s0 = [0.0_wp, -2000.0_wp, 0.0_wp, 0.0_wp, 0.0_wp, 0.0_wp]
            b = radial_hops(n, -2000.0_wp, [-1000.0_wp, -300.0_wp])
            orbits = 1.25_wp
        case (2)                                            ! a 2:1 ellipse about the target (no drift: vI = -2 n R)
            s0 = [300.0_wp, 0.0_wp, 0.0_wp, 0.0_wp, -2.0_wp * n * 300.0_wp, 0.0_wp]
            orbits = 2.0_wp
        case (3)                                            ! 1 km below, at rest: drifts ahead
            s0 = [-1000.0_wp, 0.0_wp, 0.0_wp, 0.0_wp, 0.0_wp, 0.0_wp]
            orbits = 1.0_wp
        case (4)
            s0 = [0.0_wp, -2000.0_wp, 0.0_wp, 0.0_wp, 0.0_wp, 0.0_wp]
            orbits = 1.0_wp
        case default                                        ! a football inside the keep-out sphere
            s0 = [150.0_wp, 0.0_wp, 40.0_wp, 0.0_wp, -2.0_wp * n * 150.0_wp, 0.0_wp]
            orbits = 1.5_wp
        end select
        app%building = .true.
        do j = 1, 6
            call app%state(j)%setValue(real(s0(j), c_double))
        end do
        call app%orbits%setValue(real(orbits, c_double))
        app%building = .false.
        app%n_burns = size(b)
        app%burns(1:size(b)) = b
        app%selected = 0
        app%dirty = .true.
        app%frame_pending = .true.
    end subroutine load_preset

    !> The camera on the plan: its path's (and the target's) bounding box seen
    !> from the cross-track side, slightly above (the R-V plane), as a flight;
    !> the view's reset distance to match.
    subroutine frame_plan()
        real(wp) :: lo(3), hi(3), p(3), center(3), extent
        integer :: k
        app%frame_pending = .false.
        if (.not. app%proxy%is_open() .or. app%path%num_times() == 0) return
        lo = 0.0_wp
        hi = 0.0_wp
        do k = 1, app%path%num_times()
            p = app%path%position(k)
            lo = min(lo, p)
            hi = max(hi, p)
        end do
        center = 0.5_wp * (lo + hi)
        extent = max(300.0_wp, 0.5_wp * maxval(hi - lo))
        call app%overview%set_default_view_distance(3.6_wp * extent)
        call app%proxy%set_camera(center + extent * [0.0_wp, -3.3_wp, 1.2_wp], center, [0.0_wp, 0.0_wp, 1.0_wp], &
                                  duration=merge(0.0_wp, 1.0_wp, app%frame == 0), in_root=.true.)
    end subroutine frame_plan

    !> Propagate the plan and draw it: the path (colored by safety), the burns,
    !> the selection; the readout and the burn list; the clock over the path.
    subroutine update_plan()
        real(wp), allocatable :: t(:), s(:,:), rgba(:,:)
        real(wp) :: n, t_end, r, kos, half, dv_total, closest, t_closest, p(3)
        integer :: k, worst
        logical :: violation
        real(wp) :: t_violation
        character(len=2000) :: text
        character(len=200) :: line
        app%dirty = .false.
        n = mean_motion()
        t_end = app%orbits%value() * period()
        call sort_burns(app%burns(1:app%n_burns))
        if (allocated(app%burn_at)) deallocate(app%burn_at)
        allocate(app%burn_at(6, app%n_burns))
        call propagate_plan(n, initial_state(), app%burns(1:app%n_burns), t_end, period() / 720.0_wp, t, s, app%burn_at)
        kos = app%keep_out%value()
        half = app%corridor%value() * deg
        allocate(rgba(4, size(t)))
        closest = huge(1.0_wp)
        t_closest = 0.0_wp
        violation = .false.
        t_violation = 0.0_wp
        worst = 0
        do k = 1, size(t)
            p = to_scene(s(1:3, k))
            r = norm2(p)
            if (r < closest) then
                closest = r
                t_closest = t(k)
            end if
            if (r >= kos) then
                rgba(:, k) = [safe_color, 1.0_wp]
            else if (r > 0.0_wp .and. -p(1) >= r * cos(half)) then     ! (within the corridor along -V-bar)
                rgba(:, k) = [corridor_color, 1.0_wp]
            else
                rgba(:, k) = [violation_color, 1.0_wp]
                if (.not. violation) t_violation = t(k)
                violation = .true.
                worst = worst + 1
            end if
        end do
        call app%path%clear()
        call app%path%add(t, pos=reshape([(to_scene(s(1:3, k)), k = 1, size(t))], [3, size(t)]), color=rgba)

        ! the burns: their positions, Δv as each point's optional (the vectors)
        call app%burn_points%clear()
        dv_total = 0.0_wp
        text = ''
        do k = 1, app%n_burns
            call app%burn_points%add(app%burns(k)%t, pos=to_scene(app%burn_at(1:3, k)))
            call app%burn_points%set_optional(1, to_scene(app%burns(k)%dv))
            dv_total = dv_total + norm2(app%burns(k)%dv)
            write(line, '(i0,a,f6.1,a,3f7.3,a,f6.3,a)') k, ': t = ', app%burns(k)%t / 60.0_wp, ' min, dv (R, I, C) = ', &
                app%burns(k)%dv, ' (', norm2(app%burns(k)%dv), ' m/s)'
            if (k == app%selected) line = '<b>'//trim(line)//'</b>'
            text = trim(text)//trim(line)//'<br>'
        end do
        if (app%n_burns == 0) text = 'No burns: click the path to add one.'
        call app%burn_list%setText('<small>'//trim(text)//'</small>')
        call update_selection()

        write(text, '(a,f5.3,a,f0.1,a,f0.1,a)') '<small><b>Total dv</b> ', dv_total, ' m/s<br><b>Closest approach</b> ', &
            closest, ' m, at ', t_closest / 60.0_wp, ' min<br>'
        if (violation) then
            write(line, '(a,f0.1,a)') '<span style="color:#ff5050"><b>Keep-out violated</b> from ', t_violation / 60.0_wp, &
                ' min</span>'
        else
            line = '<span style="color:#60d060">Keep-out respected</span>'
        end if
        call app%readout%setText(trim(text)//trim(line)//'</small>')
        if (app%print_plan) write(*, '(a,i0,a,i0,a,a)') 'plan: ', app%n_burns, ' burns, ', worst, ' points in violation; ', &
            trim(text)//trim(line)
        write(text, '(a,i0,a,f0.1,a,es9.3,a)') 'RPO in the target''s LVLH frame    altitude ', nint(app%altitude%value()), &
            ' km    period ', period() / 60.0_wp, ' min    n = ', n, ' rad/s'
        call app%hud_title%set_text(trim(text))
        write(text, '(a,f0.1,a,f0.6,a)') '<small>Period ', period() / 60.0_wp, ' min, mean motion ', n, ' rad/s</small>'
        call app%orbit_info%setText(trim(text))

        if (app%proxy%is_open()) then                       ! the clock over the plan (in 20 s)
            call app%proxy%set_time_range(0.0_wp, t_end)
            call app%proxy%set_time_scale(t_end / 20.0_wp)
        end if
    end subroutine update_plan

    !> The selected burn's ring (none if none selected).
    subroutine update_selection()
        call app%selection%clear()
        if (app%selected < 1 .or. app%selected > app%n_burns) then
            app%selected = 0
            return
        end if
        call app%selection%add(0.0_wp, pos=to_scene(app%burn_at(1:3, app%selected)))
    end subroutine update_selection

    !> A burn at time t (s; no Δv yet), selected.
    subroutine add_burn(t)
        real(wp), intent(in) :: t
        integer :: k
        if (app%n_burns == max_burns) return
        app%n_burns = app%n_burns + 1
        app%burns(app%n_burns) = Burn(t=t)
        call sort_burns(app%burns(1:app%n_burns))
        do k = 1, app%n_burns                               ! (its place in time order)
            if (app%burns(k)%t == t .and. all(app%burns(k)%dv == 0.0_wp)) app%selected = k
        end do
        call update_plan()
    end subroutine add_burn

    subroutine delete_burn(k)
        integer, intent(in) :: k
        if (k < 1 .or. k > app%n_burns) return
        app%burns(k:app%n_burns - 1) = app%burns(k + 1:app%n_burns)
        app%n_burns = app%n_burns - 1
        app%selected = 0
        app%dirty = .true.
    end subroutine delete_burn

    ! ==================================================================
    ! the scene

    subroutine add(parent, child)
        class(ReferenceFrame), intent(inout) :: parent
        class(ReferenceFrame), intent(inout), target :: child
        if (.not. parent%add_child(child)) write(*, '(a)') 'rpo demo: add_child failed'
    end subroutine add

    subroutine build_scene()
        app%root = ReferenceFrame('LVLH')
        call app%root%show_decorations(.false.)

        ! the LVLH axes: arrows (labeled) and tick marks every 100 m (500 m major)
        app%axes = CoordinateAxes('LVLH axes', color=[0.75_wp, 0.75_wp, 0.8_wp])
        call app%axes%set_axis_length(2500.0_wp)
        call app%axes%set_tick_spacing(500.0_wp, 100.0_wp)
        call app%axes%show_name_label(.false.)
        call app%axes%set_x_label('V-bar (in-track)')
        call app%axes%set_y_label('cross-track')
        call app%axes%set_z_label('radial (up)')
        call app%axes%move_x_axis([0.0_wp, 0.0_wp, 0.0_wp], 250.0_wp, head_ratio=0.2_wp, body_radius=1.5_wp, &
                                  head_radius=5.0_wp)             ! (thin: radii in metres)
        call app%axes%move_y_axis([0.0_wp, 0.0_wp, 0.0_wp], 250.0_wp, head_ratio=0.2_wp, body_radius=1.5_wp, &
                                  head_radius=5.0_wp)
        call app%axes%move_z_axis([0.0_wp, 0.0_wp, 0.0_wp], 250.0_wp, head_ratio=0.2_wp, body_radius=1.5_wp, &
                                  head_radius=5.0_wp)
        call app%axes%set_pickable(.false.)
        call add(app%root, app%axes)

        app%target = Sphere('Target', radius=4.0_wp, color=[0.85_wp, 0.85_wp, 0.9_wp])
        call app%target%show_axes(NO_AXES)
        call app%target%show_axes_labels(NO_AXES)
        call app%target%set_name_label_on_top(.true.)
        call app%target%set_pickable(.false.)
        call add(app%root, app%target)

        app%keep_out_zone = Sphere('Keep-out sphere', radius=200.0_wp, color=[1.0_wp, 0.25_wp, 0.2_wp])
        call app%keep_out_zone%set_opacity(0.25_wp)                     ! (translucent: set before it is shown)
        call app%keep_out_zone%show_decorations(.false.)
        call app%keep_out_zone%set_pickable(.false.)
        call add(app%root, app%keep_out_zone)
        app%approach = EllipticCone('Approach corridor', color=[0.4_wp, 1.0_wp, 0.5_wp, 0.15_wp])
        call app%approach%make_cone_look_at([0.0_wp, 0.0_wp, 0.0_wp], [-1.0_wp, 0.0_wp, 0.0_wp], [0.0_wp, 0.0_wp, 1.0_wp])
        call app%approach%show_decorations(.false.)
        call app%approach%set_pickable(.false.)
        call add(app%root, app%approach)

        ! the path: in its safety colors; its start (green)
        app%path = Trajectory()
        app%path_curve = CurveArtist(app%path, color=safe_color, width=2.5_wp)
        app%path_start = MarkerArtist(app%path, markers=MARKER_START, size=10.0_wp)
        call app%path_start%set_marker_color(MARKER_START, [0.3_wp, 1.0_wp, 0.4_wp])
        call app%path_start%set_pickable(.false.)
        ! the burns: dots and Δv vectors
        app%burn_points = Trajectory(nopt=1)
        app%burn_dots = MarkerArtist(app%burn_points, markers=ior(ior(MARKER_START, MARKER_INTERMEDIATE), MARKER_END), &
                                     size=11.0_wp)
        call app%burn_dots%set_marker_color(ior(ior(MARKER_START, MARKER_INTERMEDIATE), MARKER_END), burn_color)
        call app%burn_dots%set_intermediate_type(INTERMEDIATE_DATA)
        app%burn_vectors = SegmentArtist(app%burn_points, color=burn_color, width=2.5_wp)
        if (.not. app%burn_vectors%set_start_data(position_sources(3))) write(*, '(a)') 'rpo demo: burn vectors'
        if (.not. app%burn_vectors%set_end_data(optional_sources(1))) write(*, '(a)') 'rpo demo: burn vectors'
        call app%burn_vectors%set_end_relative(.true.)                  ! (from each burn, its Δv times the scale)
        call app%burn_vectors%set_pickable(.false.)
        app%selection = Trajectory()
        app%selection_ring = MarkerArtist(app%selection, markers=MARKER_START, size=26.0_wp)
        call app%selection_ring%set_marker_shape(MARKER_CIRCLE_PULSE)
        call app%selection_ring%set_marker_color(MARKER_START, [1.0_wp, 1.0_wp, 1.0_wp])
        call app%selection_ring%set_pickable(.false.)
        app%path_frame = DrawableTrajectory('Plan')
        call app%path_frame%show_decorations(.false.)
        call app%path_frame%add_artist(app%path_curve)
        call app%path_frame%add_artist(app%path_start)
        call app%path_frame%add_artist(app%burn_vectors)
        call app%path_frame%add_artist(app%burn_dots)
        call app%path_frame%add_artist(app%selection_ring)
        call add(app%root, app%path_frame)

        ! the chaser: a marker at the origin of a frame flying the path (Animate)
        app%chaser = ReferenceFrame('Chaser')
        call app%chaser%show_axes(NO_AXES)
        call app%chaser%show_axes_labels(NO_AXES)
        call app%chaser%set_name_label_on_top(.true.)
        call app%chaser%follow(app%path, mode=FOLLOW_LIMIT)
        app%chaser_point = Trajectory()
        call app%chaser_point%add(0.0_wp, pos=[0.0_wp, 0.0_wp, 0.0_wp])
        app%chaser_dot = MarkerArtist(app%chaser_point, markers=MARKER_START, size=12.0_wp)
        call app%chaser_dot%set_marker_color(MARKER_START, [1.0_wp, 1.0_wp, 1.0_wp])
        call app%chaser_dot%set_pickable(.false.)
        app%chaser_frame = DrawableTrajectory('Chaser marker')
        call app%chaser_frame%show_decorations(.false.)
        call app%chaser_frame%add_artist(app%chaser_dot)
        call add(app%chaser, app%chaser_frame)
        call app%chaser_frame%show_contents(.false.)
        call app%chaser%show_name_label(.false.)
        call app%chaser%set_pickable(.false.)
        call add(app%root, app%chaser)
        app%scene = FrameManager(app%root)

        app%overview = View(app%root, app%root)
        call app%overview%set_default_view_distance(6000.0_wp)
        app%follow = View(app%root, app%chaser, frame_type=ABSOLUTE_FRAME)
        call app%follow%set_default_view_distance(800.0_wp)
        app%hud_title = HUDText('', position=[0.01_wp, 0.99_wp], alignment=ALIGN_LEFT_TOP, size=15.0_wp, &
                                color=[0.9_wp, 0.9_wp, 0.9_wp])
        app%hud_help = HUDText('Click the path: add a burn there    Click a burn: edit it', position=[0.01_wp, 0.01_wp], &
                               alignment=ALIGN_LEFT_BOTTOM, size=14.0_wp, color=[0.7_wp, 0.7_wp, 0.7_wp])
        app%hud_time = HUDText('', position=[0.99_wp, 0.01_wp], alignment=ALIGN_RIGHT_BOTTOM, size=14.0_wp, &
                               color=[0.8_wp, 0.8_wp, 0.8_wp])
    end subroutine build_scene

    !> The keep-out sphere and the corridor for the controls' radius and angle.
    subroutine update_zones()
        real(wp) :: r, a
        r = app%keep_out%value()
        a = app%corridor%value() * deg
        call app%keep_out_zone%set_radius(r)
        call app%approach%set_primary_angles(a, a)
        call app%approach%set_cone_length(1.6_wp * r)
    end subroutine update_zones

    ! ==================================================================
    ! picking and the burn's panel (the window's event handler)

    subroutine planner_press(self, window, x, y, button)
        class(Planner), intent(inout) :: self
        class(WindowProxy), intent(inout) :: window
        real(wp), intent(in) :: x, y
        integer, intent(in) :: button
        if (button /= MOUSE_LEFT) return
        self%pressed = [x, y]
        self%down = .true.
    end subroutine planner_press

    !> A click (released where pressed): on a burn, select it; on the path,
    !> add a burn there; elsewhere, deselect.
    subroutine planner_release(self, window, x, y, button)
        class(Planner), intent(inout) :: self
        class(WindowProxy), intent(inout) :: window
        real(wp), intent(in) :: x, y
        integer, intent(in) :: button
        type(PickHit), allocatable :: hits(:)
        if (button /= MOUSE_LEFT .or. .not. self%down) return
        self%down = .false.
        if (norm2([x, y] - self%pressed) > 0.01_wp) return
        call window%pick(x, y, hits, kinds=[PICK_MARKER, PICK_CURVE], nearest=.true.)
        if (size(hits) == 0) then
            app%selected = 0
            call update_selection()
            return
        end if
        select type (a => hits(1)%artist)
        type is (MarkerArtist)
            if (associated(a, app%burn_dots)) call select_burn_at(hits(1)%time)
        type is (CurveArtist)
            if (associated(a, app%path_curve)) call add_burn(hits(1)%time)
        end select
    end subroutine planner_release

    !> Select the burn at time t (the nearest).
    subroutine select_burn_at(t)
        real(wp), intent(in) :: t
        if (app%n_burns == 0) return
        app%selected = minloc(abs(app%burns(1:app%n_burns)%t - t), dim=1)
        app%dirty = .true.
    end subroutine select_burn_at

    !> Each frame: the selected burn's panel, next to it (its Δv and time:
    !> the path propagated again at the next frame), and the time.
    subroutine planner_before_draw(self, window, t)
        class(Planner), intent(inout) :: self
        class(WindowProxy), intent(inout) :: window
        real(wp), intent(in) :: t
        character(len=*), parameter :: labels(3) = [character(len=18) :: 'radial (m/s)', 'in-track (m/s)', &
                                                    'cross-track (m/s)']
        type(Gui) :: g
        character(len=80) :: text
        real(wp) :: at(2), dv, minutes
        integer :: k, j
        logical :: changed
        write(text, '(a,f0.1,a)') 't = ', t / 60.0_wp, ' min'
        call app%hud_time%set_text(trim(text))
        k = app%selected
        if (k < 1 .or. k > app%n_burns) return
        if (.not. window%screen_position(to_scene(app%burn_at(1:3, k)), at)) return
        g = window%gui()
        call g%begin_panel('##burn', position=at + [0.015_wp, 0.015_wp], pivot=[0.0_wp, 1.0_wp], follow=.true.)
        write(text, '(a,i0,a,f5.3,a)') 'Burn ', k, ': ', norm2(app%burns(k)%dv), ' m/s'
        call g%text(trim(text))
        changed = .false.
        do j = 1, 3
            dv = app%burns(k)%dv(j)
            if (g%slider(trim(labels(j)), dv, -1.0_wp, 1.0_wp)) then
                app%burns(k)%dv(j) = dv
                changed = .true.
            end if
        end do
        minutes = app%burns(k)%t / 60.0_wp
        if (g%slider('time (min)', minutes, 0.0_wp, app%orbits%value() * period() / 60.0_wp)) then
            app%burns(k)%t = 60.0_wp * minutes
            changed = .true.
        end if
        if (g%button('Delete')) call delete_burn(k)
        call g%same_line()
        if (g%button('Close')) then
            app%selected = 0
            changed = .true.
        end if
        call g%end_panel()
        if (changed) app%dirty = .true.
    end subroutine planner_before_draw

    ! ==================================================================
    ! the window

    subroutine open_view()
        if (.not. app%proxy%open(app%window%width(), app%window%height(), 'vkframes', &
                                 native_window=app%window%winId(), pixel_ratio=app%window%devicePixelRatio())) then
            write(*, '(a)') 'vkframes: could not open the view'
            return
        end if
        if (app%proxy%num_views() > 0) return                    ! (opened again, on a new native window)
        call app%proxy%set_scene(app%scene)
        call app%proxy%set_background_color([0.02_wp, 0.02_wp, 0.05_wp])
        call app%proxy%add_view(app%overview)
        call app%proxy%add_view(app%follow)
        call app%proxy%set_view_transition(1.0_wp)
        call app%proxy%add_hud_text(app%hud_title)
        call app%proxy%add_hud_text(app%hud_help)
        call app%proxy%add_hud_text(app%hud_time)
        call app%proxy%set_event_handler(app%planner)
        call app%proxy%set_zoom_speed(real(app%zoom_speed%value(), wp))
        call app%proxy%set_camera_throw(.false.)
        call update_plan()
        call frame_plan()
        call app%proxy%set_time(0.0_wp)
        if (app%select_at_start > 0) then
            app%selected = app%select_at_start
            call update_plan()
        end if
    end subroutine open_view

    !> Exposed: start the frame loop; on a new native window (the old one
    !> destroyed: watch_surface closed the view), open the view again.
    subroutine on_expose(data)
        class(*), intent(inout), optional :: data
        if (.not. app%window%isExposed()) return
        if (.not. app%proxy%is_open() .and. app%proxy%num_views() > 0) then
            call open_view()
            if (app%proxy%is_open()) call app%window%requestUpdate()
            return
        end if
        if (app%proxy%is_open() .and. app%frame == 0) call app%window%requestUpdate()
    end subroutine on_expose

    !> A frame (the plan propagated first if it changed), and the next asked for.
    subroutine on_update(data)
        class(*), intent(inout), optional :: data
        if (app%window%devicePixelRatio() /= app%proxy%pixel_ratio()) &
            call app%proxy%resize(nint(app%window%width() * app%window%devicePixelRatio()), &
                                  nint(app%window%height() * app%window%devicePixelRatio()), &
                                  pixel_ratio=app%window%devicePixelRatio())
        if (app%dirty) call update_plan()
        if (app%frame_pending) call frame_plan()
        if (.not. app%proxy%frame()) return
        app%frame = app%frame + 1
        if (app%frame == 30 .and. app%click_time >= 0.0_wp) call click_path(app%click_time)
        if (allocated(app%capture) .and. app%frame == app%capture_frame) then
            if (app%proxy%capture(app%capture)) write(*, '(a)') 'captured '//app%capture
        end if
        call app%window%requestUpdate()
    end subroutine on_update

    !> --click: a click (mouse press and release) on the path at time t, as the user's.
    subroutine click_path(t)
        real(wp), intent(in) :: t
        real(wp) :: at(2), dpr, p(3)
        integer :: k, i, px, py
        if (app%path%num_times() == 0) return
        k = minloc(abs([(app%path%time(i), i = 1, app%path%num_times())] - t), dim=1)
        p = app%path%position(k)
        if (.not. app%proxy%screen_position(p, at)) return
        dpr = app%window%devicePixelRatio()
        px = nint(at(1) * app%window%width() * dpr)
        py = nint((1.0_wp - at(2)) * app%window%height() * dpr)
        call app%proxy%mouse(MOUSE_PRESS, px, py, 256, 1)
        call app%proxy%mouse(MOUSE_RELEASE, px, py, 0, 1)
    end subroutine click_path

    !> The space bar (reset the view) and v / V (the next and previous view) to vkframes.
    subroutine on_key(event, pressed)
        type(QKeyEvent), intent(in) :: event
        logical, intent(in) :: pressed
        select case (event%key())
        case (Qt_Key_Space)
            call app%proxy%key(pressed, 32, 32, 0)
        case (Qt_Key_A + 21)                                    ! v (V with shift)
            if (iand(event%modifiers(), Qt_ShiftModifier) /= 0) then
                call app%proxy%key(pressed, iachar('v'), iachar('V'), 1)
            else
                call app%proxy%key(pressed, iachar('v'), iachar('v'), 0)
            end if
        case default
            return
        end select
        call event%accept()
    end subroutine on_key

    subroutine on_key_press(event, data)
        type(QKeyEvent), intent(in) :: event
        class(*), intent(inout), optional :: data
        call on_key(event, .true.)
    end subroutine on_key_press

    subroutine on_key_release(event, data)
        type(QKeyEvent), intent(in) :: event
        class(*), intent(inout), optional :: data
        call on_key(event, .false.)
    end subroutine on_key_release

    ! ==================================================================
    ! controls (GUI thread)

    subroutine on_preset(index, data)
        integer, intent(in) :: index
        class(*), intent(inout), optional :: data
        if (app%building) return
        call load_preset(index + 1)
    end subroutine on_preset

    !> The plan's settings changed (altitude, duration, initial state): propagated again.
    subroutine on_plan_value(value, data)
        real(c_double), intent(in) :: value
        class(*), intent(inout), optional :: data
        if (.not. app%building) app%dirty = .true.
    end subroutine on_plan_value

    !> The keep-out radius or the corridor's angle: the zones and the path's colors.
    subroutine on_zone_value(value, data)
        real(c_double), intent(in) :: value
        class(*), intent(inout), optional :: data
        if (app%building) return
        call update_zones()
        app%dirty = .true.
    end subroutine on_zone_value

    !> The keep-out sphere's opacity.
    subroutine on_keep_out_opacity(value, data)
        real(c_double), intent(in) :: value
        class(*), intent(inout), optional :: data
        call app%keep_out_zone%set_opacity(real(value, wp))
    end subroutine on_keep_out_opacity

    subroutine on_arrow_scale(value, data)
        real(c_double), intent(in) :: value
        class(*), intent(inout), optional :: data
        call app%burn_vectors%set_end_scale(real(value, wp))
    end subroutine on_arrow_scale

    subroutine on_zoom_speed(value, data)
        real(c_double), intent(in) :: value
        class(*), intent(inout), optional :: data
        call app%proxy%set_zoom_speed(real(value, wp))
    end subroutine on_zoom_speed

    subroutine on_clear_burns(data)
        class(*), intent(inout), optional :: data
        app%n_burns = 0
        app%selected = 0
        app%dirty = .true.
    end subroutine on_clear_burns

    !> Animate: the path traced up to the chaser, flying it (from the start).
    subroutine on_animate(checked, data)
        logical, intent(in) :: checked
        class(*), intent(inout), optional :: data
        call app%path_curve%set_trace_mode(checked)
        call app%chaser_frame%show_contents(checked)
        call app%chaser%show_name_label(checked)
        if (app%proxy%is_open()) call app%proxy%set_time(0.0_wp)
    end subroutine on_animate

    subroutine on_replay(data)
        class(*), intent(inout), optional :: data
        if (app%proxy%is_open()) call app%proxy%set_time(0.0_wp)
    end subroutine on_replay

    subroutine on_view(index, data)
        integer, intent(in) :: index
        class(*), intent(inout), optional :: data
        if (app%proxy%is_open()) call app%proxy%select_view(index + 1)
    end subroutine on_view

    subroutine on_reset_view(data)
        class(*), intent(inout), optional :: data
        type(View), pointer :: v
        v => app%proxy%current_view()
        if (associated(v)) call v%reset_view()
    end subroutine on_reset_view

    !> Record the view to a video file chosen (60 frames per second), or stop.
    subroutine on_record(data)
        class(*), intent(inout), optional :: data
        character(len=:), allocatable :: file
        if (app%proxy%is_recording()) then
            if (.not. app%proxy%stop_recording()) write(*, '(a)') 'vkframes: the recording failed (see ffmpeg''s messages)'
            call app%record%setText('Record video...')
            return
        end if
        file = QFileDialog_getSaveFileName('Record video', 'rpo.mp4', 'Videos (*.mp4 *.mov *.webm)', app%main)
        if (len(file) == 0) return
        if (app%proxy%start_recording(file, fps=60.0_wp)) call app%record%setText('Stop recording')
    end subroutine on_record

    subroutine on_quit(data)
        class(*), intent(inout), optional :: data
        call app%main%close()
    end subroutine on_quit

    subroutine on_close(event, data)
        type(QCloseEvent), intent(in) :: event
        class(*), intent(inout), optional :: data
        call app%proxy%close()
    end subroutine on_close

    subroutine on_startup(data)
        class(*), intent(inout), optional :: data
        type(c_ptr) :: font
        call app%startup_message%setText('Starting Vulkan...')
        call QApplication_processEvents()
        font = get_font('')
        call open_view()
        call app%stack%setCurrentIndex(1)
        if (app%proxy%is_open()) call app%window%requestUpdate()
    end subroutine on_startup

    ! ==================================================================
    ! construction

    function real_box(low, high, value, decimals, step, suffix, slot) result(box)
        real(wp), intent(in) :: low, high, value, step
        integer, intent(in) :: decimals
        character(len=*), intent(in) :: suffix
        procedure(on_plan_value) :: slot
        type(QDoubleSpinBox) :: box
        box = QDoubleSpinBox()
        call box%setDecimals(decimals)
        call box%setRange(real(low, c_double), real(high, c_double))
        call box%setSingleStep(real(step, c_double))
        call box%setValue(real(value, c_double))
        if (len(suffix) > 0) call box%setSuffix(suffix)
        call box%valueChanged%connect(slot)
    end function real_box

    subroutine build_ui()
        type(QDockWidget) :: dock
        type(QScrollArea) :: scroll
        type(QWidget) :: panel
        type(QVBoxLayout) :: column
        type(QGroupBox) :: group
        type(QFormLayout) :: form
        type(QPushButton) :: button
        type(QMenuBar) :: bar
        type(QMenu) :: menu
        type(QAction) :: action
        character(len=*), parameter :: state_names(6) = [character(len=20) :: 'Start radial', 'Start in-track', &
            'Start cross-track', 'Start v radial', 'Start v in-track', 'Start v cross-track']
        integer :: k

        app%main = QMainWindow()
        call app%main%setWindowTitle('vkframes: rendezvous and proximity operations')
        call app%main%closeEvent%connect(on_close)
        bar = app%main%menuBar()
        menu = bar%addMenu('&File')
        action = QAction('&Quit', app%main)
        call action%setShortcut('Ctrl+Q')
        call action%triggered%connect(on_quit)
        call menu%addAction(action)

        app%window = QWindow()
        app%container = QWidget_createWindowContainer(app%window)
        call app%container%setMinimumSize(320, 240)
        app%events = QtfEventFilter(app%window)
        call app%events%exposeEvent%connect(on_expose)
        call app%events%updateRequest%connect(on_update)
        call connect_view(app%view_link, app%events, app%proxy, app%window, keys=.false.)   ! (mouse, resizes, the surface)
        call app%events%keyPressEvent%connect(on_key_press)                     ! (the keys: the demo's own)
        call app%events%keyReleaseEvent%connect(on_key_release)
        app%stack = QStackedWidget()
        app%startup_message = QLabel('Starting...')
        call app%startup_message%setAlignment(Qt_AlignCenter)
        call app%startup_message%setStyleSheet('background-color: black; color: #cccccc; font-size: 16pt;')
        call app%stack%addWidget(app%startup_message)
        call app%stack%addWidget(app%container)
        call app%main%setCentralWidget(app%stack)

        panel = QWidget()
        column = QVBoxLayout(panel)

        ! the plan: a preset, the target's orbit, the duration, the chaser's start
        group = QGroupBox('Plan')
        form = QFormLayout(group)
        app%preset = QComboBox()
        call app%preset%addItems(preset_names)
        call app%preset%currentIndexChanged%connect(on_preset)
        call form%addRow('Preset', app%preset)
        app%altitude = real_box(200.0_wp, 36000.0_wp, 400.0_wp, 0, 50.0_wp, ' km', on_plan_value)
        call app%altitude%setToolTip('The target''s circular orbit (sets the mean motion)')
        call form%addRow('Target altitude', app%altitude)
        app%orbit_info = QLabel('')
        call form%addRow(app%orbit_info)
        app%orbits = real_box(0.1_wp, 10.0_wp, 1.5_wp, 2, 0.25_wp, ' orbits', on_plan_value)
        call form%addRow('Duration', app%orbits)
        do k = 1, 6
            if (k <= 3) then
                app%state(k) = real_box(-20000.0_wp, 20000.0_wp, 0.0_wp, 1, 50.0_wp, ' m', on_plan_value)
            else
                app%state(k) = real_box(-20.0_wp, 20.0_wp, 0.0_wp, 4, 0.01_wp, ' m/s', on_plan_value)
            end if
            call form%addRow(trim(state_names(k)), app%state(k))
        end do
        call column%addWidget(group)

        ! safety
        group = QGroupBox('Safety')
        form = QFormLayout(group)
        app%keep_out = real_box(10.0_wp, 5000.0_wp, 200.0_wp, 0, 10.0_wp, ' m', on_zone_value)
        call form%addRow('Keep-out radius', app%keep_out)
        app%keep_out_opacity = real_box(0.02_wp, 0.95_wp, 0.25_wp, 2, 0.05_wp, '', on_keep_out_opacity)
        call app%keep_out_opacity%setToolTip('How opaque the translucent keep-out sphere is')
        call form%addRow('Keep-out opacity', app%keep_out_opacity)
        app%corridor = real_box(1.0_wp, 60.0_wp, 15.0_wp, 1, 1.0_wp, ' deg', on_zone_value)
        call app%corridor%setToolTip('The approach corridor''s half-angle about -V-bar')
        call form%addRow('Corridor half-angle', app%corridor)
        call column%addWidget(group)

        ! burns
        group = QGroupBox('Burns')
        form = QFormLayout(group)
        app%burn_list = QLabel('')
        call app%burn_list%setWordWrap(.true.)
        call form%addRow(app%burn_list)
        button = QPushButton('Clear burns')
        call button%clicked%connect(on_clear_burns)
        call form%addRow('', button)
        app%arrow_scale = real_box(10.0_wp, 5000.0_wp, 400.0_wp, 0, 50.0_wp, ' m per m/s', on_arrow_scale)
        call form%addRow('dv arrow scale', app%arrow_scale)
        app%readout = QLabel('')
        call app%readout%setWordWrap(.true.)
        call form%addRow(app%readout)
        call column%addWidget(group)

        ! playback and display
        group = QGroupBox('Playback and view')
        form = QFormLayout(group)
        app%animate = QCheckBox('Animate (the chaser along the plan)')
        call app%animate%toggled%connect(on_animate)
        call form%addRow('', app%animate)
        button = QPushButton('Replay')
        call button%clicked%connect(on_replay)
        call form%addRow('', button)
        app%view_choice = QComboBox()
        call app%view_choice%addItems([character(len=24) :: 'The whole approach', 'Following the chaser'])
        call app%view_choice%currentIndexChanged%connect(on_view)
        call form%addRow('View', app%view_choice)
        button = QPushButton('Reset view (space)')
        call button%clicked%connect(on_reset_view)
        call form%addRow('', button)
        app%zoom_speed = real_box(0.05_wp, 3.0_wp, 0.5_wp, 2, 0.05_wp, ' x', on_zoom_speed)
        call form%addRow('Zoom speed', app%zoom_speed)
        app%record = QPushButton('Record video...')
        call app%record%clicked%connect(on_record)
        call form%addRow('', app%record)
        call column%addWidget(group)

        call column%addWidget(QLabel('<small>Click the path: add a burn<br>Click a burn: edit it<br>' // &
                                     'Left drag: rotate<br>Right drag: zoom<br>Middle drag: pan<br>' // &
                                     'v / V: the next / previous view<br>Space: reset the view</small>'))
        call column%addStretch()
        scroll = QScrollArea()
        call scroll%setWidgetResizable(.true.)
        call scroll%setWidget(panel)
        dock = QDockWidget('Controls', app%main)
        call dock%setWidget(scroll)
        call app%main%addDockWidget(Qt_LeftDockWidgetArea, dock)
    end subroutine build_ui

end module rpo_demo

!> Rendezvous and proximity operations in a qt-fortran GUI (see the module documentation).
program vkframes_rpo_demo

    use qt
    use vkframes
    use rpo_demo
    implicit none

    type(QApplication) :: qapp
    character(len=1024) :: arg
    integer :: i, ierr

    qapp = QApplication()
    i = 1
    do while (i <= command_argument_count())
        call get_command_argument(i, arg)
        select case (arg)
        case ('--capture')
            call get_command_argument(i + 1, arg)
            app%capture = trim(arg)
            i = i + 1
        case ('--capture-frame')
            call get_command_argument(i + 1, arg)
            read(arg, *) app%capture_frame
            i = i + 1
        case ('--preset')
            call get_command_argument(i + 1, arg)
            read(arg, *) app%start_preset
            i = i + 1
        case ('--select')
            call get_command_argument(i + 1, arg)
            read(arg, *) app%select_at_start
            i = i + 1
        case ('--click')
            call get_command_argument(i + 1, arg)
            read(arg, *) app%click_time
            app%print_plan = .true.
            i = i + 1
        case ('--quit-after')
            call get_command_argument(i + 1, arg)
            read(arg, *) app%quit_after
            i = i + 1
        end select
        i = i + 1
    end do

    call build_scene()
    call build_ui()
    call update_zones()
    app%building = .false.
    call app%burn_vectors%set_end_scale(real(app%arrow_scale%value(), wp))
    app%start_preset = max(1, min(size(preset_names), app%start_preset))
    if (app%start_preset /= 1) then
        call app%preset%setCurrentIndex(app%start_preset - 1)          ! (loads it)
    else
        call load_preset(1)
    end if
    if (app%quit_after > 0) call QTimer_singleShot(int(1000 * app%quit_after), on_quit)
    call app%main%resize(1400, 900)
    call app%main%show()
    call QTimer_singleShot(0, on_startup)
    ierr = qapp%exec()

end program vkframes_rpo_demo
