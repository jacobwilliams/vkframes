!> The Earth-Moon circular restricted three-body problem (CR3BP) in a
!> qt-fortran GUI: the Earth and the Moon fixed in the frame rotating with
!> them (about their barycenter), the Lagrange points, and a cloud of states
!> of one Jacobi constant, drawn as dots (a MarkerArtist's markers, one per
!> state, of a trajectory that is never drawn as a line). The cloud fills the
!> region that constant allows, inside its zero velocity curves. A click on a
!> dot (not a drag) propagates that state with the CR3BP equations of motion
!> (app/cr3bp.f90: adaptive Dormand-Prince 5(4), stopping at an impact) and
!> draws it: a highlighted curve (arrows moving along it, in the direction
!> of time), its start and end, and (Animate) a spacecraft flying along it.
!>
!> The dock changes the system and the scene, each change drawn at once: the
!> Earth-Moon distance (the scene is in km; the time unit, and so the
!> propagation time in days, follows from it and the Earth's and Moon's
!> masses), the mass ratio mu, the Jacobi constant (with the Lagrange
!> points' constants for reference), the number of states, planar or 3D, the
!> random seed, the dots' size, the dots' color (all blue, or by each
!> state's fate after the propagation time: hits the Earth, hits the Moon,
!> escapes beyond the escape distance, or neither; each state propagated,
!> its dot in that color by the MarkerArtist's point colors), and the
!> propagation: its time (days),
!> forwards or backwards, animated or not; the bodies' drawn size (impacts
!> stay at their real radii); the mouse's zoom speed and whether the camera
!> keeps moving after a released drag. A readout shows the state picked
!> (nondimensional), its Jacobi constant and how the propagation ended.
!>
!> Keys (in the view): e and m fly the camera to the Earth and the Moon (it
!> then turns about them), the space bar resets the view.
!>
!> Run with `pixi run -e qt cr3bp-demo` (from the vkframes root). Options:
!> `--select k` propagates state k at the start; `--click-near x y` clicks
!> the dot nearest (x, y) (nondimensional) after 30 frames, as the user would;
!> `--go-to earth` (or `moon`) flies there after 30 frames, as the e (m) key;
!> `--color-by fate` starts with the dots colored by their fate; `--capture file` saves
!> frame `--capture-frame n` (default 120) as an image; `--quit-after
!> seconds` closes the demo.
module cr3bp_demo

    use qt
    use vkframes
    use vkframes_qt, only: qt_forward_mouse => forward_mouse, qt_forward_wheel => forward_wheel, &
                           watch_surface
    use vkf_text, only: get_font
    use cr3bp, only: propagate, jacobi, lagrange_points, sample_states, PROPAGATED, IMPACT_1, IMPACT_2, ESCAPED
    use iso_c_binding
    use, intrinsic :: iso_fortran_env, only: int64
    !$ use omp_lib, only: omp_get_max_threads

    implicit none

    real(wp), parameter :: gm_earth = 398600.4418_wp, gm_moon = 4902.800066_wp     !! km^3/s^2
    real(wp), parameter :: r_earth = 6378.137_wp, r_moon = 1737.4_wp                !! km
    real(wp), parameter :: day = 86400.0_wp                                         !! s
    real(wp), parameter :: cloud_color(3) = [0.45_wp, 0.75_wp, 1.0_wp]
    real(wp), parameter :: path_color(3) = [1.0_wp, 0.6_wp, 0.2_wp]
    !> the cloud's colors by fate (Color by: fate): hits the Earth, hits the Moon, escapes, neither
    real(wp), parameter :: fate_colors(3, 4) = reshape([0.3_wp, 0.9_wp, 0.4_wp, 0.95_wp, 0.95_wp, 0.95_wp, &
                                                        1.0_wp, 0.35_wp, 0.3_wp, 0.45_wp, 0.75_wp, 1.0_wp], [3, 4])
    character(len=*), parameter :: fate_names(4) = [character(len=13) :: 'hits the Earth', 'hits the Moon', &
                                                     'escapes', 'neither']

    !> Picks a dot of the cloud on a click (a press and release in the same place).
    type, extends(WindowEventHandler) :: Picker
        real(wp) :: pressed(2) = 0.0_wp                        !! where the left button went down
        logical :: down = .false.
    contains
        procedure :: button_press => picker_press
        procedure :: button_release => picker_release
    end type Picker

    !> The GUI, the scene and the system.
    type :: app_t
        type(QMainWindow) :: main
        type(QStackedWidget) :: stack                          !! the center: a startup message, then the view
        type(QLabel) :: startup_message
        type(QWindow) :: window
        type(QWidget) :: container
        type(QtfEventFilter) :: events
        type(WindowProxy) :: proxy
        type(Picker) :: picker
        integer :: frame = 0
        ! controls
        type(QDoubleSpinBox) :: distance, mass_ratio, jacobi_c, marker_size, days, body_size, escape, zoom_speed
        type(QCheckBox) :: camera_throw
        type(QComboBox) :: color_by                            !! 0: all blue, 1: by fate
        type(QLabel) :: fate_legend
        type(QSpinBox) :: n_points, seed
        type(QCheckBox) :: planar, backward, animate, show_lagrange
        type(QLabel) :: lagrange_c, cloud_info, readout
        ! the scene (km, in the rotating frame, about the barycenter)
        type(ReferenceFrame) :: root
        type(Sphere) :: earth, moon
        type(Trajectory) :: cloud                              !! the states' positions (times 1 .. n: their numbers)
        type(DrawableTrajectory) :: cloud_frame
        type(MarkerArtist) :: cloud_dots
        type(Trajectory) :: lagrange
        type(DrawableTrajectory) :: lagrange_frame
        type(MarkerArtist) :: lagrange_dots
        type(ReferenceFrame) :: lagrange_labels(5)
        type(Trajectory) :: path                               !! the propagated state (times in s)
        type(DrawableTrajectory) :: path_frame
        type(CurveArtist) :: path_curve
        type(MarkerArtist) :: path_start, path_end
        type(ReferenceFrame) :: craft                          !! flies along the path (Animate)
        type(Trajectory) :: craft_point
        type(DrawableTrajectory) :: craft_frame
        type(MarkerArtist) :: craft_dot
        type(FrameManager) :: scene
        type(View) :: view
        type(HUDText) :: hud_title, hud_help
        ! the system (nondimensional, except the distance)
        real(wp) :: mu = 0.012150585_wp                        !! the Moon's mass / (the Earth's + the Moon's)
        real(wp) :: length = 384400.0_wp                       !! the Earth-Moon distance (km)
        real(wp), allocatable :: states(:,:)                   !! (6, n): the cloud's states
        real(wp) :: selected(6) = 0.0_wp                       !! the state picked
        logical :: has_selection = .false.
        real(wp) :: animation_start = 0.0_wp                   !! the path's earliest time (s): where Animate starts
        logical :: building = .true.                           !! the GUI is being built (no updates yet)
        ! options
        character(len=:), allocatable :: capture
        integer :: capture_frame = 120
        integer :: select_at_start = 0
        real(wp) :: click_near(2) = huge(1.0_wp)               !! (--click-near: a click there, nondimensional)
        logical :: fate_at_start = .false.                     !! (--color-by fate)
        integer :: go_to = 0                                   !! (--go-to: fly to the Earth (1) or the Moon (2))
        real(wp) :: quit_after = -1.0_wp
    end type app_t

    !> The application (module-level, so that it is a target: frames, artists
    !> and views keep pointers to each other).
    type(app_t), target, save :: app

contains

    ! ==================================================================
    ! the system and the scene

    !> The time unit (s): 1 / the mean motion, sqrt(distance^3 / GM).
    real(wp) function time_unit()
        time_unit = sqrt(app%length**3 / (gm_earth + gm_moon))
    end function time_unit

    !> The scene: the bodies, the Lagrange points, the cloud, the path and
    !> the spacecraft, in the rotating frame (positions set by place_bodies,
    !> make_cloud and propagate_selected).
    subroutine build_scene()
        integer :: k
        character(len=2) :: name
        app%root = ReferenceFrame('Rotating frame')
        call app%root%show_decorations(.false.)

        app%earth = Sphere('Earth', radius=3.0_wp * r_earth)              ! (drawn 3 x: the Body size control)
        call app%earth%set_texture_map('2k_earth_daymap.jpg')         ! (data/, on VKFRAMES_DATA_PATH)
        call app%earth%show_axes(NO_AXES)
        call app%earth%show_axes_labels(NO_AXES)
        app%moon = Sphere('Moon', radius=3.0_wp * r_moon, color=[0.75_wp, 0.75_wp, 0.72_wp])
        call app%moon%set_texture_map('lroc_color_2k.jpg')
        call app%moon%show_axes(NO_AXES)
        call app%moon%show_axes_labels(NO_AXES)
        call app%earth%set_name_label_on_top(.true.)                  ! (not hidden by the bodies drawn larger)
        call app%moon%set_name_label_on_top(.true.)

        ! the Lagrange points: crosses (not pickable) and their labels
        app%lagrange = Trajectory()
        app%lagrange_dots = MarkerArtist(app%lagrange, markers=ior(ior(MARKER_START, MARKER_INTERMEDIATE), MARKER_END), &
                                         size=12.0_wp, color=[1.0_wp, 0.9_wp, 0.4_wp])
        call app%lagrange_dots%set_marker_shape(MARKER_PLUS)
        call app%lagrange_dots%set_intermediate_type(INTERMEDIATE_DATA)
        call app%lagrange_dots%set_intermediate_spacing(1.0_wp)
        call app%lagrange_dots%set_pickable(.false.)
        app%lagrange_frame = DrawableTrajectory('Lagrange points')
        call app%lagrange_frame%show_decorations(.false.)
        call app%lagrange_frame%add_artist(app%lagrange_dots)
        call add(app%root, app%lagrange_frame)
        do k = 1, 5
            write(name, '(a,i0)') 'L', k
            app%lagrange_labels(k) = ReferenceFrame(name, color=[1.0_wp, 0.9_wp, 0.4_wp])
            call app%lagrange_labels(k)%show_axes(NO_AXES)
            call app%lagrange_labels(k)%show_axes_labels(NO_AXES)
            call app%lagrange_labels(k)%set_label_size(14)
            call app%lagrange_labels(k)%set_pickable(.false.)
            call add(app%root, app%lagrange_labels(k))
        end do

        ! the cloud: a dot per state (start, end and every point between)
        app%cloud = Trajectory()
        app%cloud_dots = MarkerArtist(app%cloud, markers=ior(ior(MARKER_START, MARKER_INTERMEDIATE), MARKER_END), &
                                      size=4.0_wp, color=cloud_color)
        call app%cloud_dots%set_marker_color(MARKER_START, cloud_color)
        call app%cloud_dots%set_marker_color(MARKER_END, cloud_color)
        call app%cloud_dots%set_intermediate_type(INTERMEDIATE_DATA)
        call app%cloud_dots%set_intermediate_spacing(1.0_wp)
        app%cloud_frame = DrawableTrajectory('Cloud')
        call app%cloud_frame%show_decorations(.false.)
        call app%cloud_frame%add_artist(app%cloud_dots)
        call add(app%root, app%cloud_frame)

        ! the path: highlighted (arrows along it, in the direction of time), its start (pulsing) and end
        app%path = Trajectory()
        app%path_curve = CurveArtist(app%path, color=path_color, width=2.0_wp)
        call app%path_curve%set_highlight(HIGHLIGHT_ARROWS, color=[1.0_wp, 0.9_wp, 0.7_wp], arrow_spacing=50.0_wp, &
                                          arrow_speed=40.0_wp, band_width=10.0_wp)
        call app%path_curve%set_pickable(.false.)
        app%path_start = MarkerArtist(app%path, markers=MARKER_START, size=24.0_wp)
        call app%path_start%set_marker_shape(MARKER_CIRCLE_PULSE)
        call app%path_start%set_marker_color(MARKER_START, [1.0_wp, 1.0_wp, 1.0_wp])
        call app%path_start%set_pickable(.false.)
        app%path_end = MarkerArtist(app%path, markers=MARKER_END, size=9.0_wp)
        call app%path_end%set_marker_color(MARKER_END, [1.0_wp, 0.3_wp, 0.25_wp])
        call app%path_end%set_pickable(.false.)
        app%path_frame = DrawableTrajectory('Path')
        call app%path_frame%show_decorations(.false.)
        call app%path_frame%add_artist(app%path_curve)
        call app%path_frame%add_artist(app%path_start)
        call app%path_frame%add_artist(app%path_end)
        call add(app%root, app%path_frame)

        ! the spacecraft: a marker at the origin of a frame following the path
        app%craft = ReferenceFrame('Spacecraft')
        call app%craft%show_decorations(.false.)
        call app%craft%follow(app%path, mode=FOLLOW_LIMIT)
        app%craft_point = Trajectory()
        call app%craft_point%add(0.0_wp, pos=[0.0_wp, 0.0_wp, 0.0_wp])
        app%craft_dot = MarkerArtist(app%craft_point, markers=MARKER_START, size=12.0_wp)
        call app%craft_dot%set_marker_color(MARKER_START, [1.0_wp, 1.0_wp, 1.0_wp])
        call app%craft_dot%set_pickable(.false.)
        app%craft_frame = DrawableTrajectory('Spacecraft marker')
        call app%craft_frame%show_decorations(.false.)
        call app%craft_frame%add_artist(app%craft_dot)
        call add(app%craft, app%craft_frame)
        call app%craft%show_contents(.false.)
        call add(app%root, app%craft)

        call add(app%root, app%earth)
        call add(app%root, app%moon)
        app%scene = FrameManager(app%root)

        app%view = View(app%root, app%root)
        app%hud_title = HUDText('', position=[0.01_wp, 0.99_wp], alignment=ALIGN_LEFT_TOP, size=16.0_wp, &
                                color=[0.9_wp, 0.9_wp, 0.9_wp])
        app%hud_help = HUDText('Click a dot to propagate its state', position=[0.01_wp, 0.01_wp], &
                               alignment=ALIGN_LEFT_BOTTOM, size=14.0_wp, color=[0.7_wp, 0.7_wp, 0.7_wp])
    end subroutine build_scene

    subroutine add(parent, child)
        class(ReferenceFrame), intent(inout) :: parent
        class(ReferenceFrame), intent(inout), target :: child
        if (.not. parent%add_child(child)) write(*, '(a)') 'cr3bp demo: add_child failed'
    end subroutine add

    !> The bodies and the Lagrange points where mu and the distance put them
    !> (km), the reference Jacobi constants, the HUD title, and the view's
    !> distance.
    subroutine place_bodies()
        real(wp) :: l(3, 5), c(5)
        character(len=200) :: text
        integer :: k
        call app%earth%set_position([-app%mu * app%length, 0.0_wp, 0.0_wp])
        call app%moon%set_position([(1.0_wp - app%mu) * app%length, 0.0_wp, 0.0_wp])
        l = lagrange_points(app%mu)
        call app%lagrange%clear()
        call app%lagrange%add([(real(k, wp), k = 1, 5)], pos=app%length * l)
        do k = 1, 5
            call app%lagrange_labels(k)%set_position(app%length * l(:, k) + [0.0_wp, 0.035_wp * app%length, 0.0_wp])
            c(k) = jacobi(app%mu, [l(:, k), 0.0_wp, 0.0_wp, 0.0_wp])
        end do
        write(text, '(a,f7.4,a,f7.4,a,f7.4,a,f7.4)') '<small>C(L1) = ', c(1), ', C(L2) = ', c(2), '<br>C(L3) = ', c(3), &
            ', C(L4, L5) = ', c(4)
        call app%lagrange_c%setText(trim(text))
        write(text, '(a,f9.6,a,f0.0,a,f7.4)') 'Earth-Moon CR3BP, rotating frame    mu = ', app%mu, '    distance = ', &
            app%length, ' km    C = ', app%jacobi_c%value()
        call app%hud_title%set_text(trim(text))
        call app%view%set_default_view_distance(3.0_wp * app%length)
    end subroutine place_bodies

    !> A new cloud of states (the controls' constant, number, planar or not,
    !> seed), drawn as dots in km.
    subroutine make_cloud()
        real(wp), allocatable :: s(:,:)
        integer, allocatable :: seed(:)
        integer :: n, found, k
        character(len=120) :: text
        n = app%n_points%value()
        call random_seed(size=k)
        allocate(seed(k))
        seed = app%seed%value() + 7919 * [(k, k = 1, size(seed))]
        call random_seed(put=seed)
        allocate(s(6, n))
        call sample_states(app%mu, real(app%jacobi_c%value(), wp), n, 1.6_wp, app%planar%isChecked(), &
                           [3.0_wp * r_earth, 3.0_wp * r_moon] / app%length, s, found)
        app%states = s(:, 1:found)
        call draw_cloud()
        write(text, '(i0,a,i0,a)') found, ' states', n, ' wanted'
        if (found == n) write(text, '(i0,a)') found, ' states'
        if (found == 0) text = 'No states: C is above the allowed region''s'
        call app%cloud_info%setText('<small>'//trim(text)//'</small>')
        call place_bodies()                                 ! (the HUD's C)
    end subroutine make_cloud

    !> Draw the cloud's states (km): all blue, or (Color by: fate) colored by
    !> their fate after the propagation time (and direction): each state
    !> propagated (to a looser tolerance than the state picked: only where it
    !> ends matters), its dot in the trajectory's color there (MarkerArtist
    !> point colors). A legend counts each fate. The states are propagated in
    !> parallel (OpenMP: each one on its own; the widgets' values read first,
    !> Qt being for the GUI thread only), in dynamic chunks (some end early,
    !> at an impact or an escape).
    subroutine draw_cloud()
        real(wp), allocatable :: rgba(:,:), t(:), st(:,:)
        real(wp) :: tf, mu, radius(2), escape
        integer :: n, k, outcome, fate, counts(4), threads
        integer(int64) :: start, finish, rate
        character(len=500) :: text
        character(len=7) :: hex
        n = 0
        if (allocated(app%states)) n = size(app%states, 2)
        call app%cloud%clear()
        call app%cloud_dots%set_point_colors(app%color_by%currentIndex() == 1)
        if (app%color_by%currentIndex() /= 1) then
            call app%fate_legend%setText('')
            if (n > 0) call app%cloud%add([(real(k, wp), k = 1, n)], pos=app%length * app%states(1:3, :))
            return
        end if
        call app%fate_legend%setText('<small>Propagating the states...</small>')
        call QApplication_processEvents()
        tf = app%days%value() * day / time_unit()
        if (app%backward%isChecked()) tf = -tf
        mu = app%mu
        radius = [r_earth, r_moon] / app%length
        escape = app%escape%value()
        threads = 1
        !$ threads = omp_get_max_threads()
        allocate(rgba(4, n))
        counts = 0
        call system_clock(start, rate)
        !$omp parallel do default(shared) private(t, st, outcome, fate) schedule(dynamic, 8) reduction(+:counts)
        do k = 1, n
            call propagate(mu, app%states(:, k), tf, abs(tf), 1.0e-9_wp, radius, t, st, outcome, escape=escape)
            select case (outcome)
            case (IMPACT_1)
                fate = 1
            case (IMPACT_2)
                fate = 2
            case (ESCAPED)
                fate = 3
            case default
                fate = 4
            end select
            counts(fate) = counts(fate) + 1
            rgba(:, k) = [fate_colors(:, fate), 1.0_wp]
        end do
        !$omp end parallel do
        call system_clock(finish)
        if (n > 0) call app%cloud%add([(real(k, wp), k = 1, n)], pos=app%length * app%states(1:3, :), color=rgba)
        text = '<small>'
        do k = 1, 4
            write(hex, '(a,3z2.2)') '#', nint(255 * fate_colors(:, k))
            write(text, '(a,a,a,a,a,a,i0,a)') trim(text), '<span style="color:', hex, '">&#9679;</span> ', &
                trim(fate_names(k)), ': ', counts(k), '<br>'
        end do
        write(text, '(a,a,f0.2,a,i0,a)') trim(text), '(propagated in ', real(finish - start, wp) / real(rate, wp), &
            ' s, ', threads, merge(' threads)', ' thread) ', threads > 1)
        call app%fate_legend%setText(trim(text)//'</small>')
        if (app%fate_at_start) write(*, '(a,4i6,a,f0.3,a,i0,a)') 'fates (Earth, Moon, escape, neither):', counts, &
            ' in ', real(finish - start, wp) / real(rate, wp), ' s on ', threads, ' threads'
    end subroutine draw_cloud

    !> Propagate the state picked (the controls' time and direction) and draw
    !> it; the clock runs over it (in about 8 s) for the spacecraft.
    subroutine propagate_selected()
        real(wp), allocatable :: t(:), s(:,:)
        real(wp) :: tf, tu
        integer :: outcome
        character(len=600) :: text
        character(len=60) :: ended
        if (.not. app%has_selection) return
        tu = time_unit()
        tf = app%days%value() * day / tu
        if (app%backward%isChecked()) tf = -tf
        call propagate(app%mu, app%selected, tf, abs(tf) / 2000.0_wp, 1.0e-11_wp, [r_earth, r_moon] / app%length, &
                       t, s, outcome, escape=app%escape%value())
        call app%path%clear()
        call app%path%add(t * tu, pos=app%length * s(1:3, :))
        select case (outcome)
        case (PROPAGATED)
            ended = 'the whole time'
        case (IMPACT_1)
            ended = 'impact on the Earth'
        case (IMPACT_2)
            ended = 'impact on the Moon'
        case (ESCAPED)
            ended = 'escaped'
        case default
            ended = 'the step size got too small'
        end select
        write(text, '(a,3f9.5,a,3f9.5,a,f8.5,a,f0.2,a,a,a,i0,a,es8.1,a)') '<small><b>State</b> (nondimensional)<br>r = ', &
            app%selected(1:3), '<br>v = ', app%selected(4:6), '<br>C = ', jacobi(app%mu, app%selected), &
            '<br><b>Propagated</b> ', t(size(t)) * tu / day, ' days: ', trim(ended), '<br>', size(t), &
            ' steps, C drift ', jacobi(app%mu, s(:, size(t))) - jacobi(app%mu, app%selected), '</small>'
        call app%readout%setText(trim(text))
        if (app%click_near(1) < huge(1.0_wp)) write(*, '(a)') trim(text)
        ! the clock over the path, for the spacecraft (backwards: decreasing times)
        if (app%proxy%is_open()) then
            ! (always forwards in time: a backward propagation is played from its
            ! earliest time up to the state picked, its trace growing toward it)
            app%animation_start = min(0.0_wp, t(size(t)) * tu)
            call app%proxy%set_time_range(app%animation_start, max(0.0_wp, t(size(t)) * tu))
            call app%proxy%set_time_scale(abs(t(size(t)) * tu) / 8.0_wp)
            call app%proxy%set_time(app%animation_start)
        end if
    end subroutine propagate_selected

    !> Select cloud state k and propagate it.
    subroutine select_state(k)
        integer, intent(in) :: k
        if (.not. allocated(app%states)) return
        if (k < 1 .or. k > size(app%states, 2)) return
        app%selected = app%states(:, k)
        app%has_selection = .true.
        call propagate_selected()
    end subroutine select_state

    ! ==================================================================
    ! picking (the window's event handler)

    subroutine picker_press(self, window, x, y, button)
        class(Picker), intent(inout) :: self
        class(WindowProxy), intent(inout) :: window
        real(wp), intent(in) :: x, y
        integer, intent(in) :: button
        if (button /= MOUSE_LEFT) return
        self%pressed = [x, y]
        self%down = .true.
    end subroutine picker_press

    !> A click (released where pressed: not a drag of the camera) on a dot
    !> of the cloud: its state propagated (the dots' times are their numbers).
    subroutine picker_release(self, window, x, y, button)
        class(Picker), intent(inout) :: self
        class(WindowProxy), intent(inout) :: window
        real(wp), intent(in) :: x, y
        integer, intent(in) :: button
        type(PickHit), allocatable :: hits(:)
        if (button /= MOUSE_LEFT .or. .not. self%down) return
        self%down = .false.
        if (norm2([x, y] - self%pressed) > 0.01_wp) return
        call window%pick(x, y, hits, kinds=[PICK_MARKER], nearest=.true.)
        if (size(hits) == 0) return
        select type (a => hits(1)%artist)
        type is (MarkerArtist)
            if (associated(a, app%cloud_dots)) then
                call select_state(nint(hits(1)%time))
                if (app%click_near(1) < huge(1.0_wp)) write(*, '(a,i0)') 'picked state ', nint(hits(1)%time)
            end if
        end select
    end subroutine picker_release

    ! ==================================================================
    ! the window

    !> Open the view (after the main window is painted) and show the scene.
    subroutine open_view()
        if (.not. app%proxy%open(app%window%width(), app%window%height(), 'vkframes', &
                                 native_window=app%window%winId(), pixel_ratio=app%window%devicePixelRatio())) then
            write(*, '(a)') 'vkframes: could not open the view'
            return
        end if
        if (app%proxy%num_views() > 0) return                    ! (opened again, on a new native window)
        call app%proxy%set_scene(app%scene)
        call app%proxy%set_background_color([0.0_wp, 0.0_wp, 0.02_wp])
        call app%proxy%add_view(app%view)
        call app%proxy%set_camera([0.0_wp, -2.3_wp, 2.6_wp] * app%length, [0.0_wp, 0.0_wp, 0.0_wp], &
                                  [0.0_wp, 0.0_wp, 1.0_wp])     ! (from above, tilted: the plane and the cloud)
        call app%proxy%add_hud_text(app%hud_title)
        call app%proxy%add_hud_text(app%hud_help)
        call app%proxy%set_event_handler(app%picker)
        call app%proxy%set_zoom_speed(real(app%zoom_speed%value(), wp))
        call app%proxy%set_camera_throw(app%camera_throw%isChecked())
        call app%proxy%set_time_scale(0.0_wp)
        if (app%select_at_start > 0) call select_state(app%select_at_start)
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

    !> Render a frame and ask for the next one.
    subroutine on_update(data)
        class(*), intent(inout), optional :: data
        if (app%window%devicePixelRatio() /= app%proxy%pixel_ratio()) &
            call app%proxy%resize(nint(app%window%width() * app%window%devicePixelRatio()), &
                                  nint(app%window%height() * app%window%devicePixelRatio()), &
                                  pixel_ratio=app%window%devicePixelRatio())
        if (.not. app%proxy%frame()) return
        app%frame = app%frame + 1
        if (app%frame == 30 .and. app%click_near(1) < huge(1.0_wp)) call click_near()
        if (app%frame == 30 .and. app%go_to > 0) call go_to_body(app%go_to)
        if (allocated(app%capture) .and. app%frame == app%capture_frame) then
            if (app%proxy%capture(app%capture)) write(*, '(a)') 'captured '//app%capture
        end if
        call app%window%requestUpdate()
    end subroutine on_update

    !> --click-near: a click (mouse press and release, as Qt forwards them) on
    !> the cloud's dot nearest (x, y): through the picker, as the user's.
    subroutine click_near()
        real(wp) :: at(2), dpr
        integer :: k, px, py
        if (.not. allocated(app%states)) return
        if (size(app%states, 2) == 0) return
        k = minloc(norm2(app%states(1:2, :) - spread(app%click_near, 2, size(app%states, 2)), dim=1), dim=1)
        if (.not. app%proxy%screen_position(app%length * app%states(1:3, k), at)) return
        dpr = app%window%devicePixelRatio()
        px = nint(at(1) * app%window%width() * dpr)
        py = nint((1.0_wp - at(2)) * app%window%height() * dpr)
        call app%proxy%mouse(MOUSE_PRESS, px, py, 256, 1)
        call app%proxy%mouse(MOUSE_RELEASE, px, py, 0, 1)
        write(*, '(a,i0)') 'clicked state ', k
    end subroutine click_near

    !> New size: rebuild the swapchain (framebuffer pixels).
    subroutine on_resize(event, data)
        type(QResizeEvent), intent(in) :: event
        class(*), intent(inout), optional :: data
        real(wp) :: dpr
        if (.not. app%proxy%is_open()) return
        dpr = app%window%devicePixelRatio()
        call app%proxy%resize(nint(event%width() * dpr), nint(event%height() * dpr), pixel_ratio=dpr)
        call app%window%requestUpdate()
    end subroutine on_resize



    !> Forward a mouse event (MOUSE_MOVE, MOUSE_PRESS or MOUSE_RELEASE), in framebuffer pixels.
    subroutine forward_mouse(kind, event)
        integer, intent(in) :: kind
        type(QMouseEvent), intent(in) :: event
        call qt_forward_mouse(app%proxy, app%window, kind, event)
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
        call qt_forward_wheel(app%proxy, event)
    end subroutine on_wheel

    !> Keys: e and m fly to the Earth and the Moon (go_to_body); the space bar
    !> (reset the view) to vkframes; other keys are the GUI's.
    subroutine on_key_press(event, data)
        type(QKeyEvent), intent(in) :: event
        class(*), intent(inout), optional :: data
        select case (event%key())
        case (Qt_Key_A + 4)                                     ! e
            call go_to_body(1)
        case (Qt_Key_A + 12)                                    ! m
            call go_to_body(2)
        case (Qt_Key_Space)
            call app%proxy%key(.true., 32, 32, 0)
        case default
            return
        end select
        call event%accept()
    end subroutine on_key_press

    subroutine on_key_release(event, data)
        type(QKeyEvent), intent(in) :: event
        class(*), intent(inout), optional :: data
        if (event%key() /= Qt_Key_Space) return
        call app%proxy%key(.false., 32, 32, 0)
        call event%accept()
    end subroutine on_key_release

    !> Fly the camera to body k (1 the Earth, 2 the Moon): centered on it
    !> (the trackball then turns about it), looking from the same direction,
    !> close enough to see its neighbourhood (the Moon's: L1 and L2).
    subroutine go_to_body(k)
        integer, intent(in) :: k
        real(wp), parameter :: distance(2) = [0.8_wp, 0.55_wp]  !! from the body (of the Earth-Moon distance)
        real(wp) :: eye(3), center(3), up(3), d(3), body(3)
        if (.not. app%proxy%is_open()) return
        if (k == 1) then
            body = app%earth%position()
        else
            body = app%moon%position()
        end if
        call app%proxy%get_camera(eye, center, up, in_root=.true.)
        d = eye - center
        if (norm2(d) <= 0.0_wp) d = [0.0_wp, -1.0_wp, 1.0_wp]
        d = d / norm2(d)
        call app%proxy%set_camera(body + distance(k) * app%length * d, body, up, duration=1.2_wp, in_root=.true.)
    end subroutine go_to_body

    ! ==================================================================
    ! controls (GUI thread)

    !> The system changed (distance or mu): the bodies, the cloud and the path again.
    subroutine on_distance(value, data)
        real(c_double), intent(in) :: value
        class(*), intent(inout), optional :: data
        app%length = value
        if (app%building) return
        call make_cloud()
        call propagate_selected()
    end subroutine on_distance

    subroutine on_mass_ratio(value, data)
        real(c_double), intent(in) :: value
        class(*), intent(inout), optional :: data
        app%mu = value
        if (app%building) return
        call make_cloud()
        call propagate_selected()
    end subroutine on_mass_ratio

    !> The cloud changed (its constant, size, planar or not, seed): a new cloud
    !> (the state picked stays, and its path).
    subroutine on_cloud_value(value, data)
        real(c_double), intent(in) :: value
        class(*), intent(inout), optional :: data
        if (.not. app%building) call make_cloud()
    end subroutine on_cloud_value

    subroutine on_cloud_count(value, data)
        integer, intent(in) :: value
        class(*), intent(inout), optional :: data
        if (.not. app%building) call make_cloud()
    end subroutine on_cloud_count

    subroutine on_cloud_toggle(checked, data)
        logical, intent(in) :: checked
        class(*), intent(inout), optional :: data
        if (.not. app%building) call make_cloud()
    end subroutine on_cloud_toggle

    subroutine on_marker_size(value, data)
        real(c_double), intent(in) :: value
        class(*), intent(inout), optional :: data
        call app%cloud_dots%set_marker_size(real(value, wp))
    end subroutine on_marker_size

    !> The propagation changed (its time or direction): propagated again.
    !> (the fates too, if the cloud is colored by them)
    subroutine on_days(value, data)
        real(c_double), intent(in) :: value
        class(*), intent(inout), optional :: data
        if (app%building) return
        call propagate_selected()
        if (app%color_by%currentIndex() == 1) call draw_cloud()
    end subroutine on_days

    subroutine on_backward(checked, data)
        logical, intent(in) :: checked
        class(*), intent(inout), optional :: data
        if (app%building) return
        call propagate_selected()
        if (app%color_by%currentIndex() == 1) call draw_cloud()
    end subroutine on_backward

    !> Color by: all blue, or by fate (each state propagated).
    subroutine on_color_by(index, data)
        integer, intent(in) :: index
        class(*), intent(inout), optional :: data
        if (.not. app%building) call draw_cloud()
    end subroutine on_color_by

    !> Animate: the path traced up to the spacecraft, flying along it.
    subroutine on_animate(checked, data)
        logical, intent(in) :: checked
        class(*), intent(inout), optional :: data
        call app%path_curve%set_trace_mode(checked)
        call app%path_end%set_trace_mode(checked)
        call app%craft%show_contents(checked)
        if (app%proxy%is_open()) call app%proxy%set_time(app%animation_start)
    end subroutine on_animate

    subroutine on_replay(data)
        class(*), intent(inout), optional :: data
        if (app%proxy%is_open()) call app%proxy%set_time(app%animation_start)
    end subroutine on_replay

    subroutine on_clear(data)
        class(*), intent(inout), optional :: data
        app%has_selection = .false.
        call app%path%clear()
        call app%readout%setText('<small>Click a dot to propagate its state.</small>')
    end subroutine on_clear

    subroutine on_show_lagrange(checked, data)
        logical, intent(in) :: checked
        class(*), intent(inout), optional :: data
        integer :: k
        call app%lagrange_frame%show_contents(checked)
        do k = 1, 5
            call app%lagrange_labels(k)%show_name_label(checked)
        end do
    end subroutine on_show_lagrange

    !> The bodies drawn larger (only drawn: impacts are at their real radii).
    subroutine on_body_size(value, data)
        real(c_double), intent(in) :: value
        class(*), intent(inout), optional :: data
        call app%earth%set_radius(value * r_earth)
        call app%moon%set_radius(value * r_moon)
    end subroutine on_body_size

    !> The mouse's zoom speed (per wheel or trackpad step, and per right drag).
    subroutine on_zoom_speed(value, data)
        real(c_double), intent(in) :: value
        class(*), intent(inout), optional :: data
        call app%proxy%set_zoom_speed(real(value, wp))
    end subroutine on_zoom_speed

    !> The camera keeps moving after a drag released while moving, or stops.
    subroutine on_camera_throw(checked, data)
        logical, intent(in) :: checked
        class(*), intent(inout), optional :: data
        call app%proxy%set_camera_throw(checked)
    end subroutine on_camera_throw

    subroutine on_go_earth(data)
        class(*), intent(inout), optional :: data
        call go_to_body(1)
    end subroutine on_go_earth

    subroutine on_go_moon(data)
        class(*), intent(inout), optional :: data
        call go_to_body(2)
    end subroutine on_go_moon

    subroutine on_reset_view(data)
        class(*), intent(inout), optional :: data
        call app%view%reset_view()
    end subroutine on_reset_view

    subroutine on_quit(data)
        class(*), intent(inout), optional :: data
        call app%main%close()
    end subroutine on_quit

    !> Close the view (waiting for the GPU) before Qt destroys its native window.
    subroutine on_close(event, data)
        type(QCloseEvent), intent(in) :: event
        class(*), intent(inout), optional :: data
        call app%proxy%close()
    end subroutine on_close

    !> After the main window is first painted: the slow parts of the startup
    !> (the label font, the Vulkan device) while it shows a message.
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

    !> A spin box for a real value.
    function real_box(low, high, value, decimals, step, suffix, slot) result(box)
        real(wp), intent(in) :: low, high, value, step
        integer, intent(in) :: decimals
        character(len=*), intent(in) :: suffix
        procedure(on_distance) :: slot
        type(QDoubleSpinBox) :: box
        box = QDoubleSpinBox()
        call box%setDecimals(decimals)
        call box%setRange(real(low, c_double), real(high, c_double))
        call box%setSingleStep(real(step, c_double))
        call box%setValue(real(value, c_double))
        if (len(suffix) > 0) call box%setSuffix(suffix)
        call box%valueChanged%connect(slot)
    end function real_box

    !> The main window: the view in the center, and controls in a dock.
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

        app%main = QMainWindow()
        call app%main%setWindowTitle('vkframes: Earth-Moon CR3BP')
        call app%main%closeEvent%connect(on_close)
        bar = app%main%menuBar()
        menu = bar%addMenu('&File')
        action = QAction('&Quit', app%main)
        call action%setShortcut('Ctrl+Q')
        call action%triggered%connect(on_quit)
        call menu%addAction(action)

        ! the view's window
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
        call watch_surface(app%events, app%proxy)               ! (closed if Qt destroys the native window)
        app%stack = QStackedWidget()
        app%startup_message = QLabel('Starting...')
        call app%startup_message%setAlignment(Qt_AlignCenter)
        call app%startup_message%setStyleSheet('background-color: black; color: #cccccc; font-size: 16pt;')
        call app%stack%addWidget(app%startup_message)
        call app%stack%addWidget(app%container)
        call app%main%setCentralWidget(app%stack)

        panel = QWidget()
        column = QVBoxLayout(panel)

        ! the system
        group = QGroupBox('System')
        form = QFormLayout(group)
        app%distance = real_box(1.0e4_wp, 1.0e7_wp, app%length, 0, 10000.0_wp, ' km', on_distance)
        call form%addRow('Earth-Moon distance', app%distance)
        app%mass_ratio = real_box(1.0e-6_wp, 0.5_wp, app%mu, 6, 0.001_wp, '', on_mass_ratio)
        call form%addRow('Mass ratio (mu)', app%mass_ratio)
        call column%addWidget(group)

        ! the cloud
        group = QGroupBox('Point cloud')
        form = QFormLayout(group)
        app%jacobi_c = real_box(2.5_wp, 3.5_wp, 3.15_wp, 4, 0.005_wp, '', on_cloud_value)
        call form%addRow('Jacobi constant', app%jacobi_c)
        app%lagrange_c = QLabel('')
        call form%addRow(app%lagrange_c)
        app%n_points = QSpinBox()
        call app%n_points%setRange(10, 100000)
        call app%n_points%setSingleStep(500)
        call app%n_points%setValue(4000)
        call app%n_points%valueChanged%connect(on_cloud_count)
        call form%addRow('States', app%n_points)
        app%seed = QSpinBox()
        call app%seed%setRange(1, 1000000)
        call app%seed%setValue(1)
        call app%seed%valueChanged%connect(on_cloud_count)
        call form%addRow('Random seed', app%seed)
        app%planar = QCheckBox('Planar (z = 0, vz = 0)')
        call app%planar%setChecked(.true.)
        call app%planar%toggled%connect(on_cloud_toggle)
        call form%addRow('', app%planar)
        app%marker_size = real_box(1.0_wp, 20.0_wp, 4.0_wp, 1, 1.0_wp, ' px', on_marker_size)
        call form%addRow('Dot size', app%marker_size)
        app%cloud_info = QLabel('')
        call form%addRow(app%cloud_info)
        app%color_by = QComboBox()
        call app%color_by%addItems([character(len=34) :: 'Uniform (blue)', 'Fate after the propagation time'])
        call app%color_by%currentIndexChanged%connect(on_color_by)
        call form%addRow('Color by', app%color_by)
        app%fate_legend = QLabel('')
        call form%addRow(app%fate_legend)
        call column%addWidget(group)

        ! the propagation
        group = QGroupBox('Propagation')
        form = QFormLayout(group)
        app%days = real_box(0.1_wp, 3650.0_wp, 30.0_wp, 1, 5.0_wp, ' days', on_days)
        call form%addRow('Time', app%days)
        app%escape = real_box(1.2_wp, 50.0_wp, 3.0_wp, 1, 0.5_wp, ' x distance', on_days)
        call app%escape%setToolTip('Escaped: farther than this from the barycenter (in Earth-Moon distances)')
        call form%addRow('Escape beyond', app%escape)
        app%backward = QCheckBox('Backwards in time')
        call app%backward%toggled%connect(on_backward)
        call form%addRow('', app%backward)
        app%animate = QCheckBox('Animate (a spacecraft along it)')
        call app%animate%toggled%connect(on_animate)
        call form%addRow('', app%animate)
        button = QPushButton('Replay')
        call button%clicked%connect(on_replay)
        call form%addRow('', button)
        button = QPushButton('Clear')
        call button%clicked%connect(on_clear)
        call form%addRow('', button)
        app%readout = QLabel('<small>Click a dot to propagate its state.</small>')
        call app%readout%setWordWrap(.true.)
        call form%addRow(app%readout)
        call column%addWidget(group)

        ! the display
        group = QGroupBox('Display')
        form = QFormLayout(group)
        app%show_lagrange = QCheckBox('Lagrange points')
        call app%show_lagrange%setChecked(.true.)
        call app%show_lagrange%toggled%connect(on_show_lagrange)
        call form%addRow('', app%show_lagrange)
        app%body_size = real_box(1.0_wp, 20.0_wp, 3.0_wp, 1, 1.0_wp, ' x', on_body_size)
        call app%body_size%setToolTip('The Earth and the Moon drawn larger (impacts stay at their real radii)')
        call form%addRow('Body size', app%body_size)
        app%zoom_speed = real_box(0.05_wp, 3.0_wp, 0.5_wp, 2, 0.05_wp, ' x', on_zoom_speed)
        call app%zoom_speed%setToolTip('How fast the wheel, a trackpad and a right drag zoom (1: VSG''s)')
        call form%addRow('Zoom speed', app%zoom_speed)
        app%camera_throw = QCheckBox('Keep turning after a drag')
        call app%camera_throw%setToolTip('The camera keeps moving if the mouse is moving when the button is released')
        call app%camera_throw%toggled%connect(on_camera_throw)
        call form%addRow('', app%camera_throw)
        button = QPushButton('Go to the Earth (e)')
        call button%clicked%connect(on_go_earth)
        call form%addRow('', button)
        button = QPushButton('Go to the Moon (m)')
        call button%clicked%connect(on_go_moon)
        call form%addRow('', button)
        button = QPushButton('Reset view (space)')
        call button%clicked%connect(on_reset_view)
        call form%addRow('', button)
        call column%addWidget(group)

        call column%addWidget(QLabel('<small>Click a dot: propagate it<br>Left drag: rotate<br>' // &
                                     'Right drag: zoom<br>Middle drag: pan<br>Wheel: zoom<br>e / m: go to the Earth / Moon<br>' // &
                                     'Space: reset the view</small>'))
        call column%addStretch()
        scroll = QScrollArea()
        call scroll%setWidgetResizable(.true.)
        call scroll%setWidget(panel)
        dock = QDockWidget('Controls', app%main)
        call dock%setWidget(scroll)
        call app%main%addDockWidget(Qt_LeftDockWidgetArea, dock)
    end subroutine build_ui

end module cr3bp_demo

!> The Earth-Moon CR3BP in a qt-fortran GUI (see the module documentation).
program vkframes_cr3bp_demo

    use qt
    use vkframes
    use cr3bp_demo
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
        case ('--select')
            call get_command_argument(i + 1, arg)
            read(arg, *) app%select_at_start
            i = i + 1
        case ('--click-near')
            call get_command_argument(i + 1, arg)
            read(arg, *) app%click_near(1)
            call get_command_argument(i + 2, arg)
            read(arg, *) app%click_near(2)
            i = i + 2
        case ('--color-by')
            call get_command_argument(i + 1, arg)
            app%fate_at_start = arg == 'fate'
            i = i + 1
        case ('--go-to')
            call get_command_argument(i + 1, arg)
            app%go_to = merge(1, 2, arg == 'earth')
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
    if (app%fate_at_start) call app%color_by%setCurrentIndex(1)
    app%building = .false.
    call make_cloud()
    if (app%quit_after > 0) call QTimer_singleShot(int(1000 * app%quit_after), on_quit)
    call app%main%resize(1300, 850)
    call app%main%show()
    call QTimer_singleShot(0, on_startup)
    ierr = qapp%exec()

end program vkframes_cr3bp_demo
