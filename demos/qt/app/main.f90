!> vkframes in a qt-fortran GUI: a spacecraft orbiting the Earth, rendered by
!> VulkanSceneGraph into QWindows: one embedded in a QMainWindow, next to a dock
!> of qt-fortran controls, and more on demand ("New window"), each showing the
!> same scene with its own view.
!>
!> The scene uses vkframes' API: the Earth is a Sphere (the WGS84 ellipsoid,
!> textured), the spacecraft a Model (or a small Sphere without the data) that
!> follows a Trajectory (positions and attitudes over one period; LOOP or LIMIT
!> from the dock), a child of the Earth. A FrameManager holds the scene. Each
!> window has a WindowProxy showing it, with three Views (the Earth, the
!> spacecraft body-fixed, and from the spacecraft toward the Earth; chosen per
!> window). The main window's simulation time (time scale, pause and reset from
!> the dock) drives the others, whose clocks are synchronized to it. The orbit
!> line is still a low-level line node (drawn from the trajectory's positions,
!> shared by all windows) until trajectory artists come.
!>
!> How the pieces fit:
!>
!> * The host owns plain QWindows. On a window's first expose, its WindowProxy
!>   opens on the window's native handle (`winId()`): VSG renders into that
!>   window and leaves its events to Qt. All windows share one Vulkan device, so
!>   the scene's GPU data exist once.
!> * A QtfEventFilter per window forwards its events to its WindowProxy:
!>   resizes, and mouse, wheel and keys for the trackball, converted here to
!>   VSG's conventions (framebuffer pixels: logical * devicePixelRatio).
!> * Everything runs on the GUI thread (Vulkan has no current context, so there
!>   is no render thread and none of OpenGL's macOS workarounds): each window's
!>   `updateRequest` renders a frame (WindowProxy%frame: time, frames, draw)
!>   and asks for the next one, so frames follow the display's refresh.
!>
!> Run with `pixi run -e qt qt-demo` (from the vkframes root). Options:
!> `--data dir` OpenFrames' Demos folder, for the Earth texture and Hubble model
!> (default: $OPENFRAMES_DATA); `--capture file` saves a frame of each window as
!> an image (file, then file_2, ... for the other windows; `--capture-frame n`,
!> default 120); `--quit-after seconds` closes the demo; `--test-resize`
!> resizes the main window after 2 s; `--view n` starts the main window in view
!> n (1 Earth, 2 spacecraft, 3 spacecraft -> Earth); `--windows n` opens n
!> windows at startup (the others start in the next views).
module vkdemo

    use qt
    use vkframes
    use vkf_layer, only: vkf_line_create, line_set_points, line_set_style, vkf_viewer_add_node, vkf_node_delete
    use iso_c_binding
    implicit none

    real(wp), parameter :: mu = 398600.4418_wp                  !! Earth's gravitational parameter (km^3/s^2)
    real(wp), parameter :: re = 6378.137_wp                     !! WGS84 equatorial radius (km)
    real(wp), parameter :: flattening = 1.0_wp / 298.257223563_wp
    real(wp), parameter :: sma = 9000.0_wp, ecc = 0.2_wp, inc = 35.0_wp * pi / 180.0_wp
    real(wp), parameter :: model_size = 800.0_wp                 !! spacecraft bounding radius (km, enlarged to be seen)
    integer, parameter :: n_orbit = 721
    integer, parameter :: max_windows = 4
    real(c_float), parameter :: orbit_color(4) = [1.0_c_float, 0.85_c_float, 0.2_c_float, 1.0_c_float]
    character(len=19), parameter :: view_names(3) = [character(len=19) :: 'Earth', 'Spacecraft', 'Spacecraft -> Earth']

    !> One window showing the scene: a QWindow (in the main window, or in a
    !> top-level widget of its own) with its WindowProxy and views.
    type :: view_window_t
        integer :: index = 0                                   !! 1: the main window's
        logical :: used = .false.
        type(QWidget) :: top                                   !! the top-level widget (windows 2, 3, ...)
        type(QWindow) :: window
        type(QWidget) :: container
        type(QtfEventFilter) :: events
        type(QComboBox) :: view_choice
        type(WindowProxy) :: proxy
        type(View) :: views(3)                                 !! Earth, spacecraft, spacecraft -> Earth
        integer :: frame = 0
    end type view_window_t

    !> The GUI and the scene.
    type :: app_t
        type(QMainWindow) :: main
        type(QDoubleSpinBox) :: time_scale
        type(QCheckBox) :: pause_time
        type(QDoubleSpinBox) :: line_width
        type(QComboBox) :: follow_mode
        type(QLabel) :: time_label
        type(view_window_t) :: windows(max_windows)
        ! the scene
        type(Trajectory) :: orbit                              !! one period of positions and attitudes
        type(Sphere) :: earth
        class(ReferenceFrame), allocatable :: spacecraft      !! a Model, or a Sphere without the data
        type(FrameManager) :: scene
        type(c_ptr) :: orbit_line = c_null_ptr                 !! low-level line node (until artists), in every window
        ! options
        character(len=:), allocatable :: data_dir, capture
        integer :: capture_frame = 120
        integer :: start_view = 1
        integer :: start_windows = 1
        real(wp) :: quit_after = -1.0_wp
        logical :: test_resize = .false.
    end type app_t

    !> The application (module-level, so that windows created later from slots
    !> are targets too: frames, views and windows keep pointers to each other).
    type(app_t), target, save :: app

contains

    ! ==================================================================
    ! the scene

    !> Position on the orbit at time t (Kepler's equation by Newton iteration), km.
    pure function orbit_position(t) result(r)
        real(wp), intent(in) :: t
        real(wp) :: r(3)
        real(wp) :: m, ea, x, y
        integer :: k
        m = sqrt(mu / sma**3) * t
        ea = m
        do k = 1, 20
            ea = ea - (ea - ecc * sin(ea) - m) / (1.0_wp - ecc * cos(ea))
        end do
        x = sma * (cos(ea) - ecc)
        y = sma * sqrt(1.0_wp - ecc**2) * sin(ea)
        r = [x, y * cos(inc), y * sin(inc)]
    end function orbit_position

    !> The spacecraft's attitude at orbit position r: its x axis radial (from
    !> the Earth), z along the orbit normal.
    pure function orbit_attitude(r) result(q)
        real(wp), intent(in) :: r(3)
        real(wp) :: q(4)
        real(wp) :: x, y
        x = r(1)                                   ! in the orbit plane (before the inclination)
        y = r(2) * cos(inc) + r(3) * sin(inc)
        q = quat_multiply(quat_from_axis_angle([1.0_wp, 0.0_wp, 0.0_wp], inc), &
                          quat_from_axis_angle([0.0_wp, 0.0_wp, 1.0_wp], atan2(y, x)))
    end function orbit_attitude

    !> The scene: the orbit trajectory (one period), the Earth, the spacecraft
    !> following the orbit, and the orbit line.
    subroutine build_model()
        real(wp) :: period, t, r(3), center(3), radius
        real(wp), pointer :: xyz(:,:)
        logical :: ok
        integer :: k

        app%orbit = Trajectory()
        call app%orbit%reserve(n_orbit)
        period = 2.0_wp * pi * sqrt(sma**3 / mu)
        do k = 1, n_orbit
            t = period * real(k - 1, wp) / real(n_orbit - 1, wp)
            r = orbit_position(t)
            call app%orbit%add(t, pos=r, att=orbit_attitude(r))
        end do

        app%earth = Sphere('Earth', radius=re, color=[0.3_wp, 0.5_wp, 1.0_wp])
        call app%earth%set_sphere_scale([1.0_wp, 1.0_wp, 1.0_wp - flattening])
        if (len(app%data_dir) > 0) call app%earth%set_texture_map(app%data_dir//'/Images/land_shallow_topo_2048.jpg')

        ! the spacecraft: OpenFrames' Hubble model, or a small sphere without the data
        ok = .false.
        if (len(app%data_dir) > 0) then
            allocate(app%spacecraft, source=Model('Spacecraft', color=[1.0_wp, 0.85_wp, 0.2_wp]))
            select type (sc => app%spacecraft)
            type is (Model)
                ok = sc%set_model(app%data_dir//'/Models/Hubble.3ds')
                if (ok) then
                    call sc%model_bound(center, radius)
                    call sc%set_model_pivot(center)
                    call sc%set_model_scale([1.0_wp, 1.0_wp, 1.0_wp] * model_size / radius)
                end if
            end select
            if (.not. ok) deallocate(app%spacecraft)
        end if
        if (.not. ok) allocate(app%spacecraft, source=Sphere('Spacecraft', radius=model_size, &
                                                             color=[1.0_wp, 0.3_wp, 0.2_wp]))
        call app%spacecraft%follow(app%orbit)                 ! position and attitude, FOLLOW_LOOP
        if (.not. app%earth%add_child(app%spacecraft)) write(*, '(a)') 'vkframes: add_child failed'
        call app%scene%set_frame(app%earth)
        write(*, '(a)', advance='no') app%earth%frame_string()

        ! the orbit line from the trajectory's positions (not copied); shown in every window
        call app%orbit%positions(xyz)
        app%orbit_line = vkf_line_create(int(app%orbit%num_positions(), c_int))
        if (line_set_style(app%orbit_line, orbit_color, 2.0_c_float) /= 0 .or. line_set_points(app%orbit_line, xyz) /= 0) &
            write(*, '(a)') 'vkframes: could not make the orbit line'
    end subroutine build_model

    ! ==================================================================
    ! windows

    !> Set up window `w` (index k): its QWindow, event forwarding and views.
    !> Window 1 goes in the main window; the others in top-level widgets.
    subroutine build_window(k, start_view)
        integer, intent(in) :: k, start_view
        type(view_window_t), pointer :: w
        type(QVBoxLayout) :: layout
        character(len=12) :: title
        w => app%windows(k)
        w%index = k
        w%used = .true.
        w%frame = 0

        ! views: the Earth, the spacecraft (body-fixed), and from the spacecraft
        ! toward the Earth (inertial axes, a little behind the spacecraft)
        w%views(1) = View(app%earth, app%earth)
        w%views(2) = View(app%earth, app%spacecraft)
        w%views(3) = View(app%earth, app%spacecraft, lookat=app%earth, frame_type=ABSOLUTE_FRAME)
        call w%views(3)%set_default_view_distance(1.5_wp * model_size)

        ! default surface type, as vsgQt uses: VSG gives the native view its own
        ! CAMetalLayer on macOS (with QSurface_VulkanSurface, Qt manages the
        ! layer itself and MoltenVK finds no CAMetalLayer)
        w%window = QWindow()
        w%container = QWidget_createWindowContainer(w%window)
        call w%container%setMinimumSize(320, 240)
        w%view_choice = QComboBox()
        call w%view_choice%addItems(view_names)
        call w%view_choice%setCurrentIndex(modulo(start_view - 1, 3))
        call w%view_choice%currentIndexChanged%connect(on_view, w)

        w%events = QtfEventFilter(w%window)
        call w%events%exposeEvent%connect(on_expose, w)
        call w%events%updateRequest%connect(on_update, w)
        call w%events%resizeEvent%connect(on_resize, w)
        call w%events%mousePressEvent%connect(on_press, w)
        call w%events%mouseReleaseEvent%connect(on_release, w)
        call w%events%mouseMoveEvent%connect(on_move, w)
        call w%events%wheelEvent%connect(on_wheel, w)
        call w%events%keyPressEvent%connect(on_key_press, w)
        call w%events%keyReleaseEvent%connect(on_key_release, w)

        if (k > 1) then
            w%top = QWidget()
            write(title, '(a,i0)') 'vkframes ', k
            call w%top%setWindowTitle(trim(title))
            layout = QVBoxLayout(w%top)
            call layout%addWidget(w%view_choice)
            call layout%addWidget(w%container, 1)
            call w%top%closeEvent%connect(on_close_window, w)
            call w%top%resize(640, 480)
            call w%top%show()
        end if
    end subroutine build_window

    !> Open window w's view (on its first expose) and show the scene. Windows
    !> after the first follow the main window's clock.
    subroutine open_view(w)
        type(view_window_t), intent(inout), target :: w
        type(SimulationClock), pointer :: clock, master
        integer :: k
        if (.not. w%proxy%open(w%window%width(), w%window%height(), 'vkframes', &
                               native_window=w%window%winId(), pixel_ratio=w%window%devicePixelRatio())) then
            write(*, '(a)') 'vkframes: could not open the view'
            return
        end if
        call w%proxy%set_scene(app%scene)
        call w%proxy%set_background_color([0.0_wp, 0.0_wp, 0.0_wp])
        do k = 1, size(w%views)
            call w%proxy%add_view(w%views(k))
        end do
        call w%proxy%select_view(w%view_choice%currentIndex() + 1)
        if (vkf_viewer_add_node(w%proxy%viewer(), app%orbit_line) /= 0) write(*, '(a)') 'vkframes: could not add the orbit line'
        if (w%index == 1) then
            call w%proxy%set_time_scale(app%time_scale%value())
            call w%proxy%pause_time(app%pause_time%isChecked())
            call w%proxy%set_time(0.0_wp)
        else
            clock => w%proxy%clock()
            master => app%windows(1)%proxy%clock()
            if (.not. clock%synchronize(master)) write(*, '(a)') 'vkframes: could not synchronize the clocks'
        end if
    end subroutine open_view

    !> Close window w's view (waiting for the GPU) before Qt destroys its native window.
    subroutine close_view(w)
        type(view_window_t), intent(inout) :: w
        type(SimulationClock), pointer :: clock
        if (w%proxy%is_open() .and. w%index > 1) then
            clock => w%proxy%clock()
            if (clock%synchronize(null())) continue
        end if
        call w%proxy%close()
    end subroutine close_view

    ! ==================================================================
    ! window events (GUI thread; `data` is the view_window_t)

    !> First expose: open the view, then start the window's frame loop.
    subroutine on_expose(data)
        class(*), intent(inout), optional :: data
        select type (w => data)
        type is (view_window_t)
            if (w%proxy%is_open() .or. .not. w%window%isExposed()) return
            if (w%index > 1 .and. .not. app%windows(1)%proxy%is_open()) return   ! the main window opens first
            call open_view(app%windows(w%index))
            if (w%proxy%is_open()) call w%window%requestUpdate()
        end select
    end subroutine on_expose

    !> Render a frame (time, frames, draw) and ask for the next one.
    subroutine on_update(data)
        class(*), intent(inout), optional :: data
        character(len=40) :: text
        character(len=:), allocatable :: file
        integer :: dot
        select type (w => data)
        type is (view_window_t)
            if (.not. w%proxy%frame()) return
            w%frame = w%frame + 1
            if (w%index == 1) then
                write(text, '(a,f10.1,a)') 'Simulation time: ', w%proxy%time(), ' s'
                call app%time_label%setText(trim(text))
            end if
            if (allocated(app%capture) .and. w%frame == app%capture_frame) then
                file = app%capture
                if (w%index > 1) then            ! file_2.png, ...
                    dot = index(file, '.', back=.true.)
                    if (dot == 0) dot = len(file) + 1
                    write(text, '(a,i0)') '_', w%index
                    file = file(1:dot-1)//trim(text)//file(dot:)
                end if
                if (w%proxy%capture(file)) write(*, '(a)') 'captured '//file
            end if
            call w%window%requestUpdate()
        end select
    end subroutine on_update

    !> New size: rebuild the swapchain (framebuffer pixels).
    subroutine on_resize(event, data)
        type(QResizeEvent), intent(in) :: event
        class(*), intent(inout), optional :: data
        real(wp) :: dpr
        select type (w => data)
        type is (view_window_t)
            if (.not. w%proxy%is_open()) return
            dpr = w%window%devicePixelRatio()
            call w%proxy%resize(nint(event%width() * dpr), nint(event%height() * dpr))
            call w%window%requestUpdate()
        end select
    end subroutine on_resize

    !> VSG's mouse button number: 1 left, 2 middle, 3 right (0: other).
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

    !> VSG's button mask (BUTTON_MASK_1/2/3 = 256/512/1024) for Qt's held buttons.
    integer function vsg_mask(qt_buttons)
        integer, intent(in) :: qt_buttons
        vsg_mask = 0
        if (iand(qt_buttons, Qt_LeftButton) /= 0) vsg_mask = ior(vsg_mask, 256)
        if (iand(qt_buttons, Qt_MiddleButton) /= 0) vsg_mask = ior(vsg_mask, 512)
        if (iand(qt_buttons, Qt_RightButton) /= 0) vsg_mask = ior(vsg_mask, 1024)
    end function vsg_mask

    !> Forward a mouse event (MOUSE_MOVE, MOUSE_PRESS or MOUSE_RELEASE), in framebuffer pixels.
    subroutine forward_mouse(w, kind, event)
        type(view_window_t), intent(inout) :: w
        integer, intent(in) :: kind
        type(QMouseEvent), intent(in) :: event
        real(wp) :: dpr
        dpr = w%window%devicePixelRatio()
        call w%proxy%mouse(kind, nint(event%x() * dpr), nint(event%y() * dpr), vsg_mask(event%buttons()), &
                           vsg_button(event%button()))
        call event%accept()
    end subroutine forward_mouse

    subroutine on_move(event, data)
        type(QMouseEvent), intent(in) :: event
        class(*), intent(inout), optional :: data
        select type (w => data)
        type is (view_window_t)
            call forward_mouse(w, MOUSE_MOVE, event)
        end select
    end subroutine on_move

    subroutine on_press(event, data)
        type(QMouseEvent), intent(in) :: event
        class(*), intent(inout), optional :: data
        select type (w => data)
        type is (view_window_t)
            call forward_mouse(w, MOUSE_PRESS, event)
        end select
    end subroutine on_press

    subroutine on_release(event, data)
        type(QMouseEvent), intent(in) :: event
        class(*), intent(inout), optional :: data
        select type (w => data)
        type is (view_window_t)
            call forward_mouse(w, MOUSE_RELEASE, event)
        end select
    end subroutine on_release

    subroutine on_wheel(event, data)
        type(QWheelEvent), intent(in) :: event
        class(*), intent(inout), optional :: data
        select type (w => data)
        type is (view_window_t)
            if (event%angleDeltaY() == 0) return
            call w%proxy%scroll(merge(1.0_wp, -1.0_wp, event%angleDeltaY() > 0))
            call event%accept()
        end select
    end subroutine on_wheel

    !> VSG key symbol for a Qt key: ASCII for printable keys (lower case for the
    !> base symbol), X11-style codes for the others VSG's trackball uses; 0 if
    !> unmapped.
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
        if (key >= Qt_Key_A .and. key <= Qt_Key_Z .and. iand(modifier, 1) /= 0) modified_symbol = key   ! upper case
    end subroutine vsg_key

    subroutine forward_key(w, pressed, event)
        type(view_window_t), intent(inout) :: w
        logical, intent(in) :: pressed
        type(QKeyEvent), intent(in) :: event
        integer :: symbol, modified_symbol, modifier
        call vsg_key(event, symbol, modified_symbol, modifier)
        if (symbol == 0) return
        call w%proxy%key(pressed, symbol, modified_symbol, modifier)
        call event%accept()
    end subroutine forward_key

    subroutine on_key_press(event, data)
        type(QKeyEvent), intent(in) :: event
        class(*), intent(inout), optional :: data
        select type (w => data)
        type is (view_window_t)
            call forward_key(w, .true., event)
        end select
    end subroutine on_key_press

    subroutine on_key_release(event, data)
        type(QKeyEvent), intent(in) :: event
        class(*), intent(inout), optional :: data
        select type (w => data)
        type is (view_window_t)
            call forward_key(w, .false., event)
        end select
    end subroutine on_key_release

    !> Show the chosen view in this window (each view keeps its own camera).
    subroutine on_view(index, data)
        integer, intent(in) :: index
        class(*), intent(inout), optional :: data
        select type (w => data)
        type is (view_window_t)
            call w%proxy%select_view(index + 1)
        end select
    end subroutine on_view

    !> A window other than the main one closes.
    subroutine on_close_window(event, data)
        type(QCloseEvent), intent(in) :: event
        class(*), intent(inout), optional :: data
        select type (w => data)
        type is (view_window_t)
            call close_view(w)
            w%used = .false.
        end select
    end subroutine on_close_window

    ! ==================================================================
    ! controls and lifetime (GUI thread; they act on the main window, whose
    ! clock the others follow)

    subroutine on_time_scale(value, data)
        real(c_double), intent(in) :: value
        class(*), intent(inout), optional :: data
        call app%windows(1)%proxy%set_time_scale(value)
    end subroutine on_time_scale

    subroutine on_pause_time(checked, data)
        logical, intent(in) :: checked
        class(*), intent(inout), optional :: data
        call app%windows(1)%proxy%pause_time(checked)
    end subroutine on_pause_time

    subroutine on_reset_time(data)
        class(*), intent(inout), optional :: data
        call app%windows(1)%proxy%set_time(0.0_wp)
    end subroutine on_reset_time

    !> Put the main window's current view back at its default position.
    subroutine on_reset_view(data)
        class(*), intent(inout), optional :: data
        type(View), pointer :: v
        v => app%windows(1)%proxy%current_view()
        call v%reset_view()
    end subroutine on_reset_view

    !> How the spacecraft follows the orbit after one period: loop, or stop at the end.
    subroutine on_follow_mode(index, data)
        integer, intent(in) :: index
        class(*), intent(inout), optional :: data
        type(TrajectoryFollower), pointer :: follower
        follower => app%spacecraft%follower()
        call follower%set_follow_type(ior(FOLLOW_POSITION, FOLLOW_ATTITUDE), merge(FOLLOW_LIMIT, FOLLOW_LOOP, index == 1))
    end subroutine on_follow_mode

    !> Line width: a style change only (no point data is re-uploaded).
    subroutine on_line_width(value, data)
        real(c_double), intent(in) :: value
        class(*), intent(inout), optional :: data
        if (.not. c_associated(app%orbit_line)) return
        if (line_set_style(app%orbit_line, orbit_color, real(value, c_float)) /= 0) &
            write(*, '(a)') 'vkframes: line style failed'
    end subroutine on_line_width

    !> Open another window on the scene (up to max_windows).
    subroutine on_new_window(data)
        class(*), intent(inout), optional :: data
        call new_window()
    end subroutine on_new_window

    subroutine new_window()
        integer :: k
        do k = 2, max_windows
            if (.not. app%windows(k)%used) then
                call build_window(k, k)
                return
            end if
        end do
        write(*, '(a,i0,a)') 'vkframes demo: at most ', max_windows, ' windows'
    end subroutine new_window

    !> --test-resize: resize the main window (checks the swapchain rebuild).
    subroutine on_test_resize(data)
        class(*), intent(inout), optional :: data
        call app%main%resize(800, 520)
    end subroutine on_test_resize

    !> --quit-after: close all windows.
    subroutine on_quit(data)
        class(*), intent(inout), optional :: data
        integer :: k
        do k = max_windows, 2, -1
            if (app%windows(k)%used) call app%windows(k)%top%close()
        end do
        call app%main%close()
    end subroutine on_quit

    !> The main window closes: close the other windows, then its own view.
    subroutine on_close(event, data)
        type(QCloseEvent), intent(in) :: event
        class(*), intent(inout), optional :: data
        integer :: k
        do k = max_windows, 2, -1
            if (app%windows(k)%used) call app%windows(k)%top%close()
        end do
        call close_view(app%windows(1))
        if (c_associated(app%orbit_line)) call vkf_node_delete(app%orbit_line)
        app%orbit_line = c_null_ptr
    end subroutine on_close

    ! ==================================================================
    ! construction

    !> The main window: the first view in the center, controls in a dock.
    subroutine build_ui()
        type(QDockWidget) :: dock
        type(QWidget) :: panel
        type(QFormLayout) :: form
        type(QPushButton) :: reset, reset_view, new_win

        app%main = QMainWindow()
        call app%main%setWindowTitle('vkframes in qt-fortran')
        call app%main%closeEvent%connect(on_close)

        panel = QWidget()
        form = QFormLayout(panel)
        call build_window(1, app%start_view)
        call app%main%setCentralWidget(app%windows(1)%container)
        call form%addRow('View', app%windows(1)%view_choice)
        reset_view = QPushButton('Reset view')
        call reset_view%clicked%connect(on_reset_view)
        call form%addRow('', reset_view)
        new_win = QPushButton('New window')
        call new_win%clicked%connect(on_new_window)
        call form%addRow('', new_win)
        app%time_scale = QDoubleSpinBox()
        call app%time_scale%setRange(0.0_c_double, 5000.0_c_double)
        call app%time_scale%setValue(200.0_c_double)
        call app%time_scale%setSuffix(' x')
        call app%time_scale%valueChanged%connect(on_time_scale)
        call form%addRow('Time scale', app%time_scale)
        app%pause_time = QCheckBox('Pause time')
        call app%pause_time%toggled%connect(on_pause_time)
        call form%addRow('', app%pause_time)
        reset = QPushButton('Reset time')
        call reset%clicked%connect(on_reset_time)
        call form%addRow('', reset)
        app%follow_mode = QComboBox()
        call app%follow_mode%addItems([character(len=23) :: 'Loop the orbit', 'Stop at the end (limit)'])
        call app%follow_mode%currentIndexChanged%connect(on_follow_mode)
        call form%addRow('After one period', app%follow_mode)
        app%line_width = QDoubleSpinBox()
        call app%line_width%setRange(0.5_c_double, 20.0_c_double)
        call app%line_width%setValue(2.0_c_double)
        call app%line_width%setSuffix(' pt')
        call app%line_width%valueChanged%connect(on_line_width)
        call form%addRow('Orbit width', app%line_width)
        app%time_label = QLabel('Simulation time:')
        call form%addRow(app%time_label)
        call form%addRow(QLabel('<small>Left drag: rotate<br>Right drag: zoom<br>Middle drag: pan<br>' // &
                                'Wheel: zoom</small>'))
        dock = QDockWidget('Controls', app%main)
        call dock%setWidget(panel)
        call app%main%addDockWidget(Qt_LeftDockWidgetArea, dock)
    end subroutine build_ui

end module vkdemo

!> vkframes in a qt-fortran GUI (see the module documentation).
program vkframes_qt_demo

    use qt
    use vkdemo
    implicit none

    type(QApplication) :: qapp
    character(len=1024) :: arg
    integer :: i, k, ierr, status

    qapp = QApplication()

    call get_environment_variable('OPENFRAMES_DATA', arg, status=status)
    app%data_dir = ''
    if (status == 0) app%data_dir = trim(arg)
    i = 1
    do while (i <= command_argument_count())
        call get_command_argument(i, arg)
        select case (arg)
        case ('--data')
            call get_command_argument(i + 1, arg)
            app%data_dir = trim(arg)
            i = i + 1
        case ('--capture')
            call get_command_argument(i + 1, arg)
            app%capture = trim(arg)
            i = i + 1
        case ('--capture-frame')
            call get_command_argument(i + 1, arg)
            read(arg, *) app%capture_frame
            i = i + 1
        case ('--view')
            call get_command_argument(i + 1, arg)
            read(arg, *) app%start_view
            i = i + 1
        case ('--windows')
            call get_command_argument(i + 1, arg)
            read(arg, *) app%start_windows
            i = i + 1
        case ('--test-resize')
            app%test_resize = .true.
        case ('--quit-after')
            call get_command_argument(i + 1, arg)
            read(arg, *) app%quit_after
            i = i + 1
        end select
        i = i + 1
    end do

    call build_model()
    call build_ui()
    if (app%quit_after > 0) call QTimer_singleShot(int(1000 * app%quit_after), on_quit)
    if (app%test_resize) call QTimer_singleShot(2000, on_test_resize)
    call app%main%resize(1100, 700)
    call app%main%show()
    do k = 2, min(app%start_windows, max_windows)
        call new_window()
    end do
    ierr = qapp%exec()

end program vkframes_qt_demo
