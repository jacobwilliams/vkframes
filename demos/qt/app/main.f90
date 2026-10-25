!> vkframes in a qt-fortran GUI: a spacecraft orbiting the Earth, rendered by
!> VulkanSceneGraph into a QWindow embedded in a QMainWindow, next to a dock of
!> qt-fortran controls.
!>
!> The scene uses vkframes' API: the Earth is a Sphere (the WGS84 ellipsoid,
!> textured), the spacecraft a Model (or a small Sphere without the data) that
!> follows a Trajectory (positions and attitudes over one period; LOOP or LIMIT
!> from the dock), a child of the Earth. A FrameManager holds the scene and a
!> WindowProxy shows it, with its simulation time (time scale, pause and reset
!> from the dock) and three Views, chosen from the dock: the Earth, the
!> spacecraft (body-fixed), and from the spacecraft toward the Earth. The orbit line is still a low-level line node, drawn from
!> the trajectory's positions, until trajectory artists come.
!>
!> How the pieces fit:
!>
!> * The host owns a plain QWindow. On its first expose, the WindowProxy opens
!>   on the window's native handle (`winId()`): VSG renders into that window and
!>   leaves its events to Qt.
!> * A QtfEventFilter forwards the window's events to the WindowProxy:
!>   resizes, and mouse, wheel and keys for the trackball, converted here to
!>   VSG's conventions (framebuffer pixels: logical * devicePixelRatio).
!> * Everything runs on the GUI thread (Vulkan has no current context, so there
!>   is no render thread and none of OpenGL's macOS workarounds): each
!>   `updateRequest` renders a frame (WindowProxy%frame: time, frames, draw)
!>   and asks for the next one, so frames follow the display's refresh.
!>
!> Run with `pixi run -e qt qt-demo` (from the vkframes root). Options:
!> `--data dir` OpenFrames' Demos folder, for the Earth texture and Hubble model
!> (default: $OPENFRAMES_DATA); `--capture file` saves a frame as an image
!> (`--capture-frame n`, default 120); `--quit-after seconds` closes the demo;
!> `--test-resize` resizes the main window after 2 s; `--view n` starts in view n
!> (1 Earth, 2 spacecraft, 3 spacecraft -> Earth).
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
    real(c_float), parameter :: orbit_color(4) = [1.0_c_float, 0.85_c_float, 0.2_c_float, 1.0_c_float]

    !> The GUI and the scene (used by the slots).
    type :: app_t
        type(QMainWindow) :: main
        type(QWindow) :: window
        type(QWidget) :: container
        type(QtfEventFilter) :: events
        type(QDoubleSpinBox) :: time_scale
        type(QCheckBox) :: pause_time
        type(QDoubleSpinBox) :: line_width
        type(QComboBox) :: follow_mode
        type(QComboBox) :: view_choice
        type(QLabel) :: time_label
        ! the scene
        type(Trajectory) :: orbit                              !! one period of positions and attitudes
        type(Sphere) :: earth
        class(ReferenceFrame), allocatable :: spacecraft      !! a Model, or a Sphere without the data
        type(FrameManager) :: scene
        type(WindowProxy) :: view
        type(View) :: earth_view, craft_view, nadir_view
        type(c_ptr) :: orbit_line = c_null_ptr                 !! low-level line node (until artists)
        integer :: frame = 0
        character(len=:), allocatable :: data_dir, capture
        integer :: capture_frame = 120
        integer :: start_view = 1
        real(wp) :: quit_after = -1.0_wp
        logical :: test_resize = .false.
    end type app_t

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

    !> The scene: the orbit trajectory (one period), the Earth, and the
    !> spacecraft following the orbit. `a` is a target (frames keep pointers to
    !> the trajectory and to each other).
    subroutine build_model(a)
        type(app_t), intent(inout), target :: a
        real(wp) :: period, t, r(3), center(3), radius
        logical :: ok
        integer :: k

        a%orbit = Trajectory()
        call a%orbit%reserve(n_orbit)
        period = 2.0_wp * pi * sqrt(sma**3 / mu)
        do k = 1, n_orbit
            t = period * real(k - 1, wp) / real(n_orbit - 1, wp)
            r = orbit_position(t)
            call a%orbit%add(t, pos=r, att=orbit_attitude(r))
        end do

        a%earth = Sphere('Earth', radius=re, color=[0.3_wp, 0.5_wp, 1.0_wp])
        call a%earth%set_sphere_scale([1.0_wp, 1.0_wp, 1.0_wp - flattening])
        if (len(a%data_dir) > 0) call a%earth%set_texture_map(a%data_dir//'/Images/land_shallow_topo_2048.jpg')

        ! the spacecraft: OpenFrames' Hubble model, or a small sphere without the data
        ok = .false.
        if (len(a%data_dir) > 0) then
            allocate(a%spacecraft, source=Model('Spacecraft', color=[1.0_wp, 0.85_wp, 0.2_wp]))
            select type (sc => a%spacecraft)
            type is (Model)
                ok = sc%set_model(a%data_dir//'/Models/Hubble.3ds')
                if (ok) then
                    call sc%model_bound(center, radius)
                    call sc%set_model_pivot(center)
                    call sc%set_model_scale([1.0_wp, 1.0_wp, 1.0_wp] * model_size / radius)
                end if
            end select
            if (.not. ok) deallocate(a%spacecraft)
        end if
        if (.not. ok) allocate(a%spacecraft, source=Sphere('Spacecraft', radius=model_size, &
                                                           color=[1.0_wp, 0.3_wp, 0.2_wp]))
        call a%spacecraft%follow(a%orbit)                 ! position and attitude, FOLLOW_LOOP
        if (.not. a%earth%add_child(a%spacecraft)) write(*, '(a)') 'vkframes: add_child failed'
        call a%scene%set_frame(a%earth)
        write(*, '(a)', advance='no') a%earth%frame_string()

        ! views: the Earth, the spacecraft (body-fixed), and from the spacecraft
        ! toward the Earth (inertial axes, a little behind the spacecraft)
        a%earth_view = View(a%earth, a%earth)
        a%craft_view = View(a%earth, a%spacecraft)
        a%nadir_view = View(a%earth, a%spacecraft, lookat=a%earth, frame_type=ABSOLUTE_FRAME)
        call a%nadir_view%set_default_view_distance(1.5_wp * model_size)
    end subroutine build_model

    !> Open the view in the Qt window (first expose) and show the scene.
    subroutine open_view(a)
        type(app_t), intent(inout) :: a
        real(wp), pointer :: xyz(:,:)
        if (.not. a%view%open(a%window%width(), a%window%height(), 'vkframes', &
                              native_window=a%window%winId(), pixel_ratio=a%window%devicePixelRatio())) then
            write(*, '(a)') 'vkframes: could not open the view'
            return
        end if
        call a%view%set_scene(a%scene)
        call a%view%set_background_color([0.0_wp, 0.0_wp, 0.0_wp])
        call a%view%add_view(a%earth_view)
        call a%view%add_view(a%craft_view)
        call a%view%add_view(a%nadir_view)
        call a%view%select_view(a%view_choice%currentIndex() + 1)
        call a%view%set_time_scale(a%time_scale%value())
        call a%view%pause_time(a%pause_time%isChecked())
        call a%view%set_time(0.0_wp)

        ! the orbit line from the trajectory's positions (not copied)
        call a%orbit%positions(xyz)
        a%orbit_line = vkf_line_create(a%view%viewer(), int(a%orbit%num_positions(), c_int))
        if (line_set_style(a%orbit_line, orbit_color, real(a%line_width%value(), c_float)) /= 0 .or. &
            line_set_points(a%orbit_line, xyz) /= 0 .or. vkf_viewer_add_node(a%view%viewer(), a%orbit_line) /= 0) &
            write(*, '(a)') 'vkframes: could not add the orbit line'
    end subroutine open_view

    ! ==================================================================
    ! window events (GUI thread)

    !> First expose: open the view, then start the frame loop.
    subroutine on_expose(data)
        class(*), intent(inout), optional :: data
        select type (a => data)
        type is (app_t)
            if (a%view%is_open() .or. .not. a%window%isExposed()) return
            call open_view(a)
            if (a%view%is_open()) call a%window%requestUpdate()
        end select
    end subroutine on_expose

    !> Render a frame (time, frames, draw) and ask for the next one.
    subroutine on_update(data)
        class(*), intent(inout), optional :: data
        character(len=40) :: text
        select type (a => data)
        type is (app_t)
            if (.not. a%view%frame()) return
            a%frame = a%frame + 1
            write(text, '(a,f10.1,a)') 'Simulation time: ', a%view%time(), ' s'
            call a%time_label%setText(trim(text))
            if (allocated(a%capture) .and. a%frame == a%capture_frame) then
                if (a%view%capture(a%capture)) write(*, '(a)') 'captured '//a%capture
            end if
            call a%window%requestUpdate()
        end select
    end subroutine on_update

    !> New size: rebuild the swapchain (framebuffer pixels).
    subroutine on_resize(event, data)
        type(QResizeEvent), intent(in) :: event
        class(*), intent(inout), optional :: data
        real(wp) :: dpr
        select type (a => data)
        type is (app_t)
            if (.not. a%view%is_open()) return
            dpr = a%window%devicePixelRatio()
            call a%view%resize(nint(event%width() * dpr), nint(event%height() * dpr))
            call a%window%requestUpdate()
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
    subroutine forward_mouse(a, kind, event)
        type(app_t), intent(inout) :: a
        integer, intent(in) :: kind
        type(QMouseEvent), intent(in) :: event
        real(wp) :: dpr
        dpr = a%window%devicePixelRatio()
        call a%view%mouse(kind, nint(event%x() * dpr), nint(event%y() * dpr), vsg_mask(event%buttons()), &
                          vsg_button(event%button()))
        call event%accept()
    end subroutine forward_mouse

    subroutine on_move(event, data)
        type(QMouseEvent), intent(in) :: event
        class(*), intent(inout), optional :: data
        select type (a => data)
        type is (app_t)
            call forward_mouse(a, MOUSE_MOVE, event)
        end select
    end subroutine on_move

    subroutine on_press(event, data)
        type(QMouseEvent), intent(in) :: event
        class(*), intent(inout), optional :: data
        select type (a => data)
        type is (app_t)
            call forward_mouse(a, MOUSE_PRESS, event)
        end select
    end subroutine on_press

    subroutine on_release(event, data)
        type(QMouseEvent), intent(in) :: event
        class(*), intent(inout), optional :: data
        select type (a => data)
        type is (app_t)
            call forward_mouse(a, MOUSE_RELEASE, event)
        end select
    end subroutine on_release

    subroutine on_wheel(event, data)
        type(QWheelEvent), intent(in) :: event
        class(*), intent(inout), optional :: data
        select type (a => data)
        type is (app_t)
            if (event%angleDeltaY() == 0) return
            call a%view%scroll(merge(1.0_wp, -1.0_wp, event%angleDeltaY() > 0))
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

    subroutine forward_key(a, pressed, event)
        type(app_t), intent(inout) :: a
        logical, intent(in) :: pressed
        type(QKeyEvent), intent(in) :: event
        integer :: symbol, modified_symbol, modifier
        call vsg_key(event, symbol, modified_symbol, modifier)
        if (symbol == 0) return
        call a%view%key(pressed, symbol, modified_symbol, modifier)
        call event%accept()
    end subroutine forward_key

    subroutine on_key_press(event, data)
        type(QKeyEvent), intent(in) :: event
        class(*), intent(inout), optional :: data
        select type (a => data)
        type is (app_t)
            call forward_key(a, .true., event)
        end select
    end subroutine on_key_press

    subroutine on_key_release(event, data)
        type(QKeyEvent), intent(in) :: event
        class(*), intent(inout), optional :: data
        select type (a => data)
        type is (app_t)
            call forward_key(a, .false., event)
        end select
    end subroutine on_key_release

    ! ==================================================================
    ! controls and lifetime (GUI thread)

    !> Time scale (simulation seconds per second).
    subroutine on_time_scale(value, data)
        real(c_double), intent(in) :: value
        class(*), intent(inout), optional :: data
        select type (a => data)
        type is (app_t)
            call a%view%set_time_scale(value)
        end select
    end subroutine on_time_scale

    !> Pause or resume the simulation time.
    subroutine on_pause_time(checked, data)
        logical, intent(in) :: checked
        class(*), intent(inout), optional :: data
        select type (a => data)
        type is (app_t)
            call a%view%pause_time(checked)
        end select
    end subroutine on_pause_time

    !> Back to time 0.
    subroutine on_reset_time(data)
        class(*), intent(inout), optional :: data
        select type (a => data)
        type is (app_t)
            call a%view%set_time(0.0_wp)
        end select
    end subroutine on_reset_time

    !> How the spacecraft follows the orbit after one period: loop, or stop at the end.
    subroutine on_follow_mode(index, data)
        integer, intent(in) :: index
        class(*), intent(inout), optional :: data
        type(TrajectoryFollower), pointer :: follower
        select type (a => data)
        type is (app_t)
            follower => a%spacecraft%follower()
            call follower%set_follow_type(ior(FOLLOW_POSITION, FOLLOW_ATTITUDE), merge(FOLLOW_LIMIT, FOLLOW_LOOP, index == 1))
        end select
    end subroutine on_follow_mode

    !> Show the chosen view (each keeps its own camera).
    subroutine on_view(index, data)
        integer, intent(in) :: index
        class(*), intent(inout), optional :: data
        select type (a => data)
        type is (app_t)
            call a%view%select_view(index + 1)
        end select
    end subroutine on_view

    !> Put the current view's camera back at its default position.
    subroutine on_reset_view(data)
        class(*), intent(inout), optional :: data
        type(View), pointer :: v
        select type (a => data)
        type is (app_t)
            v => a%view%current_view()
            call v%reset_view()
        end select
    end subroutine on_reset_view

    !> Line width: a style change only (no point data is re-uploaded).
    subroutine on_line_width(value, data)
        real(c_double), intent(in) :: value
        class(*), intent(inout), optional :: data
        select type (a => data)
        type is (app_t)
            if (.not. c_associated(a%orbit_line)) return
            if (line_set_style(a%orbit_line, orbit_color, real(value, c_float)) /= 0) &
                write(*, '(a)') 'vkframes: line style failed'
        end select
    end subroutine on_line_width

    !> --test-resize: resize the main window (checks the swapchain rebuild).
    subroutine on_test_resize(data)
        class(*), intent(inout), optional :: data
        select type (a => data)
        type is (app_t)
            call a%main%resize(800, 520)
        end select
    end subroutine on_test_resize

    !> --quit-after: close the main window.
    subroutine on_quit(data)
        class(*), intent(inout), optional :: data
        select type (a => data)
        type is (app_t)
            call a%main%close()
        end select
    end subroutine on_quit

    !> The main window closes: close the view (waiting for the GPU) before Qt
    !> destroys the native window it renders into.
    subroutine on_close(event, data)
        type(QCloseEvent), intent(in) :: event
        class(*), intent(inout), optional :: data
        select type (a => data)
        type is (app_t)
            if (c_associated(a%orbit_line)) call vkf_node_delete(a%orbit_line)
            a%orbit_line = c_null_ptr
            call a%view%close()
        end select
    end subroutine on_close

    ! ==================================================================
    ! construction

    !> The main window: the 3D view in the center, controls in a dock.
    subroutine build_ui(a)
        type(app_t), intent(inout), target :: a
        type(QDockWidget) :: dock
        type(QWidget) :: panel
        type(QFormLayout) :: form
        type(QPushButton) :: reset, reset_view

        a%main = QMainWindow()
        call a%main%setWindowTitle('vkframes in qt-fortran')

        ! default surface type, as vsgQt uses: VSG gives the native view its own
        ! CAMetalLayer on macOS (with QSurface_VulkanSurface, Qt manages the
        ! layer itself and MoltenVK finds no CAMetalLayer)
        a%window = QWindow()
        a%container = QWidget_createWindowContainer(a%window)
        call a%container%setMinimumSize(320, 240)
        call a%main%setCentralWidget(a%container)

        a%events = QtfEventFilter(a%window)
        call a%events%exposeEvent%connect(on_expose, a)
        call a%events%updateRequest%connect(on_update, a)
        call a%events%resizeEvent%connect(on_resize, a)
        call a%events%mousePressEvent%connect(on_press, a)
        call a%events%mouseReleaseEvent%connect(on_release, a)
        call a%events%mouseMoveEvent%connect(on_move, a)
        call a%events%wheelEvent%connect(on_wheel, a)
        call a%events%keyPressEvent%connect(on_key_press, a)
        call a%events%keyReleaseEvent%connect(on_key_release, a)
        call a%main%closeEvent%connect(on_close, a)

        panel = QWidget()
        form = QFormLayout(panel)
        a%view_choice = QComboBox()
        call a%view_choice%addItems([character(len=19) :: 'Earth', 'Spacecraft', 'Spacecraft -> Earth'])
        call a%view_choice%currentIndexChanged%connect(on_view, a)
        call form%addRow('View', a%view_choice)
        reset_view = QPushButton('Reset view')
        call reset_view%clicked%connect(on_reset_view, a)
        call form%addRow('', reset_view)
        a%time_scale = QDoubleSpinBox()
        call a%time_scale%setRange(0.0_c_double, 5000.0_c_double)
        call a%time_scale%setValue(200.0_c_double)
        call a%time_scale%setSuffix(' x')
        call a%time_scale%valueChanged%connect(on_time_scale, a)
        call form%addRow('Time scale', a%time_scale)
        a%pause_time = QCheckBox('Pause time')
        call a%pause_time%toggled%connect(on_pause_time, a)
        call form%addRow('', a%pause_time)
        reset = QPushButton('Reset time')
        call reset%clicked%connect(on_reset_time, a)
        call form%addRow('', reset)
        a%follow_mode = QComboBox()
        call a%follow_mode%addItems([character(len=23) :: 'Loop the orbit', 'Stop at the end (limit)'])
        call a%follow_mode%currentIndexChanged%connect(on_follow_mode, a)
        call form%addRow('After one period', a%follow_mode)
        a%line_width = QDoubleSpinBox()
        call a%line_width%setRange(0.5_c_double, 20.0_c_double)
        call a%line_width%setValue(2.0_c_double)
        call a%line_width%setSuffix(' pt')
        call a%line_width%valueChanged%connect(on_line_width, a)
        call form%addRow('Orbit width', a%line_width)
        a%time_label = QLabel('Simulation time:')
        call form%addRow(a%time_label)
        call form%addRow(QLabel('<small>Left drag: rotate<br>Right drag: zoom<br>Middle drag: pan<br>' // &
                                'Wheel: zoom</small>'))
        dock = QDockWidget('Controls', a%main)
        call dock%setWidget(panel)
        call a%main%addDockWidget(Qt_LeftDockWidgetArea, dock)
    end subroutine build_ui

end module vkdemo

!> vkframes in a qt-fortran GUI (see the module documentation).
program vkframes_qt_demo

    use qt
    use vkdemo
    implicit none

    type(QApplication) :: app
    type(app_t), target :: a
    character(len=1024) :: arg
    integer :: i, ierr, status

    app = QApplication()

    call get_environment_variable('OPENFRAMES_DATA', arg, status=status)
    a%data_dir = ''
    if (status == 0) a%data_dir = trim(arg)
    i = 1
    do while (i <= command_argument_count())
        call get_command_argument(i, arg)
        select case (arg)
        case ('--data')
            call get_command_argument(i + 1, arg)
            a%data_dir = trim(arg)
            i = i + 1
        case ('--capture')
            call get_command_argument(i + 1, arg)
            a%capture = trim(arg)
            i = i + 1
        case ('--capture-frame')
            call get_command_argument(i + 1, arg)
            read(arg, *) a%capture_frame
            i = i + 1
        case ('--view')
            call get_command_argument(i + 1, arg)
            read(arg, *) a%start_view
            i = i + 1
        case ('--test-resize')
            a%test_resize = .true.
        case ('--quit-after')
            call get_command_argument(i + 1, arg)
            read(arg, *) a%quit_after
            i = i + 1
        end select
        i = i + 1
    end do

    call build_model(a)
    call build_ui(a)
    call a%view_choice%setCurrentIndex(max(0, min(2, a%start_view - 1)))
    if (a%quit_after > 0) call QTimer_singleShot(int(1000 * a%quit_after), on_quit, a)
    if (a%test_resize) call QTimer_singleShot(2000, on_test_resize, a)
    call a%main%resize(1100, 700)
    call a%main%show()
    ierr = app%exec()

end program vkframes_qt_demo
