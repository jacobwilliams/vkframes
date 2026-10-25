!> vkframes in a qt-fortran GUI: a spacecraft orbiting the Earth, rendered by
!> VulkanSceneGraph into a QWindow embedded in a QMainWindow, next to a dock of
!> qt-fortran controls.
!>
!> How the pieces fit:
!>
!> * The host owns a plain QWindow. On its first expose,
!>   vkframes creates its viewer on the window's native handle (`winId()`):
!>   VSG renders into that window and leaves its events to Qt.
!> * A QtfEventFilter forwards the window's events to the viewer: resizes, and
!>   mouse, wheel and keys for the trackball, converted here to VSG's
!>   conventions (framebuffer pixels: logical * devicePixelRatio).
!> * Everything runs on the GUI thread (Vulkan has no current context, so there
!>   is no render thread and none of OpenGL's macOS workarounds): each
!>   `updateRequest` advances the simulation, renders a frame and asks for the
!>   next one, so frames follow the display's refresh.
!>
!> Run with `pixi run -e qt qt-demo` (from the vkframes root). Options:
!> `--data dir` OpenFrames' Demos folder, for the Earth texture and Hubble model
!> (default: $OPENFRAMES_DATA); `--capture file` saves a frame as an image
!> (`--capture-frame n`, default 120); `--quit-after seconds` closes the demo;
!> `--test-resize` resizes the main window after 2 s.
module vkdemo

    use qt
    use vkf_layer
    use vkf_geometry, only: ellipsoid_create, sphere_create
    use iso_c_binding
    use iso_fortran_env, only: int64
    implicit none

    integer, parameter :: wp = c_double
    real(wp), parameter :: pi = acos(-1.0_wp)
    real(wp), parameter :: mu = 398600.4418_wp                  !! Earth's gravitational parameter (km^3/s^2)
    real(wp), parameter :: radii(3) = [6378.137_wp, 6378.137_wp, 6356.752314245_wp]  !! WGS84 (km)
    real(wp), parameter :: sma = 9000.0_wp, ecc = 0.2_wp, inc = 35.0_wp * pi / 180.0_wp
    real(wp), parameter :: model_size = 800.0_wp                 !! spacecraft bounding radius (km, enlarged to be seen)
    integer, parameter :: n_orbit = 721

    !> The GUI and the scene (used by the slots).
    type :: app_t
        type(QMainWindow) :: main
        type(QWindow) :: window
        type(QWidget) :: container
        type(QtfEventFilter) :: events
        type(QDoubleSpinBox) :: time_scale
        type(QCheckBox) :: pause_time
        type(QDoubleSpinBox) :: line_width
        type(QLabel) :: clock
        type(c_ptr) :: viewer = c_null_ptr
        type(c_ptr) :: spacecraft = c_null_ptr                 !! the spacecraft's transform
        type(c_ptr) :: orbit_line = c_null_ptr
        real(wp) :: model_center(3) = 0.0_wp, model_scale = 1.0_wp
        real(wp) :: sim_time = 0.0_wp                          !! s
        integer(int64) :: last_tick = -1
        integer :: frame = 0
        character(len=:), allocatable :: data_dir, capture
        integer :: capture_frame = 120
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

    pure function transform(position, scale, center) result(m)
        real(wp), intent(in) :: position(3), scale, center(3)
        real(wp) :: m(4,4)
        integer :: k
        m = 0.0_wp
        do k = 1, 3
            m(k, k) = scale
        end do
        m(4, 4) = 1.0_wp
        m(1:3, 4) = position - scale * center      ! translate(position) * scale * translate(-center)
    end function transform

    subroutine check(status, what)
        integer(c_int), intent(in) :: status
        character(len=*), intent(in) :: what
        if (status /= 0) write(*, '(a)') 'vkframes: '//what//' failed'
    end subroutine check

    !> Build the scene in the viewer: the Earth, the orbit and the spacecraft.
    subroutine build_scene(a)
        type(app_t), intent(inout) :: a
        type(c_ptr) :: texture, earth, model
        real(wp) :: xyz(3, n_orbit), period, model_radius
        character(len=:), allocatable :: file
        integer :: k

        call check(vkf_viewer_look_at(a%viewer, [0.0_wp, -38000.0_wp, 14000.0_wp], [0.0_wp, 0.0_wp, 0.0_wp], &
                                      [0.0_wp, 0.0_wp, 1.0_wp]), 'look_at')
        call check(vkf_viewer_perspective(a%viewer, 30.0_wp, 1.0_wp, 1.0e7_wp), 'perspective')

        texture = c_null_ptr
        if (len(a%data_dir) > 0) then
            file = a%data_dir//'/Images/land_shallow_topo_2048.jpg'
            texture = vkf_image_read(file, len(file, kind=c_int))
        end if
        earth = ellipsoid_create(radii, [1.0_c_float, 1.0_c_float, 1.0_c_float, 1.0_c_float], texture)
        call check(vkf_viewer_add_node(a%viewer, earth), 'add Earth')
        call vkf_node_delete(earth)
        if (c_associated(texture)) call vkf_image_delete(texture)

        period = 2.0_wp * pi * sqrt(sma**3 / mu)
        do k = 1, n_orbit
            xyz(:, k) = orbit_position(period * real(k - 1, wp) / real(n_orbit - 1, wp))
        end do
        a%orbit_line = vkf_line_create(a%viewer, int(n_orbit, c_int))
        call check(line_set_style(a%orbit_line, [1.0_c_float, 0.85_c_float, 0.2_c_float, 1.0_c_float], 2.0_c_float), &
                   'line style')
        call check(line_set_points(a%orbit_line, xyz), 'line points')
        call check(vkf_viewer_add_node(a%viewer, a%orbit_line), 'add orbit')

        ! the spacecraft: OpenFrames' Hubble model, or a small sphere without the data
        model = c_null_ptr
        if (len(a%data_dir) > 0) then
            file = a%data_dir//'/Models/Hubble.3ds'
            model = vkf_model_read(file, len(file, kind=c_int))
        end if
        if (c_associated(model)) then
            call check(vkf_node_bound(model, a%model_center, model_radius), 'model bound')
            a%model_scale = model_size / model_radius
        else
            model = sphere_create(model_size, [1.0_c_float, 0.3_c_float, 0.2_c_float, 1.0_c_float])
        end if
        a%spacecraft = vkf_transform_create()
        call check(vkf_node_add_child(a%spacecraft, model), 'add model')
        call vkf_node_delete(model)
        call check(vkf_viewer_add_node(a%viewer, a%spacecraft), 'add spacecraft')
        call place_spacecraft(a)
    end subroutine build_scene

    subroutine place_spacecraft(a)
        type(app_t), intent(inout) :: a
        call check(vkf_transform_set_matrix(a%spacecraft, &
                   transform(orbit_position(a%sim_time), a%model_scale, a%model_center)), 'spacecraft transform')
    end subroutine place_spacecraft

    ! ==================================================================
    ! window events (GUI thread)

    !> First expose: create the viewer on the window's native handle, then start
    !> the frame loop.
    subroutine on_expose(data)
        class(*), intent(inout), optional :: data
        character(len=*), parameter :: title = 'vkframes'
        select type (a => data)
        type is (app_t)
            if (c_associated(a%viewer) .or. .not. a%window%isExposed()) return
            a%viewer = vkf_viewer_create(int(a%window%width(), c_int), int(a%window%height(), c_int), &
                                         title, len(title, kind=c_int), a%window%winId(), a%window%devicePixelRatio())
            if (.not. c_associated(a%viewer)) then
                write(*, '(a)') 'vkframes: could not create the viewer'
                return
            end if
            call build_scene(a)
            call a%window%requestUpdate()
        end select
    end subroutine on_expose

    !> Advance the simulation by the elapsed wall-clock time (times the time
    !> scale), render a frame, and ask for the next one.
    subroutine on_update(data)
        class(*), intent(inout), optional :: data
        integer(int64) :: tick, rate
        integer(c_int) :: status
        character(len=40) :: text
        select type (a => data)
        type is (app_t)
            if (.not. c_associated(a%viewer)) return
            call system_clock(tick, rate)
            if (a%last_tick >= 0 .and. .not. a%pause_time%isChecked()) &
                a%sim_time = a%sim_time + a%time_scale%value() * real(tick - a%last_tick, wp) / real(rate, wp)
            a%last_tick = tick
            call place_spacecraft(a)
            status = vkf_viewer_frame(a%viewer)
            if (status /= 1) return
            a%frame = a%frame + 1
            write(text, '(a,f10.1,a)') 'Simulation time: ', a%sim_time, ' s'
            call a%clock%setText(trim(text))
            if (allocated(a%capture) .and. a%frame == a%capture_frame) then
                call check(vkf_viewer_capture(a%viewer, a%capture, len(a%capture, kind=c_int)), 'capture')
                write(*, '(a)') 'captured '//a%capture
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
            if (.not. c_associated(a%viewer)) return
            dpr = a%window%devicePixelRatio()
            call check(vkf_viewer_resize(a%viewer, nint(event%width() * dpr, c_int), nint(event%height() * dpr, c_int)), &
                       'resize')
            call a%window%requestUpdate()
        end select
    end subroutine on_resize

    !> VSG's mouse button number: 1 left, 2 middle, 3 right (0: other).
    integer(c_int) function vsg_button(qt_button)
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
    integer(c_int) function vsg_mask(qt_buttons)
        integer, intent(in) :: qt_buttons
        vsg_mask = 0
        if (iand(qt_buttons, Qt_LeftButton) /= 0) vsg_mask = ior(vsg_mask, 256)
        if (iand(qt_buttons, Qt_MiddleButton) /= 0) vsg_mask = ior(vsg_mask, 512)
        if (iand(qt_buttons, Qt_RightButton) /= 0) vsg_mask = ior(vsg_mask, 1024)
    end function vsg_mask

    !> Forward a mouse event (kind 1 move, 2 press, 3 release), in framebuffer pixels.
    subroutine forward_mouse(a, kind, event)
        type(app_t), intent(inout) :: a
        integer(c_int), intent(in) :: kind
        type(QMouseEvent), intent(in) :: event
        real(wp) :: dpr
        if (.not. c_associated(a%viewer)) return
        dpr = a%window%devicePixelRatio()
        call check(vkf_viewer_mouse(a%viewer, kind, nint(event%x() * dpr, c_int), nint(event%y() * dpr, c_int), &
                                    vsg_mask(event%buttons()), vsg_button(event%button())), 'mouse')
        call event%accept()
    end subroutine forward_mouse

    subroutine on_move(event, data)
        type(QMouseEvent), intent(in) :: event
        class(*), intent(inout), optional :: data
        select type (a => data)
        type is (app_t)
            call forward_mouse(a, 1_c_int, event)
        end select
    end subroutine on_move

    subroutine on_press(event, data)
        type(QMouseEvent), intent(in) :: event
        class(*), intent(inout), optional :: data
        select type (a => data)
        type is (app_t)
            call forward_mouse(a, 2_c_int, event)
        end select
    end subroutine on_press

    subroutine on_release(event, data)
        type(QMouseEvent), intent(in) :: event
        class(*), intent(inout), optional :: data
        select type (a => data)
        type is (app_t)
            call forward_mouse(a, 3_c_int, event)
        end select
    end subroutine on_release

    subroutine on_wheel(event, data)
        type(QWheelEvent), intent(in) :: event
        class(*), intent(inout), optional :: data
        select type (a => data)
        type is (app_t)
            if (.not. c_associated(a%viewer) .or. event%angleDeltaY() == 0) return
            call check(vkf_viewer_scroll(a%viewer, merge(1.0_c_float, -1.0_c_float, event%angleDeltaY() > 0)), 'scroll')
            call event%accept()
        end select
    end subroutine on_wheel

    !> VSG key symbol for a Qt key: ASCII for printable keys (lower case for the
    !> base symbol), X11-style codes for the others VSG's trackball uses; 0 if
    !> unmapped.
    subroutine vsg_key(event, symbol, modified_symbol, modifier)
        type(QKeyEvent), intent(in) :: event
        integer(c_int), intent(out) :: symbol, modified_symbol, modifier
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
            symbol = int(z'FF51', c_int)
        case (Qt_Key_Up)
            symbol = int(z'FF52', c_int)
        case (Qt_Key_Right)
            symbol = int(z'FF53', c_int)
        case (Qt_Key_Down)
            symbol = int(z'FF54', c_int)
        case (Qt_Key_Home)
            symbol = int(z'FF50', c_int)
        case (Qt_Key_Escape)
            symbol = int(z'FF1B', c_int)
        case (Qt_Key_Return)
            symbol = int(z'FF0D', c_int)
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
        integer(c_int), intent(in) :: pressed
        type(QKeyEvent), intent(in) :: event
        integer(c_int) :: symbol, modified_symbol, modifier
        if (.not. c_associated(a%viewer)) return
        call vsg_key(event, symbol, modified_symbol, modifier)
        if (symbol == 0) return
        call check(vkf_viewer_key(a%viewer, pressed, symbol, modified_symbol, modifier), 'key')
        call event%accept()
    end subroutine forward_key

    subroutine on_key_press(event, data)
        type(QKeyEvent), intent(in) :: event
        class(*), intent(inout), optional :: data
        select type (a => data)
        type is (app_t)
            call forward_key(a, 1_c_int, event)
        end select
    end subroutine on_key_press

    subroutine on_key_release(event, data)
        type(QKeyEvent), intent(in) :: event
        class(*), intent(inout), optional :: data
        select type (a => data)
        type is (app_t)
            call forward_key(a, 0_c_int, event)
        end select
    end subroutine on_key_release

    ! ==================================================================
    ! controls and lifetime (GUI thread)

    !> Line width: a style change only (no point data is re-uploaded).
    subroutine on_line_width(value, data)
        real(c_double), intent(in) :: value
        class(*), intent(inout), optional :: data
        select type (a => data)
        type is (app_t)
            if (.not. c_associated(a%orbit_line)) return
            call check(line_set_style(a%orbit_line, [1.0_c_float, 0.85_c_float, 0.2_c_float, 1.0_c_float], &
                                      real(value, c_float)), 'line style')
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

    !> The main window closes: release the viewer (waiting for the GPU) before
    !> Qt destroys the native window it renders into.
    subroutine on_close(event, data)
        type(QCloseEvent), intent(in) :: event
        class(*), intent(inout), optional :: data
        select type (a => data)
        type is (app_t)
            if (.not. c_associated(a%viewer)) return
            call vkf_node_delete(a%spacecraft)
            call vkf_node_delete(a%orbit_line)
            call vkf_viewer_delete(a%viewer)
            a%viewer = c_null_ptr
            a%spacecraft = c_null_ptr
            a%orbit_line = c_null_ptr
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
        a%time_scale = QDoubleSpinBox()
        call a%time_scale%setRange(0.0_c_double, 5000.0_c_double)
        call a%time_scale%setValue(200.0_c_double)
        call a%time_scale%setSuffix(' x')
        call form%addRow('Time scale', a%time_scale)
        a%pause_time = QCheckBox('Pause time')
        call form%addRow('', a%pause_time)
        a%line_width = QDoubleSpinBox()
        call a%line_width%setRange(0.5_c_double, 20.0_c_double)
        call a%line_width%setValue(2.0_c_double)
        call a%line_width%setSuffix(' pt')
        call a%line_width%valueChanged%connect(on_line_width, a)
        call form%addRow('Orbit width', a%line_width)
        a%clock = QLabel('Simulation time:')
        call form%addRow(a%clock)
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
        case ('--test-resize')
            a%test_resize = .true.
        case ('--quit-after')
            call get_command_argument(i + 1, arg)
            read(arg, *) a%quit_after
            i = i + 1
        end select
        i = i + 1
    end do

    call build_ui(a)
    if (a%quit_after > 0) call QTimer_singleShot(int(1000 * a%quit_after), on_quit, a)
    if (a%test_resize) call QTimer_singleShot(2000, on_test_resize, a)
    call a%main%resize(1100, 700)
    call a%main%show()
    ierr = app%exec()

end program vkframes_qt_demo
