!> The OpenFrames side of the vkframes / OpenFrames benchmark (bench/README.md):
!> the same scene and scenarios as the vkframes app (bench_common), rendered by
!> OpenFrames (OpenSceneGraph, OpenGL) into a qt-fortran window, through
!> openframes-fortran, as qt-fortran's OpenFrames demo does it.
!>
!> OpenFrames renders on its own thread, calling `make_current` and
!> `swap_buffers` (below) every frame; each frame's time is taken in
!> `swap_buffers`. Frames are rendered as fast as possible: no vsync (the
!> default surface format's swap interval 0) and OpenFrames' frame limiter
!> set to 10000 frames per second. Points are added from the GUI thread
!> (oftraj_addtime and oftraj_addposition for each point: OpenFrames' C
!> interface has no bulk add), while the render thread draws.
!>
!> Run with `pixi run -e openframes bench-openframes -- --scenario grow ...`
!> (options in bench_common), or all scenarios of both libraries with
!> `pixi run bench`.
module ofbench

    use qt
    use openframes_module
    use bench_common
    use iso_c_binding
    use iso_fortran_env, only: real32, int32
    implicit none

    integer(int32), parameter :: winid = 1              !! the OpenFrames window proxy

    ! used by the C callbacks (on OpenFrames' render thread)
    type(QWindow), save :: window
    type(QOpenGLContext), save :: context
    type(FrameLog), save :: log                         !! written by the render thread
    logical, save :: gui_thread_updates = .false.       !! macOS: context updates on the GUI thread
    logical, save :: started = .false.                  !! OpenFrames' render thread is running
    logical, save :: stopping = .false.                 !! stopping it: no more OpenGL on its thread (see make_current)

    !> The GUI and the measurements.
    type :: app_t
        type(BenchOptions) :: opt
        type(QMainWindow) :: main
        type(QWidget) :: container
        type(QtfEventFilter) :: events
        type(QTimer) :: phases, appends
        real(dp), allocatable :: t(:), pos(:,:)          !! all the points (made before timing)
        integer :: n_added = 0
        type(AppendLog) :: appended
        real(dp) :: insert_s = 0.0_dp, first_frame_s = 0.0_dp, rss_load = 0.0_dp
        real(dp) :: t_start = 0.0_dp, t0 = 0.0_dp, t1 = 0.0_dp, cpu0 = 0.0_dp, cpu1 = 0.0_dp
        integer :: phase = 0                             !! 0 first frame, 1 warmup, 2 measuring, 3 done
        logical :: capture_requested = .false.
    end type app_t

    type(app_t), target, save :: app

contains

    ! ==================================================================
    ! OpenFrames' graphics callbacks (called on its render thread)

    !> Make the context current on the window (creating it on first use).
    !> Refused once OpenFrames is stopping: its render thread then closes its
    !> graphics context, which makes the context current, and on macOS that can
    !> make Qt update the context (NSOpenGLContext update) on that thread, which
    !> AppKit aborts unless on the main thread. (OpenSceneGraph then doesn't
    !> release its GL objects: the process is about to exit.)
    subroutine make_current(id, success) bind(C)
        integer(c_int), intent(in) :: id
        logical(c_bool), intent(out) :: success
        success = .false.
        if (stopping) return
        if (context%isNull()) then
            context = QOpenGLContext()
            if (.not. context%create()) return
        end if
        success = logical(context%makeCurrent(window), c_bool)
    end subroutine make_current

    !> Show the frame, and record when it was finished.
    subroutine swap_buffers(id) bind(C)
        integer(c_int), intent(in) :: id
        call context%swapBuffers(window)
        call log%record(now())
    end subroutine swap_buffers

    !> macOS: let AppKit attach the context to the window, on the GUI thread
    !> with OpenFrames paused (the first time, this creates the context).
    subroutine update_context()
        logical :: ok
        if (.not. gui_thread_updates .or. .not. window%isExposed()) return
        call ofwin_activate(winid)
        if (started) call ofwin_pauseanimation(.true.)
        if (context%isNull()) then
            context = QOpenGLContext()
            ok = context%create()
        end if
        if (context%makeCurrent(window)) call context%doneCurrent()
        if (started) call ofwin_pauseanimation(.false.)
    end subroutine update_context

    ! ==================================================================
    ! the scene

    !> The data (not timed), then the trajectory (timed: `insert_s`), and the scene.
    subroutine build_scene()
        real(dp) :: t
        integer :: n

        n = total_points(app%opt)
        allocate(app%t(n), app%pos(3, n))
        call make_orbit(n, app%t, app%pos)

        call oftraj_create('Orbit', 3, 0)
        t = now()
        call add_points(app%opt%points)
        app%insert_s = now() - t

        call offrame_create('Root')
        call hide_decorations()
        call ofsphere_create('Earth')
        call ofsphere_setradius(earth_radius)
        call offrame_setcolor(0.2_real32, 0.4_real32, 1.0_real32, 1.0_real32)
        call hide_decorations()

        call ofcurveartist_create('OrbitLine')
        call oftrajartist_settrajectory()
        call ofcurveartist_setxdata(of_posopt, of_x, 0, 1.0_dp)
        call ofcurveartist_setydata(of_posopt, of_y, 0, 1.0_dp)
        call ofcurveartist_setzdata(of_posopt, of_z, 0, 1.0_dp)
        call ofcurveartist_setcolor(1.0_real32, 0.75_real32, 0.2_real32)
        call ofcurveartist_setwidth(2.0_real32)
        call ofdrawtraj_create('Orbit')
        call ofdrawtraj_addartist('OrbitLine')
        call hide_decorations()

        call ofsphere_create('Spacecraft')
        call ofsphere_setradius(200.0_dp)
        call offrame_setcolor(1.0_real32, 0.3_real32, 0.2_real32, 1.0_real32)
        call offrame_followtrajectory('Orbit')
        call offrame_followtype(offollow_position, offollow_loop)
        call hide_decorations()

        call offrame_activate('Root')
        call offrame_addchild('Earth')
        call offrame_addchild('Orbit')
        call offrame_addchild('Spacecraft')

        call offm_create(1)
        call offrame_activate('Root')
        call offm_setframe()
        call ofwin_activate(winid)
        call ofwin_setscene(0, 0)
        call ofwin_setbackgroundcolor(0, 0, 0.0_real32, 0.0_real32, 0.02_real32)
        call ofwin_setdesiredframerate(10000.0_dp)
        call ofwin_settimescale(20.0_dp)                     ! one revolution every 5 s

        call ofview_create('View')
        call ofview_setviewframe('Root', 'Earth')
        call ofview_setdefaultviewdistance(view_distance)
        call ofwin_addview(0, 0)
        call ofview_activate('View')
        call ofwin_selectview(0, 0)
    end subroutine build_scene

    !> Add points up to `n`, one at a time (OpenFrames' C interface).
    subroutine add_points(n)
        integer, intent(in) :: n
        integer :: i
        call oftraj_activate('Orbit')
        do i = app%n_added + 1, min(n, size(app%t))
            call oftraj_addtime(app%t(i))
            call oftraj_addposition(app%pos(1, i), app%pos(2, i), app%pos(3, i))
        end do
        app%n_added = max(app%n_added, min(n, size(app%t)))
    end subroutine add_points

    !> No axes or labels on the current frame.
    subroutine hide_decorations()
        call offrame_showaxes(of_noaxes)
        call offrame_showaxeslabels(of_noaxes)
        call offrame_shownamelabel(.false.)
    end subroutine hide_decorations

    ! ==================================================================
    ! running (GUI thread)

    !> The window was exposed: start OpenFrames the first time.
    subroutine on_expose(data)
        class(*), intent(inout), optional :: data
        call update_context()
        if (window%isExposed() .and. .not. started) then
            started = .true.
            call log%init(2000000)
            app%t_start = now()
            call ofwin_activate(winid)
            call ofwin_start()
            app%phases = QTimer(app%main)
            call app%phases%timeout%connect(on_phase)
            call app%phases%start(2)
        end if
    end subroutine on_expose

    !> The phases, from the frames the render thread has finished: the first
    !> frame (timed), the warmup, the measurement, the result.
    subroutine on_phase(data)
        class(*), intent(inout), optional :: data
        real(dp) :: t
        t = now()
        select case (app%phase)
        case (0)
            if (log%n < 1) return
            app%first_frame_s = log%stamps(1) - app%t_start
            app%rss_load = peak_rss_mb()
            app%t0 = log%stamps(1) + app%opt%warmup
            app%phase = 1
            call show_status('warming up')
        case (1)
            if (t >= app%t0) call start_measuring(t)
        case (2)
            if (allocated(app%opt%capture) .and. .not. app%capture_requested .and. t >= app%t1 - 0.3_dp) then
                call ofwin_activate(winid)
                call ofwin_setwindowcapturefile(app%opt%capture, 'png')
                call ofwin_capturewindow()
                app%capture_requested = .true.
            end if
            if (t >= app%t1) call finish()
        end select
    end subroutine on_phase

    subroutine start_measuring(t)
        real(dp), intent(in) :: t
        app%t0 = t
        app%t1 = t + app%opt%seconds
        app%cpu0 = cpu_seconds()
        app%phase = 2
        call show_status('measuring: '//app%opt%scenario)
        if (app%opt%scenario == 'grow') then
            app%appends = QTimer(app%main)
            call app%appends%timeout%connect(on_append)
            call app%appends%start(nint(1000.0_dp * append_interval))
        end if
    end subroutine start_measuring

    !> grow: append the points due at --rate since the measurement began (timed).
    subroutine on_append(data)
        class(*), intent(inout), optional :: data
        real(dp) :: t, t_add
        integer :: before, target
        if (app%phase /= 2) return
        t = now()
        target = app%opt%points + int(app%opt%rate * (t - app%t0))
        before = app%n_added
        t_add = now()
        call add_points(target)
        if (app%n_added > before) call app%appended%add(now() - t_add, app%n_added - before)
    end subroutine on_append

    !> Stop the render thread (so that the frame log is complete), write the result and quit.
    subroutine finish()
        app%cpu1 = cpu_seconds()
        app%phase = 3
        call app%phases%stop()
        if (app%opt%scenario == 'grow') call app%appends%stop()
        call stop_openframes()
        call write_result(app%opt, 'OpenFrames', 'OpenSceneGraph (OpenGL)', 'oftraj_addtime + addposition, per point', &
                          app%insert_s, app%first_frame_s, log, app%t0, app%t1, app%appended, app%cpu1 - app%cpu0, &
                          app%rss_load)
        if (app%capture_requested) write(*, '(a)') 'captured '//app%opt%capture//'_0_0.png'
        call app%main%close()
    end subroutine finish

    subroutine stop_openframes()
        integer(int32) :: running
        if (.not. started) return
        stopping = .true.
        call ofwin_activate(winid)
        call ofwin_stop()
        do
            call ofwin_isrunning(running)
            if (running == 0) exit
            call QApplication_processEvents()
            call QThread_msleep(10)
        end do
        started = .false.
    end subroutine stop_openframes

    !> Tell OpenFrames the new size (in physical pixels).
    subroutine on_resize(event, data)
        type(QResizeEvent), intent(in) :: event
        class(*), intent(inout), optional :: data
        real(c_double) :: dpr
        call update_context()
        dpr = window%devicePixelRatio()
        call ofwin_activate(winid)
        call ofwin_resizewindow(0, 0, int(event%width() * dpr, int32), int(event%height() * dpr, int32))
    end subroutine on_resize

    subroutine on_close(event, data)
        type(QCloseEvent), intent(in) :: event
        class(*), intent(inout), optional :: data
        call stop_openframes()
    end subroutine on_close

    subroutine build_ui()
        app%main = QMainWindow()
        call app%main%setWindowTitle('OpenFrames benchmark: '//app%opt%scenario)
        call app%main%closeEvent%connect(on_close)
        window = QWindow()
        call window%setSurfaceType(QSurface_OpenGLSurface)
        app%container = QWidget_createWindowContainer(window)
        call app%container%setFixedSize(app%opt%width, app%opt%height)
        call app%main%setCentralWidget(app%container)
        app%events = QtfEventFilter(window)
        call app%events%exposeEvent%connect(on_expose)
        call app%events%resizeEvent%connect(on_resize)
        call show_status('starting')
    end subroutine build_ui

    subroutine show_status(text)
        character(len=*), intent(in) :: text
        type(QStatusBar) :: bar
        bar = app%main%statusBar()
        call bar%showMessage(text)
    end subroutine show_status

end module ofbench

!> The OpenFrames benchmark app (see the module documentation).
program openframes_bench

    use qt
    use openframes_module
    use bench_common
    use ofbench
    implicit none

    type(QApplication) :: qapp
    type(QSurfaceFormat) :: fmt
    integer :: ierr

    call parse_options(app%opt)
    app%opt%lod = .false.                           ! (OpenFrames draws every point)
    ! every OpenGL surface (set before the application): a depth buffer, and vsync or not
    fmt = QSurfaceFormat()
    call fmt%setDepthBufferSize(24)
    call fmt%setSwapInterval(merge(1, 0, app%opt%vsync))
    call QSurfaceFormat_setDefaultFormat(fmt)
    call fmt%delete()
    qapp = QApplication()
    gui_thread_updates = .not. QOpenGLContext_supportsThreadedOpenGL()
    if (gui_thread_updates) call QApplication_setAttribute(Qt_AA_DontCheckOpenGLContextThreadAffinity)

    ! OpenFrames: an embedded window proxy (we supply the OpenGL context)
    call of_initialize()
    call ofwin_createproxy(0, 0, int(app%opt%width, int32), int(app%opt%height, int32), 1, 1, .true., winid, .false.)
    call ofwin_setmakecurrentfunction(make_current)
    call ofwin_setswapbuffersfunction(swap_buffers)
    call build_scene()

    call build_ui()
    call app%main%show()
    ierr = qapp%exec()
    call of_cleanup()

end program openframes_bench
