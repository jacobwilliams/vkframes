!> The vkframes side of the vkframes / OpenFrames benchmark (bench/README.md):
!> a qt-fortran window showing an Earth, a trajectory of many points drawn by
!> a CurveArtist and a body following it, in one of the scenarios of
!> bench_common (static, grow), and the measurements written as one
!> JSON line.
!>
!> Frames are rendered as fast as possible: no vsync (`WindowProxy%open(...,
!> vsync=.false.)`), and a zero-interval QTimer calling `WindowProxy%frame`
!> (instead of the display's update requests). Everything runs on the GUI
!> thread, so a frame's time includes vkframes' Fortran update.
!>
!> Run with `pixi run -e qt bench-vkframes -- --scenario grow ...` (options in
!> bench_common), or all scenarios of both libraries with `pixi run bench`.
module vkbench

    use qt
    use vkframes
    use vkf_text, only: get_font
    use bench_common
    use iso_c_binding
    implicit none

    !> The GUI, the scene and the measurements.
    type :: app_t
        type(BenchOptions) :: opt
        type(QMainWindow) :: main
        type(QWindow) :: window
        type(QWidget) :: container
        type(QtfEventFilter) :: events
        type(QTimer) :: frames, appends
        type(WindowProxy) :: proxy
        ! the scene
        real(dp), allocatable :: t(:), pos(:,:)              !! all the points (made before timing)
        integer :: n_added = 0
        type(ReferenceFrame) :: root
        type(Sphere) :: earth, craft
        type(Trajectory) :: traj
        type(DrawableTrajectory) :: drawing
        type(CurveArtist) :: curve
        type(FrameManager) :: scene
        type(View) :: view
        ! measurements
        type(FrameLog) :: log
        type(AppendLog) :: appended
        real(dp) :: insert_s = 0.0_dp, first_frame_s = 0.0_dp, rss_load = 0.0_dp
        real(dp) :: t_start = 0.0_dp, t0 = 0.0_dp, t1 = 0.0_dp, cpu0 = 0.0_dp, cpu1 = 0.0_dp
        integer :: phase = 0                                 !! 0 first frame, 1 warmup, 2 measuring, 3 done
        logical :: started = .false.                         !! the view opened (on_expose)
    end type app_t

    type(app_t), target, save :: app

contains

    !> The data (not timed), then the trajectory (timed: `insert_s`), and the scene.
    subroutine build_scene()
        real(dp) :: t
        integer :: n
        n = total_points(app%opt)
        allocate(app%t(n), app%pos(3, n))
        call make_orbit(n, app%t, app%pos)

        app%traj = Trajectory()
        t = now()
        call add_points(app%opt%points)
        app%insert_s = now() - t

        app%root = ReferenceFrame('Root')
        call hide_decorations(app%root)
        app%earth = Sphere('Earth', radius=earth_radius, color=[0.2_dp, 0.4_dp, 1.0_dp])
        call hide_decorations(app%earth)
        app%curve = CurveArtist(app%traj, color=[1.0_dp, 0.75_dp, 0.2_dp], width=2.0_dp)
        call app%curve%set_level_of_detail(app%opt%lod)
        if (app%opt%dashed) call app%curve%set_pattern(4, int(z'0F0F'))
        app%drawing = DrawableTrajectory('Orbit')
        call hide_decorations(app%drawing)
        call app%drawing%add_artist(app%curve)
        app%craft = Sphere('Spacecraft', radius=200.0_dp, color=[1.0_dp, 0.3_dp, 0.2_dp])
        call hide_decorations(app%craft)
        call app%craft%follow(app%traj, data=FOLLOW_POSITION, mode=FOLLOW_LOOP)
        if (.not. (app%root%add_child(app%earth) .and. app%root%add_child(app%drawing) .and. &
                   app%root%add_child(app%craft))) error stop 'add_child failed'
        call app%scene%set_frame(app%root)
        app%view = View(app%root, app%earth)
        call app%view%set_default_view_distance(view_distance)
    end subroutine build_scene

    !> Add points up to `n` (in bulk, or one at a time with --per-point).
    subroutine add_points(n)
        integer, intent(in) :: n
        integer :: i, i1, i2
        i1 = app%n_added + 1
        i2 = min(n, size(app%t))
        if (i2 < i1) return
        if (app%opt%per_point) then
            do i = i1, i2
                call app%traj%add(app%t(i), pos=app%pos(:, i))
            end do
        else
            call app%traj%add(app%t(i1:i2), pos=app%pos(:, i1:i2))
        end if
        app%n_added = i2
    end subroutine add_points

    subroutine hide_decorations(frame)
        class(ReferenceFrame), intent(inout) :: frame
        call frame%show_axes(NO_AXES)
        call frame%show_axes_labels(NO_AXES)
        call frame%show_name_label(.false.)
    end subroutine hide_decorations

    ! ==================================================================
    ! running

    !> When the window is first exposed (as the OpenFrames app starts
    !> OpenFrames): open the view (no vsync) and start the frames. The first
    !> frame is timed from here, opening the view included (the Vulkan device
    !> and swapchain, as OpenFrames' OpenGL context).
    subroutine on_expose(data)
        class(*), intent(inout), optional :: data
        type(c_ptr) :: font
        if (app%started .or. .not. app%window%isExposed()) return
        app%started = .true.
        font = get_font('')                                  ! (as the demos: read before timing the first frame)
        call app%log%init(2000000)
        app%t_start = now()
        if (.not. app%proxy%open(app%window%width(), app%window%height(), 'vkframes', &
                                 native_window=app%window%winId(), pixel_ratio=app%window%devicePixelRatio(), &
                                 vsync=app%opt%vsync)) error stop 'could not open the view'
        ! the line 2 physical pixels wide, as OpenFrames' (its widths are in pixels, ours in window units)
        call app%curve%set_width(2.0_dp / app%window%devicePixelRatio())
        call app%proxy%set_scene(app%scene)
        call app%proxy%set_background_color([0.0_dp, 0.0_dp, 0.02_dp])
        call app%proxy%add_view(app%view)
        call app%proxy%set_time_range(app%t(1), app%t(size(app%t)))
        call app%proxy%set_time_scale(20.0_dp)               ! one revolution every 5 s
        if (app%opt%paced) then                              ! as the demos: a frame per update request
            call app%events%updateRequest%connect(on_update)
            call app%window%requestUpdate()
        else
            app%frames = QTimer(app%main)
            call app%frames%timeout%connect(on_frame)
            call app%frames%start(0)
        end if
        ! the first frame now (as OpenFrames' render thread starts drawing), not
        ! after AppKit has finished with the expose (tens of ms on macOS)
        call on_frame()
    end subroutine on_expose

    !> --paced: a frame, then ask for the next one (at the display's next refresh).
    subroutine on_update(data)
        class(*), intent(inout), optional :: data
        call on_frame()
        if (app%phase /= 3) call app%window%requestUpdate()
    end subroutine on_update

    !> One frame, and the phases: the first frame (timed), the warmup, the
    !> measurement, the result.
    subroutine on_frame(data)
        class(*), intent(inout), optional :: data
        real(dp) :: t
        if (app%phase == 3) return
        if (.not. app%proxy%frame()) return
        t = now()
        call app%log%record(t)
        select case (app%phase)
        case (0)
            app%first_frame_s = t - app%t_start
            app%rss_load = peak_rss_mb()
            app%t0 = t + app%opt%warmup
            app%phase = 1
            call show_status('warming up')
        case (1)
            if (t >= app%t0) call start_measuring(t)
        case (2)
            if (t >= app%t1) call finish()
        end select
    end subroutine on_frame

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

    subroutine finish()
        app%cpu1 = cpu_seconds()
        app%phase = 3
        if (.not. app%opt%paced) call app%frames%stop()
        if (app%opt%scenario == 'grow') call app%appends%stop()
        call write_result(app%opt, 'vkframes', 'VulkanSceneGraph', &
                          trim(merge('add, per point', 'add, bulk     ', app%opt%per_point)), app%insert_s, &
                          app%first_frame_s, app%log, app%t0, app%t1, app%appended, app%cpu1 - app%cpu0, app%rss_load)
        if (allocated(app%opt%capture)) then
            if (app%proxy%capture(app%opt%capture)) write(*, '(a)') 'captured '//app%opt%capture
        end if
        call app%main%close()
    end subroutine finish

    subroutine on_resize(event, data)
        type(QResizeEvent), intent(in) :: event
        class(*), intent(inout), optional :: data
        real(dp) :: dpr
        if (.not. app%proxy%is_open()) return
        dpr = app%window%devicePixelRatio()
        call app%proxy%resize(nint(event%width() * dpr), nint(event%height() * dpr))
    end subroutine on_resize

    subroutine on_close(event, data)
        type(QCloseEvent), intent(in) :: event
        class(*), intent(inout), optional :: data
        call app%proxy%close()
    end subroutine on_close

    subroutine build_ui()
        app%main = QMainWindow()
        call app%main%setWindowTitle('vkframes benchmark: '//app%opt%scenario)
        call app%main%closeEvent%connect(on_close)
        app%window = QWindow()
        app%container = QWidget_createWindowContainer(app%window)
        call app%container%setFixedSize(app%opt%width, app%opt%height)
        call app%main%setCentralWidget(app%container)
        app%events = QtfEventFilter(app%window)
        call app%events%resizeEvent%connect(on_resize)
        call app%events%exposeEvent%connect(on_expose)
        call show_status('starting')
    end subroutine build_ui

    subroutine show_status(text)
        character(len=*), intent(in) :: text
        type(QStatusBar) :: bar
        bar = app%main%statusBar()
        call bar%showMessage(text)
    end subroutine show_status

end module vkbench

!> The vkframes benchmark app (see the module documentation).
program vkframes_bench

    use qt
    use bench_common
    use vkbench
    implicit none

    type(QApplication) :: qapp
    integer :: ierr

    qapp = QApplication()
    call parse_options(app%opt)
    call build_scene()
    call build_ui()
    call app%main%show()                                    ! (the view is opened when exposed: on_expose)
    ierr = qapp%exec()

end program vkframes_bench
