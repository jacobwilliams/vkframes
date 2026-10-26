!> Checks CurveArtist and DrawableTrajectory without a window: the points
!> drawn (read back from the line buffers) match the trajectory to double
!> precision after a bulk load, many appends (more line chunks, each uploaded
!> only while filling), a clear, a changed last point and other data sources;
!> trace mode draws the points passed and ends at the interpolated point, for
!> increasing and decreasing times. SegmentArtist: segments at every stride-th
!> point from the start to the end sources, appends, trace mode. MarkerArtist:
!> start and end markers, intermediate markers by data, time and distance,
!> from the start or the end, appends, trace mode.
program test_artists

    use, intrinsic :: iso_c_binding, only: c_ptr, c_null_ptr
    use vkframes
    use vkf_line_chain, only: max_chunk
    use vkf_trajectory, only: SOURCE_POSOPT, SOURCE_TIME, SOURCE_ZERO
    use vkf_layer, only: vkf_switch_create

    implicit none

    real(wp), parameter :: r = 4.2e7_wp            ! large coordinates: floats alone would lose metres
    integer :: failures

    failures = 0
    call check_bulk_and_appends()
    call check_trace(1.0_wp)
    call check_trace(-1.0_wp)
    call check_sources()
    call check_segments()
    call check_markers()
    call check_drawable()

    if (failures > 0) then
        write(*, '(a,i0,a)') 'FAIL: ', failures, ' checks failed'
        error stop 1
    end if
    write(*, '(a)') 'PASS: CurveArtist, SegmentArtist, MarkerArtist, DrawableTrajectory'

contains

    subroutine check(ok, what)
        logical, intent(in) :: ok
        character(len=*), intent(in) :: what
        if (.not. ok) then
            write(*, '(a)') '  failed: '//what
            failures = failures + 1
        end if
    end subroutine check

    pure function orbit(t) result(p)
        real(wp), intent(in) :: t
        real(wp) :: p(3)
        p = r * [cos(1.0e-3_wp * t), sin(1.0e-3_wp * t), 1.0e-3_wp * sin(3.0e-3_wp * t)] + [0.123456789_wp, 0.0_wp, 0.0_wp]
    end function orbit

    !> Largest difference between the drawn points and the trajectory's.
    real(wp) function drawn_error(a, traj) result(err)
        type(CurveArtist), intent(in) :: a
        type(Trajectory), intent(in) :: traj
        integer :: i
        err = 0.0_wp
        do i = 1, a%num_drawn()
            err = max(err, maxval(abs(a%drawn_point(i) - traj%position(i))))
        end do
    end function drawn_error

    !> Largest relative difference between the drawn distances along the curve
    !> and the trajectory's.
    real(wp) function distance_error(a, traj) result(err)
        type(CurveArtist), intent(in) :: a
        type(Trajectory), intent(in) :: traj
        real(wp) :: s
        integer :: i
        err = abs(a%drawn_distance(1))
        s = 0.0_wp
        do i = 2, a%num_drawn()
            s = s + norm2(traj%position(i) - traj%position(i - 1))
            err = max(err, abs(a%drawn_distance(i) - s) / s)
        end do
    end function distance_error

    subroutine check_bulk_and_appends()
        type(Trajectory), target :: traj
        type(CurveArtist) :: a
        real(wp), allocatable :: t(:), pos(:,:)
        integer :: i, k, n0, chunks
        n0 = 1000
        traj = Trajectory()
        allocate(t(n0), pos(3, n0))
        do i = 1, n0
            t(i) = real(i, wp)
            pos(:, i) = orbit(t(i))
        end do
        call traj%add(t, pos)
        a = CurveArtist(traj, color=[1.0_wp, 0.0_wp, 0.0_wp], width=2.0_wp)
        call a%build(c_null_ptr, vkf_switch_create())
        call a%update(0.0_wp)
        call check(a%num_chunks() == 1 .and. a%num_drawn() == n0, 'bulk load: one chunk, all points drawn')
        call check(drawn_error(a, traj) < 1.0e-6_wp, 'bulk load: points drawn to double precision')
        call check(all(a%color() == [1.0_wp, 0.0_wp, 0.0_wp, 1.0_wp]) .and. a%width() == 2.0_wp, 'style')

        ! appends, a frame update after each
        do k = 1, 300
            do i = 1, 1000
                call traj%add(real(traj%num_times() + 1, wp), orbit(real(traj%num_times() + 1, wp)))
            end do
            call a%update(0.0_wp)
        end do
        write(*, '(a,i0,a,i0,a)') 'appends: ', a%num_drawn(), ' points in ', a%num_chunks(), ' chunks'
        call check(a%num_drawn() == n0 + 300000, 'appends: all points drawn')
        ! chunks double from the first (n0 points) up to max_chunk, then stay at max_chunk
        call check(a%num_chunks() <= 2 + ceiling(log(real(max_chunk) / n0) / log(2.0)) + (n0 + 300000) / max_chunk, &
                   'appends: few chunks')
        call check(drawn_error(a, traj) < 1.0e-6_wp, 'appends: points drawn to double precision')
        call check(distance_error(a, traj) < 1.0e-12_wp, 'appends: distances along the curve, across chunks')
        call a%set_pattern(3, int(z'00FF'))
        call a%pattern(k, i)
        call check(k == 3 .and. i == int(z'00FF'), 'dash pattern')

        ! cleared and refilled: the same chunks
        chunks = a%num_chunks()
        call traj%clear()
        do i = 1, 5000
            call traj%add(real(i, wp), 2.0_wp * orbit(real(i, wp)))
        end do
        call a%update(0.0_wp)
        call check(a%num_drawn() == 5000 .and. a%num_chunks() == chunks, 'clear: redrawn into the same chunks')
        call check(drawn_error(a, traj) < 1.0e-6_wp, 'clear: new points drawn')

        call traj%clear()
        call a%update(0.0_wp)
        call check(a%num_drawn() == 0, 'clear: nothing drawn')
    end subroutine check_bulk_and_appends

    !> Trace mode, with times increasing (dir = 1) or decreasing (dir = -1).
    subroutine check_trace(dir)
        real(wp), intent(in) :: dir
        type(Trajectory), target :: traj
        type(CurveArtist) :: a
        real(wp) :: v(3), expected(3)
        integer :: i
        character(len=:), allocatable :: what
        what = merge('trace (increasing times): ', 'trace (decreasing times): ', dir > 0.0_wp)
        traj = Trajectory()
        do i = 1, 200
            call traj%add(dir * 10.0_wp * real(i - 1, wp), orbit(10.0_wp * real(i, wp)))
        end do
        a = CurveArtist(traj)
        call a%set_trace_mode(.true.)
        call a%build(c_null_ptr, vkf_switch_create())

        call a%update(dir * 505.0_wp)                  ! between points 51 and 52
        call check(a%num_drawn() == 51, what//'points passed')
        expected = 0.5_wp * (orbit(510.0_wp) + orbit(520.0_wp))
        call check(a%trace_end(v), what//'tail shown')
        call check(maxval(abs(v - expected)) < 1.0e-6_wp, what//'tail ends at the interpolated point')
        call check(drawn_error(a, traj) < 1.0e-6_wp, what//'points drawn')

        call a%update(dir * 500.0_wp)                  ! at point 51
        call check(a%num_drawn() == 51 .and. a%trace_end(v), what//'at a point')
        call check(maxval(abs(v - orbit(510.0_wp))) < 1.0e-6_wp, what//'at a point: tail of zero length')

        call a%update(-dir * 1.0_wp)                   ! before the first point
        call check(a%num_drawn() == 0 .and. .not. a%trace_end(v), what//'before the start: nothing')
        call a%update(dir * 1.0e6_wp)                  ! after the last
        call check(a%num_drawn() == 200 .and. .not. a%trace_end(v), what//'after the end: everything')

        call a%set_trace_mode(.false.)
        call a%update(dir * 505.0_wp)
        call check(a%num_drawn() == 200 .and. .not. a%trace_end(v), what//'off: everything')
    end subroutine check_trace

    !> Other data sources, and a change to the last point (set_optional).
    subroutine check_sources()
        type(Trajectory), target :: traj
        type(CurveArtist) :: a
        real(wp) :: v(3), p(3)
        integer :: i
        traj = Trajectory(nopt=1)
        do i = 1, 10
            call traj%add(real(i, wp), orbit(real(i, wp)))
            call traj%set_optional(1, [real(i, wp), 2.0_wp * i, 3.0_wp * i])
        end do
        a = CurveArtist(traj)
        call check(a%set_x_data(DataSource(SOURCE_TIME, scale=2.0_wp)), 'x from the time')
        call check(a%set_y_data(DataSource(SOURCE_POSOPT, element=2, opt=1)), 'y from optional 1')
        call check(a%set_z_data(DataSource(SOURCE_POSOPT, element=3, opt=0)), 'z from the position')
        call check(.not. a%set_z_data(DataSource(SOURCE_POSOPT, element=1, opt=2)), 'missing optional rejected')
        call check(a%set_z_data(DataSource(SOURCE_POSOPT, element=3, opt=0)), 'z from the position again')
        call a%build(c_null_ptr, vkf_switch_create())
        call a%update(0.0_wp)
        v = a%drawn_point(4)
        p = orbit(4.0_wp)
        call check(a%num_drawn() == 10 .and. all(abs(v - [8.0_wp, 8.0_wp, p(3)]) < 1.0e-6_wp), 'points from other sources')
        call traj%set_optional(1, [0.0_wp, -1.0_wp, 0.0_wp])
        call a%update(0.0_wp)
        v = a%drawn_point(10)
        call check(v(2) == -1.0_wp, 'changed last point redrawn')
    end subroutine check_sources

    subroutine check_segments()
        type(Trajectory), target :: traj
        type(SegmentArtist) :: radius, arrows
        real(wp) :: a(3), b(3), err
        integer :: i, s
        traj = Trajectory(nopt=1)
        do i = 1, 1000
            call traj%add(real(i, wp), orbit(real(i, wp)))
            call traj%set_optional(1, 1.01_wp * orbit(real(i, wp)))
        end do

        ! default: the origin to the positions, every 7th point
        radius = SegmentArtist(traj, stride=7)
        call radius%build(c_null_ptr, vkf_switch_create())
        call radius%update(0.0_wp)
        call check(radius%num_drawn() == 143 .and. radius%stride() == 7, 'segments: every 7th point')
        err = 0.0_wp
        do s = 1, radius%num_drawn()
            call radius%drawn_segment(s, a, b)
            err = max(err, maxval(abs(a)), maxval(abs(b - orbit(real(1 + 7 * (s - 1), wp)))))
        end do
        call check(err < 1.0e-6_wp, 'segments: origin to the positions')

        ! position to optional 1, with appends
        arrows = SegmentArtist(traj, color=[0.0_wp, 1.0_wp, 0.0_wp])
        call check(arrows%set_start_x_data(DataSource(SOURCE_POSOPT, element=1)) .and. &
                   arrows%set_start_y_data(DataSource(SOURCE_POSOPT, element=2)) .and. &
                   arrows%set_start_z_data(DataSource(SOURCE_POSOPT, element=3)), 'segments: start sources')
        call check(arrows%set_end_x_data(DataSource(SOURCE_POSOPT, element=1, opt=1)) .and. &
                   arrows%set_end_y_data(DataSource(SOURCE_POSOPT, element=2, opt=1)) .and. &
                   arrows%set_end_z_data(DataSource(SOURCE_POSOPT, element=3, opt=1)), 'segments: end sources')
        call arrows%build(c_null_ptr, vkf_switch_create())
        call arrows%update(0.0_wp)
        do i = 1001, 100000
            call traj%add(real(i, wp), orbit(real(i, wp)))
            call traj%set_optional(1, 1.01_wp * orbit(real(i, wp)))
            if (modulo(i, 5000) == 0) call arrows%update(0.0_wp)
        end do
        call arrows%update(0.0_wp)
        write(*, '(a,i0,a,i0,a)') 'segments: ', arrows%num_drawn(), ' in ', arrows%num_chunks(), ' chunks'
        call check(arrows%num_drawn() == 100000, 'segments: appended')
        err = 0.0_wp
        do s = 1, arrows%num_drawn()
            call arrows%drawn_segment(s, a, b)
            err = max(err, maxval(abs(a - orbit(real(s, wp)))), maxval(abs(b - 1.01_wp * orbit(real(s, wp)))))
        end do
        call check(err < 1.0e-6_wp, 'segments: position to optional')

        ! trace mode: the segments at the points passed
        call radius%set_trace_mode(.true.)
        call radius%update(50.5_wp)                    ! points 1 .. 50 passed: segments at 1, 8, ..., 50
        call check(radius%num_drawn() == 8, 'segments: trace')
        call radius%update(0.5_wp)
        call check(radius%num_drawn() == 0, 'segments: trace before the start')
        call radius%set_trace_mode(.false.)
        call radius%update(0.5_wp)
        call check(radius%num_drawn() == (100000 - 1) / 7 + 1, 'segments: trace off, appended points too')
    end subroutine check_segments

    !> Marker `which` is drawn, at [x, 0, 0].
    logical function marker_at(a, which, x) result(ok)
        type(MarkerArtist), intent(in) :: a
        integer, intent(in) :: which
        real(wp), intent(in) :: x
        real(wp) :: v(3)
        ok = a%drawn_marker(which, v)
        if (ok) ok = maxval(abs(v - [x, 0.0_wp, 0.0_wp])) < 1.0e-6_wp
    end function marker_at

    !> Intermediate markers at x = 1000 * expected(:) (on the x axis).
    logical function markers_at(a, expected) result(ok)
        type(MarkerArtist), intent(in) :: a
        real(wp), intent(in) :: expected(:)
        real(wp) :: v(3)
        integer :: i
        ok = a%num_intermediate() == size(expected)
        if (.not. ok) then
            write(*, '(a,i0,a,i0)') '  intermediate markers: ', a%num_intermediate(), ', expected ', size(expected)
            return
        end if
        do i = 1, size(expected)
            if (.not. a%drawn_marker(MARKER_INTERMEDIATE, v, i)) ok = .false.
            if (maxval(abs(v - [1000.0_wp * expected(i), 0.0_wp, 0.0_wp])) >= 1.0e-6_wp) ok = .false.
        end do
    end function markers_at

    subroutine check_markers()
        type(Trajectory), target :: traj
        type(MarkerArtist) :: a, origin
        real(wp) :: v(3)
        integer :: i, k
        traj = Trajectory()
        do i = 1, 101                                  ! x = 1000 t, t = 0 .. 100
            call traj%add(real(i - 1, wp), [1000.0_wp * (i - 1), 0.0_wp, 0.0_wp])
        end do
        a = MarkerArtist(traj)
        call a%build(c_null_ptr, vkf_switch_create())
        call a%update(0.0_wp)
        call check(marker_at(a, MARKER_START, 0.0_wp), 'markers: start')
        call check(marker_at(a, MARKER_END, 1.0e5_wp), 'markers: end')
        call check(a%num_intermediate() == 0 .and. all(a%marker_color(MARKER_START) == [1.0_wp, 0.0_wp, 0.0_wp, 1.0_wp]), &
                   'markers: OpenFrames defaults')

        call a%set_markers(ior(ior(MARKER_START, MARKER_INTERMEDIATE), MARKER_END))
        call a%set_intermediate_spacing(10.0_wp)       ! data: points 11, 21, ..., 91
        call a%update(0.0_wp)
        call check(markers_at(a, [(10.0_wp * k, k = 1, 9)]), 'markers: every 10th point')
        call a%set_intermediate_spacing(30.0_wp)
        call a%set_intermediate_direction(MARKER_END)  ! points 71, 41, 11 (in trajectory order)
        call a%update(0.0_wp)
        call check(markers_at(a, [10.0_wp, 40.0_wp, 70.0_wp]), 'markers: data from the end')
        call a%set_intermediate_direction(MARKER_START)
        call a%set_intermediate_type(INTERMEDIATE_TIME)
        call a%set_intermediate_spacing(7.5_wp)
        call a%update(0.0_wp)
        call check(markers_at(a, [(7.5_wp * k, k = 1, 13)]), 'markers: by time')
        call a%set_intermediate_type(INTERMEDIATE_DISTANCE)
        call a%set_intermediate_spacing(2500.0_wp)     ! the last at the end (arc length reaches 100000)
        call a%update(0.0_wp)
        call check(markers_at(a, [(2.5_wp * k, k = 1, 40)]), 'markers: by distance')

        ! appends: markers continued
        call a%set_intermediate_type(INTERMEDIATE_TIME)
        call a%set_intermediate_spacing(7.5_wp)
        call a%update(0.0_wp)
        do i = 102, 151
            call traj%add(real(i - 1, wp), [1000.0_wp * (i - 1), 0.0_wp, 0.0_wp])
            if (modulo(i, 10) == 0) call a%update(0.0_wp)
        end do
        call a%update(0.0_wp)
        call check(markers_at(a, [(7.5_wp * k, k = 1, 19)]), 'markers: appended (by time)')
        call check(marker_at(a, MARKER_END, 1.5e5_wp), 'markers: end after appends')

        ! trace mode: markers passed; the end marker only once the end is reached
        call a%set_trace_mode(.true.)
        call a%update(45.5_wp)
        call check(markers_at(a, [(7.5_wp * k, k = 1, 6)]), 'markers: trace')
        call check(marker_at(a, MARKER_START, 0.0_wp) .and. .not. a%drawn_marker(MARKER_END, v), &
                   'markers: trace, end not reached')
        call a%update(150.0_wp)
        call check(marker_at(a, MARKER_END, 1.5e5_wp), 'markers: trace, at the end')
        call a%update(-1.0_wp)
        call check(.not. a%drawn_marker(MARKER_START, v) .and. .not. a%drawn_marker(MARKER_END, v) .and. &
                   a%num_intermediate() == 0, 'markers: trace before the start')
        call a%update(1000.0_wp)
        call check(marker_at(a, MARKER_END, 1.5e5_wp) .and. a%num_intermediate() == 19, 'markers: trace after the end')

        call check(.not. a%set_marker_image('does/not/exist.png') .and. a%marker_shape() == MARKER_DISK, &
                   'markers: missing image')
        call a%set_marker_shape(MARKER_TARGET)
        call check(a%marker_shape() == MARKER_TARGET, 'markers: shape')

        ! no trajectory: a marker at the origin
        origin = MarkerArtist()
        call check(origin%set_x_data(DataSource(SOURCE_ZERO)) .and. origin%set_y_data(DataSource(SOURCE_ZERO)) .and. &
                   origin%set_z_data(DataSource(SOURCE_ZERO)), 'markers: zero sources')
        call origin%build(c_null_ptr, vkf_switch_create())
        call origin%update(0.0_wp)
        call check(marker_at(origin, MARKER_START, 0.0_wp) .and. .not. origin%drawn_marker(MARKER_END, v), &
                   'markers: at the origin')
    end subroutine check_markers

    subroutine check_drawable()
        type(Trajectory), target :: traj
        type(CurveArtist), target :: a, b
        type(DrawableTrajectory), target :: dt
        class(TrajectoryArtist), pointer :: p
        integer :: i
        traj = Trajectory()
        do i = 1, 100
            call traj%add(real(i, wp), orbit(real(i, wp)))
        end do
        a = CurveArtist(traj)
        b = CurveArtist(traj)
        dt = DrawableTrajectory('Orbit')
        call dt%add_artist(a)
        call dt%add_artist(a)
        call check(dt%num_artists() == 1, 'an artist added once')
        call check(dt%frame_info() == 'DrawableTrajectory', 'frame_info')
        call dt%realize(c_null_ptr)
        call dt%update(0.0_wp)
        call check(a%num_drawn() == 100, 'artist updated with its frame')
        call dt%add_artist(b)                          ! after the frame is shown
        call dt%update(0.0_wp)
        call check(b%num_drawn() == 100, 'artist added later: built at the next update')
        p => dt%artist(2)
        call check(associated(p, b), 'artist(i)')
        call dt%remove_artist(a)
        call check(dt%num_artists() == 1 .and. associated(dt%artist(1), b), 'remove_artist')
        call dt%add_artist(a)                          ! again: built anew
        call dt%update(0.0_wp)
        call check(a%num_drawn() == 100 .and. a%num_chunks() == 1, 'artist added again')
        call dt%remove_all_artists()
        call check(dt%num_artists() == 0, 'remove_all_artists')
    end subroutine check_drawable

end program test_artists
