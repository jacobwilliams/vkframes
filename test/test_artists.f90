!> Checks CurveArtist and DrawableTrajectory without a window: the points
!> drawn (read back from the line buffers) match the trajectory to double
!> precision after a bulk load, many appends (more line chunks, each uploaded
!> only while filling), a clear, a changed last point and other data sources;
!> trace mode draws the points with times up to the current time (the first
!> points for increasing times, the last for decreasing ones) and ends at the
!> interpolated point. SegmentArtist: segments at every stride-th
!> point from the start to the end sources, appends, trace mode; relative ends
!> (a vector per point drawn from the positions, its scale changed, zero
!> vectors drawn as nothing: both ends the same). MarkerArtist:
!> start and end markers, intermediate markers by data, time and distance,
!> from the start or the end, appends, trace mode. Point colors: a curve of a
!> trajectory with colors is drawn in them (colors added after the positions,
!> with appended points, in bulk), points without one in the artist's color,
!> and all in the artist's color with point colors off. RibbonArtist: the
!> pairs (origin and position, or other sources) drawn to double precision
!> across chunks, appends, trace mode (the quad to the interpolated pair),
!> point colors with the artist's alpha. Marker point colors: off by default
!> (each kind's color); on, the start and end in their points' colors, the
!> intermediate markers in the color at their time (blended between points),
!> points without a color in the marker's, and colors added later drawn.
!> Empty trajectories: every artist
!> draws nothing (and then the points added), a frame follows nothing. Highlights: set on an artist, and
!> on a DrawableTrajectory for its curve and segment artists.
program test_artists

    use, intrinsic :: iso_c_binding, only: c_ptr, c_null_ptr
    use vkframes
    use vkf_line_chain, only: max_chunk, LineChain, CHAIN_STRIP, split_points
    use, intrinsic :: iso_c_binding, only: c_float
    use vkf_trajectory, only: SOURCE_POSOPT, SOURCE_TIME, SOURCE_ZERO
    use vkf_layer, only: vkf_switch_create

    implicit none

    real(wp), parameter :: r = 4.2e7_wp            ! large coordinates: floats alone would lose metres
    integer :: failures

    failures = 0
    call check_bulk_and_appends()
    call check_large_bulk()
    call check_level_of_detail()
    call check_trace(1.0_wp)
    call check_trace(-1.0_wp)
    call check_sources()
    call check_point_colors()
    call check_segments()
    call check_relative_segments()
    call check_ribbon()
    call check_markers()
    call check_drawable()
    call check_empty()
    call check_marker_colors()

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
        do i = a%first_drawn(), a%first_drawn() + a%num_drawn() - 1
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

    !> A bulk load larger than max_chunk: chunks of at most max_chunk points
    !> (consecutive chunks of a strip share a point), drawn and measured as one
    !> chunk would be.
    subroutine check_large_bulk()
        type(Trajectory), target :: traj
        type(CurveArtist) :: a
        real(wp), allocatable :: t(:), pos(:,:)
        integer :: i, n
        n = 3 * max_chunk + 5
        allocate(t(n), pos(3, n))
        do i = 1, n
            t(i) = real(i, wp)
            pos(:, i) = orbit(t(i))
        end do
        traj = Trajectory()
        call traj%add(t, pos)
        a = CurveArtist(traj)
        call a%build(c_null_ptr, vkf_switch_create())
        call a%update(0.0_wp)
        call check(a%num_drawn() == n, 'large bulk load: all points drawn')
        call check(a%num_chunks() == ceiling(real(n - 1) / real(max_chunk - 1)), 'large bulk load: max_chunk chunks')
        call check(drawn_error(a, traj) < 1.0e-6_wp, 'large bulk load: points drawn to double precision')
        call check(distance_error(a, traj) < 1.0e-12_wp, 'large bulk load: distances along the curve')
    end subroutine check_large_bulk

    !> Level of detail: each chunk's bound at stride 2^k holds for every point
    !> skipped (its distance from the segment drawn over it: every stride-th
    !> point and the last), is not much larger than the largest such distance,
    !> and is the same whether the points came at once or in pieces.
    subroutine check_level_of_detail()
        type(Trajectory), target :: once
        type(CurveArtist) :: a
        type(LineChain) :: bulk, pieces
        real(wp), allocatable :: e(:), eb(:), x(:,:)
        real(wp) :: worst, ratio
        integer :: i, k, n
        logical :: valid, same
        n = 10007                                    ! (not a multiple of any stride)
        once = Trajectory()
        allocate(x(3, n))
        do i = 1, n
            x(:, i) = orbit(real(i, wp)) + 30.0_wp * [sin(0.7_wp * i), cos(1.3_wp * i), sin(0.1_wp * i)]   ! with noise
            call once%add(real(i, wp), x(:, i))
        end do
        a = CurveArtist(once)
        call a%build(c_null_ptr, vkf_switch_create())
        call a%update(0.0_wp)
        call check(a%num_chunks() == 1, 'level of detail: one chunk')
        e = a%lod_error(1)
        ! the same points in one chunk, written at once and in pieces
        call write_chain(bulk, x, n)
        call write_chain(pieces, x, 777)
        eb = real(pieces%lod_error(1), wp)
        call check(all(abs(real(bulk%lod_error(1), wp) - e) <= 1.0e-6_wp * max(1.0_wp, e)), &
                   'level of detail: a chain written at once, as the curve')
        valid = .true.
        ratio = 0.0_wp
        do k = 1, size(e)
            worst = largest_deviation(once, 2**k)
            if (worst > e(k) * (1.0_wp + 1.0e-6_wp) + 1.0e-9_wp) valid = .false.
            if (worst > 0.0_wp) ratio = max(ratio, e(k) / worst)
        end do
        same = all(abs(e - eb) <= 1.0e-6_wp * max(1.0_wp, abs(e)))
        write(*, '(a,f6.2)') 'level of detail: bound / largest deviation at most ', ratio
        call check(valid, 'level of detail: the bounds hold for every point skipped')
        call check(ratio < 8.0_wp, 'level of detail: the bounds are not much larger than the deviations')
        call check(same, 'level of detail: the same bounds from pieces as at once')
        call check(e(1) > 0.0_wp .and. all(e(2:) >= e(:size(e)-1)), 'level of detail: bounds grow with the stride')
        call a%set_level_of_detail(.false.)
        call check(.not. a%level_of_detail(), 'level of detail: off')
    end subroutine check_level_of_detail

    !> A strip chain of the points x, reserved for all of them (one chunk) and
    !> written `piece` points at a time.
    subroutine write_chain(chain, x, piece)
        type(LineChain), intent(inout) :: chain
        real(wp), intent(in) :: x(:,:)
        integer, intent(in) :: piece
        real(c_float), dimension(:,:,:), pointer :: p
        integer :: i1, i2, j, lo, hi
        call chain%init(vkf_switch_create(), CHAIN_STRIP)
        call chain%reserve(size(x, 2))
        do i1 = 1, size(x, 2), piece
            i2 = min(i1 + piece - 1, size(x, 2))
            do j = 1, chain%num_chunks()
                call chain%slice(j, i1, i2, lo, hi, p)
                if (associated(p)) call split_points(x(:, lo:hi), p)
            end do
            call chain%written(i1, i2, x)
        end do
    end subroutine write_chain

    !> The largest distance of a point from the segment drawn over it at stride s.
    real(wp) function largest_deviation(traj, s) result(worst)
        type(Trajectory), intent(in) :: traj
        integer, intent(in) :: s
        real(wp) :: a(3), b(3), v(3), ab(3), t
        integer :: n, i, i0, i1
        n = traj%num_positions()
        worst = 0.0_wp
        do i0 = 1, n - 1, s
            i1 = min(i0 + s, n)
            a = traj%position(i0)
            b = traj%position(i1)
            ab = b - a
            do i = i0 + 1, i1 - 1
                v = traj%position(i)
                t = max(0.0_wp, min(1.0_wp, dot_product(v - a, ab) / dot_product(ab, ab)))
                worst = max(worst, norm2(v - (a + t * ab)))
            end do
        end do
    end function largest_deviation

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
        do i = 1, 200                                  ! point i at time dir * 10 (i - 1)
            call traj%add(dir * 10.0_wp * real(i - 1, wp), orbit(10.0_wp * real(i, wp)))
        end do
        a = CurveArtist(traj)
        call a%set_trace_mode(.true.)
        call a%build(c_null_ptr, vkf_switch_create())

        ! between points 51 and 52: the points with times up to it; the tail half way to the next in time
        call a%update(dir * 505.0_wp)
        if (dir > 0.0_wp) then
            call check(a%num_drawn() == 51 .and. a%first_drawn() == 1, what//'points passed (the first ones)')
        else
            call check(a%num_drawn() == 149 .and. a%first_drawn() == 52, what//'points passed (the last ones)')
        end if
        expected = 0.5_wp * (orbit(510.0_wp) + orbit(520.0_wp))
        call check(a%trace_end(v), what//'tail shown')
        call check(maxval(abs(v - expected)) < 1.0e-6_wp, what//'tail ends at the interpolated point')
        call check(drawn_error(a, traj) < 1.0e-6_wp, what//'points drawn')

        call a%update(dir * 500.0_wp)                  ! at point 51
        if (dir > 0.0_wp) then
            call check(a%num_drawn() == 51 .and. a%trace_end(v), what//'at a point')
        else
            call check(a%num_drawn() == 150 .and. a%first_drawn() == 51 .and. a%trace_end(v), what//'at a point')
        end if
        call check(maxval(abs(v - orbit(510.0_wp))) < 1.0e-6_wp, what//'at a point: tail of zero length')

        call a%update(-1.0e6_wp)                       ! before the earliest time
        call check(a%num_drawn() == 0 .and. .not. a%trace_end(v), what//'before the start: nothing')
        call a%update(1.0e6_wp)                        ! after the latest
        call check(a%num_drawn() == 200 .and. a%first_drawn() == 1 .and. .not. a%trace_end(v), &
                   what//'after the end: everything')

        call a%set_trace_mode(.false.)
        call a%update(dir * 505.0_wp)
        call check(a%num_drawn() == 200 .and. .not. a%trace_end(v), what//'off: everything')
    end subroutine check_trace

    !> RibbonArtist: from the origin to the positions, in chunks; trace mode;
    !> other sources (a curtain down to z = 0); point colors.
    subroutine check_ribbon()
        type(Trajectory), target :: traj
        type(RibbonArtist) :: a
        real(wp) :: s(3), e(3), worst, expected(3)
        real(wp), parameter :: green(3) = [0.0_wp, 1.0_wp, 0.0_wp]
        integer :: i
        traj = Trajectory()
        do i = 1, 1000
            call traj%add(real(i, wp), orbit(real(i, wp)))
        end do
        a = RibbonArtist(traj, color=[0.2_wp, 0.6_wp, 1.0_wp])
        call check(all(a%color() == [0.2_wp, 0.6_wp, 1.0_wp, 0.25_wp]), 'ribbon: color, alpha 0.25 by default')
        call a%build(c_null_ptr, vkf_switch_create())
        call a%update(0.0_wp)
        call a%drawn_pair(500, s, e)
        call check(a%num_drawn() == 1000 .and. all(s == 0.0_wp) .and. maxval(abs(e - traj%position(500))) < 1.0e-6_wp, &
                   'ribbon: from the origin to the positions, double precision')

        ! appends: more chunks (sharing their last pair), every pair as written
        do i = 1001, 150000
            call traj%add(real(i, wp), orbit(real(i, wp)))
            if (modulo(i, 10000) == 0) call a%update(0.0_wp)
        end do
        call a%update(0.0_wp)
        worst = 0.0_wp
        do i = 1, a%num_drawn()
            call a%drawn_pair(i, s, e)
            worst = max(worst, maxval(abs(s)), maxval(abs(e - traj%position(i))))
        end do
        call check(a%num_drawn() == 150000 .and. a%num_chunks() > 2 .and. worst < 1.0e-6_wp, &
                   'ribbon: appends, across chunks')

        ! trace mode: the quads passed, and one to the pair at the time
        call a%set_trace_mode(.true.)
        call a%update(500.5_wp)
        expected = 0.5_wp * (orbit(500.0_wp) + orbit(501.0_wp))
        call check(a%num_drawn() == 500 .and. a%trace_end(s, e), 'ribbon trace: points passed, tail')
        call check(all(abs(s) < 1.0e-9_wp) .and. maxval(abs(e - expected)) < 1.0e-6_wp, &
                   'ribbon trace: to the interpolated pair')
        call a%update(1.0e9_wp)
        call check(a%num_drawn() == 150000 .and. .not. a%trace_end(s, e), 'ribbon trace: after the end, all')
        call a%set_trace_mode(.false.)

        ! a curtain: from the positions' (x, y, 0) up to the positions
        call check(a%set_start_x_data(DataSource(SOURCE_POSOPT, element=1)) .and. &
                   a%set_start_y_data(DataSource(SOURCE_POSOPT, element=2)), 'ribbon: start sources')
        call a%update(0.0_wp)
        call a%drawn_pair(77, s, e)
        expected = traj%position(77)
        call check(maxval(abs(s - [expected(1:2), 0.0_wp])) < 1.0e-6_wp .and. maxval(abs(e - expected)) < 1.0e-6_wp, &
                   'ribbon: other sources (a curtain)')

        ! point colors: the trajectory's, with the artist's alpha
        call traj%clear()
        do i = 1, 100
            call traj%add(real(i, wp), orbit(real(i, wp)), color=green)
        end do
        call a%update(0.0_wp)
        call check(all(abs(a%drawn_color(50) - [green, 0.25_wp]) < 1.0e-7_wp), "ribbon: point colors, the artist's alpha")
        call a%set_point_colors(.false.)
        call a%update(0.0_wp)
        call check(all(a%drawn_color(50) == a%color()) .and. a%num_drawn() == 100, "ribbon: point colors off")
    end subroutine check_ribbon

    !> A trajectory's colors: drawn when it has some (the chain made again with
    !> colors), the artist's color for points without one yet, colors added
    !> later redrawn; off: the artist's color.
    subroutine check_point_colors()
        type(Trajectory), target :: traj
        type(CurveArtist) :: a
        real(wp), parameter :: red(4) = [1.0_wp, 0.0_wp, 0.0_wp, 1.0_wp], blue(3) = [0.0_wp, 0.0_wp, 1.0_wp]
        real(wp), parameter :: green(4) = [0.0_wp, 1.0_wp, 0.0_wp, 0.5_wp], yellow(3) = [1.0_wp, 1.0_wp, 0.0_wp]
        real(wp), allocatable :: t(:), pos(:,:), col(:,:)
        real(wp) :: v(3)
        integer :: i
        traj = Trajectory()
        do i = 1, 100
            call traj%add(real(i, wp), orbit(real(i, wp)))
        end do
        a = CurveArtist(traj, color=red)
        call a%build(c_null_ptr, vkf_switch_create())
        call a%update(0.0_wp)
        call check(all(a%drawn_color(50) == red), "point colors: none, the artist's color")

        ! colors for the first 60 points: the chain made again, with colors
        do i = 1, 60
            if (i <= 30) then
                call traj%add_color(blue)
            else
                call traj%add_color(green)
            end if
        end do
        call a%update(0.0_wp)
        call check(a%num_drawn() == 100 .and. drawn_error(a, traj) < 1.0e-6_wp, 'point colors: all points drawn')
        call check(all(a%drawn_color(10) == [blue, 1.0_wp]) .and. all(abs(a%drawn_color(45) - green) < 1.0e-7_wp), &
                   "point colors: the trajectory's")
        call check(all(a%drawn_color(80) == red), "point colors: without one yet, the artist's color")

        ! the rest of the colors, then appended points with colors
        do i = 61, 100
            call traj%add_color(yellow)
        end do
        call a%update(0.0_wp)
        call check(all(a%drawn_color(80) == [yellow, 1.0_wp]), 'point colors: added later, redrawn')
        do i = 101, 3000
            call traj%add(real(i, wp), orbit(real(i, wp)), color=blue)
        end do
        call a%update(0.0_wp)
        call check(a%num_drawn() == 3000 .and. all(a%drawn_color(2500) == [blue, 1.0_wp]) .and. &
                   drawn_error(a, traj) < 1.0e-6_wp, 'point colors: appended points')

        ! bulk, after a clear
        call traj%clear()
        allocate(t(200), pos(3, 200), col(4, 200))
        do i = 1, 200
            t(i) = real(i, wp)
            pos(:, i) = orbit(t(i))
            col(:, i) = [real(i, wp) / 200.0_wp, 0.0_wp, 1.0_wp - real(i, wp) / 200.0_wp, 1.0_wp]
        end do
        call traj%add(t, pos, color=col)
        call a%update(0.0_wp)
        call check(a%num_drawn() == 200 .and. all(abs(a%drawn_color(150) - col(:, 150)) < 1.0e-7_wp), &
                   'point colors: bulk')

        ! trace mode: the tail drawn
        call a%set_trace_mode(.true.)
        call a%update(100.5_wp)
        call check(a%num_drawn() == 100 .and. a%trace_end(v), 'point colors: trace mode')
        call a%set_trace_mode(.false.)

        ! off: the artist's color
        call a%set_point_colors(.false.)
        call a%update(0.0_wp)
        call check(.not. a%point_colors() .and. all(a%drawn_color(150) == red) .and. a%num_drawn() == 200 .and. &
                   drawn_error(a, traj) < 1.0e-6_wp, "point colors off: the artist's color")
        call a%set_point_colors(.true.)
        call a%update(0.0_wp)
        call check(all(abs(a%drawn_color(150) - col(:, 150)) < 1.0e-7_wp), 'point colors on again')
    end subroutine check_point_colors

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

    !> Relative ends: a thrust vector per point (zero while coasting), drawn
    !> from each position at the end sources' scale.
    subroutine check_relative_segments()
        type(Trajectory), target :: traj
        type(SegmentArtist) :: thrust
        real(wp) :: a(3), b(3)
        integer :: i
        logical :: zero_ok
        traj = Trajectory(nopt=1)
        do i = 1, 300
            call traj%add(real(i, wp), orbit(real(i, wp)))
            call traj%set_optional(1, thrust_at(i))
        end do
        thrust = SegmentArtist(traj, color=[1.0_wp, 0.4_wp, 0.1_wp])
        call check(thrust%set_start_data(position_sources(3)) .and. thrust%set_end_data(optional_sources(1, scale=2.0_wp)), &
                   'relative segments: sources')
        call thrust%set_end_relative(.true.)
        call check(thrust%end_relative(), 'relative segments: on')
        call thrust%build(c_null_ptr, vkf_switch_create())
        call thrust%update(0.0_wp)
        call check(thrust%num_drawn() == 300, 'relative segments: one per point')
        call check(relative_error(thrust, 2.0_wp, zero_ok) < 1.0e-6_wp, 'relative segments: position + 2 x the vector')
        call check(zero_ok, 'relative segments: no thrust, both ends the same')

        ! a new scale: the same trajectory, the segments written again
        call thrust%set_end_scale(50.0_wp)
        call thrust%update(0.0_wp)
        call check(relative_error(thrust, 50.0_wp, zero_ok) < 1.0e-6_wp .and. zero_ok, 'relative segments: scale changed')

        ! absolute ends again: to the scaled vector itself
        call thrust%set_end_relative(.false.)
        call thrust%update(0.0_wp)
        call thrust%drawn_segment(10, a, b)
        call check(maxval(abs(b - 50.0_wp * thrust_at(10))) < 1.0e-6_wp, 'relative segments: off again')
    end subroutine check_relative_segments

    !> The thrust at point i (check_relative_segments): along the velocity for
    !> points 1-50 and 251-300, none between.
    function thrust_at(i) result(v)
        integer, intent(in) :: i
        real(wp) :: v(3), d(3)
        v = 0.0_wp
        d = orbit(real(i, wp) + 0.5_wp) - orbit(real(i, wp) - 0.5_wp)
        if (i <= 50 .or. i > 250) v = 3.0_wp * d / norm2(d)
    end function thrust_at

    !> The largest error of the drawn segments from position + scale x thrust,
    !> and whether those without thrust have both ends the same.
    real(wp) function relative_error(thrust, scale, zero_ok) result(e)
        type(SegmentArtist), intent(in) :: thrust
        real(wp), intent(in) :: scale
        logical, intent(out) :: zero_ok
        real(wp) :: a(3), b(3), f(3)
        integer :: s
        e = 0.0_wp
        zero_ok = .true.
        do s = 1, thrust%num_drawn()
            call thrust%drawn_segment(s, a, b)
            f = thrust_at(s)
            e = max(e, maxval(abs(a - orbit(real(s, wp)))), maxval(abs(b - (orbit(real(s, wp)) + scale * f))))
            if (all(f == 0.0_wp) .and. any(a /= b)) zero_ok = .false.
        end do
    end function relative_error

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
        type(MarkerArtist) :: a, origin, bare
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
        ! no trajectory and the default sources (OpenFrames' marker at a frame's
        ! position): the same
        bare = MarkerArtist(markers=MARKER_START, size=18.0_wp)
        call bare%build(c_null_ptr, vkf_switch_create())
        call bare%update(0.0_wp)
        call check(marker_at(bare, MARKER_START, 0.0_wp) .and. .not. bare%drawn_marker(MARKER_END, v), &
                   'markers: no trajectory, at the origin')
    end subroutine check_markers

    subroutine check_drawable()
        type(Trajectory), target :: traj
        type(CurveArtist), target :: a, b
        type(SegmentArtist), target :: s
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
        s = SegmentArtist(traj)
        call dt%add_artist(s)
        call check(a%highlight() == HIGHLIGHT_NONE .and. s%highlight() == HIGHLIGHT_NONE, 'not highlighted at first')
        call a%set_highlight(HIGHLIGHT_PULSE, color=[1.0_wp, 1.0_wp, 0.0_wp])
        call check(a%highlight() == HIGHLIGHT_PULSE .and. b%highlight() == HIGHLIGHT_NONE, 'an artist highlighted')
        call dt%set_highlight(HIGHLIGHT_PULSE + HIGHLIGHT_ARROWS, arrow_spacing=30.0_wp)
        call dt%update(0.0_wp)
        call check(a%highlight() == HIGHLIGHT_PULSE + HIGHLIGHT_ARROWS .and. &
                   b%highlight() == HIGHLIGHT_PULSE + HIGHLIGHT_ARROWS .and. &
                   s%highlight() == HIGHLIGHT_PULSE + HIGHLIGHT_ARROWS, 'the frame highlights its line artists')
        call check(s%num_drawn() == 100, 'highlighted segments drawn')
        call dt%set_highlight(HIGHLIGHT_NONE)
        call check(a%highlight() == HIGHLIGHT_NONE .and. s%highlight() == HIGHLIGHT_NONE, 'highlights cleared')
        call dt%remove_all_artists()
        call check(dt%num_artists() == 0, 'remove_all_artists')
    end subroutine check_drawable

    !> An empty trajectory: each kind of artist draws nothing (no crash: a
    !> marker's start once read point 1), then the points added; a frame
    !> following it stays where it is.
    subroutine check_empty()
        type(Trajectory), target :: traj
        type(CurveArtist), target :: c
        type(SegmentArtist), target :: s
        type(MarkerArtist), target :: m
        type(RibbonArtist), target :: r
        type(DrawableTrajectory), target :: dt
        type(ReferenceFrame), target :: root, craft
        integer :: i
        traj = Trajectory()
        c = CurveArtist(traj)
        call c%set_highlight(HIGHLIGHT_ARROWS)
        s = SegmentArtist(traj)
        m = MarkerArtist(traj, markers=ior(ior(MARKER_START, MARKER_INTERMEDIATE), MARKER_END))
        call m%set_intermediate_type(INTERMEDIATE_DATA)
        r = RibbonArtist(traj)
        dt = DrawableTrajectory('Empty')
        call dt%add_artist(c)
        call dt%add_artist(s)
        call dt%add_artist(m)
        call dt%add_artist(r)
        call dt%realize(c_null_ptr)
        call dt%update(0.0_wp)
        call check(c%num_drawn() == 0 .and. s%num_drawn() == 0 .and. m%num_intermediate() == 0 .and. &
                   r%num_drawn() == 0, 'empty trajectory: nothing drawn')
        root = ReferenceFrame('Root')
        craft = ReferenceFrame('Craft')
        call craft%set_position([1.0_wp, 2.0_wp, 3.0_wp])
        call craft%follow(traj, mode=FOLLOW_LIMIT)
        if (.not. root%add_child(craft)) error stop 'add_child failed'
        call root%update(0.0_wp)
        call check(all(craft%position() == [1.0_wp, 2.0_wp, 3.0_wp]), 'empty trajectory: a follower stays')
        do i = 1, 10
            call traj%add(real(i, wp), orbit(real(i, wp)))
        end do
        call dt%update(0.0_wp)
        call check(c%num_drawn() == 10 .and. m%num_intermediate() == 8, 'empty trajectory: then the points added')
    end subroutine check_empty

    !> MarkerArtist point colors (read back from the marker buffers).
    subroutine check_marker_colors()
        type(Trajectory), target :: traj
        type(MarkerArtist) :: a, b
        real(wp), parameter :: red(4) = [1.0_wp, 0.0_wp, 0.0_wp, 1.0_wp], blue(4) = [0.0_wp, 0.0_wp, 1.0_wp, 1.0_wp]
        real(wp), parameter :: own(4) = [0.5_wp, 0.5_wp, 0.5_wp, 1.0_wp]
        real(wp), parameter :: tol = 1.0e-6_wp
        integer :: i
        traj = Trajectory()
        do i = 1, 5                                    ! t = 1 .. 5: red, blue, red, blue, red
            call traj%add(real(i, wp), pos=[real(i, wp), 0.0_wp, 0.0_wp], color=merge(red, blue, mod(i, 2) == 1))
        end do
        a = MarkerArtist(traj, markers=ior(ior(MARKER_START, MARKER_INTERMEDIATE), MARKER_END))
        call a%set_marker_color(ior(ior(MARKER_START, MARKER_INTERMEDIATE), MARKER_END), own)
        call a%set_intermediate_type(INTERMEDIATE_DATA)
        call a%build(c_null_ptr, vkf_switch_create())
        call a%update(0.0_wp)
        call check(.not. a%point_colors() .and. all(a%drawn_color(MARKER_START) == own) .and. &
                   all(a%drawn_color(MARKER_INTERMEDIATE, 1) == own), 'marker colors: off by default (their own)')
        call a%set_point_colors(.true.)
        call a%update(0.0_wp)
        call check(all(abs(a%drawn_color(MARKER_START) - red) < tol) .and. all(abs(a%drawn_color(MARKER_END) - red) < tol), &
                   'marker colors: start and end in their points'' colors')
        call check(a%num_intermediate() == 3, 'marker colors: the intermediate markers')
        call check(all(abs(a%drawn_color(MARKER_INTERMEDIATE, 1) - blue) < tol) .and. &
                   all(abs(a%drawn_color(MARKER_INTERMEDIATE, 2) - red) < tol), 'marker colors: each at its point')
        call a%set_point_colors(.false.)
        call a%update(0.0_wp)
        call check(all(a%drawn_color(MARKER_INTERMEDIATE, 1) == own), 'marker colors: off again')

        ! intermediate markers by time, between points: blended; points without a color: the marker's own
        b = MarkerArtist(traj, markers=MARKER_INTERMEDIATE)
        call b%set_marker_color(MARKER_INTERMEDIATE, own)
        call b%set_intermediate_type(INTERMEDIATE_TIME)
        call b%set_intermediate_spacing(0.5_wp)
        call b%set_point_colors(.true.)
        call b%build(c_null_ptr, vkf_switch_create())
        call b%update(0.0_wp)
        call check(all(abs(b%drawn_color(MARKER_INTERMEDIATE, 1) - 0.5_wp * (red + blue)) < tol), &
                   'marker colors: blended between points (by time)')
        call traj%add(6.0_wp, pos=[6.0_wp, 0.0_wp, 0.0_wp])                    ! (no color yet)
        call traj%add(7.0_wp, pos=[7.0_wp, 0.0_wp, 0.0_wp])
        call b%update(0.0_wp)
        call check(all(abs(b%drawn_color(MARKER_INTERMEDIATE, b%num_intermediate()) - own) < tol), &
                   'marker colors: a point without one in the marker''s own')
        call traj%add_color(blue)
        call traj%add_color(blue)
        call b%update(0.0_wp)
        call check(all(abs(b%drawn_color(MARKER_INTERMEDIATE, b%num_intermediate()) - blue) < tol), &
                   'marker colors: colors added later drawn')
    end subroutine check_marker_colors

end program test_artists
