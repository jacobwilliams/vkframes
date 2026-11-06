!> Checks trajectories drawn in another frame of the scene (DrawableTrajectory's
!> set_data_frame), without a window: a trajectory computed in an inertial
!> frame, drawn under a "Moon" frame that follows an ephemeris (positions and
!> attitudes, sampled at the trajectory's times, so their interpolation there
!> is exact), each point where it is in the Moon's frame at its own time,
!> q_i = R(t_i)^T (p_i - o(t_i)), read back from the GPU buffers to double
!> precision: a curve, segments with relative ends (the end added to the
!> start before the transform), a ribbon (its zero start: the inertial
!> origin), start and end markers, the trace tail at a time between points.
!> A frame following the same data with `follow(..., data_frame=)` rides on
!> the curve. Poses at any time (world_matrix_at) agree with the scene after
!> an update at that time. Points appended later are transformed; a change of
!> the Moon's motion (its ephemeris), a move of the drawable to another
!> parent (a fixed, rotated frame: one transform for all points), and
!> unsetting the data frame each write the points again; a data frame in
!> another tree draws the data as they are. Slerp between close attitudes
!> stays a unit quaternion (else a frame between them scales slightly).
program test_data_frames

    use, intrinsic :: iso_c_binding, only: c_null_ptr
    use vkframes
    use vkf_checks, only: check, report

    implicit none

    real(wp), parameter :: rm = 384400.0_wp     ! the Moon's distance (km)
    real(wp), parameter :: dt = 600.0_wp        ! the data's time step (s)
    real(wp), parameter :: tol = 1.0e-6_wp      ! (km, coordinates of 1e5 to 1e6 km)
    integer, parameter :: n = 400               ! points at first

    type(ReferenceFrame), target :: eci, moon, offset, craft, other_root
    type(Trajectory), target :: eph, path
    type(DrawableTrajectory), target :: shown
    type(CurveArtist), target :: line
    type(SegmentArtist), target :: arrows
    type(MarkerArtist), target :: ends
    type(RibbonArtist), target :: fill
    real(wp) :: w = 2.6617e-6_wp                ! the Moon's rate (rad/s), changed later
    real(wp) :: err, a(3), b(3), v(3), m(4,4), t
    integer :: i

    ! the data: the Moon's ephemeris and a spacecraft's path, both inertial
    eph = Trajectory()
    call fill_ephemeris(n)
    path = Trajectory(nopt=1)
    do i = 1, n
        call path%add(time_of(i), craft_at(time_of(i)))
        call path%set_optional(1, push_at(i))
    end do

    ! the scene: the inertial root, the Moon following its ephemeris
    eci = ReferenceFrame('ECI')
    moon = ReferenceFrame('Moon')
    call moon%follow(eph, mode=FOLLOW_LIMIT)
    call check(eci%add_child(moon), 'the Moon in the scene')

    ! the path drawn in the Moon's frame, its data inertial
    line = CurveArtist(path)
    arrows = SegmentArtist(path)
    call check(arrows%set_start_data(position_sources(3)) .and. arrows%set_end_data(optional_sources(1, scale=1000.0_wp)), &
               'segments: sources')
    call arrows%set_end_relative(.true.)
    ends = MarkerArtist(path)
    fill = RibbonArtist(path)
    shown = DrawableTrajectory('Path in the Moon frame')
    call shown%add_artist(line)
    call shown%add_artist(arrows)
    call shown%add_artist(ends)
    call shown%add_artist(fill)
    call shown%set_data_frame(eci)
    call check(associated(shown%data_frame(), eci), 'data_frame')
    call check(moon%add_child(shown), 'the drawable under the Moon')

    ! a spacecraft frame under the Moon, following the inertial data
    craft = ReferenceFrame('Craft')
    call craft%follow(path, data=FOLLOW_POSITION, mode=FOLLOW_LIMIT, data_frame=eci)
    call check(associated(craft%follow_frame(), eci), 'follow_frame')
    call check(moon%add_child(craft), 'the craft under the Moon')

    call eci%realize(c_null_ptr)
    call eci%update(0.0_wp)

    ! each point at its own time
    call check(line%num_drawn() == n .and. curve_error(n) < tol, 'curve: each point in the Moon frame at its time')
    err = 0.0_wp
    do i = 1, n
        call arrows%drawn_segment(i, a, b)
        err = max(err, maxval(abs(a - in_moon(time_of(i), path%position(i)))), &
                  maxval(abs(b - in_moon(time_of(i), path%position(i) + 1000.0_wp * push_at(i)))))
    end do
    call check(arrows%num_drawn() == n .and. err < tol, 'segments: start and relative end, both at the point''s time')
    err = 0.0_wp
    do i = 1, n
        call fill%drawn_pair(i, a, b)
        err = max(err, maxval(abs(a - in_moon(time_of(i), [0.0_wp, 0.0_wp, 0.0_wp]))), &
                  maxval(abs(b - in_moon(time_of(i), path%position(i)))))
    end do
    call check(fill%num_drawn() == n .and. err < tol, 'ribbon: the inertial origin to the positions')
    call check(ends%drawn_marker(MARKER_START, a) .and. ends%drawn_marker(MARKER_END, b), 'markers drawn')
    call check(maxval(abs(a - in_moon(time_of(1), path%position(1)))) < tol .and. &
               maxval(abs(b - in_moon(time_of(n), path%position(n)))) < tol, 'markers: start and end')

    ! the scene at a time: the craft on the curve, poses at any time as after an update
    do i = 1, n, 37
        call eci%update(time_of(i))
        call check(maxval(abs(craft%position() - line%drawn_point(i))) < tol, 'the craft rides on the curve')
    end do
    t = time_of(100) + 0.37_wp * dt                       ! (between the ephemeris' points)
    call eci%update(t)
    call check(maxval(abs(moon%world_matrix_at(t) - moon%world_matrix())) < 1.0e-9_wp * rm, &
               'world_matrix_at: as after the update')
    call check(maxval(abs(craft%world_matrix_at(t) - craft%world_matrix())) < 1.0e-9_wp * rm, &
               'world_matrix_at: a frame following data in another frame')
    call check(maxval(abs(moon%world_matrix_at(time_of(7)) - expected_moon(time_of(7)))) < 1.0e-9_wp * rm, &
               'world_matrix_at: at a point of the ephemeris')

    ! trace mode: the tail to the point at a time between points
    call line%set_trace_mode(.true.)
    call eci%update(t)
    m = rigid_inverse(moon%world_matrix_at(t))
    v = path%point_between(100, 101, t, position_sources(3))
    v = matmul(m(1:3, 1:3), v) + m(1:3, 4)
    call check(line%trace_end(a), 'trace: a tail')
    call check(maxval(abs(a - v)) < tol, 'trace: the tail ends at the point at t, by the Moon''s pose at t')
    call check(maxval(abs(craft%position() - v)) < tol, 'trace: the craft at the tail''s end')
    call line%set_trace_mode(.false.)

    ! appends: the ephemeris extended (its motion changed: all written again), then the path
    call fill_ephemeris(n + 50)
    do i = n + 1, n + 50
        call path%add(time_of(i), craft_at(time_of(i)))
        call path%set_optional(1, push_at(i))
    end do
    call eci%update(0.0_wp)
    call check(line%num_drawn() == n + 50 .and. curve_error(n + 50) < tol, 'appended points: transformed')

    ! the Moon's motion changed: all the points written again
    w = 3.1e-6_wp
    call fill_ephemeris(n + 50)
    call eci%update(0.0_wp)
    call check(curve_error(n + 50) < tol, 'the Moon''s ephemeris changed: the points written again')

    ! moved to a fixed, rotated frame (one transform for all the points)
    offset = ReferenceFrame('Offset')
    call offset%set_position([1000.0_wp, -2000.0_wp, 300.0_wp])
    call offset%set_attitude(quat_from_axis_angle([1.0_wp, 1.0_wp, 0.0_wp], 0.5_wp))
    call check(eci%add_child(offset), 'a fixed frame')
    call check(moon%remove_child(shown) .and. offset%add_child(shown), 'the drawable moved to the fixed frame')
    call eci%update(0.0_wp)
    m = rigid_inverse(offset%world_matrix())
    err = 0.0_wp
    do i = 1, n + 50
        err = max(err, maxval(abs(line%drawn_point(i) - (matmul(m(1:3, 1:3), path%position(i)) + m(1:3, 4)))))
    end do
    call check(err < tol, 'moved to another parent: the points written again in its frame')

    ! the data frame unset: the data drawn as they are
    call shown%set_data_frame()
    call eci%update(0.0_wp)
    err = 0.0_wp
    do i = 1, n + 50
        err = max(err, maxval(abs(line%drawn_point(i) - path%position(i))))
    end do
    call check(.not. associated(shown%data_frame()) .and. err < tol, 'data frame unset: the data as they are')

    ! a data frame in another tree: the data drawn as they are (with a warning)
    other_root = ReferenceFrame('Elsewhere')
    call shown%set_data_frame(other_root)
    call eci%update(0.0_wp)
    err = 0.0_wp
    do i = 1, n + 50
        err = max(err, maxval(abs(line%drawn_point(i) - path%position(i))))
    end do
    call check(err < tol, 'a data frame in another tree: the data as they are')

    ! attitudes close together (0.25 deg): slerp's linear case still a unit quaternion
    v(1) = norm2(quat_slerp(0.4_wp, quat_from_axis_angle([0.0_wp, 0.0_wp, 1.0_wp], 0.0_wp), &
                            quat_from_axis_angle([0.0_wp, 0.0_wp, 1.0_wp], 0.0044_wp)))
    call check(abs(v(1) - 1.0_wp) < 1.0e-14_wp, 'quat_slerp of close attitudes: a unit quaternion')

    call report('data frames (trajectories drawn in another frame of the scene, each point at its time)')

contains

    pure real(wp) function time_of(i)
        integer, intent(in) :: i
        time_of = (i - 1) * dt
    end function time_of

    !> The Moon's position and attitude (turning about z with its orbit: its x
    !> axis away from the Earth), as a 4x4 transform to the inertial frame.
    function expected_moon(t) result(mm)
        real(wp), intent(in) :: t
        real(wp) :: mm(4,4)
        mm = frame_matrix(rm * [cos(w * t), sin(w * t), 0.0_wp], [0.0_wp, 0.0_wp, sin(0.5_wp * w * t), cos(0.5_wp * w * t)])
    end function expected_moon

    !> An inertial point p at time t in the Moon's frame.
    function in_moon(tt, p) result(q)
        real(wp), intent(in) :: tt, p(3)
        real(wp) :: q(3), mm(4,4)
        mm = rigid_inverse(expected_moon(tt))
        q = matmul(mm(1:3, 1:3), p) + mm(1:3, 4)
    end function in_moon

    !> The ephemeris for points 1 .. count (at the rate w now).
    subroutine fill_ephemeris(count)
        integer, intent(in) :: count
        integer :: k
        call eph%clear()
        do k = 1, count
            call eph%add(time_of(k), rm * [cos(w * time_of(k)), sin(w * time_of(k)), 0.0_wp], &
                         att=[0.0_wp, 0.0_wp, sin(0.5_wp * w * time_of(k)), cos(0.5_wp * w * time_of(k))])
        end do
    end subroutine fill_ephemeris

    !> The spacecraft's inertial path: out from the Earth toward the Moon's orbit.
    pure function craft_at(tt) result(p)
        real(wp), intent(in) :: tt
        real(wp) :: p(3)
        p = [7000.0_wp + 1.2_wp * tt, 2.0e5_wp * sin(4.0e-6_wp * tt), 3000.0_wp * sin(1.0e-5_wp * tt)]
    end function craft_at

    !> A vector at point i (the segments' relative ends).
    pure function push_at(i) result(d)
        integer, intent(in) :: i
        real(wp) :: d(3)
        d = [cos(0.1_wp * i), sin(0.1_wp * i), 0.5_wp]
    end function push_at

    !> The largest difference of the curve's points 1 .. count from where
    !> they should be in the Moon's frame.
    real(wp) function curve_error(count) result(e)
        integer, intent(in) :: count
        integer :: k
        e = 0.0_wp
        do k = 1, count
            e = max(e, maxval(abs(line%drawn_point(k) - in_moon(time_of(k), path%position(k)))))
        end do
    end function curve_error

end program test_data_frames
