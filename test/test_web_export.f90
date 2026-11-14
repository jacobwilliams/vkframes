!> Checks the export of a scene to the web (vkf_web_writer, and the frames'
!> and artists' export_web), without a window: base64 and JSON numbers; a
!> scene (a Moon frame following an ephemeris, a trajectory drawn in its
!> frame from inertial data, a sphere) written as the page's JSON and arrays,
!> then read back: the frames and their items, the Moon's track (sampled
!> poses) against its pose at those times (local_matrix_at), the curve's
!> points (each chunk's origin plus its float32 offsets) against the points
!> drawn, its times, a page written with the viewer script, and the page
!> recorded from poses the program sets.
program test_web_export

    use, intrinsic :: iso_c_binding, only: c_null_ptr
    use, intrinsic :: iso_fortran_env, only: int8, real32, real64
    use vkframes
    use vkf_frames, only: WebFrameList, collect_web_frames, write_web_frames, web_time_range
    use vkf_web_writer, only: WebWriter, base64_encode, base64_decode, json_number, WEB_CHUNK
    use vkf_checks, only: check, report

    implicit none

    real(wp), parameter :: rm = 384400.0_wp, w_moon = 2.6617e-6_wp, dt = 600.0_wp
    integer, parameter :: n = 20000                     ! (more than a chunk: chunks checked too)

    type(ReferenceFrame), target :: eci, moon, spinner
    type(Trajectory), target :: eph, path
    type(DrawableTrajectory), target :: shown
    type(CurveArtist), target :: line
    type(Sphere), target :: body
    type(RadialPlane), target :: plane
    type(Polyhedron), target :: tetra
    type(WebWriter) :: w
    type(WebFrameList) :: list
    character(len=:), allocatable :: json
    integer(int8), allocatable :: bytes(:)
    real(wp), allocatable :: tt(:), pp(:), qq(:), o(:), p(:), times(:)
    real(wp) :: m(4,4), err, t0, t1, x
    integer :: i, k, pos, ref(2), count, first
    logical :: ok

    ! base64 (RFC 4648's examples) and back
    call check(base64_encode(bytes_of('Man')) == 'TWFu' .and. base64_encode(bytes_of('Ma')) == 'TWE=' .and. &
               base64_encode(bytes_of('M')) == 'TQ==', 'base64: the RFC''s examples')
    allocate(bytes(1000))
    do i = 1, 1000
        bytes(i) = int(modulo(i * 37, 256) - 128, int8)
    end do
    call check(all(base64_decode(base64_encode(bytes)) == bytes), 'base64: there and back')
    ! JSON numbers: whole numbers as integers, others to 17 digits
    json = json_number(0.1_wp)
    read(json, *) x
    call check(json_number(3.0_wp) == '3' .and. x == 0.1_wp .and. json_number(huge(1.0_wp)) /= '', 'JSON numbers')

    ! the scene: the Moon following its ephemeris, a path in its frame from inertial data, a body
    eph = Trajectory()
    path = Trajectory()
    do i = 1, n
        call eph%add(time_of(i), pos=rm * [cos(w_moon * time_of(i)), sin(w_moon * time_of(i)), 0.0_wp], &
                     att=[0.0_wp, 0.0_wp, sin(0.5_wp * w_moon * time_of(i)), cos(0.5_wp * w_moon * time_of(i))])
        call path%add(time_of(i), pos=[7000.0_wp + 1.2_wp * time_of(i), 2.0e5_wp * sin(4.0e-7_wp * time_of(i)), 3000.0_wp])
    end do
    eci = ReferenceFrame('ECI')
    moon = ReferenceFrame('Moon')
    call moon%follow(eph, mode=FOLLOW_LIMIT)
    line = CurveArtist(path, color=[1.0_wp, 0.5_wp, 0.2_wp], width=2.0_wp)
    call line%set_trace_mode(.true.)
    shown = DrawableTrajectory('Path')
    call shown%add_artist(line)
    call shown%set_data_frame(eci)
    body = Sphere('Body', radius=1737.0_wp)
    plane = RadialPlane('Plane')
    tetra = Polyhedron('Tetrahedron')
    call tetra%set_triangles(reshape([0.0_wp, 0.0_wp, 0.0_wp, 1.0_wp, 0.0_wp, 0.0_wp, 0.0_wp, 1.0_wp, 0.0_wp, &
                                      0.0_wp, 0.0_wp, 1.0_wp], [3, 4]), reshape([1, 3, 2, 1, 2, 4, 1, 4, 3, 2, 3, 4], [3, 4]))
    call tetra%set_lit(.true.)
    ok = eci%add_child(moon) .and. moon%add_child(shown) .and. moon%add_child(body) .and. eci%add_child(plane) .and. &
         eci%add_child(tetra)
    call check(ok, 'the scene')
    call eci%realize(c_null_ptr)
    call eci%update(0.0_wp)

    ! the page's JSON and arrays, animated over the data's times
    call collect_web_frames(eci, list)
    call check(list%n == 6, 'the frames, each once')
    call check(web_time_range(list, t0, t1) .and. t0 == 0.0_wp .and. t1 == time_of(n), 'the data''s time range')
    w%animate = .true.
    w%t_start = t0
    w%t_end = t1
    w%t_ref = t0
    w%samples = 50
    call w%begin_object()
    call write_web_frames(list, w)
    call w%end_object()
    json = w%json_text()
    call check(index(json, '"name":"Moon"') > 0 .and. index(json, '"type":"sphere"') > 0 .and. &
               index(json, '"type":"lines"') > 0 .and. index(json, '"trace":true') > 0, 'the frames and their items')
    pos = index(json, '"name":"Plane"')
    call check(pos > 0 .and. index(json(pos:), '"type":"mesh"') > 0, 'a RadialPlane: its surface and lines')
    pos = index(json, '"name":"Tetrahedron"')
    call check(pos > 0 .and. index(json(pos:), '"lit":true') > 0, 'a Polyhedron: its faces, lit')

    ! the Moon's track: its pose at each sampled time
    pos = index(json, '"name":"Moon"')
    pos = pos + index(json(pos:), '"track":') - 1
    tt = array_at('"t":', 8)
    pp = array_at('"p":', 8)
    qq = array_at('"q":', 4)
    call check(size(tt) >= n .and. size(pp) == 3 * size(tt) .and. size(qq) == 4 * size(tt), &
               'the track: the data''s times and the samples')
    err = 0.0_wp
    do k = 1, size(tt), 97
        m = moon%local_matrix_at(tt(k) + w%t_ref)
        err = max(err, maxval(abs(pp(3*k-2:3*k) - m(1:3, 4))), &
                  rm * min(maxval(abs(qq(4*k-3:4*k) - quat_from_matrix(m(1:3, 1:3)))), &
                           maxval(abs(qq(4*k-3:4*k) + quat_from_matrix(m(1:3, 1:3))))))
    end do
    call check(err < 1.0e-6_wp * rm, 'the track: the poses at their times')

    ! the curve: each chunk's points (its origin plus float32 offsets), as drawn, and their times
    pos = index(json, '"type":"lines"')
    err = 0.0_wp
    first = 1
    count = 0
    do while (count < n)
        o = numbers_at('"o":')
        p = array_at('"p":', 4)
        times = array_at('"t":', 4)
        do k = 1, size(p) / 3
            i = first + k - 1
            err = max(err, maxval(abs(o + p(3*k-2:3*k) - line%drawn_point(i))) / max(1.0_wp, norm2(line%drawn_point(i))))
            err = max(err, abs(times(k) - time_of(i)) / max(1.0_wp, time_of(n)))
        end do
        count = first + size(p) / 3 - 1
        first = count                                       ! (a strip's chunks share their ends)
        pos = pos + 1
    end do
    call check(count == n .and. err < 1.0e-6_wp, 'the curve: the points drawn (in the Moon''s frame), and their times')

    ! a page, with the viewer script (written to the build directory)
    ok = w%write_html('build/test_web_export.html', 'test', 'data/web/vkf_viewer.js', '0.170.0')
    call check(ok, 'the page written')

    ! recorded: a frame the program turns, its poses kept only when they change
    spinner = ReferenceFrame('Spinner')
    ok = eci%add_child(spinner)
    do i = 0, 10
        if (i <= 5) call spinner%set_attitude(quat_from_axis_angle([0.0_wp, 0.0_wp, 1.0_wp], 0.1_wp * i))
        call eci%record_web_pose(real(i, wp))
    end do
    w = WebWriter()
    w%animate = .true.
    w%t_start = 0.0_wp
    w%t_end = 10.0_wp
    call collect_web_frames(eci, list)
    call w%begin_object()
    call write_web_frames(list, w)
    call w%end_object()
    json = w%json_text()
    pos = index(json, '"name":"Spinner"')
    tt = array_at('"t":', 8)
    call check(size(tt) == 6, 'recorded: the poses that changed')
    call eci%clear_web_poses()

    call report('export to the web (the page''s JSON and arrays)')

contains

    !> The time of point i of the test's trajectories.
    pure real(wp) function time_of(i)
        integer, intent(in) :: i                            !! the point (from 1)
        time_of = (i - 1) * dt
    end function time_of

    !> Text as bytes (for base64_encode).
    function bytes_of(s) result(b)
        character(len=*), intent(in) :: s                   !! the text
        integer(int8) :: b(len(s))                          !! its bytes
        integer :: j
        do j = 1, len(s)
            b(j) = int(iachar(s(j:j)), int8)
        end do
    end function bytes_of

    !> The numbers of the inline array after `key`, from pos (pos moved past it).
    function numbers_at(key) result(v)
        character(len=*), intent(in) :: key                 !! e.g. '"o":'
        real(wp), allocatable :: v(:)                       !! the numbers
        integer :: a, b, c, j
        a = pos + index(json(pos:), key) - 1 + len(key)
        b = a + index(json(a:), ']') - 1
        c = 1
        do j = a + 1, b - 1
            if (json(j:j) == ',') c = c + 1
        end do
        allocate(v(c))
        read(json(a+1:b-1), *) v
        pos = b
    end function numbers_at

    !> The array in the buffer referred to after `key` ([offset, count]), from
    !> pos: float64 (bytes 8) or float32 (4).
    function array_at(key, bytes_each) result(v)
        character(len=*), intent(in) :: key                 !! e.g. '"p":'
        integer, intent(in) :: bytes_each                   !! 8 (float64) or 4 (float32)
        real(wp), allocatable :: v(:)                       !! the values
        real(wp), allocatable :: r(:)
        r = numbers_at(key)
        ref = nint(r(1:2))
        if (bytes_each == 8) then
            v = real(transfer(w%bin(ref(1)+1:ref(1)+8*ref(2)), 0.0_real64, ref(2)), wp)
        else
            v = real(transfer(w%bin(ref(1)+1:ref(1)+4*ref(2)), 0.0_real32, ref(2)), wp)
        end if
    end function array_at

end program test_web_export
