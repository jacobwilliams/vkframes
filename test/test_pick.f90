!> Checks picking (WindowProxy%pick, in a window of its own), with the camera
!> set so that where things are on the screen is known: a body (the point
!> on the surface, its latitude and longitude), a trajectory's curve (the
!> point between two trajectory points and the time there, interpolated;
!> within the tolerance and not beyond it), its markers, its segments, trace
!> mode (what isn't drawn isn't picked; the tail is), and the vkf_pick
!> geometry (perspective-correct fractions along a segment). Callouts:
!> screen_position (where a point is, as HUD positions), HUD text and a
!> frame's name label changed while shown (position, size, alignment, color,
!> font, text).
program test_pick

    use, intrinsic :: iso_c_binding, only: c_int
    use vkframes
    use vkf_layer, only: vkf_viewer_size, vkf_viewer_pixel_ratio
    use vkf_pick, only: PickCamera, pick_camera, segment_pick

    implicit none

    real(wp), parameter :: deg = pi / 180.0_wp
    real(wp), parameter :: eye(3) = [0.0_wp, -10000.0_wp, 0.0_wp]
    integer, parameter :: n_points = 41
    type(ReferenceFrame), target :: root
    type(Sphere), target :: earth
    type(Trajectory), target :: traj
    type(DrawableTrajectory), target :: path
    type(CurveArtist), target :: curve
    type(MarkerArtist), target :: markers
    type(SegmentArtist), target :: spokes
    type(FrameManager), target :: scene
    type(WindowProxy) :: window
    type(View), target :: top
    type(PickHit), allocatable :: hits(:)
    type(HUDText), target :: callout
    real(wp) :: pos(2)
    integer(c_int) :: width, height
    real(wp) :: aspect, ratio, x, y
    integer :: failures, k

    failures = 0
    call check_geometry()

    ! the scene: a body at the origin, and a straight trajectory above it
    ! (x = -2000 to 2000, z = 1500; a point every 100, t = 0 to 400)
    root = ReferenceFrame('Root')
    call root%show_decorations(.false.)
    earth = Sphere('Earth', radius=1000.0_wp)
    call earth%show_decorations(.false.)
    traj = Trajectory()
    do k = 1, n_points
        call traj%add(10.0_wp * (k - 1), pos=[-2000.0_wp + 100.0_wp * (k - 1), 0.0_wp, 1500.0_wp])
    end do
    curve = CurveArtist(traj, width=2.0_wp)
    markers = MarkerArtist(traj, markers=ior(MARKER_START, MARKER_END), size=10.0_wp)
    spokes = SegmentArtist(traj, stride=10)                        ! (from the origin to every 10th point)
    path = DrawableTrajectory('Path')
    call path%show_decorations(.false.)
    call path%add_artist(curve)
    call path%add_artist(markers)
    call path%add_artist(spokes)
    if (.not. (root%add_child(earth) .and. root%add_child(path))) error stop 'add_child failed'
    scene = FrameManager(root)

    if (.not. window%open(400, 300, 'vkframes: pick test')) then
        write(*, '(a)') 'FAIL: could not open a window'
        error stop 1
    end if
    call window%set_scene(scene)
    top = View(root, root)
    call window%add_view(top)
    call window%pause_time(.true.)
    call window%set_time(400.0_wp)
    call frame()
    call window%set_camera(eye, [0.0_wp, 0.0_wp, 0.0_wp], [0.0_wp, 0.0_wp, 1.0_wp])
    call frame()
    if (vkf_viewer_size(window%viewer(), width, height) /= 0) error stop 'window size'
    aspect = real(width, wp) / real(height, wp)
    ratio = vkf_viewer_pixel_ratio(window%viewer())

    ! nothing there
    call at([3000.0_wp, 0.0_wp, -3000.0_wp], x, y)
    call window%pick(x, y, hits)
    call check(size(hits) == 0, 'empty space: no hits')

    ! the body: the point facing the eye, and one at latitude 30 degrees
    call window%pick(0.0_wp, 0.0_wp, hits)
    call check(size(hits) >= 1, 'body: hit')
    if (size(hits) >= 1) then
        call check(hits(1)%kind == PICK_BODY .and. associated(hits(1)%frame, earth), 'body: the sphere')
        call check(near3(hits(1)%position, [0.0_wp, -1000.0_wp, 0.0_wp], 1.0e-6_wp), 'body: the point facing the eye')
        call check(abs(hits(1)%latitude) < 1.0e-9_wp .and. abs(hits(1)%longitude + 90.0_wp * deg) < 1.0e-9_wp, &
                   'body: latitude 0, longitude -90')
    end if
    call at(1000.0_wp * [0.0_wp, -cos(30.0_wp * deg), sin(30.0_wp * deg)], x, y)
    call window%pick(x, y, hits)
    call check(size(hits) >= 1, 'body: hit at latitude 30')
    if (size(hits) >= 1) call check(abs(hits(1)%latitude - 30.0_wp * deg) < 1.0e-6_wp, 'body: latitude 30')

    ! the curve: between points 26 and 27 (x = 500 and 600): the time there
    call at([550.0_wp, 0.0_wp, 1500.0_wp], x, y)
    call window%pick(x, y, hits)
    k = first_hit(PICK_CURVE)
    call check(k > 0, 'curve: hit')
    if (k > 0) then
        call check(associated(hits(k)%frame, path) .and. associated(hits(k)%trajectory, traj) .and. &
                   hits(k)%index == 26, 'curve: the frame, the trajectory and the point before')
        call check(abs(hits(k)%time - 255.0_wp) < 1.0e-6_wp, 'curve: the time, interpolated')
        call check(near3(hits(k)%position, [550.0_wp, 0.0_wp, 1500.0_wp], 1.0e-6_wp), 'curve: the point')
        select type (a => hits(k)%artist)
        type is (CurveArtist)
            call check(.true., '')
        class default
            call check(.false., 'curve: the artist')
        end select
    end if
    call window%pick(x, y + 3.0_wp * ratio / (0.5_wp * height), hits)       ! 3 pixels above: within reach
    call check(first_hit(PICK_CURVE) > 0, 'curve: 3 pixels off')
    call window%pick(x, y + 20.0_wp * ratio / (0.5_wp * height), hits)      ! 20 pixels above: not
    call check(first_hit(PICK_CURVE) == 0, 'curve: 20 pixels off, missed')

    ! the start marker
    call at([-2000.0_wp, 0.0_wp, 1500.0_wp], x, y)
    call window%pick(x + 4.0_wp * ratio / (0.5_wp * width), y, hits)        ! (within half its size)
    k = first_hit(PICK_MARKER)
    call check(k > 0, 'marker: hit')
    if (k > 0) call check(hits(k)%marker == MARKER_START .and. hits(k)%index == 1 .and. hits(k)%time == 0.0_wp, &
                          'marker: the start, its point and time')

    ! a segment: the radius vector to point 1, 70% of the way
    call at(0.7_wp * [-2000.0_wp, 0.0_wp, 1500.0_wp], x, y)
    call window%pick(x, y, hits)
    k = first_hit(PICK_SEGMENT)
    call check(k > 0, 'segment: hit')
    if (k > 0) call check(hits(k)%index == 1 .and. near3(hits(k)%position, 0.7_wp * [-2000.0_wp, 0.0_wp, 1500.0_wp], &
                          1.0e-6_wp), 'segment: the first, the point')

    ! what is picked: frames and artists left out, kinds, the nearest only
    earth_pickable: block
        call earth%set_pickable(.false.)
        call window%pick(0.0_wp, 0.0_wp, hits)
        call check(first_hit(PICK_BODY) == 0, 'not pickable: the body left out')
        call earth%set_pickable(.true.)
    end block earth_pickable
    call at([-2000.0_wp, 0.0_wp, 1500.0_wp], x, y)                  ! (the curve, the start marker and a spoke)
    call window%pick(x, y, hits)
    call check(first_hit(PICK_CURVE) > 0 .and. first_hit(PICK_MARKER) > 0 .and. first_hit(PICK_SEGMENT) > 0, &
               'several hits there')
    call window%pick(x, y, hits, kinds=[PICK_MARKER])
    call check(size(hits) >= 1 .and. all(hits%kind == PICK_MARKER), 'kinds: markers only')
    call window%pick(x, y, hits, nearest=.true.)
    call check(size(hits) == 1, 'nearest: one hit')
    call markers%set_pickable(.false.)
    call window%pick(x, y, hits)
    call check(first_hit(PICK_MARKER) == 0 .and. first_hit(PICK_CURVE) > 0, 'artist not pickable: left out')
    call markers%set_pickable(.true.)

    ! callouts: where a point is (HUD positions: 0 to 1, y up), and HUD text
    ! and a frame's label changed while shown
    call at([550.0_wp, 0.0_wp, 1500.0_wp], x, y)
    call check(window%screen_position([550.0_wp, 0.0_wp, 1500.0_wp], pos), 'screen position: in front')
    call check(abs(pos(1) - 0.5_wp * (x + 1.0_wp)) < 1.0e-9_wp .and. abs(pos(2) - 0.5_wp * (y + 1.0_wp)) < 1.0e-9_wp, &
               'screen position: the point')
    call check(.not. window%screen_position([0.0_wp, -20000.0_wp, 0.0_wp], pos), 'screen position: behind the eye')
    callout = HUDText('callout', position=[0.1_wp, 0.1_wp])
    call window%add_hud_text(callout)
    call frame()
    call callout%set_position(pos)
    call callout%set_size(30.0_wp)
    call callout%set_alignment(ALIGN_CENTER_BOTTOM)
    call callout%set_color([1.0_wp, 0.5_wp, 0.0_wp])
    call callout%set_text('t = 255 s')
    call frame()
    call callout%set_font('DejaVuSans-Bold.ttf')                  ! (the text's node made again)
    call frame()
    call check(callout%font() == 'DejaVuSans-Bold.ttf' .and. all(callout%position() == pos), 'HUD text changed while shown')
    call path%show_name_label(.true.)
    call path%set_name('readout: t = 255 s')
    call path%set_color([0.2_wp, 1.0_wp, 0.4_wp])
    call path%set_label_size(28)
    call path%set_label_font('DejaVuSans-Oblique.ttf')
    call frame()
    call check(path%name() == 'readout: t = 255 s', 'frame label changed while shown')
    call path%show_name_label(.false.)
    call window%remove_hud_text(callout)
    call frame()

    ! trace mode at t = 155: drawn up to point 16 (x = -500), and the tail to x = -450
    call curve%set_trace_mode(.true.)
    call window%set_time(155.0_wp)
    call frame()
    call at([550.0_wp, 0.0_wp, 1500.0_wp], x, y)
    call window%pick(x, y, hits)
    call check(first_hit(PICK_CURVE) == 0, 'trace: not drawn, not picked')
    call at([-1500.0_wp, 0.0_wp, 1500.0_wp], x, y)
    call window%pick(x, y, hits)
    call check(first_hit(PICK_CURVE) > 0, 'trace: drawn, picked')
    call at([-470.0_wp, 0.0_wp, 1500.0_wp], x, y)
    call window%pick(x, y, hits)
    k = first_hit(PICK_CURVE)
    call check(k > 0, 'trace: the tail')
    if (k > 0) call check(abs(hits(k)%time - 153.0_wp) < 1.0e-6_wp, 'trace: the time on the tail')

    call window%close()
    if (failures > 0) then
        write(*, '(a,i0,a)') 'FAIL: ', failures, ' checks failed'
        error stop 1
    end if
    write(*, '(a)') 'PASS: picking (bodies, curves, markers, segments, trace mode), callouts'

contains

    !> The window position (-1 to 1, y up) of root point p, for the camera set.
    subroutine at(p, x, y)
        real(wp), intent(in) :: p(3)
        real(wp), intent(out) :: x, y
        real(wp) :: depth, t
        depth = p(2) - eye(2)                                  ! (the camera looks along +y, z up)
        t = tan(0.5_wp * top%field_of_view() * deg)
        x = (p(1) - eye(1)) / (depth * t * aspect)
        y = (p(3) - eye(3)) / (depth * t)
    end subroutine at

    !> The first hit of `kind`, or 0.
    integer function first_hit(kind) result(k)
        integer, intent(in) :: kind
        do k = 1, size(hits)
            if (hits(k)%kind == kind) return
        end do
        k = 0
    end function first_hit

    !> Fractions along a segment are perspective-correct: halfway on the
    !> screen is not halfway in the scene when the ends are at different depths.
    subroutine check_geometry()
        type(PickCamera) :: cam
        real(wp) :: px, s, depth
        cam = pick_camera([0.0_wp, 0.0_wp, 0.0_wp], [0.0_wp, 1.0_wp, 0.0_wp], [0.0_wp, 0.0_wp, 1.0_wp], .true., &
                          90.0_wp, 1.0_wp, [-1.0_wp, 1.0_wp, -1.0_wp, 1.0_wp], 100, 100, 0.0_wp, 0.0_wp, 1.0_wp)
        ! from (-1, 1, 0) (screen x -1) to (3, 3, 0) (screen x 1): the cursor (x = 0)
        ! is on the ray along +y, which the segment crosses at (0, 1.5, 0): 1/4 of the way
        call check(segment_pick(cam, [-1.0_wp, 1.0_wp, 0.0_wp], [3.0_wp, 3.0_wp, 0.0_wp], px, s, depth), &
                   'geometry: segment hit')
        call check(abs(s - 0.25_wp) < 1.0e-12_wp .and. abs(depth - 1.5_wp) < 1.0e-12_wp .and. px < 1.0e-9_wp, &
                   'geometry: perspective-correct fraction and depth')
    end subroutine check_geometry


    subroutine frame()
        if (.not. window%frame()) then
            write(*, '(a)') 'FAIL: the window closed'
            error stop 1
        end if
    end subroutine frame

    logical function near3(a, b, tol)
        real(wp), intent(in) :: a(3), b(3), tol
        near3 = maxval(abs(a - b)) <= tol * max(1.0_wp, maxval(abs(b)))
    end function near3

    subroutine check(ok, what)
        logical, intent(in) :: ok
        character(len=*), intent(in) :: what
        if (.not. ok) then
            write(*, '(a)') 'FAIL: '//what
            failures = failures + 1
        end if
    end subroutine check

end program test_pick
