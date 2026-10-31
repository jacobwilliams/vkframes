!> Checks CustomLineSegments without a window: segments set directly (with
!> and without colors) are drawn as set, in double precision; a callback
!> computes them each scene update after the whole tree is updated, so a
!> segment between two moving frames follows them at the update's time even
!> when the segments' frame comes first in the tree; position_of gives a
!> frame's origin in another's coordinates (rotated and offset).
module test_line_segments_links

    use vkframes

    implicit none

    !> A segment from frame `from` to frame `to`, red to blue.
    type, extends(LineSegmentsCallback) :: Link
        class(ReferenceFrame), pointer :: from => null(), to => null()
        integer :: calls = 0
        real(wp) :: last_t = -1.0_wp
    contains
        procedure :: compute => link_compute
    end type Link

contains

    subroutine link_compute(self, segments, t)
        class(Link), intent(inout) :: self
        class(CustomLineSegments), intent(inout) :: segments
        real(wp), intent(in) :: t
        real(wp) :: a(3, 1), b(3, 1)
        self%calls = self%calls + 1
        self%last_t = t
        a(:, 1) = segments%position_of(self%from)
        b(:, 1) = segments%position_of(self%to)
        call segments%set_segments(a, b, reshape([1.0_wp, 0.0_wp, 0.0_wp, 1.0_wp], [4, 1]), &
                                   reshape([0.0_wp, 0.0_wp, 1.0_wp, 0.5_wp], [4, 1]))
    end subroutine link_compute

end module test_line_segments_links

program test_line_segments

    use, intrinsic :: iso_c_binding, only: c_null_ptr
    use vkframes
    use test_line_segments_links
    use vkf_checks, only: check, report

    implicit none

    real(wp), parameter :: r = 4.2e7_wp                 ! large coordinates: floats alone would lose metres
    type(ReferenceFrame), target :: root, craft_a, craft_b
    type(CustomLineSegments), target :: links, fixed
    type(Trajectory), target :: path_a, path_b
    type(FrameManager) :: scene
    type(Link), target :: ab_link
    real(wp) :: a(3), b(3), ca(4), cb(4), p(3), q(4)
    real(wp) :: ends_a(3, 2), ends_b(3, 2)
    integer :: seen


    ! two spacecraft moving along straight lines; the links' frame first in the tree
    path_a = Trajectory()
    call path_a%add(0.0_wp, pos=[r, 0.0_wp, 0.0_wp])
    call path_a%add(10.0_wp, pos=[r, 1000.0_wp, 0.0_wp])
    path_b = Trajectory()
    call path_b%add(0.0_wp, pos=[0.0_wp, r, 0.0_wp])
    call path_b%add(10.0_wp, pos=[0.0_wp, r, 2000.0_wp])
    root = ReferenceFrame('Root')
    links = CustomLineSegments('Links', width=3.0_wp)
    craft_a = ReferenceFrame('A')
    call craft_a%follow(path_a)
    craft_b = ReferenceFrame('B')
    call craft_b%follow(path_b)
    fixed = CustomLineSegments('Fixed', color=[0.2_wp, 0.8_wp, 0.4_wp])
    call check(root%add_child(links) .and. root%add_child(craft_a) .and. root%add_child(craft_b) .and. &
               root%add_child(fixed), 'tree')

    ! position_of: through a rotated, offset frame
    q = quat_from_axis_angle([0.0_wp, 0.0_wp, 1.0_wp], 0.5_wp * pi)
    call fixed%set_position([10.0_wp, 0.0_wp, 0.0_wp])
    call fixed%set_attitude(q)
    call craft_a%set_position([10.0_wp, 5.0_wp, 0.0_wp])          ! (before following takes over)
    p = fixed%position_of(craft_a)
    call check(all(abs(p - [5.0_wp, 0.0_wp, 0.0_wp]) < 1.0e-9_wp), 'position_of: rotated and offset')

    ! segments set directly, without colors: the frame's color
    ends_a(:, 1) = [r, 0.0_wp, 0.0_wp]
    ends_b(:, 1) = [r, 0.0_wp, 1.0_wp]
    ends_a(:, 2) = [-r, 2.5_wp, 0.0_wp]
    ends_b(:, 2) = [-r, 2.5_wp, -3.25_wp]
    call fixed%set_segments(ends_a, ends_b)
    call check(fixed%num_segments() == 2, 'set_segments: count')

    ! shown (headless), and one scene update at t = 5
    ab_link%from => craft_a
    ab_link%to => craft_b
    call links%set_callback(ab_link)
    call root%realize(c_null_ptr)
    seen = -1
    call scene%set_frame(root)
    call scene%update(5.0_wp, seen)
    call check(ab_link%calls == 1 .and. ab_link%last_t == 5.0_wp, 'callback: called once, with the time')
    call links%drawn_segment(1, a, b, ca, cb)
    call check(links%num_drawn() == 1, 'callback: one segment drawn')
    call check(maxval(abs(a - [r, 500.0_wp, 0.0_wp])) < 1.0e-6_wp .and. maxval(abs(b - [0.0_wp, r, 1000.0_wp])) < 1.0e-6_wp, &
               'callback: between the frames at their positions for t, to double precision')
    call check(all(abs(ca - [1.0_wp, 0.0_wp, 0.0_wp, 1.0_wp]) < 1.0e-6_wp) .and. &
               all(abs(cb - [0.0_wp, 0.0_wp, 1.0_wp, 0.5_wp]) < 1.0e-6_wp), 'callback: a color per endpoint')
    call fixed%drawn_segment(2, a, b, ca, cb)
    call check(fixed%num_drawn() == 2 .and. maxval(abs(a - ends_a(:, 2))) < 1.0e-6_wp .and. &
               maxval(abs(b - ends_b(:, 2))) < 1.0e-6_wp, 'set_segments: drawn as set')
    call check(all(abs(ca - fixed%color()) < 1.0e-6_wp) .and. all(abs(cb - ca) < 1.0e-6_wp), &
               "set_segments without colors: the frame's color")

    ! the next update: the segment follows the frames
    call scene%update(7.5_wp, seen)
    call links%drawn_segment(1, a, b)
    call check(ab_link%calls == 2 .and. maxval(abs(a - [r, 750.0_wp, 0.0_wp])) < 1.0e-6_wp .and. &
               maxval(abs(b - [0.0_wp, r, 1500.0_wp])) < 1.0e-6_wp, 'callback: the segment follows the frames')

    ! the frame's color changed: segments without colors take it
    call fixed%set_color([1.0_wp, 1.0_wp, 0.0_wp])
    call scene%update(7.5_wp, seen)
    call fixed%drawn_segment(1, a, b, ca)
    call check(all(abs(ca(1:3) - [1.0_wp, 1.0_wp, 0.0_wp]) < 1.0e-6_wp), "the frame's new color")
    call check(links%line_width() == 3.0_wp, 'line width')

    call report('CustomLineSegments')

end program test_line_segments
