!> Custom line segments, as OpenFrames' CustomLineSegments: a frame drawing
!> line segments whose endpoints and colors (one per endpoint, blended along
!> the segment) are set by the program (`set_segments`), or computed each
!> frame by a callback (`set_callback`): e.g. lines of sight between
!> spacecraft, or links between ground stations and satellites.
!>
!> The endpoints are in the frame's own coordinates (the frame can move like
!> any other); `position_of(other)` gives another frame's origin in them. The
!> callback, an extension of the abstract LineSegmentsCallback (OpenFrames'
!> CustomLineSegments::Callback), is called once per scene update with the
!> simulation time, after the whole tree has been updated
!> (ReferenceFrame%late_update), so the positions of other frames are those
!> for that time whatever their place in the tree; it sets the segments with
!> `set_segments`. Setting the same segments again uploads nothing.
!>
!> The segments are drawn by a LineChain of separate segments (vkf_line_chain:
!> thick lines in window units, square caps, dash patterns, points kept to
!> double precision), with a color per point (vkf_line_create_colored).
!> Without colors, the segments take the frame's color (`set_color`).
!> (OpenFrames' custom line shader, setLineShader, isn't available: it would
!> need GLSL compiled at run time.)
module vkf_line_segments

    use, intrinsic :: iso_c_binding, only: c_ptr, c_associated, c_float
    use vkf_math, only: wp
    use vkf_frames, only: ReferenceFrame
    use vkf_line_chain, only: LineChain, CHAIN_SEGMENTS, split_points

    implicit none

    private

    !> Computes a CustomLineSegments' segments each frame: extend it with a
    !> `compute` procedure, which calls `segments%set_segments` (OpenFrames'
    !> CustomLineSegments::Callback). The program owns the callback object (it
    !> must stay alive while set).
    type, abstract, public :: LineSegmentsCallback
    contains
        procedure(segments_compute), deferred :: compute
    end type LineSegmentsCallback

    !> Line segments with a color per endpoint (see the module documentation).
    type, extends(ReferenceFrame), public :: CustomLineSegments
        private
        integer :: n = 0                                    !! segments
        real(wp), allocatable :: a(:,:), b(:,:)             !! (3, n): their endpoints (frame coordinates)
        real(c_float), allocatable :: color_a(:,:), color_b(:,:)   !! (4, n), or not allocated: the frame's color
        real(wp) :: width_ = 1.0_wp                         !! line width (window units)
        integer :: pattern_factor = 1, pattern_ = int(z'FFFF')
        class(LineSegmentsCallback), pointer :: callback => null()
        logical :: changed = .true.                         !! to write and upload at the next update
        integer :: n_drawn = 0
        type(LineChain) :: chain
    contains
        procedure :: set_segments, num_segments, segment, clear_segments, drawn_segment, num_drawn
        procedure :: set_callback, clear_callback
        procedure :: set_line_width, line_width
        procedure :: set_pattern, pattern
        procedure :: set_color => segments_set_color
        procedure :: frame_info => segments_frame_info
        procedure :: bound => segments_bound
        procedure :: build_contents => segments_build_contents
        procedure :: late_update => segments_late_update
    end type CustomLineSegments

    abstract interface
        !> Set `segments`' segments for simulation time t (with set_segments).
        subroutine segments_compute(self, segments, t)
            import :: LineSegmentsCallback, CustomLineSegments, wp
            implicit none
            class(LineSegmentsCallback), intent(inout) :: self
            class(CustomLineSegments), intent(inout) :: segments
            real(wp), intent(in) :: t
        end subroutine segments_compute
    end interface

    interface CustomLineSegments
        module procedure new_custom_line_segments
    end interface CustomLineSegments

contains

    !> Line segments named `name`, of `color` (for segments set without
    !> colors; default white) and `width` (window units; default 1).
    function new_custom_line_segments(name, color, width) result(s)
        character(len=*), intent(in) :: name
        real(wp), intent(in), optional :: color(:), width
        type(CustomLineSegments) :: s
        s%ReferenceFrame = ReferenceFrame(name, color)
        if (present(width)) s%width_ = max(0.0_wp, width)
        allocate(s%a(3, 0), s%b(3, 0))
    end function new_custom_line_segments

    !> The segments: from a(:, i) to b(:, i) (frame coordinates), colored
    !> color_a(:, i) at a and color_b(:, i) at b ([r, g, b] or [r, g, b, a]
    !> columns; one of them for both ends; neither: the frame's color). Setting
    !> the same segments again changes nothing (no upload).
    subroutine set_segments(self, a, b, color_a, color_b)
        class(CustomLineSegments), intent(inout) :: self
        real(wp), intent(in) :: a(:,:), b(:,:)
        real(wp), intent(in), optional :: color_a(:,:), color_b(:,:)
        real(c_float), allocatable :: ca(:,:), cb(:,:)
        integer :: n
        n = min(size(a, 2), size(b, 2))
        if (present(color_a) .or. present(color_b)) then
            allocate(ca(4, n), cb(4, n))
            if (present(color_a)) then
                call rgba(color_a, ca)
            else
                call rgba(color_b, ca)
            end if
            if (present(color_b)) then
                call rgba(color_b, cb)
            else
                cb = ca
            end if
        end if
        if (n == self%n .and. same(a(:, 1:n), self%a) .and. same(b(:, 1:n), self%b) .and. &
            (allocated(ca) .eqv. allocated(self%color_a))) then
            if (.not. allocated(ca)) return
            if (all(ca == self%color_a) .and. all(cb == self%color_b)) return
        end if
        self%n = n
        self%a = a(1:3, 1:n)
        self%b = b(1:3, 1:n)
        if (allocated(ca)) then
            call move_alloc(ca, self%color_a)
            call move_alloc(cb, self%color_b)
        else
            if (allocated(self%color_a)) deallocate(self%color_a, self%color_b)
        end if
        self%changed = .true.

    contains

        pure logical function same(x, y)
            real(wp), intent(in) :: x(:,:), y(:,:)
            same = size(x, 2) == size(y, 2)
            if (same) same = all(x(1:3, :) == y)
        end function same

    end subroutine set_segments

    !> Colors c (3 or 4 rows) into rgba(4, n) (alpha 1 if not given).
    pure subroutine rgba(c, out)
        real(wp), intent(in) :: c(:,:)
        real(c_float), intent(out) :: out(:,:)
        integer :: n, m
        n = min(size(c, 2), size(out, 2))
        m = min(4, size(c, 1))
        out = 1.0_c_float
        out(1:m, 1:n) = real(c(1:m, 1:n), c_float)
    end subroutine rgba

    pure integer function num_segments(self)
        class(CustomLineSegments), intent(in) :: self
        num_segments = self%n
    end function num_segments

    !> Segment i: its endpoints, and their colors (rgba).
    subroutine segment(self, i, a, b, color_a, color_b)
        class(CustomLineSegments), intent(in) :: self
        integer, intent(in) :: i
        real(wp), intent(out) :: a(3), b(3)
        real(wp), intent(out), optional :: color_a(4), color_b(4)
        a = self%a(:, i)
        b = self%b(:, i)
        if (present(color_a)) color_a = segment_color(self, self%color_a, i)
        if (present(color_b)) color_b = segment_color(self, self%color_b, i)
    end subroutine segment

    function segment_color(self, c, i) result(rgba4)
        type(CustomLineSegments), intent(in) :: self
        real(c_float), allocatable, intent(in) :: c(:,:)
        integer, intent(in) :: i
        real(wp) :: rgba4(4)
        if (allocated(c)) then
            rgba4 = real(c(:, i), wp)
        else
            rgba4 = self%color()
        end if
    end function segment_color

    !> Segment i as drawn (read back from the GPU buffers, for checks): its
    !> endpoints and colors.
    subroutine drawn_segment(self, i, a, b, color_a, color_b)
        class(CustomLineSegments), intent(in) :: self
        integer, intent(in) :: i
        real(wp), intent(out) :: a(3), b(3)
        real(wp), intent(out), optional :: color_a(4), color_b(4)
        a = self%chain%point(2 * i - 1)
        b = self%chain%point(2 * i)
        if (present(color_a)) color_a = self%chain%point_color(2 * i - 1)
        if (present(color_b)) color_b = self%chain%point_color(2 * i)
    end subroutine drawn_segment

    !> Segments drawn now (0 until shown and updated).
    pure integer function num_drawn(self)
        class(CustomLineSegments), intent(in) :: self
        num_drawn = self%n_drawn
    end function num_drawn

    subroutine clear_segments(self)
        class(CustomLineSegments), intent(inout) :: self
        real(wp) :: none(3, 0)
        call self%set_segments(none, none)
    end subroutine clear_segments

    !> Compute the segments each frame with `callback` (see the module
    !> documentation; it must stay alive while set).
    subroutine set_callback(self, callback)
        class(CustomLineSegments), intent(inout) :: self
        class(LineSegmentsCallback), intent(in), target :: callback
        self%callback => callback
    end subroutine set_callback

    subroutine clear_callback(self)
        class(CustomLineSegments), intent(inout) :: self
        self%callback => null()
    end subroutine clear_callback

    !> Line width (window units: logical pixels).
    subroutine set_line_width(self, width)
        class(CustomLineSegments), intent(inout) :: self
        real(wp), intent(in) :: width
        self%width_ = max(0.0_wp, width)
        call apply_style(self)
    end subroutine set_line_width

    pure real(wp) function line_width(self)
        class(CustomLineSegments), intent(in) :: self
        line_width = self%width_
    end function line_width

    !> Dash pattern: OpenGL's line stipple (16 bits, bit 0 first, each `factor`
    !> window units; 65535: solid), as CurveArtist%set_pattern.
    subroutine set_pattern(self, factor, pattern)
        class(CustomLineSegments), intent(inout) :: self
        integer, intent(in) :: factor, pattern
        self%pattern_factor = max(1, factor)
        self%pattern_ = iand(pattern, int(z'FFFF'))
        call apply_style(self)
    end subroutine set_pattern

    pure subroutine pattern(self, factor, bits)
        class(CustomLineSegments), intent(in) :: self
        integer, intent(out) :: factor, bits
        factor = self%pattern_factor
        bits = self%pattern_
    end subroutine pattern

    !> The frame's color: also that of segments set without colors.
    subroutine segments_set_color(self, color)
        class(CustomLineSegments), intent(inout) :: self
        real(wp), intent(in) :: color(:)
        call self%ReferenceFrame%set_color(color)
        if (.not. allocated(self%color_a)) self%changed = .true.
    end subroutine segments_set_color

    function segments_frame_info(self) result(info)
        class(CustomLineSegments), intent(in) :: self
        character(len=:), allocatable :: info
        info = 'CustomLineSegments'
    end function segments_frame_info

    !> The segments' bounding sphere (frame coordinates).
    subroutine segments_bound(self, center, radius)
        class(CustomLineSegments), intent(in) :: self
        real(wp), intent(out) :: center(3), radius
        real(wp) :: lo(3), hi(3)
        center = 0.0_wp
        radius = 0.0_wp
        if (self%n == 0) return
        lo = min(minval(self%a, dim=2), minval(self%b, dim=2))
        hi = max(maxval(self%a, dim=2), maxval(self%b, dim=2))
        center = 0.5_wp * (lo + hi)
        radius = 0.5_wp * norm2(hi - lo)
    end subroutine segments_bound

    !> The chain of separate segments with a color per point, under `group`.
    subroutine segments_build_contents(self, viewer, group)
        class(CustomLineSegments), intent(inout) :: self
        type(c_ptr), intent(in) :: viewer, group
        call self%chain%init(group, CHAIN_SEGMENTS, colored=.true.)
        call apply_style(self)
        call self%chain%reserve(max(2, 2 * self%n))          ! (compiled with the scene)
        call self%chain%set_compile(c_associated(viewer))
        self%changed = .true.
    end subroutine segments_build_contents

    subroutine apply_style(self)
        type(CustomLineSegments), intent(inout) :: self
        if (.not. self%chain%is_built()) return
        ! white: each point's own color shows (shaders/line.vert multiplies them)
        call self%chain%set_style([1.0_wp, 1.0_wp, 1.0_wp, 1.0_wp], self%width_)
        call self%chain%set_pattern(self%pattern_factor, self%pattern_)
    end subroutine apply_style

    !> After the tree's update for time t: the callback's segments (all frames
    !> at their positions for t), written and uploaded if they changed.
    recursive subroutine segments_late_update(self, t)
        class(CustomLineSegments), intent(inout) :: self
        real(wp), intent(in) :: t
        if (associated(self%callback)) call self%callback%compute(self, t)
        if (self%changed .and. self%chain%is_built()) call write_segments(self)
        call self%ReferenceFrame%late_update(t)
    end subroutine segments_late_update

    !> The segments into the chain: segment i as points 2i-1 (a) and 2i (b),
    !> with their colors; uploaded, and drawn.
    subroutine write_segments(self)
        type(CustomLineSegments), intent(inout) :: self
        real(c_float), dimension(:,:,:), pointer :: p
        real(c_float), dimension(:,:), pointer :: c
        real(wp), allocatable :: x(:,:)
        real(c_float), allocatable :: colors(:,:)
        integer :: n, j, lo, hi
        n = 2 * self%n
        self%changed = .false.
        self%n_drawn = self%n
        if (n == 0) then
            call self%chain%draw(0)
            return
        end if
        allocate(x(3, n), colors(4, n))
        x(:, 1:n:2) = self%a
        x(:, 2:n:2) = self%b
        if (allocated(self%color_a)) then
            colors(:, 1:n:2) = self%color_a
            colors(:, 2:n:2) = self%color_b
        else
            colors = spread(real(self%color(), c_float), 2, n)
        end if
        call self%chain%reserve(n)
        do j = 1, self%chain%num_chunks()
            call self%chain%slice(j, 1, n, lo, hi, p)
            if (.not. associated(p)) cycle
            call split_points(x(:, lo:hi), p)
            call self%chain%color_slice(j, 1, n, lo, hi, c)
            if (associated(c)) c = colors(:, lo:hi)
        end do
        call self%chain%written(1, n)
        call self%chain%draw(n)
    end subroutine write_segments

end module vkf_line_segments
