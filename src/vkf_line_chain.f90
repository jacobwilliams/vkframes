!> A growing line drawn from a chain of line buffers ("chunks"), for the
!> trajectory artists: points (or separate segments, pairs of points) are
!> written in place, as (high, low) float pairs for double precision (see
!> src/vsg/vkf_layer.cpp: EyeRelative), and only the chunks written to are
!> uploaded again. A full chain gets a new chunk as large as the chain so far
!> (at least min_chunk, at most max_chunk points, or what is needed at once),
!> so nothing is ever copied or reallocated. In a strip, consecutive chunks
!> share their end point so that the line is continuous.
!>
!> Writing points i1 .. i2 (indices along the whole chain):
!>
!>     call chain%reserve(i2)
!>     do j = 1, chain%num_chunks()
!>         call chain%slice(j, i1, i2, lo, hi, p)      ! p: points lo .. hi of chunk j
!>         if (.not. associated(p)) cycle
!>         ... fill p(:, :, 1:hi-lo+1) (e.g. with split_points) ...
!>         call chain%changed(j, hi)
!>     end do
!>     call chain%draw(i2)
module vkf_line_chain

    use, intrinsic :: iso_c_binding, only: c_ptr, c_null_ptr, c_associated, c_int, c_float, c_f_pointer
    use vkf_math, only: wp
    use vkf_layer, only: vkf_line_create, vkf_line_points, vkf_line_points_changed, vkf_line_set_range, &
                         vkf_node_add_child, vkf_node_delete, vkf_viewer_compile_node, line_set_style, &
                         line_set_segments

    implicit none

    private

    integer, parameter :: min_chunk = 64                !! points in the smallest chunk
    integer, parameter, public :: max_chunk = 65536     !! points in a chunk made for appending (1.5 MB)

    public :: split_points, line_buffer, check

    !> A line buffer holding points first .. first+capacity-1 of the chain.
    type :: LineChunk
        type(c_ptr) :: line = c_null_ptr
        integer :: first = 1, capacity = 0
    end type LineChunk

    type, public :: LineChain
        private
        type(c_ptr) :: viewer = c_null_ptr, group = c_null_ptr
        type(LineChunk), allocatable :: chunks(:)
        integer :: n_chunks = 0
        logical :: segments = .false.
        real(c_float) :: color(4) = 1.0_c_float, width = 1.0_c_float
    contains
        procedure :: init, set_viewer, is_built, release
        procedure :: reserve, slice, changed, draw
        procedure :: set_style, point, num_chunks
    end type LineChain

contains

    !> Draw under `group`: a strip, or separate segments (points 1-2, 3-4, ...).
    !> Chunks made later are compiled by the viewer set with set_viewer.
    subroutine init(self, group, segments)
        class(LineChain), intent(inout) :: self
        type(c_ptr), intent(in) :: group
        logical, intent(in) :: segments
        self%group = group
        self%segments = segments
        self%viewer = c_null_ptr
    end subroutine init

    !> Forget the chunks (after their group was removed from the scene): init
    !> starts again.
    subroutine release(self)
        class(LineChain), intent(inout) :: self
        integer :: j
        do j = 1, self%n_chunks
            call vkf_node_delete(self%chunks(j)%line)
        end do
        self%n_chunks = 0
        self%group = c_null_ptr
        self%viewer = c_null_ptr
    end subroutine release

    !> The viewer that compiles chunks made from now on (null: none, e.g. while
    !> building, when they are compiled with the group).
    subroutine set_viewer(self, viewer)
        class(LineChain), intent(inout) :: self
        type(c_ptr), intent(in) :: viewer
        self%viewer = viewer
    end subroutine set_viewer

    pure logical function is_built(self)
        class(LineChain), intent(in) :: self
        is_built = c_associated(self%group)
    end function is_built

    pure integer function num_chunks(self)
        class(LineChain), intent(in) :: self
        num_chunks = self%n_chunks
    end function num_chunks

    !> Chunks for at least n points (at least one chunk). Separate segments:
    !> chunks hold whole segments (n should be even).
    subroutine reserve(self, n)
        class(LineChain), intent(inout) :: self
        integer, intent(in) :: n
        type(LineChunk), allocatable :: tmp(:)
        type(LineChunk) :: c
        integer :: covered
        covered = 0
        if (self%n_chunks > 0) covered = self%chunks(self%n_chunks)%first + self%chunks(self%n_chunks)%capacity - 1
        do while (self%n_chunks == 0 .or. covered < n)
            if (self%n_chunks == 0) then
                c%first = 1
                c%capacity = max(min_chunk, n)                 ! the data so far in one chunk
            else if (self%segments) then
                c%first = covered + 1
                c%capacity = max(n - covered, min(max_chunk, max(min_chunk, covered)))
            else
                c%first = covered                              ! sharing the previous chunk's last point
                c%capacity = max(n - covered + 1, min(max_chunk, max(min_chunk, covered)))
            end if
            if (self%segments) c%capacity = c%capacity + modulo(c%capacity, 2)
            c%line = vkf_line_create(int(c%capacity, c_int))
            if (.not. c_associated(c%line)) return
            if (self%segments) call check(line_set_segments(c%line, .true.))
            call check(line_set_style(c%line, self%color, self%width))
            if (c_associated(self%viewer)) call check(vkf_viewer_compile_node(self%viewer, c%line))
            call check(vkf_node_add_child(self%group, c%line))
            if (.not. allocated(self%chunks)) allocate(self%chunks(4))
            if (self%n_chunks == size(self%chunks)) then
                allocate(tmp(2 * self%n_chunks))
                tmp(1:self%n_chunks) = self%chunks(1:self%n_chunks)
                call move_alloc(tmp, self%chunks)
            end if
            self%n_chunks = self%n_chunks + 1
            self%chunks(self%n_chunks) = c
            covered = c%first + c%capacity - 1
        end do
    end subroutine reserve

    !> The part lo .. hi of points i1 .. i2 that chunk j holds, and its buffer
    !> p(xyz, high/low, lo:hi) (null if none).
    subroutine slice(self, j, i1, i2, lo, hi, p)
        class(LineChain), intent(in) :: self
        integer, intent(in) :: j, i1, i2
        integer, intent(out) :: lo, hi
        real(c_float), dimension(:,:,:), pointer, intent(out) :: p
        real(c_float), dimension(:,:,:), pointer :: all
        p => null()
        associate (c => self%chunks(j))
            lo = max(i1, c%first)
            hi = min(i2, c%first + c%capacity - 1)
            if (lo > hi) return
            call line_buffer(c%line, all)
            if (associated(all)) p => all(:, :, lo-c%first+1:hi-c%first+1)
        end associate
    end subroutine slice

    !> Chunk j was written up to point hi: upload it.
    subroutine changed(self, j, hi)
        class(LineChain), intent(in) :: self
        integer, intent(in) :: j, hi
        call check(vkf_line_points_changed(self%chunks(j)%line, int(hi - self%chunks(j)%first + 1, c_int)))
    end subroutine changed

    !> Draw points 1 .. k (no upload).
    subroutine draw(self, k)
        class(LineChain), intent(in) :: self
        integer, intent(in) :: k
        integer :: j, m
        do j = 1, self%n_chunks
            m = max(0, min(self%chunks(j)%capacity, k - self%chunks(j)%first + 1))
            call check(vkf_line_set_range(self%chunks(j)%line, 1_c_int, int(m, c_int)))
        end do
    end subroutine draw

    !> Color (rgba) and width (window units) of the whole chain.
    subroutine set_style(self, color, width)
        class(LineChain), intent(inout) :: self
        real(wp), intent(in) :: color(4), width
        integer :: j
        self%color = real(color, c_float)
        self%width = real(width, c_float)
        do j = 1, self%n_chunks
            call check(line_set_style(self%chunks(j)%line, self%color, self%width))
        end do
    end subroutine set_style

    !> Point i as written: read back from the buffers.
    function point(self, i) result(v)
        class(LineChain), intent(in) :: self
        integer, intent(in) :: i
        real(wp) :: v(3)
        real(c_float), dimension(:,:,:), pointer :: p
        integer :: j
        v = 0.0_wp
        do j = 1, self%n_chunks
            associate (c => self%chunks(j))
                if (i < c%first .or. i > c%first + c%capacity - 1) cycle
                call line_buffer(c%line, p)
                if (associated(p)) v = real(p(:, 1, i-c%first+1), wp) + real(p(:, 2, i-c%first+1), wp)
                return
            end associate
        end do
    end function point

    !> Points x(3, n) into p(xyz, high/low, n): the floats nearest, and the
    !> rest.
    pure subroutine split_points(x, p)
        real(wp), intent(in) :: x(:,:)
        real(c_float), intent(out) :: p(:,:,:)
        p(:, 1, :) = real(x, c_float)
        p(:, 2, :) = real(x - real(p(:, 1, :), wp), c_float)
    end subroutine split_points

    !> The point buffer of a line node: (xyz, high/low, point).
    subroutine line_buffer(line, p)
        type(c_ptr), intent(in) :: line
        real(c_float), dimension(:,:,:), pointer, intent(out) :: p
        integer(c_int) :: capacity
        type(c_ptr) :: data
        p => null()
        data = vkf_line_points(line, capacity)
        if (c_associated(data)) call c_f_pointer(data, p, [3, 2, int(capacity)])
    end subroutine line_buffer

    subroutine check(status)
        integer(c_int), intent(in) :: status
        if (status /= 0) write(*, '(a)') 'vkframes: a rendering-layer call failed (trajectory artists)'
    end subroutine check

end module vkf_line_chain
