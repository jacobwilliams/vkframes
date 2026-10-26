!> A growing set of points drawn from a chain of GPU buffers ("chunks"), for
!> the trajectory artists: a line strip, separate segments (pairs of points)
!> or markers (one per point). Points are written in place, as (high, low)
!> float pairs for double precision (see src/vsg/vkf_layer.cpp: EyeRelative),
!> with a strip's distance along it (for dash patterns, see shaders/line.vert;
!> measured from the points written), and only the chunks written to are
!> uploaded again. A full chain gets a new
!> chunk as large as the chain so far (at least min_chunk, at most max_chunk
!> points, or what is needed at once), so nothing is ever copied or
!> reallocated. In a strip, consecutive chunks share their end point so that
!> the line is continuous.
!>
!> Writing points i1 .. i2 (indices along the whole chain):
!>
!>     call chain%reserve(i2)
!>     do j = 1, chain%num_chunks()
!>         call chain%slice(j, i1, i2, lo, hi, p)      ! p: points lo .. hi of chunk j
!>         if (.not. associated(p)) cycle
!>         ... fill p(1:3, :, 1:hi-lo+1) (e.g. with split_points) ...
!>     end do
!>     call chain%written(i1, i2)                     ! measure (strips) and upload
!>     call chain%draw(i2)
module vkf_line_chain

    use, intrinsic :: iso_c_binding, only: c_ptr, c_null_ptr, c_associated, c_int, c_float, c_f_pointer
    use vkf_math, only: wp
    use vkf_layer, only: vkf_line_create, vkf_marker_create, vkf_line_points, vkf_line_points_changed, &
                         vkf_line_set_range, vkf_node_add_child, vkf_node_remove_child, vkf_node_delete, &
                         vkf_viewer_compile_node, line_set_style, line_set_pattern, line_set_segments, &
                         marker_set_style

    implicit none

    private

    integer, parameter :: min_chunk = 64                !! points in the smallest chunk
    integer, parameter, public :: max_chunk = 65536     !! points in a chunk made for appending (1.5 MB)

    ! what a chain draws
    integer, parameter, public :: CHAIN_STRIP = 0, CHAIN_SEGMENTS = 1, CHAIN_MARKERS = 2

    public :: split_points, split_distances, line_buffer, check

    !> A buffer holding points first .. first+capacity-1 of the chain.
    type :: LineChunk
        type(c_ptr) :: line = c_null_ptr
        integer :: first = 1, capacity = 0
    end type LineChunk

    type, public :: LineChain
        private
        type(c_ptr) :: viewer = c_null_ptr, group = c_null_ptr
        type(LineChunk), allocatable :: chunks(:)
        integer :: n_chunks = 0
        integer :: kind = CHAIN_STRIP
        type(c_ptr) :: image = c_null_ptr               !! markers: the image (shape 10)
        ! style
        real(c_float) :: color(4) = 1.0_c_float
        real(c_float) :: width = 1.0_c_float            !! line width, or marker size
        integer :: shape = 0                            !! markers
        real(c_float) :: radius = 0.0_c_float           !! markers: attenuation radius
        integer :: pattern_factor = 1                   !! lines: dash pattern (see line_set_pattern)
        integer :: pattern = int(z'FFFF')
    contains
        procedure :: init, set_viewer, is_built, release, detach
        procedure :: reserve, slice, written, draw
        procedure :: set_style, set_pattern, set_marker_style, point, distance, num_chunks
    end type LineChain

contains

    !> Draw under `group` (a CHAIN_* kind; markers with `image`, a
    !> vkf_image_read handle, for the image shape). Chunks made later are
    !> compiled by the viewer set with set_viewer.
    subroutine init(self, group, kind, image)
        class(LineChain), intent(inout) :: self
        type(c_ptr), intent(in) :: group
        integer, intent(in) :: kind
        type(c_ptr), intent(in), optional :: image
        self%group = group
        self%kind = kind
        self%viewer = c_null_ptr
        self%image = c_null_ptr
        if (present(image)) self%image = image
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

    !> Remove the chunks from the group (also while running), and release them.
    subroutine detach(self)
        class(LineChain), intent(inout) :: self
        integer :: j
        do j = 1, self%n_chunks
            call check(vkf_node_remove_child(self%group, self%chunks(j)%line))
        end do
        call self%release()
    end subroutine detach

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
            else if (self%kind == CHAIN_STRIP) then
                c%first = covered                              ! sharing the previous chunk's last point
                c%capacity = max(n - covered + 1, min(max_chunk, max(min_chunk, covered)))
            else
                c%first = covered + 1
                c%capacity = max(n - covered, min(max_chunk, max(min_chunk, covered)))
            end if
            if (self%kind == CHAIN_SEGMENTS) c%capacity = c%capacity + modulo(c%capacity, 2)
            if (self%kind == CHAIN_MARKERS) then
                c%line = vkf_marker_create(int(c%capacity, c_int), self%image)
            else
                c%line = vkf_line_create(int(c%capacity, c_int))
            end if
            if (.not. c_associated(c%line)) return
            if (self%kind == CHAIN_SEGMENTS) call check(line_set_segments(c%line, .true.))
            call apply_style(self, c%line)
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
    !> p(xyz and distance, high/low, lo:hi) (null if none).
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

    !> Points i1 .. i2 were written: measure a strip's distances along it (from
    !> the distance at point i1-1), and upload the chunks holding them.
    subroutine written(self, i1, i2)
        class(LineChain), intent(in) :: self
        integer, intent(in) :: i1, i2
        integer :: j, lo, hi
        do j = 1, self%n_chunks
            associate (c => self%chunks(j))
                lo = max(i1, c%first)
                hi = min(i2, c%first + c%capacity - 1)
                if (lo > hi) cycle
                if (self%kind == CHAIN_STRIP) call measure(self, j, lo, hi)
                call check(vkf_line_points_changed(c%line, int(hi - c%first + 1, c_int)))
            end associate
        end do
    end subroutine written

    !> The distances along the strip of points lo .. hi of chunk j: from the
    !> previous point (in this chunk; the first point of a chunk after the
    !> first is the previous chunk's last, measured before), or 0 at point 1.
    subroutine measure(self, j, lo, hi)
        type(LineChain), intent(in) :: self
        integer, intent(in) :: j, lo, hi
        real(c_float), dimension(:,:,:), pointer :: p, prev
        real(wp), allocatable :: x(:,:), s(:)
        integer :: k0, k1, k
        call line_buffer(self%chunks(j)%line, p)
        if (.not. associated(p)) return
        k1 = hi - self%chunks(j)%first + 1                 ! local indices k0 .. k1
        k0 = lo - self%chunks(j)%first + 1
        if (lo > 1 .and. k0 > 1) k0 = k0 - 1               ! from the point before
        allocate(s(k0:k1))
        if (lo == 1) then
            s(k0) = 0.0_wp
        else if (k0 == lo - self%chunks(j)%first + 1) then ! the chunk's first point: shared with the previous
            call line_buffer(self%chunks(j-1)%line, prev)
            k = self%chunks(j)%first - self%chunks(j-1)%first + 1
            s(k0) = real(prev(4, 1, k), wp) + real(prev(4, 2, k), wp)
        else
            s(k0) = real(p(4, 1, k0), wp) + real(p(4, 2, k0), wp)
        end if
        x = real(p(1:3, 1, k0:k1), wp) + real(p(1:3, 2, k0:k1), wp)
        s(k0+1:k1) = norm2(x(:, 2:) - x(:, :k1-k0), dim=1)
        do k = k0 + 1, k1
            s(k) = s(k - 1) + s(k)
        end do
        call split_distances(s, p(4, :, k0:k1))
    end subroutine measure

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

    !> Lines: a dash pattern (see line_set_pattern; z'FFFF' solid).
    subroutine set_pattern(self, factor, pattern)
        class(LineChain), intent(inout) :: self
        integer, intent(in) :: factor, pattern
        integer :: j
        self%pattern_factor = factor
        self%pattern = pattern
        do j = 1, self%n_chunks
            call apply_style(self, self%chunks(j)%line)
        end do
    end subroutine set_pattern

    !> Color (rgba) and width (window units) of the whole chain.
    subroutine set_style(self, color, width)
        class(LineChain), intent(inout) :: self
        real(wp), intent(in) :: color(4), width
        integer :: j
        self%color = real(color, c_float)
        self%width = real(width, c_float)
        do j = 1, self%n_chunks
            call apply_style(self, self%chunks(j)%line)
        end do
    end subroutine set_style

    !> Markers: color (rgba), size (window units), shape (shaders/marker.frag)
    !> and attenuation radius (scene units, 0 for none).
    subroutine set_marker_style(self, color, size, shape, radius)
        class(LineChain), intent(inout) :: self
        real(wp), intent(in) :: color(4), size, radius
        integer, intent(in) :: shape
        integer :: j
        self%color = real(color, c_float)
        self%width = real(size, c_float)
        self%shape = shape
        self%radius = real(radius, c_float)
        do j = 1, self%n_chunks
            call apply_style(self, self%chunks(j)%line)
        end do
    end subroutine set_marker_style

    subroutine apply_style(self, line)
        type(LineChain), intent(in) :: self
        type(c_ptr), intent(in) :: line
        if (self%kind == CHAIN_MARKERS) then
            call check(marker_set_style(line, self%color, self%width, self%shape, self%radius))
        else
            call check(line_set_style(line, self%color, self%width))
            call check(line_set_pattern(line, self%pattern_factor, self%pattern))
        end if
    end subroutine apply_style

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
                if (associated(p)) v = real(p(1:3, 1, i-c%first+1), wp) + real(p(1:3, 2, i-c%first+1), wp)
                return
            end associate
        end do
    end function point

    !> A strip's distance along it at point i, as written.
    function distance(self, i) result(s)
        class(LineChain), intent(in) :: self
        integer, intent(in) :: i
        real(wp) :: s
        real(c_float), dimension(:,:,:), pointer :: p
        integer :: j
        s = 0.0_wp
        do j = 1, self%n_chunks
            associate (c => self%chunks(j))
                if (i < c%first .or. i > c%first + c%capacity - 1) cycle
                call line_buffer(c%line, p)
                if (associated(p)) s = real(p(4, 1, i-c%first+1), wp) + real(p(4, 2, i-c%first+1), wp)
                return
            end associate
        end do
    end function distance

    !> Points x(3, n) into p(xyz and distance, high/low, n): the floats
    !> nearest, and the rest (the distances are left as they are).
    pure subroutine split_points(x, p)
        real(wp), intent(in) :: x(:,:)
        real(c_float), intent(inout) :: p(:,:,:)
        p(1:3, 1, :) = real(x, c_float)
        p(1:3, 2, :) = real(x - real(p(1:3, 1, :), wp), c_float)
    end subroutine split_points

    !> Distances s(n) into d(high/low, n).
    pure subroutine split_distances(s, d)
        real(wp), intent(in) :: s(:)
        real(c_float), intent(inout) :: d(:,:)
        d(1, :) = real(s, c_float)
        d(2, :) = real(s - real(d(1, :), wp), c_float)
    end subroutine split_distances

    !> The point buffer of a line or marker node: (xyz and distance, high/low,
    !> point).
    subroutine line_buffer(line, p)
        type(c_ptr), intent(in) :: line
        real(c_float), dimension(:,:,:), pointer, intent(out) :: p
        integer(c_int) :: capacity
        type(c_ptr) :: data
        p => null()
        data = vkf_line_points(line, capacity)
        if (c_associated(data)) call c_f_pointer(data, p, [4, 2, int(capacity)])
    end subroutine line_buffer

    subroutine check(status)
        integer(c_int), intent(in) :: status
        if (status /= 0) write(*, '(a)') 'vkframes: a rendering-layer call failed (trajectory artists)'
    end subroutine check

end module vkf_line_chain
