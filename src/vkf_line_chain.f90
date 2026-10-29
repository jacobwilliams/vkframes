!> A growing set of points drawn from a chain of GPU buffers ("chunks"), for
!> the trajectory artists: a line strip, separate segments (pairs of points),
!> markers (one per point) or a ribbon (a translucent surface through pairs of
!> points: a quad between each pair and the next). Points are written in place, as (high, low)
!> float pairs for double precision (see src/vsg/vkf_layer.cpp: EyeRelative),
!> with a strip's distance along it (for dash patterns, see shaders/line.vert;
!> measured from the trajectory's points), and only the chunks written to are
!> uploaded again. A full chain gets new chunks, each as large as the chain so
!> far or as what is needed at once, but at least min_chunk and at most
!> max_chunk points, so nothing is ever copied or reallocated, and changing a
!> point uploads at most max_chunk points again. In a strip, consecutive chunks
!> share their end point so that the line is continuous; in a ribbon, their
!> end pair.
!>
!> Level of detail (strips): seen from far away, a dense trajectory is mostly
!> segments shorter than a pixel. For each chunk, the chain keeps the bounding
!> sphere of its points and, for strides 2, 4, ... 2^lod_levels, a bound on how
!> far the points skipped can be from the segments drawn (every stride-th point
!> and the last), and gives them to the renderer (vkf_line_set_lod), which picks
!> the stride per view (src/vsg/vkf_layer.cpp: EyeRelative). The bounds are
!> built level by level: a segment at stride 2s is within the larger of its two
!> halves' bounds, plus its middle point's distance from it, of the points it
!> skips (the halves' segments lie within that distance of it). They are
!> updated only for the segments the points written change.
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
    use vkf_layer, only: check_status, &
                         vkf_line_create, vkf_line_create_colored, vkf_line_colors, vkf_marker_create, vkf_ribbon_create, &
                         vkf_line_points, vkf_line_points_changed, &
                         vkf_line_set_range, vkf_node_add_child, vkf_node_remove_child, vkf_node_delete, &
                         vkf_compile_node, line_set_style, line_set_pattern, line_set_segments, &
                         marker_set_style, vkf_line_set_lod, vkf_line_set_highlight, vkf_line_set_trail

    implicit none

    private

    integer, parameter :: min_chunk = 64                !! points in the smallest chunk
    integer, parameter, public :: max_chunk = 65536     !! points in a chunk made for appending (1.5 MB)

    integer, parameter :: lod_levels = 12               !! strides up to 2^12 = 4096 (strips)

    !> what a chain draws
    integer, parameter, public :: CHAIN_STRIP = 0       !! a line strip
    integer, parameter, public :: CHAIN_SEGMENTS = 1    !! separate segments (pairs of points)
    integer, parameter, public :: CHAIN_MARKERS = 2     !! a marker per point
    integer, parameter, public :: CHAIN_RIBBON = 3      !! a ribbon (a quad between each pair of points and the next)

    public :: split_points, split_distances, line_buffer

    !> Error bounds of a strip's segments at one stride (level of detail).
    type :: LodLevel
        real(c_float), allocatable :: bound(:)          !! (0 : segments - 1)
    end type LodLevel

    !> A buffer holding points first .. first+capacity-1 of the chain.
    type :: LineChunk
        type(c_ptr) :: line = c_null_ptr                !! the line, marker or ribbon node
        integer :: first = 1                            !! the chain index of its first point
        integer :: capacity = 0                         !! number of points it holds
        ! level of detail (strips)
        type(LodLevel) :: lod(lod_levels)               !! error bounds at strides 2, 4, ... 2^lod_levels
        real(wp) :: box_lo(3) = 0.0_wp                  !! bounding box of the points written: lower corner
        real(wp) :: box_hi(3) = 0.0_wp                  !! bounding box of the points written: upper corner
        integer :: n_written = 0                        !! points written (the bounds are for them)
    end type LineChunk

    !> A chain of point buffers (see the module documentation).
    type, public :: LineChain
        private
        type(c_ptr) :: group = c_null_ptr               !! the group node the chunks are under (null: not built)
        logical :: compile_new = .false.                !! chunks made from now on are compiled (the group is shown)
        type(LineChunk), allocatable :: chunks(:)       !! (capacity): the chunks, in chain order
        integer :: n_chunks = 0                         !! number of chunks
        integer :: kind = CHAIN_STRIP                   !! what is drawn: a CHAIN_* constant
        type(c_ptr) :: image = c_null_ptr               !! (markers: the image (shape 10))
        logical :: colored = .false.                    !! (lines, markers: a color per point (color_slice))
        ! style
        real(c_float) :: color(4) = 1.0_c_float         !! [r, g, b, a] (0 to 1)
        real(c_float) :: width = 1.0_c_float            !! line width, or marker size
        integer :: shape = 0                            !! markers
        real(c_float) :: radius = 0.0_c_float           !! (markers: attenuation radius)
        integer :: pattern_factor = 1                   !! (lines: dash pattern (see line_set_pattern))
        integer :: pattern = int(z'FFFF')               !! (lines: 16-bit dash pattern (z'FFFF': solid))
        logical :: lod = .true.                         !! (strips: level of detail (else every point drawn))
        real(c_float) :: highlight(12) = [1.0_c_float, 1.0_c_float, 1.0_c_float, 1.0_c_float, &
                                          0.0_c_float, 40.0_c_float, 50.0_c_float, 12.0_c_float, &
                                          1.5_c_float, 0.0_c_float, 0.0_c_float, 0.0_c_float]
                                                        !! (lines: highlight, see vkf_line_set_highlight)
        real(c_float) :: trail(20) = 0.0_c_float        !! (strips: the trail, see vkf_line_set_trail; z = 0: none)
    contains
        procedure :: init, set_compile, is_built, is_colored, release, detach
        procedure :: reserve, slice, color_slice, written, draw, draw_range
        procedure :: set_style, set_pattern, set_marker_style, set_highlight, set_trail, style_node, point, point_color, distance, num_chunks
        procedure :: set_lod, lod_error, chunk_bounds
    end type LineChain

contains

    !> Draw under `group` (a CHAIN_* kind; markers with `image`, a
    !> vkf_image_read handle, for the image shape). Chunks made later are
    !> compiled once set_compile says so.
    subroutine init(self, group, kind, image, colored)
        class(LineChain), intent(inout) :: self
        type(c_ptr), intent(in) :: group
        integer, intent(in) :: kind                     !! a CHAIN_* constant
        type(c_ptr), intent(in), optional :: image      !! (markers: a vkf_image_read handle)
        logical, intent(in), optional :: colored       !! (lines, markers: a color per point (color_slice))
        self%group = group
        self%kind = kind
        self%compile_new = .false.
        self%image = c_null_ptr
        if (present(image)) self%image = image
        self%colored = .false.
        if (present(colored)) self%colored = colored
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
        self%compile_new = .false.
    end subroutine release

    !> Remove the chunks from the group (also while running), and release them.
    subroutine detach(self)
        class(LineChain), intent(inout) :: self
        integer :: j
        do j = 1, self%n_chunks
            call check_status(vkf_node_remove_child(self%group, self%chunks(j)%line), 'trajectory artists')
        end do
        call self%release()
    end subroutine detach

    !> Compile chunks made from now on (for the windows running): once the
    !> group is shown (not while building, when they are compiled with it).
    subroutine set_compile(self, compile)
        class(LineChain), intent(inout) :: self
        logical, intent(in) :: compile
        self%compile_new = compile
    end subroutine set_compile

    !> True if the chain has a group to draw under (between `init` and `release`).
    pure logical function is_built(self)
        class(LineChain), intent(in) :: self
        is_built = c_associated(self%group)
    end function is_built

    !> For lines and markers, whether each point has its own color (init's `colored`).
    pure logical function is_colored(self)
        class(LineChain), intent(in) :: self
        is_colored = self%colored
    end function is_colored

    !> Number of chunks.
    pure integer function num_chunks(self)
        class(LineChain), intent(in) :: self
        num_chunks = self%n_chunks
    end function num_chunks

    !> Chunks for at least n points (at least one chunk). Separate segments and
    !> ribbons: chunks hold whole pairs (n should be even).
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
                c%capacity = max(min_chunk, min(max_chunk, n))       ! the data so far, up to max_chunk
            else if (self%kind == CHAIN_STRIP) then
                c%first = covered                              ! sharing the previous chunk's last point
                c%capacity = min(max_chunk, max(min_chunk, covered, n - covered + 1))
            else if (self%kind == CHAIN_RIBBON) then
                c%first = covered - 1                          ! sharing the previous chunk's last pair
                c%capacity = min(max_chunk, max(min_chunk, covered, n - covered + 2))
            else
                c%first = covered + 1
                c%capacity = min(max_chunk, max(min_chunk, covered, n - covered))
            end if
            if (self%kind == CHAIN_SEGMENTS .or. self%kind == CHAIN_RIBBON) c%capacity = c%capacity + modulo(c%capacity, 2)
            if (self%kind == CHAIN_MARKERS) then
                c%line = vkf_marker_create(int(c%capacity, c_int), self%image, merge(1_c_int, 0_c_int, self%colored))
            else if (self%kind == CHAIN_RIBBON) then
                c%line = vkf_ribbon_create(int(c%capacity, c_int), merge(1_c_int, 0_c_int, self%colored))
            else
                if (self%colored) then
                    c%line = vkf_line_create_colored(int(c%capacity, c_int))
                else
                    c%line = vkf_line_create(int(c%capacity, c_int))
                end if
            end if
            if (.not. c_associated(c%line)) return
            if (self%kind == CHAIN_SEGMENTS) call check_status(line_set_segments(c%line, .true.), 'trajectory artists')
            call apply_style(self, c%line)
            if (self%compile_new) call check_status(vkf_compile_node(c%line), 'trajectory artists')
            call check_status(vkf_node_add_child(self%group, c%line), 'trajectory artists')
            if (.not. allocated(self%chunks)) allocate(self%chunks(4))
            if (self%n_chunks == size(self%chunks)) then
                allocate(tmp(2 * self%n_chunks))
                call move_chunks(self%chunks(1:self%n_chunks), tmp(1:self%n_chunks))
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
        integer, intent(out) :: lo, hi                              !! chain indices (lo > hi: none)
        real(c_float), dimension(:,:,:), pointer, intent(out) :: p  !! (4, 2, hi-lo+1)
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

    !> As `slice`, for the colors of a colored chain: c(rgba, point) of points
    !> lo .. hi of chunk j (null if not colored or nothing there).
    subroutine color_slice(self, j, i1, i2, lo, hi, c)
        class(LineChain), intent(in) :: self
        integer, intent(in) :: j, i1, i2
        integer, intent(out) :: lo, hi                              !! chain indices (lo > hi: none)
        real(c_float), dimension(:,:), pointer, intent(out) :: c    !! (4, hi-lo+1): [r, g, b, a]
        real(c_float), dimension(:,:), pointer :: all
        integer(c_int) :: capacity
        type(c_ptr) :: data
        c => null()
        associate (k => self%chunks(j))
            lo = max(i1, k%first)
            hi = min(i2, k%first + k%capacity - 1)
            if (lo > hi .or. .not. self%colored) return
            data = vkf_line_colors(k%line, capacity)
            if (.not. c_associated(data)) return
            call c_f_pointer(data, all, [4, int(capacity)])
            c => all(:, lo-k%first+1:hi-k%first+1)
        end associate
    end subroutine color_slice

    !> Chunks moved to a new array (their level-of-detail bounds moved, not copied).
    subroutine move_chunks(from, to)
        type(LineChunk), intent(inout) :: from(:), to(:)
        integer :: i, k
        do i = 1, size(from)
            to(i)%line = from(i)%line
            to(i)%first = from(i)%first
            to(i)%capacity = from(i)%capacity
            to(i)%box_lo = from(i)%box_lo
            to(i)%box_hi = from(i)%box_hi
            to(i)%n_written = from(i)%n_written
            do k = 1, lod_levels
                if (allocated(from(i)%lod(k)%bound)) call move_alloc(from(i)%lod(k)%bound, to(i)%lod(k)%bound)
            end do
        end do
    end subroutine move_chunks

    !> Points i1 .. i2 were written: measure a strip's distances along it (from
    !> the distance at point i1-1), update its level of detail, and upload the
    !> chunks holding them. `x`: the points in double precision, x(:, i) for
    !> chain point i (at least i1-1 .. i2), to measure from; without it, from
    !> the floats written. (i2 is the last point written in the chain.)
    subroutine written(self, i1, i2, x)
        class(LineChain), intent(inout) :: self
        integer, intent(in) :: i1, i2
        real(wp), intent(in), optional :: x(:,:)        !! (3, at least i1-1 .. i2): the points
        integer :: j, lo, hi
        do j = 1, self%n_chunks
            associate (c => self%chunks(j))
                lo = max(i1, c%first)
                hi = min(i2, c%first + c%capacity - 1)
                if (lo > hi) cycle
                if (self%kind == CHAIN_STRIP) then
                    call measure(self, j, lo, hi, x)
                    call update_lod(self, j, lo, hi, x)
                end if
                call check_status(vkf_line_points_changed(c%line, int(hi - c%first + 1, c_int)), 'trajectory artists')
            end associate
        end do
    end subroutine written

    !> The distances along the strip of points lo .. hi of chunk j: from the
    !> previous point (in this chunk; the first point of a chunk after the
    !> first is the previous chunk's last, measured before), or 0 at point 1.
    !> From `x` (as in `written`) if given, else from the floats written.
    subroutine measure(self, j, lo, hi, x)
        type(LineChain), intent(in) :: self
        integer, intent(in) :: j, lo, hi
        real(wp), intent(in), optional :: x(:,:)
        real(c_float), dimension(:,:,:), pointer :: p, prev
        real(wp), allocatable :: xf(:,:), s(:)
        integer :: k0, k1, k, g
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
        if (present(x)) then                               ! (one pass, no temporary arrays)
            g = self%chunks(j)%first - 1                   ! chain index = g + local index
            do k = k0 + 1, k1
                s(k) = s(k - 1) + norm2(x(:, g + k) - x(:, g + k - 1))
            end do
        else
            xf = real(p(1:3, 1, k0:k1), wp) + real(p(1:3, 2, k0:k1), wp)
            s(k0+1:k1) = norm2(xf(:, 2:) - xf(:, :k1-k0), dim=1)
            do k = k0 + 1, k1
                s(k) = s(k - 1) + s(k)
            end do
        end if
        call split_distances(s, p(4, :, k0:k1))
    end subroutine measure

    !> Points lo .. hi of chunk j (a strip's) were written, the last being hi:
    !> update the bounding box and the error bounds of the segments they change
    !> at each stride, and give them to the renderer. `x` as in `written`.
    subroutine update_lod(self, j, lo, hi, x)
        type(LineChain), intent(inout) :: self
        integer, intent(in) :: j, lo, hi
        real(wp), intent(in), optional :: x(:,:)
        real(c_float), dimension(:,:,:), pointer :: p
        real(wp) :: b0, b1, q(3)
        integer :: n, q0, k, s, m, seg, a, e, mid, i
        associate (c => self%chunks(j))
            call line_buffer(c%line, p)
            if (.not. associated(p)) return
            n = hi - c%first + 1                        ! points written in the chunk (local 0 .. n-1)
            c%n_written = n
            q0 = lo - c%first                           ! the first one written now
            do i = q0, n - 1                            ! the bounding box
                q = pt(i)
                if (i == 0) then
                    c%box_lo = q
                    c%box_hi = q
                else
                    c%box_lo = min(c%box_lo, q)
                    c%box_hi = max(c%box_hi, q)
                end if
            end do
            do k = 1, lod_levels
                s = 2**k
                m = 0
                if (n >= 2) m = (n - 2) / s + 1         ! segments at stride s
                if (m == 0) exit
                if (.not. allocated(c%lod(k)%bound)) allocate(c%lod(k)%bound(0:(c%capacity - 2) / s))
                do seg = max(0, q0 - 1) / s, m - 1     ! the segments the points written change
                    a = seg * s
                    e = min(a + s, n - 1)
                    mid = a + s / 2
                    b0 = 0.0_wp                         ! the halves' bounds (none at stride 1)
                    b1 = 0.0_wp
                    if (k > 1) b0 = c%lod(k - 1)%bound(2 * seg)
                    if (mid < e) then                   ! two halves: also the middle point's distance
                        if (k > 1) b1 = c%lod(k - 1)%bound(2 * seg + 1)
                        c%lod(k)%bound(seg) = real(max(b0, b1) + segment_distance(pt(mid), pt(a), pt(e)), c_float)
                    else
                        c%lod(k)%bound(seg) = real(b0, c_float)
                    end if
                end do
            end do
        end associate
        call send_lod(self, j)

    contains

        !> Local point i of the chunk, in double precision.
        function pt(i) result(v)
            integer, intent(in) :: i
            real(wp) :: v(3)
            if (present(x)) then
                v = x(:, self%chunks(j)%first + i)
            else
                v = real(p(1:3, 1, i + 1), wp) + real(p(1:3, 2, i + 1), wp)
            end if
        end function pt

    end subroutine update_lod

    !> Give chunk j's level of detail to the renderer (none if switched off).
    subroutine send_lod(self, j)
        type(LineChain), intent(in) :: self
        integer, intent(in) :: j
        associate (c => self%chunks(j))
            call check_status(vkf_line_set_lod(c%line, 0.5_wp * (c%box_lo + c%box_hi), 0.5_wp * norm2(c%box_hi - c%box_lo), &
                                        self%lod_error(j), merge(int(lod_levels, c_int), 0_c_int, self%lod)), 'trajectory artists')
        end associate
    end subroutine send_lod

    !> Level of detail on (the default: seen from far away, a strip is drawn
    !> from fewer points, within half a pixel of all of them) or off (every
    !> point drawn).
    subroutine set_lod(self, enabled)
        class(LineChain), intent(inout) :: self
        logical, intent(in) :: enabled
        integer :: j
        self%lod = enabled
        if (self%kind /= CHAIN_STRIP) return
        do j = 1, self%n_chunks
            call send_lod(self, j)
        end do
    end subroutine set_lod

    !> Chunk j's error bounds: how far the points skipped can be from the
    !> segments drawn at stride 2^k (k = 1 .. lod_levels; 0 where not known).
    function lod_error(self, j) result(error)
        class(LineChain), intent(in) :: self
        integer, intent(in) :: j
        real(c_float) :: error(lod_levels)
        integer :: k, n, m
        error = 0.0_c_float
        if (j < 1 .or. j > self%n_chunks) return
        n = 0
        do k = 1, lod_levels
            if (.not. allocated(self%chunks(j)%lod(k)%bound)) exit
            if (k == 1) n = self%chunks(j)%n_written
            m = 0
            if (n >= 2) m = (n - 2) / 2**k + 1
            if (m > 0) error(k) = maxval(self%chunks(j)%lod(k)%bound(0:m-1))
        end do
    end function lod_error

    !> Distance from point v to the segment from a to b.
    pure real(wp) function segment_distance(v, a, b) result(d)
        real(wp), intent(in) :: v(3), a(3), b(3)
        real(wp) :: ab(3), t, len2
        ab = b - a
        len2 = dot_product(ab, ab)
        t = 0.0_wp
        if (len2 > 0.0_wp) t = max(0.0_wp, min(1.0_wp, dot_product(v - a, ab) / len2))
        d = norm2(v - (a + t * ab))
    end function segment_distance

    !> Draw points 1 .. k (no upload).
    subroutine draw(self, k)
        class(LineChain), intent(in) :: self
        integer, intent(in) :: k
        call self%draw_range(1, k)
    end subroutine draw

    !> Draw points i1 .. i2 only (chain indices; none if i2 < i1): e.g. a trace
    !> of decreasing times, its last points.
    subroutine draw_range(self, i1, i2)
        class(LineChain), intent(in) :: self
        integer, intent(in) :: i1, i2
        integer :: j, lo, hi
        do j = 1, self%n_chunks
            associate (c => self%chunks(j))
                lo = max(i1, c%first)
                hi = min(i2, c%first + c%capacity - 1)
                if (hi >= lo) then
                    call check_status(vkf_line_set_range(c%line, int(lo - c%first + 1, c_int), int(hi - lo + 1, c_int)), &
                                      'trajectory artists')
                else
                    call check_status(vkf_line_set_range(c%line, 1_c_int, 0_c_int), 'trajectory artists')
                end if
            end associate
        end do
    end subroutine draw_range

    !> For lines, a dash pattern (see line_set_pattern; z'FFFF' solid).
    subroutine set_pattern(self, factor, pattern)
        class(LineChain), intent(inout) :: self
        integer, intent(in) :: factor           !! repeat count of each bit of the pattern
        integer, intent(in) :: pattern          !! 16-bit dash pattern (z'FFFF': solid)
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
        real(wp), intent(in) :: color(4)        !! [r, g, b, a] (0 to 1)
        real(wp), intent(in) :: width           !! (window units)
        integer :: j
        self%color = real(color, c_float)
        self%width = real(width, c_float)
        do j = 1, self%n_chunks
            call apply_style(self, self%chunks(j)%line)
        end do
    end subroutine set_style

    !> For lines, the highlight (see shaders/line.vert): `flags` (1 pulse, 2
    !> arrows; 0 none), its color (rgba), the arrows' spacing and speed (window
    !> units, and per second), the band's width (window units) and the pulse's
    !> period (s).
    subroutine set_highlight(self, flags, color, spacing, speed, band, period)
        class(LineChain), intent(inout) :: self
        integer, intent(in) :: flags            !! 1 pulse + 2 arrows (0: none)
        real(wp), intent(in) :: color(4)        !! [r, g, b, a] (0 to 1)
        real(wp), intent(in) :: spacing         !! between arrows (window units)
        real(wp), intent(in) :: speed           !! of the arrows (window units per second)
        real(wp), intent(in) :: band            !! width of the band the arrows are drawn in (window units)
        real(wp), intent(in) :: period          !! of the pulse (s)
        integer :: j
        self%highlight = real([color, real(flags, wp), spacing, speed, band, period, 0.0_wp, 0.0_wp, 0.0_wp], &
                              c_float)
        do j = 1, self%n_chunks
            call apply_style(self, self%chunks(j)%line)
        end do
    end subroutine set_highlight

    !> For strips, the trail (see vkf_line_set_trail: 20 values; the third 0:
    !> none), given to every chunk (changed every frame the time does: a few
    !> floats per chunk uploaded).
    subroutine set_trail(self, values)
        class(LineChain), intent(inout) :: self
        real(c_float), intent(in) :: values(20)
        integer :: j
        self%trail = values
        if (self%kind /= CHAIN_STRIP) return
        do j = 1, self%n_chunks
            call check_status(vkf_line_set_trail(self%chunks(j)%line, self%trail), 'trajectory artists')
        end do
    end subroutine set_trail

    !> Give the chain's style to `line`, a node not in the chain (e.g. a
    !> curve's trace-mode tail, drawn as part of it).
    subroutine style_node(self, line)
        class(LineChain), intent(in) :: self
        type(c_ptr), intent(in) :: line
        call apply_style(self, line)
    end subroutine style_node

    !> For markers, the color (rgba), size (window units), shape (shaders/marker.frag)
    !> and attenuation radius (scene units, 0 for none).
    subroutine set_marker_style(self, color, size, shape, radius)
        class(LineChain), intent(inout) :: self
        real(wp), intent(in) :: color(4)        !! [r, g, b, a] (0 to 1)
        real(wp), intent(in) :: size            !! (window units)
        real(wp), intent(in) :: radius          !! attenuation radius (scene units; 0: none)
        integer, intent(in) :: shape            !! marker shape (shaders/marker.frag)
        integer :: j
        self%color = real(color, c_float)
        self%width = real(size, c_float)
        self%shape = shape
        self%radius = real(radius, c_float)
        do j = 1, self%n_chunks
            call apply_style(self, self%chunks(j)%line)
        end do
    end subroutine set_marker_style

    !> Give the chain's style (color, width or size, pattern, highlight, marker shape) to the node `line`.
    subroutine apply_style(self, line)
        type(LineChain), intent(in) :: self
        type(c_ptr), intent(in) :: line
        if (self%kind == CHAIN_MARKERS) then
            call check_status(marker_set_style(line, self%color, self%width, self%shape, self%radius), 'trajectory artists')
        else
            call check_status(line_set_style(line, self%color, self%width), 'trajectory artists')
            call check_status(line_set_pattern(line, self%pattern_factor, self%pattern), 'trajectory artists')
            if (self%kind /= CHAIN_RIBBON) &
                call check_status(vkf_line_set_highlight(line, self%highlight), 'trajectory artists')
            if (self%kind == CHAIN_STRIP) call check_status(vkf_line_set_trail(line, self%trail), 'trajectory artists')
        end if
    end subroutine apply_style

    !> Chunk j's points (strips: first .. first + n - 1 of the chain) and the
    !> bounding box of them (n = 0 if none are measured yet), e.g. to skip a
    !> chunk when picking.
    subroutine chunk_bounds(self, j, first, n, lo, hi)
        class(LineChain), intent(in) :: self
        integer, intent(in) :: j
        integer, intent(out) :: first, n
        real(wp), intent(out) :: lo(3), hi(3)   !! the bounding box's corners
        first = self%chunks(j)%first
        n = self%chunks(j)%n_written
        lo = self%chunks(j)%box_lo
        hi = self%chunks(j)%box_hi
    end subroutine chunk_bounds

    !> Point i as written: read back from the buffers (zero if not held).
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

    !> The color of point i of a colored chain, as stored (rgba; white if not colored).
    function point_color(self, i) result(rgba)
        class(LineChain), intent(in) :: self
        integer, intent(in) :: i
        real(wp) :: rgba(4)
        real(c_float), dimension(:,:), pointer :: c
        integer :: j, lo, hi
        rgba = 1.0_wp
        do j = 1, self%n_chunks
            call self%color_slice(j, i, i, lo, hi, c)
            if (associated(c)) then
                rgba = real(c(:, 1), wp)
                return
            end if
        end do
    end function point_color

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
        real(wp), intent(in) :: x(:,:)              !! (3, n)
        real(c_float), intent(inout) :: p(:,:,:)    !! (4, 2, n)
        p(1:3, 1, :) = real(x, c_float)
        p(1:3, 2, :) = real(x - real(p(1:3, 1, :), wp), c_float)
    end subroutine split_points

    !> Distances s(n) into d(high/low, n).
    pure subroutine split_distances(s, d)
        real(wp), intent(in) :: s(:)                !! (n)
        real(c_float), intent(inout) :: d(:,:)      !! (2, n)
        d(1, :) = real(s, c_float)
        d(2, :) = real(s - real(d(1, :), wp), c_float)
    end subroutine split_distances

    !> The point buffer of a line or marker node: (xyz and distance, high/low,
    !> point).
    subroutine line_buffer(line, p)
        type(c_ptr), intent(in) :: line
        real(c_float), dimension(:,:,:), pointer, intent(out) :: p  !! (4, 2, capacity); null if none
        integer(c_int) :: capacity
        type(c_ptr) :: data
        p => null()
        data = vkf_line_points(line, capacity)
        if (c_associated(data)) call c_f_pointer(data, p, [4, 2, int(capacity)])
    end subroutine line_buffer

end module vkf_line_chain
