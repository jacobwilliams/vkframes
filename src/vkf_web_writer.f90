!> Writing a scene for the web (WindowProxy%export_web): the page's scene
!> description (JSON), its binary arrays (one buffer of float32, float64 and
!> uint32 arrays, referred to from the JSON by byte offset and count), its
!> assets (textures and models, copied from their files), and the page
!> itself (one self-contained HTML file: the JSON, the arrays and the assets
!> inlined, base64 for the binary ones, and vkframes' Three.js viewer,
!> data/web/vkf_viewer.js).
!>
!> Frames write what they draw through the item writers here
!> ([[WebWriter:begin_line_item]], [[WebWriter:begin_point_item]],
!> [[WebWriter:begin_mesh_item]]): positions in float32, cut into chunks
!> that each have their own origin (in double precision), so that points far
!> from the frame's origin keep their precision (the viewer places each chunk
!> at its origin, and Three.js composes the transforms in double precision);
!> times in float32 relative to the export's reference time
!> ([[WebWriter:t_ref]]).
module vkf_web_writer

    use, intrinsic :: iso_fortran_env, only: int8, int32, int64, real32, real64
    use vkf_math, only: wp

    implicit none

    private

    integer, parameter :: max_depth = 64                !! JSON nesting
    integer, parameter, public :: WEB_CHUNK = 8192      !! points per chunk of a line or point item

    !> A file inlined in the page (a texture or a model).
    type :: WebAsset
        character(len=:), allocatable :: path           !! the file
        character(len=:), allocatable :: mime           !! its media type
    end type WebAsset

    !> The scene being written (see the module documentation).
    type, public :: WebWriter
        character(len=:), allocatable :: json           !! the JSON so far (1:nj used)
        integer(int64) :: nj = 0
        logical :: first(0:max_depth) = .true.         !! nothing written yet at each depth
        integer :: depth = 0
        integer(int8), allocatable :: bin(:)            !! the binary buffer (1:nb used)
        integer(int64) :: nb = 0
        type(WebAsset), allocatable :: assets(:)        !! the assets (1:n_assets)
        integer :: n_assets = 0
        character(len=:), allocatable :: warned         !! the warnings given ('|key|key|')
        ! the export's times
        real(wp) :: t_ref = 0.0_wp                      !! times are written relative to it
        real(wp) :: now = 0.0_wp                        !! the scene's time (a snapshot's, or the animation's start)
        real(wp) :: t_start = 0.0_wp, t_end = 0.0_wp    !! the animation's time range
        logical :: animate = .false.                    !! animated (else a snapshot at `now`)
        integer :: samples = 600                        !! poses sampled across the range for each followed frame
        integer :: max_points = 0                       !! the most points written for a curve (0: all)
    contains
        procedure :: begin_object, end_object, begin_array, end_array
        procedure :: put_string, put_real, put_int, put_logical, put_reals, put_ints, put_strings
        generic :: put => put_string, put_real, put_int, put_logical, put_reals, put_ints, put_strings
        procedure :: put_f32, put_f64, put_u32
        procedure :: begin_line_item, begin_point_item, begin_mesh_item
        procedure :: add_asset, warn_once
        procedure :: write_html, json_text, num_bytes
    end type WebWriter

    public :: base64_encode, base64_decode, json_number

contains

    ! ==================================================================
    ! JSON

    !> Append text to the JSON.
    subroutine emit(self, s)
        class(WebWriter), intent(inout) :: self
        character(len=*), intent(in) :: s
        character(len=:), allocatable :: tmp
        integer(int64) :: need, cap
        if (.not. allocated(self%json)) allocate(character(len=65536) :: self%json)
        need = self%nj + len(s)
        if (need > len(self%json, int64)) then
            cap = max(2 * len(self%json, int64), need)
            allocate(character(len=cap) :: tmp)
            tmp(1:self%nj) = self%json(1:self%nj)
            call move_alloc(tmp, self%json)
        end if
        self%json(self%nj+1:need) = s
        self%nj = need
    end subroutine emit

    !> A separator if something came before at this depth, and the key.
    subroutine start_value(self, key)
        class(WebWriter), intent(inout) :: self
        character(len=*), intent(in), optional :: key
        if (.not. self%first(self%depth)) call emit(self, ',')
        self%first(self%depth) = .false.
        if (present(key)) call emit(self, quoted(key)//':')
    end subroutine start_value

    !> Begin an object (a value of `key`, or in an array).
    subroutine begin_object(self, key)
        class(WebWriter), intent(inout) :: self
        character(len=*), intent(in), optional :: key      !! (absent: in an array, or the top level)
        call start_value(self, key)
        call emit(self, '{')
        self%depth = min(self%depth + 1, max_depth)
        self%first(self%depth) = .true.
    end subroutine begin_object

    subroutine end_object(self)
        class(WebWriter), intent(inout) :: self
        call emit(self, '}')
        self%depth = max(self%depth - 1, 0)
    end subroutine end_object

    !> Begin an array (a value of `key`, or in an array).
    subroutine begin_array(self, key)
        class(WebWriter), intent(inout) :: self
        character(len=*), intent(in), optional :: key
        call start_value(self, key)
        call emit(self, '[')
        self%depth = min(self%depth + 1, max_depth)
        self%first(self%depth) = .true.
    end subroutine begin_array

    subroutine end_array(self)
        class(WebWriter), intent(inout) :: self
        call emit(self, ']')
        self%depth = max(self%depth - 1, 0)
    end subroutine end_array

    subroutine put_string(self, key, value)
        class(WebWriter), intent(inout) :: self
        character(len=*), intent(in) :: key, value
        call start_value(self, key)
        call emit(self, quoted(value))
    end subroutine put_string

    subroutine put_real(self, key, value)
        class(WebWriter), intent(inout) :: self
        character(len=*), intent(in) :: key
        real(wp), intent(in) :: value
        call start_value(self, key)
        call emit(self, json_number(value))
    end subroutine put_real

    subroutine put_int(self, key, value)
        class(WebWriter), intent(inout) :: self
        character(len=*), intent(in) :: key
        integer, intent(in) :: value
        character(len=24) :: s
        call start_value(self, key)
        write(s, '(i0)') value
        call emit(self, trim(s))
    end subroutine put_int

    subroutine put_logical(self, key, value)
        class(WebWriter), intent(inout) :: self
        character(len=*), intent(in) :: key
        logical, intent(in) :: value
        call start_value(self, key)
        if (value) then
            call emit(self, 'true')
        else
            call emit(self, 'false')
        end if
    end subroutine put_logical

    !> A small array of numbers, inline (large ones: put_f32, put_f64).
    subroutine put_reals(self, key, values)
        class(WebWriter), intent(inout) :: self
        character(len=*), intent(in) :: key
        real(wp), intent(in) :: values(:)
        integer :: k
        call start_value(self, key)
        call emit(self, '[')
        do k = 1, size(values)
            if (k > 1) call emit(self, ',')
            call emit(self, json_number(values(k)))
        end do
        call emit(self, ']')
    end subroutine put_reals

    subroutine put_ints(self, key, values)
        class(WebWriter), intent(inout) :: self
        character(len=*), intent(in) :: key
        integer, intent(in) :: values(:)
        character(len=24) :: s
        integer :: k
        call start_value(self, key)
        call emit(self, '[')
        do k = 1, size(values)
            if (k > 1) call emit(self, ',')
            write(s, '(i0)') values(k)
            call emit(self, trim(s))
        end do
        call emit(self, ']')
    end subroutine put_ints

    subroutine put_strings(self, key, values)
        class(WebWriter), intent(inout) :: self
        character(len=*), intent(in) :: key
        character(len=*), intent(in) :: values(:)
        integer :: k
        call start_value(self, key)
        call emit(self, '[')
        do k = 1, size(values)
            if (k > 1) call emit(self, ',')
            call emit(self, quoted(trim(values(k))))
        end do
        call emit(self, ']')
    end subroutine put_strings

    !> A number as JSON (17 significant digits; whole numbers without a
    !> fraction; beyond +-1e30, +-1e30, as the viewer's shaders are single
    !> precision).
    function json_number(x) result(s)
        real(wp), intent(in) :: x
        character(len=:), allocatable :: s
        character(len=32) :: buf
        real(wp) :: y
        y = x
        if (y /= y) y = 0.0_wp                            ! (NaN)
        y = max(-1.0e30_wp, min(1.0e30_wp, y))
        if (y == aint(y) .and. abs(y) < 1.0e15_wp) then
            write(buf, '(i0)') int(y, int64)
        else
            write(buf, '(es25.16e3)') y
        end if
        s = trim(adjustl(buf))
    end function json_number

    !> A JSON string (quoted, escaped; '<' as <, so no '</script>' in the page).
    function quoted(s) result(q)
        character(len=*), intent(in) :: s
        character(len=:), allocatable :: q
        character(len=6) :: u
        integer :: k, c
        q = '"'
        do k = 1, len(s)
            c = iachar(s(k:k))
            select case (c)
            case (34)
                q = q//'\"'
            case (92)
                q = q//'\\'
            case (10)
                q = q//'\n'
            case (60, 0:9, 11:31)
                write(u, '(a2,z4.4)') '\u', c
                q = q//u
            case default
                q = q//s(k:k)
            end select
        end do
        q = q//'"'
    end function quoted

    !> The JSON written so far.
    function json_text(self) result(s)
        class(WebWriter), intent(in) :: self
        character(len=:), allocatable :: s
        if (allocated(self%json)) then
            s = self%json(1:self%nj)
        else
            s = ''
        end if
    end function json_text

    !> The binary buffer's size so far (bytes).
    pure integer(int64) function num_bytes(self)
        class(WebWriter), intent(in) :: self
        num_bytes = self%nb
    end function num_bytes

    ! ==================================================================
    ! binary arrays

    !> Room for n more bytes, after padding to `align`; the offset where they go.
    integer(int64) function reserve(self, n, align) result(offset)
        class(WebWriter), intent(inout) :: self
        integer(int64), intent(in) :: n
        integer, intent(in) :: align
        integer(int8), allocatable :: tmp(:)
        integer(int64) :: need
        integer(int64) :: start
        if (.not. allocated(self%bin)) then
            allocate(self%bin(1048576))
            self%bin = 0_int8
        end if
        start = ((self%nb + align - 1) / align) * align
        need = start + n
        if (need > size(self%bin, kind=int64)) then
            allocate(tmp(max(2 * size(self%bin, kind=int64), need)))
            tmp(1:self%nb) = self%bin(1:self%nb)
            tmp(self%nb+1:) = 0_int8
            call move_alloc(tmp, self%bin)
        end if
        self%bin(self%nb+1:start) = 0_int8
        offset = start
        self%nb = need
    end function reserve

    !> The reference to an array in the buffer: [byte offset, count].
    subroutine put_ref(self, key, offset, count)
        class(WebWriter), intent(inout) :: self
        character(len=*), intent(in) :: key
        integer(int64), intent(in) :: offset
        integer, intent(in) :: count
        character(len=48) :: s
        call start_value(self, key)
        write(s, '(a,i0,a,i0,a)') '[', offset, ',', count, ']'
        call emit(self, trim(s))
    end subroutine put_ref

    !> Values as float32 in the buffer (the JSON: [byte offset, count]).
    subroutine put_f32(self, key, values)
        class(WebWriter), intent(inout) :: self
        character(len=*), intent(in) :: key
        real(wp), intent(in) :: values(:)
        integer(int64) :: off, nbytes
        nbytes = 4_int64 * size(values)
        off = reserve(self, nbytes, 8)
        if (nbytes > 0) self%bin(off+1:off+nbytes) = transfer(real(values, real32), [0_int8], nbytes)
        call put_ref(self, key, off, size(values))
    end subroutine put_f32

    !> Values as float64 in the buffer.
    subroutine put_f64(self, key, values)
        class(WebWriter), intent(inout) :: self
        character(len=*), intent(in) :: key
        real(wp), intent(in) :: values(:)
        integer(int64) :: off, nbytes
        nbytes = 8_int64 * size(values)
        off = reserve(self, nbytes, 8)
        if (nbytes > 0) self%bin(off+1:off+nbytes) = transfer(real(values, real64), [0_int8], nbytes)
        call put_ref(self, key, off, size(values))
    end subroutine put_f64

    !> Values as uint32 in the buffer.
    subroutine put_u32(self, key, values)
        class(WebWriter), intent(inout) :: self
        character(len=*), intent(in) :: key
        integer, intent(in) :: values(:)
        integer(int64) :: off, nbytes
        nbytes = 4_int64 * size(values)
        off = reserve(self, nbytes, 8)
        if (nbytes > 0) self%bin(off+1:off+nbytes) = transfer(int(values, int32), [0_int8], nbytes)
        call put_ref(self, key, off, size(values))
    end subroutine put_u32

    ! ==================================================================
    ! items: what frames draw

    !> Points xyz(3, i1:i2) relative to their bounding box's center: the
    !> chunk's origin ("o", double precision) and the points ("p", float32).
    subroutine put_chunk_points(self, xyz)
        class(WebWriter), intent(inout) :: self
        real(wp), intent(in) :: xyz(:,:)
        real(wp) :: o(3)
        integer :: k
        o = 0.0_wp
        if (size(xyz, 2) > 0) then
            do k = 1, 3
                o(k) = 0.5_wp * (minval(xyz(k, :)) + maxval(xyz(k, :)))
            end do
        end if
        call self%put_reals('o', o)
        call self%put_f32('p', reshape(xyz - spread(o, 2, size(xyz, 2)), [3 * size(xyz, 2)]))
    end subroutine put_chunk_points

    !> One chunk of points lo .. hi, with their times and colors if given.
    subroutine put_chunk(self, xyz, lo, hi, times, colors)
        class(WebWriter), intent(inout) :: self
        real(wp), intent(in) :: xyz(:,:)
        integer, intent(in) :: lo, hi
        real(wp), intent(in), optional :: times(:), colors(:,:)
        call self%begin_object()
        call put_chunk_points(self, xyz(:, lo:hi))
        if (present(times)) call self%put_f32('t', times(lo:hi) - self%t_ref)
        if (present(colors)) call self%put_f32('c', reshape(colors(:, lo:hi), [4 * (hi - lo + 1)]))
        call self%end_object()
    end subroutine put_chunk

    !> Begin a line item (the caller adds keys, then calls end_object): a
    !> strip through points xyz(3, n), or separate segments (points 1-2, 3-4,
    !> ...), of color rgba and width (window units), with each point's time
    !> (trace mode, trails) and color (4, n) if given. Chunks of a strip
    !> share their end points.
    subroutine begin_line_item(self, xyz, rgba, width, segments, times, colors)
        class(WebWriter), intent(inout) :: self
        real(wp), intent(in) :: xyz(:,:)                    !! (3, n) points
        real(wp), intent(in) :: rgba(4)                     !! [r, g, b, a]
        real(wp), intent(in) :: width                       !! window units
        logical, intent(in) :: segments                     !! separate segments (else a strip)
        real(wp), intent(in), optional :: times(:)          !! (n)
        real(wp), intent(in), optional :: colors(:,:)       !! (4, n)
        integer :: n, lo, hi, step
        n = size(xyz, 2)
        call self%begin_object()
        call self%put('type', 'lines')
        call self%put('segments', segments)
        call self%put('color', rgba)
        call self%put('width', width)
        call self%begin_array('chunks')
        step = WEB_CHUNK
        lo = 1
        do while (lo <= n)
            hi = min(n, lo + step - 1)
            if (.not. segments .and. hi == lo .and. lo > 1) exit
            call put_chunk(self, xyz, lo, hi, times, colors)
            if (hi == n) exit
            if (segments) then
                lo = hi + 1
            else
                lo = hi                                   ! (a strip: the next chunk starts at this one's end)
            end if
        end do
        call self%end_array()
    end subroutine begin_line_item

    !> Begin a point (marker) item: points xyz(3, n), of color rgba, `size`
    !> window units, a MARKER_* `shape`, shrinking beyond `attenuation` from
    !> the camera (0: never), with times and colors if given.
    subroutine begin_point_item(self, xyz, rgba, size, shape, attenuation, times, colors)
        class(WebWriter), intent(inout) :: self
        real(wp), intent(in) :: xyz(:,:)
        real(wp), intent(in) :: rgba(4), size, attenuation
        integer, intent(in) :: shape
        real(wp), intent(in), optional :: times(:), colors(:,:)
        integer :: n, lo, hi
        n = size_of(xyz)
        call self%begin_object()
        call self%put('type', 'points')
        call self%put('color', rgba)
        call self%put('size', size)
        call self%put('shape', shape)
        call self%put('attenuation', attenuation)
        call self%begin_array('chunks')
        lo = 1
        do while (lo <= n)
            hi = min(n, lo + WEB_CHUNK - 1)
            call put_chunk(self, xyz, lo, hi, times, colors)
            lo = hi + 1
        end do
        call self%end_array()
    contains
        pure integer function size_of(a)
            real(wp), intent(in) :: a(:,:)
            size_of = ubound(a, 2) - lbound(a, 2) + 1
        end function size_of
    end subroutine begin_point_item

    !> Begin a mesh item: triangles idx(3, m) (1-based) through points xyz(3,
    !> n), of color rgba; lit (with normals) or unlit (transparent, both
    !> sides, no depth written); per-vertex colors (4, n) and times if given.
    subroutine begin_mesh_item(self, xyz, idx, rgba, normals, colors, times, two_sided)
        class(WebWriter), intent(inout) :: self
        real(wp), intent(in) :: xyz(:,:)
        integer, intent(in) :: idx(:,:)
        real(wp), intent(in) :: rgba(4)
        real(wp), intent(in), optional :: normals(:,:), colors(:,:), times(:)
        logical, intent(in), optional :: two_sided
        logical :: both
        both = .false.
        if (present(two_sided)) both = two_sided
        call self%begin_object()
        call self%put('type', 'mesh')
        call self%put('color', rgba)
        call self%put('lit', present(normals))
        call self%put('two_sided', both .or. .not. present(normals))
        call put_chunk_points(self, xyz)
        call self%put_u32('i', reshape(idx, [size(idx)]) - 1)
        if (present(normals)) call self%put_f32('n', reshape(normals, [size(normals)]))
        if (present(colors)) call self%put_f32('c', reshape(colors, [size(colors)]))
        if (present(times)) call self%put_f32('t', times - self%t_ref)
    end subroutine begin_mesh_item

    ! ==================================================================
    ! assets, warnings

    !> The asset number (0-based, as the page's) of a file to inline (once
    !> for each file); -1 if it doesn't exist.
    integer function add_asset(self, path, mime) result(id)
        class(WebWriter), intent(inout) :: self
        character(len=*), intent(in) :: path                !! the file
        character(len=*), intent(in), optional :: mime      !! its media type (default: from its extension)
        type(WebAsset), allocatable :: tmp(:)
        logical :: exists
        integer :: k
        id = -1
        inquire(file=path, exist=exists)
        if (.not. exists) return
        do k = 1, self%n_assets
            if (self%assets(k)%path == path) then
                id = k - 1
                return
            end if
        end do
        if (.not. allocated(self%assets)) allocate(self%assets(8))
        if (self%n_assets == size(self%assets)) then
            allocate(tmp(2 * self%n_assets))
            tmp(1:self%n_assets) = self%assets(1:self%n_assets)
            call move_alloc(tmp, self%assets)
        end if
        self%n_assets = self%n_assets + 1
        self%assets(self%n_assets)%path = path
        if (present(mime)) then
            self%assets(self%n_assets)%mime = mime
        else
            self%assets(self%n_assets)%mime = mime_of(path)
        end if
        id = self%n_assets - 1
    end function add_asset

    !> A media type from a file's extension.
    function mime_of(path) result(mime)
        character(len=*), intent(in) :: path
        character(len=:), allocatable :: mime, ext
        integer :: k
        k = index(path, '.', back=.true.)
        ext = ''
        if (k > 0) ext = lower(path(k+1:))
        select case (ext)
        case ('jpg', 'jpeg')
            mime = 'image/jpeg'
        case ('png')
            mime = 'image/png'
        case ('webp')
            mime = 'image/webp'
        case ('glb')
            mime = 'model/gltf-binary'
        case ('gltf')
            mime = 'model/gltf+json'
        case default
            mime = 'application/octet-stream'
        end select
    end function mime_of

    pure function lower(s) result(t)
        character(len=*), intent(in) :: s
        character(len=len(s)) :: t
        integer :: k
        t = s
        do k = 1, len(s)
            if (s(k:k) >= 'A' .and. s(k:k) <= 'Z') t(k:k) = achar(iachar(s(k:k)) + 32)
        end do
    end function lower

    !> Print a warning once for each key (e.g. a frame type not exported).
    subroutine warn_once(self, key, message)
        class(WebWriter), intent(inout) :: self
        character(len=*), intent(in) :: key, message
        if (.not. allocated(self%warned)) self%warned = '|'
        if (index(self%warned, '|'//key//'|') > 0) return
        self%warned = self%warned//key//'|'
        write(*, '(a)') 'vkframes: export_web: '//message
    end subroutine warn_once

    ! ==================================================================
    ! the page

    !> Write the page: `file` (HTML), titled `title`, with the JSON written
    !> (one complete object), the buffer and the assets inlined, and the
    !> viewer script `viewer` (a file: data/web/vkf_viewer.js) inlined.
    !> Three.js (`three_version`) is loaded from a CDN. False if a file can't be
    !> read or written.
    logical function write_html(self, file, title, viewer, three_version) result(ok)
        class(WebWriter), intent(inout) :: self
        character(len=*), intent(in) :: file, title, viewer, three_version
        integer(int8), allocatable :: bytes(:)
        character(len=:), allocatable :: cdn
        character(len=24) :: id
        integer :: u, k, status
        ok = .false.
        if (.not. read_bytes(viewer, bytes)) then
            write(*, '(a)') 'vkframes: export_web: no viewer script '//viewer
            return
        end if
        open(newunit=u, file=file, access='stream', form='unformatted', status='replace', action='write', iostat=status)
        if (status /= 0) then
            write(*, '(a)') 'vkframes: export_web: could not write '//file
            return
        end if
        cdn = 'https://cdn.jsdelivr.net/npm/three@'//three_version
        write(u) '<!doctype html>'//new_line('a')//'<html lang="en"><head><meta charset="utf-8">'//new_line('a')
        write(u) '<meta name="viewport" content="width=device-width, initial-scale=1">'//new_line('a')
        write(u) '<title>'//escape_html(title)//'</title>'//new_line('a')
        write(u) '<meta name="generator" content="vkframes">'//new_line('a')
        write(u) '<script type="importmap">{"imports":{"three":"'//cdn//'/build/three.module.js",' // &
                 '"three/addons/":"'//cdn//'/examples/jsm/"}}</script>'//new_line('a')
        write(u) '</head><body>'//new_line('a')
        write(u) '<script type="application/json" id="vkf-scene">'
        if (allocated(self%json)) write(u) self%json(1:self%nj)
        write(u) '</script>'//new_line('a')
        write(u) '<script type="application/octet-stream" id="vkf-data">'
        if (self%nb > 0) call write_base64(u, self%bin(1:self%nb))
        write(u) '</script>'//new_line('a')
        do k = 1, self%n_assets
            if (.not. read_bytes(self%assets(k)%path, bytes)) then
                write(*, '(a)') 'vkframes: export_web: could not read '//self%assets(k)%path
                allocate(bytes(0))
            end if
            write(id, '(i0)') k - 1
            write(u) '<script type="application/octet-stream" id="vkf-asset-'//trim(id)//'" data-mime="' // &
                     self%assets(k)%mime//'">'
            if (size(bytes) > 0) call write_base64(u, bytes)
            write(u) '</script>'//new_line('a')
        end do
        if (.not. read_bytes(viewer, bytes)) return
        write(u) '<script type="module">'//new_line('a')
        write(u) bytes
        write(u) '</script>'//new_line('a')//'</body></html>'//new_line('a')
        close(u)
        ok = .true.
    end function write_html

    pure function escape_html(s) result(t)
        character(len=*), intent(in) :: s
        character(len=:), allocatable :: t
        integer :: k
        t = ''
        do k = 1, len(s)
            select case (s(k:k))
            case ('<')
                t = t//'&lt;'
            case ('>')
                t = t//'&gt;'
            case ('&')
                t = t//'&amp;'
            case default
                t = t//s(k:k)
            end select
        end do
    end function escape_html

    !> A file's bytes; false if it can't be read.
    logical function read_bytes(path, bytes) result(ok)
        character(len=*), intent(in) :: path
        integer(int8), allocatable, intent(out) :: bytes(:)
        integer :: u, status
        integer(int64) :: n
        ok = .false.
        open(newunit=u, file=path, access='stream', form='unformatted', status='old', action='read', iostat=status)
        if (status /= 0) return
        inquire(unit=u, size=n)
        allocate(bytes(max(n, 0_int64)))
        if (n > 0) read(u, iostat=status) bytes
        close(u)
        ok = status == 0
    end function read_bytes

    !> Write bytes to a stream unit as base64.
    subroutine write_base64(u, bytes)
        integer, intent(in) :: u
        integer(int8), intent(in) :: bytes(:)
        integer(int64), parameter :: block = 3 * 65536
        integer(int64) :: i, n
        n = size(bytes, kind=int64)
        i = 1
        do while (i <= n)
            write(u) base64_encode(bytes(i:min(n, i + block - 1)))
            i = i + block
        end do
    end subroutine write_base64

    !> Bytes as base64 (RFC 4648, with padding).
    function base64_encode(bytes) result(s)
        integer(int8), intent(in) :: bytes(:)
        character(len=:), allocatable :: s
        character(len=64), parameter :: abc = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/'
        integer(int64) :: n, i, j
        integer :: b0, b1, b2
        n = size(bytes, kind=int64)
        allocate(character(len=4 * ((n + 2) / 3)) :: s)
        j = 0
        do i = 1, n, 3
            b0 = iand(int(bytes(i)), 255)
            b1 = 0
            b2 = 0
            if (i + 1 <= n) b1 = iand(int(bytes(i + 1)), 255)
            if (i + 2 <= n) b2 = iand(int(bytes(i + 2)), 255)
            s(j+1:j+1) = abc(ishft(b0, -2) + 1:ishft(b0, -2) + 1)
            s(j+2:j+2) = abc(ior(ishft(iand(b0, 3), 4), ishft(b1, -4)) + 1:ior(ishft(iand(b0, 3), 4), ishft(b1, -4)) + 1)
            if (i + 1 <= n) then
                s(j+3:j+3) = abc(ior(ishft(iand(b1, 15), 2), ishft(b2, -6)) + 1:ior(ishft(iand(b1, 15), 2), ishft(b2, -6)) + 1)
            else
                s(j+3:j+3) = '='
            end if
            if (i + 2 <= n) then
                s(j+4:j+4) = abc(iand(b2, 63) + 1:iand(b2, 63) + 1)
            else
                s(j+4:j+4) = '='
            end if
            j = j + 4
        end do
    end function base64_encode

    !> Base64 text as bytes (the inverse of base64_encode; for tests).
    function base64_decode(s) result(bytes)
        character(len=*), intent(in) :: s
        integer(int8), allocatable :: bytes(:)
        character(len=64), parameter :: abc = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/'
        integer :: v(4), i, j, k, n, pad
        n = len(s) / 4
        pad = 0
        if (len(s) >= 1) then
            if (s(len(s):len(s)) == '=') pad = pad + 1
        end if
        if (len(s) >= 2) then
            if (s(len(s)-1:len(s)-1) == '=') pad = pad + 1
        end if
        allocate(bytes(3 * n - pad))
        j = 0
        do i = 1, n
            do k = 1, 4
                v(k) = max(0, index(abc, s(4*(i-1)+k:4*(i-1)+k)) - 1)
            end do
            if (j + 1 <= size(bytes)) bytes(j + 1) = to_byte(iand(ior(ishft(v(1), 2), ishft(v(2), -4)), 255))
            if (j + 2 <= size(bytes)) bytes(j + 2) = to_byte(iand(ior(ishft(v(2), 4), ishft(v(3), -2)), 255))
            if (j + 3 <= size(bytes)) bytes(j + 3) = to_byte(iand(ior(ishft(v(3), 6), v(4)), 255))
            j = j + 3
        end do
    contains
        pure integer(int8) function to_byte(x)
            integer, intent(in) :: x                        !! 0 to 255
            if (x > 127) then
                to_byte = int(x - 256, int8)
            else
                to_byte = int(x, int8)
            end if
        end function to_byte
    end function base64_decode

end module vkf_web_writer
