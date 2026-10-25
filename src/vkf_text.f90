!> Text for vkframes: fonts (found by name on a search path, read once) and
!> labels (text that always faces the screen).
!>
!> A font is given as a file name (e.g. 'DejaVuSans.ttf', the default) or a
!> path. A name is looked for in, in order: the directories of the
!> VKFRAMES_FONT_PATH environment variable (separated by ':' or ';'), the
!> `fonts` directory of the active conda/pixi environment (where pixi installs
!> DejaVu), and the usual system font directories of macOS, Linux and Windows.
module vkf_text

    use, intrinsic :: iso_c_binding, only: c_ptr, c_null_ptr, c_associated, c_int, c_float, c_double
    use vkf_math, only: wp
    use vkf_layer, only: vkf_font_read, vkf_label_create

    implicit none

    private

    character(len=*), parameter, public :: default_font = 'DejaVuSans.ttf'

    ! vsg::StandardLayout's alignments
    integer, parameter :: VSG_BASELINE = 0, VSG_LEFT = 1, VSG_TOP = 1, VSG_CENTER = 2, VSG_RIGHT = 3, VSG_BOTTOM = 3

    type :: font_entry
        character(len=:), allocatable :: name
        type(c_ptr) :: font = c_null_ptr
    end type font_entry

    !> Fonts read so far (each is read once; kept for the whole run).
    type(font_entry), allocatable, save :: fonts(:)

    public :: find_font, get_font, label_create

contains

    !> The path of font `name` (a file name or a path), or '' if not found.
    function find_font(name) result(path)
        character(len=*), intent(in) :: name
        character(len=:), allocatable :: path
        character(len=4096) :: env
        character(len=:), allocatable :: dirs
        character(len=*), parameter :: system_dirs = &
            '/System/Library/Fonts/Supplemental;/System/Library/Fonts;/Library/Fonts;' // &   ! macOS
            '/usr/share/fonts/truetype/dejavu;/usr/share/fonts/TTF;/usr/share/fonts/dejavu;' // &  ! Linux
            '/usr/share/fonts/truetype;/usr/share/fonts;/usr/local/share/fonts;' // &
            'C:/Windows/Fonts'                                                                   ! Windows
        integer :: status, i, j
        logical :: exists

        path = ''
        if (len_trim(name) == 0) return
        if (index(name, '/') > 0 .or. index(name, '\') > 0) then
            inquire(file=name, exist=exists)
            if (exists) path = name
            return
        end if
        dirs = ''
        call get_environment_variable('VKFRAMES_FONT_PATH', env, status=status)
        if (status == 0) dirs = trim(env)//';'
        call get_environment_variable('CONDA_PREFIX', env, status=status)
        if (status == 0) dirs = dirs//trim(env)//'/fonts;'//trim(env)//'/Library/fonts;'
        dirs = dirs//system_dirs
        i = 1
        do while (i <= len(dirs))
            j = i
            do while (j <= len(dirs))
                if (dirs(j:j) == ';' .or. (dirs(j:j) == ':' .and. j > i + 1)) exit   ! ':' not after a drive letter
                j = j + 1
            end do
            if (j > i) then
                inquire(file=dirs(i:j-1)//'/'//name, exist=exists)
                if (exists) then
                    path = dirs(i:j-1)//'/'//name
                    return
                end if
            end if
            i = j + 1
        end do
    end function find_font

    !> The font handle for `name` (found with `find_font`, read on first use);
    !> the default font if `name` is ''; null if it cannot be found or read.
    function get_font(name) result(font)
        character(len=*), intent(in) :: name
        type(c_ptr) :: font
        type(font_entry), allocatable :: tmp(:)
        character(len=:), allocatable :: key, path
        integer :: k
        key = trim(name)
        if (len(key) == 0) key = default_font
        font = c_null_ptr
        if (allocated(fonts)) then
            do k = 1, size(fonts)
                if (fonts(k)%name == key) then
                    font = fonts(k)%font
                    return
                end if
            end do
        end if
        path = find_font(key)
        if (len(path) == 0) then
            write(*, '(a)') 'vkframes: font '//key//' not found (set VKFRAMES_FONT_PATH)'
        else
            font = vkf_font_read(path, len(path, kind=c_int))
        end if
        ! remember the result (also a failure, so it is not looked for again)
        if (.not. allocated(fonts)) allocate(fonts(0))
        allocate(tmp(size(fonts) + 1))
        tmp(1:size(fonts)) = fonts
        tmp(size(tmp))%name = key
        tmp(size(tmp))%font = font
        call move_alloc(tmp, fonts)
    end function get_font

    !> A label node: `text` at `position` in color `rgba`, in font `font_name`
    !> ('' for the default), aligned by `alignment` (an ALIGN_* constant of
    !> vkf_frames). `size` > 0: `size` window units tall; `size` < 0: -size
    !> scene units tall, but at most `max_size` window units on screen. Null if
    !> the font is missing.
    function label_create(text, position, rgba, size, max_size, alignment, font_name) result(node)
        character(len=*), intent(in) :: text
        real(wp), intent(in) :: position(3), rgba(4), size, max_size
        integer, intent(in) :: alignment
        character(len=*), intent(in) :: font_name
        type(c_ptr) :: node
        type(c_ptr) :: font
        integer :: h, v
        node = c_null_ptr
        font = get_font(font_name)
        if (.not. c_associated(font)) return
        call vsg_alignment(alignment, h, v)
        node = vkf_label_create(font, text, len(text, kind=c_int), real(rgba, c_float), real(position, c_double), &
                                real(size, c_float), real(max_size, c_float), int(h, c_int), int(v, c_int))
    end function label_create

    !> vsg::StandardLayout's horizontal and vertical alignments for an ALIGN_*
    !> constant (vkf_frames: 0 left-top ... 8 right-bottom, 9 to 11 base line).
    pure subroutine vsg_alignment(alignment, h, v)
        integer, intent(in) :: alignment
        integer, intent(out) :: h, v
        integer, parameter :: horizontal(0:11) = [VSG_LEFT, VSG_LEFT, VSG_LEFT, VSG_CENTER, VSG_CENTER, VSG_CENTER, &
                                                  VSG_RIGHT, VSG_RIGHT, VSG_RIGHT, VSG_LEFT, VSG_CENTER, VSG_RIGHT]
        integer, parameter :: vertical(0:11) = [VSG_TOP, VSG_CENTER, VSG_BOTTOM, VSG_TOP, VSG_CENTER, VSG_BOTTOM, &
                                                VSG_TOP, VSG_CENTER, VSG_BOTTOM, VSG_BASELINE, VSG_BASELINE, VSG_BASELINE]
        integer :: a
        a = max(0, min(11, alignment))
        h = horizontal(a)
        v = vertical(a)
    end subroutine vsg_alignment

end module vkf_text
