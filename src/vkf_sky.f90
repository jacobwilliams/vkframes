!> The sky behind a scene (OpenFrames' SkySphere): a texture map and/or stars
!> from a star catalog, at infinity (they turn with the view, never move with
!> it), in the root frame's axes turned by the sky's attitude. A window shows
!> a sky with WindowProxy%set_sky; one sky can be shown in several windows.
!>
!> * Texture: an equirectangular image of the sky (right ascension across,
!>   declination down; as a Sphere's texture map), e.g. OpenFrames'
!>   Images/StarMap.tif.
!> * Stars: a star catalog in OpenFrames' text format (e.g. the HYG v3
!>   catalog of OpenFrames, data/Stars_HYGv3.txt): a header line
!>   "ra[_limit] dec mag ci proper", then a star per line: right ascension (in
!>   hours, or 0 to `limit` if the header says "ra_limit", e.g. "ra_360.0"),
!>   declination (degrees), apparent magnitude, optional B-V color index and
!>   name. Stars in a magnitude range are drawn (at most a number of them, in
!>   catalog order), sized from their brightness between a minimum and a
!>   maximum size (window units), colored from their color index, and the
!>   smaller dimmed (OpenFrames' SkySphere::processStars and StarToPoint).
!>
!> * Star names: the names of the catalog's named stars drawn (those of
!>   them up to a magnitude), as labels beside them, at infinity as the stars
!>   (behind the scene); off by default (set_star_names). Shown with the
!>   stars (draw mode SKY_STARS).
!> * Constellations: faint lines between their stars, from a constellation
!>   file (e.g. data/inp_Constellation.txt, Chris Dolan's outlines): "N name"
!>   starts a constellation, then a line per segment, "dec1 ra1 dec2 ra2"
!>   (degrees, hours), then an optional "# comment"; lines starting with "#"
!>   are comments. Off by default (set_constellations); at infinity, behind
!>   the stars, shown unless the draw mode is SKY_NONE.
!>
!> Both are drawn when given (draw mode SKY_TEXTURE + SKY_STARS); the stars
!> over the texture, by the brighter color.
module vkf_sky

    use, intrinsic :: iso_c_binding, only: c_ptr, c_null_ptr, c_associated, c_int, c_float, c_double, c_f_pointer
    use vkf_math, only: wp, pi, quat_to_matrix
    use vkf_files, only: find_data_file
    use vkf_text, only: label_create
    use vkf_layer, only: check_status, vkf_stars_create, vkf_sky_texture_create, vkf_sky_stars, vkf_sky_stars_changed, &
                         vkf_sky_style, vkf_sky_style_changed, vkf_image_read, vkf_image_delete, &
                         vkf_transform_create, vkf_switch_create, vkf_switch_set, vkf_node_add_child, &
                         vkf_node_remove_child, vkf_node_delete, vkf_compile_node, vkf_transform_set_matrix, &
                         vkf_label_set_at_infinity, vkf_sky_lines_create

    implicit none

    private

    !> What a sky draws (OpenFrames' SkySphere::DrawMode; add them).
    integer, parameter, public :: SKY_NONE = 0, SKY_TEXTURE = 1, SKY_STARS = 2

    integer, parameter :: NAME_ALIGNMENT = 1                !! star names' alignment: vkf_frames' ALIGN_LEFT_CENTER

    !> The constellation file read when none is given: vkframes' (data/).
    character(len=*), parameter, public :: default_constellations = 'inp_Constellation.txt'

    !> A constellation of those read: its name and its lines (columns of
    !> self%lines).
    type :: Constellation
        character(len=:), allocatable :: name               !! the file's name for it
        integer :: first = 1, last = 0                      !! its lines: first .. last
    end type Constellation

    !> A named star of those read: its name and magnitude, its direction in
    !> the stars read (self%stars).
    type :: StarName
        character(len=:), allocatable :: text               !! the catalog's name
        integer :: star = 0                                 !! the star's column in self%stars
        real(wp) :: mag = 0.0_wp                            !! apparent magnitude
    end type StarName

    !> The sky behind a scene (OpenFrames' SkySphere; see the module documentation).
    type, public :: SkySphere
        private
        character(len=:), allocatable :: texture_file       !! texture image file (unallocated or '': none)
        character(len=:), allocatable :: catalog            !! star catalog file (unallocated or '': none)
        real(wp) :: min_mag = -2.0_wp, max_mag = 8.0_wp     !! OpenFrames' SkySphere defaults
        integer :: max_stars = 10000                        !! the most stars drawn
        real(wp) :: min_size = 1.0_wp, max_size = 10.0_wp   !! window units
        real(wp) :: min_dim = 0.5_wp                        !! the smallest stars' color scale (0 to 1)
        integer :: mode = SKY_TEXTURE + SKY_STARS           !! SKY_NONE, or a sum of SKY_TEXTURE and SKY_STARS
        real(wp) :: attitude_(4) = [0.0_wp, 0.0_wp, 0.0_wp, 1.0_wp]  !! in the root frame (quaternion [x, y, z, w])
        real(c_float), allocatable :: stars(:,:)            !! (8, n): direction, size, rgb, -
        integer :: n_stars = 0                              !! stars read: columns of stars used
        real(wp) :: mag_range(2) = 0.0_wp                   !! of the stars read
        type(StarName), allocatable :: names(:)             !! the named stars read (n_names of them)
        integer :: n_names = 0                              !! named stars read
        ! star names
        logical :: names_shown = .false.                    !! the names are drawn
        real(wp) :: names_max_mag = huge(1.0_wp)            !! names of stars up to this magnitude
        real(wp) :: names_size = 12.0_wp                    !! window units
        real(wp) :: names_rgba(4) = [0.6_wp, 0.7_wp, 0.9_wp, 1.0_wp]  !! color
        character(len=:), allocatable :: names_font         !! font ('' or unallocated: the default)
        integer :: n_labels = 0                             !! names drawn
        ! constellations
        character(len=:), allocatable :: constellation_file !! the file read (unallocated: none yet)
        real(c_float), allocatable :: lines(:,:)            !! (8, n): the lines' ends, directions (x, y, z, -)
        integer :: n_lines = 0                              !! lines read: columns of lines used
        type(Constellation), allocatable :: groups(:)       !! the constellations read (n_groups of them)
        integer :: n_groups = 0                             !! constellations read
        logical :: lines_shown = .false.                    !! the constellations are drawn
        real(wp) :: lines_rgba(4) = [0.35_wp, 0.5_wp, 0.8_wp, 0.45_wp]  !! their color (faint)
        real(wp) :: lines_width = 1.0_wp                    !! window units
        ! rendering
        type(c_ptr) :: image = c_null_ptr                   !! the texture image (null: none)
        type(c_ptr) :: root = c_null_ptr                    !! the sky's node, a transform (null until made)
        type(c_ptr) :: texture_switch = c_null_ptr          !! shows or hides the texture (draw mode)
        type(c_ptr) :: star_switch = c_null_ptr             !! shows or hides the stars (draw mode)
        type(c_ptr) :: texture_node = c_null_ptr            !! the texture drawing (null: none)
        type(c_ptr) :: star_node = c_null_ptr               !! the stars drawing (null: none)
        type(c_ptr) :: name_node = c_null_ptr               !! the star names, a transform: the attitude (null: none)
        type(c_ptr) :: line_switch = c_null_ptr             !! shows or hides the constellations (draw mode)
        type(c_ptr) :: line_node = c_null_ptr               !! the constellations drawing (null: none)
        logical :: shown = .false.                          !! made for a window: nodes made later are compiled
    contains
        procedure :: set_texture_map, texture_map
        procedure :: set_star_data, star_catalog, star_magnitudes, star_sizes
        procedure :: num_stars, star_magnitude_range, star
        procedure :: set_star_names, star_names, num_star_names, num_star_names_drawn, star_name
        procedure :: set_constellations, constellations, constellation_file_read, num_constellations
        procedure :: num_constellation_lines, constellation_name, constellation_lines, constellation_line
        procedure :: set_draw_mode, draw_mode
        procedure :: set_attitude, attitude
        procedure :: node
    end type SkySphere

contains

    !> Draw an image file as the sky (found with find_data_file); '' for none.
    !> Returns false if it cannot be read (the sky is then unchanged).
    logical function set_texture_map(self, file) result(ok)
        class(SkySphere), intent(inout) :: self
        character(len=*), intent(in) :: file                !! image file; '' for none
        type(c_ptr) :: image
        character(len=:), allocatable :: path
        ok = .true.
        image = c_null_ptr
        if (len_trim(file) > 0) then
            path = find_data_file(file)
            ok = len(path) > 0
            if (ok) image = vkf_image_read(path, len(path, kind=c_int))
            ok = c_associated(image)
            if (.not. ok) then
                write(*, '(a)') 'vkframes: could not read the sky texture '//trim(file)
                return
            end if
        end if
        if (c_associated(self%image)) call vkf_image_delete(self%image)
        self%image = image
        self%texture_file = trim(file)
        if (c_associated(self%root)) call make_texture_node(self)
    end function set_texture_map

    !> The texture file ('' for none).
    function texture_map(self) result(file)
        class(SkySphere), intent(in) :: self
        character(len=:), allocatable :: file
        file = ''
        if (allocated(self%texture_file)) file = self%texture_file
    end function texture_map

    !> Draw the stars of `catalog` (found with find_data_file; '' for none)
    !> with magnitudes from `min_mag` to `max_mag` (default -2 to 8), at most
    !> `max_stars` (default 10000), sized from `min_size` to `max_size` window
    !> units (default 1 to 10) by brightness, the smallest dimmed to
    !> `min_dim_ratio` (0 to 1, default 0.5) of their color (OpenFrames'
    !> setSkySphereStarData). Returns false if the catalog cannot be read or the
    !> limits are invalid (min_mag > max_mag, max_stars < 1, min_size >
    !> max_size, a ratio outside 0 to 1).
    logical function set_star_data(self, catalog, min_mag, max_mag, max_stars, min_size, max_size, min_dim_ratio) &
            result(ok)
        class(SkySphere), intent(inout) :: self
        character(len=*), intent(in) :: catalog             !! star catalog file; '' for none
        real(wp), intent(in), optional :: min_mag, max_mag  !! apparent magnitudes
        real(wp), intent(in), optional :: min_size, max_size  !! window units
        real(wp), intent(in), optional :: min_dim_ratio     !! 0 to 1
        integer, intent(in), optional :: max_stars
        real(wp) :: mags(2), sizes(2), dim
        integer :: n
        mags = [self%min_mag, self%max_mag]
        if (present(min_mag)) mags(1) = min_mag
        if (present(max_mag)) mags(2) = max_mag
        sizes = [self%min_size, self%max_size]
        if (present(min_size)) sizes(1) = min_size
        if (present(max_size)) sizes(2) = max_size
        dim = self%min_dim
        if (present(min_dim_ratio)) dim = min_dim_ratio
        n = self%max_stars
        if (present(max_stars)) n = max_stars
        ok = mags(1) <= mags(2) .and. n >= 1 .and. sizes(1) <= sizes(2) .and. dim >= 0.0_wp .and. dim <= 1.0_wp
        if (.not. ok) return
        self%min_mag = mags(1)
        self%max_mag = mags(2)
        self%min_size = sizes(1)
        self%max_size = sizes(2)
        self%min_dim = dim
        self%max_stars = n
        self%catalog = trim(catalog)
        self%n_stars = 0
        if (len_trim(catalog) > 0) ok = read_stars(self)
        if (c_associated(self%root)) call make_star_node(self)
    end function set_star_data

    !> The star catalog file ('' for none).
    function star_catalog(self) result(file)
        class(SkySphere), intent(in) :: self
        character(len=:), allocatable :: file
        file = ''
        if (allocated(self%catalog)) file = self%catalog
    end function star_catalog

    !> The magnitude range drawn (as set).
    pure function star_magnitudes(self) result(m)
        class(SkySphere), intent(in) :: self
        real(wp) :: m(2)
        m = [self%min_mag, self%max_mag]
    end function star_magnitudes

    !> The size range of the stars (window units).
    pure function star_sizes(self) result(s)
        class(SkySphere), intent(in) :: self
        real(wp) :: s(2)
        s = [self%min_size, self%max_size]
    end function star_sizes

    !> Number of stars read.
    pure integer function num_stars(self)
        class(SkySphere), intent(in) :: self
        num_stars = self%n_stars
    end function num_stars

    !> The range of magnitudes of the stars read.
    pure function star_magnitude_range(self) result(m)
        class(SkySphere), intent(in) :: self
        real(wp) :: m(2)
        m = self%mag_range
    end function star_magnitude_range

    !> Star i (1 to num_stars): its direction (unit vector, sky frame), color
    !> (rgb, dimmed) and size (window units).
    pure subroutine star(self, i, direction, rgb, size)
        class(SkySphere), intent(in) :: self
        integer, intent(in) :: i                            !! 1 to num_stars
        real(wp), intent(out) :: direction(3)               !! unit vector, sky frame
        real(wp), intent(out) :: rgb(3)                     !! color, dimmed (0 to 1)
        real(wp), intent(out) :: size                       !! window units
        direction = real(self%stars(1:3, i), wp)
        size = real(self%stars(4, i), wp)
        rgb = real(self%stars(5:7, i), wp)
    end subroutine star

    !> Draw the names of the named stars drawn (those of the catalog's last
    !> field), beside them: `shown` (default false), of the stars up to
    !> magnitude `max_mag` (default all of them), `size` window units tall
    !> (default 12), in color `rgba` (default a pale blue) and font `font`
    !> ('' or default: vkframes' default font). Also while shown; the
    !> settings not given are kept. Returns false (nothing changed) if `size`
    !> isn't > 0.
    logical function set_star_names(self, shown, max_mag, size, rgba, font) result(ok)
        class(SkySphere), intent(inout) :: self
        logical, intent(in) :: shown                        !! draw the names
        real(wp), intent(in), optional :: max_mag           !! the dimmest named (apparent magnitude)
        real(wp), intent(in), optional :: size              !! height in window units (> 0)
        real(wp), intent(in), optional :: rgba(4)           !! color [r, g, b, a] (0 to 1)
        character(len=*), intent(in), optional :: font      !! font file ('' for the default)
        ok = .true.
        if (present(size)) ok = size > 0.0_wp
        if (.not. ok) return
        self%names_shown = shown
        if (present(max_mag)) self%names_max_mag = max_mag
        if (present(size)) self%names_size = size
        if (present(rgba)) self%names_rgba = rgba
        if (present(font)) self%names_font = font
        if (c_associated(self%root)) call make_name_node(self)
    end function set_star_names

    !> Whether the star names are drawn (as set).
    pure logical function star_names(self)
        class(SkySphere), intent(in) :: self
        star_names = self%names_shown
    end function star_names

    !> Number of named stars read (of the stars read).
    pure integer function num_star_names(self)
        class(SkySphere), intent(in) :: self
        num_star_names = self%n_names
    end function num_star_names

    !> Number of star names drawn now (0 until the sky is shown, or if the
    !> names aren't).
    pure integer function num_star_names_drawn(self)
        class(SkySphere), intent(in) :: self
        num_star_names_drawn = self%n_labels
    end function num_star_names_drawn

    !> Named star i (1 to num_star_names): its name, apparent magnitude and
    !> star (1 to num_stars: see `star`).
    subroutine star_name(self, i, name, mag, which)
        class(SkySphere), intent(in) :: self
        integer, intent(in) :: i                            !! 1 to num_star_names
        character(len=:), allocatable, intent(out) :: name  !! the catalog's name
        real(wp), intent(out) :: mag                        !! apparent magnitude
        integer, intent(out) :: which                       !! the star (1 to num_stars)
        name = self%names(i)%text
        mag = self%names(i)%mag
        which = self%names(i)%star
    end subroutine star_name

    !> Draw the constellations: faint lines between their stars, from
    !> constellation file `file` (found with find_data_file; read when given,
    !> or when first shown without one: default_constellations), `shown`
    !> (default false), in color `rgba` (default a faint blue, alpha 0.45) and
    !> `width` window units wide (default 1). Also while shown; the settings
    !> not given are kept. Returns false if the file can't be read (the
    !> constellations read before are kept) or `width` isn't > 0 (nothing
    !> changed).
    logical function set_constellations(self, shown, file, rgba, width) result(ok)
        class(SkySphere), intent(inout) :: self
        logical, intent(in) :: shown                        !! draw the constellations
        character(len=*), intent(in), optional :: file      !! constellation file
        real(wp), intent(in), optional :: rgba(4)           !! color [r, g, b, a] (0 to 1)
        real(wp), intent(in), optional :: width             !! window units (> 0)
        ok = .true.
        if (present(width)) ok = width > 0.0_wp
        if (.not. ok) return
        self%lines_shown = shown
        if (present(rgba)) self%lines_rgba = rgba
        if (present(width)) self%lines_width = width
        if (present(file)) then
            ok = read_constellations(self, file)
        else if (shown .and. .not. allocated(self%constellation_file)) then
            ok = read_constellations(self, default_constellations)
        end if
        if (c_associated(self%root)) call make_line_node(self)
    end function set_constellations

    !> Whether the constellations are drawn (as set).
    pure logical function constellations(self)
        class(SkySphere), intent(in) :: self
        constellations = self%lines_shown
    end function constellations

    !> The constellation file read ('' for none).
    function constellation_file_read(self) result(file)
        class(SkySphere), intent(in) :: self
        character(len=:), allocatable :: file
        file = ''
        if (allocated(self%constellation_file)) file = self%constellation_file
    end function constellation_file_read

    !> Number of constellations read.
    pure integer function num_constellations(self)
        class(SkySphere), intent(in) :: self
        num_constellations = self%n_groups
    end function num_constellations

    !> Number of constellation lines read (of all of them).
    pure integer function num_constellation_lines(self)
        class(SkySphere), intent(in) :: self
        num_constellation_lines = self%n_lines
    end function num_constellation_lines

    !> Constellation i's name (1 to num_constellations).
    function constellation_name(self, i) result(name)
        class(SkySphere), intent(in) :: self
        integer, intent(in) :: i                            !! 1 to num_constellations
        character(len=:), allocatable :: name
        name = self%groups(i)%name
    end function constellation_name

    !> Constellation i's lines (1 to num_constellations): first .. last (see
    !> constellation_line; none if last < first).
    pure subroutine constellation_lines(self, i, first, last)
        class(SkySphere), intent(in) :: self
        integer, intent(in) :: i                            !! 1 to num_constellations
        integer, intent(out) :: first, last                 !! its lines
        first = self%groups(i)%first
        last = self%groups(i)%last
    end subroutine constellation_lines

    !> Constellation line i (1 to num_constellation_lines): its ends'
    !> directions (unit vectors, sky frame).
    pure subroutine constellation_line(self, i, a, b)
        class(SkySphere), intent(in) :: self
        integer, intent(in) :: i                            !! 1 to num_constellation_lines
        real(wp), intent(out) :: a(3), b(3)                 !! unit vectors, sky frame
        a = real(self%lines(1:3, i), wp)
        b = real(self%lines(5:7, i), wp)
    end subroutine constellation_line

    !> What to draw: SKY_NONE, or SKY_TEXTURE + SKY_STARS (default; each only
    !> if given).
    subroutine set_draw_mode(self, mode)
        class(SkySphere), intent(inout) :: self
        integer, intent(in) :: mode                         !! SKY_NONE, or a sum of SKY_TEXTURE and SKY_STARS
        self%mode = mode
        call apply_mode(self)
    end subroutine set_draw_mode

    !> What is drawn: SKY_NONE, or a sum of SKY_TEXTURE and SKY_STARS.
    pure integer function draw_mode(self)
        class(SkySphere), intent(in) :: self
        draw_mode = self%mode
    end function draw_mode

    !> The sky's attitude in the root frame (quaternion [x, y, z, w]): e.g. to
    !> turn an equatorial catalog into an ecliptic root frame.
    subroutine set_attitude(self, q)
        class(SkySphere), intent(inout) :: self
        real(wp), intent(in) :: q(4)                        !! quaternion [x, y, z, w] (any length > 0)
        if (norm2(q) <= 0.0_wp) return
        self%attitude_ = q / norm2(q)
        call apply_attitude(self, self%texture_node)
        call apply_attitude(self, self%star_node)
        call apply_attitude(self, self%line_node)
        call apply_name_attitude(self)
    end subroutine set_attitude

    !> The sky's attitude in the root frame (quaternion [x, y, z, w]).
    pure function attitude(self) result(q)
        class(SkySphere), intent(in) :: self
        real(wp) :: q(4)
        q = self%attitude_
    end function attitude

    !> The sky's node, made on first use, for a window (WindowProxy%set_sky).
    !> For a window's `viewer` (null: headless), nodes made later (a new texture
    !> or catalog) are compiled for the windows running.
    function node(self, viewer) result(n)
        class(SkySphere), intent(inout) :: self
        type(c_ptr), intent(in) :: viewer                   !! a window's viewer (null: headless)
        type(c_ptr) :: n
        if (.not. c_associated(self%root)) then
            self%root = vkf_transform_create()
            self%texture_switch = vkf_switch_create()
            self%star_switch = vkf_switch_create()
            self%line_switch = vkf_switch_create()
            call check_status(vkf_node_add_child(self%root, self%texture_switch), 'SkySphere')   ! the texture first,
            call check_status(vkf_node_add_child(self%root, self%line_switch), 'SkySphere')      ! the constellations,
            call check_status(vkf_node_add_child(self%root, self%star_switch), 'SkySphere')      ! the stars over them
            call make_texture_node(self)
            call make_line_node(self)
            call make_star_node(self)
            self%shown = c_associated(viewer)          ! (the first nodes are compiled with the scene)
        end if
        n = self%root
    end function node

    ! ==================================================================
    ! rendering

    !> The texture node for the current image, in place of the old one.
    subroutine make_texture_node(self)
        type(SkySphere), intent(inout) :: self
        if (c_associated(self%texture_node)) then
            call check_status(vkf_node_remove_child(self%texture_switch, self%texture_node), 'SkySphere')
            call vkf_node_delete(self%texture_node)
            self%texture_node = c_null_ptr
        end if
        if (c_associated(self%image)) then
            self%texture_node = vkf_sky_texture_create(self%image)
            if (c_associated(self%texture_node)) then
                call apply_attitude(self, self%texture_node)
                if (self%shown) call check_status(vkf_compile_node(self%texture_node), 'SkySphere')
                call check_status(vkf_node_add_child(self%texture_switch, self%texture_node), 'SkySphere')
            end if
        end if
        call apply_mode(self)
    end subroutine make_texture_node

    !> The star node for the current stars, in place of the old one.
    subroutine make_star_node(self)
        type(SkySphere), intent(inout) :: self
        real(c_float), dimension(:,:), pointer :: p
        integer(c_int) :: capacity
        type(c_ptr) :: data
        if (c_associated(self%star_node)) then
            call check_status(vkf_node_remove_child(self%star_switch, self%star_node), 'SkySphere')
            call vkf_node_delete(self%star_node)
            self%star_node = c_null_ptr
        end if
        if (self%n_stars > 0) then
            self%star_node = vkf_stars_create(int(self%n_stars, c_int))
            if (c_associated(self%star_node)) then
                data = vkf_sky_stars(self%star_node, capacity)
                if (c_associated(data)) then
                    call c_f_pointer(data, p, [8, int(capacity)])
                    p(:, 1:self%n_stars) = self%stars(:, 1:self%n_stars)
                    call check_status(vkf_sky_stars_changed(self%star_node, int(self%n_stars, c_int)), 'SkySphere')
                end if
                call apply_attitude(self, self%star_node)
                if (self%shown) call check_status(vkf_compile_node(self%star_node), 'SkySphere')
                call check_status(vkf_node_add_child(self%star_switch, self%star_node), 'SkySphere')
            end if
        end if
        call make_name_node(self)                     ! (the names after the stars: in front of them)
        call apply_mode(self)
    end subroutine make_star_node

    !> The constellations node for the lines read and the settings, in place of
    !> the old one (none unless shown).
    subroutine make_line_node(self)
        type(SkySphere), intent(inout) :: self
        real(c_float), dimension(:,:), pointer :: p
        real(c_float), dimension(:), pointer :: style
        integer(c_int) :: capacity
        type(c_ptr) :: data
        if (c_associated(self%line_node)) then
            call check_status(vkf_node_remove_child(self%line_switch, self%line_node), 'SkySphere')
            call vkf_node_delete(self%line_node)
            self%line_node = c_null_ptr
        end if
        if (self%lines_shown .and. self%n_lines > 0) then
            self%line_node = vkf_sky_lines_create(int(self%n_lines, c_int))
            if (c_associated(self%line_node)) then
                data = vkf_sky_stars(self%line_node, capacity)
                if (c_associated(data)) then
                    call c_f_pointer(data, p, [8, int(capacity)])
                    p(:, 1:self%n_lines) = self%lines(:, 1:self%n_lines)
                    call check_status(vkf_sky_stars_changed(self%line_node, int(self%n_lines, c_int)), 'SkySphere')
                end if
                data = vkf_sky_style(self%line_node)
                if (c_associated(data)) then                ! (the attitude: apply_attitude)
                    call c_f_pointer(data, style, [24])
                    style(17:24) = real([self%lines_width, 0.0_wp, 0.0_wp, 0.0_wp, self%lines_rgba], c_float)
                end if
                call apply_attitude(self, self%line_node)
                if (self%shown) call check_status(vkf_compile_node(self%line_node), 'SkySphere')
                call check_status(vkf_node_add_child(self%line_switch, self%line_node), 'SkySphere')
            end if
        end if
        call apply_mode(self)
    end subroutine make_line_node

    !> The star names node for the current names and settings, in place of the
    !> old one: a label per named star drawn up to the names' magnitude, under
    !> the stars' switch (shown with them), turned by the sky's attitude.
    subroutine make_name_node(self)
        type(SkySphere), intent(inout) :: self
        type(c_ptr) :: label
        integer :: i
        if (c_associated(self%name_node)) then
            call check_status(vkf_node_remove_child(self%star_switch, self%name_node), 'SkySphere')
            call vkf_node_delete(self%name_node)
            self%name_node = c_null_ptr
        end if
        self%n_labels = 0
        if (.not. self%names_shown .or. self%n_names == 0) return
        if (.not. allocated(self%names_font)) self%names_font = ''
        self%name_node = vkf_transform_create()
        if (.not. c_associated(self%name_node)) return
        do i = 1, self%n_names
            if (self%names(i)%mag > self%names_max_mag) cycle
            ! (two spaces: beside the star, not over it)
            label = label_create('  '//self%names(i)%text, real(self%stars(1:3, self%names(i)%star), wp), &
                                 self%names_rgba, self%names_size, 0.0_wp, NAME_ALIGNMENT, self%names_font)
            if (.not. c_associated(label)) exit                ! (no font: said)
            call check_status(vkf_label_set_at_infinity(label, 1_c_int), 'SkySphere')
            call check_status(vkf_node_add_child(self%name_node, label), 'SkySphere')
            call vkf_node_delete(label)                        ! (held by the node)
            self%n_labels = self%n_labels + 1
        end do
        call apply_name_attitude(self)
        if (self%shown) call check_status(vkf_compile_node(self%name_node), 'SkySphere')
        call check_status(vkf_node_add_child(self%star_switch, self%name_node), 'SkySphere')
        call apply_mode(self)
    end subroutine make_name_node

    !> The sky's attitude into the star names' transform.
    subroutine apply_name_attitude(self)
        type(SkySphere), intent(in) :: self
        real(c_double) :: m(4, 4)
        if (.not. c_associated(self%name_node)) return
        m = 0.0_c_double
        m(1:3, 1:3) = quat_to_matrix(self%attitude_)
        m(4, 4) = 1.0_c_double
        call check_status(vkf_transform_set_matrix(self%name_node, m), 'SkySphere')
    end subroutine apply_name_attitude

    !> Show or hide the texture, the constellations and the stars (and their
    !> names), as the draw mode says.
    subroutine apply_mode(self)
        type(SkySphere), intent(in) :: self
        if (c_associated(self%texture_switch)) &
            call check_status(vkf_switch_set(self%texture_switch, merge(1_c_int, 0_c_int, iand(self%mode, SKY_TEXTURE) /= 0)), &
                              'SkySphere')
        if (c_associated(self%star_switch)) &
            call check_status(vkf_switch_set(self%star_switch, merge(1_c_int, 0_c_int, iand(self%mode, SKY_STARS) /= 0)), &
                              'SkySphere')
        if (c_associated(self%line_switch)) &
            call check_status(vkf_switch_set(self%line_switch, merge(1_c_int, 0_c_int, self%mode /= SKY_NONE)), 'SkySphere')
    end subroutine apply_mode

    !> The attitude matrix (column-major 4x4) into a sky node's style.
    subroutine apply_attitude(self, n)
        type(SkySphere), intent(in) :: self
        type(c_ptr), intent(in) :: n                        !! a sky node, texture or stars (null: nothing done)
        real(c_float), dimension(:,:), pointer :: m
        type(c_ptr) :: style
        if (.not. c_associated(n)) return
        style = vkf_sky_style(n)
        if (.not. c_associated(style)) return
        call c_f_pointer(style, m, [4, 4])
        m = 0.0_c_float
        m(1:3, 1:3) = real(quat_to_matrix(self%attitude_), c_float)
        m(4, 4) = 1.0_c_float
        call check_status(vkf_sky_style_changed(n), 'SkySphere')
    end subroutine apply_attitude

    ! ==================================================================
    ! the star catalog (OpenFrames' SkySphere::processStars)

    !> Read the stars of the catalog into self%stars.
    logical function read_stars(self) result(ok)
        type(SkySphere), intent(inout) :: self
        character(len=:), allocatable :: path
        character(len=1024) :: line
        character(len=64) :: label
        real(wp) :: ra_limit, ra, dec, mag, ci, raw_min, raw_max, ratio, size, dim, dir(3), rgb(3)
        real(c_float), allocatable :: tmp(:,:)
        integer :: unit, status, k

        ok = .false.
        path = find_data_file(self%catalog)
        if (len(path) == 0) then
            write(*, '(a)') 'vkframes: star catalog '//self%catalog//' not found (set VKFRAMES_DATA_PATH)'
            return
        end if
        open(newunit=unit, file=path, status='old', action='read', iostat=status)
        if (status /= 0) then
            write(*, '(a)') 'vkframes: could not open the star catalog '//path
            return
        end if

        ! right ascension in hours, unless the header says "ra_<limit>"
        ra_limit = 24.0_wp
        read(unit, '(a)', iostat=status) line
        if (status == 0) then
            read(line, *, iostat=status) label
            k = index(label, '_')
            if (status == 0 .and. k > 0) then
                read(label(k+1:), *, iostat=status) ra_limit
                if (status /= 0 .or. ra_limit <= 0.0_wp) ra_limit = 24.0_wp
            end if
        end if

        ! sizes from brightness, scaled from [raw_min, raw_max] to [min_size, max_size]
        raw_max = raw_size(self%min_mag)                ! the brightest: the largest
        raw_min = raw_size(self%max_mag)
        if (.not. allocated(self%stars)) allocate(self%stars(8, min(self%max_stars, 1024)))
        self%n_stars = 0
        self%n_names = 0
        self%mag_range = [huge(1.0_wp), -huge(1.0_wp)]
        do while (self%n_stars < self%max_stars)
            read(unit, '(a)', iostat=status) line
            if (status /= 0) exit
            read(line, *, iostat=status) ra, dec, mag, ci
            if (status /= 0) then
                read(line, *, iostat=status) ra, dec, mag
                if (status /= 0) cycle
                ci = 0.0_wp
            end if
            if (mag < self%min_mag .or. mag > self%max_mag) cycle

            call star_to_point(ra / ra_limit * 2.0_wp * pi, dec * pi / 180.0_wp, mag, ci, dir, rgb, size)
            ratio = 0.0_wp
            if (raw_max /= raw_min) then
                ratio = (size - raw_min) / (raw_max - raw_min)
                size = self%min_size + ratio * (self%max_size - self%min_size)
            else
                size = self%min_size
            end if
            dim = max(self%min_dim, ratio)            ! big stars at full color, small ones dimmed

            if (self%n_stars == size_of(self%stars)) then
                allocate(tmp(8, min(self%max_stars, 2 * self%n_stars)))
                tmp(:, 1:self%n_stars) = self%stars(:, 1:self%n_stars)
                call move_alloc(tmp, self%stars)
            end if
            self%n_stars = self%n_stars + 1
            self%stars(:, self%n_stars) = real([dir, size, dim * rgb, 1.0_wp], c_float)
            self%mag_range = [min(self%mag_range(1), mag), max(self%mag_range(2), mag)]
            call add_name(self, after_fields(line, 4), mag)
        end do
        close(unit)
        if (self%n_stars == 0) self%mag_range = 0.0_wp
        ok = .true.
    end function read_stars

    !> Read the constellations of `file` (found with find_data_file; '' for
    !> none) into self%lines and self%groups, in place of those read before.
    !> False (those kept) if it can't be found or read.
    logical function read_constellations(self, file) result(ok)
        type(SkySphere), intent(inout) :: self
        character(len=*), intent(in) :: file                !! constellation file ('' for none)
        character(len=:), allocatable :: path
        character(len=1024) :: line
        real(c_float), allocatable :: lines(:,:), tmp(:,:)
        type(Constellation), allocatable :: groups(:), gtmp(:)
        real(wp) :: v(4)
        integer :: unit, status, n, ng, k

        ok = .true.
        if (len_trim(file) == 0) then
            self%constellation_file = ''
            self%n_lines = 0
            self%n_groups = 0
            return
        end if
        ok = .false.
        path = find_data_file(file)
        if (len(path) == 0) then
            write(*, '(a)') 'vkframes: constellation file '//trim(file)//' not found (set VKFRAMES_DATA_PATH)'
            return
        end if
        open(newunit=unit, file=path, status='old', action='read', iostat=status)
        if (status /= 0) then
            write(*, '(a)') 'vkframes: could not open the constellation file '//path
            return
        end if
        allocate(lines(8, 1024), groups(128))
        n = 0
        ng = 0
        do
            read(unit, '(a)', iostat=status) line
            if (status /= 0) exit
            k = scan(line, achar(13))                       ! (CRLF line ends)
            if (k > 0) line(k:) = ''
            line = adjustl(line)
            if (line(1:2) == 'N ') then                     ! a constellation
                if (ng == size(groups)) then
                    allocate(gtmp(2 * ng))
                    gtmp(1:ng) = groups(1:ng)
                    call move_alloc(gtmp, groups)
                end if
                ng = ng + 1
                groups(ng) = Constellation(trim(adjustl(line(3:))), n + 1, n)
                cycle
            end if
            if (len_trim(line) == 0 .or. line(1:1) == '#') cycle
            k = index(line, '#')                            ! (a comment after the line)
            if (k > 0) line(k:) = ''
            read(line, *, iostat=status) v                  ! dec1, ra1, dec2, ra2 (degrees, hours)
            if (status /= 0) cycle
            if (n == size(lines, 2)) then
                allocate(tmp(8, 2 * n))
                tmp(:, 1:n) = lines(:, 1:n)
                call move_alloc(tmp, lines)
            end if
            n = n + 1
            lines(:, n) = real([sky_direction(v(2), v(1)), 0.0_wp, sky_direction(v(4), v(3)), 0.0_wp], c_float)
            if (ng > 0) groups(ng)%last = n
        end do
        close(unit)
        call move_alloc(lines, self%lines)
        call move_alloc(groups, self%groups)
        self%n_lines = n
        self%n_groups = ng
        self%constellation_file = trim(file)
        ok = .true.
    end function read_constellations

    !> The unit vector of a direction on the sky from its right ascension
    !> (hours) and declination (degrees), as the stars' (star_to_point).
    pure function sky_direction(ra, dec) result(dir)
        real(wp), intent(in) :: ra                          !! right ascension (hours)
        real(wp), intent(in) :: dec                         !! declination (degrees)
        real(wp) :: dir(3)
        real(wp) :: a, d
        a = ra / 24.0_wp * 2.0_wp * pi
        d = dec * pi / 180.0_wp
        dir = [cos(a) * cos(d), sin(a) * cos(d), sin(d)]
    end function sky_direction

    !> Remember the name of the star just read (self%n_stars), if it has one.
    subroutine add_name(self, name, mag)
        type(SkySphere), intent(inout) :: self
        character(len=*), intent(in) :: name                !! its name ('' for none)
        real(wp), intent(in) :: mag                         !! its apparent magnitude
        type(StarName), allocatable :: tmp(:)
        if (len(name) == 0) return
        if (.not. allocated(self%names)) allocate(self%names(64))
        if (self%n_names == size(self%names)) then
            allocate(tmp(2 * self%n_names))
            tmp(1:self%n_names) = self%names(1:self%n_names)
            call move_alloc(tmp, self%names)
        end if
        self%n_names = self%n_names + 1
        self%names(self%n_names) = StarName(name, self%n_stars, mag)
    end subroutine add_name

    !> The text of `line` after its first n fields (separated by blanks or
    !> tabs), trimmed: a star's name ('' if none: e.g. without a color index).
    pure function after_fields(line, n) result(rest)
        character(len=*), intent(in) :: line
        integer, intent(in) :: n                            !! fields skipped
        character(len=:), allocatable :: rest
        character(len=*), parameter :: blanks = ' '//achar(9)//achar(13)
        integer :: i, j, k
        rest = ''
        i = 1
        do k = 1, n
            j = verify(line(i:), blanks)                    ! the field's start
            if (j == 0) return
            i = i + j - 1
            j = scan(line(i:), blanks)                      ! the blank after it
            if (j == 0) return
            i = i + j - 1
        end do
        j = verify(line(i:), blanks)                        ! the rest, without blanks around it
        if (j == 0) return
        i = i + j - 1
        k = verify(line, blanks, back=.true.)
        rest = line(i:k)
    end function after_fields

    !> The number of columns of a (the stars' capacity).
    pure integer function size_of(a)
        real(c_float), intent(in) :: a(:,:)
        size_of = size(a, 2)
    end function size_of

    !> A star's pixel diameter from its apparent magnitude, from its brightness
    !> and luminance (OpenFrames' getStarPixelSizeFromMagnitude).
    pure real(wp) function raw_size(mag)
        real(wp), intent(in) :: mag                         !! apparent magnitude
        real(wp) :: la, l
        la = 100.0_wp**(-2.0_wp / 5.0_wp)                ! transition luminance
        l = 100.0_wp**(-mag / 5.0_wp)                    ! relative luminance
        raw_size = 2.0_wp * sqrt(l / (l**(2.0_wp / 3.0_wp) + la))
    end function raw_size

    !> A star's direction, color (from its B-V color index: Ballesteros'
    !> temperature, then Tanner Helland's temperature to RGB) and raw size
    !> (OpenFrames' SkySphere::StarToPoint).
    pure subroutine star_to_point(ra, dec, mag, ci, dir, rgb, size)
        real(wp), intent(in) :: ra, dec                     !! right ascension and declination (radians)
        real(wp), intent(in) :: mag, ci                     !! apparent magnitude and B-V color index
        real(wp), intent(out) :: dir(3)                     !! unit vector
        real(wp), intent(out) :: rgb(3)                     !! color (0 to 1)
        real(wp), intent(out) :: size                       !! raw size (as raw_size)
        real(wp) :: t
        dir = [cos(ra) * cos(dec), sin(ra) * cos(dec), sin(dec)]
        size = raw_size(mag)
        t = 0.01_wp * 4600.0_wp * (1.0_wp / (0.92_wp * ci + 1.7_wp) + 1.0_wp / (0.92_wp * ci + 0.62_wp))
        if (t <= 66.0_wp) then
            rgb(1) = 255.0_wp
            rgb(2) = 99.4708025861_wp * log(t) - 161.1195681661_wp
        else
            rgb(1) = 329.698727446_wp * (t - 60.0_wp)**(-0.1332047592_wp)
            rgb(2) = 288.1221695283_wp * (t - 60.0_wp)**(-0.0755148492_wp)
        end if
        if (t >= 66.0_wp) then
            rgb(3) = 255.0_wp
        else if (t <= 19.0_wp) then
            rgb(3) = 0.0_wp
        else
            rgb(3) = 138.5177312231_wp * log(t - 10.0_wp) - 305.0447927307_wp
        end if
        rgb = max(0.0_wp, min(255.0_wp, rgb)) / 255.0_wp
    end subroutine star_to_point

end module vkf_sky
