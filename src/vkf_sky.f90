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
!> Both are drawn when given (draw mode SKY_TEXTURE + SKY_STARS); the stars
!> over the texture, by the brighter color.
module vkf_sky

    use, intrinsic :: iso_c_binding, only: c_ptr, c_null_ptr, c_associated, c_int, c_float, c_f_pointer
    use vkf_math, only: wp, pi, quat_to_matrix
    use vkf_files, only: find_data_file
    use vkf_layer, only: vkf_stars_create, vkf_sky_texture_create, vkf_sky_stars, vkf_sky_stars_changed, &
                         vkf_sky_style, vkf_sky_style_changed, vkf_image_read, vkf_image_delete, &
                         vkf_transform_create, vkf_switch_create, vkf_switch_set, vkf_node_add_child, &
                         vkf_node_remove_child, vkf_node_delete, vkf_compile_node

    implicit none

    private

    !> What a sky draws (OpenFrames' SkySphere::DrawMode; add them).
    integer, parameter, public :: SKY_NONE = 0, SKY_TEXTURE = 1, SKY_STARS = 2

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
        ! rendering
        type(c_ptr) :: image = c_null_ptr                   !! the texture image (null: none)
        type(c_ptr) :: root = c_null_ptr                    !! the sky's node, a transform (null until made)
        type(c_ptr) :: texture_switch = c_null_ptr          !! shows or hides the texture (draw mode)
        type(c_ptr) :: star_switch = c_null_ptr             !! shows or hides the stars (draw mode)
        type(c_ptr) :: texture_node = c_null_ptr            !! the texture drawing (null: none)
        type(c_ptr) :: star_node = c_null_ptr               !! the stars drawing (null: none)
        logical :: shown = .false.                          !! made for a window: nodes made later are compiled
    contains
        procedure :: set_texture_map, texture_map
        procedure :: set_star_data, star_catalog, star_magnitudes, star_sizes
        procedure :: num_stars, star_magnitude_range, star
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
            call check(vkf_node_add_child(self%root, self%texture_switch))   ! the texture first, the stars over it
            call check(vkf_node_add_child(self%root, self%star_switch))
            call make_texture_node(self)
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
            call check(vkf_node_remove_child(self%texture_switch, self%texture_node))
            call vkf_node_delete(self%texture_node)
            self%texture_node = c_null_ptr
        end if
        if (c_associated(self%image)) then
            self%texture_node = vkf_sky_texture_create(self%image)
            if (c_associated(self%texture_node)) then
                call apply_attitude(self, self%texture_node)
                if (self%shown) call check(vkf_compile_node(self%texture_node))
                call check(vkf_node_add_child(self%texture_switch, self%texture_node))
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
            call check(vkf_node_remove_child(self%star_switch, self%star_node))
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
                    call check(vkf_sky_stars_changed(self%star_node, int(self%n_stars, c_int)))
                end if
                call apply_attitude(self, self%star_node)
                if (self%shown) call check(vkf_compile_node(self%star_node))
                call check(vkf_node_add_child(self%star_switch, self%star_node))
            end if
        end if
        call apply_mode(self)
    end subroutine make_star_node

    !> Show or hide the texture and the stars, as the draw mode says.
    subroutine apply_mode(self)
        type(SkySphere), intent(in) :: self
        if (c_associated(self%texture_switch)) &
            call check(vkf_switch_set(self%texture_switch, merge(1_c_int, 0_c_int, iand(self%mode, SKY_TEXTURE) /= 0)))
        if (c_associated(self%star_switch)) &
            call check(vkf_switch_set(self%star_switch, merge(1_c_int, 0_c_int, iand(self%mode, SKY_STARS) /= 0)))
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
        call check(vkf_sky_style_changed(n))
    end subroutine apply_attitude

    !> Report a failed rendering-layer call (status not 0).
    subroutine check(status)
        integer(c_int), intent(in) :: status
        if (status /= 0) write(*, '(a)') 'vkframes: a rendering-layer call failed (SkySphere)'
    end subroutine check

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
        end do
        close(unit)
        if (self%n_stars == 0) self%mag_range = 0.0_wp
        ok = .true.
    end function read_stars

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
