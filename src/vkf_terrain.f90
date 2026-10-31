!> Maps and terrain: a celestial body drawn from map layers by Rocky
!> (https://github.com/pelicanmapping/rocky; optional: `rocky_available()`),
!> e.g. the Earth from tile services, or the Moon with NASA's imagery and
!> LOLA's elevation. An addition to OpenFrames, which has only textured
!> spheres.
!>
!> The body is centered on its frame's origin, in body-fixed axes (z through
!> the north pole, x through longitude 0), so the frame's attitude turns it.
!> Its shape and coordinates come from Rocky's tiling profile: '' (Rocky's
!> default) or 'global-geodetic' for the Earth (WGS84), 'moon' for the Moon
!> (a sphere of 1737.4 km), or another of Rocky's profiles. Rocky works in
!> meters; the body is drawn at `units_per_meter` scene units per meter
!> (default 1e-3: kilometers, as vkframes' examples).
!>
!> Layers (`add_image_layer`, `add_elevation_layer`) are tiled sources Rocky
!> pages in as the view needs them: a TMS service (LAYER_TMS), a file or
!> service GDAL reads (LAYER_GDAL; a file is found with find_data_file, e.g.
!> data/'s moon_lola_ldem16.tif from `pixi run -e rocky moon-data`), or a
!> {z}/{x}/{y} URL template of geodetic tiles on the body (LAYER_GEODETIC: 2 x
!> 1 tiles at level 0, rows from the top, as WMTS and NASA's Moon Trek) or of
!> web Mercator tiles (LAYER_MERCATOR: OpenStreetMap-style). Elevation is in
!> meters (ELEVATION_HEIGHTS) or encoded in RGB (ELEVATION_MAPBOX,
!> ELEVATION_TERRARIUM). Layers can be added before or after the body is
!> shown.
!>
!> Lighting: lit by the scene's lights as a Sphere (a frame's light source, e.g.
!> the Sun, or the window's light; `set_lighting(.false.)` shows the imagery
!> as it is). The frame's color is the color where no imagery covers the body.
!>
!> The map is made for a window that shows the body (Rocky's tiles are loaded
!> through that window's viewer, and the map is drawn in that window only);
!> when that window closes, it is made again for one still open (other
!> windows then show it). Headless (no viewer), the body has no map.
module vkf_terrain

    use, intrinsic :: iso_c_binding, only: c_ptr, c_null_ptr, c_associated, c_int, c_float, c_double
    use vkf_math, only: wp, identity_matrix
    use vkf_frames, only: ReferenceFrame, PickHit, pick_ellipsoid
    use vkf_pick, only: PickCamera
    use vkf_layer, only: vkf_rocky_available, vkf_rocky_map_create, vkf_rocky_map_settings, vkf_rocky_map_alive, &
                         vkf_rocky_add_image, vkf_rocky_add_elevation, vkf_rocky_elevation, vkf_node_add_child, &
                         vkf_node_remove_child, vkf_node_delete, vkf_compile_node, vkf_viewer_any
    use vkf_files, only: find_data_file

    implicit none

    private

    public :: rocky_available

    !> layer sources (see the module documentation)
    integer, parameter, public :: LAYER_TMS = 0             !! a TMS service
    integer, parameter, public :: LAYER_GDAL = 1            !! a file or service GDAL reads
    integer, parameter, public :: LAYER_GEODETIC = 2        !! a {z}/{x}/{y} URL template of geodetic tiles
    integer, parameter, public :: LAYER_MERCATOR = 3        !! a {z}/{x}/{y} URL template of web Mercator tiles
    !> elevation encodings
    integer, parameter, public :: ELEVATION_HEIGHTS = 0     !! heights in meters
    integer, parameter, public :: ELEVATION_MAPBOX = 1      !! Mapbox Terrain-RGB
    integer, parameter, public :: ELEVATION_TERRARIUM = 2   !! Terrarium RGB

    real(wp), parameter :: earth_radius = 6378137.0_wp      !! m (WGS84)
    real(wp), parameter :: moon_radius = 1737400.0_wp       !! m

    !> A map layer as given (applied when the map is made).
    type :: MapLayer
        logical :: elevation = .false.                  !! (true: an elevation layer; false: an image layer)
        integer :: source = LAYER_TMS                   !! a LAYER_* constant
        integer :: encoding = ELEVATION_HEIGHTS         !! (elevation: an ELEVATION_* constant)
        character(len=:), allocatable :: uri            !! URL, URL template or file
    end type MapLayer

    !> A body drawn from map layers by Rocky (see the module documentation).
    type, extends(ReferenceFrame), public :: TerrainBody
        private
        character(len=:), allocatable :: profile_                !! Rocky's tiling profile ('': the Earth)
        real(wp) :: units_per_meter_ = 1.0e-3_wp                !! scene units per meter
        logical :: lighting_ = .true.                           !! lit by the scene's lights
        type(MapLayer), allocatable :: layers(:)                !! (capacity): the layers, in the order added
        integer :: n_layers = 0                                 !! number of layers
        type(c_ptr) :: map = c_null_ptr                         !! Rocky's map node (null: not made)
        type(c_ptr) :: map_group = c_null_ptr                   !! the group the map is in (once shown)
    contains
        procedure :: add_image_layer, add_elevation_layer, num_layers
        procedure :: set_lighting, lighting
        procedure :: profile, units_per_meter, body_radius, elevation
        procedure :: frame_info => terrain_frame_info
        procedure :: bound => terrain_bound
        procedure :: pick => terrain_pick
        procedure :: build_contents => terrain_build_contents
        procedure :: update => terrain_update
    end type TerrainBody

    !> The TerrainBody constructor: `TerrainBody(...)` makes one (see `new_terrain_body` for its
    !> arguments).
    interface TerrainBody
        module procedure new_terrain_body
    end interface TerrainBody

contains

    !> Whether vkframes was built with Rocky (the `rocky` pixi environment).
    logical function rocky_available()
        rocky_available = vkf_rocky_available() /= 0
    end function rocky_available

    !> A body named `name` in Rocky's tiling profile `profile` (default '':
    !> the Earth; 'moon': the Moon), drawn at `units_per_meter` scene units per
    !> meter (default 1e-3), in color `color` where it has no imagery ([r, g, b]
    !> or [r, g, b, a]; default the frame's).
    function new_terrain_body(name, profile, units_per_meter, color) result(b)
        character(len=*), intent(in) :: name
        character(len=*), intent(in), optional :: profile          !! Rocky's tiling profile (default '')
        real(wp), intent(in), optional :: units_per_meter          !! scene units per meter (default 1e-3)
        real(wp), intent(in), optional :: color(:)                 !! [r, g, b] or [r, g, b, a] (0 to 1)
        type(TerrainBody) :: b
        b%ReferenceFrame = ReferenceFrame(name, color)
        b%profile_ = ''
        if (present(profile)) b%profile_ = profile
        if (present(units_per_meter)) b%units_per_meter_ = units_per_meter
        allocate(b%layers(4))
    end function new_terrain_body

    !> Add an image layer: `uri` (a URL, URL template or file) of a `source`
    !> kind (LAYER_*; default LAYER_TMS). Layers added later are drawn on top.
    subroutine add_image_layer(self, uri, source)
        class(TerrainBody), intent(inout) :: self
        character(len=*), intent(in) :: uri
        integer, intent(in), optional :: source         !! a LAYER_* constant (default LAYER_TMS)
        type(MapLayer) :: layer
        layer%uri = uri
        if (present(source)) layer%source = source
        call add_layer(self, layer)
    end subroutine add_image_layer

    !> Add an elevation layer: `uri` of a `source` kind (LAYER_*; default
    !> LAYER_TMS), heights in meters or `encoding` (ELEVATION_*; default
    !> ELEVATION_HEIGHTS).
    subroutine add_elevation_layer(self, uri, source, encoding)
        class(TerrainBody), intent(inout) :: self
        character(len=*), intent(in) :: uri
        integer, intent(in), optional :: source         !! a LAYER_* constant (default LAYER_TMS)
        integer, intent(in), optional :: encoding       !! an ELEVATION_* constant (default ELEVATION_HEIGHTS)
        type(MapLayer) :: layer
        layer%elevation = .true.
        layer%uri = uri
        if (present(source)) layer%source = source
        if (present(encoding)) layer%encoding = encoding
        call add_layer(self, layer)
    end subroutine add_elevation_layer

    !> Keep `layer`, and add it to the map if it is made.
    subroutine add_layer(self, layer)
        type(TerrainBody), intent(inout) :: self
        type(MapLayer), intent(in) :: layer
        type(MapLayer), allocatable :: tmp(:)
        if (.not. allocated(self%layers)) allocate(self%layers(4))
        if (self%n_layers == size(self%layers)) then
            allocate(tmp(2 * self%n_layers))
            tmp(1:self%n_layers) = self%layers(1:self%n_layers)
            call move_alloc(tmp, self%layers)
        end if
        self%n_layers = self%n_layers + 1
        self%layers(self%n_layers) = layer
        if (c_associated(self%map)) call apply_layer(self, layer)
    end subroutine add_layer

    !> Number of layers (image and elevation).
    pure integer function num_layers(self)
        class(TerrainBody), intent(in) :: self
        num_layers = self%n_layers
    end function num_layers

    !> Light the body with the scene's lights (default), or show its imagery as it is.
    subroutine set_lighting(self, lit)
        class(TerrainBody), intent(inout) :: self
        logical, intent(in) :: lit
        self%lighting_ = lit
        call apply_settings(self)
    end subroutine set_lighting

    !> True if the body is lit by the scene's lights.
    pure logical function lighting(self)
        class(TerrainBody), intent(in) :: self
        lighting = self%lighting_
    end function lighting

    !> Rocky's tiling profile ('': Rocky's default, the Earth).
    function profile(self) result(p)
        class(TerrainBody), intent(in) :: self
        character(len=:), allocatable :: p
        p = self%profile_
    end function profile

    !> Scene units per meter the body is drawn at.
    pure real(wp) function units_per_meter(self)
        class(TerrainBody), intent(in) :: self
        units_per_meter = self%units_per_meter_
    end function units_per_meter

    !> The body's equatorial radius in scene units (from its profile: the
    !> Moon's for 'moon', else the Earth's).
    pure real(wp) function body_radius(self)
        class(TerrainBody), intent(in) :: self
        if (self%profile_ == 'moon') then
            body_radius = moon_radius * self%units_per_meter_
        else
            body_radius = earth_radius * self%units_per_meter_
        end if
    end function body_radius

    !> The height of the terrain (scene units, above the body's sphere or
    !> ellipsoid) at latitude and longitude `lat`, `lon` (degrees), from the
    !> last elevation layer added that is a file (LAYER_GDAL), read directly
    !> (also before the body is shown); `found` is false (and the height 0)
    !> without such a layer or data there, or without Rocky.
    function elevation(self, lat, lon, found) result(h)
        class(TerrainBody), intent(in) :: self
        real(wp), intent(in) :: lat, lon                !! latitude and longitude (degrees)
        logical, intent(out), optional :: found         !! false if there is no height there
        real(wp) :: h
        character(len=:), allocatable :: file
        integer(c_int) :: ok
        integer :: k
        h = 0.0_wp
        if (present(found)) found = .false.
        if (.not. rocky_available()) return
        do k = self%n_layers, 1, -1
            if (.not. (self%layers(k)%elevation .and. self%layers(k)%source == LAYER_GDAL)) cycle
            file = self%layers(k)%uri
            if (index(file, '://') == 0) file = find_data_file(file)
            if (len(file) == 0) return
            h = vkf_rocky_elevation(file, len(file, kind=c_int), self%profile_, len(self%profile_, kind=c_int), &
                                    real(lat, c_double), real(lon, c_double), ok) * self%units_per_meter_
            if (present(found)) found = ok /= 0
            return
        end do
    end function elevation

    !> The body's bounding sphere (frame coordinates): its equatorial radius, at the origin.
    subroutine terrain_bound(self, center, radius)
        class(TerrainBody), intent(in) :: self
        real(wp), intent(out) :: center(3), radius
        center = 0.0_wp
        radius = self%body_radius()
    end subroutine terrain_bound

    !> The kind of frame: 'TerrainBody'.
    function terrain_frame_info(self) result(info)
        class(TerrainBody), intent(in) :: self
        character(len=:), allocatable :: info
        info = 'TerrainBody'
    end function terrain_frame_info

    !> The map, made for the viewer showing the body (none headless or without Rocky).
    subroutine terrain_build_contents(self, viewer, group)
        class(TerrainBody), intent(inout) :: self
        type(c_ptr), intent(in) :: viewer   !! a window's viewer handle (null: headless)
        type(c_ptr), intent(in) :: group    !! the group node to build under
        if (.not. c_associated(viewer)) return
        if (.not. rocky_available()) then
            write(*, '(a)') 'vkframes: TerrainBody '//self%name()//': vkframes was built without Rocky ' // &
                            '(pixi run -e rocky rocky-build, then run in the rocky environment)'
            return
        end if
        self%map_group = group
        call make_map(self, viewer)
    end subroutine terrain_build_contents

    !> The map (in the contents group) for `viewer`, with the settings and layers.
    subroutine make_map(self, viewer)
        type(TerrainBody), intent(inout) :: self
        type(c_ptr), intent(in) :: viewer
        integer :: k
        self%map = vkf_rocky_map_create(viewer, self%profile_, len(self%profile_, kind=c_int), &
                                        real(self%units_per_meter_, c_double))
        if (.not. c_associated(self%map)) then
            write(*, '(a)') 'vkframes: TerrainBody '//self%name()//': could not make the map'
            return
        end if
        call apply_settings(self)
        do k = 1, self%n_layers
            call apply_layer(self, self%layers(k))
        end do
        if (vkf_node_add_child(self%map_group, self%map) /= 0) &
            write(*, '(a)') 'vkframes: TerrainBody '//self%name()//': could not build the scene'
    end subroutine make_map

    !> Each frame: if the map's window has closed (the map was released with
    !> it), the map again for a window still open (compiled for the windows
    !> running), then the frame's update.
    recursive subroutine terrain_update(self, t)
        class(TerrainBody), intent(inout) :: self
        real(wp), intent(in) :: t           !! simulation time (s)
        type(c_ptr) :: viewer
        integer(c_int) :: status
        if (c_associated(self%map)) then
            if (vkf_rocky_map_alive(self%map) == 0) then
                viewer = vkf_viewer_any()
                if (c_associated(viewer)) then
                    status = vkf_node_remove_child(self%map_group, self%map)
                    call vkf_node_delete(self%map)
                    call make_map(self, viewer)
                    if (c_associated(self%map)) then
                        if (vkf_compile_node(self%map) /= 0) &
                            write(*, '(a)') 'vkframes: TerrainBody '//self%name()//': could not compile the map'
                    end if
                end if
            end if
        end if
        call self%ReferenceFrame%update(t)
    end subroutine terrain_update

    !> Give the lighting and the frame's color to the map (if made).
    subroutine apply_settings(self)
        type(TerrainBody), intent(inout) :: self
        if (.not. c_associated(self%map)) return
        if (vkf_rocky_map_settings(self%map, merge(1_c_int, 0_c_int, self%lighting_), real(self%color(), c_float)) /= 0) &
            write(*, '(a)') 'vkframes: TerrainBody '//self%name()//': could not apply the settings'
    end subroutine apply_settings

    !> Add `layer` to the map (a file found on VKFRAMES_DATA_PATH).
    subroutine apply_layer(self, layer)
        type(TerrainBody), intent(inout) :: self
        type(MapLayer), intent(in) :: layer
        character(len=:), allocatable :: uri
        integer(c_int) :: status
        uri = layer%uri
        if (layer%source == LAYER_GDAL .and. index(uri, '://') == 0) then     ! a file: by name on VKFRAMES_DATA_PATH
            uri = find_data_file(layer%uri)
            if (len(uri) == 0) then
                write(*, '(a)') 'vkframes: TerrainBody '//self%name()//': could not find '//layer%uri
                return
            end if
        end if
        if (layer%elevation) then
            status = vkf_rocky_add_elevation(self%map, int(layer%source, c_int), uri, len(uri, kind=c_int), &
                                             int(layer%encoding, c_int))
        else
            status = vkf_rocky_add_image(self%map, int(layer%source, c_int), uri, len(uri, kind=c_int))
        end if
        if (status /= 0) write(*, '(a)') 'vkframes: TerrainBody '//self%name()//': could not add layer '//layer%uri
    end subroutine apply_layer

    !> Picked on a sphere of the body's radius (not the terrain's heights), with
    !> the latitude and longitude of the point hit; else the frame's origin.
    subroutine terrain_pick(self, cam, to_root, hits, n)
        class(TerrainBody), intent(in), target :: self
        type(PickCamera), intent(in) :: cam                 !! the cursor's ray and the pick's settings
        real(wp), intent(in) :: to_root(4,4)                !! the frame's transform to the root frame
        type(PickHit), allocatable, intent(inout) :: hits(:)  !! hits so far (grown as needed)
        integer, intent(inout) :: n                         !! number of hits in hits
        real(wp) :: r, m(4,4)
        r = self%body_radius()
        if (self%contents_shown() .and. r > 0.0_wp) then
            m = identity_matrix()
            if (pick_ellipsoid(self, cam, to_root, [0.0_wp, 0.0_wp, 0.0_wp], m(1:3, 1:3), [r, r, r], .true., hits, n)) return
        end if
        call self%ReferenceFrame%pick(cam, to_root, hits, n)
    end subroutine terrain_pick

end module vkf_terrain
