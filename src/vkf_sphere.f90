!> Spheres and ellipsoids: a textured celestial body, as OpenFrames' Sphere.
!>
!> The body is centered on its frame's origin (or at `set_sphere_position`,
!> turned by `set_sphere_attitude`, e.g. to orient a texture). Its shape is a
!> sphere of `radius`, or an ellipsoid with semi-axes radius * `sphere_scale`;
!> the ellipsoid is built directly (exact normals, geodetic texture mapping; see
!> vkf_geometry), where OpenFrames scales a sphere. The texture is an
!> equirectangular image (longitude -180 to 180, north at the top). The color is
!> used when there is no texture.
!>
!> Lighting: the body is lit by the scene's lights (a frame's light source,
!> e.g. the Sun, or the window's light), with a material (`set_material`;
!> OpenFrames' default: no specular reflection). The night texture shows where
!> the surface is lit weakly (the night side: city lights), as OpenFrames'
!> setNightTextureMap: weighted by 1 - the lit color (the material's ambient
!> and diffuse colors in the lights' colors and intensities, and its
!> emission), per channel and per pixel, so a brighter light narrows the
!> band where the day fades into the night, and ambient light dims the night
!> texture. The twilight (`set_twilight`; not in OpenFrames; default none:
!> the light ends at the terminator) extends the diffuse light past the
!> terminator, to where the light is a given angle below the horizon, fading
!> out: e.g. sunlight scattered by an atmosphere.
!>
!> Texture files are found with find_data_file (a path, or a name on
!> VKFRAMES_DATA_PATH, e.g. data/'s 2k_earth_daymap.jpg and
!> 2k_earth_nightmap.jpg). The textures and the mesh resolution are read when
!> a window first shows the sphere: set them before that. The radius, scale,
!> color and material can change at any time: the meshes are built once, then
!> scaled by their transform (radii / the built radii), and the color is applied
!> through the material (the shaders multiply the vertex color, white here, by
!> the diffuse color).
!>
!> Level of detail (`set_auto_lod`, on by default; OpenFrames' setAutoLOD): the
!> body has meshes of 32, 64, ... 1024 segments around (half as many from pole
!> to pole), and each view draws the coarsest whose facets stay within half a
!> pixel of the true outline (src/vsg/vkf_layer.cpp: SphereLod), so that the
!> body isn't faceted however close the view. Meshes are built when a view
!> first wants them (the 128-segment one at once), in the sphere's update.
!> Off: one mesh of `set_resolution`'s segments (default 128 x 64).
module vkf_sphere

    use, intrinsic :: iso_c_binding, only: c_ptr, c_null_ptr, c_associated, c_int, c_float
    use vkf_math, only: wp, quat_identity, quat_normalize, frame_matrix, quat_to_matrix
    use vkf_frames, only: ReferenceFrame, PickHit, pick_ellipsoid
    use vkf_pick, only: PickCamera
    use vkf_layer, only: vkf_image_read, vkf_image_delete, vkf_transform_create, vkf_transform_set_matrix, &
                         vkf_node_add_child, vkf_node_delete, mesh_set_material, vkf_mesh_set_twilight, vkf_compile_node, &
                         vkf_sphere_lod_create, vkf_sphere_lod_add, vkf_sphere_lod_requested
    use vkf_geometry, only: ellipsoid_create
    use vkf_files, only: find_data_file

    implicit none

    private

    !> The level-of-detail meshes: segments around the equator (half as many
    !> from pole to pole); the one built at once.
    integer, parameter :: lod_segments(6) = [32, 64, 128, 256, 512, 1024]
    integer, parameter :: lod_first = 3

    !> A sphere or ellipsoid (see the module documentation).
    type, extends(ReferenceFrame), public :: Sphere
        private
        real(wp) :: radius_ = 1.0_wp
        real(wp) :: scale_(3) = 1.0_wp
        real(wp) :: sphere_position_(3) = 0.0_wp
        real(wp) :: sphere_attitude_(4) = quat_identity
        real(wp) :: built_radii(3) = 1.0_wp                   !! the mesh's semi-axes
        logical :: textured = .false.                         !! (the color is not used)
        character(len=:), allocatable :: texture, night_texture
        logical :: auto_lod_ = .true.
        integer :: n_lon = 128, n_lat = 64                    !! without level of detail
        ! material (VSG's Phong; OpenGL's default: no specular reflection)
        real(wp) :: ambient_(4) = 1.0_wp, diffuse_(4) = 1.0_wp
        real(wp) :: specular_(4) = [0.0_wp, 0.0_wp, 0.0_wp, 1.0_wp], emission_(4) = [0.0_wp, 0.0_wp, 0.0_wp, 1.0_wp]
        real(wp) :: shininess_ = 0.0_wp
        real(wp) :: twilight_ = 0.0_wp                        !! radians below the horizon (0: none)
        type(c_ptr) :: sphere_node = c_null_ptr              !! transform of the body within the frame
        type(c_ptr) :: mesh = c_null_ptr                     !! without level of detail
        ! level of detail
        type(c_ptr) :: lod_node = c_null_ptr                 !! chooses among the meshes, per view
        type(c_ptr) :: lod_mesh(size(lod_segments)) = c_null_ptr
        type(c_ptr) :: image = c_null_ptr, night = c_null_ptr   !! the textures, for meshes built later
        logical :: shown = .false.                            !! (meshes built later are compiled)
    contains
        procedure :: set_radius, radius
        procedure :: set_color => sphere_set_color
        procedure :: set_sphere_scale, sphere_scale, radii
        procedure :: set_sphere_position, sphere_position
        procedure :: set_sphere_attitude, sphere_attitude
        procedure :: set_texture_map, texture_map
        procedure :: set_night_texture_map, night_texture_map
        procedure :: set_auto_lod, auto_lod
        procedure :: set_material, material
        procedure :: set_twilight, twilight
        procedure :: set_resolution
        procedure :: frame_info => sphere_frame_info
        procedure :: bound => sphere_bound
        procedure :: pick => sphere_pick
        procedure :: build_contents => sphere_build_contents
        procedure :: update => sphere_update
        procedure :: num_lod_meshes
    end type Sphere

    interface Sphere
        module procedure new_sphere
    end interface Sphere

contains

    !> A sphere named `name`, of radius `radius` (default 1) and color `color`
    !> ([r, g, b] or [r, g, b, a]; default white, 90% opaque).
    function new_sphere(name, radius, color) result(s)
        character(len=*), intent(in) :: name
        real(wp), intent(in), optional :: radius
        real(wp), intent(in), optional :: color(:)
        type(Sphere) :: s
        s%ReferenceFrame = ReferenceFrame(name, color)
        if (present(radius)) s%radius_ = radius
    end function new_sphere

    subroutine set_radius(self, radius)
        class(Sphere), intent(inout) :: self
        real(wp), intent(in) :: radius
        if (radius <= 0.0_wp) return
        self%radius_ = radius
        call update_sphere_transform(self)
    end subroutine set_radius

    !> The body's color (used without a texture); [r, g, b] or [r, g, b, a].
    subroutine sphere_set_color(self, color)
        class(Sphere), intent(inout) :: self
        real(wp), intent(in) :: color(:)
        call self%ReferenceFrame%set_color(color)
        call apply_material(self)
    end subroutine sphere_set_color

    pure real(wp) function radius(self)
        class(Sphere), intent(in) :: self
        radius = self%radius_
    end function radius

    !> Scale the sphere along its x, y and z axes (all > 0), making an
    !> ellipsoid with semi-axes radius * scale.
    subroutine set_sphere_scale(self, scale)
        class(Sphere), intent(inout) :: self
        real(wp), intent(in) :: scale(3)
        if (any(scale <= 0.0_wp)) return
        self%scale_ = scale
        call update_sphere_transform(self)
    end subroutine set_sphere_scale

    pure function sphere_scale(self) result(s)
        class(Sphere), intent(in) :: self
        real(wp) :: s(3)
        s = self%scale_
    end function sphere_scale

    !> The semi-axes: radius * sphere_scale.
    pure function radii(self) result(r)
        class(Sphere), intent(in) :: self
        real(wp) :: r(3)
        r = self%radius_ * self%scale_
    end function radii

    !> Position of the body's center in its frame.
    subroutine set_sphere_position(self, position)
        class(Sphere), intent(inout) :: self
        real(wp), intent(in) :: position(3)
        self%sphere_position_ = position
        call update_sphere_transform(self)
    end subroutine set_sphere_position

    pure function sphere_position(self) result(p)
        class(Sphere), intent(in) :: self
        real(wp) :: p(3)
        p = self%sphere_position_
    end function sphere_position

    !> Attitude of the body in its frame (quaternion [x, y, z, w]), e.g. to
    !> orient its texture.
    subroutine set_sphere_attitude(self, q)
        class(Sphere), intent(inout) :: self
        real(wp), intent(in) :: q(4)
        self%sphere_attitude_ = quat_normalize(q)
        call update_sphere_transform(self)
    end subroutine set_sphere_attitude

    pure function sphere_attitude(self) result(q)
        class(Sphere), intent(in) :: self
        real(wp) :: q(4)
        q = self%sphere_attitude_
    end function sphere_attitude

    !> The texture: an equirectangular image file ('' for none).
    subroutine set_texture_map(self, file)
        class(Sphere), intent(inout) :: self
        character(len=*), intent(in) :: file
        self%texture = file
    end subroutine set_texture_map

    function texture_map(self) result(file)
        class(Sphere), intent(in) :: self
        character(len=:), allocatable :: file
        file = ''
        if (allocated(self%texture)) file = self%texture
    end function texture_map

    !> The texture for the side away from the light (e.g. city lights; an
    !> equirectangular image file as the texture, '' for none), shown where the
    !> diffuse light is weak.
    subroutine set_night_texture_map(self, file)
        class(Sphere), intent(inout) :: self
        character(len=*), intent(in) :: file
        self%night_texture = file
    end subroutine set_night_texture_map

    function night_texture_map(self) result(file)
        class(Sphere), intent(in) :: self
        character(len=:), allocatable :: file
        file = ''
        if (allocated(self%night_texture)) file = self%night_texture
    end function night_texture_map

    !> The material (OpenFrames' setMaterial; each optional): ambient, diffuse,
    !> specular and emission colors ([r, g, b] or [r, g, b, a], 0 to 1) and
    !> shininess (0 to 128: the specular highlight's sharpness; 0 for none).
    !> The ambient and diffuse colors scale the body's color or texture (VSG's
    !> Phong); the specular color is reflected from the lights' specular parts.
    !> Defaults: ambient and diffuse white, no specular reflection or emission.
    subroutine set_material(self, ambient, diffuse, specular, emission, shininess)
        class(Sphere), intent(inout) :: self
        real(wp), intent(in), optional :: ambient(:), diffuse(:), specular(:), emission(:), shininess
        if (present(ambient)) self%ambient_(1:min(4, size(ambient))) = ambient(1:min(4, size(ambient)))
        if (present(diffuse)) self%diffuse_(1:min(4, size(diffuse))) = diffuse(1:min(4, size(diffuse)))
        if (present(specular)) self%specular_(1:min(4, size(specular))) = specular(1:min(4, size(specular)))
        if (present(emission)) self%emission_(1:min(4, size(emission))) = emission(1:min(4, size(emission)))
        if (present(shininess)) self%shininess_ = max(0.0_wp, shininess)
        call apply_material(self)
    end subroutine set_material

    pure subroutine material(self, ambient, diffuse, specular, emission, shininess)
        class(Sphere), intent(in) :: self
        real(wp), intent(out), optional :: ambient(4), diffuse(4), specular(4), emission(4), shininess
        if (present(ambient)) ambient = self%ambient_
        if (present(diffuse)) diffuse = self%diffuse_
        if (present(specular)) specular = self%specular_
        if (present(emission)) emission = self%emission_
        if (present(shininess)) shininess = self%shininess_
    end subroutine material

    !> The material, with the diffuse color times the body's color (without a texture).
    !> The material, with the diffuse color times the body's color (without a
    !> texture), on every mesh built.
    !> The twilight: the diffuse light extended past the terminator to where
    !> the light is `angle` (radians, 0 to 1.5) below the horizon, fading out
    !> smoothly (where it meets the light on the day side: the same brightness
    !> and slope, so no line shows), and the night texture fading in across it.
    !> 0 (the default): none, the light ends at the terminator (an airless
    !> body). Not in OpenFrames. Can change at any time.
    subroutine set_twilight(self, angle)
        class(Sphere), intent(inout) :: self
        real(wp), intent(in) :: angle
        self%twilight_ = max(0.0_wp, min(1.5_wp, angle))
        call apply_material(self)
    end subroutine set_twilight

    pure real(wp) function twilight(self)
        class(Sphere), intent(in) :: self
        twilight = self%twilight_
    end function twilight

    subroutine apply_material(self)
        type(Sphere), intent(inout) :: self
        integer :: k
        call apply_material_to(self, self%mesh)
        do k = 1, size(self%lod_mesh)
            call apply_material_to(self, self%lod_mesh(k))
        end do
    end subroutine apply_material

    subroutine apply_material_to(self, mesh)
        type(Sphere), intent(in) :: self
        type(c_ptr), intent(in) :: mesh
        real(wp) :: diffuse(4)
        if (.not. c_associated(mesh)) return
        diffuse = self%diffuse_
        if (.not. self%textured) diffuse = diffuse * self%color()
        if (mesh_set_material(mesh, real(self%ambient_, c_float), real(diffuse, c_float), &
                              real(self%specular_, c_float), real(self%emission_, c_float), &
                              real(self%shininess_, c_float)) /= 0) &
            write(*, '(a)') 'vkframes: Sphere '//self%name()//': could not set the material'
        if (vkf_mesh_set_twilight(mesh, real(self%twilight_, c_float)) /= 0) &
            write(*, '(a)') 'vkframes: Sphere '//self%name()//': could not set the twilight'
    end subroutine apply_material_to

    !> Level of detail (default on; see the module documentation): meshes of
    !> several resolutions, the one drawn chosen per view. Off: one mesh of
    !> `set_resolution`'s segments. Set it before a window shows the sphere.
    subroutine set_auto_lod(self, lod)
        class(Sphere), intent(inout) :: self
        logical, intent(in) :: lod
        self%auto_lod_ = lod
    end subroutine set_auto_lod

    pure logical function auto_lod(self)
        class(Sphere), intent(in) :: self
        auto_lod = self%auto_lod_
    end function auto_lod

    !> The mesh resolution: segments in longitude and latitude (default 128 x 64).
    !> The mesh resolution without level of detail: segments in longitude and
    !> latitude (default 128 x 64).
    subroutine set_resolution(self, n_lon, n_lat)
        class(Sphere), intent(inout) :: self
        integer, intent(in) :: n_lon, n_lat
        self%n_lon = max(3, n_lon)
        self%n_lat = max(2, n_lat)
    end subroutine set_resolution

    !> The body's bounding sphere: its center, and its largest semi-axis.
    subroutine sphere_bound(self, center, radius)
        class(Sphere), intent(in) :: self
        real(wp), intent(out) :: center(3), radius
        center = self%sphere_position_
        radius = maxval(self%radii())
    end subroutine sphere_bound

    !> Picking: the ellipsoid (exact), with the geodetic latitude and longitude
    !> of the point hit (in the body's axes: sphere_attitude); else the frame's
    !> origin.
    subroutine sphere_pick(self, cam, to_root, hits, n)
        class(Sphere), intent(in), target :: self
        type(PickCamera), intent(in) :: cam
        real(wp), intent(in) :: to_root(4,4)
        type(PickHit), allocatable, intent(inout) :: hits(:)
        integer, intent(inout) :: n
        if (self%contents_shown()) then
            if (pick_ellipsoid(self, cam, to_root, self%sphere_position_, quat_to_matrix(self%sphere_attitude_), &
                          self%radii(), .true., hits, n)) return
        end if
        call self%ReferenceFrame%pick(cam, to_root, hits, n)
    end subroutine sphere_pick

    function sphere_frame_info(self) result(info)
        class(Sphere), intent(in) :: self
        character(len=:), allocatable :: info
        info = 'Sphere'
    end function sphere_frame_info

    !> The body: an ellipsoid mesh under the sphere's transform.
    subroutine sphere_build_contents(self, viewer, group)
        class(Sphere), intent(inout) :: self
        type(c_ptr), intent(in) :: viewer, group
        type(c_ptr) :: image, night, body
        image = read_texture(self, self%texture)
        night = read_texture(self, self%night_texture)
        self%textured = c_associated(image)
        self%built_radii = self%radii()
        if (self%auto_lod_) then                           ! meshes chosen per view, built when wanted
            self%image = image                              ! (kept for those built later)
            self%night = night
            self%shown = c_associated(viewer)
            self%lod_node = vkf_sphere_lod_create(maxval(self%built_radii))
            call build_lod_mesh(self, lod_first, compile=.false.)    ! (compiled with the scene)
            if (.not. c_associated(self%lod_mesh(lod_first))) return
            body = self%lod_node
        else
            self%mesh = ellipsoid_create(self%built_radii, [1.0_c_float, 1.0_c_float, 1.0_c_float, 1.0_c_float], image, &
                                         self%n_lon, self%n_lat, night)
            if (c_associated(image)) call vkf_image_delete(image)
            if (c_associated(night)) call vkf_image_delete(night)
            if (.not. c_associated(self%mesh)) return
            body = self%mesh
        end if
        call apply_material(self)
        self%sphere_node = vkf_transform_create()
        call update_sphere_transform(self)
        if (vkf_node_add_child(self%sphere_node, body) /= 0 .or. vkf_node_add_child(group, self%sphere_node) /= 0) &
            write(*, '(a)') 'vkframes: Sphere '//self%name()//': could not build the scene'
    end subroutine sphere_build_contents

    !> Build level-of-detail mesh k (lod_segments(k) segments around) and add it
    !> to the level-of-detail node (compiled for the windows if `compile`).
    subroutine build_lod_mesh(self, k, compile)
        type(Sphere), intent(inout) :: self
        integer, intent(in) :: k
        logical, intent(in) :: compile
        type(c_ptr) :: mesh
        mesh = ellipsoid_create(self%built_radii, [1.0_c_float, 1.0_c_float, 1.0_c_float, 1.0_c_float], self%image, &
                                lod_segments(k), lod_segments(k) / 2, self%night)
        if (.not. c_associated(mesh)) then
            write(*, '(a,i0,a)') 'vkframes: Sphere '//self%name()//': could not build its ', lod_segments(k), '-segment mesh'
            return
        end if
        self%lod_mesh(k) = mesh
        call apply_material_to(self, mesh)
        if (compile) then
            if (vkf_compile_node(mesh) /= 0) write(*, '(a)') 'vkframes: Sphere '//self%name()//': could not compile a mesh'
        end if
        if (vkf_sphere_lod_add(self%lod_node, mesh, int(lod_segments(k), c_int)) /= 0) &
            write(*, '(a)') 'vkframes: Sphere '//self%name()//': could not add a mesh'
    end subroutine build_lod_mesh

    !> Update the frame (followed trajectories, children) and, with level of
    !> detail, build the mesh the views have wanted if it isn't built yet (the
    !> coarsest with as many segments as wanted, or the finest).
    recursive subroutine sphere_update(self, t)
        class(Sphere), intent(inout) :: self
        real(wp), intent(in) :: t
        integer :: wanted, k
        call self%ReferenceFrame%update(t)
        if (.not. c_associated(self%lod_node)) return
        wanted = vkf_sphere_lod_requested(self%lod_node)
        if (wanted <= 0) return
        k = findloc(lod_segments >= wanted, .true., 1)
        if (k == 0) k = size(lod_segments)
        if (.not. c_associated(self%lod_mesh(k))) call build_lod_mesh(self, k, compile=self%shown)
    end subroutine sphere_update

    !> Number of level-of-detail meshes built so far (0 without level of detail).
    integer function num_lod_meshes(self) result(n)
        class(Sphere), intent(in) :: self
        integer :: k
        n = 0
        do k = 1, size(self%lod_mesh)
            if (c_associated(self%lod_mesh(k))) n = n + 1
        end do
    end function num_lod_meshes

    !> An image handle for texture file `file` (null if none or unreadable).
    function read_texture(self, file) result(image)
        type(Sphere), intent(in) :: self
        character(len=:), allocatable, intent(in) :: file
        type(c_ptr) :: image
        character(len=:), allocatable :: path
        image = c_null_ptr
        if (.not. allocated(file)) return
        if (len(file) == 0) return
        path = find_data_file(file)
        if (len(path) > 0) image = vkf_image_read(path, len(path, kind=c_int))
        if (.not. c_associated(image)) write(*, '(a)') 'vkframes: Sphere '//self%name()//': could not read texture '//file
    end function read_texture

    subroutine update_sphere_transform(self)
        type(Sphere), intent(inout) :: self
        if (.not. c_associated(self%sphere_node)) return
        if (vkf_transform_set_matrix(self%sphere_node, frame_matrix(self%sphere_position_, self%sphere_attitude_, &
                                                                    self%radii() / self%built_radii)) /= 0) &
            write(*, '(a)') 'vkframes: Sphere '//self%name()//': transform failed'
    end subroutine update_sphere_transform

end module vkf_sphere
