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
!> the diffuse light is weak (the night side: city lights), as OpenFrames'
!> setNightTextureMap.
!>
!> Texture files are found with find_data_file (a path, or a name on
!> VKFRAMES_DATA_PATH, e.g. data/'s 2k_earth_daymap.jpg and
!> 2k_earth_nightmap.jpg). Shape and textures are read when a window first
!> shows the sphere; set them
!> before that; the material can change at any time. (Not done yet: automatic
!> level of detail; its setting is kept.)
module vkf_sphere

    use, intrinsic :: iso_c_binding, only: c_ptr, c_null_ptr, c_associated, c_int, c_float
    use vkf_math, only: wp, quat_identity, quat_normalize, frame_matrix
    use vkf_frames, only: ReferenceFrame
    use vkf_layer, only: vkf_image_read, vkf_image_delete, vkf_transform_create, vkf_transform_set_matrix, &
                         vkf_node_add_child, vkf_node_delete, mesh_set_material
    use vkf_geometry, only: ellipsoid_create
    use vkf_files, only: find_data_file

    implicit none

    private

    !> A sphere or ellipsoid (see the module documentation).
    type, extends(ReferenceFrame), public :: Sphere
        private
        real(wp) :: radius_ = 1.0_wp
        real(wp) :: scale_(3) = 1.0_wp
        real(wp) :: sphere_position_(3) = 0.0_wp
        real(wp) :: sphere_attitude_(4) = quat_identity
        character(len=:), allocatable :: texture, night_texture
        logical :: auto_lod_ = .false.
        integer :: n_lon = 128, n_lat = 64
        ! material (VSG's Phong; OpenGL's default: no specular reflection)
        real(wp) :: ambient_(4) = 1.0_wp, diffuse_(4) = 1.0_wp
        real(wp) :: specular_(4) = [0.0_wp, 0.0_wp, 0.0_wp, 1.0_wp], emission_(4) = [0.0_wp, 0.0_wp, 0.0_wp, 1.0_wp]
        real(wp) :: shininess_ = 0.0_wp
        type(c_ptr) :: sphere_node = c_null_ptr              !! transform of the body within the frame
        type(c_ptr) :: mesh = c_null_ptr
    contains
        procedure :: set_radius, radius
        procedure :: set_sphere_scale, sphere_scale, radii
        procedure :: set_sphere_position, sphere_position
        procedure :: set_sphere_attitude, sphere_attitude
        procedure :: set_texture_map, texture_map
        procedure :: set_night_texture_map, night_texture_map
        procedure :: set_auto_lod, auto_lod
        procedure :: set_material, material
        procedure :: set_resolution
        procedure :: frame_info => sphere_frame_info
        procedure :: bound => sphere_bound
        procedure :: build_contents => sphere_build_contents
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
        self%radius_ = radius
    end subroutine set_radius

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

    subroutine apply_material(self)
        type(Sphere), intent(inout) :: self
        if (.not. c_associated(self%mesh)) return
        if (mesh_set_material(self%mesh, real(self%ambient_, c_float), real(self%diffuse_, c_float), &
                              real(self%specular_, c_float), real(self%emission_, c_float), &
                              real(self%shininess_, c_float)) /= 0) &
            write(*, '(a)') 'vkframes: Sphere '//self%name()//': could not set the material'
    end subroutine apply_material

    !> Adjust the detail to the viewing distance. Kept for later; the mesh has a
    !> fixed resolution for now (`set_resolution`).
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

    function sphere_frame_info(self) result(info)
        class(Sphere), intent(in) :: self
        character(len=:), allocatable :: info
        info = 'Sphere'
    end function sphere_frame_info

    !> The body: an ellipsoid mesh under the sphere's transform.
    subroutine sphere_build_contents(self, viewer, group)
        class(Sphere), intent(inout) :: self
        type(c_ptr), intent(in) :: viewer, group
        type(c_ptr) :: image, night
        real(wp) :: c(4)
        image = read_texture(self, self%texture)
        night = read_texture(self, self%night_texture)
        c = 1.0_wp
        if (.not. c_associated(image)) c = self%color()
        self%mesh = ellipsoid_create(self%radii(), real(c, c_float), image, self%n_lon, self%n_lat, night)
        if (c_associated(image)) call vkf_image_delete(image)
        if (c_associated(night)) call vkf_image_delete(night)
        if (.not. c_associated(self%mesh)) return
        call apply_material(self)
        self%sphere_node = vkf_transform_create()
        call update_sphere_transform(self)
        if (vkf_node_add_child(self%sphere_node, self%mesh) /= 0 .or. vkf_node_add_child(group, self%sphere_node) /= 0) &
            write(*, '(a)') 'vkframes: Sphere '//self%name()//': could not build the scene'
    end subroutine sphere_build_contents

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
        if (vkf_transform_set_matrix(self%sphere_node, frame_matrix(self%sphere_position_, self%sphere_attitude_)) /= 0) &
            write(*, '(a)') 'vkframes: Sphere '//self%name()//': transform failed'
    end subroutine update_sphere_transform

end module vkf_sphere
