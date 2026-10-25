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
!> Shape and textures are read when a window first shows the sphere; set them
!> before that. (Not drawn yet: the night texture, which needs the Sun's light,
!> and automatic level of detail; their settings are kept.)
module vkf_sphere

    use, intrinsic :: iso_c_binding, only: c_ptr, c_null_ptr, c_associated, c_int, c_float
    use vkf_math, only: wp, quat_identity, quat_normalize, frame_matrix
    use vkf_frames, only: ReferenceFrame
    use vkf_layer, only: vkf_image_read, vkf_image_delete, vkf_transform_create, vkf_transform_set_matrix, &
                         vkf_node_add_child, vkf_node_delete
    use vkf_geometry, only: ellipsoid_create

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
        type(c_ptr) :: sphere_node = c_null_ptr              !! transform of the body within the frame
    contains
        procedure :: set_radius, radius
        procedure :: set_sphere_scale, sphere_scale, radii
        procedure :: set_sphere_position, sphere_position
        procedure :: set_sphere_attitude, sphere_attitude
        procedure :: set_texture_map, texture_map
        procedure :: set_night_texture_map, night_texture_map
        procedure :: set_auto_lod, auto_lod
        procedure :: set_resolution
        procedure :: frame_info => sphere_frame_info
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

    !> The texture for the side away from the light (e.g. city lights). Kept
    !> for when lighting is added; not drawn yet.
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

    function sphere_frame_info(self) result(info)
        class(Sphere), intent(in) :: self
        character(len=:), allocatable :: info
        info = 'Sphere'
    end function sphere_frame_info

    !> The body: an ellipsoid mesh under the sphere's transform.
    subroutine sphere_build_contents(self, viewer, group)
        class(Sphere), intent(inout) :: self
        type(c_ptr), intent(in) :: viewer, group
        type(c_ptr) :: image, mesh
        real(wp) :: c(4)
        image = c_null_ptr
        if (allocated(self%texture)) then
            if (len(self%texture) > 0) then
                image = vkf_image_read(self%texture, len(self%texture, kind=c_int))
                if (.not. c_associated(image)) write(*, '(a)') 'vkframes: Sphere '//self%name()// &
                                                               ': could not read texture '//self%texture
            end if
        end if
        c = 1.0_wp
        if (.not. c_associated(image)) c = self%color()
        mesh = ellipsoid_create(self%radii(), real(c, c_float), image, self%n_lon, self%n_lat)
        if (c_associated(image)) call vkf_image_delete(image)
        if (.not. c_associated(mesh)) return
        self%sphere_node = vkf_transform_create()
        call update_sphere_transform(self)
        if (vkf_node_add_child(self%sphere_node, mesh) /= 0 .or. vkf_node_add_child(group, self%sphere_node) /= 0) &
            write(*, '(a)') 'vkframes: Sphere '//self%name()//': could not build the scene'
        call vkf_node_delete(mesh)
    end subroutine sphere_build_contents

    subroutine update_sphere_transform(self)
        type(Sphere), intent(inout) :: self
        if (.not. c_associated(self%sphere_node)) return
        if (vkf_transform_set_matrix(self%sphere_node, frame_matrix(self%sphere_position_, self%sphere_attitude_)) /= 0) &
            write(*, '(a)') 'vkframes: Sphere '//self%name()//': transform failed'
    end subroutine update_sphere_transform

end module vkf_sphere
