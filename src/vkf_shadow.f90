!> Shadow cones: the regions of a celestial body's shadow in the light of
!> another (e.g. the Earth's or the Moon's shadow in sunlight), drawn as
!> transparent cones:
!>
!> * the umbra (total eclipse: the light hidden), converging behind the body
!>   to its apex, at d R / (Rs - R) from the body's center;
!> * the antumbra (annular eclipse: the light seen around the body), from the
!>   umbra's apex outward, widening at the umbra's angle;
!> * the penumbra (partial eclipse), widening behind the body.
!>
!> (R and Rs: the body's and the light's radii; d: the distance between their
!> centers. The edges are the lines tangent to both spheres: the outer
!> tangents for the umbra and antumbra, half-angle asin((Rs - R) / d), the
!> inner ones for the penumbra, asin((Rs + R) / d).) A body larger than its
!> light has no apex: its umbra widens (or is a cylinder), with no antumbra.
!>
!> A ShadowCones is a frame of its own, made a child of the body (`add_child`)
!> for one body's shadow, and shown or hidden with `show_contents`: a scene
!> without one does no shadow work at all. It refers to the body and the light
!> frames (they must stay alive while set); each scene update, after the whole
!> tree has moved (late_update), it moves itself to the body's center and
!> turns its +z axis away from the light (its own position and attitude are
!> set there, whatever its parent), and reshapes the cones for the distance
!> between them: the cones' vertices are rewritten in place and uploaded only
!> when the shape changed (not as the body only turns). Hidden, it does
!> nothing (and is placed again when shown).
!>
!> The radii: given (`set_body_radius`, `set_light_radius`), or the frames'
!> own: a Sphere's equatorial radius, a TerrainBody's (a light that is a plain
!> ReferenceFrame needs its radius given: none, no shadow). The shadow is drawn to
!> `set_length` behind the body's center (default: twice the umbra's length;
!> a hundred body radii without an apex); the umbra stops there if shorter.
!> Each region has a color ([r, g, b, a]; the surfaces are unlit,
!> transparent, seen from both sides, as the cones of vkf_shapes) and can be
!> drawn or not (`set_regions`).
!>
!> Bodies in a shadow are not darkened (that needs shadows in the lighting).
module vkf_shadow

    use, intrinsic :: iso_c_binding, only: c_ptr, c_null_ptr, c_associated, c_int, c_int32_t, c_float
    use vkf_math, only: wp, pi, quat_from_vectors, rigid_inverse
    use vkf_frames, only: ReferenceFrame, NO_AXES
    use vkf_sphere, only: Sphere
    use vkf_terrain, only: TerrainBody
    use vkf_layer, only: check_status, vkf_mesh_create, vkf_mesh_vertices_changed, vkf_switch_create, vkf_switch_set, &
                         vkf_node_add_child, mesh_arrays, mesh_set_material

    implicit none

    private

    !> The regions (combine with ior).
    integer, parameter, public :: SHADOW_UMBRA = 1, SHADOW_PENUMBRA = 2, SHADOW_ANTUMBRA = 4
    integer, parameter, public :: SHADOW_ALL = 7            !! SHADOW_UMBRA + SHADOW_PENUMBRA + SHADOW_ANTUMBRA

    integer, parameter :: n_regions = 3                     !! umbra, penumbra and antumbra
    integer, parameter :: umbra = 1, penumbra = 2, antumbra = 3     !! region indices
    integer, parameter :: n_around = 128                            !! segments around each cone

    !> A shadow region's surface: a frustum along z from z0 (radius r0) to z1
    !> (radius r1), in the shadow frame's coordinates.
    type :: ShadowRegion
        logical :: drawn = .false.                          !! has a surface (and is to be drawn)
        real(wp) :: z0 = 0.0_wp                             !! start along z (scene units)
        real(wp) :: r0 = 0.0_wp                             !! radius at z0 (scene units)
        real(wp) :: z1 = 0.0_wp                             !! end along z (scene units)
        real(wp) :: r1 = 0.0_wp                             !! radius at z1 (scene units)
        real(wp) :: color(4) = 1.0_wp                       !! [r, g, b, a] (0 to 1)
        type(c_ptr) :: switch = c_null_ptr                  !! shows or hides the mesh (null until built)
        type(c_ptr) :: mesh = c_null_ptr                    !! the surface: rings of vertices at z0 and z1
    end type ShadowRegion

    !> A body's shadow cones (see the module documentation).
    type, extends(ReferenceFrame), public :: ShadowCones
        private
        class(ReferenceFrame), pointer :: body_ => null()   !! the body casting the shadow (not owned)
        class(ReferenceFrame), pointer :: light_ => null()  !! the light (not owned)
        real(wp) :: body_radius_ = 0.0_wp, light_radius_ = 0.0_wp   !! (0: the frames' own)
        real(wp) :: length_ = 0.0_wp                                !! (0: automatic)
        integer :: regions_ = SHADOW_ALL                    !! an ior of SHADOW_UMBRA, SHADOW_PENUMBRA, SHADOW_ANTUMBRA
        type(ShadowRegion) :: region(n_regions)             !! umbra, penumbra and antumbra
        real(wp) :: shaped(4) = -1.0_wp                     !! R, Rs, d and length the cones have now
        real(wp) :: distance_ = 0.0_wp                      !! body to light at the last update (scene units)
        real(wp) :: drawn_length_ = 0.0_wp                  !! the length drawn at the last update (scene units)
    contains
        procedure :: set_body, body, set_light, light
        procedure :: set_body_radius, set_light_radius, body_radius, light_radius
        procedure :: set_length, length, drawn_length, distance
        procedure :: set_regions, regions
        procedure :: set_region_color, region_color
        procedure :: region_extent, umbra_length
        procedure :: frame_info => shadow_frame_info
        procedure :: bound => shadow_bound
        procedure :: build_contents => shadow_build_contents
        procedure :: late_update => shadow_late_update
    end type ShadowCones

    interface ShadowCones
        module procedure new_shadow_cones
    end interface ShadowCones

contains

    !> The shadow named `name` of `body` in the light of `light` (frames of the
    !> same tree; optional here, set later with set_body and set_light), with
    !> their radii if given (else the frames': see the module documentation).
    !> Its axes and name label are hidden.
    function new_shadow_cones(name, body, light, body_radius, light_radius) result(s)
        character(len=*), intent(in) :: name
        class(ReferenceFrame), intent(in), target, optional :: body, light
        real(wp), intent(in), optional :: body_radius, light_radius  !! scene units (default: the frames' own)
        type(ShadowCones) :: s
        s%ReferenceFrame = ReferenceFrame(name)
        call s%show_axes(NO_AXES)
        call s%show_axes_labels(NO_AXES)
        call s%show_name_label(.false.)
        if (present(body)) s%body_ => body
        if (present(light)) s%light_ => light
        if (present(body_radius)) call s%set_body_radius(body_radius)
        if (present(light_radius)) call s%set_light_radius(light_radius)
        s%region(umbra)%color = [0.95_wp, 0.25_wp, 0.2_wp, 0.25_wp]
        s%region(penumbra)%color = [0.95_wp, 0.75_wp, 0.25_wp, 0.08_wp]
        s%region(antumbra)%color = [0.35_wp, 0.5_wp, 1.0_wp, 0.25_wp]
    end function new_shadow_cones

    !> The body casting the shadow (it must stay alive while set), and its
    !> radius (optional: see set_body_radius).
    subroutine set_body(self, body, radius)
        class(ShadowCones), intent(inout) :: self
        class(ReferenceFrame), intent(in), target :: body
        real(wp), intent(in), optional :: radius            !! scene units (see set_body_radius)
        self%body_ => body
        if (present(radius)) call self%set_body_radius(radius)
    end subroutine set_body

    !> The body casting the shadow (null if not set).
    function body(self) result(p)
        class(ShadowCones), intent(in) :: self
        class(ReferenceFrame), pointer :: p
        p => self%body_
    end function body

    !> The light (e.g. the Sun; it must stay alive while set), and its radius
    !> (optional: see set_light_radius).
    subroutine set_light(self, light, radius)
        class(ShadowCones), intent(inout) :: self
        class(ReferenceFrame), intent(in), target :: light
        real(wp), intent(in), optional :: radius            !! scene units (see set_light_radius)
        self%light_ => light
        if (present(radius)) call self%set_light_radius(radius)
    end subroutine set_light

    !> The light (null if not set).
    function light(self) result(p)
        class(ShadowCones), intent(in) :: self
        class(ReferenceFrame), pointer :: p
        p => self%light_
    end function light

    !> The body's radius (scene units; 0: the body's own, a Sphere's
    !> equatorial radius or a TerrainBody's).
    subroutine set_body_radius(self, radius)
        class(ShadowCones), intent(inout) :: self
        real(wp), intent(in) :: radius                      !! scene units (0: the body's own)
        self%body_radius_ = max(0.0_wp, radius)
    end subroutine set_body_radius

    !> The light's radius (scene units; 0: the light's own, if a Sphere).
    subroutine set_light_radius(self, radius)
        class(ShadowCones), intent(inout) :: self
        real(wp), intent(in) :: radius                      !! scene units (0: the light's own)
        self%light_radius_ = max(0.0_wp, radius)
    end subroutine set_light_radius

    !> The body's radius used (0: none known).
    real(wp) function body_radius(self)
        class(ShadowCones), intent(in) :: self
        body_radius = self%body_radius_
        if (body_radius > 0.0_wp .or. .not. associated(self%body_)) return
        body_radius = frame_radius(self%body_)
    end function body_radius

    !> The light's radius used (0: none known).
    real(wp) function light_radius(self)
        class(ShadowCones), intent(in) :: self
        light_radius = self%light_radius_
        if (light_radius > 0.0_wp .or. .not. associated(self%light_)) return
        light_radius = frame_radius(self%light_)
    end function light_radius

    !> A body's own radius: a Sphere's equatorial one, a TerrainBody's; else 0.
    real(wp) function frame_radius(frame) result(r)
        class(ReferenceFrame), intent(in) :: frame
        real(wp) :: radii(3)
        r = 0.0_wp
        select type (frame)
        class is (Sphere)
            radii = frame%radii()
            r = max(radii(1), radii(2))
        class is (TerrainBody)
            r = frame%body_radius()
        end select
    end function frame_radius

    !> How far behind the body's center the shadow is drawn (scene units; 0:
    !> automatic, twice the umbra's length).
    subroutine set_length(self, length)
        class(ShadowCones), intent(inout) :: self
        real(wp), intent(in) :: length                      !! scene units (0: automatic)
        self%length_ = max(0.0_wp, length)
    end subroutine set_length

    !> The length set (0: automatic).
    pure real(wp) function length(self)
        class(ShadowCones), intent(in) :: self
        length = self%length_
    end function length

    !> The length drawn at the last update.
    pure real(wp) function drawn_length(self)
        class(ShadowCones), intent(in) :: self
        drawn_length = self%drawn_length_
    end function drawn_length

    !> The distance between the body and the light at the last update.
    pure real(wp) function distance(self)
        class(ShadowCones), intent(in) :: self
        distance = self%distance_
    end function distance

    !> The regions to draw: an ior of SHADOW_UMBRA, SHADOW_PENUMBRA and
    !> SHADOW_ANTUMBRA (default SHADOW_ALL).
    subroutine set_regions(self, regions)
        class(ShadowCones), intent(inout) :: self
        integer, intent(in) :: regions                      !! an ior of SHADOW_UMBRA, SHADOW_PENUMBRA, SHADOW_ANTUMBRA
        self%regions_ = iand(regions, SHADOW_ALL)
        self%shaped = -1.0_wp                               ! (redrawn at the next update)
    end subroutine set_regions

    !> The regions drawn: an ior of SHADOW_UMBRA, SHADOW_PENUMBRA and
    !> SHADOW_ANTUMBRA.
    pure integer function regions(self)
        class(ShadowCones), intent(in) :: self
        regions = self%regions_
    end function regions

    !> A region's color (region: one of SHADOW_UMBRA, SHADOW_PENUMBRA,
    !> SHADOW_ANTUMBRA; [r, g, b], alpha unchanged, or [r, g, b, a]).
    subroutine set_region_color(self, region, color)
        class(ShadowCones), intent(inout) :: self
        integer, intent(in) :: region                       !! SHADOW_UMBRA, SHADOW_PENUMBRA or SHADOW_ANTUMBRA
        real(wp), intent(in) :: color(:)                    !! [r, g, b] (alpha unchanged) or [r, g, b, a], 0 to 1
        integer :: k, n
        k = region_index(region)
        if (k == 0) return
        n = min(4, size(color))
        self%region(k)%color(1:n) = color(1:n)
        call apply_color(self%region(k))
    end subroutine set_region_color

    !> A region's color [r, g, b, a] (0 for an unknown region).
    function region_color(self, region) result(c)
        class(ShadowCones), intent(in) :: self
        integer, intent(in) :: region                       !! SHADOW_UMBRA, SHADOW_PENUMBRA or SHADOW_ANTUMBRA
        real(wp) :: c(4)
        integer :: k
        c = 0.0_wp
        k = region_index(region)
        if (k > 0) c = self%region(k)%color
    end function region_color

    !> The index in self%region of SHADOW_UMBRA, SHADOW_PENUMBRA or
    !> SHADOW_ANTUMBRA (0 for anything else).
    pure integer function region_index(region) result(k)
        integer, intent(in) :: region                       !! SHADOW_UMBRA, SHADOW_PENUMBRA or SHADOW_ANTUMBRA
        select case (region)
        case (SHADOW_UMBRA)
            k = umbra
        case (SHADOW_PENUMBRA)
            k = penumbra
        case (SHADOW_ANTUMBRA)
            k = antumbra
        case default
            k = 0
        end select
    end function region_index

    !> A region's surface at the last update, along the frame's +z axis (away
    !> from the light; the body's center at the origin): drawn, from z0
    !> (radius r0) to z1 (radius r1).
    subroutine region_extent(self, region, drawn, z0, r0, z1, r1)
        class(ShadowCones), intent(in) :: self
        integer, intent(in) :: region                       !! SHADOW_UMBRA, SHADOW_PENUMBRA or SHADOW_ANTUMBRA
        logical, intent(out) :: drawn                       !! it has a surface and is drawn
        real(wp), intent(out), optional :: z0, r0, z1, r1   !! scene units
        type(ShadowRegion) :: g
        integer :: k
        k = region_index(region)
        if (k > 0) g = self%region(k)
        drawn = k > 0 .and. g%drawn
        if (present(z0)) z0 = g%z0
        if (present(r0)) r0 = g%r0
        if (present(z1)) z1 = g%z1
        if (present(r1)) r1 = g%r1
    end subroutine region_extent

    !> The distance from the body's center to the umbra's apex at the last
    !> update (huge() for a body no smaller than its light; 0 before).
    pure real(wp) function umbra_length(self)
        class(ShadowCones), intent(in) :: self
        real(wp) :: r, rs, d
        r = self%shaped(1)
        rs = self%shaped(2)
        d = self%shaped(3)
        umbra_length = 0.0_wp
        if (d <= 0.0_wp) return
        if (rs > r) then
            umbra_length = d * r / (rs - r)
        else
            umbra_length = huge(1.0_wp)
        end if
    end function umbra_length

    !> What kind of frame this is: 'ShadowCones'.
    function shadow_frame_info(self) result(info)
        class(ShadowCones), intent(in) :: self
        character(len=:), allocatable :: info
        info = 'ShadowCones'
    end function shadow_frame_info

    !> The drawn regions' bounding sphere (frame coordinates).
    subroutine shadow_bound(self, center, radius)
        class(ShadowCones), intent(in) :: self
        real(wp), intent(out) :: center(3), radius          !! the bounding sphere, frame coordinates (scene units)
        real(wp) :: lo, hi, r
        integer :: k
        center = 0.0_wp
        radius = 0.0_wp
        lo = huge(1.0_wp)
        hi = -huge(1.0_wp)
        r = 0.0_wp
        do k = 1, n_regions
            if (.not. self%region(k)%drawn) cycle
            lo = min(lo, self%region(k)%z0)
            hi = max(hi, self%region(k)%z1)
            r = max(r, self%region(k)%r0, self%region(k)%r1)
        end do
        if (hi < lo) return
        center(3) = 0.5_wp * (lo + hi)
        radius = hypot(0.5_wp * (hi - lo), r)
    end subroutine shadow_bound

    !> A switch with a mesh for each region (vertices written at each change
    !> of shape), under `group`.
    subroutine shadow_build_contents(self, viewer, group)
        class(ShadowCones), intent(inout) :: self
        type(c_ptr), intent(in) :: viewer                   !! a window's viewer (null: headless)
        type(c_ptr), intent(in) :: group                    !! the frame's contents group
        real(c_float), dimension(:,:), pointer :: v, normals, uv
        integer(c_int32_t), dimension(:), pointer :: indices
        integer :: k, i, j, n
        n = n_around
        do k = 1, n_regions
            associate (g => self%region(k))
                g%mesh = vkf_mesh_create(int(2 * n, c_int), int(6 * n, c_int), [1.0_c_float, 1.0_c_float, 1.0_c_float, &
                                         1.0_c_float], c_null_ptr, c_null_ptr, 5_c_int)    ! unlit, transparent; vertices change
                if (.not. c_associated(g%mesh)) cycle
                call mesh_arrays(g%mesh, 2 * n, 6 * n, v, normals, uv, indices)
                v = 0.0_c_float
                normals = 0.0_c_float
                normals(3, :) = 1.0_c_float
                uv = 0.0_c_float
                do i = 1, n                                 ! ring 0: vertices 0..n-1; ring 1: n..2n-1
                    j = modulo(i, n)
                    indices(6*i-5:6*i) = int([i - 1, j, n + i - 1, j, n + j, n + i - 1], c_int32_t)
                end do
                call apply_color(g)
                g%switch = vkf_switch_create()
                call check_status(vkf_node_add_child(g%switch, g%mesh), 'shadow')
                call check_status(vkf_switch_set(g%switch, 0_c_int), 'shadow')      ! (until shaped)
                call check_status(vkf_node_add_child(group, g%switch), 'shadow')
            end associate
        end do
        self%shaped = -1.0_wp                               ! (shaped at the next update)
    end subroutine shadow_build_contents

    !> A region's color, through its mesh's material (the unlit shader
    !> multiplies the vertex color, white, by the diffuse color).
    subroutine apply_color(g)
        type(ShadowRegion), intent(in) :: g
        real(c_float) :: c(4)
        if (.not. c_associated(g%mesh)) return
        c = real(g%color, c_float)
        call check_status(mesh_set_material(g%mesh, c, c, [0.0_c_float, 0.0_c_float, 0.0_c_float, 1.0_c_float], &
                                     [0.0_c_float, 0.0_c_float, 0.0_c_float, 1.0_c_float], 0.0_c_float), 'shadow')
    end subroutine apply_color

    !> After the tree's update for time t: the shadow moved to the body, turned
    !> away from the light, and reshaped if the distance or a setting changed.
    recursive subroutine shadow_late_update(self, t)
        class(ShadowCones), intent(inout) :: self
        real(wp), intent(in) :: t                           !! the scene time
        if (associated(self%body_) .and. associated(self%light_) .and. self%contents_shown()) call place(self)
        call self%ReferenceFrame%late_update(t)
    end subroutine shadow_late_update

    !> Move the shadow to the body's center (in the parent's coordinates) with its
    !> +z axis away from the light, and reshape the cones if the radii, the
    !> distance or the length set changed.
    subroutine place(self)
        type(ShadowCones), intent(inout) :: self
        real(wp) :: to_parent(4,4), m(4,4), b(3), s(3), u(3), d, now(4)
        ! the body's and the light's centers in the parent's coordinates
        to_parent = rigid_inverse(matmul(self%world_matrix(), rigid_inverse(self%local_matrix())))
        m = matmul(to_parent, self%body_%world_matrix())
        b = m(1:3, 4)
        m = matmul(to_parent, self%light_%world_matrix())
        s = m(1:3, 4)
        u = b - s
        d = norm2(u)
        self%distance_ = d
        call self%set_position(b)
        if (d > 0.0_wp) call self%set_attitude(quat_from_vectors([0.0_wp, 0.0_wp, 1.0_wp], u / d))
        now = [self%body_radius(), self%light_radius(), d, self%length_]
        if (all(now == self%shaped)) return
        self%shaped = now
        call shape_regions(self, now(1), now(2), d)
    end subroutine place

    !> The regions' frustums for body radius r, light radius rs and distance d
    !> (see the module documentation), written to their meshes.
    subroutine shape_regions(self, r, rs, d)
        type(ShadowCones), intent(inout) :: self
        real(wp), intent(in) :: r, rs, d                    !! scene units
        real(wp) :: sa, ca, ta, sb, cb, tb, apex, len
        integer :: k
        self%region%drawn = .false.
        self%drawn_length_ = 0.0_wp
        if (r > 0.0_wp .and. rs > 0.0_wp .and. d > r + rs) then
            sa = (rs - r) / d                               ! the umbra's half-angle (sine; < 0: widening)
            ca = sqrt(1.0_wp - sa**2)
            ta = sa / ca
            sb = (rs + r) / d                               ! the penumbra's
            cb = sqrt(1.0_wp - sb**2)
            tb = sb / cb
            apex = huge(1.0_wp)
            if (sa > 0.0_wp) apex = r / sa
            len = self%length_
            if (len <= 0.0_wp) then
                len = 100.0_wp * r
                if (sa > 0.0_wp) len = 2.0_wp * apex
            end if
            self%drawn_length_ = len
            ! the umbra: from the outer tangents' circle on the body to the apex (or len)
            associate (g => self%region(umbra))
                g%z0 = r * sa
                g%r0 = r * ca
                g%z1 = min(apex, len)
                g%r1 = max(0.0_wp, g%r0 - (g%z1 - g%z0) * ta)
                if (g%z1 >= apex) g%r1 = 0.0_wp
                g%drawn = g%z1 > g%z0
            end associate
            ! the antumbra: from the apex to len
            associate (g => self%region(antumbra))
                g%z0 = apex
                g%r0 = 0.0_wp
                g%z1 = len
                g%r1 = (len - apex) * ta
                g%drawn = sa > 0.0_wp .and. len > apex
            end associate
            ! the penumbra: from the inner tangents' circle (on the light's side) to len
            associate (g => self%region(penumbra))
                g%z0 = -r * sb
                g%r0 = r * cb
                g%z1 = len
                g%r1 = g%r0 + (len - g%z0) * tb
                g%drawn = len > g%z0
            end associate
        end if
        self%region(umbra)%drawn = self%region(umbra)%drawn .and. iand(self%regions_, SHADOW_UMBRA) /= 0
        self%region(penumbra)%drawn = self%region(penumbra)%drawn .and. iand(self%regions_, SHADOW_PENUMBRA) /= 0
        self%region(antumbra)%drawn = self%region(antumbra)%drawn .and. iand(self%regions_, SHADOW_ANTUMBRA) /= 0
        do k = 1, n_regions
            call write_region(self%region(k))
        end do
    end subroutine shape_regions

    !> A region's frustum into its mesh (uploaded), and shown or hidden.
    subroutine write_region(g)
        type(ShadowRegion), intent(in) :: g
        real(c_float), dimension(:,:), pointer :: v, normals, uv
        integer(c_int32_t), dimension(:), pointer :: indices
        real(wp) :: a
        integer :: i, n
        if (.not. c_associated(g%mesh)) return
        n = n_around
        if (g%drawn) then
            call mesh_arrays(g%mesh, 2 * n, 6 * n, v, normals, uv, indices)
            do i = 1, n
                a = 2.0_wp * pi * (i - 1) / n
                v(:, i) = real([g%r0 * cos(a), g%r0 * sin(a), g%z0], c_float)
                v(:, n + i) = real([g%r1 * cos(a), g%r1 * sin(a), g%z1], c_float)
            end do
            call check_status(vkf_mesh_vertices_changed(g%mesh), 'shadow')
        end if
        call check_status(vkf_switch_set(g%switch, merge(1_c_int, 0_c_int, g%drawn)), 'shadow')
    end subroutine write_region

end module vkf_shadow
