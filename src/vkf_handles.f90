!> Handles: things in the scene the mouse drags (not in OpenFrames), to
!> design graphically, e.g. a trajectory's initial position and velocity.
!>
!> A DragHandle is a frame with a point the mouse can grab (`handle_point`,
!> in the frame's coordinates) and move (`move_handle_point`); extensions say
!> what the point is and how they are drawn. Windows drive them (WindowProxy,
!> see vkf_window): the handle under the cursor is hovered (drawn brighter); a
!> left button press on it grabs it (the camera stays where it is: the
!> trackball doesn't see the press), the mouse's moves drag it, and the
!> release lets it go. The window's event handler is told (`handle_drag`,
!> with DRAG_START, DRAG_MOVE and DRAG_END), e.g. to compute a trajectory again
!> from the new state while the handle is dragged. Handles can also be moved
!> by the program (VectorHandle%set_vector), and dragged by it (`grab`,
!> `drag` and `release`, given the cursor's ray).
!>
!> Where the point goes is where the cursor's ray (WindowProxy%cursor_ray)
!> meets the handle's constraint (`set_constraint`), as it was when the
!> handle was grabbed, plus the point's offset from the cursor then (so the
!> point doesn't jump to the cursor):
!> * HANDLE_FREE (the default): the plane through the point facing the camera;
!> * HANDLE_PLANE: the plane through the point with the normal given;
!> * HANDLE_LINE: the line through the point along the direction given;
!> * HANDLE_RADIAL: the line from the frame's origin through the point (e.g.
!>   a vector's length only);
!> * HANDLE_SPHERE: the sphere about the frame's origin through the point
!>   (e.g. a vector's direction only), on the side (near or far, from the
!>   camera) where it was grabbed; where the ray misses it, its edge nearest
!>   the ray.
!> Normals and directions are in the frame's coordinates. A ray parallel to a
!> plane or a line doesn't move the point. Positions go through the frames'
!> first parents to the root (ReferenceFrame%world_matrix).
!>
!> A VectorHandle is an arrow from its frame's origin to its tip, the tip
!> being the handle: a vector (`set_vector`, in the frame's coordinates)
!> times a display scale (`set_display_scale`: e.g. 1000 s to draw a velocity
!> in km/s as km). Dragging the tip changes the vector. The arrow is in the
!> frame's color, lit as frames' axes (by how it faces the viewer); its head
!> is `set_head_size` long (scene units; default 0.2 of the arrow, at most
!> half of it). It is built once, as a unit shaft and head scaled and turned
!> by two transforms: a new vector only sets them (cheap enough for every
!> frame of a drag).
module vkf_handles

    use, intrinsic :: iso_c_binding, only: c_ptr, c_null_ptr, c_associated, c_int, c_int32_t, c_float, c_double
    use vkf_math, only: wp, cross
    use vkf_frames, only: ReferenceFrame, PickHit, add_pick_hit, PICK_HANDLE
    use vkf_pick, only: PickCamera, point_pick, segment_pick, wants, ray_plane, ray_sphere, ray_line
    use vkf_geometry, only: arrow_size, arrow
    use vkf_layer, only: check_status, vkf_mesh_create, mesh_arrays, mesh_set_material, vkf_transform_create, &
                         vkf_transform_set_matrix, vkf_switch_create, vkf_switch_set, vkf_node_add_child

    implicit none

    private

    !> A handle's constraint (`set_constraint`; see the module documentation).
    integer, parameter, public :: HANDLE_FREE = 0           !! the plane facing the camera
    integer, parameter, public :: HANDLE_PLANE = 1          !! a plane (its normal given)
    integer, parameter, public :: HANDLE_LINE = 2           !! a line (its direction given)
    integer, parameter, public :: HANDLE_RADIAL = 3         !! the line from the frame's origin through the point
    integer, parameter, public :: HANDLE_SPHERE = 4         !! the sphere about the frame's origin through the point

    !> A handle the mouse drags (see the module documentation): extensions
    !> give its point and draw it.
    type, abstract, extends(ReferenceFrame), public :: DragHandle
        private
        logical :: draggable_ = .true.                      !! the mouse may grab it
        logical :: hovered_ = .false.                       !! under the cursor
        logical :: dragged_ = .false.                       !! grabbed, until released
        integer :: constraint_ = HANDLE_FREE                !! HANDLE_*
        real(wp) :: axis_(3) = [0.0_wp, 0.0_wp, 1.0_wp]     !! the plane's normal or the line's direction (frame coordinates)
        ! the drag, from the grab (frame coordinates)
        real(wp) :: ref_point(3) = 0.0_wp                   !! the point when grabbed
        real(wp) :: ref_axis(3) = 0.0_wp                    !! the plane's normal or the line's direction then
        real(wp) :: radius = 0.0_wp                         !! (HANDLE_SPHERE) the sphere's radius
        logical :: far_side = .false.                       !! (HANDLE_SPHERE) grabbed on the far side
        real(wp) :: offset(3) = 0.0_wp                      !! the point's offset from the cursor's
    contains
        procedure :: set_draggable, draggable
        procedure :: set_constraint, constraint, constraint_axis
        procedure :: set_hovered, hovered, dragged
        procedure :: grab, drag, release
        procedure(handle_point_interface), deferred :: handle_point
        procedure(move_handle_point_interface), deferred :: move_handle_point
        procedure(show_state_interface), deferred :: show_state
    end type DragHandle

    abstract interface

        !> The point the mouse grabs (frame coordinates).
        pure function handle_point_interface(self) result(p)
            import :: DragHandle, wp
            implicit none
            class(DragHandle), intent(in) :: self
            real(wp) :: p(3)
        end function handle_point_interface

        !> Move the point to `p` (frame coordinates), and the drawing with it.
        subroutine move_handle_point_interface(self, p)
            import :: DragHandle, wp
            implicit none
            class(DragHandle), intent(inout) :: self
            real(wp), intent(in) :: p(3)                    !! the new point (frame coordinates)
        end subroutine move_handle_point_interface

        !> Draw the handle as hovered, dragged or neither (`hovered`, `dragged`).
        subroutine show_state_interface(self)
            import :: DragHandle
            implicit none
            class(DragHandle), intent(inout) :: self
        end subroutine show_state_interface

    end interface

    !> An arrow whose tip the mouse drags (see the module documentation).
    type, extends(DragHandle), public :: VectorHandle
        private
        real(wp) :: vector_(3) = [1.0_wp, 0.0_wp, 0.0_wp]   !! the vector (frame coordinates)
        real(wp) :: scale_ = 1.0_wp                         !! drawn as scale * vector (scene units)
        real(wp) :: head_size_ = 0.0_wp                     !! the head's length (scene units; 0: 0.2 of the arrow)
        type(c_ptr) :: switch = c_null_ptr                  !! shows the arrow (not when the vector is 0)
        type(c_ptr) :: shaft = c_null_ptr, head = c_null_ptr  !! the transforms of the unit shaft and head
        type(c_ptr) :: shaft_mesh = c_null_ptr, head_mesh = c_null_ptr  !! the unit shaft and head (colored by their materials)
    contains
        procedure :: set_vector, vector, tip
        procedure :: set_display_scale, display_scale
        procedure :: set_head_size, head_size
        procedure :: set_color => vector_set_color
        procedure :: handle_point => vector_handle_point
        procedure :: move_handle_point => vector_move_handle_point
        procedure :: show_state => vector_show_state
        procedure :: frame_info => vector_frame_info
        procedure :: bound => vector_bound
        procedure :: pick => vector_pick
        procedure :: build_contents => vector_build_contents
    end type VectorHandle

    interface VectorHandle
        module procedure new_vector_handle
    end interface VectorHandle

    public :: handles_shown

    integer, save :: n_shown = 0                            !! handles shown in a window (so far)

contains

    ! ==================================================================
    ! DragHandle

    !> Whether the mouse may grab the handle (default true).
    subroutine set_draggable(self, draggable)
        class(DragHandle), intent(inout) :: self
        logical, intent(in) :: draggable                    !! true: the mouse may grab it
        self%draggable_ = draggable
        if (.not. draggable .and. self%hovered_) call self%set_hovered(.false.)
    end subroutine set_draggable

    !> Whether the mouse may grab the handle.
    pure logical function draggable(self)
        class(DragHandle), intent(in) :: self
        draggable = self%draggable_
    end function draggable

    !> Where the handle can be dragged (HANDLE_*; see the module documentation),
    !> with `axis`: the plane's normal (HANDLE_PLANE) or the line's direction
    !> (HANDLE_LINE), in the frame's coordinates. Taken when the handle is
    !> next grabbed.
    subroutine set_constraint(self, constraint, axis)
        class(DragHandle), intent(inout) :: self
        integer, intent(in) :: constraint                   !! HANDLE_*
        real(wp), intent(in), optional :: axis(3)           !! (frame coordinates; any length but 0)
        if (constraint < HANDLE_FREE .or. constraint > HANDLE_SPHERE) return
        self%constraint_ = constraint
        if (present(axis)) then
            if (norm2(axis) > 0.0_wp) self%axis_ = axis / norm2(axis)
        end if
    end subroutine set_constraint

    !> The handle's constraint (HANDLE_*).
    pure integer function constraint(self)
        class(DragHandle), intent(in) :: self
        constraint = self%constraint_
    end function constraint

    !> The constraint's normal or direction (unit vector, frame coordinates).
    pure function constraint_axis(self) result(axis)
        class(DragHandle), intent(in) :: self
        real(wp) :: axis(3)
        axis = self%axis_
    end function constraint_axis

    !> The handle is under the cursor (set by the window), or not.
    subroutine set_hovered(self, hovered)
        class(DragHandle), intent(inout) :: self
        logical, intent(in) :: hovered                      !! true: under the cursor
        if (hovered .eqv. self%hovered_) return
        self%hovered_ = hovered
        call self%show_state()
    end subroutine set_hovered

    !> The handle is under the cursor.
    pure logical function hovered(self)
        class(DragHandle), intent(in) :: self
        hovered = self%hovered_
    end function hovered

    !> The handle is grabbed (until released).
    pure logical function dragged(self)
        class(DragHandle), intent(in) :: self
        dragged = self%dragged_
    end function dragged

    !> Grab the handle with the cursor's ray (origin, unit dir; root frame):
    !> its constraint is taken as it is now, and the point's offset from where
    !> the ray meets it. False if it isn't draggable.
    logical function grab(self, origin, dir) result(ok)
        class(DragHandle), intent(inout) :: self
        real(wp), intent(in) :: origin(3), dir(3)           !! the cursor's ray (root frame)
        real(wp) :: m(4,4), p(3), h(3), near, far, c(3)
        ok = .false.
        if (.not. self%draggable_) return
        m = self%world_matrix()
        p = self%handle_point()
        self%ref_point = p
        self%ref_axis = self%axis_
        select case (self%constraint_)
        case (HANDLE_FREE)
            self%ref_axis = matmul(transpose(m(1:3, 1:3)), -dir)  ! (facing the camera, now)
        case (HANDLE_RADIAL)
            if (norm2(p) > 0.0_wp) self%ref_axis = p / norm2(p)
        case (HANDLE_SPHERE)
            self%radius = norm2(p)
            self%far_side = .false.
            c = m(1:3, 4)
            if (ray_sphere(origin, dir, c, self%radius, near, far)) &   ! (the side nearer the point)
                self%far_side = norm2(origin + far * dir - (matmul(m(1:3, 1:3), p) + c)) < &
                                norm2(origin + near * dir - (matmul(m(1:3, 1:3), p) + c))
        end select
        self%offset = 0.0_wp
        if (constrained(self, origin, dir, m, h)) self%offset = p - h
        self%dragged_ = .true.
        call self%show_state()
        ok = .true.
    end function grab

    !> Drag the grabbed handle to the cursor's ray (origin, unit dir; root
    !> frame), held to its constraint. False (and not moved) if it isn't
    !> grabbed or the ray doesn't meet the constraint.
    logical function drag(self, origin, dir) result(moved)
        class(DragHandle), intent(inout) :: self
        real(wp), intent(in) :: origin(3), dir(3)           !! the cursor's ray (root frame)
        real(wp) :: h(3), p(3)
        moved = .false.
        if (.not. self%dragged_) return
        if (.not. constrained(self, origin, dir, self%world_matrix(), h)) return
        p = h + self%offset
        if (self%constraint_ == HANDLE_SPHERE .and. norm2(p) > 0.0_wp) p = self%radius * p / norm2(p)
        call self%move_handle_point(p)
        moved = .true.
    end function drag

    !> Let the handle go.
    subroutine release(self)
        class(DragHandle), intent(inout) :: self
        if (.not. self%dragged_) return
        self%dragged_ = .false.
        call self%show_state()
    end subroutine release

    !> Where the ray (origin, unit dir; root frame) meets the constraint taken
    !> at the grab, the frame at transform `m` to the root: `h`, in the frame's
    !> coordinates. False if it doesn't.
    logical function constrained(self, origin, dir, m, h) result(ok)
        class(DragHandle), intent(in) :: self
        real(wp), intent(in) :: origin(3)                   !! the cursor's ray: its origin (root frame)
        real(wp), intent(in) :: dir(3)                      !! its direction (a unit vector)
        real(wp), intent(in) :: m(4,4)                      !! the frame's transform to the root
        real(wp), intent(out) :: h(3)                       !! where it meets the constraint (frame coordinates)
        real(wp) :: p(3), a(3), c(3), w(3), q(3), t, near, far
        c = m(1:3, 4)
        p = matmul(m(1:3, 1:3), self%ref_point) + c
        a = matmul(m(1:3, 1:3), self%ref_axis)
        h = self%ref_point
        select case (self%constraint_)
        case (HANDLE_FREE, HANDLE_PLANE)
            ok = ray_plane(origin, dir, p, a, t, w)
        case (HANDLE_LINE, HANDLE_RADIAL)
            ok = ray_line(origin, dir, p, a, t, w)
        case (HANDLE_SPHERE)
            ok = self%radius > 0.0_wp
            if (.not. ok) return
            if (ray_sphere(origin, dir, c, self%radius, near, far)) then
                t = near
                if (self%far_side .or. near < 0.0_wp) t = far
                w = origin + t * dir
            else                                            ! (missed: the edge nearest the ray)
                q = origin + max(0.0_wp, dot_product(c - origin, dir)) * dir
                ok = norm2(q - c) > 0.0_wp
                if (.not. ok) return
                w = c + self%radius * (q - c) / norm2(q - c)
            end if
        case default
            ok = .false.
        end select
        if (ok) h = matmul(transpose(m(1:3, 1:3)), w - c)
    end function constrained

    ! ==================================================================
    ! VectorHandle

    !> A vector handle (see the module documentation): `vector` in its frame's
    !> coordinates (default [1, 0, 0]), drawn `scale` times as long (default 1),
    !> in `color` ([r, g, b] or [r, g, b, a]). Its axes and labels are hidden.
    function new_vector_handle(name, vector, color, scale) result(h)
        character(len=*), intent(in) :: name                !! the frame's name
        real(wp), intent(in), optional :: vector(3)         !! frame coordinates
        real(wp), intent(in), optional :: color(:)          !! [r, g, b] or [r, g, b, a] (0 to 1)
        real(wp), intent(in), optional :: scale             !! display scale (scene units per vector unit; > 0)
        type(VectorHandle) :: h
        h%ReferenceFrame = ReferenceFrame(name, color)
        call h%show_decorations(.false.)
        if (present(vector)) h%vector_ = vector
        if (present(scale)) then
            if (scale > 0.0_wp) h%scale_ = scale
        end if
    end function new_vector_handle

    !> The vector (frame coordinates); can change at any time.
    subroutine set_vector(self, vector)
        class(VectorHandle), intent(inout) :: self
        real(wp), intent(in) :: vector(3)                   !! frame coordinates
        self%vector_ = vector
        call update_arrow(self)
    end subroutine set_vector

    !> The vector (frame coordinates).
    pure function vector(self) result(v)
        class(VectorHandle), intent(in) :: self
        real(wp) :: v(3)
        v = self%vector_
    end function vector

    !> The arrow's tip: the display scale times the vector (frame coordinates).
    pure function tip(self) result(p)
        class(VectorHandle), intent(in) :: self
        real(wp) :: p(3)
        p = self%scale_ * self%vector_
    end function tip

    !> Scene units per unit of the vector (> 0; default 1): the arrow is
    !> `scale` * vector.
    subroutine set_display_scale(self, scale)
        class(VectorHandle), intent(inout) :: self
        real(wp), intent(in) :: scale                       !! scene units per vector unit (> 0; else ignored)
        if (scale <= 0.0_wp) return
        self%scale_ = scale
        call update_arrow(self)
    end subroutine set_display_scale

    !> The display scale.
    pure real(wp) function display_scale(self)
        class(VectorHandle), intent(in) :: self
        display_scale = self%scale_
    end function display_scale

    !> The head's length (scene units; at most half the arrow); 0 (the
    !> default): 0.2 of the arrow's length.
    subroutine set_head_size(self, size)
        class(VectorHandle), intent(inout) :: self
        real(wp), intent(in) :: size                        !! the head's length (scene units; 0: 0.2 of the arrow)
        self%head_size_ = max(0.0_wp, size)
        call update_arrow(self)
    end subroutine set_head_size

    !> The head's length set (0: 0.2 of the arrow's).
    pure real(wp) function head_size(self)
        class(VectorHandle), intent(in) :: self
        head_size = self%head_size_
    end function head_size

    !> The arrow's color; [r, g, b] or [r, g, b, a].
    subroutine vector_set_color(self, color)
        class(VectorHandle), intent(inout) :: self
        real(wp), intent(in) :: color(:)                    !! [r, g, b] or [r, g, b, a] (0 to 1)
        call self%ReferenceFrame%set_color(color)
        call self%show_state()
    end subroutine vector_set_color

    !> The point the mouse grabs: the tip.
    pure function vector_handle_point(self) result(p)
        class(VectorHandle), intent(in) :: self
        real(wp) :: p(3)
        p = self%tip()
    end function vector_handle_point

    !> The tip dragged to `p`: the vector p / the display scale.
    subroutine vector_move_handle_point(self, p)
        class(VectorHandle), intent(inout) :: self
        real(wp), intent(in) :: p(3)                        !! the tip's new place (frame coordinates)
        call self%set_vector(p / self%scale_)
    end subroutine vector_move_handle_point

    !> The arrow's color, brighter when hovered and more when dragged.
    subroutine vector_show_state(self)
        class(VectorHandle), intent(inout) :: self
        real(c_float) :: color(4), glow(4)
        if (.not. c_associated(self%shaft_mesh)) return
        color = real(self%color(), c_float)
        glow = 0.0_c_float
        if (self%dragged()) then
            glow(1:3) = 0.45_c_float
        else if (self%hovered()) then
            glow(1:3) = 0.25_c_float
        end if
        call check_status(mesh_set_material(self%shaft_mesh, [1.0_c_float, 1.0_c_float, 1.0_c_float, 1.0_c_float], color, &
                                            [0.0_c_float, 0.0_c_float, 0.0_c_float, 1.0_c_float], glow, 0.0_c_float))
        call check_status(mesh_set_material(self%head_mesh, [1.0_c_float, 1.0_c_float, 1.0_c_float, 1.0_c_float], color, &
                                            [0.0_c_float, 0.0_c_float, 0.0_c_float, 1.0_c_float], glow, 0.0_c_float))
    end subroutine vector_show_state

    !> What kind of frame this is: 'VectorHandle'.
    function vector_frame_info(self) result(info)
        class(VectorHandle), intent(in) :: self
        character(len=:), allocatable :: info
        info = 'VectorHandle'
    end function vector_frame_info

    !> The arrow's bounding sphere (frame coordinates).
    subroutine vector_bound(self, center, radius)
        class(VectorHandle), intent(in) :: self
        real(wp), intent(out) :: center(3)                  !! the bound's center (frame coordinates)
        real(wp), intent(out) :: radius                     !! its radius (scene units)
        center = 0.5_wp * self%tip()
        radius = 0.5_wp * norm2(self%tip())
    end subroutine vector_bound

    !> Picked at its tip and along its head (within the tolerance, plus a few
    !> pixels), as PICK_HANDLE; else the frame's origin.
    subroutine vector_pick(self, cam, to_root, hits, n)
        class(VectorHandle), intent(in), target :: self
        type(PickCamera), intent(in) :: cam                 !! the camera and cursor of the pick
        real(wp), intent(in) :: to_root(4,4)                !! the frame's transform to the root frame
        type(PickHit), allocatable, intent(inout) :: hits(:)  !! hits so far, in hits(1:n); added to
        integer, intent(inout) :: n                         !! number of hits
        type(PickHit) :: h
        real(wp) :: p(3), base(3), len, head_len, radius, pixels, depth, s, head_pixels, head_depth
        logical :: at_tip, on_head
        call self%ReferenceFrame%pick(cam, to_root, hits, n)
        if (.not. (self%draggable() .and. self%contents_shown() .and. wants(cam, PICK_HANDLE))) return
        call arrow_sizes(self, len, head_len, radius)
        if (len <= 0.0_wp) return
        p = matmul(to_root(1:3, 1:3), self%tip()) + to_root(1:3, 4)
        base = matmul(to_root(1:3, 1:3), self%tip() * (1.0_wp - head_len / len)) + to_root(1:3, 4)
        at_tip = point_pick(cam, p, pixels, depth, extra=6.0_wp * cam%pixel_ratio)
        on_head = segment_pick(cam, base, p, head_pixels, s, head_depth, extra=4.0_wp * cam%pixel_ratio)
        if (.not. (at_tip .or. on_head)) return
        if (on_head .and. (.not. at_tip .or. head_pixels < pixels)) then
            pixels = head_pixels
            depth = head_depth
        end if
        h%kind = PICK_HANDLE
        h%frame => self
        h%position = p
        h%local = self%tip()
        h%distance = depth
        h%pixels = pixels
        call add_pick_hit(hits, n, h)
    end subroutine vector_pick

    !> The arrow: a unit shaft and head under their transforms, in a switch.
    subroutine vector_build_contents(self, viewer, group)
        class(VectorHandle), intent(inout) :: self
        type(c_ptr), intent(in) :: viewer                   !! a window's viewer (null: headless)
        type(c_ptr), intent(in) :: group                    !! the frame's contents group
        self%shaft_mesh = unit_part(1.0_c_double, 0.0_c_double, 1.0_c_double, 1.0_c_double)   ! (a closed cylinder)
        self%head_mesh = unit_part(0.0_c_double, 1.0_c_double, 0.0_c_double, 1.0_c_double)    ! (a cone on a disk)
        if (.not. (c_associated(self%shaft_mesh) .and. c_associated(self%head_mesh))) then
            write(*, '(a)') 'vkframes: VectorHandle '//self%name()//': could not build its arrow'
            return
        end if
        self%switch = vkf_switch_create()
        self%shaft = vkf_transform_create()
        self%head = vkf_transform_create()
        call check_status(vkf_node_add_child(self%shaft, self%shaft_mesh))
        call check_status(vkf_node_add_child(self%head, self%head_mesh))
        call check_status(vkf_node_add_child(self%switch, self%shaft))
        call check_status(vkf_node_add_child(self%switch, self%head))
        call check_status(vkf_node_add_child(group, self%switch))
        call self%show_state()
        call update_arrow(self)
        if (c_associated(viewer)) n_shown = n_shown + 1
    end subroutine vector_build_contents

    !> A unit part of an arrow along z from the origin (vkf_geometry's arrow:
    !> body and head lengths and radii), white (colored by its material), lit
    !> as frames' axes. Null on failure.
    function unit_part(body_length, head_length, body_radius, head_radius) result(mesh)
        real(c_double), intent(in) :: body_length           !! the shaft's length (0: none)
        real(c_double), intent(in) :: head_length           !! the head's length (0: a flat top)
        real(c_double), intent(in) :: body_radius           !! the shaft's radius
        real(c_double), intent(in) :: head_radius           !! the head's radius at its base
        type(c_ptr) :: mesh
        integer, parameter :: segments = 24
        real(c_float), dimension(:,:), pointer :: xyz, normals, uv
        integer(c_int32_t), dimension(:), pointer :: indices
        integer :: n_vertices, n_indices
        call arrow_size(segments, n_vertices, n_indices)
        mesh = vkf_mesh_create(int(n_vertices, c_int), int(n_indices, c_int), &
                               [1.0_c_float, 1.0_c_float, 1.0_c_float, 1.0_c_float], c_null_ptr, c_null_ptr, 2_c_int)
        if (.not. c_associated(mesh)) return
        call mesh_arrays(mesh, n_vertices, n_indices, xyz, normals, uv, indices)
        call arrow([0.0_c_double, 0.0_c_double, 0.0_c_double], [0.0_c_double, 0.0_c_double, 1.0_c_double], &
                   body_length, head_length, body_radius, head_radius, segments, xyz, normals, uv, indices)
    end function unit_part

    !> The arrow's length, its head's length and radius (scene units).
    pure subroutine arrow_sizes(self, len, head_len, head_radius)
        class(VectorHandle), intent(in) :: self
        real(wp), intent(out) :: len                        !! the arrow's length (the tip's distance)
        real(wp), intent(out) :: head_len                   !! its head's length
        real(wp), intent(out) :: head_radius                !! its head's radius
        len = norm2(self%tip())
        head_len = 0.2_wp * len
        if (self%head_size_ > 0.0_wp) head_len = min(self%head_size_, 0.5_wp * len)
        head_radius = 0.35_wp * head_len
    end subroutine arrow_sizes

    !> The shaft and head placed for the vector (hidden if it is 0).
    subroutine update_arrow(self)
        class(VectorHandle), intent(inout) :: self
        real(wp) :: len, head_len, head_radius, u(3), a(3), b(3), m(4,4)
        if (.not. c_associated(self%switch)) return
        call arrow_sizes(self, len, head_len, head_radius)
        call check_status(vkf_switch_set(self%switch, merge(1_c_int, 0_c_int, len > 0.0_wp)))
        if (len <= 0.0_wp) return
        u = self%tip() / len
        a = cross(u, [1.0_wp, 0.0_wp, 0.0_wp])
        if (norm2(a) < 1.0e-6_wp) a = cross(u, [0.0_wp, 1.0_wp, 0.0_wp])
        a = a / norm2(a)
        b = cross(u, a)                                     ! (a, b, u right-handed)
        m = 0.0_wp
        m(4, 4) = 1.0_wp
        m(1:3, 1) = 0.4_wp * head_radius * a                ! the shaft: 0.4 of the head's radius
        m(1:3, 2) = 0.4_wp * head_radius * b
        m(1:3, 3) = (len - head_len) * u
        call check_status(vkf_transform_set_matrix(self%shaft, m))
        m(1:3, 1) = head_radius * a
        m(1:3, 2) = head_radius * b
        m(1:3, 3) = head_len * u
        m(1:3, 4) = (len - head_len) * u
        call check_status(vkf_transform_set_matrix(self%head, m))
    end subroutine update_arrow

    !> Whether any handle has been shown in a window (so windows look for
    !> handles under the cursor).
    logical function handles_shown()
        handles_shown = n_shown > 0
    end function handles_shown

end module vkf_handles
