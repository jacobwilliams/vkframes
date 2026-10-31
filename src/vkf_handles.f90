!> Handles (not in OpenFrames): things in the scene the mouse drags, to
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
!> Handles on the same spot of the screen (e.g. a trajectory's end marker on
!> its state's arrow): the window hovers the one nearest the cursor on the
!> screen; within a few pixels of each other, the one of the highest
!> priority (`set_priority`; markers, TimeHandle, 1 by default, before
!> arrows, 0), then the nearest to the eye.
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
!>
!> A TimeHandle is a marker at a time along a path (a TimePath: a position
!> for any time, in the coordinates of the handle's parent frame, e.g. a
!> propagator's), dragged along the path to change the time: e.g. a
!> trajectory's end, dragged to make it longer or shorter in time. The path
!> needn't be drawn where the handle goes (beyond a trajectory's last point,
!> the program propagates further). The frame is placed at the path's
!> position for the time (`set_time`), so its name label (e.g. the time,
!> `set_name`) goes with it; the marker is the size of MarkerArtist's
!> markers (window units, not shrinking with distance). A drag takes the time
!> within `set_time_range` whose point is nearest the cursor's ray (as seen
!> from the eye): among the path's points sampled at the grab, then refined
!> between them; where the path passes near itself (orbits), the match
!> nearest in time to the handle's time is taken, so it doesn't jump to
!> another revolution. A TrajectoryPath follows a Trajectory's points
!> (interpolated linearly), within its times.
module vkf_handles

    use, intrinsic :: iso_c_binding, only: c_ptr, c_null_ptr, c_associated, c_int, c_int32_t, c_float, c_double
    use vkf_math, only: wp, cross, rigid_inverse
    use vkf_frames, only: ReferenceFrame, PickHit, add_pick_hit, PICK_HANDLE
    use vkf_pick, only: PickCamera, point_pick, segment_pick, wants, ray_plane, ray_sphere, ray_line
    use vkf_geometry, only: arrow_size, arrow
    use vkf_trajectory, only: Trajectory
    use vkf_artists, only: MARKER_DISK, MARKER_IMAGE
    use vkf_layer, only: check_status, vkf_mesh_create, mesh_arrays, mesh_set_material, vkf_transform_create, &
                         vkf_transform_set_matrix, vkf_switch_create, vkf_switch_set, vkf_node_add_child, &
                         vkf_marker_create, marker_set_style, line_set_points

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
        integer :: priority_ = 0                            !! over other handles on the same spot (higher first)
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
        procedure :: set_priority, priority
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

    !> A path for a TimeHandle (see the module documentation): a position for
    !> any time.
    type, abstract, public :: TimePath
    contains
        procedure(path_position_interface), deferred :: position
    end type TimePath

    abstract interface

        !> The position at time `t` (the handle's parent's coordinates).
        function path_position_interface(self, t) result(p)
            import :: TimePath, wp
            implicit none
            class(TimePath), intent(in) :: self
            real(wp), intent(in) :: t                       !! the time
            real(wp) :: p(3)
        end function path_position_interface

    end interface

    !> A TimePath along a Trajectory's points, interpolated linearly (before
    !> its first time: its first point; after its last: its last).
    type, extends(TimePath), public :: TrajectoryPath
        type(Trajectory), pointer :: trajectory => null()   !! the trajectory followed (not owned)
    contains
        procedure :: position => trajectory_path_position
    end type TrajectoryPath

    interface TrajectoryPath
        module procedure new_trajectory_path
    end interface TrajectoryPath

    !> A marker dragged along a path to change its time (see the module
    !> documentation).
    type, extends(DragHandle), public :: TimeHandle
        private
        class(TimePath), pointer :: path_ => null()         !! the path (not owned)
        real(wp) :: time_ = 0.0_wp                          !! the time (its point: the frame's position)
        real(wp) :: t_min = 0.0_wp, t_max = 1.0_wp          !! the times a drag may reach
        real(wp) :: size_ = 14.0_wp                         !! the marker's size (window units)
        integer :: shape_ = MARKER_DISK                     !! a MARKER_* shape
        type(c_ptr) :: marker = c_null_ptr                  !! the marker node (at the frame's origin)
        ! the drag: the path sampled at the grab (times; positions, the parent's coordinates)
        real(wp), allocatable :: sample_t(:), sample_p(:,:)
    contains
        procedure :: set_path, set_time, time, set_time_range, time_range
        procedure :: set_marker_size, marker_size, set_marker_shape, marker_shape
        procedure :: set_color => time_set_color
        procedure :: grab => time_grab
        procedure :: drag => time_drag
        procedure :: handle_point => time_handle_point
        procedure :: move_handle_point => time_move_handle_point
        procedure :: show_state => time_show_state
        procedure :: frame_info => time_frame_info
        procedure :: bound => time_bound
        procedure :: pick => time_pick
        procedure :: build_contents => time_build_contents
    end type TimeHandle

    interface TimeHandle
        module procedure new_time_handle
    end interface TimeHandle

    integer, parameter :: path_samples = 600                !! a TimeHandle's path sampled at a grab

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

    !> The handle's priority over others on the same spot of the screen (higher
    !> first; default 0, a TimeHandle's 1; see the module documentation).
    subroutine set_priority(self, priority)
        class(DragHandle), intent(inout) :: self
        integer, intent(in) :: priority                     !! higher: hovered first
        self%priority_ = priority
    end subroutine set_priority

    !> The handle's priority over others on the same spot.
    pure integer function priority(self)
        class(DragHandle), intent(in) :: self
        priority = self%priority_
    end function priority

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

    ! ==================================================================
    ! TrajectoryPath

    !> A path along `traj` (which must stay alive while used).
    function new_trajectory_path(traj) result(path)
        type(Trajectory), intent(in), target :: traj        !! the trajectory followed
        type(TrajectoryPath) :: path
        path%trajectory => traj
    end function new_trajectory_path

    !> The trajectory's position at time `t` (Trajectory%point_at: linear
    !> between its points, its first or last point outside its times; 0 if
    !> it has none).
    function trajectory_path_position(self, t) result(p)
        class(TrajectoryPath), intent(in) :: self
        real(wp), intent(in) :: t                           !! the time
        real(wp) :: p(3)
        p = 0.0_wp
        if (associated(self%trajectory)) p = self%trajectory%point_at(t)
    end function trajectory_path_position

    ! ==================================================================
    ! TimeHandle

    !> A time handle (see the module documentation) on `path` (which must stay
    !> alive while used), at time `time` (default 0) within `t_min` to `t_max`
    !> (default: the time), in `color` ([r, g, b] or [r, g, b, a]). Its axes
    !> and labels are hidden (its name label can be shown: e.g. the time).
    function new_time_handle(name, path, time, t_min, t_max, color) result(h)
        character(len=*), intent(in) :: name                !! the frame's name
        class(TimePath), intent(in), target :: path         !! the path it moves along
        real(wp), intent(in), optional :: time              !! its time
        real(wp), intent(in), optional :: t_min, t_max      !! the times a drag may reach
        real(wp), intent(in), optional :: color(:)          !! [r, g, b] or [r, g, b, a] (0 to 1)
        type(TimeHandle) :: h
        h%ReferenceFrame = ReferenceFrame(name, color)
        call h%show_decorations(.false.)
        h%priority_ = 1                                     ! (a marker over an arrow on the same spot)
        h%path_ => path
        if (present(time)) h%time_ = time
        h%t_min = h%time_
        h%t_max = h%time_
        if (present(t_min)) h%t_min = t_min
        if (present(t_max)) h%t_max = t_max
        call h%set_time(h%time_)
    end function new_time_handle

    !> The path the handle moves along (which must stay alive while used); its
    !> time kept (the frame placed on the new path).
    subroutine set_path(self, path)
        class(TimeHandle), intent(inout) :: self
        class(TimePath), intent(in), target :: path         !! the path
        self%path_ => path
        call self%set_time(self%time_)
    end subroutine set_path

    !> The handle's time (kept within its time range): the frame placed at the
    !> path's point for it.
    subroutine set_time(self, time)
        class(TimeHandle), intent(inout) :: self
        real(wp), intent(in) :: time                        !! the time
        self%time_ = max(self%t_min, min(self%t_max, time))
        if (associated(self%path_)) call self%set_position(self%path_%position(self%time_))
    end subroutine set_time

    !> The handle's time.
    pure real(wp) function time(self)
        class(TimeHandle), intent(in) :: self
        time = self%time_
    end function time

    !> The times a drag may reach (t_min <= t_max; else swapped); the handle's
    !> time brought within them. Taken at the next grab.
    subroutine set_time_range(self, t_min, t_max)
        class(TimeHandle), intent(inout) :: self
        real(wp), intent(in) :: t_min, t_max                !! the earliest and latest times
        self%t_min = min(t_min, t_max)
        self%t_max = max(t_min, t_max)
        call self%set_time(self%time_)
    end subroutine set_time_range

    !> The times a drag may reach.
    pure subroutine time_range(self, t_min, t_max)
        class(TimeHandle), intent(in) :: self
        real(wp), intent(out) :: t_min, t_max               !! the earliest and latest times
        t_min = self%t_min
        t_max = self%t_max
    end subroutine time_range

    !> The marker's size (window units, > 0; default 14).
    subroutine set_marker_size(self, size)
        class(TimeHandle), intent(inout) :: self
        real(wp), intent(in) :: size                        !! window units
        if (size <= 0.0_wp) return
        self%size_ = size
        call self%show_state()
    end subroutine set_marker_size

    !> The marker's size (window units).
    pure real(wp) function marker_size(self)
        class(TimeHandle), intent(in) :: self
        marker_size = self%size_
    end function marker_size

    !> The marker's shape (a MARKER_* shape of vkf_artists but MARKER_IMAGE;
    !> default MARKER_DISK).
    subroutine set_marker_shape(self, shape)
        class(TimeHandle), intent(inout) :: self
        integer, intent(in) :: shape                        !! a MARKER_* shape
        self%shape_ = max(MARKER_DISK, min(MARKER_IMAGE - 1, shape))
        call self%show_state()
    end subroutine set_marker_shape

    !> The marker's shape (MARKER_*).
    pure integer function marker_shape(self)
        class(TimeHandle), intent(in) :: self
        marker_shape = self%shape_
    end function marker_shape

    !> The marker's color; [r, g, b] or [r, g, b, a].
    subroutine time_set_color(self, color)
        class(TimeHandle), intent(inout) :: self
        real(wp), intent(in) :: color(:)                    !! [r, g, b] or [r, g, b, a] (0 to 1)
        call self%ReferenceFrame%set_color(color)
        call self%show_state()
    end subroutine time_set_color

    !> Grab the handle with the cursor's ray (origin, unit dir; root frame):
    !> the path sampled over the time range. False if it isn't draggable or
    !> has no path.
    logical function time_grab(self, origin, dir) result(ok)
        class(TimeHandle), intent(inout) :: self
        real(wp), intent(in) :: origin(3), dir(3)           !! the cursor's ray (root frame)
        integer :: k
        ok = .false.
        if (.not. (self%draggable() .and. associated(self%path_))) return
        if (allocated(self%sample_t)) deallocate(self%sample_t, self%sample_p)
        allocate(self%sample_t(path_samples), self%sample_p(3, path_samples))
        do k = 1, path_samples
            self%sample_t(k) = self%t_min + (self%t_max - self%t_min) * (k - 1) / (path_samples - 1)
            self%sample_p(:, k) = self%path_%position(self%sample_t(k))
        end do
        self%dragged_ = .true.
        call self%show_state()
        ok = .true.
    end function time_grab

    !> Drag the grabbed handle to the cursor's ray (origin, unit dir; root
    !> frame): its time where the path is nearest the ray (see the module
    !> documentation). False (not moved) if it isn't grabbed or the path is
    !> all behind the eye.
    logical function time_drag(self, origin, dir) result(moved)
        class(TimeHandle), intent(inout) :: self
        real(wp), intent(in) :: origin(3), dir(3)           !! the cursor's ray (root frame)
        real(wp) :: parent(4,4), miss(path_samples), best, t_best
        real(wp) :: t_min(path_samples), m_min(path_samples)  !! the local minima refined: their times and misses
        integer :: k, n_min
        logical :: found
        moved = .false.
        if (.not. (self%dragged_ .and. allocated(self%sample_t) .and. associated(self%path_))) return
        parent = matmul(self%world_matrix(), rigid_inverse(self%local_matrix()))   ! (the path's coordinates to the root)
        do k = 1, path_samples
            miss(k) = ray_miss(self%sample_p(:, k))
        end do
        if (minval(miss) >= huge(1.0_wp)) return
        ! each local minimum of the samples refined; of those nearly as near as
        ! the nearest (a path passing the same place again: orbits), the one
        ! nearest in time to the handle's
        n_min = 0
        do k = 1, path_samples
            if (.not. is_minimum(k)) cycle
            n_min = n_min + 1
            call refine(k, t_min(n_min), m_min(n_min))
        end do
        if (n_min == 0) return
        best = minval(m_min(1:n_min))
        found = .false.
        t_best = self%time_
        do k = 1, n_min
            if (m_min(k) > 1.5_wp * best + 1.0e-4_wp) cycle
            if (found .and. abs(t_min(k) - self%time_) >= abs(t_best - self%time_)) cycle
            t_best = t_min(k)
            found = .true.
        end do
        if (.not. found) return
        call self%set_time(t_best)
        moved = .true.

    contains

        !> How far point p (the path's coordinates) is from the ray, as seen
        !> from its origin (the tangent of the angle; huge() behind it).
        real(wp) function ray_miss(p)
            real(wp), intent(in) :: p(3)                    !! the point (the path's coordinates)
            real(wp) :: v(3), along
            v = matmul(parent(1:3, 1:3), p) + parent(1:3, 4) - origin
            along = dot_product(v, dir)
            ray_miss = huge(1.0_wp)
            if (along > 0.0_wp) ray_miss = norm2(v - along * dir) / along
        end function ray_miss

        !> Sample k is a local minimum of the samples' misses (in front of the eye).
        logical function is_minimum(j)
            integer, intent(in) :: j                        !! the sample
            is_minimum = miss(j) < huge(1.0_wp)
            if (j > 1) is_minimum = is_minimum .and. miss(j - 1) >= miss(j)
            if (j < path_samples) is_minimum = is_minimum .and. miss(j + 1) > miss(j)
        end function is_minimum

        !> The time of the least miss between sample j's neighbours (golden-section
        !> search), and that miss.
        subroutine refine(j, t, m)
            integer, intent(in) :: j                        !! the sample
            real(wp), intent(out) :: t                      !! the time found
            real(wp), intent(out) :: m                      !! its miss
            real(wp), parameter :: golden = 0.5_wp * (sqrt(5.0_wp) - 1.0_wp)
            real(wp) :: a, b, c, d, fc, fd
            integer :: it
            a = self%sample_t(max(1, j - 1))
            b = self%sample_t(min(path_samples, j + 1))
            c = b - golden * (b - a)
            d = a + golden * (b - a)
            fc = ray_miss(self%path_%position(c))
            fd = ray_miss(self%path_%position(d))
            do it = 1, 40
                if (fc < fd) then
                    b = d
                    d = c
                    fd = fc
                    c = b - golden * (b - a)
                    fc = ray_miss(self%path_%position(c))
                else
                    a = c
                    c = d
                    fc = fd
                    d = a + golden * (b - a)
                    fd = ray_miss(self%path_%position(d))
                end if
            end do
            t = 0.5_wp * (a + b)
            m = min(fc, fd)
        end subroutine refine

    end function time_drag

    !> The point the mouse grabs: the marker, at the frame's origin.
    pure function time_handle_point(self) result(p)
        class(TimeHandle), intent(in) :: self
        real(wp) :: p(3)
        p = 0.0_wp
    end function time_handle_point

    !> The handle moved to the sampled point of the path nearest `p` (frame
    !> coordinates; after a grab: the path's points are sampled then).
    subroutine time_move_handle_point(self, p)
        class(TimeHandle), intent(inout) :: self
        real(wp), intent(in) :: p(3)                        !! the point (frame coordinates)
        real(wp) :: q(3)
        integer :: k
        if (.not. allocated(self%sample_t)) return
        q = p + self%position()                             ! (the parent's coordinates)
        k = minloc(norm2(self%sample_p - spread(q, 2, size(self%sample_t)), dim=1), dim=1)
        call self%set_time(self%sample_t(k))
    end subroutine time_move_handle_point

    !> The marker's color and size: brighter and larger when hovered, more
    !> when dragged.
    subroutine time_show_state(self)
        class(TimeHandle), intent(inout) :: self
        real(wp) :: rgba(4), size, light
        if (.not. c_associated(self%marker)) return
        rgba = self%color()
        size = self%size_
        light = 0.0_wp
        if (self%dragged()) then
            light = 0.6_wp
            size = 1.4_wp * size
        else if (self%hovered()) then
            light = 0.35_wp
            size = 1.2_wp * size
        end if
        rgba(1:3) = rgba(1:3) + light * (1.0_wp - rgba(1:3))
        call check_status(marker_set_style(self%marker, real(rgba, c_float), real(size, c_float), self%shape_, &
                                           0.0_c_float))
    end subroutine time_show_state

    !> What kind of frame this is: 'TimeHandle'.
    function time_frame_info(self) result(info)
        class(TimeHandle), intent(in) :: self
        character(len=:), allocatable :: info
        info = 'TimeHandle'
    end function time_frame_info

    !> The marker's bounding sphere: the frame's origin.
    subroutine time_bound(self, center, radius)
        class(TimeHandle), intent(in) :: self
        real(wp), intent(out) :: center(3)                  !! the bound's center (frame coordinates)
        real(wp), intent(out) :: radius                     !! its radius (scene units: 0, the marker sized on the screen)
        center = 0.0_wp
        radius = 0.0_wp
    end subroutine time_bound

    !> Picked at its marker (within the tolerance plus half its size), as
    !> PICK_HANDLE; else the frame's origin.
    subroutine time_pick(self, cam, to_root, hits, n)
        class(TimeHandle), intent(in), target :: self
        type(PickCamera), intent(in) :: cam                 !! the camera and cursor of the pick
        real(wp), intent(in) :: to_root(4,4)                !! the frame's transform to the root frame
        type(PickHit), allocatable, intent(inout) :: hits(:)  !! hits so far, in hits(1:n); added to
        integer, intent(inout) :: n                         !! number of hits
        type(PickHit) :: h
        real(wp) :: pixels, depth
        call self%ReferenceFrame%pick(cam, to_root, hits, n)
        if (.not. (self%draggable() .and. self%contents_shown() .and. wants(cam, PICK_HANDLE))) return
        if (.not. point_pick(cam, to_root(1:3, 4), pixels, depth, extra=0.5_wp * self%size_ * cam%pixel_ratio)) return
        h%kind = PICK_HANDLE
        h%frame => self
        h%position = to_root(1:3, 4)
        h%local = 0.0_wp
        h%time = self%time_
        h%distance = depth
        h%pixels = pixels
        call add_pick_hit(hits, n, h)
    end subroutine time_pick

    !> The marker: one point at the frame's origin.
    subroutine time_build_contents(self, viewer, group)
        class(TimeHandle), intent(inout) :: self
        type(c_ptr), intent(in) :: viewer                   !! a window's viewer (null: headless)
        type(c_ptr), intent(in) :: group                    !! the frame's contents group
        self%marker = vkf_marker_create(1_c_int, c_null_ptr, 0_c_int)
        if (.not. c_associated(self%marker)) then
            write(*, '(a)') 'vkframes: TimeHandle '//self%name()//': could not build its marker'
            return
        end if
        call check_status(line_set_points(self%marker, reshape([0.0_c_double, 0.0_c_double, 0.0_c_double], [3, 1])))
        call self%show_state()
        call check_status(vkf_node_add_child(group, self%marker))
        if (c_associated(viewer)) n_shown = n_shown + 1
    end subroutine time_build_contents

    !> Whether any handle has been shown in a window (so windows look for
    !> handles under the cursor).
    logical function handles_shown()
        handles_shown = n_shown > 0
    end function handles_shown

end module vkf_handles
