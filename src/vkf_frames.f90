!> Reference frames: the tree of frames that every scene object belongs to, as
!> OpenFrames' ReferenceFrame.
!>
!> A frame has a position and attitude relative to its parent, a color for its
!> decorations (axes, labels), and children. It can follow trajectories
!> (`follow`), which then set its position and/or attitude at each `update`.
!> Scene objects (spheres, models, trajectories' artists, ...) extend this type.
!>
!> Frames are owned by the program, not by the tree: `add_child` stores
!> pointers, so a frame (declared `target`, or allocated) must stay alive while
!> it is in a tree, or be removed first (`detach`). A frame may have several
!> parents; adding a child that is an ancestor of the parent is refused.
!>
!> Rendering: when a window first shows a tree (`realize`), each frame gets a
!> transform node, with its contents (`build_contents`, which extensions
!> override) under a switch for `show_contents`; children's nodes go under the
!> parent's transform, so a frame with several parents is drawn under each.
!> Each frame, `update` moves followed frames to the time, then `late_update`
!> lets contents depend on other frames' positions at that time (e.g.
!> CustomLineSegments between spacecraft), and `sync` copies positions and
!> attitudes to the transforms. A shown
!> tree can change: children added to a shown frame are realized and compiled
!> (for the windows running) and drawn from the next frame;
!> removed ones stop being drawn; moved axes (move_x_axis, ...) are made again.
!>
!> Labels: the name label (at the origin, facing the screen) and the axes'
!> labels can change while shown: the name (`set_name`, e.g. a readout that
!> follows the frame), color, label size, alignment and font, and the axes'
!> label texts.
!>
!> Picking (WindowProxy%pick): each frame tests what it draws against a
!> PickCamera (vkf_pick) in its `pick` procedure, given its transform to the
!> root frame, and adds a PickHit for each thing within reach (the window
!> sorts them nearest first). A plain frame is picked at its origin if it
!> shows axes or its name label (PICK_FRAME); extensions pick their contents
!> (bodies, trajectories).
module vkf_frames

    use, intrinsic :: iso_c_binding, only: c_ptr, c_null_ptr, c_associated, c_int, c_float, c_double
    use vkf_layer, only: check_status, vkf_transform_create, vkf_transform_set_matrix, vkf_switch_create, vkf_switch_set, &
                         vkf_node_add_child, vkf_node_remove_child, vkf_node_delete, vkf_compile_node, vkf_viewer_any, &
                         vkf_light_create, vkf_light_set, vkf_label_set_text, vkf_label_set_color, vkf_label_set_on_top
    use vkf_geometry, only: arrow_create
    use vkf_text, only: label_create, label_set_layout
    use vkf_math, only: wp, quat_identity, quat_normalize, frame_matrix, rigid_inverse
    use vkf_pick, only: PickCamera, point_pick, ray, ray_ellipsoid, wants
    use vkf_trajectory, only: Trajectory
    use vkf_follower, only: TrajectoryFollower, FOLLOW_LOOP, FOLLOW_POSITION, FOLLOW_ATTITUDE

    implicit none

    private

    !> which axes to show (OpenFrames' AxesType), combined with ior
    integer, parameter, public :: NO_AXES = 0               !! none
    integer, parameter, public :: X_AXIS = 1                !! the x axis
    integer, parameter, public :: Y_AXIS = 2                !! the y axis
    integer, parameter, public :: Z_AXIS = 4                !! the z axis
    integer, parameter, public :: ALL_AXES = 7              !! all three

    !> text alignment of labels (osgText's AlignmentType, as OpenFrames uses)
    integer, parameter, public :: ALIGN_LEFT_TOP = 0, ALIGN_LEFT_CENTER = 1, ALIGN_LEFT_BOTTOM = 2
    integer, parameter, public :: ALIGN_CENTER_TOP = 3, ALIGN_CENTER_CENTER = 4, ALIGN_CENTER_BOTTOM = 5
    integer, parameter, public :: ALIGN_RIGHT_TOP = 6, ALIGN_RIGHT_CENTER = 7, ALIGN_RIGHT_BOTTOM = 8
    integer, parameter, public :: ALIGN_LEFT_BASE_LINE = 9, ALIGN_CENTER_BASE_LINE = 10, ALIGN_RIGHT_BASE_LINE = 11

    !> A frame in a list of frames (a frame's children and parents): a pointer
    !> to it (Fortran has no arrays of pointers).
    type :: FramePointer
        class(ReferenceFrame), pointer :: p => null()       !! the frame (not owned)
    end type FramePointer

    !> what a pick hit (PickHit%kind)
    integer, parameter, public :: PICK_FRAME = 1        !! a frame's origin (its axes or name label shown)
    integer, parameter, public :: PICK_BODY = 2         !! a body's surface (Sphere, Model, TerrainBody)
    integer, parameter, public :: PICK_CURVE = 3        !! a trajectory's curve (CurveArtist)
    integer, parameter, public :: PICK_SEGMENT = 4      !! a trajectory's segment (SegmentArtist)
    integer, parameter, public :: PICK_MARKER = 5       !! a trajectory's marker (MarkerArtist)
    integer, parameter, public :: PICK_HANDLE = 6       !! a handle the mouse drags (vkf_handles: VectorHandle)

    !> What a pick found (WindowProxy%pick): the frame it belongs to and, for
    !> trajectories, the artist (select type to use it) and the trajectory.
    type, public :: PickHit
        integer :: kind = 0                                 !! PICK_*
        class(ReferenceFrame), pointer :: frame => null()   !! the frame hit (what it draws, or its origin)
        class(*), pointer :: artist => null()               !! the trajectory artist (curves, segments, markers)
        type(Trajectory), pointer :: trajectory => null()   !! (trajectories: the artist's trajectory)
        !> For curves, the trajectory point the hit is after (on the segment from it
        !> to the next); segments: the segment (from point 1 + (index - 1) *
        !> stride); markers: the point (start, end) or the intermediate
        !> marker's number
        integer :: index = 0
        integer :: marker = 0                               !! (markers: MARKER_START, MARKER_END or MARKER_INTERMEDIATE)
        real(wp) :: time = 0.0_wp                           !! (trajectories: the time there (interpolated on curves))
        real(wp) :: position(3) = 0.0_wp                    !! the point hit, in the root frame
        real(wp) :: local(3) = 0.0_wp                       !! the point hit, in the frame's coordinates
        real(wp) :: distance = 0.0_wp                       !! its depth from the eye (nearest first)
        real(wp) :: pixels = 0.0_wp                         !! its distance from the cursor (framebuffer pixels; 0 on a body)
        real(wp) :: latitude = 0.0_wp, longitude = 0.0_wp   !! (bodies: geodetic, radians (Sphere, TerrainBody))
    end type PickHit

    public :: add_pick_hit, pick_ellipsoid

    !> A reference frame (see the module documentation).
    type, public :: ReferenceFrame
        private
        character(len=:), allocatable :: name_              !! the name (shown by the name label)
        real(wp) :: color_(4) = [1.0_wp, 1.0_wp, 1.0_wp, 0.9_wp]  !! decorations' color [r, g, b, a] (0 to 1)
        real(wp) :: position_(3) = 0.0_wp                   !! relative to the parent
        real(wp) :: attitude_(4) = quat_identity            !! relative to the parent: quaternion [x, y, z, w]
        type(FramePointer), allocatable :: children(:), parents(:)  !! not owned (first n_children, n_parents used)
        integer :: n_children = 0, n_parents = 0            !! number of children and parents
        type(TrajectoryFollower) :: follower_               !! sets position and attitude from followed trajectories
        logical :: following = .false.                      !! following trajectories (`follow`, until `unfollow`)
        ! decorations (drawn by the renderer)
        integer :: axes = ALL_AXES                          !! OpenFrames' defaults: all shown
        integer :: axes_labels = ALL_AXES                   !! axes labels shown (as axes)
        logical :: name_label = .true.                      !! the name label shown
        logical :: name_on_top = .false.                    !! the name label drawn on top of the scene
        character(len=:), allocatable :: x_label_, y_label_, z_label_  !! axes labels (unallocated: 'X', 'Y', 'Z')
        character(len=:), allocatable :: label_font_        !! labels' font (unallocated: the renderer's default)
        integer :: label_size_ = 20                         !! labels' size (pixels)
        integer :: label_alignment_ = ALIGN_LEFT_BASE_LINE  !! labels' alignment (ALIGN_*)
        logical :: light_source = .false.                   !! carries a light source (set_light_source_enabled)
        ! the light source's colors (OSG's default light, as OpenFrames')
        real(wp) :: light_ambient_(3) = 0.05_wp, light_diffuse_(3) = 0.8_wp, light_specular_(3) = 0.05_wp  !! [r, g, b]
        type(c_ptr) :: light_node = c_null_ptr, light_switch = c_null_ptr  !! light and switch (made when enabled)
        logical :: contents = .true.                        !! what the frame draws shown (show_contents)
        logical :: pickable_ = .true.                       !! picked (WindowProxy%pick); its children aside
        ! rendering
        type(c_ptr) :: node_ = c_null_ptr                   !! the frame's transform node
        type(c_ptr) :: contents_node = c_null_ptr           !! switch holding the contents
        logical :: shown_ = .false.                         !! realized for a window (not headless): later nodes are compiled
        logical :: realized = .false.                       !! render nodes made (realize)
        logical :: synced = .false.                         !! the node has synced_position/attitude
        real(wp) :: synced_position(3) = 0.0_wp, synced_attitude(4) = 0.0_wp  !! last copied to the node
        ! axes (OpenFrames' moveXAxis/...: base, length, head ratio, radii; 0 radius: auto)
        real(wp) :: axis_base(3,3) = 0.0_wp                 !! each axis's base (columns; frame coordinates)
        real(wp) :: axis_len(3) = 1.0_wp                    !! each axis's length
        real(wp) :: axis_head_ratio(3) = 0.3_wp             !! each axis's head, as a fraction of its length
        real(wp) :: axis_body_radius(3) = 0.0_wp, axis_head_radius(3) = 0.0_wp  !! (0: 0.05 and 0.1 of the length)
        logical :: axes_moved = .false.                     !! set by move_*_axis (else reposition_axes places them)
        type(c_ptr) :: axis_node(3) = c_null_ptr            !! the axes arrows' switches
        type(c_ptr) :: axis_label_node(3) = c_null_ptr      !! the axes labels' switches
        type(c_ptr) :: axis_arrow(3) = c_null_ptr           !! the axes arrows (in axis_node; made again when an axis moves)
        type(c_ptr) :: axis_text(3) = c_null_ptr            !! the axes labels (in axis_label_node)
        type(c_ptr) :: name_label_node = c_null_ptr         !! the name label's switch
        type(c_ptr) :: name_text = c_null_ptr               !! the name label (in name_label_node)
    contains
        procedure :: set_name, name
        procedure :: set_color, color
        procedure :: set_position, position
        procedure :: set_attitude, attitude
        procedure :: local_matrix, world_matrix, position_of
        procedure :: show_axes, axes_shown
        procedure :: show_axes_labels, axes_labels_shown
        procedure :: show_name_label, name_label_shown
        procedure :: show_decorations
        procedure :: set_name_label_on_top, name_label_on_top
        procedure :: set_x_label, set_y_label, set_z_label, axis_label
        procedure :: set_label_font, label_font
        procedure :: set_label_size, label_size
        procedure :: set_label_alignment, label_alignment
        procedure :: set_light_source_enabled, light_source_enabled, has_light_source
        procedure :: set_light_ambient, set_light_diffuse, set_light_specular
        procedure :: light_ambient, light_diffuse, light_specular
        procedure :: show_contents, contents_shown
        procedure :: frame_info
        procedure :: add_child, remove_child, detach
        procedure :: num_children, child, child_index
        procedure :: num_parents, parent, parent_index
        procedure :: frame_string
        procedure :: follow, unfollow, follower, is_following
        procedure :: update, late_update
        procedure :: realize, build_contents, sync, node
        procedure :: bound, pick, set_pickable, pickable
        procedure :: move_x_axis, move_y_axis, move_z_axis, reposition_axes
    end type ReferenceFrame

    interface ReferenceFrame
        module procedure new_reference_frame
    end interface ReferenceFrame

    public :: FramePointer

contains

    !> A frame named `name`, with decorations of color `color` ([r, g, b] or
    !> [r, g, b, a], 0 to 1; default white, 90% opaque).
    function new_reference_frame(name, color) result(frame)
        character(len=*), intent(in) :: name
        real(wp), intent(in), optional :: color(:)
        type(ReferenceFrame) :: frame
        frame%name_ = name
        if (present(color)) call frame%set_color(color)
    end function new_reference_frame

    ! ==================================================================
    ! name, color, position, attitude

    !> The frame's name, shown by its name label (also once shown: e.g. a
    !> readout that follows the frame).
    subroutine set_name(self, name)
        class(ReferenceFrame), intent(inout) :: self
        character(len=*), intent(in) :: name
        self%name_ = name
        if (c_associated(self%name_text)) call check_status(vkf_label_set_text(self%name_text, name, len(name, kind=c_int)))
    end subroutine set_name

    !> The frame's name ('' if none).
    function name(self) result(n)
        class(ReferenceFrame), intent(in) :: self
        character(len=:), allocatable :: n
        if (allocated(self%name_)) then
            n = self%name_
        else
            n = ''
        end if
    end function name

    !> The color of the frame's decorations: [r, g, b] (alpha unchanged) or [r, g, b, a].
    subroutine set_color(self, color)
        class(ReferenceFrame), intent(inout) :: self
        real(wp), intent(in) :: color(:)
        integer :: n
        n = min(4, size(color))
        self%color_(1:n) = color(1:n)
        if (.not. self%realized) return
        if (c_associated(self%name_text)) call check_status(vkf_label_set_color(self%name_text, real(self%color_, c_float)))
        call remake_axes(self)
    end subroutine set_color

    !> The color of the frame's decorations, [r, g, b, a] (0 to 1).
    pure function color(self) result(c)
        class(ReferenceFrame), intent(in) :: self
        real(wp) :: c(4)
        c = self%color_
    end function color

    !> Position relative to the parent frame (overridden by a followed trajectory).
    subroutine set_position(self, position)
        class(ReferenceFrame), intent(inout) :: self
        real(wp), intent(in) :: position(3)                 !! parent frame's coordinates
        self%position_ = position
    end subroutine set_position

    !> Position relative to the parent frame.
    pure function position(self) result(p)
        class(ReferenceFrame), intent(in) :: self
        real(wp) :: p(3)
        p = self%position_
    end function position

    !> Attitude relative to the parent frame, a quaternion [x, y, z, w]
    !> (normalized here; overridden by a followed trajectory).
    subroutine set_attitude(self, q)
        class(ReferenceFrame), intent(inout) :: self
        real(wp), intent(in) :: q(4)
        self%attitude_ = quat_normalize(q)
    end subroutine set_attitude

    !> Attitude relative to the parent frame, a quaternion [x, y, z, w].
    pure function attitude(self) result(q)
        class(ReferenceFrame), intent(in) :: self
        real(wp) :: q(4)
        q = self%attitude_
    end function attitude

    !> The transform from this frame to its parent (4x4).
    pure function local_matrix(self) result(m)
        class(ReferenceFrame), intent(in) :: self
        real(wp) :: m(4,4)
        m = frame_matrix(self%position_, self%attitude_)
    end function local_matrix

    !> The transform from this frame to the root of its tree, through the first
    !> parent at each level.
    recursive function world_matrix(self) result(m)
        class(ReferenceFrame), intent(in) :: self
        real(wp) :: m(4,4)
        m = self%local_matrix()
        if (self%n_parents > 0) m = matmul(self%parents(1)%p%world_matrix(), m)
    end function world_matrix

    !> The origin of `other` (a frame of the same tree) in this frame's
    !> coordinates, from both frames' transforms to the root (through their
    !> first parents).
    function position_of(self, other) result(p)
        class(ReferenceFrame), intent(in) :: self, other
        real(wp) :: p(3)
        real(wp) :: m(4,4)
        m = matmul(rigid_inverse(self%world_matrix()), other%world_matrix())
        p = m(1:3, 4)
    end function position_of

    ! ==================================================================
    ! decorations

    !> Which axes vectors to show: NO_AXES, or X_AXIS, Y_AXIS, Z_AXIS combined with ior.
    subroutine show_axes(self, axes)
        class(ReferenceFrame), intent(inout) :: self
        integer, intent(in) :: axes
        self%axes = iand(axes, ALL_AXES)
        call update_switches(self)
    end subroutine show_axes

    !> Which axes vectors are shown (NO_AXES, or X_AXIS, Y_AXIS, Z_AXIS combined).
    pure integer function axes_shown(self)
        class(ReferenceFrame), intent(in) :: self
        axes_shown = self%axes
    end function axes_shown

    !> Which axes labels to show (as `show_axes`).
    subroutine show_axes_labels(self, labels)
        class(ReferenceFrame), intent(inout) :: self
        integer, intent(in) :: labels
        self%axes_labels = iand(labels, ALL_AXES)
        call update_switches(self)
    end subroutine show_axes_labels

    !> Which axes labels are shown (as `axes_shown`).
    pure integer function axes_labels_shown(self)
        class(ReferenceFrame), intent(in) :: self
        axes_labels_shown = self%axes_labels
    end function axes_labels_shown

    !> Show or hide the name label (at the frame's origin; default shown).
    subroutine show_name_label(self, show)
        class(ReferenceFrame), intent(inout) :: self
        logical, intent(in) :: show                         !! (true: shown)
        self%name_label = show
        call update_switches(self)
    end subroutine show_name_label

    !> Show (true) or hide all the frame's decorations at once: its axes,
    !> their labels and its name label (show_axes, show_axes_labels,
    !> show_name_label).
    subroutine show_decorations(self, show)
        class(ReferenceFrame), intent(inout) :: self
        logical, intent(in) :: show                         !! true: all axes, axes labels and the name label
        call self%show_axes(merge(ALL_AXES, NO_AXES, show))
        call self%show_axes_labels(merge(ALL_AXES, NO_AXES, show))
        call self%show_name_label(show)
    end subroutine show_decorations

    !> Draw the name label on top of the scene (true: not hidden by bodies in
    !> front of it, as HUD text; e.g. a readout) or depth-tested (false, the
    !> default, as OpenFrames').
    subroutine set_name_label_on_top(self, on_top)
        class(ReferenceFrame), intent(inout) :: self
        logical, intent(in) :: on_top
        self%name_on_top = on_top
        if (c_associated(self%name_text)) &
            call check_status(vkf_label_set_on_top(self%name_text, merge(1_c_int, 0_c_int, on_top)))
    end subroutine set_name_label_on_top

    !> Whether the name label is drawn on top of the scene (`set_name_label_on_top`).
    pure logical function name_label_on_top(self)
        class(ReferenceFrame), intent(in) :: self
        name_label_on_top = self%name_on_top
    end function name_label_on_top

    !> Whether the name label is shown.
    pure logical function name_label_shown(self)
        class(ReferenceFrame), intent(in) :: self
        name_label_shown = self%name_label
    end function name_label_shown

    !> The text of the x-axis label (default 'X').
    subroutine set_x_label(self, text)
        class(ReferenceFrame), intent(inout) :: self
        character(len=*), intent(in) :: text
        self%x_label_ = text
        if (self%realized) call make_axis(self, 1)
    end subroutine set_x_label

    !> The text of the y-axis label (default 'Y').
    subroutine set_y_label(self, text)
        class(ReferenceFrame), intent(inout) :: self
        character(len=*), intent(in) :: text
        self%y_label_ = text
        if (self%realized) call make_axis(self, 2)
    end subroutine set_y_label

    !> The text of the z-axis label (default 'Z').
    subroutine set_z_label(self, text)
        class(ReferenceFrame), intent(inout) :: self
        character(len=*), intent(in) :: text
        self%z_label_ = text
        if (self%realized) call make_axis(self, 3)
    end subroutine set_z_label

    !> The label text of axis X_AXIS, Y_AXIS or Z_AXIS.
    function axis_label(self, axis) result(text)
        class(ReferenceFrame), intent(in) :: self
        integer, intent(in) :: axis
        character(len=:), allocatable :: text
        select case (axis)
        case (X_AXIS)
            text = 'X'
            if (allocated(self%x_label_)) text = self%x_label_
        case (Y_AXIS)
            text = 'Y'
            if (allocated(self%y_label_)) text = self%y_label_
        case (Z_AXIS)
            text = 'Z'
            if (allocated(self%z_label_)) text = self%z_label_
        case default
            text = ''
        end select
    end function axis_label

    !> The font of the labels: a font file name (found on the data search
    !> path) or a full path; default: the renderer's default font.
    subroutine set_label_font(self, font)
        class(ReferenceFrame), intent(inout) :: self
        character(len=*), intent(in) :: font
        self%label_font_ = font
        if (.not. self%realized) return
        call make_name_label(self)                              ! (another font: made again)
        call remake_axes(self)
    end subroutine set_label_font

    !> The labels' font as set ('' for the renderer's default).
    function label_font(self) result(font)
        class(ReferenceFrame), intent(in) :: self
        character(len=:), allocatable :: font
        if (allocated(self%label_font_)) then
            font = self%label_font_
        else
            font = ''
        end if
    end function label_font

    !> The size of the labels in pixels (default 20): the name label's size,
    !> and the maximum size of the axes labels (which shrink with distance).
    subroutine set_label_size(self, size)
        class(ReferenceFrame), intent(inout) :: self
        integer, intent(in) :: size
        self%label_size_ = size
        call relayout_labels(self)
    end subroutine set_label_size

    !> The labels' size in pixels.
    pure integer function label_size(self)
        class(ReferenceFrame), intent(in) :: self
        label_size = self%label_size_
    end function label_size

    !> The alignment of the labels relative to their positions (ALIGN_*;
    !> default ALIGN_LEFT_BASE_LINE).
    subroutine set_label_alignment(self, alignment)
        class(ReferenceFrame), intent(inout) :: self
        integer, intent(in) :: alignment
        self%label_alignment_ = alignment
        call relayout_labels(self)
    end subroutine set_label_alignment

    !> The labels' alignment (ALIGN_*).
    pure integer function label_alignment(self)
        class(ReferenceFrame), intent(in) :: self
        label_alignment = self%label_alignment_
    end function label_alignment

    !> Whether this frame carries a light source: a point light at its origin
    !> (e.g. the Sun), not dimmed with distance (default false; OpenFrames'
    !> setLightSourceEnabled). While any frame of a window's scene has one, the
    !> window's own light (WindowProxy%set_light_*) is off.
    subroutine set_light_source_enabled(self, enable)
        class(ReferenceFrame), intent(inout) :: self
        logical, intent(in) :: enable
        self%light_source = enable
        if (self%realized .and. enable .and. .not. c_associated(self%light_node)) call build_light(self)
        if (c_associated(self%light_switch)) &
            call check_status(vkf_switch_set(self%light_switch, merge(1_c_int, 0_c_int, enable)))
    end subroutine set_light_source_enabled

    !> Whether this frame carries a light source.
    pure logical function light_source_enabled(self)
        class(ReferenceFrame), intent(in) :: self
        light_source_enabled = self%light_source
    end function light_source_enabled

    !> Whether this frame or one of its descendants has a light source enabled.
    recursive logical function has_light_source(self) result(has)
        class(ReferenceFrame), intent(in) :: self
        integer :: k
        has = self%light_source
        do k = 1, self%n_children
            if (has) return
            has = self%children(k)%p%has_light_source()
        end do
    end function has_light_source

    !> The light source's ambient, diffuse and specular colors ([r, g, b], 0
    !> to 1; defaults 0.05, 0.8 and 0.05 gray).
    subroutine set_light_ambient(self, rgb)
        class(ReferenceFrame), intent(inout) :: self
        real(wp), intent(in) :: rgb(3)
        self%light_ambient_ = rgb
        call apply_light(self)
    end subroutine set_light_ambient

    !> The light source's diffuse color (see `set_light_ambient`).
    subroutine set_light_diffuse(self, rgb)
        class(ReferenceFrame), intent(inout) :: self
        real(wp), intent(in) :: rgb(3)
        self%light_diffuse_ = rgb
        call apply_light(self)
    end subroutine set_light_diffuse

    !> The light source's specular color (see `set_light_ambient`).
    subroutine set_light_specular(self, rgb)
        class(ReferenceFrame), intent(inout) :: self
        real(wp), intent(in) :: rgb(3)
        self%light_specular_ = rgb
        call apply_light(self)
    end subroutine set_light_specular

    !> The light source's ambient color [r, g, b].
    pure function light_ambient(self) result(rgb)
        class(ReferenceFrame), intent(in) :: self
        real(wp) :: rgb(3)
        rgb = self%light_ambient_
    end function light_ambient

    !> The light source's diffuse color [r, g, b].
    pure function light_diffuse(self) result(rgb)
        class(ReferenceFrame), intent(in) :: self
        real(wp) :: rgb(3)
        rgb = self%light_diffuse_
    end function light_diffuse

    !> The light source's specular color [r, g, b].
    pure function light_specular(self) result(rgb)
        class(ReferenceFrame), intent(in) :: self
        real(wp) :: rgb(3)
        rgb = self%light_specular_
    end function light_specular

    !> The light source's node: a point light at the origin, under a switch.
    subroutine build_light(self)
        class(ReferenceFrame), intent(inout) :: self
        self%light_node = vkf_light_create()
        if (.not. c_associated(self%light_node)) return
        self%light_switch = vkf_switch_create()
        call check_status(vkf_node_add_child(self%light_switch, self%light_node))
        call check_status(vkf_node_add_child(self%node_, self%light_switch))
        call apply_light(self)
    end subroutine build_light

    !> Give the light source's node its colors (if it has been made).
    subroutine apply_light(self)
        class(ReferenceFrame), intent(inout) :: self
        if (.not. c_associated(self%light_node)) return
        call check_status(vkf_light_set(self%light_node, real(self%light_ambient_, c_float), real(self%light_diffuse_, c_float), &
                                 real(self%light_specular_, c_float), [0.0_c_double, 0.0_c_double, 0.0_c_double, 1.0_c_double]))
    end subroutine apply_light

    !> Show or hide what the frame draws (its sphere, model, ...), but not its
    !> axes, labels or children.
    subroutine show_contents(self, show)
        class(ReferenceFrame), intent(inout) :: self
        logical, intent(in) :: show
        self%contents = show
        if (c_associated(self%contents_node)) call check_status(vkf_switch_set(self%contents_node, merge(1_c_int, 0_c_int, show)))
    end subroutine show_contents

    !> Whether what the frame draws is shown (`show_contents`).
    pure logical function contents_shown(self)
        class(ReferenceFrame), intent(in) :: self
        contents_shown = self%contents
    end function contents_shown

    !> Whether `pick` tests what this frame draws (default true; e.g. false
    !> for an overlay such as a selection marker). Its children are picked or
    !> not by their own setting.
    subroutine set_pickable(self, pickable)
        class(ReferenceFrame), intent(inout) :: self
        logical, intent(in) :: pickable
        self%pickable_ = pickable
    end subroutine set_pickable

    !> Whether `pick` tests what this frame draws (`set_pickable`).
    pure logical function pickable(self)
        class(ReferenceFrame), intent(in) :: self
        pickable = self%pickable_
    end function pickable

    !> Pick what the frame draws (see the module documentation): `to_root`
    !> is its transform to the root frame; hits are added to hits(1:n). A
    !> plain frame: its origin, if it shows axes or its name label.
    subroutine pick(self, cam, to_root, hits, n)
        class(ReferenceFrame), intent(in), target :: self
        type(PickCamera), intent(in) :: cam                 !! the cursor's ray and the pick's settings
        real(wp), intent(in) :: to_root(4,4)                !! the frame's transform to the root frame
        type(PickHit), allocatable, intent(inout) :: hits(:)  !! hits so far (grown as needed)
        integer, intent(inout) :: n                         !! number of hits in hits
        type(PickHit) :: h
        if (self%axes == NO_AXES .and. .not. self%name_label) return
        if (.not. wants(cam, PICK_FRAME)) return
        if (.not. point_pick(cam, to_root(1:3, 4), h%pixels, h%distance)) return
        h%kind = PICK_FRAME
        h%frame => self
        h%position = to_root(1:3, 4)
        call add_pick_hit(hits, n, h)
    end subroutine pick

    !> Pick a body of frame `self`: the ellipsoid centered at `center` with
    !> semi-axes `radii` along the columns of `axes` (unit vectors; all in the
    !> frame's coordinates), hit by the ray through the cursor. Adds a
    !> PICK_BODY hit (with the geodetic latitude and longitude of the point
    !> on the ellipsoid if `geodetic`); true if hit.
    logical function pick_ellipsoid(self, cam, to_root, center, axes, radii, geodetic, hits, n) result(hit)
        class(ReferenceFrame), intent(in), target :: self
        type(PickCamera), intent(in) :: cam
        real(wp), intent(in) :: to_root(4,4), center(3), axes(3,3), radii(3)  !! center, axes: frame coordinates
        logical, intent(in) :: geodetic
        type(PickHit), allocatable, intent(inout) :: hits(:)  !! hits so far (grown as needed)
        integer, intent(inout) :: n                         !! number of hits in hits
        type(PickHit) :: h
        real(wp) :: origin(3), dir(3), along, local(3), normal(3)
        hit = .false.
        if (.not. wants(cam, PICK_BODY)) return
        call ray(cam, origin, dir)
        hit = ray_ellipsoid(origin, dir, matmul(to_root(1:3, 1:3), center) + to_root(1:3, 4), &
                            matmul(to_root(1:3, 1:3), axes), radii, along, h%position, local)
        if (hit) hit = dot_product(h%position - cam%eye, cam%forward) <= cam%max_depth
        if (.not. hit) return
        h%kind = PICK_BODY
        h%frame => self
        h%local = matmul(transpose(to_root(1:3, 1:3)), h%position - to_root(1:3, 4))
        h%distance = dot_product(h%position - cam%eye, cam%forward)
        h%pixels = 0.0_wp
        if (geodetic) then
            normal = local / radii**2                       ! (the surface normal there)
            h%latitude = asin(max(-1.0_wp, min(1.0_wp, normal(3) / norm2(normal))))
            h%longitude = atan2(normal(2), normal(1))
        end if
        call add_pick_hit(hits, n, h)
    end function pick_ellipsoid

    !> Add `hit` to hits(1:n) (growing the array).
    subroutine add_pick_hit(hits, n, hit)
        type(PickHit), allocatable, intent(inout) :: hits(:)  !! grown as needed
        integer, intent(inout) :: n                         !! number of hits in hits
        type(PickHit), intent(in) :: hit
        type(PickHit), allocatable :: more(:)
        if (.not. allocated(hits)) allocate(hits(8))
        if (n == size(hits)) then
            allocate(more(2 * n))
            more(1:n) = hits(1:n)
            call move_alloc(more, hits)
        end if
        n = n + 1
        hits(n) = hit
    end subroutine add_pick_hit

    !> The bounding sphere of what the frame draws, in its own coordinates
    !> (radius 0 if it draws nothing; extensions override it). Views use it for
    !> their default distance.
    subroutine bound(self, center, radius)
        class(ReferenceFrame), intent(in) :: self
        real(wp), intent(out) :: center(3), radius          !! frame's coordinates
        center = 0.0_wp
        radius = 0.0_wp
    end subroutine bound

    !> Place the x axis vector: from `base`, `length` long, with a head of
    !> `head_ratio` of the length (default 0.3) and body and head radii (default
    !> 0.05 and 0.1 of the length); its label is at the tip, 0.4 of the length
    !> tall (at most `label_size` pixels). As OpenFrames' moveXAxis. Set before
    !> the frame is first shown.
    subroutine move_x_axis(self, base, length, head_ratio, body_radius, head_radius)
        class(ReferenceFrame), intent(inout) :: self
        real(wp), intent(in) :: base(3), length             !! (base: frame's coordinates)
        real(wp), intent(in), optional :: head_ratio, body_radius, head_radius
        call move_axis(self, 1, base, length, head_ratio, body_radius, head_radius)
    end subroutine move_x_axis

    !> Place the y axis vector (as `move_x_axis`; OpenFrames' moveYAxis).
    subroutine move_y_axis(self, base, length, head_ratio, body_radius, head_radius)
        class(ReferenceFrame), intent(inout) :: self
        real(wp), intent(in) :: base(3), length
        real(wp), intent(in), optional :: head_ratio, body_radius, head_radius
        call move_axis(self, 2, base, length, head_ratio, body_radius, head_radius)
    end subroutine move_y_axis

    !> Place the z axis vector (as `move_x_axis`; OpenFrames' moveZAxis).
    subroutine move_z_axis(self, base, length, head_ratio, body_radius, head_radius)
        class(ReferenceFrame), intent(inout) :: self
        real(wp), intent(in) :: base(3), length
        real(wp), intent(in), optional :: head_ratio, body_radius, head_radius
        call move_axis(self, 3, base, length, head_ratio, body_radius, head_radius)
    end subroutine move_z_axis

    !> Place axis k (as `move_x_axis`), made again if the frame is shown.
    subroutine move_axis(self, k, base, length, head_ratio, body_radius, head_radius)
        class(ReferenceFrame), intent(inout) :: self
        integer, intent(in) :: k                            !! the axis: 1 (x), 2 (y) or 3 (z)
        real(wp), intent(in) :: base(3), length
        real(wp), intent(in), optional :: head_ratio, body_radius, head_radius
        self%axis_base(:, k) = base
        self%axis_len(k) = length
        self%axis_head_ratio(k) = 0.3_wp
        self%axis_body_radius(k) = 0.0_wp
        self%axis_head_radius(k) = 0.0_wp
        if (present(head_ratio)) self%axis_head_ratio(k) = head_ratio
        if (present(body_radius)) self%axis_body_radius(k) = body_radius
        if (present(head_radius)) self%axis_head_radius(k) = head_radius
        self%axes_moved = .true.
        if (self%realized) call make_axis(self, k)              ! shown: made again
    end subroutine move_axis

    !> Place the axes for what the frame draws, when it is first shown (unless
    !> move_*_axis was called): at its bound's surface, half the bound's radius
    !> long (as OpenFrames' Sphere and Model do); at the origin, 1 long, for a
    !> frame that draws nothing.
    subroutine reposition_axes(self)
        class(ReferenceFrame), intent(inout) :: self
        real(wp) :: c(3), r, e(3)
        integer :: k
        call self%bound(c, r)
        if (r <= 0.0_wp) return
        do k = 1, 3
            e = 0.0_wp
            e(k) = 1.0_wp
            self%axis_base(:, k) = c + r * e
            self%axis_len(k) = 0.5_wp * r
        end do
    end subroutine reposition_axes

    !> What kind of frame this is (shown by `frame_string`; extensions override it).
    function frame_info(self) result(info)
        class(ReferenceFrame), intent(in) :: self
        character(len=:), allocatable :: info
        info = 'ReferenceFrame'
    end function frame_info

    ! ==================================================================
    ! the tree

    !> Add `child` to this frame's children. Returns false (and does nothing)
    !> for this frame itself or an ancestor of it; true if it is already a child.
    logical function add_child(self, child) result(ok)
        class(ReferenceFrame), intent(inout), target :: self
        class(ReferenceFrame), intent(inout), target :: child
        ok = .false.
        if (same_frame(self, child)) return
        if (self%child_index(child) > 0) then
            ok = .true.
            return
        end if
        if (is_descendant(child, self)) return
        call append(self%children, self%n_children, child)
        call append(child%parents, child%n_parents, self)
        ok = .true.
        if (self%realized) then                         ! shown: draw the child from the next frame
            if (.not. child%realized) then
                if (self%shown_) then                   ! (for a window that exists now)
                    call child%realize(vkf_viewer_any())
                    call check_status(vkf_compile_node(child%node_))
                else
                    call child%realize(c_null_ptr)
                end if
            end if
            call check_status(vkf_node_add_child(self%node_, child%node_))
        end if
    end function add_child

    !> Remove `child` from this frame's children. Returns false if it is not one.
    logical function remove_child(self, child) result(ok)
        class(ReferenceFrame), intent(inout), target :: self
        class(ReferenceFrame), intent(inout), target :: child
        ok = remove(self%children, self%n_children, child)
        if (ok) ok = remove(child%parents, child%n_parents, self)
        if (ok .and. self%realized .and. child%realized) call check_status(vkf_node_remove_child(self%node_, child%node_))
    end function remove_child

    !> Remove this frame from all its parents and children (before it goes out
    !> of scope or is deallocated while in a tree).
    subroutine detach(self)
        class(ReferenceFrame), intent(inout), target :: self
        logical :: ok
        do while (self%n_parents > 0)
            ok = self%parents(self%n_parents)%p%remove_child(self)
        end do
        do while (self%n_children > 0)
            ok = self%remove_child(self%children(self%n_children)%p)
        end do
    end subroutine detach

    !> Number of children.
    pure integer function num_children(self)
        class(ReferenceFrame), intent(in) :: self
        num_children = self%n_children
    end function num_children

    !> Child i (1 to num_children).
    function child(self, i) result(c)
        class(ReferenceFrame), intent(in) :: self
        integer, intent(in) :: i
        class(ReferenceFrame), pointer :: c
        c => self%children(i)%p
    end function child

    !> Index of `frame` among the children, or 0.
    integer function child_index(self, frame)
        class(ReferenceFrame), intent(in) :: self
        class(ReferenceFrame), intent(in), target :: frame
        child_index = find(self%children, self%n_children, frame)
    end function child_index

    !> Number of parents.
    pure integer function num_parents(self)
        class(ReferenceFrame), intent(in) :: self
        num_parents = self%n_parents
    end function num_parents

    !> Parent i (1 to num_parents).
    function parent(self, i) result(p)
        class(ReferenceFrame), intent(in) :: self
        integer, intent(in) :: i
        class(ReferenceFrame), pointer :: p
        p => self%parents(i)%p
    end function parent

    !> Index of `frame` among the parents, or 0.
    integer function parent_index(self, frame)
        class(ReferenceFrame), intent(in) :: self
        class(ReferenceFrame), intent(in), target :: frame
        parent_index = find(self%parents, self%n_parents, frame)
    end function parent_index

    !> The tree below this frame, one line per frame ("name (info)"), in
    !> OpenFrames' createFrameString format: children are prefixed "|-->".
    recursive function frame_string(self, prefix) result(str)
        class(ReferenceFrame), intent(in) :: self
        character(len=*), intent(in), optional :: prefix    !! before this frame's line (default ' ')
        character(len=:), allocatable :: str, pre
        integer :: k
        pre = ' '
        if (present(prefix)) pre = prefix
        str = pre//self%name()//' ('//self%frame_info()//')'//new_line('a')
        if (pre(len(pre):len(pre)) == '>') pre(len(pre):len(pre)) = '-'
        pre = pre//'|-->'
        do k = 1, self%n_children
            str = str//self%children(k)%p%frame_string(pre)
        end do
    end function frame_string

    ! ==================================================================
    ! following trajectories

    !> Follow `traj` (in addition to trajectories already followed, if `add`
    !> is true), with what to follow (`data`: FOLLOW_POSITION and/or
    !> FOLLOW_ATTITUDE; default both) and how (`mode`: FOLLOW_LOOP, the
    !> default, or FOLLOW_LIMIT). `traj` must stay alive while followed.
    subroutine follow(self, traj, data, mode, add)
        class(ReferenceFrame), intent(inout) :: self
        type(Trajectory), intent(in), target :: traj        !! the trajectory to follow (not owned)
        integer, intent(in), optional :: data, mode         !! FOLLOW_POSITION/FOLLOW_ATTITUDE; FOLLOW_LOOP/FOLLOW_LIMIT
        logical, intent(in), optional :: add                !! (true: in addition to those followed (default false))
        type(Trajectory), pointer :: p
        integer :: d, m
        logical :: adding
        p => traj
        adding = .false.
        if (present(add)) adding = add
        d = ior(FOLLOW_POSITION, FOLLOW_ATTITUDE)
        m = FOLLOW_LOOP
        if (present(data)) d = data
        if (present(mode)) m = mode
        if (adding) then
            call self%follower_%add_trajectory(p)
        else
            call self%follower_%set_trajectory(p)
        end if
        call self%follower_%set_follow_type(d, m)
        self%following = .true.
    end subroutine follow

    !> Stop following trajectories (the frame keeps its last position and attitude).
    subroutine unfollow(self)
        class(ReferenceFrame), intent(inout) :: self
        call self%follower_%remove_all()
        self%following = .false.
    end subroutine unfollow

    !> The frame's trajectory follower, for finer control (data sources, time
    !> offset, ...).
    function follower(self) result(f)
        class(ReferenceFrame), intent(in), target :: self
        type(TrajectoryFollower), pointer :: f
        f => self%follower_
    end function follower

    !> Whether the frame follows trajectories (at least one, since `follow`).
    pure logical function is_following(self)
        class(ReferenceFrame), intent(in) :: self
        is_following = self%following .and. self%follower_%num_trajectories() > 0
    end function is_following

    !> Update this frame and its descendants for simulation time `t` (followed
    !> trajectories set positions and attitudes).
    recursive subroutine update(self, t)
        class(ReferenceFrame), intent(inout) :: self
        real(wp), intent(in) :: t                           !! simulation time
        logical :: has_position, has_attitude
        integer :: k
        if (self%following) &
            call self%follower_%update(t, self%position_, self%attitude_, has_position, has_attitude)
        do k = 1, self%n_children
            call self%children(k)%p%update(t)
        end do
    end subroutine update

    !> After the whole tree's update for time t (all frames at their positions
    !> for t): for contents that depend on other frames' positions, e.g.
    !> CustomLineSegments between spacecraft. Here, only the descendants'.
    recursive subroutine late_update(self, t)
        class(ReferenceFrame), intent(inout) :: self
        real(wp), intent(in) :: t                           !! simulation time
        integer :: k
        do k = 1, self%n_children
            call self%children(k)%p%late_update(t)
        end do
    end subroutine late_update

    ! ==================================================================
    ! rendering

    !> Create the render nodes of this frame and its descendants (once; for
    !> `viewer`, a window's viewer handle, which some contents need while they
    !> are built (not kept); null: headless).
    recursive subroutine realize(self, viewer)
        class(ReferenceFrame), intent(inout) :: self
        type(c_ptr), intent(in) :: viewer                   !! a window's viewer handle (null: headless)
        integer :: k
        if (self%realized) return
        self%realized = .true.
        self%shown_ = c_associated(viewer)
        self%node_ = vkf_transform_create()
        self%synced = .false.
        self%contents_node = vkf_switch_create()
        call check_status(vkf_node_add_child(self%node_, self%contents_node))
        call self%build_contents(viewer, self%contents_node)
        call build_decorations(self)
        if (self%light_source) call build_light(self)
        if (.not. self%contents) call check_status(vkf_switch_set(self%contents_node, 0_c_int))
        call self%sync()
        do k = 1, self%n_children
            call self%children(k)%p%realize(viewer)
            call check_status(vkf_node_add_child(self%node_, self%children(k)%p%node_))
        end do
    end subroutine realize

    !> Add what this frame draws to `group` (a node handle). ReferenceFrame
    !> draws nothing itself (its axes and labels come later); extensions such as
    !> Sphere and Model override this.
    subroutine build_contents(self, viewer, group)
        class(ReferenceFrame), intent(inout) :: self
        type(c_ptr), intent(in) :: viewer, group            !! (viewer: as `realize`'s)
    end subroutine build_contents

    !> Copy the positions and attitudes of this frame and its descendants to
    !> their transform nodes (only those that changed since the last sync).
    recursive subroutine sync(self)
        class(ReferenceFrame), intent(inout) :: self
        integer :: k
        if (.not. self%realized) return
        if (.not. (self%synced .and. all(self%position_ == self%synced_position) .and. &
                   all(self%attitude_ == self%synced_attitude))) then
            call check_status(vkf_transform_set_matrix(self%node_, self%local_matrix()))
            self%synced_position = self%position_
            self%synced_attitude = self%attitude_
            self%synced = .true.
        end if
        do k = 1, self%n_children
            call self%children(k)%p%sync()
        end do
    end subroutine sync

    !> The frame's transform node (null until realized).
    function node(self) result(n)
        class(ReferenceFrame), intent(in) :: self
        type(c_ptr) :: n
        n = self%node_
    end function node

    !> The axes (arrows), axes labels and name label, each under a switch, in
    !> the frame's color (OpenFrames' ReferenceFrame decorations).
    subroutine build_decorations(self)
        class(ReferenceFrame), intent(inout) :: self
        integer :: k
        if (.not. self%axes_moved) call self%reposition_axes()
        do k = 1, 3
            self%axis_node(k) = vkf_switch_create()
            self%axis_label_node(k) = vkf_switch_create()
            call make_axis(self, k)
            call check_status(vkf_node_add_child(self%node_, self%axis_node(k)))
            call check_status(vkf_node_add_child(self%node_, self%axis_label_node(k)))
        end do
        ! the name label at the origin, label_size pixels tall
        self%name_label_node = vkf_switch_create()
        call make_name_label(self)
        call check_status(vkf_node_add_child(self%node_, self%name_label_node))
        call update_switches(self)
    end subroutine build_decorations

    !> The name label (in place of the old one; compiled first if shown).
    subroutine make_name_label(self)
        class(ReferenceFrame), intent(inout) :: self
        call replace_child(self, self%name_label_node, self%name_text, &
                           label_create(self%name(), [0.0_wp, 0.0_wp, 0.0_wp], self%color_, &
                                        real(self%label_size_, wp), 0.0_wp, self%label_alignment_, self%label_font()))
        if (self%name_on_top .and. c_associated(self%name_text)) call check_status(vkf_label_set_on_top(self%name_text, 1_c_int))
        call update_switches(self)                              ! (a node added to a switch is shown)
    end subroutine make_name_label

    !> The axes and their labels made again (another color, size or font).
    subroutine remake_axes(self)
        class(ReferenceFrame), intent(inout) :: self
        integer :: k
        if (.not. self%realized) return
        do k = 1, 3
            call make_axis(self, k)
        end do
    end subroutine remake_axes

    !> The labels' size and alignment changed: the name label's in place, the
    !> axes' labels made again.
    subroutine relayout_labels(self)
        class(ReferenceFrame), intent(inout) :: self
        if (.not. self%realized) return
        if (c_associated(self%name_text)) then
            if (.not. label_set_layout(self%name_text, [0.0_wp, 0.0_wp, 0.0_wp], real(self%label_size_, wp), 0.0_wp, &
                                       self%label_alignment_)) call check_status(-1_c_int)
        end if
        call remake_axes(self)
    end subroutine relayout_labels

    !> Axis k's arrow and label, in their switches (in place of the old ones;
    !> compiled first if the window already runs).
    subroutine make_axis(self, k)
        class(ReferenceFrame), intent(inout) :: self
        integer, intent(in) :: k                            !! the axis: 1 (x), 2 (y) or 3 (z)
        real(wp) :: e(3), len, head, rb, rh, tip(3)
        e = 0.0_wp
        e(k) = 1.0_wp
        len = self%axis_len(k)
        head = self%axis_head_ratio(k)
        if (head <= 0.0_wp .or. head >= 1.0_wp) head = 0.3_wp
        rb = self%axis_body_radius(k)
        if (rb <= 0.0_wp) rb = 0.05_wp * len
        rh = self%axis_head_radius(k)
        if (rh <= 0.0_wp) rh = 0.1_wp * len
        call replace_child(self, self%axis_node(k), self%axis_arrow(k), &
                           arrow_create(self%axis_base(:, k), e, (1.0_wp - head) * len, head * len, rb, rh, &
                                        real(self%color_, c_float)))
        ! the label at the tip, 0.4 of the axis length tall, at most label_size pixels
        tip = self%axis_base(:, k) + len * e
        call replace_child(self, self%axis_label_node(k), self%axis_text(k), &
                           label_create(self%axis_label(2**(k-1)), tip, self%color_, -0.4_wp * len, &
                                        real(self%label_size_, wp), self%label_alignment_, self%label_font()))
        call update_switches(self)                              ! (a node added to a switch is shown)
    end subroutine make_axis

    !> Put `new` in `switch` in place of `old` (a kept handle, deleted).
    subroutine replace_child(self, switch, old, new)
        class(ReferenceFrame), intent(in) :: self
        type(c_ptr), intent(in) :: switch, new
        type(c_ptr), intent(inout) :: old
        if (c_associated(old)) then
            call check_status(vkf_node_remove_child(switch, old))
            call vkf_node_delete(old)
        end if
        old = new
        if (.not. c_associated(new)) return
        if (self%shown_) call check_status(vkf_compile_node(new))
        call check_status(vkf_node_add_child(switch, new))
    end subroutine replace_child

    !> Show or hide the decorations' switches as set.
    subroutine update_switches(self)
        class(ReferenceFrame), intent(inout) :: self
        integer :: k
        if (.not. self%realized) return
        do k = 1, 3
            if (c_associated(self%axis_node(k))) &
                call check_status(vkf_switch_set(self%axis_node(k), merge(1_c_int, 0_c_int, iand(self%axes, 2**(k-1)) /= 0)))
            if (c_associated(self%axis_label_node(k))) &
                call check_status(vkf_switch_set(self%axis_label_node(k), &
                                          merge(1_c_int, 0_c_int, iand(self%axes_labels, 2**(k-1)) /= 0)))
        end do
        if (c_associated(self%name_label_node)) &
            call check_status(vkf_switch_set(self%name_label_node, merge(1_c_int, 0_c_int, self%name_label)))
    end subroutine update_switches

    ! ==================================================================
    ! helpers

    !> True if `a` and `b` are the same frame (object).
    logical function same_frame(a, b)
        class(ReferenceFrame), intent(in), target :: a, b
        class(ReferenceFrame), pointer :: pa
        pa => a
        same_frame = associated(pa, b)
    end function same_frame

    !> True if `frame` is `root` or one of its descendants.
    recursive logical function is_descendant(root, frame) result(yes)
        class(ReferenceFrame), intent(in), target :: root, frame
        integer :: k
        yes = same_frame(root, frame)
        do k = 1, root%n_children
            if (yes) return
            yes = is_descendant(root%children(k)%p, frame)
        end do
    end function is_descendant

    !> Index of `frame` in list(1:n), or 0.
    integer function find(list, n, frame)
        type(FramePointer), allocatable, intent(in) :: list(:)  !! first n used
        integer, intent(in) :: n
        class(ReferenceFrame), intent(in), target :: frame
        integer :: k
        find = 0
        do k = 1, n
            if (associated(list(k)%p, frame)) then
                find = k
                return
            end if
        end do
    end function find

    !> Add `frame` to list(1:n) (growing the array).
    subroutine append(list, n, frame)
        type(FramePointer), allocatable, intent(inout) :: list(:)  !! first n used (grown as needed)
        integer, intent(inout) :: n
        class(ReferenceFrame), intent(in), target :: frame
        type(FramePointer), allocatable :: tmp(:)
        if (.not. allocated(list)) allocate(list(4))
        if (n == size(list)) then
            allocate(tmp(2 * n))
            tmp(1:n) = list(1:n)
            call move_alloc(tmp, list)
        end if
        n = n + 1
        list(n)%p => frame
    end subroutine append

    !> Remove `frame` from list(1:n); false if it is not in it.
    logical function remove(list, n, frame)
        type(FramePointer), allocatable, intent(inout) :: list(:)  !! first n used
        integer, intent(inout) :: n
        class(ReferenceFrame), intent(in), target :: frame
        integer :: k
        k = find(list, n, frame)
        remove = k > 0
        if (.not. remove) return
        list(k:n-1) = list(k+1:n)
        list(n)%p => null()
        n = n - 1
    end function remove

end module vkf_frames
