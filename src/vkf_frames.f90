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
!> Each frame, `sync` copies positions and attitudes to the transforms. For now
!> a tree must be complete before its window's first frame.
module vkf_frames

    use, intrinsic :: iso_c_binding, only: c_ptr, c_null_ptr, c_associated, c_int
    use vkf_layer, only: vkf_transform_create, vkf_transform_set_matrix, vkf_switch_create, vkf_switch_set, &
                         vkf_node_add_child
    use vkf_math, only: wp, quat_identity, quat_normalize, frame_matrix
    use vkf_trajectory, only: Trajectory
    use vkf_follower, only: TrajectoryFollower, FOLLOW_LOOP, FOLLOW_POSITION, FOLLOW_ATTITUDE

    implicit none

    private

    ! which axes to show (OpenFrames' AxesType), combined with ior
    integer, parameter, public :: NO_AXES = 0
    integer, parameter, public :: X_AXIS = 1
    integer, parameter, public :: Y_AXIS = 2
    integer, parameter, public :: Z_AXIS = 4
    integer, parameter, public :: ALL_AXES = 7

    ! text alignment of labels (osgText's AlignmentType, as OpenFrames uses)
    integer, parameter, public :: ALIGN_LEFT_TOP = 0, ALIGN_LEFT_CENTER = 1, ALIGN_LEFT_BOTTOM = 2
    integer, parameter, public :: ALIGN_CENTER_TOP = 3, ALIGN_CENTER_CENTER = 4, ALIGN_CENTER_BOTTOM = 5
    integer, parameter, public :: ALIGN_RIGHT_TOP = 6, ALIGN_RIGHT_CENTER = 7, ALIGN_RIGHT_BOTTOM = 8
    integer, parameter, public :: ALIGN_LEFT_BASE_LINE = 9, ALIGN_CENTER_BASE_LINE = 10, ALIGN_RIGHT_BASE_LINE = 11

    type :: FramePointer
        class(ReferenceFrame), pointer :: p => null()
    end type FramePointer

    !> A reference frame (see the module documentation).
    type, public :: ReferenceFrame
        private
        character(len=:), allocatable :: name_
        real(wp) :: color_(4) = [1.0_wp, 1.0_wp, 1.0_wp, 0.9_wp]
        real(wp) :: position_(3) = 0.0_wp
        real(wp) :: attitude_(4) = quat_identity
        type(FramePointer), allocatable :: children(:), parents(:)
        integer :: n_children = 0, n_parents = 0
        type(TrajectoryFollower) :: follower_
        logical :: following = .false.
        ! decorations (drawn by the renderer)
        integer :: axes = NO_AXES
        integer :: axes_labels = NO_AXES
        logical :: name_label = .true.
        character(len=:), allocatable :: x_label_, y_label_, z_label_
        character(len=:), allocatable :: label_font_
        integer :: label_size_ = 30
        integer :: label_alignment_ = ALIGN_LEFT_BASE_LINE
        logical :: light_source = .false.
        logical :: contents = .true.
        ! rendering
        type(c_ptr) :: node_ = c_null_ptr                   !! the frame's transform node
        type(c_ptr) :: contents_node = c_null_ptr           !! switch holding the contents
        logical :: realized = .false.
    contains
        procedure :: set_name, name
        procedure :: set_color, color
        procedure :: set_position, position
        procedure :: set_attitude, attitude
        procedure :: local_matrix, world_matrix
        procedure :: show_axes, axes_shown
        procedure :: show_axes_labels, axes_labels_shown
        procedure :: show_name_label, name_label_shown
        procedure :: set_x_label, set_y_label, set_z_label, axis_label
        procedure :: set_label_font, label_font
        procedure :: set_label_size, label_size
        procedure :: set_label_alignment, label_alignment
        procedure :: set_light_source_enabled, light_source_enabled
        procedure :: show_contents, contents_shown
        procedure :: frame_info
        procedure :: add_child, remove_child, detach
        procedure :: num_children, child, child_index
        procedure :: num_parents, parent, parent_index
        procedure :: frame_string
        procedure :: follow, unfollow, follower, is_following
        procedure :: update
        procedure :: realize, build_contents, sync, node
        procedure :: bound
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

    subroutine set_name(self, name)
        class(ReferenceFrame), intent(inout) :: self
        character(len=*), intent(in) :: name
        self%name_ = name
    end subroutine set_name

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
    end subroutine set_color

    pure function color(self) result(c)
        class(ReferenceFrame), intent(in) :: self
        real(wp) :: c(4)
        c = self%color_
    end function color

    !> Position relative to the parent frame (overridden by a followed trajectory).
    subroutine set_position(self, position)
        class(ReferenceFrame), intent(inout) :: self
        real(wp), intent(in) :: position(3)
        self%position_ = position
    end subroutine set_position

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

    ! ==================================================================
    ! decorations

    !> Which axes vectors to show: NO_AXES, or X_AXIS, Y_AXIS, Z_AXIS combined with ior.
    subroutine show_axes(self, axes)
        class(ReferenceFrame), intent(inout) :: self
        integer, intent(in) :: axes
        self%axes = iand(axes, ALL_AXES)
    end subroutine show_axes

    pure integer function axes_shown(self)
        class(ReferenceFrame), intent(in) :: self
        axes_shown = self%axes
    end function axes_shown

    !> Which axes labels to show (as `show_axes`).
    subroutine show_axes_labels(self, labels)
        class(ReferenceFrame), intent(inout) :: self
        integer, intent(in) :: labels
        self%axes_labels = iand(labels, ALL_AXES)
    end subroutine show_axes_labels

    pure integer function axes_labels_shown(self)
        class(ReferenceFrame), intent(in) :: self
        axes_labels_shown = self%axes_labels
    end function axes_labels_shown

    subroutine show_name_label(self, show)
        class(ReferenceFrame), intent(inout) :: self
        logical, intent(in) :: show
        self%name_label = show
    end subroutine show_name_label

    pure logical function name_label_shown(self)
        class(ReferenceFrame), intent(in) :: self
        name_label_shown = self%name_label
    end function name_label_shown

    !> The text of the x-axis label (default 'X').
    subroutine set_x_label(self, text)
        class(ReferenceFrame), intent(inout) :: self
        character(len=*), intent(in) :: text
        self%x_label_ = text
    end subroutine set_x_label

    subroutine set_y_label(self, text)
        class(ReferenceFrame), intent(inout) :: self
        character(len=*), intent(in) :: text
        self%y_label_ = text
    end subroutine set_y_label

    subroutine set_z_label(self, text)
        class(ReferenceFrame), intent(inout) :: self
        character(len=*), intent(in) :: text
        self%z_label_ = text
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
    end subroutine set_label_font

    function label_font(self) result(font)
        class(ReferenceFrame), intent(in) :: self
        character(len=:), allocatable :: font
        if (allocated(self%label_font_)) then
            font = self%label_font_
        else
            font = ''
        end if
    end function label_font

    !> The size of the labels in pixels (default 30): the name label's size,
    !> and the maximum size of the axes labels (which shrink with distance).
    subroutine set_label_size(self, size)
        class(ReferenceFrame), intent(inout) :: self
        integer, intent(in) :: size
        self%label_size_ = size
    end subroutine set_label_size

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
    end subroutine set_label_alignment

    pure integer function label_alignment(self)
        class(ReferenceFrame), intent(in) :: self
        label_alignment = self%label_alignment_
    end function label_alignment

    !> Whether this frame carries a light source (at its origin; default false).
    subroutine set_light_source_enabled(self, enable)
        class(ReferenceFrame), intent(inout) :: self
        logical, intent(in) :: enable
        self%light_source = enable
    end subroutine set_light_source_enabled

    pure logical function light_source_enabled(self)
        class(ReferenceFrame), intent(in) :: self
        light_source_enabled = self%light_source
    end function light_source_enabled

    !> Show or hide what the frame draws (its sphere, model, ...), but not its
    !> axes, labels or children.
    subroutine show_contents(self, show)
        class(ReferenceFrame), intent(inout) :: self
        logical, intent(in) :: show
        self%contents = show
        if (c_associated(self%contents_node)) call check(vkf_switch_set(self%contents_node, merge(1_c_int, 0_c_int, show)))
    end subroutine show_contents

    pure logical function contents_shown(self)
        class(ReferenceFrame), intent(in) :: self
        contents_shown = self%contents
    end function contents_shown

    !> The bounding sphere of what the frame draws, in its own coordinates
    !> (radius 0 if it draws nothing; extensions override it). Views use it for
    !> their default distance.
    subroutine bound(self, center, radius)
        class(ReferenceFrame), intent(in) :: self
        real(wp), intent(out) :: center(3), radius
        center = 0.0_wp
        radius = 0.0_wp
    end subroutine bound

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
    end function add_child

    !> Remove `child` from this frame's children. Returns false if it is not one.
    logical function remove_child(self, child) result(ok)
        class(ReferenceFrame), intent(inout), target :: self
        class(ReferenceFrame), intent(inout), target :: child
        ok = remove(self%children, self%n_children, child)
        if (ok) ok = remove(child%parents, child%n_parents, self)
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
        character(len=*), intent(in), optional :: prefix
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
        type(Trajectory), intent(in), target :: traj
        integer, intent(in), optional :: data, mode
        logical, intent(in), optional :: add
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

    pure logical function is_following(self)
        class(ReferenceFrame), intent(in) :: self
        is_following = self%following .and. self%follower_%num_trajectories() > 0
    end function is_following

    !> Update this frame and its descendants for simulation time `t` (followed
    !> trajectories set positions and attitudes).
    recursive subroutine update(self, t)
        class(ReferenceFrame), intent(inout) :: self
        real(wp), intent(in) :: t
        logical :: has_position, has_attitude
        integer :: k
        if (self%following) &
            call self%follower_%update(t, self%position_, self%attitude_, has_position, has_attitude)
        do k = 1, self%n_children
            call self%children(k)%p%update(t)
        end do
    end subroutine update

    ! ==================================================================
    ! rendering

    !> Create the render nodes of this frame and its descendants (once; for
    !> `viewer`, the window's viewer handle, which some contents need).
    recursive subroutine realize(self, viewer)
        class(ReferenceFrame), intent(inout) :: self
        type(c_ptr), intent(in) :: viewer
        integer :: k
        if (self%realized) return
        self%realized = .true.
        self%node_ = vkf_transform_create()
        self%contents_node = vkf_switch_create()
        call check(vkf_node_add_child(self%node_, self%contents_node))
        call self%build_contents(viewer, self%contents_node)
        if (.not. self%contents) call check(vkf_switch_set(self%contents_node, 0_c_int))
        call self%sync()
        do k = 1, self%n_children
            call self%children(k)%p%realize(viewer)
            call check(vkf_node_add_child(self%node_, self%children(k)%p%node_))
        end do
    end subroutine realize

    !> Add what this frame draws to `group` (a node handle). ReferenceFrame
    !> draws nothing itself (its axes and labels come later); extensions such as
    !> Sphere and Model override this.
    subroutine build_contents(self, viewer, group)
        class(ReferenceFrame), intent(inout) :: self
        type(c_ptr), intent(in) :: viewer, group
    end subroutine build_contents

    !> Copy the positions and attitudes of this frame and its descendants to
    !> their transform nodes.
    recursive subroutine sync(self)
        class(ReferenceFrame), intent(inout) :: self
        integer :: k
        if (.not. self%realized) return
        call check(vkf_transform_set_matrix(self%node_, self%local_matrix()))
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

    subroutine check(status)
        integer(c_int), intent(in) :: status
        if (status /= 0) write(*, '(a)') 'vkframes: a rendering-layer call failed'
    end subroutine check

    ! ==================================================================
    ! helpers

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

    integer function find(list, n, frame)
        type(FramePointer), allocatable, intent(in) :: list(:)
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

    subroutine append(list, n, frame)
        type(FramePointer), allocatable, intent(inout) :: list(:)
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

    logical function remove(list, n, frame)
        type(FramePointer), allocatable, intent(inout) :: list(:)
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
