!> Views: where the camera looks from, as OpenFrames' View.
!>
!> A view follows a frame: the camera's trackball (eye, center, up, which the
!> mouse moves) is kept in the view frame's coordinates, so the camera moves
!> with the frame. The view frame is
!> * RELATIVE_FRAME (the default): the viewed frame itself (a body-fixed view);
!> * ABSOLUTE_FRAME: the root frame's axes, at the viewed frame's origin (an
!>   inertial view that follows the frame as it moves).
!> A view between frames (`set_view_between_frames`) looks from one frame at
!> another: the view frame is turned so that its +y axis (the trackball's
!> initial viewing direction) points at the second frame, in one direct rotation
!> (VIEW_DIRECT) or first in azimuth, then elevation (VIEW_AZEL, the default,
!> which keeps z up).
!>
!> The default camera (`reset_view`) looks along +y at the frame's bound from the
!> distance at which it fills the field of view (or from `default_view_distance`,
!> or from `set_default_view_parameters`). Views are not owned by windows: a
!> view, and the frames it refers to, must stay alive while used.
module vkf_view

    use vkf_math, only: wp, pi, quat_from_vectors, quat_to_matrix, translation_matrix, identity_matrix, &
                        rigid_inverse
    use vkf_frames, only: ReferenceFrame

    implicit none

    private

    ! the base frame of the trackball (OpenFrames' ViewFrameType)
    integer, parameter, public :: ABSOLUTE_FRAME = 0
    integer, parameter, public :: RELATIVE_FRAME = 1

    ! how a view between frames turns toward the look-at frame (OpenFrames' ViewRotationType)
    integer, parameter, public :: VIEW_DIRECT = 0
    integer, parameter, public :: VIEW_AZEL = 1

    ! projections (OpenFrames' ProjectionType)
    integer, parameter, public :: ORTHOGRAPHIC = 0
    integer, parameter, public :: PERSPECTIVE = 1

    !> A view (see the module documentation).
    type, public :: View
        private
        class(ReferenceFrame), pointer :: root => null()
        class(ReferenceFrame), pointer :: frame => null()
        class(ReferenceFrame), pointer :: lookat => null()
        integer :: frame_type = RELATIVE_FRAME
        integer :: rotation_type = VIEW_AZEL
        integer :: projection = PERSPECTIVE
        real(wp) :: fovy = 45.0_wp                          !! degrees
        real(wp) :: ortho(4) = [-1.0_wp, 1.0_wp, -1.0_wp, 1.0_wp]   !! left, right, bottom, top
        real(wp) :: default_distance = 0.0_wp
        real(wp) :: default_eye(3) = 0.0_wp, default_center(3) = 0.0_wp, default_up(3) = 0.0_wp
        ! the trackball state of this view (view-frame coordinates), kept while another view is shown
        real(wp) :: eye(3) = 0.0_wp, center(3) = 0.0_wp, up(3) = 0.0_wp
        logical :: has_state = .false.
        logical :: reset_pending = .true.
    contains
        procedure :: set_view_frame, set_view_between_frames
        procedure :: view_root, view_frame, lookat_frame, view_frame_type, view_rotation_type, is_valid
        procedure :: set_perspective, field_of_view, set_orthographic, orthographic_bounds, projection_type
        procedure :: set_default_view_distance, default_view_distance
        procedure :: set_default_view_parameters, default_view_parameters, clear_default_view_parameters
        procedure :: reset_view
        procedure :: world_to_view, home
        procedure :: save_state, saved_state, take_reset
    end type View

    interface View
        module procedure new_view
    end interface View

contains

    !> A view of `frame` (in the tree of `root`), with base `frame_type`
    !> (default RELATIVE_FRAME); with `lookat`, a view from `frame` at `lookat`,
    !> turned by `rotation_type` (default VIEW_AZEL). Without frames, the view
    !> looks at the root frame's origin. The frames must stay alive while the
    !> view is used.
    function new_view(root, frame, lookat, frame_type, rotation_type) result(v)
        class(ReferenceFrame), intent(in), target, optional :: root, frame, lookat
        integer, intent(in), optional :: frame_type, rotation_type
        type(View) :: v
        integer :: ft, rt
        ft = RELATIVE_FRAME
        rt = VIEW_AZEL
        if (present(frame_type)) ft = frame_type
        if (present(rotation_type)) rt = rotation_type
        if (present(root)) v%root => root
        if (present(frame)) v%frame => frame
        if (present(lookat)) v%lookat => lookat
        v%frame_type = ft
        v%rotation_type = rt
    end function new_view

    !> View `frame` (in the tree of `root`) with base `frame_type` (default RELATIVE_FRAME).
    subroutine set_view_frame(self, root, frame, frame_type)
        class(View), intent(inout) :: self
        class(ReferenceFrame), intent(in), target :: root, frame
        integer, intent(in), optional :: frame_type
        self%root => root
        self%frame => frame
        self%lookat => null()
        self%frame_type = RELATIVE_FRAME
        if (present(frame_type)) self%frame_type = frame_type
    end subroutine set_view_frame

    !> Look from `frame` at `lookat` (both in the tree of `root`).
    subroutine set_view_between_frames(self, root, frame, lookat, frame_type, rotation_type)
        class(View), intent(inout) :: self
        class(ReferenceFrame), intent(in), target :: root, frame, lookat
        integer, intent(in), optional :: frame_type, rotation_type
        self%root => root
        self%frame => frame
        self%lookat => lookat
        self%frame_type = RELATIVE_FRAME
        self%rotation_type = VIEW_AZEL
        if (present(frame_type)) self%frame_type = frame_type
        if (present(rotation_type)) self%rotation_type = rotation_type
    end subroutine set_view_between_frames

    function view_root(self) result(f)
        class(View), intent(in) :: self
        class(ReferenceFrame), pointer :: f
        f => self%root
    end function view_root

    function view_frame(self) result(f)
        class(View), intent(in) :: self
        class(ReferenceFrame), pointer :: f
        f => self%frame
    end function view_frame

    function lookat_frame(self) result(f)
        class(View), intent(in) :: self
        class(ReferenceFrame), pointer :: f
        f => self%lookat
    end function lookat_frame

    pure integer function view_frame_type(self)
        class(View), intent(in) :: self
        view_frame_type = self%frame_type
    end function view_frame_type

    pure integer function view_rotation_type(self)
        class(View), intent(in) :: self
        view_rotation_type = self%rotation_type
    end function view_rotation_type

    !> True if the view has a frame to view.
    logical function is_valid(self)
        class(View), intent(in) :: self
        is_valid = associated(self%frame)
    end function is_valid

    ! ==================================================================
    ! projection and defaults

    !> A perspective projection with vertical field of view `fovy` (degrees; default 45).
    subroutine set_perspective(self, fovy)
        class(View), intent(inout) :: self
        real(wp), intent(in) :: fovy
        self%projection = PERSPECTIVE
        self%fovy = fovy
    end subroutine set_perspective

    !> The vertical field of view (degrees) of the perspective projection.
    pure real(wp) function field_of_view(self) result(fovy)
        class(View), intent(in) :: self
        fovy = self%fovy
    end function field_of_view

    !> An orthographic projection with the given bounds (in view units).
    subroutine set_orthographic(self, left, right, bottom, top)
        class(View), intent(inout) :: self
        real(wp), intent(in) :: left, right, bottom, top
        self%projection = ORTHOGRAPHIC
        self%ortho = [left, right, bottom, top]
    end subroutine set_orthographic

    !> The bounds [left, right, bottom, top] of the orthographic projection.
    pure function orthographic_bounds(self) result(bounds)
        class(View), intent(in) :: self
        real(wp) :: bounds(4)
        bounds = self%ortho
    end function orthographic_bounds

    pure integer function projection_type(self)
        class(View), intent(in) :: self
        projection_type = self%projection
    end function projection_type

    !> The radius to frame when the view is reset (<= 0: the viewed frame's
    !> bound, the default), as OpenFrames' default view distance.
    subroutine set_default_view_distance(self, distance)
        class(View), intent(inout) :: self
        real(wp), intent(in) :: distance
        self%default_distance = distance
    end subroutine set_default_view_distance

    pure real(wp) function default_view_distance(self)
        class(View), intent(in) :: self
        default_view_distance = self%default_distance
    end function default_view_distance

    !> The camera after a reset, in view-frame coordinates (overrides the
    !> default view distance).
    subroutine set_default_view_parameters(self, eye, center, up)
        class(View), intent(inout) :: self
        real(wp), intent(in) :: eye(3), center(3), up(3)
        self%default_eye = eye
        self%default_center = center
        self%default_up = up
    end subroutine set_default_view_parameters

    pure subroutine default_view_parameters(self, eye, center, up)
        class(View), intent(in) :: self
        real(wp), intent(out) :: eye(3), center(3), up(3)
        eye = self%default_eye
        center = self%default_center
        up = self%default_up
    end subroutine default_view_parameters

    subroutine clear_default_view_parameters(self)
        class(View), intent(inout) :: self
        self%default_eye = 0.0_wp
        self%default_center = 0.0_wp
        self%default_up = 0.0_wp
    end subroutine clear_default_view_parameters

    !> Put the camera back at the default position (applied at the next frame
    !> in which the view is shown).
    subroutine reset_view(self)
        class(View), intent(inout) :: self
        self%reset_pending = .true.
    end subroutine reset_view

    ! ==================================================================
    ! camera (used by the window showing the view)

    !> The transform from the root frame to the view frame (4x4), from the
    !> frames' current positions and attitudes.
    function world_to_view(self) result(m)
        class(View), intent(in) :: self
        real(wp) :: m(4,4)
        real(wp) :: f(4,4), rel(3), xy(3), r(3,3), len
        real(wp), parameter :: eps = 1.0e-6_wp
        m = identity_matrix()
        if (.not. associated(self%frame)) return
        f = frame_to_root(self, self%frame)
        if (self%frame_type == ABSOLUTE_FRAME) then
            m = translation_matrix(-f(1:3, 4))
        else
            m = rigid_inverse(f)
        end if
        if (.not. associated(self%lookat)) return
        ! the look-at frame's origin in the view frame, turned onto +y
        f = frame_to_root(self, self%lookat)
        rel = matmul(m(1:3, 1:3), f(1:3, 4)) + m(1:3, 4)
        if (norm2(rel) <= eps) return
        if (self%rotation_type == VIEW_DIRECT) then
            m = rotated(quat_to_matrix(quat_from_vectors(rel, [0.0_wp, 1.0_wp, 0.0_wp])), m)
        else
            xy = [rel(1), rel(2), 0.0_wp]
            len = norm2(xy)
            if (len > eps) m = rotated(quat_to_matrix(quat_from_vectors(xy, [0.0_wp, 1.0_wp, 0.0_wp])), m)
            r = quat_to_matrix(quat_from_vectors([0.0_wp, len, rel(3)], [0.0_wp, 1.0_wp, 0.0_wp]))
            m = rotated(r, m)
        end if
    end function world_to_view

    !> The default camera (eye, center, up in view-frame coordinates) for a
    !> viewport of aspect ratio `aspect` (width / height): OpenFrames' resetView.
    subroutine home(self, aspect, eye, center, up)
        class(View), intent(in) :: self
        real(wp), intent(in) :: aspect
        real(wp), intent(out) :: eye(3), center(3), up(3)
        real(wp) :: c(3), r, dist, half_fovy, half_fovx, m(4,4)
        if (norm2(self%default_up) /= 0.0_wp) then
            eye = self%default_eye
            center = self%default_center
            up = self%default_up
            return
        end if
        c = 0.0_wp
        r = 0.0_wp
        if (associated(self%frame)) then
            call self%frame%bound(c, r)
            m = matmul(self%world_to_view(), frame_to_root(self, self%frame))    ! frame to view frame
            c = matmul(m(1:3, 1:3), c) + m(1:3, 4)
        end if
        if (self%default_distance > 0.0_wp) then
            r = self%default_distance
        else if (r <= 0.0_wp) then
            r = 1.0_wp
        end if
        dist = 2.0_wp * r
        if (self%projection == PERSPECTIVE) then
            half_fovy = 0.5_wp * self%fovy * pi / 180.0_wp
            half_fovx = atan(aspect * tan(half_fovy))
            dist = r / sin(min(half_fovy, half_fovx))
        end if
        center = c
        eye = c + [0.0_wp, -dist, 0.0_wp]
        up = [0.0_wp, 0.0_wp, 1.0_wp]
    end subroutine home

    !> Keep the trackball state while another view is shown.
    subroutine save_state(self, eye, center, up)
        class(View), intent(inout) :: self
        real(wp), intent(in) :: eye(3), center(3), up(3)
        self%eye = eye
        self%center = center
        self%up = up
        self%has_state = .true.
    end subroutine save_state

    !> The kept trackball state; false if there is none.
    logical function saved_state(self, eye, center, up)
        class(View), intent(in) :: self
        real(wp), intent(out) :: eye(3), center(3), up(3)
        saved_state = self%has_state
        eye = self%eye
        center = self%center
        up = self%up
    end function saved_state

    !> True (once) if a reset is pending.
    logical function take_reset(self)
        class(View), intent(inout) :: self
        take_reset = self%reset_pending
        self%reset_pending = .false.
        if (take_reset) self%has_state = .false.
    end function take_reset

    ! ==================================================================
    ! helpers

    !> The transform from `f` to the view's root frame (the tree root if none).
    function frame_to_root(self, f) result(m)
        type(View), intent(in) :: self
        class(ReferenceFrame), intent(in) :: f
        real(wp) :: m(4,4)
        m = f%world_matrix()
        if (associated(self%root)) m = matmul(rigid_inverse(self%root%world_matrix()), m)
    end function frame_to_root

    !> rotate(r) * m, for a rotation r (3x3).
    pure function rotated(r, m) result(rm)
        real(wp), intent(in) :: r(3,3), m(4,4)
        real(wp) :: rm(4,4)
        rm = m
        rm(1:3, :) = matmul(r, m(1:3, :))
    end function rotated

end module vkf_view
