!> Models: a 3D model file (3ds, obj, glTF, ... through vsgXchange) in a
!> frame, as OpenFrames' Model.
!>
!> The model is placed in its frame by its own transform:
!> translate(model_position) * rotate(model_attitude) * scale(model_scale) *
!> translate(-model_pivot), so e.g. the pivot can be the model's center
!> (`model_bound`) and the scale can size it.
module vkf_model

    use, intrinsic :: iso_c_binding, only: c_ptr, c_null_ptr, c_associated, c_int
    use vkf_math, only: wp, quat_identity, quat_normalize, frame_matrix
    use vkf_frames, only: ReferenceFrame
    use vkf_layer, only: vkf_model_read, vkf_node_bound, vkf_transform_create, vkf_transform_set_matrix, &
                         vkf_node_add_child

    implicit none

    private

    !> A model (see the module documentation).
    type, extends(ReferenceFrame), public :: Model
        private
        character(len=:), allocatable :: file
        type(c_ptr) :: model_ = c_null_ptr                  !! the loaded model's node
        real(wp) :: model_position_(3) = 0.0_wp
        real(wp) :: model_attitude_(4) = quat_identity
        real(wp) :: scale_(3) = 1.0_wp
        real(wp) :: pivot_(3) = 0.0_wp
        real(wp) :: bound_center(3) = 0.0_wp, bound_radius = 0.0_wp
        type(c_ptr) :: model_node = c_null_ptr               !! transform of the model within the frame
    contains
        procedure :: set_model, share_model, model_file, has_model, model_bound
        procedure :: set_model_position, model_position
        procedure :: set_model_attitude, model_attitude
        procedure :: set_model_scale, model_scale
        procedure :: set_model_pivot, model_pivot
        procedure :: frame_info => model_frame_info
        procedure :: build_contents => model_build_contents
    end type Model

    interface Model
        module procedure new_model
    end interface Model

contains

    !> A model frame named `name` (decorations of color `color`), with the
    !> model file `file` if given (see `set_model`).
    function new_model(name, file, color) result(m)
        character(len=*), intent(in) :: name
        character(len=*), intent(in), optional :: file
        real(wp), intent(in), optional :: color(:)
        type(Model) :: m
        logical :: ok
        m%ReferenceFrame = ReferenceFrame(name, color)
        if (present(file)) ok = m%set_model(file)
    end function new_model

    !> Read the model file (now, so its bound is known). Returns false if it
    !> could not be read. Set it before a window first shows the model.
    logical function set_model(self, file) result(ok)
        class(Model), intent(inout) :: self
        character(len=*), intent(in) :: file
        type(c_ptr) :: node
        node = vkf_model_read(file, len(file, kind=c_int))
        ok = c_associated(node)
        if (.not. ok) return
        self%file = file
        self%model_ = node
        if (vkf_node_bound(node, self%bound_center, self%bound_radius) /= 0) then
            self%bound_center = 0.0_wp
            self%bound_radius = 0.0_wp
        end if
    end function set_model

    !> Use the model already read by `other` (no second copy in memory).
    logical function share_model(self, other) result(ok)
        class(Model), intent(inout) :: self
        class(Model), intent(in) :: other
        ok = c_associated(other%model_)
        if (.not. ok) return
        self%model_ = other%model_
        self%file = other%file
        self%bound_center = other%bound_center
        self%bound_radius = other%bound_radius
    end function share_model

    function model_file(self) result(file)
        class(Model), intent(in) :: self
        character(len=:), allocatable :: file
        file = ''
        if (allocated(self%file)) file = self%file
    end function model_file

    pure logical function has_model(self)
        class(Model), intent(in) :: self
        has_model = c_associated(self%model_)
    end function has_model

    !> The model's bounding sphere, in the model's own coordinates (before its transform).
    subroutine model_bound(self, center, radius)
        class(Model), intent(in) :: self
        real(wp), intent(out) :: center(3), radius
        center = self%bound_center
        radius = self%bound_radius
    end subroutine model_bound

    subroutine set_model_position(self, position)
        class(Model), intent(inout) :: self
        real(wp), intent(in) :: position(3)
        self%model_position_ = position
        call update_model_transform(self)
    end subroutine set_model_position

    pure function model_position(self) result(p)
        class(Model), intent(in) :: self
        real(wp) :: p(3)
        p = self%model_position_
    end function model_position

    !> Attitude of the model in its frame (quaternion [x, y, z, w]).
    subroutine set_model_attitude(self, q)
        class(Model), intent(inout) :: self
        real(wp), intent(in) :: q(4)
        self%model_attitude_ = quat_normalize(q)
        call update_model_transform(self)
    end subroutine set_model_attitude

    pure function model_attitude(self) result(q)
        class(Model), intent(in) :: self
        real(wp) :: q(4)
        q = self%model_attitude_
    end function model_attitude

    !> Scale along the model's x, y and z axes.
    subroutine set_model_scale(self, scale)
        class(Model), intent(inout) :: self
        real(wp), intent(in) :: scale(3)
        self%scale_ = scale
        call update_model_transform(self)
    end subroutine set_model_scale

    pure function model_scale(self) result(s)
        class(Model), intent(in) :: self
        real(wp) :: s(3)
        s = self%scale_
    end function model_scale

    !> The point of the model (in its own coordinates) placed at model_position,
    !> about which it is rotated and scaled.
    subroutine set_model_pivot(self, pivot)
        class(Model), intent(inout) :: self
        real(wp), intent(in) :: pivot(3)
        self%pivot_ = pivot
        call update_model_transform(self)
    end subroutine set_model_pivot

    pure function model_pivot(self) result(p)
        class(Model), intent(in) :: self
        real(wp) :: p(3)
        p = self%pivot_
    end function model_pivot

    function model_frame_info(self) result(info)
        class(Model), intent(in) :: self
        character(len=:), allocatable :: info
        info = 'Model'
    end function model_frame_info

    !> The model under the model transform.
    subroutine model_build_contents(self, viewer, group)
        class(Model), intent(inout) :: self
        type(c_ptr), intent(in) :: viewer, group
        if (.not. c_associated(self%model_)) return
        self%model_node = vkf_transform_create()
        call update_model_transform(self)
        if (vkf_node_add_child(self%model_node, self%model_) /= 0 .or. vkf_node_add_child(group, self%model_node) /= 0) &
            write(*, '(a)') 'vkframes: Model '//self%name()//': could not build the scene'
    end subroutine model_build_contents

    subroutine update_model_transform(self)
        type(Model), intent(inout) :: self
        if (.not. c_associated(self%model_node)) return
        if (vkf_transform_set_matrix(self%model_node, &
                frame_matrix(self%model_position_, self%model_attitude_, self%scale_, self%pivot_)) /= 0) &
            write(*, '(a)') 'vkframes: Model '//self%name()//': transform failed'
    end subroutine update_model_transform

end module vkf_model
