!> Checks Sphere and Model settings (no window): shape, sphere and model
!> transforms, frame_info in the tree, a missing model file, FrameManager.
program test_bodies

    use vkframes

    implicit none

    type(Sphere), target :: earth
    type(Model), target :: craft
    type(FrameManager) :: scene
    class(ReferenceFrame), pointer :: root
    character(len=*), parameter :: nl = new_line('a')
    integer :: failures

    failures = 0
    earth = Sphere('Earth', radius=6378.137_wp)
    call check(earth%radius() == 6378.137_wp .and. all(earth%radii() == 6378.137_wp), 'sphere radius')
    call earth%set_sphere_scale([1.0_wp, 1.0_wp, 0.5_wp])
    call check(all(earth%radii() == [6378.137_wp, 6378.137_wp, 3189.0685_wp]), 'set_sphere_scale makes an ellipsoid')
    call earth%set_sphere_scale([1.0_wp, 0.0_wp, 1.0_wp])
    call check(all(earth%sphere_scale() == [1.0_wp, 1.0_wp, 0.5_wp]), 'non-positive scale ignored')
    call earth%set_texture_map('earth.jpg')
    call check(earth%texture_map() == 'earth.jpg' .and. earth%night_texture_map() == '', 'texture maps')
    call earth%set_sphere_attitude([0.0_wp, 0.0_wp, 2.0_wp, 0.0_wp])
    call check(all(earth%sphere_attitude() == [0.0_wp, 0.0_wp, 1.0_wp, 0.0_wp]), 'sphere attitude normalized')

    craft = Model('Craft')
    call check(.not. craft%set_model('does/not/exist.3ds') .and. .not. craft%has_model(), 'missing model file')
    call craft%set_model_scale([2.0_wp, 2.0_wp, 2.0_wp])
    call craft%set_model_pivot([1.0_wp, 0.0_wp, 0.0_wp])
    call check(all(craft%model_scale() == 2.0_wp) .and. all(craft%model_pivot() == [1.0_wp, 0.0_wp, 0.0_wp]), &
               'model transform settings')

    call check(earth%add_child(craft), 'add_child')
    call check(earth%frame_string() == ' Earth (Sphere)'//nl//' |-->Craft (Model)'//nl, 'frame_info in frame_string')

    scene = FrameManager(earth)
    root => scene%frame()
    call check(associated(root, earth), 'FrameManager root')

    if (failures > 0) then
        write(*, '(a,i0,a)') 'FAIL: ', failures, ' checks failed'
        error stop 1
    end if
    write(*, '(a)') 'PASS: Sphere, Model, FrameManager'

contains

    subroutine check(ok, what)
        logical, intent(in) :: ok
        character(len=*), intent(in) :: what
        if (.not. ok) then
            write(*, '(a)') '  failed: '//what
            failures = failures + 1
        end if
    end subroutine check

end program test_bodies
