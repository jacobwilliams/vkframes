!> Checks that the Vulkan loader finds a physical device (MoltenVK on macOS).
!> Needs no window or display.
program test_vulkan

    use vkf_layer, only: vulkan_device_name

    implicit none

    character(len=:), allocatable :: name

    name = vulkan_device_name()
    if (len(name) == 0) then
        write(*, '(a)') 'FAIL: no Vulkan physical device found'
        error stop 1
    end if
    write(*, '(a)') 'PASS: Vulkan device: '//name

end program test_vulkan
