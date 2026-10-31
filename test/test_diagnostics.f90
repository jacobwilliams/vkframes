!> Checks the diagnostics (vkf_diagnostics, in a window of its own): the
!> libraries' versions, the window's device, its frame rate and frame times
!> after a second of frames (the GPU's time measured, or -1 if the device has
!> no timestamps), and its scene graph's counts, which drop when a body is
!> hidden.
program test_diagnostics

    use, intrinsic :: iso_fortran_env, only: int64
    use vkframes
    use vkf_checks, only: check, report

    implicit none

    type(ReferenceFrame), target :: root
    type(Sphere), target :: ball
    type(FrameManager), target :: scene
    type(WindowProxy) :: window
    type(LibraryVersions) :: versions
    type(DeviceInfo) :: device
    type(RenderStats) :: stats
    type(SceneCounts) :: shown, hidden
    real(wp) :: start
    integer :: k

    versions = library_versions()
    write(*, '(a)') 'vkframes '//versions%vkframes//', VSG '//versions%vsg//', vsgXchange '//versions%vsgxchange// &
                    ', Dear ImGui '//versions%imgui//', Vulkan headers '//versions%vulkan_headers// &
                    ', Vulkan loader '//versions%vulkan_loader
    call check(versions%vkframes == vkframes_version .and. versions%vsg == '1.1.16', 'versions: vkframes and VSG')
    call check(len(versions%vsgxchange) > 0 .and. len(versions%imgui) > 0 .and. len(versions%vulkan_headers) > 0 .and. &
               len(versions%vulkan_loader) > 0, 'versions: vsgXchange, Dear ImGui, Vulkan')

    root = ReferenceFrame('Root')
    ball = Sphere('Ball', radius=1.0_wp)
    if (.not. root%add_child(ball)) error stop 'add_child failed'
    scene = FrameManager(root)
    if (.not. window%open(320, 240, 'vkframes: diagnostics test')) then
        write(*, '(a)') 'SKIP: no window'
        stop
    end if
    call window%set_scene(scene)
    stats = window%render_stats()
    call check(stats%frames == 0_int64 .and. stats%fps == 0.0_wp, 'no frames yet')

    device = window%device_info()
    write(*, '(a)') 'device: '//device%name//' ('//device%kind//', Vulkan '//device%vulkan//'), driver: '// &
                    device%driver//' '//device%driver_info
    call check(len(device%name) > 0 .and. len(device%kind) > 0 .and. len(device%vulkan) > 0, 'device info')

    start = wall_time()
    k = 0
    do while (wall_time() - start < 1.2_wp .or. k < 10)      ! (over the rate's half second, twice)
        if (.not. window%frame()) error stop 'the window closed'
        k = k + 1
    end do
    stats = window%render_stats()
    write(*, '(a,i0,a,f7.1,a,f6.2,a,f6.2,a,f6.2,a,f6.2,a,f6.2,a,f6.2)') 'frames ', stats%frames, ', ', stats%fps, &
        ' fps, frame ', stats%frame_ms, ' ms: program ', stats%program_ms, ', acquire ', stats%acquire_ms, &
        ', record ', stats%record_ms, ', present ', stats%present_ms, ', GPU ', stats%gpu_ms
    write(*, '(a,i0,a,i0,a,es10.3,a,es10.3)') 'image ', stats%width, ' x ', stats%height, ', GPU memory ', &
        stats%gpu_memory_used, ' of ', stats%gpu_memory_budget
    call check(stats%frames == int(k, int64), 'frames drawn')
    call check(stats%fps > 0.0_wp .and. stats%frame_ms > 0.0_wp, 'frame rate')
    call check(stats%acquire_ms >= 0.0_wp .and. stats%record_ms > 0.0_wp .and. stats%present_ms >= 0.0_wp .and. &
               stats%program_ms >= 0.0_wp, 'frame times')
    call check(stats%gpu_ms > 0.0_wp .or. stats%gpu_ms == -1.0_wp, 'GPU time: measured, or -1')
    call check(stats%width > 0 .and. stats%height > 0, 'image size')
    call check(stats%gpu_memory_budget == -1.0_wp .or. &
               (stats%gpu_memory_used > 0.0_wp .and. stats%gpu_memory_budget >= stats%gpu_memory_used), 'GPU memory')

    shown = window%scene_counts()
    call ball%show_contents(.false.)
    if (.not. window%frame()) error stop 'the window closed'
    hidden = window%scene_counts()
    write(*, '(a,3(i0,a))') 'scene graph: ', shown%nodes, ' nodes, ', shown%draws, ' draws, ', shown%vertices, ' vertices'
    call check(shown%nodes > 0 .and. shown%draws > 0 .and. shown%vertices > 0, 'scene counts')
    call check(hidden%draws < shown%draws .and. hidden%vertices < shown%vertices, 'a hidden body is not counted')

    call window%close()
    call report('diagnostics (versions, device, frame rate and times, scene counts)')

end program test_diagnostics
