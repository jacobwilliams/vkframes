!> Diagnostics (not in OpenFrames): what vkframes is built with and runs on,
!> and how fast a window draws, for an information or debugging panel.
!>
!> * `library_versions()`: the versions of vkframes, VSG, vsgXchange, Dear
!>   ImGui, the Vulkan headers it was built with and the Vulkan loader it runs
!>   with (LibraryVersions).
!> * A window's `device_info()`: its Vulkan device and driver (DeviceInfo).
!> * A window's `render_stats()`: its frame rate and where a frame's time goes
!>   (RenderStats), measured at every frame at no cost: the program's part
!>   (the scene's update and the event handler), and the renderer's: waiting
!>   for the next image to draw into (with vsync, this is mostly waiting for
!>   the display), recording and submitting, and presenting; the GPU's time
!>   for the frame (timestamps around its commands, read frames later, so it
!>   lags a few frames; not measured on devices without timestamps); and the
!>   device's memory in use and its budget (if the driver says).
!> * A window's `scene_counts()`: the size of its scene graph (SceneCounts),
!>   counted when asked (a traversal of the whole graph: for an occasional
!>   update, not every frame).
module vkf_diagnostics

    use, intrinsic :: iso_c_binding, only: c_int, c_char
    use, intrinsic :: iso_fortran_env, only: int64
    use vkf_math, only: wp
    use vkf_layer, only: vkf_version

    implicit none

    private

    !> vkframes' version (fpm.toml's).
    character(len=*), parameter, public :: vkframes_version = '0.0.1'

    !> The versions of vkframes and the libraries it uses (each '' if unknown).
    type, public :: LibraryVersions
        character(len=:), allocatable :: vkframes           !! vkframes
        character(len=:), allocatable :: vsg                !! VulkanSceneGraph
        character(len=:), allocatable :: vsgxchange         !! vsgXchange (images, models, fonts)
        character(len=:), allocatable :: imgui              !! Dear ImGui (GUI panels)
        character(len=:), allocatable :: vulkan_headers     !! the Vulkan headers vkframes was built with
        character(len=:), allocatable :: vulkan_loader      !! the Vulkan loader it runs with (its instance version)
    end type LibraryVersions

    !> A window's Vulkan device and driver (each '' if unknown).
    type, public :: DeviceInfo
        character(len=:), allocatable :: name               !! the device (e.g. "Apple M3 Max")
        character(len=:), allocatable :: kind               !! integrated GPU, discrete GPU, virtual GPU, CPU or other
        character(len=:), allocatable :: vulkan             !! the Vulkan version it supports
        character(len=:), allocatable :: driver             !! its driver's name
        character(len=:), allocatable :: driver_info        !! its driver's information (e.g. its version)
    end type DeviceInfo

    !> A window's frame rate and frame times (see the module documentation).
    !> Times are in milliseconds; the rate and frame time are averaged over
    !> about half a second, the rest are the last frame's.
    type, public :: RenderStats
        real(wp) :: fps = 0.0_wp                            !! frames per second
        real(wp) :: frame_ms = 0.0_wp                       !! the time between frames
        real(wp) :: program_ms = 0.0_wp                    !! the program's part of frame(): the scene's update, the handler
        real(wp) :: acquire_ms = 0.0_wp                     !! waiting for the next image to draw into
        real(wp) :: record_ms = 0.0_wp                      !! recording and submitting the GPU's commands
        real(wp) :: present_ms = 0.0_wp                     !! presenting the image
        real(wp) :: gpu_ms = -1.0_wp                        !! the GPU's time for a frame (-1: not measured)
        real(wp) :: gpu_memory_used = -1.0_wp               !! device memory in use (bytes; -1: unknown)
        real(wp) :: gpu_memory_budget = -1.0_wp             !! device memory available to the program (bytes; -1: unknown)
        integer(int64) :: frames = 0_int64                  !! frames drawn
        integer :: width = 0, height = 0                    !! the image's size (framebuffer pixels)
    end type RenderStats

    !> The size of a window's scene graph: what is switched off isn't counted.
    type, public :: SceneCounts
        integer(int64) :: nodes = 0_int64                   !! nodes
        integer(int64) :: draws = 0_int64                   !! draw commands
        integer(int64) :: vertices = 0_int64                !! vertices drawn (indices for indexed draws, times instances)
    end type SceneCounts

    public :: library_versions

contains

    !> The versions of vkframes and the libraries it uses.
    function library_versions() result(v)
        type(LibraryVersions) :: v
        v%vkframes = vkframes_version
        v%vsg = version_string(1)
        v%vsgxchange = version_string(2)
        v%imgui = version_string(3)
        v%vulkan_headers = version_string(4)
        v%vulkan_loader = version_string(5)
    end function library_versions

    !> The rendering layer's version string `which` (see vkf_version), '' if unknown.
    function version_string(which) result(s)
        integer, intent(in) :: which                        !! 1 VSG ... 5 the Vulkan loader
        character(len=:), allocatable :: s
        character(kind=c_char, len=256) :: buf
        integer(c_int) :: n
        n = vkf_version(int(which, c_int), buf, len(buf, kind=c_int))
        s = ''
        if (n > 0) s = buf(1:min(int(n), len(buf)))
    end function version_string

end module vkf_diagnostics
