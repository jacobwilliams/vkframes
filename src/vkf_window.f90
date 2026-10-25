!> Windows: showing a scene of frames, with its simulation time, as OpenFrames'
!> FrameManager and WindowProxy (for now with a single viewport: OpenFrames'
!> grid of RenderRectangles, each with its own views, comes later).
!>
!> A FrameManager holds the root frame of a scene. A WindowProxy shows one: in
!> a window of its own, or in a host's native window (e.g. a Qt QWindow's
!> `winId()`), in which case the host forwards the window's events
!> (`mouse`, `scroll`, `key`, `resize`, `close`). Each `frame()` advances the
!> simulation clock, updates the frames (followed trajectories), copies their
!> positions and attitudes to the renderer and draws.
module vkf_window

    use, intrinsic :: iso_c_binding, only: c_ptr, c_null_ptr, c_associated, c_int, c_long_long, c_double, c_float
    use vkf_math, only: wp
    use vkf_frames, only: ReferenceFrame
    use vkf_clock, only: SimulationClock
    use vkf_layer, only: vkf_viewer_create, vkf_viewer_delete, vkf_viewer_add_node, vkf_viewer_frame, &
                         vkf_viewer_capture, vkf_viewer_look_at, vkf_viewer_perspective, vkf_viewer_set_background, &
                         vkf_viewer_mouse, vkf_viewer_scroll, vkf_viewer_key, vkf_viewer_resize, vkf_viewer_close

    implicit none

    private

    ! mouse event kinds for WindowProxy%mouse
    integer, parameter, public :: MOUSE_MOVE = 1, MOUSE_PRESS = 2, MOUSE_RELEASE = 3

    !> The root frame of a scene (OpenFrames' FrameManager). The frame is not
    !> owned: it must stay alive while managed.
    type, public :: FrameManager
        private
        class(ReferenceFrame), pointer :: root => null()
    contains
        procedure :: set_frame => fm_set_frame
        procedure :: frame => fm_frame
    end type FrameManager

    interface FrameManager
        module procedure new_frame_manager
    end interface FrameManager

    !> A window showing a scene (see the module documentation).
    type, public :: WindowProxy
        private
        type(c_ptr) :: viewer_ = c_null_ptr
        type(FrameManager), pointer :: scene => null()
        type(SimulationClock) :: clock_
        logical :: scene_added = .false.
        real(wp) :: background(3) = [0.0_wp, 0.0_wp, 0.0_wp]
        logical :: running = .false.
    contains
        procedure :: open, close, is_open
        procedure :: set_scene
        procedure :: frame, run
        procedure :: capture
        procedure :: set_background_color, background_color
        procedure :: set_look_at, set_perspective
        ! simulation time (OpenFrames' WindowProxy time control)
        procedure :: set_time, time, pause_time, is_time_paused, set_time_scale, time_scale
        procedure :: set_time_range, time_range, clock
        procedure :: viewer => window_viewer
        ! events from a host window
        procedure :: mouse, scroll, key, resize
    end type WindowProxy

contains

    ! ==================================================================
    ! FrameManager

    !> A frame manager for the scene with root `frame` (optional).
    function new_frame_manager(frame) result(fm)
        class(ReferenceFrame), intent(in), target, optional :: frame
        type(FrameManager) :: fm
        if (present(frame)) fm%root => frame
    end function new_frame_manager

    subroutine fm_set_frame(self, frame)
        class(FrameManager), intent(inout) :: self
        class(ReferenceFrame), intent(in), target :: frame
        self%root => frame
    end subroutine fm_set_frame

    !> The root frame (null if none).
    function fm_frame(self) result(root)
        class(FrameManager), intent(in) :: self
        class(ReferenceFrame), pointer :: root
        root => self%root
    end function fm_frame

    ! ==================================================================
    ! the window

    !> Open the window: `width` x `height` (logical pixels) titled `title`, or,
    !> with `native_window` (a host window's native handle) and its
    !> `pixel_ratio`, render into that window. Returns false on failure.
    logical function open(self, width, height, title, native_window, pixel_ratio) result(ok)
        class(WindowProxy), intent(inout) :: self
        integer, intent(in) :: width, height
        character(len=*), intent(in), optional :: title
        integer(c_long_long), intent(in), optional :: native_window
        real(wp), intent(in), optional :: pixel_ratio
        character(len=:), allocatable :: t
        integer(c_long_long) :: handle
        real(c_double) :: ratio
        t = 'vkframes'
        if (present(title)) t = title
        handle = 0
        ratio = 1.0_c_double
        if (present(native_window)) handle = native_window
        if (present(pixel_ratio)) ratio = pixel_ratio
        self%viewer_ = vkf_viewer_create(int(width, c_int), int(height, c_int), t, len(t, kind=c_int), handle, ratio)
        ok = c_associated(self%viewer_)
        if (.not. ok) return
        self%running = .true.
        self%scene_added = .false.
        call self%set_background_color(self%background)
    end function open

    !> Close the window (waiting for the GPU). The scene is kept, and can be
    !> shown again in another window.
    subroutine close(self)
        class(WindowProxy), intent(inout) :: self
        if (c_associated(self%viewer_)) call vkf_viewer_delete(self%viewer_)
        self%viewer_ = c_null_ptr
        self%running = .false.
    end subroutine close

    pure logical function is_open(self)
        class(WindowProxy), intent(in) :: self
        is_open = c_associated(self%viewer_) .and. self%running
    end function is_open

    !> Show the scene of `fm` (which must stay alive while shown). For now, set
    !> it before the window's first frame.
    subroutine set_scene(self, fm)
        class(WindowProxy), intent(inout) :: self
        type(FrameManager), intent(in), target :: fm
        self%scene => fm
        self%scene_added = .false.
    end subroutine set_scene

    !> Render one frame: advance the clock, update the frames and draw. Returns
    !> false once the window has been closed (or on failure).
    logical function frame(self) result(running)
        class(WindowProxy), intent(inout) :: self
        class(ReferenceFrame), pointer :: root
        real(wp) :: t
        integer(c_int) :: status
        running = .false.
        if (.not. self%is_open()) return
        root => null()
        if (associated(self%scene)) root => self%scene%frame()
        if (associated(root) .and. .not. self%scene_added) then
            call root%realize(self%viewer_)
            if (vkf_viewer_add_node(self%viewer_, root%node()) /= 0) write(*, '(a)') 'vkframes: could not add the scene'
            self%scene_added = .true.
        end if
        t = self%clock_%update()
        if (associated(root)) then
            call root%update(t)
            call root%sync()
        end if
        status = vkf_viewer_frame(self%viewer_)
        self%running = status == 1
        running = self%running
    end function frame

    !> Render frames until the window is closed (or for `max_frames` frames).
    subroutine run(self, max_frames)
        class(WindowProxy), intent(inout) :: self
        integer, intent(in), optional :: max_frames
        integer :: n
        n = 0
        do while (self%frame())
            n = n + 1
            if (present(max_frames)) then
                if (n >= max_frames) exit
            end if
        end do
    end subroutine run

    !> Save the last frame as an image (format from the file extension, e.g.
    !> .png). Returns false on failure.
    logical function capture(self, file) result(ok)
        class(WindowProxy), intent(inout) :: self
        character(len=*), intent(in) :: file
        ok = .false.
        if (.not. self%is_open()) return
        ok = vkf_viewer_capture(self%viewer_, file, len(file, kind=c_int)) == 0
    end function capture

    !> The background color [r, g, b] (0 to 1; default black).
    subroutine set_background_color(self, rgb)
        class(WindowProxy), intent(inout) :: self
        real(wp), intent(in) :: rgb(3)
        self%background = rgb
        if (c_associated(self%viewer_)) then
            if (vkf_viewer_set_background(self%viewer_, real([rgb, 1.0_wp], c_float)) /= 0) &
                write(*, '(a)') 'vkframes: could not set the background color'
        end if
    end subroutine set_background_color

    pure function background_color(self) result(rgb)
        class(WindowProxy), intent(in) :: self
        real(wp) :: rgb(3)
        rgb = self%background
    end function background_color

    !> Place the camera (in the scene's root frame). Interim: Views will set the
    !> camera from the frames they follow.
    subroutine set_look_at(self, eye, center, up)
        class(WindowProxy), intent(inout) :: self
        real(wp), intent(in) :: eye(3), center(3), up(3)
        if (c_associated(self%viewer_)) then
            if (vkf_viewer_look_at(self%viewer_, eye, center, up) /= 0) write(*, '(a)') 'vkframes: set_look_at failed'
        end if
    end subroutine set_look_at

    !> A perspective projection: vertical field of view (degrees) and near and
    !> far distances.
    subroutine set_perspective(self, fovy, near, far)
        class(WindowProxy), intent(inout) :: self
        real(wp), intent(in) :: fovy, near, far
        if (c_associated(self%viewer_)) then
            if (vkf_viewer_perspective(self%viewer_, fovy, near, far) /= 0) write(*, '(a)') 'vkframes: set_perspective failed'
        end if
    end subroutine set_perspective

    ! ==================================================================
    ! simulation time

    subroutine set_time(self, t)
        class(WindowProxy), intent(inout) :: self
        real(wp), intent(in) :: t
        call self%clock_%set_time(t)
    end subroutine set_time

    real(wp) function time(self)
        class(WindowProxy), intent(in) :: self
        time = self%clock_%time()
    end function time

    subroutine pause_time(self, paused)
        class(WindowProxy), intent(inout) :: self
        logical, intent(in) :: paused
        call self%clock_%pause(paused)
    end subroutine pause_time

    logical function is_time_paused(self)
        class(WindowProxy), intent(in) :: self
        is_time_paused = self%clock_%is_paused()
    end function is_time_paused

    subroutine set_time_scale(self, scale)
        class(WindowProxy), intent(inout) :: self
        real(wp), intent(in) :: scale
        call self%clock_%set_time_scale(scale)
    end subroutine set_time_scale

    real(wp) function time_scale(self)
        class(WindowProxy), intent(in) :: self
        time_scale = self%clock_%time_scale()
    end function time_scale

    subroutine set_time_range(self, t_min, t_max)
        class(WindowProxy), intent(inout) :: self
        real(wp), intent(in) :: t_min, t_max
        call self%clock_%set_time_range(t_min, t_max)
    end subroutine set_time_range

    subroutine time_range(self, t_min, t_max)
        class(WindowProxy), intent(in) :: self
        real(wp), intent(out) :: t_min, t_max
        call self%clock_%time_range(t_min, t_max)
    end subroutine time_range

    !> The window's viewer handle in the rendering layer (null until open), for
    !> adding low-level nodes (src/vsg/vkf_layer.f90) before the first frame.
    function window_viewer(self) result(v)
        class(WindowProxy), intent(in) :: self
        type(c_ptr) :: v
        v = self%viewer_
    end function window_viewer

    !> The window's simulation clock (e.g. to synchronize it with another window's).
    function clock(self) result(c)
        class(WindowProxy), intent(in), target :: self
        type(SimulationClock), pointer :: c
        c => self%clock_
    end function clock

    ! ==================================================================
    ! events from a host window (positions in framebuffer pixels from the top
    ! left; buttons 1 left, 2 middle, 3 right; `mask` the buttons held, as
    ! VSG's BUTTON_MASK_1/2/3 = 256/512/1024)

    !> A mouse event: kind MOUSE_MOVE, MOUSE_PRESS or MOUSE_RELEASE.
    subroutine mouse(self, kind, x, y, mask, button)
        class(WindowProxy), intent(inout) :: self
        integer, intent(in) :: kind, x, y, mask, button
        if (.not. c_associated(self%viewer_)) return
        if (vkf_viewer_mouse(self%viewer_, int(kind, c_int), int(x, c_int), int(y, c_int), int(mask, c_int), &
                             int(button, c_int)) /= 0) write(*, '(a)') 'vkframes: mouse event failed'
    end subroutine mouse

    !> The scroll wheel: dy > 0 away from the user.
    subroutine scroll(self, dy)
        class(WindowProxy), intent(inout) :: self
        real(wp), intent(in) :: dy
        if (.not. c_associated(self%viewer_)) return
        if (vkf_viewer_scroll(self%viewer_, real(dy, c_float)) /= 0) write(*, '(a)') 'vkframes: scroll event failed'
    end subroutine scroll

    !> A key press (pressed true) or release, as VSG key symbols and modifiers
    !> (ASCII for printable keys; vsg/ui/KeyEvent.h).
    subroutine key(self, pressed, symbol, modified_symbol, modifier)
        class(WindowProxy), intent(inout) :: self
        logical, intent(in) :: pressed
        integer, intent(in) :: symbol, modified_symbol, modifier
        if (.not. c_associated(self%viewer_)) return
        if (vkf_viewer_key(self%viewer_, merge(1_c_int, 0_c_int, pressed), int(symbol, c_int), &
                           int(modified_symbol, c_int), int(modifier, c_int)) /= 0) &
            write(*, '(a)') 'vkframes: key event failed'
    end subroutine key

    !> The host window was resized (framebuffer pixels).
    subroutine resize(self, width, height)
        class(WindowProxy), intent(inout) :: self
        integer, intent(in) :: width, height
        if (.not. c_associated(self%viewer_)) return
        if (vkf_viewer_resize(self%viewer_, int(width, c_int), int(height, c_int)) /= 0) &
            write(*, '(a)') 'vkframes: resize failed'
    end subroutine resize

end module vkf_window
