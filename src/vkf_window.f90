!> Windows: showing a scene of frames, with its simulation time, as OpenFrames'
!> FrameManager and WindowProxy (for now with a single viewport, which holds
!> the views as OpenFrames' RenderRectangle does; OpenFrames' grid of
!> viewports comes later).
!>
!> A FrameManager holds the root frame of a scene. A WindowProxy shows one: in
!> a window of its own, or in a host's native window (e.g. a Qt QWindow's
!> `winId()`), in which case the host forwards the window's events
!> (`mouse`, `scroll`, `key`, `resize`, `close`). Each `frame()` advances the
!> simulation clock, updates the frames (followed trajectories), copies their
!> positions and attitudes to the renderer, places the camera for the current
!> View and draws.
!>
!> Several windows can show one scene (its render nodes are shared). The scene
!> is updated once per simulation time: a window whose time is the one the
!> scene was last updated for, by another window since this one's last frame,
!> doesn't update it again (`FrameManager%update`). Windows whose clocks are
!> synchronized (`SimulationClock%synchronize`) render the same time, their
!> master's at its last frame, so they show the scene in the same state and
!> update it once per round of frames, also while the time is paused (then
!> each round still updates once, for data added meanwhile).
!>
!> A window has a list of views (`add_view`, `select_view`, `next_view`, ...);
!> each keeps its own camera (the trackball) while another one is shown.
!> Without views, a default view of the scene's root frame is used.
!>
!> Keyboard and mouse (OpenFrames' WindowProxy callbacks, setKeyPressCallback,
!> setMouseMotionCallback, setButtonPressCallback, setButtonReleaseCallback):
!> a program's WindowEventHandler, an extension overriding the procedures it
!> wants (`key_press`, `key_release`, `mouse_motion`, `button_press`,
!> `button_release`), is given the window's events (`set_event_handler`),
!> also those a host forwards. They're passed on after the frame in which they
!> arrived (from `frame()`), so a handler may change anything, the window
!> included (its views, its time, closing it). Positions are in -1 to 1 across
!> the window, y up, as OpenFrames'. The built-in keys, as OpenFrames': `v` the
!> next view, `V` the previous one, the space bar resets the view (unless the
!> handler's `key_press` says it handled the key, or `set_default_keys(.false.)`).
!> The mouse moves the camera (the trackball); the keys don't.
!>
!> The camera (OpenFrames' trackball getTransformation / setTransformation):
!> `get_camera` and `set_camera` (eye, center, up in the shown view's frame,
!> or in its root frame with `in_root`). Smooth transitions (not in
!> OpenFrames): `set_camera` with a `duration` flies there, and with
!> `set_view_transition(seconds)` (default 0: a cut, as OpenFrames) changing
!> the view (select_view, next_view, previous_view, the keys v and V; or with
!> their own `duration`) and resetting it (the space bar) fly from the camera
!> shown to the new one. A flight moves the center along a straight line, the
!> distance geometrically and the orientation by the shortest rotation, eased
!> in and out, in the root frame (both views' frames as they move), over the
!> wall-clock time given; a mouse press, `stop_transition` or another flight
!> ends it.
module vkf_window

    use, intrinsic :: iso_c_binding, only: c_ptr, c_null_ptr, c_associated, c_int, c_long_long, c_double, c_float
    use, intrinsic :: iso_fortran_env, only: int64
    use vkf_math, only: wp, rigid_inverse
    use vkf_frames, only: ReferenceFrame
    use vkf_clock, only: SimulationClock
    use vkf_view, only: View, PERSPECTIVE, camera_between, transform_camera
    use vkf_hud, only: HUDText
    use vkf_sky, only: SkySphere
    use vkf_text, only: label_create
    use vkf_layer, only: vkf_viewer_create, vkf_viewer_delete, vkf_viewer_add_node, vkf_viewer_frame, &
                         vkf_viewer_capture, vkf_viewer_look_at, vkf_viewer_perspective, vkf_viewer_set_background, &
                         vkf_viewer_get_look_at, vkf_viewer_set_view_transform, vkf_viewer_orthographic, &
                         vkf_viewer_hud_add, vkf_label_set_text, vkf_label_set_color, &
                         vkf_viewer_mouse, vkf_viewer_scroll, vkf_viewer_key, vkf_viewer_resize, vkf_viewer_close, &
                         vkf_viewer_light, vkf_viewer_light_enable, vkf_light_set, vkf_viewer_remove_node, &
                         vkf_viewer_hud_remove, vkf_node_delete, vkf_viewer_set_pixel_ratio, &
                         vkf_viewer_events, vkf_viewer_size

    implicit none

    private

    ! mouse event kinds for WindowProxy%mouse
    integer, parameter, public :: MOUSE_MOVE = 1, MOUSE_PRESS = 2, MOUSE_RELEASE = 3
    ! mouse buttons (WindowEventHandler, WindowProxy%mouse)
    integer, parameter, public :: MOUSE_LEFT = 1, MOUSE_MIDDLE = 2, MOUSE_RIGHT = 3
    ! keys (WindowEventHandler): printable keys are their characters,
    ! iachar('a'), iachar('A') with shift; the others (VSG's key symbols):
    integer, parameter, public :: KEY_SPACE = 32, KEY_BACKSPACE = int(z'FF08'), KEY_TAB = int(z'FF09'), &
                                  KEY_RETURN = int(z'FF0D'), KEY_ESCAPE = int(z'FF1B'), KEY_DELETE = int(z'FFFF'), &
                                  KEY_HOME = int(z'FF50'), KEY_LEFT = int(z'FF51'), KEY_UP = int(z'FF52'), &
                                  KEY_RIGHT = int(z'FF53'), KEY_DOWN = int(z'FF54'), KEY_PAGE_UP = int(z'FF55'), &
                                  KEY_PAGE_DOWN = int(z'FF56'), KEY_END = int(z'FF57'), KEY_INSERT = int(z'FF63'), &
                                  KEY_F1 = int(z'FFBE')                             !! F2 to F12: KEY_F1 + 1 to 11
    ! modifier keys held (bits of `modifiers`: iand(modifiers, MOD_SHIFT) /= 0)
    integer, parameter, public :: MOD_SHIFT = 1, MOD_CAPS_LOCK = 2, MOD_CONTROL = 4, MOD_ALT = 8, &
                                  MOD_NUM_LOCK = 16, MOD_META = 128

    !> The root frame of a scene (OpenFrames' FrameManager). The frame is not
    !> owned: it must stay alive while managed.
    type, public :: FrameManager
        private
        class(ReferenceFrame), pointer :: root => null()
        integer :: updates = 0                              !! scene updates so far
        real(wp) :: updated_time = 0.0_wp                   !! the time of the last one
    contains
        procedure :: set_frame => fm_set_frame
        procedure :: frame => fm_frame
        procedure :: update => fm_update, num_updates => fm_num_updates
    end type FrameManager

    interface FrameManager
        module procedure new_frame_manager
    end interface FrameManager

    type :: ViewPointer
        type(View), pointer :: p => null()
    end type ViewPointer

    !> A HUD text shown in a window, with the window's node for it.
    type :: HUDEntry
        type(HUDText), pointer :: p => null()
        type(c_ptr) :: node = c_null_ptr
        integer :: revision = -1                    !! the text's revision last shown
    end type HUDEntry

    !> A camera flight (see the module documentation): from camera `start`
    !> (in view `from`'s frame) to camera `finish` (in view `to`'s).
    type :: CameraFlight
        logical :: active = .false.
        type(View), pointer :: from => null(), to => null()
        real(wp) :: start(3,3) = 0.0_wp, finish(3,3) = 0.0_wp   !! (columns: eye, center, up)
        integer(int64) :: started = 0_int64                 !! system_clock count
        real(wp) :: duration = 0.0_wp                       !! s
    end type CameraFlight

    !> A program's handler of a window's keyboard and mouse events (see the
    !> module documentation): extend it and override the procedures wanted
    !> (the others do nothing).
    type, public :: WindowEventHandler
    contains
        procedure :: key_press => handler_key_press
        procedure :: key_release => handler_key_release
        procedure :: mouse_motion => handler_mouse_motion
        procedure :: button_press => handler_button_press
        procedure :: button_release => handler_button_release
    end type WindowEventHandler

    !> A window showing a scene (see the module documentation).
    type, public :: WindowProxy
        private
        type(c_ptr) :: viewer_ = c_null_ptr
        type(FrameManager), pointer :: scene => null()
        integer :: scene_seen = -1                          !! the scene's updates at this window's last frame
        type(SimulationClock) :: clock_
        logical :: scene_added = .false.
        real(wp) :: background(3) = [0.0_wp, 0.0_wp, 0.0_wp]
        logical :: running = .false.
        integer :: width_ = 1, height_ = 1                  !! logical size (for the views' aspect ratio)
        real(wp) :: pixel_ratio_ = 1.0_wp                   !! framebuffer pixels per window unit (a host's window)
        ! views
        type(ViewPointer), allocatable :: views(:)
        integer :: n_views = 0, current = 0
        type(View) :: default_view                          !! used when there are no views
        type(View), pointer :: shown => null()              !! the view whose camera is in the renderer
        ! HUD text
        type(HUDEntry), allocatable :: huds(:)
        integer :: n_huds = 0
        type(SkySphere), pointer :: sky_ => null()          !! the background sky
        ! the window's light (OpenFrames' default: a headlight)
        real(wp) :: light_ambient_(3) = 0.2_wp, light_diffuse_(3) = 1.0_wp, light_specular_(3) = 0.0_wp
        real(wp) :: light_position_(4) = [0.0_wp, 0.0_wp, 1.0_wp, 0.0_wp]
        logical :: light_on = .true.
        ! keyboard and mouse
        class(WindowEventHandler), pointer :: handler => null()
        logical :: default_keys_ = .true.
        logical :: hosted = .false.                         !! in a host's window (its size from `resize`)
        ! camera transitions
        real(wp) :: transition_ = 0.0_wp                    !! s, for view changes and resets
        real(wp) :: switch_duration = -1.0_wp               !! s, for the next view change (< 0: transition_)
        type(CameraFlight) :: flight
    contains
        procedure :: open, close, is_open, pixel_ratio
        procedure :: set_scene
        procedure :: frame, run
        procedure :: capture
        procedure :: set_background_color, background_color
        ! views (OpenFrames' RenderRectangle)
        procedure :: add_view, remove_view, remove_all_views
        procedure :: select_view, next_view, previous_view
        procedure :: current_view, num_views
        ! the camera (OpenFrames' trackball transformation), smooth transitions
        procedure :: get_camera, set_camera
        procedure :: set_view_transition, view_transition, in_transition, stop_transition
        ! HUD text (OpenFrames' RenderRectangle HUD)
        procedure :: add_hud_text, remove_hud_text
        procedure :: set_sky, sky
        procedure :: set_light_ambient, set_light_diffuse, set_light_specular, set_light_position
        procedure :: light_ambient, light_diffuse, light_specular, light_position
        procedure :: view => window_view
        ! simulation time (OpenFrames' WindowProxy time control)
        procedure :: set_time, time, pause_time, is_time_paused, set_time_scale, time_scale
        procedure :: set_time_range, time_range, clock
        procedure :: viewer => window_viewer
        ! events from a host window
        procedure :: mouse, scroll, key, resize
        ! the program's event handler (OpenFrames' callbacks), built-in keys
        procedure :: set_event_handler, clear_event_handler, event_handler
        procedure :: set_default_keys, default_keys
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

    !> Update the scene for time t (followed trajectories, artists) and copy
    !> the frames' positions and attitudes to the renderer, unless another
    !> window already did for this time since this window's last frame.
    !> `seen` is the window's own: the number of updates at its last frame.
    subroutine fm_update(self, t, seen)
        class(FrameManager), intent(inout) :: self
        real(wp), intent(in) :: t
        integer, intent(inout) :: seen
        if (.not. associated(self%root)) return
        if (.not. (self%updates > 0 .and. t == self%updated_time .and. seen /= self%updates)) then
            call self%root%update(t)
            call self%root%late_update(t)                   ! (all frames at their positions for t)
            call self%root%sync()
            self%updates = self%updates + 1
            self%updated_time = t
        end if
        seen = self%updates
    end subroutine fm_update

    !> Number of scene updates so far (`update`).
    pure integer function fm_num_updates(self) result(n)
        class(FrameManager), intent(in) :: self
        n = self%updates
    end function fm_num_updates

    ! ==================================================================
    ! the window

    !> Open the window: `width` x `height` (logical pixels) titled `title`, or,
    !> with `native_window` (a host window's native handle) and its
    !> `pixel_ratio`, render into that window. `vsync` (default true): frames
    !> are presented at the display's refresh; false: as soon as they are ready
    !> (where the driver allows it), e.g. to measure frame times. Returns false
    !> on failure.
    logical function open(self, width, height, title, native_window, pixel_ratio, vsync) result(ok)
        class(WindowProxy), intent(inout) :: self
        integer, intent(in) :: width, height
        character(len=*), intent(in), optional :: title
        integer(c_long_long), intent(in), optional :: native_window
        real(wp), intent(in), optional :: pixel_ratio
        logical, intent(in), optional :: vsync
        character(len=:), allocatable :: t
        integer(c_long_long) :: handle
        real(c_double) :: ratio
        integer(c_int) :: sync
        t = 'vkframes'
        if (present(title)) t = title
        handle = 0
        ratio = 1.0_c_double
        if (present(native_window)) handle = native_window
        if (present(pixel_ratio)) ratio = pixel_ratio
        self%pixel_ratio_ = ratio
        sync = 1_c_int
        if (present(vsync)) sync = merge(1_c_int, 0_c_int, vsync)
        self%viewer_ = vkf_viewer_create(int(width, c_int), int(height, c_int), t, len(t, kind=c_int), handle, ratio, sync)
        ok = c_associated(self%viewer_)
        if (.not. ok) return
        self%running = .true.
        self%scene_added = .false.
        self%shown => null()
        self%width_ = max(1, width)
        self%height_ = max(1, height)
        self%hosted = handle /= 0
        call self%set_background_color(self%background)
        self%light_on = .true.
        call apply_window_light(self)
    end function open

    !> Close the window (waiting for the GPU). The scene is kept, and can be
    !> shown again in another window.
    subroutine close(self)
        class(WindowProxy), intent(inout) :: self
        if (c_associated(self%viewer_)) call vkf_viewer_delete(self%viewer_)
        self%viewer_ = c_null_ptr
        self%running = .false.
    end subroutine close

    !> Framebuffer pixels per window unit of a host's window (as given to
    !> `open` or `resize`; 1 for a window of its own).
    pure real(wp) function pixel_ratio(self)
        class(WindowProxy), intent(in) :: self
        pixel_ratio = self%pixel_ratio_
    end function pixel_ratio

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
        self%scene_seen = -1
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
        if (.not. self%scene_added) then
            if (associated(root)) then
                call root%realize(self%viewer_)
                if (vkf_viewer_add_node(self%viewer_, root%node()) /= 0) write(*, '(a)') 'vkframes: could not add the scene'
            end if
            if (associated(self%sky_)) then
                if (vkf_viewer_add_node(self%viewer_, self%sky_%node(self%viewer_)) /= 0) &
                    write(*, '(a)') 'vkframes: could not add the sky'
            end if
            call create_huds(self)
            self%scene_added = .true.
        end if
        call update_huds(self)
        t = self%clock_%update()
        if (associated(root)) call self%scene%update(t, self%scene_seen)     ! (once per time across windows)
        call update_window_light(self, root)
        call place_camera(self)
        status = vkf_viewer_frame(self%viewer_)
        self%running = status == 1
        if (self%running) call dispatch_events(self)
        running = self%running .and. self%is_open()
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

    ! ==================================================================
    ! the window's light

    !> The ambient, diffuse and specular colors ([r, g, b], 0 to 1) of the
    !> window's light (OpenFrames' defaults: 0.2 gray, white, none). The
    !> window's light is used while no frame of the scene has a light source
    !> (ReferenceFrame%set_light_source_enabled).
    subroutine set_light_ambient(self, rgb)
        class(WindowProxy), intent(inout) :: self
        real(wp), intent(in) :: rgb(3)
        self%light_ambient_ = rgb
        call apply_window_light(self)
    end subroutine set_light_ambient

    subroutine set_light_diffuse(self, rgb)
        class(WindowProxy), intent(inout) :: self
        real(wp), intent(in) :: rgb(3)
        self%light_diffuse_ = rgb
        call apply_window_light(self)
    end subroutine set_light_diffuse

    subroutine set_light_specular(self, rgb)
        class(WindowProxy), intent(inout) :: self
        real(wp), intent(in) :: rgb(3)
        self%light_specular_ = rgb
        call apply_window_light(self)
    end subroutine set_light_specular

    !> The light's position in eye coordinates (x right, y up, z toward the
    !> viewer): [x, y, z, 0] for a directional light from direction (x, y, z),
    !> [x, y, z, 1] for a point light (OpenFrames' setLightPosition; default
    !> [0, 0, 1, 0], a headlight).
    subroutine set_light_position(self, position)
        class(WindowProxy), intent(inout) :: self
        real(wp), intent(in) :: position(4)
        if (all(position(1:3) == 0.0_wp) .and. position(4) == 0.0_wp) return
        self%light_position_ = position
        call apply_window_light(self)
    end subroutine set_light_position

    pure function light_ambient(self) result(rgb)
        class(WindowProxy), intent(in) :: self
        real(wp) :: rgb(3)
        rgb = self%light_ambient_
    end function light_ambient

    pure function light_diffuse(self) result(rgb)
        class(WindowProxy), intent(in) :: self
        real(wp) :: rgb(3)
        rgb = self%light_diffuse_
    end function light_diffuse

    pure function light_specular(self) result(rgb)
        class(WindowProxy), intent(in) :: self
        real(wp) :: rgb(3)
        rgb = self%light_specular_
    end function light_specular

    pure function light_position(self) result(p)
        class(WindowProxy), intent(in) :: self
        real(wp) :: p(4)
        p = self%light_position_
    end function light_position

    subroutine apply_window_light(self)
        type(WindowProxy), intent(inout) :: self
        type(c_ptr) :: light
        if (.not. c_associated(self%viewer_)) return
        light = vkf_viewer_light(self%viewer_)
        if (.not. c_associated(light)) return
        if (vkf_light_set(light, real(self%light_ambient_, c_float), real(self%light_diffuse_, c_float), &
                          real(self%light_specular_, c_float), real(self%light_position_, c_double)) /= 0) &
            write(*, '(a)') 'vkframes: could not set the window light'
    end subroutine apply_window_light

    !> The window's light on while no frame of the scene has a light source.
    subroutine update_window_light(self, root)
        type(WindowProxy), intent(inout) :: self
        class(ReferenceFrame), pointer, intent(in) :: root
        logical :: on
        on = .true.
        if (associated(root)) on = .not. root%has_light_source()
        if (on .eqv. self%light_on) return
        self%light_on = on
        if (vkf_viewer_light_enable(self%viewer_, merge(1_c_int, 0_c_int, on)) /= 0) &
            write(*, '(a)') 'vkframes: could not switch the window light'
    end subroutine update_window_light

    ! ==================================================================
    ! sky

    !> Draw `sky` behind the scene (it must stay alive while shown; it can be
    !> shown in several windows), in place of the window's sky so far; at any
    !> time (its texture and stars can change too).
    subroutine set_sky(self, sky)
        class(WindowProxy), intent(inout) :: self
        type(SkySphere), intent(inout), target :: sky
        integer(c_int) :: status
        if (self%scene_added) then                      ! shown: swap the sky's node
            if (associated(self%sky_)) status = vkf_viewer_remove_node(self%viewer_, self%sky_%node(self%viewer_))
            if (vkf_viewer_add_node(self%viewer_, sky%node(self%viewer_)) /= 0) &
                write(*, '(a)') 'vkframes: could not add the sky'
        end if
        self%sky_ => sky
    end subroutine set_sky

    function sky(self) result(s)
        class(WindowProxy), intent(in) :: self
        type(SkySphere), pointer :: s
        s => self%sky_
    end function sky

    ! ==================================================================
    ! HUD text

    !> Show `h` in the window, on top of the scene (it must stay alive while
    !> shown), from the next frame.
    subroutine add_hud_text(self, h)
        class(WindowProxy), intent(inout) :: self
        type(HUDText), intent(in), target :: h
        type(HUDEntry), allocatable :: tmp(:)
        if (.not. allocated(self%huds)) allocate(self%huds(4))
        if (self%n_huds == size(self%huds)) then
            allocate(tmp(2 * self%n_huds))
            tmp(1:self%n_huds) = self%huds(1:self%n_huds)
            call move_alloc(tmp, self%huds)
        end if
        self%n_huds = self%n_huds + 1
        self%huds(self%n_huds)%p => h
        self%huds(self%n_huds)%node = c_null_ptr
        self%huds(self%n_huds)%revision = -1
        if (self%scene_added) call create_hud(self, self%n_huds)    ! shown: made now
    end subroutine add_hud_text

    !> Stop showing `h` in the window.
    subroutine remove_hud_text(self, h)
        class(WindowProxy), intent(inout) :: self
        type(HUDText), intent(in), target :: h
        integer(c_int) :: status
        integer :: k
        do k = 1, self%n_huds
            if (.not. associated(self%huds(k)%p, h)) cycle
            if (c_associated(self%huds(k)%node)) then
                status = vkf_viewer_hud_remove(self%viewer_, self%huds(k)%node)    ! (not shown yet: -1)
                call vkf_node_delete(self%huds(k)%node)
            end if
            self%huds(k:self%n_huds-1) = self%huds(k+1:self%n_huds)
            self%n_huds = self%n_huds - 1
            return
        end do
    end subroutine remove_hud_text

    !> Create the window's nodes for its HUD texts (before its first frame).
    subroutine create_huds(self)
        type(WindowProxy), intent(inout) :: self
        integer :: k
        do k = 1, self%n_huds
            call create_hud(self, k)
        end do
    end subroutine create_huds

    !> The node of HUD text k, added to the window (compiled if it runs).
    subroutine create_hud(self, k)
        type(WindowProxy), intent(inout) :: self
        integer, intent(in) :: k
        type(HUDText), pointer :: h
        h => self%huds(k)%p
        self%huds(k)%node = label_create(h%text(), [h%position(), 0.0_wp], h%color(), h%size(), 0.0_wp, &
                                         h%alignment(), h%font())
        if (.not. c_associated(self%huds(k)%node)) return
        if (vkf_viewer_hud_add(self%viewer_, self%huds(k)%node) /= 0) write(*, '(a)') 'vkframes: could not add HUD text'
        self%huds(k)%revision = h%revision()
    end subroutine create_hud

    !> Show the HUD texts' new text and color, if they changed.
    subroutine update_huds(self)
        type(WindowProxy), intent(inout) :: self
        type(HUDText), pointer :: h
        character(len=:), allocatable :: t
        integer :: k
        do k = 1, self%n_huds
            h => self%huds(k)%p
            if (.not. c_associated(self%huds(k)%node) .or. h%revision() == self%huds(k)%revision) cycle
            t = h%text()
            if (vkf_label_set_text(self%huds(k)%node, t, len(t, kind=c_int)) /= 0 .or. &
                vkf_label_set_color(self%huds(k)%node, real(h%color(), c_float)) /= 0) &
                write(*, '(a)') 'vkframes: could not update HUD text'
            self%huds(k)%revision = h%revision()
        end do
    end subroutine update_huds

    ! ==================================================================
    ! views

    !> Add `v` to the end of the window's views (it must stay alive while added).
    subroutine add_view(self, v)
        class(WindowProxy), intent(inout) :: self
        type(View), intent(in), target :: v
        type(ViewPointer), allocatable :: tmp(:)
        if (.not. allocated(self%views)) allocate(self%views(4))
        if (self%n_views == size(self%views)) then
            allocate(tmp(2 * self%n_views))
            tmp(1:self%n_views) = self%views(1:self%n_views)
            call move_alloc(tmp, self%views)
        end if
        self%n_views = self%n_views + 1
        self%views(self%n_views)%p => v
        if (self%current == 0) self%current = 1
    end subroutine add_view

    !> Remove all instances of `v` from the window's views.
    subroutine remove_view(self, v)
        class(WindowProxy), intent(inout) :: self
        type(View), intent(in), target :: v
        integer :: k
        k = 1
        do while (k <= self%n_views)
            if (associated(self%views(k)%p, v)) then
                self%views(k:self%n_views-1) = self%views(k+1:self%n_views)
                self%n_views = self%n_views - 1
                if (self%current > k) self%current = self%current - 1
            else
                k = k + 1
            end if
        end do
        self%current = min(self%current, self%n_views)
        if (self%current == 0 .and. self%n_views > 0) self%current = 1
        if (associated(self%shown, v)) self%shown => null()
    end subroutine remove_view

    subroutine remove_all_views(self)
        class(WindowProxy), intent(inout) :: self
        self%n_views = 0
        self%current = 0
    end subroutine remove_all_views

    !> Show view i (1 to num_views), flying there over `duration` s (default:
    !> the view transition, `set_view_transition`; 0: a cut).
    subroutine select_view(self, i, duration)
        class(WindowProxy), intent(inout) :: self
        integer, intent(in) :: i
        real(wp), intent(in), optional :: duration
        if (i < 1 .or. i > self%n_views .or. i == self%current) return
        self%current = i
        if (present(duration)) self%switch_duration = max(0.0_wp, duration)
    end subroutine select_view

    !> Show the next view (wrapping around); `duration` as select_view's.
    subroutine next_view(self, duration)
        class(WindowProxy), intent(inout) :: self
        real(wp), intent(in), optional :: duration
        if (self%n_views > 0) call self%select_view(modulo(self%current, self%n_views) + 1, duration)
    end subroutine next_view

    !> Show the previous view (wrapping around); `duration` as select_view's.
    subroutine previous_view(self, duration)
        class(WindowProxy), intent(inout) :: self
        real(wp), intent(in), optional :: duration
        if (self%n_views > 0) call self%select_view(modulo(self%current - 2, self%n_views) + 1, duration)
    end subroutine previous_view

    ! ==================================================================
    ! the camera

    !> The camera of the view shown (from the last frame): eye, center and up
    !> in the view frame's coordinates (OpenFrames' trackball
    !> getTransformation), or in its root frame's if `in_root`. Zero before
    !> the window's first frame.
    subroutine get_camera(self, eye, center, up, in_root)
        class(WindowProxy), intent(in) :: self
        real(wp), intent(out) :: eye(3), center(3), up(3)
        logical, intent(in), optional :: in_root
        real(wp) :: cam(3,3)
        eye = 0.0_wp
        center = 0.0_wp
        up = 0.0_wp
        if (.not. self%is_open() .or. .not. associated(self%shown)) return
        if (vkf_viewer_get_look_at(self%viewer_, eye, center, up) /= 0) return
        if (.not. present(in_root)) return
        if (.not. in_root) return
        cam = transform_camera(rigid_inverse(self%shown%world_to_view()), reshape([eye, center, up], [3, 3]))
        eye = cam(:, 1)
        center = cam(:, 2)
        up = cam(:, 3)
    end subroutine get_camera

    !> Set the camera of the current view: eye, center and up in the view
    !> frame's coordinates (OpenFrames' trackball setTransformation), or in its
    !> root frame's if `in_root` (as the frames are now). With `duration` (s),
    !> fly there from the camera shown. Before the view is shown, it is its
    !> camera when it is (View%set_camera).
    subroutine set_camera(self, eye, center, up, duration, in_root)
        class(WindowProxy), intent(inout), target :: self
        real(wp), intent(in) :: eye(3), center(3), up(3)
        real(wp), intent(in), optional :: duration
        logical, intent(in), optional :: in_root
        type(View), pointer :: v
        real(wp) :: cam(3,3), now(3,3)
        logical :: root
        v => self%current_view()
        cam = reshape([eye, center, up], [3, 3])
        root = .false.
        if (present(in_root)) root = in_root
        if (root) cam = transform_camera(v%world_to_view(), cam)
        if (.not. self%is_open() .or. .not. associated(self%shown, v)) then
            call v%set_camera(cam(:, 1), cam(:, 2), cam(:, 3))
            return
        end if
        if (present(duration)) then
            if (duration > 0.0_wp .and. vkf_viewer_get_look_at(self%viewer_, now(:, 1), now(:, 2), now(:, 3)) == 0) then
                call start_flight(self, v, now, v, cam, duration)
                return
            end if
        end if
        self%flight%active = .false.
        call put_camera(self, cam)
    end subroutine set_camera

    !> The time (s) a change of view or a reset flies from one camera to the
    !> other (default 0: a cut, as OpenFrames).
    subroutine set_view_transition(self, seconds)
        class(WindowProxy), intent(inout) :: self
        real(wp), intent(in) :: seconds
        self%transition_ = max(0.0_wp, seconds)
    end subroutine set_view_transition

    pure real(wp) function view_transition(self)
        class(WindowProxy), intent(in) :: self
        view_transition = self%transition_
    end function view_transition

    !> True while the camera is flying (a transition).
    pure logical function in_transition(self)
        class(WindowProxy), intent(in) :: self
        in_transition = self%flight%active
    end function in_transition

    !> End a flight where the camera is now.
    subroutine stop_transition(self)
        class(WindowProxy), intent(inout) :: self
        self%flight%active = .false.
    end subroutine stop_transition

    !> The view shown (the default view of the scene's root if none were added).
    function current_view(self) result(v)
        class(WindowProxy), intent(in), target :: self
        type(View), pointer :: v
        if (self%current >= 1 .and. self%current <= self%n_views) then
            v => self%views(self%current)%p
        else
            v => self%default_view
        end if
    end function current_view

    !> View i (1 to num_views).
    function window_view(self, i) result(v)
        class(WindowProxy), intent(in) :: self
        integer, intent(in) :: i
        type(View), pointer :: v
        v => self%views(i)%p
    end function window_view

    pure integer function num_views(self)
        class(WindowProxy), intent(in) :: self
        num_views = self%n_views
    end function num_views

    !> Set the renderer's camera for the current view: keep the previous view's
    !> trackball state and restore (or reset) the current one's when the view
    !> changes, then set its world-to-view transform and projection.
    subroutine place_camera(self)
        type(WindowProxy), intent(inout), target :: self
        type(View), pointer :: v, from
        class(ReferenceFrame), pointer :: root
        real(wp) :: eye(3), center(3), up(3), dist, near, far, bounds(4), cam(3,3), from_cam(3,3), duration
        logical :: have_target
        v => self%current_view()
        if (associated(v, self%default_view) .and. associated(self%scene)) then
            root => self%scene%frame()
            if (associated(root) .and. .not. self%default_view%is_valid()) call self%default_view%set_view_frame(root, root)
        end if
        ! the camera shown (to fly from), and the new one: a view changed to (its
        ! camera set, kept, or reset), a camera set, or a reset
        from => null()
        if (associated(self%shown)) then
            if (vkf_viewer_get_look_at(self%viewer_, from_cam(:, 1), from_cam(:, 2), from_cam(:, 3)) == 0) &
                from => self%shown
        end if
        duration = self%transition_
        have_target = .false.
        if (.not. associated(v, self%shown)) then
            if (associated(from)) call from%save_state(from_cam(:, 1), from_cam(:, 2), from_cam(:, 3))
            if (self%switch_duration >= 0.0_wp) duration = self%switch_duration
            self%switch_duration = -1.0_wp
            if (v%take_camera(cam)) then
                have_target = .true.
            else if (v%saved_state(eye, center, up)) then
                cam = reshape([eye, center, up], [3, 3])
                have_target = .true.
            else
                call v%reset_view()
            end if
            self%shown => v
        else if (v%take_camera(cam)) then
            have_target = .true.
            duration = 0.0_wp                               ! (View%set_camera: at once)
        end if
        if (v%take_reset()) then
            call v%home(aspect_ratio(self), eye, center, up)
            cam = reshape([eye, center, up], [3, 3])
            have_target = .true.
        end if
        if (have_target) then
            if (associated(from) .and. duration > 0.0_wp) then
                call start_flight(self, from, from_cam, v, cam, duration)
            else
                self%flight%active = .false.
                call put_camera(self, cam)
            end if
        end if
        call fly(self, v)
        if (vkf_viewer_set_view_transform(self%viewer_, v%world_to_view()) /= 0) &
            write(*, '(a)') 'vkframes: could not set the view'
        ! near and far planes from the camera's distance to its center (reverse-Z
        ! float depth covers the wide ratio)
        if (vkf_viewer_get_look_at(self%viewer_, eye, center, up) /= 0) return
        dist = max(norm2(eye - center), tiny(1.0_wp))
        near = 1.0e-6_wp * dist
        far = 1.0e9_wp * dist
        if (v%projection_type() == PERSPECTIVE) then
            if (vkf_viewer_perspective(self%viewer_, v%field_of_view(), near, far) /= 0) &
                write(*, '(a)') 'vkframes: could not set the projection'
        else
            bounds = v%orthographic_bounds()
            if (vkf_viewer_orthographic(self%viewer_, bounds(1), bounds(2), bounds(3), bounds(4), -far, far) /= 0) &
                write(*, '(a)') 'vkframes: could not set the projection'
        end if
    end subroutine place_camera

    !> The renderer's camera (columns: eye, center, up; the shown view's frame).
    subroutine put_camera(self, cam)
        type(WindowProxy), intent(inout) :: self
        real(wp), intent(in) :: cam(3,3)
        if (vkf_viewer_look_at(self%viewer_, cam(:, 1), cam(:, 2), cam(:, 3)) /= 0) &
            write(*, '(a)') 'vkframes: could not set the camera'
    end subroutine put_camera

    !> Fly from camera `start` (in view `from`'s frame) to `finish` (in view
    !> `to`'s) over `duration` s, from now.
    subroutine start_flight(self, from, start, to, finish, duration)
        type(WindowProxy), intent(inout) :: self
        type(View), intent(in), target :: from, to
        real(wp), intent(in) :: start(3,3), finish(3,3), duration
        self%flight%from => from
        self%flight%to => to
        self%flight%start = start
        self%flight%finish = finish
        self%flight%duration = duration
        call system_clock(self%flight%started)
        self%flight%active = .true.
    end subroutine start_flight

    !> The camera of an active flight to view `v`, at this moment: both
    !> cameras in the root frame (their views' frames as they are now), in
    !> between by the eased fraction of the time passed, back in `v`'s frame.
    subroutine fly(self, v)
        type(WindowProxy), intent(inout) :: self
        type(View), intent(in), target :: v
        integer(int64) :: count, rate
        real(wp) :: s, from(3,3), to(3,3), m(4,4)
        if (.not. self%flight%active) return
        if (.not. associated(self%flight%to, v)) then             ! (another view since)
            self%flight%active = .false.
            return
        end if
        call system_clock(count, rate)
        s = real(count - self%flight%started, wp) / (real(rate, wp) * self%flight%duration)
        if (s >= 1.0_wp) then
            self%flight%active = .false.
            call put_camera(self, self%flight%finish)
            return
        end if
        s = s * s * (3.0_wp - 2.0_wp * s)                           ! (eased in and out)
        m = v%world_to_view()
        from = transform_camera(rigid_inverse(self%flight%from%world_to_view()), self%flight%start)
        to = transform_camera(rigid_inverse(m), self%flight%finish)
        call put_camera(self, transform_camera(m, camera_between(s, from, to)))
    end subroutine fly

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
    !> adding low-level nodes (src/vsg/vkf_layer.f90).
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

    !> The host window was resized (framebuffer pixels), and `pixel_ratio`: its
    !> framebuffer pixels per window unit (Qt's devicePixelRatio), if it may
    !> have changed (the window moved to a screen of another scale, or the
    !> display's scaling changed: sizes in window units, such as line widths,
    !> markers and labels, are scaled by it). Once the scene is shown, a frame
    !> of the new size is rendered at once (call this from the host's resize
    !> event), so that the window isn't shown stretched or blank while it is
    !> resized.
    subroutine resize(self, width, height, pixel_ratio)
        class(WindowProxy), intent(inout) :: self
        integer, intent(in) :: width, height
        real(wp), intent(in), optional :: pixel_ratio
        if (.not. c_associated(self%viewer_)) return
        if (present(pixel_ratio)) then
            if (pixel_ratio > 0.0_wp .and. pixel_ratio /= self%pixel_ratio_) then
                self%pixel_ratio_ = pixel_ratio
                if (vkf_viewer_set_pixel_ratio(self%viewer_, real(pixel_ratio, c_double)) /= 0) &
                    write(*, '(a)') 'vkframes: could not set the pixel ratio'
            end if
        end if
        self%width_ = max(1, width)
        self%height_ = max(1, height)
        if (vkf_viewer_resize(self%viewer_, int(width, c_int), int(height, c_int)) /= 0) &
            write(*, '(a)') 'vkframes: resize failed'
        ! a frame of the new size at once (once the scene is shown), so the window
        ! isn't shown stretched or blank while being resized
        if (self%scene_added) then
            if (self%frame()) continue
        end if
    end subroutine resize

    ! ==================================================================
    ! keyboard and mouse: the program's handler (OpenFrames' callbacks)

    !> Give the window's keyboard and mouse events to `handler` (not owned: it
    !> must stay alive while set), in place of any handler set before.
    subroutine set_event_handler(self, handler)
        class(WindowProxy), intent(inout) :: self
        class(WindowEventHandler), intent(in), target :: handler
        self%handler => handler
    end subroutine set_event_handler

    subroutine clear_event_handler(self)
        class(WindowProxy), intent(inout) :: self
        self%handler => null()
    end subroutine clear_event_handler

    !> The event handler (null if none).
    function event_handler(self) result(handler)
        class(WindowProxy), intent(in) :: self
        class(WindowEventHandler), pointer :: handler
        handler => self%handler
    end function event_handler

    !> The built-in keys (default on): `v` the next view, `V` the previous
    !> one, the space bar resets the view.
    subroutine set_default_keys(self, on)
        class(WindowProxy), intent(inout) :: self
        logical, intent(in) :: on
        self%default_keys_ = on
    end subroutine set_default_keys

    pure logical function default_keys(self)
        class(WindowProxy), intent(in) :: self
        default_keys = self%default_keys_
    end function default_keys

    !> A key was pressed: `key` (a character's code, iachar('v'), or KEY_*),
    !> with the modifier keys held (MOD_* bits). Set `handled` (false on entry)
    !> to skip the built-in key's action.
    subroutine handler_key_press(self, window, key, modifiers, handled)
        class(WindowEventHandler), intent(inout) :: self
        class(WindowProxy), intent(inout) :: window
        integer, intent(in) :: key, modifiers
        logical, intent(inout) :: handled
    end subroutine handler_key_press

    !> A key was released (see `key_press`).
    subroutine handler_key_release(self, window, key, modifiers)
        class(WindowEventHandler), intent(inout) :: self
        class(WindowProxy), intent(inout) :: window
        integer, intent(in) :: key, modifiers
    end subroutine handler_key_release

    !> The mouse moved to (x, y) (-1 to 1 across the window, y up), with the
    !> buttons held (left, middle, right): moving (none held, OpenFrames' mouse
    !> motion) or dragging.
    subroutine handler_mouse_motion(self, window, x, y, buttons)
        class(WindowEventHandler), intent(inout) :: self
        class(WindowProxy), intent(inout) :: window
        real(wp), intent(in) :: x, y
        logical, intent(in) :: buttons(3)
    end subroutine handler_mouse_motion

    !> A mouse button (MOUSE_LEFT, MOUSE_MIDDLE, MOUSE_RIGHT) was pressed at
    !> (x, y) (see `mouse_motion`).
    subroutine handler_button_press(self, window, x, y, button)
        class(WindowEventHandler), intent(inout) :: self
        class(WindowProxy), intent(inout) :: window
        real(wp), intent(in) :: x, y
        integer, intent(in) :: button
    end subroutine handler_button_press

    !> A mouse button was released at (x, y) (see `button_press`).
    subroutine handler_button_release(self, window, x, y, button)
        class(WindowEventHandler), intent(inout) :: self
        class(WindowProxy), intent(inout) :: window
        real(wp), intent(in) :: x, y
        integer, intent(in) :: button
    end subroutine handler_button_release

    !> The keyboard and mouse events of the last frame, to the handler and the
    !> built-in keys (stopping if the window is closed meanwhile).
    subroutine dispatch_events(self)
        class(WindowProxy), intent(inout), target :: self
        integer, parameter :: capacity = 64
        integer(c_int) :: events(12, capacity), n
        integer :: i
        do
            if (.not. self%is_open()) return
            n = vkf_viewer_events(self%viewer_, events, int(capacity, c_int))
            do i = 1, n
                if (.not. self%is_open()) return
                call dispatch_event(self, int(events(:, i)))
            end do
            if (n < capacity) return
        end do
    end subroutine dispatch_events

    !> One event (see vkf_viewer_events): kind, key, key base, modifiers,
    !> button, buttons held, x, y (framebuffer pixels from the top left), the
    !> render area (x, y, width, height).
    subroutine dispatch_event(self, e)
        class(WindowProxy), intent(inout), target :: self
        integer, intent(in) :: e(12)
        integer, parameter :: KEY_PRESS = 1, KEY_RELEASE = 2, MOVE = 3, BUTTON_PRESS = 4, BUTTON_RELEASE = 5
        integer, parameter :: BUTTON_MASKS(3) = [256, 512, 1024]            ! (VSG's BUTTON_MASK_1/2/3)
        integer :: key
        real(wp) :: x, y
        logical :: handled
        key = e(2)
        if (key == 0) key = e(3)                                             ! (no modified key given)
        x = 0.0_wp
        y = 0.0_wp
        if (e(11) > 0) x = 2.0_wp * real(e(7) - e(9), wp) / real(e(11), wp) - 1.0_wp
        if (e(12) > 0) y = 1.0_wp - 2.0_wp * real(e(8) - e(10), wp) / real(e(12), wp)
        select case (e(1))
        case (KEY_PRESS)
            handled = .false.
            if (associated(self%handler)) call self%handler%key_press(self, key, e(4), handled)
            if (.not. handled .and. self%default_keys_ .and. self%is_open()) call default_key(self, key, e(4))
        case (KEY_RELEASE)
            if (associated(self%handler)) call self%handler%key_release(self, key, e(4))
        case (MOVE)
            if (associated(self%handler)) call self%handler%mouse_motion(self, x, y, iand(e(6), BUTTON_MASKS) /= 0)
        case (BUTTON_PRESS)
            self%flight%active = .false.                                     ! (the mouse takes the camera)
            if (associated(self%handler)) call self%handler%button_press(self, x, y, e(5))
        case (BUTTON_RELEASE)
            if (associated(self%handler)) call self%handler%button_release(self, x, y, e(5))
        end select
    end subroutine dispatch_event

    !> The built-in keys (OpenFrames'): `v` the next view, `V` the previous
    !> one, the space bar resets the view; not with control, alt or meta held.
    subroutine default_key(self, key, modifiers)
        class(WindowProxy), intent(inout), target :: self
        integer, intent(in) :: key, modifiers
        type(View), pointer :: v
        if (iand(modifiers, MOD_CONTROL + MOD_ALT + MOD_META) /= 0) return
        if (key == iachar('v')) then
            call self%next_view()
        else if (key == iachar('V')) then
            call self%previous_view()
        else if (key == KEY_SPACE) then
            v => self%current_view()
            call v%reset_view()
        end if
    end subroutine default_key

    !> The width / height of the window drawn: a host's (from `resize`), or of
    !> the window's own, as it is now.
    real(wp) function aspect_ratio(self)
        class(WindowProxy), intent(in) :: self
        integer(c_int) :: width, height
        aspect_ratio = real(self%width_, wp) / real(self%height_, wp)
        if (self%hosted .or. .not. c_associated(self%viewer_)) return
        if (vkf_viewer_size(self%viewer_, width, height) /= 0) return
        if (width > 0 .and. height > 0) aspect_ratio = real(width, wp) / real(height, wp)
    end function aspect_ratio

end module vkf_window
