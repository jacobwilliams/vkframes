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
!> A window has a list of views (`add_view`, `select_view`, `next_view`, ...);
!> each keeps its own camera (the trackball) while another one is shown.
!> Without views, a default view of the scene's root frame is used.
module vkf_window

    use, intrinsic :: iso_c_binding, only: c_ptr, c_null_ptr, c_associated, c_int, c_long_long, c_double, c_float
    use vkf_math, only: wp
    use vkf_frames, only: ReferenceFrame
    use vkf_clock, only: SimulationClock
    use vkf_view, only: View, PERSPECTIVE
    use vkf_hud, only: HUDText
    use vkf_sky, only: SkySphere
    use vkf_text, only: label_create
    use vkf_layer, only: vkf_viewer_create, vkf_viewer_delete, vkf_viewer_add_node, vkf_viewer_frame, &
                         vkf_viewer_capture, vkf_viewer_look_at, vkf_viewer_perspective, vkf_viewer_set_background, &
                         vkf_viewer_get_look_at, vkf_viewer_set_view_transform, vkf_viewer_orthographic, &
                         vkf_viewer_hud_add, vkf_label_set_text, vkf_label_set_color, &
                         vkf_viewer_mouse, vkf_viewer_scroll, vkf_viewer_key, vkf_viewer_resize, vkf_viewer_close, &
                         vkf_viewer_light, vkf_viewer_light_enable, vkf_light_set, vkf_viewer_remove_node, &
                         vkf_viewer_hud_remove, vkf_node_delete

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

    type :: ViewPointer
        type(View), pointer :: p => null()
    end type ViewPointer

    !> A HUD text shown in a window, with the window's node for it.
    type :: HUDEntry
        type(HUDText), pointer :: p => null()
        type(c_ptr) :: node = c_null_ptr
        integer :: revision = -1                    !! the text's revision last shown
    end type HUDEntry

    !> A window showing a scene (see the module documentation).
    type, public :: WindowProxy
        private
        type(c_ptr) :: viewer_ = c_null_ptr
        type(FrameManager), pointer :: scene => null()
        type(SimulationClock) :: clock_
        logical :: scene_added = .false.
        real(wp) :: background(3) = [0.0_wp, 0.0_wp, 0.0_wp]
        logical :: running = .false.
        integer :: width_ = 1, height_ = 1                  !! logical size (for the views' aspect ratio)
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
    contains
        procedure :: open, close, is_open
        procedure :: set_scene
        procedure :: frame, run
        procedure :: capture
        procedure :: set_background_color, background_color
        ! views (OpenFrames' RenderRectangle)
        procedure :: add_view, remove_view, remove_all_views
        procedure :: select_view, next_view, previous_view
        procedure :: current_view, num_views
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
        self%shown => null()
        self%width_ = max(1, width)
        self%height_ = max(1, height)
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
        if (associated(root)) then
            call root%update(t)
            call root%sync()
        end if
        call update_window_light(self, root)
        call place_camera(self)
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

    !> Show view i (1 to num_views).
    subroutine select_view(self, i)
        class(WindowProxy), intent(inout) :: self
        integer, intent(in) :: i
        if (i >= 1 .and. i <= self%n_views) self%current = i
    end subroutine select_view

    !> Show the next view (wrapping around).
    subroutine next_view(self)
        class(WindowProxy), intent(inout) :: self
        if (self%n_views > 0) self%current = modulo(self%current, self%n_views) + 1
    end subroutine next_view

    !> Show the previous view (wrapping around).
    subroutine previous_view(self)
        class(WindowProxy), intent(inout) :: self
        if (self%n_views > 0) self%current = modulo(self%current - 2, self%n_views) + 1
    end subroutine previous_view

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
        type(View), pointer :: v
        class(ReferenceFrame), pointer :: root
        real(wp) :: eye(3), center(3), up(3), dist, near, far, bounds(4)
        logical :: restored
        v => self%current_view()
        if (associated(v, self%default_view) .and. associated(self%scene)) then
            root => self%scene%frame()
            if (associated(root) .and. .not. self%default_view%is_valid()) call self%default_view%set_view_frame(root, root)
        end if
        if (.not. associated(v, self%shown)) then
            if (associated(self%shown)) then
                if (vkf_viewer_get_look_at(self%viewer_, eye, center, up) == 0) call self%shown%save_state(eye, center, up)
            end if
            restored = v%saved_state(eye, center, up)
            if (.not. restored) call v%reset_view()
            if (restored) call set_camera(self, eye, center, up)
            self%shown => v
        end if
        if (v%take_reset()) then
            call v%home(real(self%width_, wp) / real(self%height_, wp), eye, center, up)
            call set_camera(self, eye, center, up)
        end if
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

    subroutine set_camera(self, eye, center, up)
        type(WindowProxy), intent(inout) :: self
        real(wp), intent(in) :: eye(3), center(3), up(3)
        if (vkf_viewer_look_at(self%viewer_, eye, center, up) /= 0) write(*, '(a)') 'vkframes: could not set the camera'
    end subroutine set_camera

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

    !> The host window was resized (framebuffer pixels).
    subroutine resize(self, width, height)
        class(WindowProxy), intent(inout) :: self
        integer, intent(in) :: width, height
        if (.not. c_associated(self%viewer_)) return
        self%width_ = max(1, width)
        self%height_ = max(1, height)
        if (vkf_viewer_resize(self%viewer_, int(width, c_int), int(height, c_int)) /= 0) &
            write(*, '(a)') 'vkframes: resize failed'
    end subroutine resize

end module vkf_window
