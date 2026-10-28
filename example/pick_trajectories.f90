!> Picking trajectories (WindowProxy%pick, not in OpenFrames): three orbits
!> with markers; the event handler picks what is under the mouse.
!>
!> * Hover: the orbit under the pointer is drawn thicker.
!> * Click: selects the orbit or marker under it, with two callouts:
!>   - a ring and a readout at the point picked (the orbit, the time there,
!>     the altitude): a frame moved there, with a marker artist drawing one
!>     marker at its origin, and its name label, changed to the readout, in
!>     the orbit's color, drawn on top of the scene (it follows the point as
!>     the view turns);
!>   - a GUI panel (vkf_gui: Dear ImGui) kept next to the orbit's spacecraft
!>     each frame, with its altitude as it moves, a checkbox for the orbit's
!>     trace mode, a slider for its line width, and buttons to go to the time
!>     picked and to close it: described in the handler's `before_draw`
!>     (called each frame once the spacecraft and the camera are where they
!>     are drawn) at the spacecraft's place on the screen
!>     (WindowProxy%screen_position).
!>   Clicking the Earth shows the latitude and longitude instead.
!> * A Controls panel (top left, movable): the time scale and pause.
!> * g: goes to the time selected (the spacecraft move there).
!>
!> Arguments (optional): number of frames to run, then a file to capture the
!> last frame to; with `--demo-pick`, a pick is made at the start (for a
!> capture): `pixi run example pick_trajectories -- 120 pick.png --demo-pick`
module pick_trajectories_handler

    use vkframes

    implicit none

    private

    integer, parameter, public :: n_orbits = 3

    !> The program's state the handler needs (pointers to the program's objects).
    type, extends(WindowEventHandler), public :: Picker
        type(CurveArtist), pointer :: curves(:) => null()
        type(ReferenceFrame), pointer :: selection => null()     !! moved to the point picked; its label: the readout
        type(DrawableTrajectory), pointer :: ring => null()      !! the ring there (a child of the selection)
        type(HUDText), pointer :: hud => null()
        type(DrawableTrajectory), pointer :: crafts(:) => null() !! the spacecraft (markers following the orbits)
        character(len=16) :: names(n_orbits) = ''
        real(wp) :: colors(3, n_orbits) = 1.0_wp
        integer :: selected = 0                                  !! the orbit selected (its spacecraft's panel), or 0
        real(wp), pointer :: widths(:) => null()
        integer :: hovered = 0
        real(wp) :: selected_time = 0.0_wp
        logical :: have_time = .false.
        real(wp) :: radius = 1.0_wp                              !! the body's (for altitudes)
    contains
        procedure :: mouse_motion => picker_motion
        procedure :: button_press => picker_press
        procedure :: key_press => picker_key
        procedure :: before_draw => picker_before_draw
        procedure :: select => picker_select
    end type Picker

contains

    !> Hover: the orbit under the pointer drawn thicker (not while dragging).
    subroutine picker_motion(self, window, x, y, buttons)
        class(Picker), intent(inout) :: self
        class(WindowProxy), intent(inout) :: window
        real(wp), intent(in) :: x, y
        logical, intent(in) :: buttons(3)
        type(PickHit), allocatable :: hits(:)
        integer :: k
        if (any(buttons)) return
        call window%pick(x, y, hits, kinds=[PICK_CURVE, PICK_MARKER], nearest=.true.)   ! (cheap: every move)
        k = 0
        if (size(hits) > 0) k = orbit_of(self, hits(1))
        if (k == self%hovered) return
        if (self%hovered > 0) call self%curves(self%hovered)%set_width(self%widths(self%hovered))
        if (k > 0) call self%curves(k)%set_width(2.5_wp * self%widths(k))
        self%hovered = k
    end subroutine picker_motion

    subroutine picker_press(self, window, x, y, button)
        class(Picker), intent(inout) :: self
        class(WindowProxy), intent(inout) :: window
        real(wp), intent(in) :: x, y
        integer, intent(in) :: button
        if (button == MOUSE_LEFT) call self%select(window, x, y)
    end subroutine picker_press

    !> Each frame, before drawing: the GUI's panels, the selected orbit's next
    !> to its spacecraft, where it is in this frame.
    subroutine picker_before_draw(self, window, t)
        class(Picker), intent(inout) :: self
        class(WindowProxy), intent(inout) :: window
        real(wp), intent(in) :: t
        type(Gui) :: g
        character(len=96) :: text
        real(wp) :: p(3), at(2), scale, width
        logical :: paused, trace
        integer :: k
        g = window%gui()
        ! the controls: a panel at the top left (movable)
        call g%begin_panel('Controls', position=[0.01_wp, 0.98_wp])
        scale = window%time_scale()
        if (g%slider('time scale', scale, 0.0_wp, 2000.0_wp)) call window%set_time_scale(scale)
        paused = window%is_time_paused()
        if (g%checkbox('pause', paused)) call window%pause_time(paused)
        call g%end_panel()
        ! the selected orbit's spacecraft: a panel kept next to it, where it is now
        k = self%selected
        if (k == 0) return
        p = self%crafts(k)%position()                           ! (a child of the root: root coordinates)
        if (.not. window%screen_position(p, at)) return
        call g%begin_panel('##spacecraft', position=at + [0.01_wp, 0.01_wp], pivot=[0.0_wp, 1.0_wp], follow=.true.)
        write(text, '(a,i0,a)') trim(self%names(k))//' spacecraft: ', nint(norm2(p) - self%radius), ' km'
        call g%text(trim(text))
        trace = self%curves(k)%trace_mode()
        if (g%checkbox('trace', trace)) call self%curves(k)%set_trace_mode(trace)
        width = self%widths(k)
        if (g%slider('line width', width, 1.0_wp, 8.0_wp)) then
            self%widths(k) = width
            call self%curves(k)%set_width(width)
        end if
        if (self%have_time) then
            if (g%button('go to the time picked')) call window%set_time(self%selected_time)
            call g%same_line()
        end if
        if (g%button('close')) self%selected = 0
        call g%end_panel()
    end subroutine picker_before_draw

    !> g: go to the time selected.
    subroutine picker_key(self, window, key, modifiers, handled)
        class(Picker), intent(inout) :: self
        class(WindowProxy), intent(inout) :: window
        integer, intent(in) :: key, modifiers
        logical, intent(inout) :: handled
        if (key /= iachar('g') .or. .not. self%have_time) return
        call window%set_time(self%selected_time)
        handled = .true.
    end subroutine picker_key

    !> Select what is at (x, y): show it in the HUD and mark the point.
    subroutine picker_select(self, window, x, y)
        class(Picker), intent(inout) :: self
        class(WindowProxy), intent(inout) :: window
        real(wp), intent(in) :: x, y
        type(PickHit), allocatable :: hits(:)
        character(len=160) :: text
        real(wp) :: at(2)
        logical :: right
        integer :: k
        call window%pick(x, y, hits)
        self%have_time = .false.
        self%selected = 0
        if (size(hits) == 0) then
            call self%ring%show_contents(.false.)
            call self%selection%show_name_label(.false.)
            return
        end if
        associate (h => hits(1))
            call self%selection%set_color([1.0_wp, 1.0_wp, 1.0_wp])
            select case (h%kind)
            case (PICK_CURVE, PICK_MARKER)
                k = orbit_of(self, h)
                if (k > 0) then                                 ! an orbit, or one of its markers
                    write(text, '(a,a,i0,a,i0,a)') trim(self%names(k))//trim(merge(' marker', '       ', &
                        h%kind == PICK_MARKER)), ': t = ', nint(h%time / 60.0_wp), ' min, ', &
                        nint(norm2(h%position) - self%radius), ' km'
                    self%selected_time = h%time
                    self%have_time = .true.
                else                                            ! a spacecraft (a marker with no trajectory)
                    k = spacecraft_of(self, h)
                    write(text, '(a,a,i0,a)') h%frame%name(), ': ', nint(norm2(h%position) - self%radius), ' km'
                end if
                if (k > 0) then
                    self%selected = k
                    call self%selection%set_color(self%colors(:, k))
                end if
            case (PICK_BODY)
                write(text, '(a,f6.1,a,f7.1)') h%frame%name()//': latitude ', h%latitude * 180.0_wp / pi, &
                    ', longitude ', h%longitude * 180.0_wp / pi
            case default
                text = h%frame%name()
            end select
            ! the readout: the selection frame's name label, beside the ring on
            ! the side with room (left of it in the right of the window)
            right = .false.
            if (window%screen_position(h%position, at)) right = at(1) > 0.6_wp
            if (right) then
                call self%selection%set_label_alignment(ALIGN_RIGHT_CENTER)
                text = trim(text)//'    '
            else
                call self%selection%set_label_alignment(ALIGN_LEFT_CENTER)
                text = '    '//trim(text)
            end if
            call self%selection%set_name(trim(text))
            call self%selection%show_name_label(.true.)
            call self%selection%set_position(h%position)
            call self%ring%show_contents(.true.)
        end associate
    end subroutine picker_select

    !> The spacecraft a hit is on (its frame), or 0.
    integer function spacecraft_of(self, hit) result(k)
        type(Picker), intent(in) :: self
        type(PickHit), intent(in) :: hit
        do k = 1, n_orbits
            if (associated(hit%frame, self%crafts(k))) return
        end do
        k = 0
    end function spacecraft_of

    !> The orbit a hit is on (its curve's or its markers' trajectory), or 0.
    integer function orbit_of(self, hit) result(k)
        type(Picker), intent(in) :: self
        type(PickHit), intent(in) :: hit
        type(Trajectory), pointer :: traj
        do k = 1, n_orbits
            if (.not. associated(hit%trajectory)) exit
            traj => self%curves(k)%get_trajectory()
            if (associated(hit%trajectory, traj)) return
        end do
        k = 0
    end function orbit_of

end module pick_trajectories_handler

program pick_trajectories

    use vkframes
    use pick_trajectories_handler, only: Picker, n_orbits

    implicit none

    real(wp), parameter :: mu = 398600.4418_wp, re = 6378.137_wp, deg = pi / 180.0_wp
    integer, parameter :: n_points = 400
    real(wp), parameter :: elements(4, n_orbits) = reshape([ &           ! a (km), e, i, node (degrees)
        8000.0_wp, 0.05_wp, 30.0_wp, 0.0_wp, &
        12000.0_wp, 0.2_wp, 60.0_wp, 90.0_wp, &
        20000.0_wp, 0.5_wp, 10.0_wp, 200.0_wp], [4, n_orbits])
    real(wp), parameter :: colors(3, n_orbits) = reshape([ &
        1.0_wp, 0.8_wp, 0.2_wp, 0.3_wp, 0.8_wp, 1.0_wp, 1.0_wp, 0.4_wp, 0.6_wp], [3, n_orbits])
    character(len=16), parameter :: names(n_orbits) = [character(len=16) :: 'Low orbit', 'Inclined orbit', 'High orbit']

    type(ReferenceFrame), target :: root, selection
    type(Sphere), target :: earth
    type(Trajectory), target :: orbits(n_orbits)
    type(DrawableTrajectory), target :: frames(n_orbits), ring_frame, crafts(n_orbits)
    type(CurveArtist), target :: curves(n_orbits)
    type(MarkerArtist), target :: markers(n_orbits), ring, craft_markers(n_orbits)
    real(wp), target :: widths(n_orbits)
    type(FrameManager), target :: scene
    type(WindowProxy), target :: window
    type(View), target :: earth_view
    type(HUDText), target :: hud
    type(Picker), target :: picker_
    character(len=1024) :: arg, file
    real(wp) :: period, t, x, y
    integer :: i, k, frames_to_run, frame
    logical :: running, demo_pick

    frames_to_run = huge(frames_to_run)
    file = ''
    demo_pick = .false.
    do i = 1, command_argument_count()
        call get_command_argument(i, arg)
        if (arg == '--demo-pick') then
            demo_pick = .true.
        else if (i == 1) then
            read(arg, *) frames_to_run
        else if (i == 2) then
            file = arg
        end if
    end do

    root = ReferenceFrame('Inertial')
    call root%show_decorations(.false.)
    earth = Sphere('Earth', radius=re)
    call earth%set_texture_map('2k_earth_daymap.jpg')                    ! (data/, on VKFRAMES_DATA_PATH)
    call earth%show_decorations(.false.)
    if (.not. root%add_child(earth)) error stop 'add_child failed'

    ! the orbits: a curve, start and end markers and markers every 15 minutes,
    ! and a spacecraft marker following each
    do i = 1, n_orbits
        period = 2.0_wp * pi * sqrt(elements(1, i)**3 / mu)
        orbits(i) = Trajectory()
        call orbits(i)%reserve(n_points)
        do k = 1, n_points
            t = period * real(k - 1, wp) / real(n_points - 1, wp)
            call orbits(i)%add(t, pos=orbit_position(t, elements(:, i)))
        end do
        widths(i) = 2.0_wp
        curves(i) = CurveArtist(orbits(i), color=colors(:, i), width=widths(i))
        markers(i) = MarkerArtist(orbits(i), markers=ior(ior(MARKER_START, MARKER_END), MARKER_INTERMEDIATE), &
                                  size=9.0_wp, color=colors(:, i))
        call markers(i)%set_intermediate_type(INTERMEDIATE_TIME)
        call markers(i)%set_intermediate_spacing(900.0_wp)
        frames(i) = DrawableTrajectory(trim(names(i)))
        call frames(i)%show_decorations(.false.)
        call frames(i)%add_artist(curves(i))
        call frames(i)%add_artist(markers(i))
        craft_markers(i) = MarkerArtist(markers=MARKER_START, size=14.0_wp, color=[1.0_wp, 1.0_wp, 1.0_wp])
        crafts(i) = DrawableTrajectory(trim(names(i))//' spacecraft')
        call crafts(i)%show_decorations(.false.)
        call crafts(i)%add_artist(craft_markers(i))
        call crafts(i)%follow(orbits(i), data=FOLLOW_POSITION)
        if (.not. (root%add_child(frames(i)) .and. root%add_child(crafts(i)))) error stop 'add_child failed'
    end do

    ! the selection: a ring (a marker at the frame's origin), moved to what is picked
    selection = ReferenceFrame('Selection')
    call selection%show_decorations(.false.)
    ring = MarkerArtist(markers=MARKER_START, size=26.0_wp, color=[1.0_wp, 1.0_wp, 1.0_wp])
    call ring%set_marker_shape(MARKER_CIRCLE)
    ring_frame = DrawableTrajectory('Ring')
    call ring_frame%show_decorations(.false.)
    call ring_frame%add_artist(ring)
    call ring_frame%show_contents(.false.)
    call ring_frame%set_pickable(.false.)                             ! (the selection's own marks: not picked)
    call selection%set_pickable(.false.)
    call selection%set_label_size(16)
    call selection%set_name_label_on_top(.true.)                     ! (a readout: not hidden by the Earth)
    call selection%set_label_alignment(ALIGN_LEFT_CENTER)
    if (.not. (selection%add_child(ring_frame) .and. root%add_child(selection))) error stop 'add_child failed'
    scene = FrameManager(root)

    if (.not. window%open(1100, 800, 'vkframes: picking trajectories')) error stop 'could not open a window'
    call window%set_scene(scene)
    call window%set_background_color([0.0_wp, 0.0_wp, 0.02_wp])
    call window%set_time_scale(300.0_wp)
    earth_view = View(root, earth)
    call earth_view%set_default_view_parameters(eye=[2000.0_wp, -75000.0_wp, 38000.0_wp], &
                                                center=[4000.0_wp, 0.0_wp, 0.0_wp], up=[0.0_wp, 0.0_wp, 1.0_wp])
    call window%add_view(earth_view)
    hud = HUDText('click an orbit, a marker or the Earth   (g: go to the time selected)', position=[0.01_wp, 0.01_wp], &
                  alignment=ALIGN_LEFT_BOTTOM, size=16.0_wp, color=[0.85_wp, 0.85_wp, 0.85_wp])
    call window%add_hud_text(hud)

    picker_%curves => curves
    picker_%widths => widths
    picker_%selection => selection
    picker_%ring => ring_frame
    picker_%hud => hud
    picker_%crafts => crafts
    picker_%names = names
    picker_%colors = colors
    picker_%radius = re
    call window%set_event_handler(picker_)

    running = .true.
    frame = 0
    do while (running .and. frame < frames_to_run)
        running = window%frame()
        frame = frame + 1
        if (demo_pick .and. frame == 3) then                       ! (a pick on the high orbit, for a capture)
            call screen_position(orbit_position(0.3_wp * 2.0_wp * pi * sqrt(elements(1, 3)**3 / mu), elements(:, 3)), &
                                 x, y)
            call picker_%select(window, x, y)
            call picker_%mouse_motion(window, x, y, [.false., .false., .false.])
        end if
    end do
    if (running .and. len_trim(file) > 0) then
        if (window%capture(trim(file))) write(*, '(a)') 'captured '//trim(file)
    end if
    call window%close()

contains

    !> Where root point p is in the window (-1 to 1, y up), from its camera.
    subroutine screen_position(p, x, y)
        real(wp), intent(in) :: p(3)
        real(wp), intent(out) :: x, y
        real(wp) :: eye(3), center(3), up(3), f(3), r(3), u(3), v(3), tan_y, depth
        call window%get_camera(eye, center, up, in_root=.true.)
        f = (center - eye) / norm2(center - eye)
        r = [f(2) * up(3) - f(3) * up(2), f(3) * up(1) - f(1) * up(3), f(1) * up(2) - f(2) * up(1)]
        r = r / norm2(r)
        u = [r(2) * f(3) - r(3) * f(2), r(3) * f(1) - r(1) * f(3), r(1) * f(2) - r(2) * f(1)]
        v = p - eye
        depth = dot_product(v, f)
        tan_y = tan(0.5_wp * earth_view%field_of_view() * deg)
        x = dot_product(v, r) / (depth * tan_y * 1100.0_wp / 800.0_wp)
        y = dot_product(v, u) / (depth * tan_y)
    end subroutine screen_position

    !> Position at time t (km) on the orbit of elements e (a, e, i, node;
    !> perigee on the node line), from Kepler's equation.
    pure function orbit_position(t, e) result(r)
        real(wp), intent(in) :: t, e(4)
        real(wp) :: r(3), m, ea, x, y, ci, si, co, so
        integer :: k
        m = sqrt(mu / e(1)**3) * t
        ea = m
        do k = 1, 20
            ea = ea - (ea - e(2) * sin(ea) - m) / (1.0_wp - e(2) * cos(ea))
        end do
        x = e(1) * (cos(ea) - e(2))
        y = e(1) * sqrt(1.0_wp - e(2)**2) * sin(ea)
        ci = cos(e(3) * deg)
        si = sin(e(3) * deg)
        co = cos(e(4) * deg)
        so = sin(e(4) * deg)
        r = [co * x - so * ci * y, so * x + co * ci * y, si * y]
    end function orbit_position

end program pick_trajectories
