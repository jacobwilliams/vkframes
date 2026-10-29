!> The invariant manifolds of a halo orbit in the Earth-Moon circular
!> restricted three-body problem (CR3BP), drawn as Polyhedron surfaces: the
!> tubes of trajectories on the Earth's side that leave the orbit (unstable,
!> orange) and that arrive at it (stable, cyan), in the rotating frame (the
!> Earth and Moon fixed on the x axis); and the zero-velocity curves at the
!> orbit's Jacobi constant (in the x-y plane: the edges of the region the
!> orbit's energy can't reach; none for L3's halos, whose energy reaches
!> everywhere), a Polyhedron of edges only. The five Lagrange points are
!> marked and labeled (as in the CR3BP demo), in front of the HYG v3
!> catalog's stars (a SkySphere: to magnitude 6; fixed, as the view is of
!> the rotating frame at one instant). Ticked x, y, z axes (the rotating
!> frame's directions; a CoordinateAxes, sized to the view, with round tick
!> spacings, a whole number of major ticks long: each ends on one) mark the center chosen with the panel's buttons (at first the
!> barycenter; x hides them).
!>
!> The halo orbit (northern), about L1, L2 or L3 (a menu), of a z amplitude
!> (a slider): Richardson's third-order approximation (ported from the
!> Fortran Astrodynamics Toolkit's halo_orbit_module), corrected: from the
!> x-z plane, perpendicular to it, half a period to its next crossing, the
!> corrections to x and vy (z kept) making that crossing perpendicular too
!> (with the state transition matrix's partials). Where that doesn't
!> converge (large amplitudes), by continuation in z from the orbit shown.
!> Its monodromy matrix (the state transition matrix over a period) has a
!> real eigenvalue above 1 and its inverse (from the traces of the matrix
!> and its square, also ported from the toolkit): their eigenvectors (by
!> inverse iteration), carried along the orbit by the state transition
!> matrix, give the directions of the manifolds at each of 72 points. Each
!> tube: those points displaced toward the Earth (50 km; 2000 km for L3,
!> whose halos are only weakly unstable: an eigenvalue of about 2, against
!> about 1000 for L1's and L2's), propagated (19.5 days at first, 80 for L3:
!> a slider sets it, 2 to 120) forward (unstable) or backward (stable),
!> sampled at common times: a grid of points (around the orbit by along the
!> time), a Polyhedron (set_grid), closed around. Trajectories that reach
!> the Earth or the Moon stop there.
!>
!> The surfaces: lit, smooth, translucent, a color per vertex fading out
!> along the time (set_vertex_colors), with every 6th trajectory also drawn
!> as a line (a CurveArtist, pickable). Hover over one: it's drawn thicker;
!> click it: it pulses, with arrows moving along it in its direction of
!> motion (set_highlight), a readout at the point clicked (the time there,
!> from leaving the orbit or before arriving at it, and the distance from
!> the Earth), and its details at the bottom left (its place around the
!> orbit, its closest approach to the Earth, or its impact, and its Jacobi
!> constant with its change along it: the integration's error).
!>
!> The panel (top right; Dear ImGui, WindowProxy%gui): the Lagrange point
!> (a menu: its family's first orbit), the amplitude (each family's range:
!> from 1000 km to the largest the correction reaches), the Jacobi
!> constant (another orbit of the family, by continuation from the one
!> shown: steps in C of at most 0.002, each corrected with C held: x0, z0
!> and vy0 together) and the propagation time (the manifolds only). Each
!> change computes the orbit's manifolds and zero-velocity curves again
!> (the latest values asked for, once the last are done; the panel says how
!> long it took, and the orbit's period, amplitude and unstable
!> eigenvalue). The manifolds' trajectories are propagated in parallel with
!> OpenMP (the demo's fpm.toml; about 0.005 s on ten threads of an Apple
!> M-series). Its buttons fly the camera to the Earth, the Moon, the
!> Lagrange point or the point picked on a trajectory (centered on it, the
!> trackball then turning about it, looking from the same direction).
!>
!> The zero-velocity curves: 2 U(x, y, 0) = C (U the pseudo-potential, C
!> the Jacobi constant), traced by marching squares on a 600 x 600 grid of
!> the plane, as segments.
!>
!> Keys: 1 and 2 each tube shown or hidden, z the zero-velocity curves, x
!> the axes, e
!> the trajectory lines on or off, l lit or unlit, s smooth or flat, c
!> colors fading or not, + / - more or less opaque, space resets the view.
!>
!> Run it with `pixi run halo-demo` (from the vkframes root). Arguments
!> (optional): number of frames to run, then a file to capture the last
!> frame to; with `--demo-pick`, a trajectory is picked at the start (for a
!> capture), and the panel's settings at the start: `--point=N` (1, 2 or 3),
!> `--az=KM`, `--jacobi=C`, `--days=D`, and the camera centered on
!> `--center=earth`, `moon` or `point` (the Lagrange point):
!> `pixi run halo-demo 60 halo.png --point=2 --az=20000 --days=30`
program halo_manifolds

    use vkframes
    use halo_cr3bp
    use halo_manifolds_keys
    !$ use omp_lib, only: omp_get_max_threads

    implicit none

    integer, parameter :: n_around = 72                 !! points around the orbit (the tubes' circumference)
    integer, parameter :: n_along = 360                 !! samples along each trajectory
    integer, parameter :: n_orbit = 4 * n_around        !! points along the orbit drawn
    real(wp), parameter :: offsets(3) = [50.0_wp, 50.0_wp, 2000.0_wp] / lu   !! each point's displacement off the orbit
                                                        !! (50 km; L3's halos are only weakly unstable: 2000)
    real(wp), parameter :: days_first(3) = [19.5_wp, 19.5_wp, 80.0_wp]       !! each point's first propagation time
    real(wp), parameter :: re = 6378.137_wp, rm = 1737.4_wp     !! km
    character(len=*), parameter :: names(n_tubes) = [character(len=24) :: 'Unstable', 'Stable']
    integer, parameter :: branch(n_tubes) = [1, 2]      !! each tube's eigenvector: 1 unstable, 2 stable
    real(wp), parameter :: az_largest(3) = [60000.0_wp, 60000.0_wp, 400000.0_wp]  !! each point's largest Az asked (km)
    real(wp), parameter :: az_first(3) = [30000.0_wp, 20000.0_wp, 20000.0_wp]     !! and the first shown
    real(wp), parameter :: direction(n_tubes) = [1.0_wp, -1.0_wp]  !! its time: forward (unstable), backward
    real(wp), parameter :: tube_colors(3, n_tubes) = reshape([1.0_wp, 0.65_wp, 0.15_wp, 0.2_wp, 0.75_wp, 1.0_wp], &
                                                             [3, n_tubes])

    type(ReferenceFrame), target :: rotating, sun
    type(Sphere), target :: earth, moon
    type(Trajectory), target :: halo
    type(CurveArtist), target :: halo_curve
    type(DrawableTrajectory), target :: halo_frame
    type(Polyhedron), target :: tubes(n_tubes), zvc
    type(Trajectory), target :: lagrange                !! the Lagrange points (one point each)
    type(MarkerArtist), target :: lagrange_dots
    type(DrawableTrajectory), target :: lagrange_frame
    type(ReferenceFrame), target :: lagrange_labels(5)
    type(SkySphere), target :: sky                      !! the stars (HYG v3)
    type(CoordinateAxes), target :: axes                !! ticked x, y, z axes at the center chosen
    type(Trajectory), target :: paths(n_lines, n_tubes)
    type(CurveArtist), target :: lines(n_lines, n_tubes)
    type(DrawableTrajectory), target :: line_frames(n_tubes), ring_frame
    type(MarkerArtist), target :: ring
    type(ReferenceFrame), target :: selection
    type(HUDText), target :: details
    type(FrameManager), target :: scene
    type(WindowProxy), target :: window
    type(View), target :: view_
    type(HUDText), target :: legend
    type(ManifoldKeys), target :: keys
    character(len=1024) :: arg, file
    real(wp) :: x0, z0, vy0, period, c_reached, c_start, days_start, az_start
    real(wp) :: earth_side                              !! the tubes' side: -1 (L1, L2: toward -x), +1 (L3)
    real(wp) :: offset                                  !! the displacement off the orbit (offsets: the point's)
    real(wp) :: az_asked                                !! the amplitude last asked for (km)
    integer :: point_start
    character(len=8) :: center_start                    !! (--center=: earth, moon or point)
    real(wp) :: span                                    !! each trajectory's time (time units: keys%days)
    real(wp) :: orbit(6, n_orbit + 1), dirs(6, n_around, 2), times(n_along)
    real(wp) :: grid(3, n_around, n_along, n_tubes)
    integer :: last(n_around, n_tubes)                  !! each trajectory's samples drawn (up to an impact)
    integer :: j, k, l, frames, frame
    integer(8) :: tick, tock, rate
    logical :: running, demo, ok, orbit_changed, by_c

    frames = huge(frames)
    file = ''
    if (command_argument_count() >= 1) then
        call get_command_argument(1, arg)
        read(arg, *) frames
    end if
    if (command_argument_count() >= 2) call get_command_argument(2, file)
    demo = .false.
    c_start = 0.0_wp
    days_start = 0.0_wp
    point_start = 1
    center_start = ''
    az_start = 0.0_wp
    do j = 3, command_argument_count()
        call get_command_argument(j, arg)
        if (trim(arg) == '--demo-pick') demo = .true.
        if (arg(1:9) == '--jacobi=') read(arg(10:), *) c_start
        if (arg(1:7) == '--days=') read(arg(8:), *) days_start
        if (arg(1:8) == '--point=') read(arg(9:), *) point_start
        if (arg(1:5) == '--az=') read(arg(6:), *) az_start
        if (arg(1:9) == '--center=') center_start = arg(10:)
    end do
    keys%threads = 1
    !$ keys%threads = omp_get_max_threads()

    ! the first halo orbit: about L1 (or --point=), of Az 30000 km (or --az=)
    call set_point(max(1, min(3, point_start)))
    if (days_start > 0.0_wp) keys%days_requested = days_start
    if (az_start > 0.0_wp) keys%az_requested = max(keys%az_min, min(keys%az_max, az_start))
    if (.not. from_amplitude(keys%az_requested, .false.)) error stop 'no halo orbit found'
    write(*, '(a,i0,a,f12.9,a,f9.6,a,f12.9,a,f9.6,a,f6.2,a)') 'L', keys%point, ' halo orbit: x0 = ', x0, &
        ', z0 = ', z0, ', vy0 = ', vy0, ', period ', period, ' (', period * tu_days, ' days)'

    ! the scene: the rotating frame, the Earth and the Moon on its x axis; the
    ! orbit, the tubes (and their lines) and the zero-velocity curves (filled
    ! in by compute)
    rotating = ReferenceFrame('Rotating frame')
    call rotating%show_decorations(.false.)
    earth = Sphere('Earth', radius=re)
    call earth%set_texture_map('2k_earth_daymap.jpg')
    call earth%set_position([-mu * lu, 0.0_wp, 0.0_wp])
    call earth%show_axes(NO_AXES)
    call earth%show_axes_labels(NO_AXES)
    moon = Sphere('Moon', radius=rm)
    call moon%set_texture_map('lroc_color_2k.jpg')
    call moon%set_position([(1.0_wp - mu) * lu, 0.0_wp, 0.0_wp])
    call moon%show_axes(NO_AXES)
    call moon%show_axes_labels(NO_AXES)
    halo = Trajectory()
    halo_curve = CurveArtist(halo, color=[1.0_wp, 1.0_wp, 1.0_wp], width=2.5_wp)
    halo_frame = DrawableTrajectory('Halo orbit')
    call halo_frame%show_decorations(.false.)
    call halo_frame%add_artist(halo_curve)
    call halo_frame%set_pickable(.false.)
    zvc = Polyhedron('Zero-velocity curves', color=[0.85_wp, 0.85_wp, 0.85_wp, 0.8_wp])
    call zvc%set_draw_mode(POLY_EDGES)
    call zvc%set_line_width(1.5_wp)
    call zvc%show_decorations(.false.)
    call zvc%set_pickable(.false.)
    call add_lagrange_points()
    ! ticked axes (the rotating frame's directions) at the center chosen (the
    ! panel's buttons: at first the frame's origin, the barycenter)
    axes = CoordinateAxes('Axes', color=[0.75_wp, 0.75_wp, 0.75_wp])
    call axes%set_axis_width(1.5_wp)
    call axes%set_tick_size(10.0_wp, 5.0_wp)
    call axes%show_name_label(.false.)
    call axes%show_axes(NO_AXES)                        ! (the ticked lines only: no arrows, no labels)
    call axes%show_axes_labels(NO_AXES)
    call axes%set_pickable(.false.)
    if (.not. rotating%add_child(axes)) error stop 'add_child failed'
    keys%axes => axes
    sun = ReferenceFrame('Sun')
    call sun%set_position([-0.4_wp, -1.0_wp, 0.6_wp] * 1.5e8_wp)
    call sun%set_light_source_enabled(.true.)
    call sun%set_light_diffuse([1.0_wp, 1.0_wp, 1.0_wp])
    call sun%set_light_ambient([0.25_wp, 0.25_wp, 0.25_wp])
    call sun%show_decorations(.false.)
    if (.not. (rotating%add_child(earth) .and. rotating%add_child(moon) .and. rotating%add_child(halo_frame) .and. &
               rotating%add_child(sun) .and. rotating%add_child(zvc))) error stop 'add_child failed'
    allocate(keys%colors(4, n_around * n_along, n_tubes))
    do k = 1, n_tubes
        line_frames(k) = DrawableTrajectory(trim(names(k))//' trajectories')
        call line_frames(k)%show_decorations(.false.)
        do l = 1, n_lines
            paths(l, k) = Trajectory()
            lines(l, k) = CurveArtist(paths(l, k), color=[tube_colors(:, k), 0.7_wp], width=1.2_wp)
            call line_frames(k)%add_artist(lines(l, k))
        end do
        tubes(k) = Polyhedron(trim(names(k)), color=tube_colors(:, k))
        call tubes(k)%set_draw_mode(POLY_FACES)
        call tubes(k)%show_decorations(.false.)
        call tubes(k)%set_pickable(.false.)
        keys%names(k) = names(k)
        keys%line_colors(:, k) = [tube_colors(:, k), 1.0_wp]
        if (.not. (rotating%add_child(tubes(k)) .and. rotating%add_child(line_frames(k)))) error stop 'add_child failed'
    end do

    ! the selection: a ring (a marker at the frame's origin) and a readout,
    ! moved to the point picked
    selection = ReferenceFrame('Selection')
    call selection%show_decorations(.false.)
    ring = MarkerArtist(markers=MARKER_START, size=22.0_wp, color=[1.0_wp, 1.0_wp, 1.0_wp])
    call ring%set_marker_shape(MARKER_CIRCLE)
    ring_frame = DrawableTrajectory('Ring')
    call ring_frame%show_decorations(.false.)
    call ring_frame%add_artist(ring)
    call ring_frame%show_contents(.false.)
    call ring_frame%set_pickable(.false.)
    call selection%set_pickable(.false.)
    call selection%set_label_size(15)
    call selection%set_name_label_on_top(.true.)
    if (.not. (selection%add_child(ring_frame) .and. rotating%add_child(selection))) error stop 'add_child failed'

    keys%tubes => tubes
    keys%curves => zvc
    keys%lines => lines
    keys%line_frames => line_frames
    keys%selection => selection
    keys%ring => ring_frame
    keys%details => details
    keys%earth = [-mu * lu, 0.0_wp, 0.0_wp]
    keys%moon = [(1.0_wp - mu) * lu, 0.0_wp, 0.0_wp]
    keys%lu = lu
    call place_axes(keys, [0.0_wp, 0.0_wp, 0.0_wp], 1.0_wp)
    keys%re = re
    call compute(.true.)
    keys%c_requested = keys%c
    if (c_start > 0.0_wp) keys%c_requested = c_start   ! (--jacobi=C: computed in the first frame)
    scene = FrameManager(rotating)

    if (.not. window%open(1200, 800, 'vkframes: halo orbit manifolds')) error stop 'could not open a window'
    call window%set_scene(scene)
    ! the stars behind the scene (HYG v3, to magnitude 6)
    if (sky%set_star_data('Stars_HYGv3.txt', min_mag=-2.0_wp, max_mag=6.0_wp, max_stars=40000)) then
        call window%set_sky(sky)
    else
        write(*, '(a)') 'no star catalog (Stars_HYGv3.txt)'
    end if
    call window%set_background_color([0.0_wp, 0.0_wp, 0.02_wp])
    view_ = View(rotating, rotating)
    call view_%set_default_view_parameters(eye=[0.2_wp, -2.0_wp, 1.35_wp] * lu, center=[0.25_wp, 0.0_wp, 0.0_wp] * lu, &
                                           up=[0.0_wp, 0.0_wp, 1.0_wp])
    call window%add_view(view_)
    call view_point()
    legend = HUDText('Earth-Moon halo orbit (white), its manifolds on the Earth side:  1 unstable (orange)   ' // &
                     '2 stable (cyan)   z: zero-velocity curves (gray)   x: axes' // &
                     new_line('a') // 'click a line: its details (arrows: its motion)    e: trajectory lines    ' // &
                     'l: lit / unlit    s: smooth / flat    c: fading colors / plain    + / -: opacity', &
                     position=[0.01_wp, 0.99_wp], alignment=ALIGN_LEFT_TOP, size=14.0_wp, color=[0.75_wp, 0.75_wp, 0.75_wp])
    call window%add_hud_text(legend)
    details = HUDText('click a trajectory line for its details', position=[0.01_wp, 0.01_wp], &
                      alignment=ALIGN_LEFT_BOTTOM, size=15.0_wp, color=[0.9_wp, 0.9_wp, 0.9_wp])
    call window%add_hud_text(details)
    call window%set_event_handler(keys)

    running = .true.
    frame = 0
    do while (running .and. frame < frames)
        running = window%frame()
        frame = frame + 1
        if (demo .and. frame == 2) call demo_pick()
        if (frame == 2) then                            ! (--center=: as the panel's buttons)
            select case (trim(center_start))
            case ('earth')
                call center_on(keys, window, keys%earth, 0.8_wp)
            case ('moon')
                call center_on(keys, window, keys%moon, 0.5_wp)
            case ('point')
                call center_on(keys, window, keys%lagrange, merge(1.6_wp, 0.6_wp, keys%point == 3))
            end select
        end if
        ! the panel changed (the latest values asked for, once the last are
        ! done): another Lagrange point (its family's first orbit), another
        ! amplitude (Richardson's approximation, corrected), another Jacobi
        ! constant (continued from this orbit), or another propagation time
        ! (the manifolds only)
        orbit_changed = keys%point_requested /= keys%point .or. abs(keys%az_requested - az_asked) > 1.0e-6_wp .or. &
                        abs(keys%c_requested - keys%c) > 1.0e-9_wp
        if (orbit_changed .or. abs(keys%days_requested - keys%days) > 1.0e-9_wp) then
            call system_clock(tick, rate)
            keys%status = ''
            by_c = .false.
            if (keys%point_requested /= keys%point) then
                call set_point(keys%point_requested)
                ok = from_amplitude(keys%az_requested, .false.)
                if (.not. ok) keys%status = 'no halo orbit found there'
                call view_point()
            else if (abs(keys%az_requested - az_asked) > 1.0e-6_wp) then
                ok = from_amplitude(keys%az_requested, .true.)
                if (.not. ok) then
                    write(keys%status, '(a,i0,a)') 'no halo orbit found for Az = ', nint(keys%az_requested), ' km'
                    keys%az_requested = az_asked
                end if
            else if (orbit_changed) then
                by_c = .true.
                call continue_halo(x0, z0, vy0, keys%c_requested, period, c_reached, ok)
                if (.not. ok) then
                    write(keys%status, '(a,f9.6)') 'no halo orbit found beyond C = ', c_reached
                    keys%c_requested = c_reached
                end if
            end if
            call system_clock(tock)
            keys%continued = real(tock - tick, wp) / rate
            call keys%clear_selection()
            call compute(orbit_changed)
            if (orbit_changed) keys%c_requested = keys%c          ! (the other slider showing this orbit's)
            if (by_c) then
                keys%az_requested = max(keys%az_min, min(keys%az_max, keys%az))
                az_asked = keys%az_requested
            end if
        end if
    end do
    if (len_trim(file) > 0 .and. running) then
        if (window%capture(trim(file))) write(*, '(a)') 'captured '//trim(file)
    end if
    call window%close()

contains

    !> The Lagrange points, as in the CR3BP demo: crosses (a MarkerArtist,
    !> not pickable) and their labels (frames' name labels, just above them).
    subroutine add_lagrange_points()
        real(wp) :: l(3, 5), gl
        character(len=2) :: name
        integer :: k_
        l = 0.0_wp
        do k_ = 1, 3
            call collinear_point(k_, l(1, k_), gl)
        end do
        l(:, 4) = [0.5_wp - mu, sqrt(3.0_wp) / 2.0_wp, 0.0_wp]
        l(:, 5) = [0.5_wp - mu, -sqrt(3.0_wp) / 2.0_wp, 0.0_wp]
        lagrange = Trajectory()
        call lagrange%add([(real(k_, wp), k_ = 1, 5)], pos=lu * l)
        lagrange_dots = MarkerArtist(lagrange, markers=ior(ior(MARKER_START, MARKER_INTERMEDIATE), MARKER_END), &
                                     size=12.0_wp, color=[1.0_wp, 0.9_wp, 0.4_wp])
        call lagrange_dots%set_marker_shape(MARKER_PLUS)
        call lagrange_dots%set_intermediate_type(INTERMEDIATE_DATA)
        call lagrange_dots%set_intermediate_spacing(1.0_wp)
        call lagrange_dots%set_pickable(.false.)
        lagrange_frame = DrawableTrajectory('Lagrange points')
        call lagrange_frame%show_decorations(.false.)
        call lagrange_frame%add_artist(lagrange_dots)
        call lagrange_frame%set_pickable(.false.)
        if (.not. rotating%add_child(lagrange_frame)) error stop 'add_child failed'
        do k_ = 1, 5
            write(name, '(a,i0)') 'L', k_
            lagrange_labels(k_) = ReferenceFrame(name, color=[1.0_wp, 0.9_wp, 0.4_wp])
            call lagrange_labels(k_)%show_axes(NO_AXES)
            call lagrange_labels(k_)%show_axes_labels(NO_AXES)
            call lagrange_labels(k_)%set_label_size(14)
            call lagrange_labels(k_)%set_pickable(.false.)
            call lagrange_labels(k_)%set_position(lu * l(:, k_) + [0.0_wp, 0.035_wp * lu, 0.0_wp])
            if (.not. rotating%add_child(lagrange_labels(k_))) error stop 'add_child failed'
        end do
    end subroutine add_lagrange_points

    !> Lagrange point `lib`'s family of halo orbits: the amplitude slider's
    !> range (from the smallest halo's, where Richardson's approximation
    !> becomes feasible, to az_largest, less if no orbit is found there),
    !> the Jacobi constant slider's (those orbits'), the first amplitude
    !> asked for, and the tubes' side (toward the Earth).
    subroutine set_point(lib)
        integer, intent(in) :: lib
        real(wp) :: lo, hi, mid, rv(6), p, xa, za, va, c_lo, c_hi
        logical :: feasible
        integer :: it
        keys%point = lib
        keys%point_requested = lib
        earth_side = merge(1.0_wp, -1.0_wp, lib == 3)
        call collinear_point(lib, xa, za)
        keys%lagrange = [xa * lu, 0.0_wp, 0.0_wp]
        offset = offsets(lib)
        keys%days_requested = days_first(lib)
        lo = 0.0_wp                                     ! the smallest feasible amplitude (bisection)
        hi = az_largest(lib)
        do it = 1, 60
            mid = 0.5_wp * (lo + hi)
            call richardson_halo(lib, mid, 1, rv, p, feasible)
            if (feasible) then
                hi = mid
            else
                lo = mid
            end if
        end do
        keys%az_min = max(1000.0_wp, real(ceiling(hi * 1.02_wp / 100.0_wp), wp) * 100.0_wp)
        keys%az_max = az_largest(lib)
        do it = 1, 10                                   ! (the largest found)
            call halo_from_amplitude(lib, keys%az_max, xa, za, va, p, feasible)
            if (feasible) exit
            keys%az_max = keys%az_min + 0.8_wp * (keys%az_max - keys%az_min)
        end do
        c_lo = jacobi([xa, 0.0_wp, za, 0.0_wp, va, 0.0_wp])
        call halo_from_amplitude(lib, keys%az_min, xa, za, va, p, feasible)
        c_hi = merge(jacobi([xa, 0.0_wp, za, 0.0_wp, va, 0.0_wp]), c_lo + 0.05_wp, feasible)
        keys%c_min = min(c_lo, c_hi)
        keys%c_max = max(c_lo, c_hi)
        keys%az_requested = max(keys%az_min, min(keys%az_max, az_first(lib)))
        write(*, '(a,i0,a,i0,a,i0,a,f8.5,a,f8.5)') 'L', lib, ' halos: Az ', nint(keys%az_min), ' to ', &
            nint(keys%az_max), ' km, C ', keys%c_min, ' to ', keys%c_max
    end subroutine set_point

    !> The halo orbit of amplitude `az` (km) about the point: Richardson's
    !> approximation, corrected; failing that, with `same_family` (the orbit
    !> shown is the point's), by continuation in z0 from it (steps of at most
    !> 0.004, ~1500 km) toward the approximation's, else at smaller
    !> amplitudes (toward the smallest) until one converges. False if none
    !> did (the orbit kept).
    logical function from_amplitude(az, same_family) result(ok)
        real(wp), intent(in) :: az
        logical, intent(in) :: same_family
        real(wp) :: xa, za, va, p, rv(6), target, z_from, xs, zs, vs, ps, a
        integer :: n, k_
        az_asked = az
        call halo_from_amplitude(keys%point, az, xa, za, va, p, ok)
        if (.not. ok .and. .not. same_family) then
            a = az
            do k_ = 1, 12
                a = keys%az_min + 0.7_wp * (a - keys%az_min)
                call halo_from_amplitude(keys%point, a, xa, za, va, p, ok)
                if (ok) exit
            end do
            if (ok) then
                az_asked = a
                keys%az_requested = a
            end if
        else if (.not. ok) then
            call richardson_halo(keys%point, az, 1, rv, p, ok)
            if (.not. ok) return
            target = rv(3)
            xs = x0
            zs = z0
            vs = vy0
            z_from = z0
            n = max(1, ceiling(abs(target - z_from) / 0.004_wp))
            do k_ = 1, n
                zs = z_from + (target - z_from) * k_ / n
                call correct_halo(xs, zs, vs, ps, ok)
                if (.not. ok) return
            end do
            xa = xs
            za = zs
            va = vs
            p = ps
        end if
        if (.not. ok) return
        x0 = xa
        z0 = za
        vy0 = va
        period = p
    end function from_amplitude

    !> The camera: the point's halos, their tubes and the Earth (from above,
    !> behind; framed for each point: L3's halos are larger than the Earth-Moon
    !> distance).
    subroutine view_point()
        real(wp), parameter :: centers(3) = [0.25_wp, 0.45_wp, -0.75_wp]     !! x (Earth-Moon distances)
        real(wp), parameter :: scales(3) = [1.0_wp, 1.0_wp, 1.7_wp]          !! the camera's distance (times L1's)
        real(wp) :: center(3)
        center = [centers(keys%point), 0.0_wp, 0.0_wp] * lu
        call window%set_camera(center + scales(keys%point) * [-0.05_wp, -2.0_wp, 1.35_wp] * lu, center, &
                               [0.0_wp, 0.0_wp, 1.0_wp], in_root=.true.)
    end subroutine view_point

    !> The orbit (x0, z0, vy0; if `orbit`, else only the propagation time
    !> changed), its manifolds and zero-velocity curves, computed (the
    !> trajectories in parallel) and drawn.
    subroutine compute(orbit)
        logical, intent(in) :: orbit
        integer(8) :: t0, t1, rate
        integer :: j_
        call system_clock(t0, rate)
        if (orbit) call compute_orbit()
        keys%days = keys%days_requested
        span = keys%days / tu_days
        times = [(span * (j_ - 1) / (n_along - 1) * tu_days, j_ = 1, n_along)]
        call compute_tubes()
        call system_clock(t1)
        keys%seconds = real(t1 - t0, wp) / rate
        write(*, '(a,f9.6,a,f6.2,a,i0,a,f5.1,a,es10.3,a,f6.3,a,f6.3,a,i0,a)') 'C = ', keys%c, ': period ', keys%period_days, &
            ' days, Az ', nint(keys%az), ' km, propagated ', keys%days, ' days, lambda ', keys%lambda_u, &
            '; continued in ', keys%continued, ' s, manifolds in ', keys%seconds, ' s (', &
            keys%threads, ' threads)'
        call draw()
    end subroutine compute

    !> The orbit (n_orbit + 1 states over its period) and the manifolds'
    !> directions at n_around points: the monodromy matrix's eigenvectors
    !> carried along it by the state transition matrix, each a unit
    !> position, + toward the Moon at the first point.
    subroutine compute_orbit()
        real(wp) :: s(42), vu(6), vs(6), phi(6, 6)
        integer :: i, k_, sgn
        s = initial_state()
        call propagate(eom_stm, s, period)
        call monodromy_vectors(reshape(s(7:42), [6, 6]), vu, vs, keys%lambda_u)
        s = initial_state()
        do i = 1, n_orbit + 1
            orbit(:, i) = s(1:6)
            if (modulo(i - 1, 4) == 0 .and. i <= n_orbit) then
                phi = reshape(s(7:42), [6, 6])
                dirs(:, (i - 1) / 4 + 1, 1) = matmul(phi, vu)
                dirs(:, (i - 1) / 4 + 1, 2) = matmul(phi, vs)
            end if
            if (i <= n_orbit) call propagate(eom_stm, s, period / n_orbit)
        end do
        do k_ = 1, 2
            sgn = merge(1, -1, dirs(1, 1, k_) > 0.0_wp)
            do i = 1, n_around
                dirs(:, i, k_) = sgn * dirs(:, i, k_) / norm2(dirs(1:3, i, k_))
            end do
        end do
        keys%c = jacobi(orbit(:, 1))
        keys%period_days = period * tu_days
        keys%az = maxval(abs(orbit(3, :))) * lu
    end subroutine compute_orbit

    !> The tubes: each trajectory propagated (in parallel: they're
    !> independent), unstable forward, stable backward, toward the Earth,
    !> sampled at common times (stopping at the Earth or the Moon); the
    !> lines' details.
    subroutine compute_tubes()
        real(wp) :: w(6), r
        integer :: i, j_, k_, l_
        logical :: stopped
        !$omp parallel do collapse(2) schedule(dynamic) private(i, j_, k_, l_, w, r, stopped)
        do k_ = 1, n_tubes
            do i = 1, n_around
                w = orbit(:, 4 * (i - 1) + 1) + earth_side * offset * dirs(:, i, branch(k_))
                l_ = 0
                if (modulo(i - 1, n_around / n_lines) == 0) l_ = (i - 1) / (n_around / n_lines) + 1
                if (l_ > 0) keys%info(l_, k_) = LineInfo(index=i, n_around=n_around, closest=huge(1.0_wp), jacobi=jacobi(w))
                stopped = .false.
                last(i, k_) = n_along
                do j_ = 1, n_along
                    if (j_ > 1 .and. .not. stopped) then
                        call propagate(eom, w, direction(k_) * span / (n_along - 1))
                        stopped = norm2(w(1:3) - [-mu, 0.0_wp, 0.0_wp]) < re / lu .or. &
                                  norm2(w(1:3) - [1.0_wp - mu, 0.0_wp, 0.0_wp]) < rm / lu
                        if (stopped) last(i, k_) = j_
                        if (l_ > 0 .and. (j_ == n_along .or. stopped)) &
                            keys%info(l_, k_)%drift = jacobi(w) - keys%info(l_, k_)%jacobi
                    end if
                    grid(:, i, j_, k_) = w(1:3) * lu
                    keys%colors(:, i + (j_ - 1) * n_around, k_) = &
                        [tube_colors(:, k_), 1.0_wp - 0.85_wp * real(j_ - 1, wp) / (n_along - 1)]
                    if (l_ > 0 .and. j_ <= last(i, k_)) then
                        r = norm2(w(1:3) - [-mu, 0.0_wp, 0.0_wp]) * lu
                        if (r < keys%info(l_, k_)%closest) then
                            keys%info(l_, k_)%closest = r
                            keys%info(l_, k_)%closest_time = direction(k_) * times(j_)
                        end if
                    end if
                end do
                if (l_ > 0) keys%info(l_, k_)%impact = norm2(w(1:3) - [-mu, 0.0_wp, 0.0_wp]) < re / lu
            end do
        end do
        !$omp end parallel do
    end subroutine compute_tubes

    !> The orbit, the tubes, their lines (points in increasing time: the
    !> stable ones' toward the orbit, so the highlight's arrows show the
    !> motion; times in days) and the zero-velocity curves, drawn again.
    subroutine draw()
        real(wp), allocatable :: segments(:,:)
        integer, allocatable :: pairs(:,:)
        integer :: i, j_, k_, l_
        call halo%clear()
        do i = 1, n_orbit + 1
            call halo%add(period * (i - 1) / n_orbit * tu_days, pos=orbit(1:3, i) * lu)
        end do
        do k_ = 1, n_tubes
            call tubes(k_)%set_grid(grid(:, :, :, k_), closed_u=.true., lines=[0, 0])
            do l_ = 1, n_lines
                i = (l_ - 1) * (n_around / n_lines) + 1
                call paths(l_, k_)%clear()
                if (direction(k_) > 0.0_wp) then
                    do j_ = 1, last(i, k_)
                        call paths(l_, k_)%add(times(j_), pos=grid(:, i, j_, k_))
                    end do
                else
                    do j_ = last(i, k_), 1, -1
                        call paths(l_, k_)%add(-times(j_), pos=grid(:, i, j_, k_))
                    end do
                end if
            end do
        end do
        segments = zero_velocity_curves(keys%c, 1.4_wp, 600) * lu
        pairs = reshape([(i, i = 1, size(segments, 2))], [2, size(segments, 2) / 2])
        call zvc%set_triangles(segments, reshape([integer ::], [3, 0]))
        call zvc%set_edges(pairs)
        call keys%apply()                               ! (the tubes' colors and settings again)
    end subroutine draw

    !> (--demo-pick, for a capture) Select a point 3/5 along the 4th unstable
    !> trajectory, where it is on the screen.
    subroutine demo_pick()
        real(wp) :: at(2), p(3)
        p = paths(4, 1)%position(3 * paths(4, 1)%num_positions() / 5)
        if (window%screen_position(p, at)) call keys%select(window, 2.0_wp * at(1) - 1.0_wp, 2.0_wp * at(2) - 1.0_wp)
    end subroutine demo_pick

    !> The halo orbit's initial state, with the identity STM.
    function initial_state() result(s_)
        real(wp) :: s_(42)
        integer :: c
        s_ = 0.0_wp
        s_(1:6) = [x0, 0.0_wp, z0, 0.0_wp, vy0, 0.0_wp]
        do c = 1, 6
            s_(6 + 6 * (c - 1) + c) = 1.0_wp
        end do
    end function initial_state

end program halo_manifolds
