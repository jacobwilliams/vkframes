!> NASA's Artemis I mission (November 16 to December 11, 2022) with vkframes
!> in a qt-fortran GUI, from the flight's trajectory: Orion's states every
!> minute from JPL Horizons (the reconstructed trajectory, from after the
!> trans-lunar injection to just before entry), with the Moon's and the
!> Sun's, downloaded by tools/get_artemis1.py into data/artemis1/ (`pixi run
!> artemis-data`; the demo's run does it if they're missing), and the events
!> found in them: the burns (Orion's acceleration beyond gravity's, with
!> their delta-v), the two lunar flybys (130 km up) and the farthest point
!> from the Earth.
!>
!> The scene: the Earth turning (its rotation angle; with its night lights),
!> the Moon on its ephemeris (its near side toward the Earth), the Sun as the
!> scene's light (at its position), the stars and constellations, Orion and
!> its trajectory, colored by phase (outbound, in the distant retrograde
!> orbit, return), with the events marked and labeled along it.
!>
!> The dock's Frame menu shows the mission from one of five frames, the
!> trajectories drawn in it (each point where it was in that frame at its
!> time: DrawableTrajectory's set_data_frame), the camera riding with it:
!> * Earth-centered inertial (J2000): the trajectory as flown;
!> * Earth-Moon rotating (x toward the Moon, z along its orbit's normal): the
!>   mission's familiar picture, the Moon fixed on the right, the outbound
!>   and return legs looping about it, the distant retrograde orbit beyond;
!> * Moon-centered inertial, and Moon-fixed (the Moon-centered rotating
!>   frame, x toward the Earth): the flybys and the distant retrograde orbit;
!> * Orion: the camera following the spacecraft (inertial axes).
!> Ticked axes mark the frame chosen. Trails (the last two days), event
!> labels, the time scale, Pause / Resume, a time slider (the mission's
!> days) and "Go to event". "Export web page..." writes the mission to a web
!> page (WindowProxy%export_web: the 25 days animated, the frame chosen, to
!> open in a browser).
!>
!> Run with `pixi run -e qt artemis-demo` (from the vkframes root). Options:
!> `--frame k` (1 to 5) to start in, `--export file.html` (a web page written
!> at the 20th frame), `--capture file`, `--capture-frame n`, `--quit-after
!> seconds`.
module artemis_demo

    use qt
    use vkframes
    use vkframes_qt, only: connect_view, ViewLink, DemoOptions
    use vkf_math, only: cross
    use iso_c_binding

    implicit none

    real(wp), parameter :: re = 6378.137_wp, rm = 1737.4_wp     !! km
    real(wp), parameter :: tdb_utc = 69.184_wp                    !! TDB - UTC in 2022 (s)
    real(wp), parameter :: launch_jd_utc = 2459899.5_wp + (6.0_wp + 47.0_wp / 60.0_wp + 44.0_wp / 3600.0_wp) / 24.0_wp
    !! launch: 2022-11-16 06:47:44 UTC
    real(wp), parameter :: trail_days = 2.0_wp

    integer, parameter :: n_frames = 5
    character(len=40), parameter :: frame_names(n_frames) = [character(len=40) :: &
        'Earth-centered inertial (J2000)', 'Earth-Moon rotating', 'Moon-centered inertial', &
        'Moon-fixed (Moon-centered rotating)', 'Orion']
    !> each frame's view: the eye's direction and distance, the center, up (km)
    real(wp), parameter :: view_distance(n_frames) = [1.3e6_wp, 8.5e5_wp, 3.2e5_wp, 3.2e5_wp, 6.0e4_wp]
    real(wp), parameter :: view_center(3, n_frames) = reshape([0.0_wp, 0.0_wp, 0.0_wp, 2.2e5_wp, 0.0_wp, 0.0_wp, &
        0.0_wp, 0.0_wp, 0.0_wp, 0.0_wp, 0.0_wp, 0.0_wp, 0.0_wp, 0.0_wp, 0.0_wp], [3, n_frames])
    real(wp), parameter :: view_eye(3, n_frames) = reshape([0.0_wp, -0.35_wp, 0.94_wp, 0.0_wp, -0.35_wp, 0.94_wp, &
        0.0_wp, -0.35_wp, 0.94_wp, 0.0_wp, 0.35_wp, 0.94_wp, 0.55_wp, -0.65_wp, 0.52_wp], [3, n_frames])
    real(wp), parameter :: view_up(3, n_frames) = reshape([0.0_wp, 1.0_wp, 0.0_wp, 0.0_wp, 1.0_wp, 0.0_wp, &
        0.0_wp, 1.0_wp, 0.0_wp, 0.0_wp, -1.0_wp, 0.0_wp, 0.0_wp, 0.0_wp, 1.0_wp], [3, n_frames])
    !> each frame's ticked axes: length, major and minor tick spacing (km)
    real(wp), parameter :: tick_axes(3, n_frames) = reshape([4.0e5_wp, 1.0e5_wp, 5.0e4_wp, 4.0e5_wp, 1.0e5_wp, 5.0e4_wp, &
        1.0e5_wp, 2.0e4_wp, 1.0e4_wp, 1.0e5_wp, 2.0e4_wp, 1.0e4_wp, 2.0e4_wp, 5.0e3_wp, 1.0e3_wp], [3, n_frames])

    real(wp), parameter :: outbound_color(3) = [0.35_wp, 0.8_wp, 1.0_wp], dro_color(3) = [1.0_wp, 0.65_wp, 0.25_wp], &
                           return_color(3) = [0.45_wp, 1.0_wp, 0.55_wp], moon_color(3) = [0.75_wp, 0.75_wp, 0.72_wp], &
                           event_color(3) = [1.0_wp, 0.95_wp, 0.7_wp]

    !> A frame pointer (for the frames' menu).
    type :: FrameRef
        class(ReferenceFrame), pointer :: p => null()
    end type FrameRef

    !> An event: its time, text, and its marker (a one-point trajectory at its frame's origin).
    type :: MissionEvent
        real(wp) :: t = 0.0_wp
        character(len=:), allocatable :: text
        type(Trajectory) :: point
        type(MarkerArtist) :: marker
        type(DrawableTrajectory) :: frame
    end type MissionEvent

    !> Each frame: the time looped over the mission.
    type, extends(WindowEventHandler) :: ArtemisHandler
    contains
        procedure :: before_update => artemis_before_update
    end type ArtemisHandler

    type :: app_t
        ! the window
        type(QMainWindow) :: main
        type(QWindow) :: window
        type(QWidget) :: container
        type(QtfEventFilter) :: events_filter
        type(ViewLink) :: view_link
        type(WindowProxy) :: proxy
        type(View) :: views(n_frames)
        integer :: frame = 0
        ! controls
        type(QComboBox) :: frame_choice, event_choice
        type(QCheckBox) :: trails, labels
        type(QDoubleSpinBox) :: time_scale
        type(QPushButton) :: pause
        type(QSlider) :: slider                             !! the time, in 10-minute steps
        type(QLabel) :: slider_time, export_status
        type(QWidget) :: central
        logical :: setting_slider = .false., holding_slider = .false., paused = .false.
        type(HUDText) :: hud_title, hud_state, hud_event
        ! the data: Orion's states, the Moon's and the Sun's, the events
        real(wp) :: epoch = 0.0_wp                          !! julian date (TDB) of t = 0
        real(wp), allocatable :: ot(:), os(:,:), mt(:), ms(:,:), st(:), ss(:,:)
        type(Trajectory) :: orion, moon_eph, rot_eph, earth_spin, sun_eph
        type(MissionEvent), allocatable :: events(:)
        real(wp) :: span = 0.0_wp                           !! the mission's time (s)
        ! the scene
        type(ReferenceFrame) :: eci, rot, moon_ci, sun
        type(Sphere) :: earth, moon
        type(DrawableTrajectory) :: craft, orion_path, moon_path
        type(MarkerArtist) :: craft_marker
        type(CurveArtist) :: orion_curve, moon_curve
        type(CoordinateAxes) :: ticks
        type(FrameRef) :: frames(n_frames), shown_in
        type(SkySphere) :: sky
        type(FrameManager) :: scene
        ! options
        integer :: start_frame = 2
        type(DemoOptions) :: options
        character(len=:), allocatable :: export_file
    end type app_t

    type(app_t), target, save :: app
    type(ArtemisHandler), target, save :: handler

contains

    ! ==================================================================
    ! the data

    !> A data file's states (data/artemis1/<name>, as get_artemis1.py writes
    !> them): times (s from the epoch) and states (6, n); its epoch (julian
    !> date, TDB).
    subroutine read_states(name, t, s, epoch)
        character(len=*), intent(in) :: name
        real(wp), allocatable, intent(out) :: t(:), s(:,:)
        real(wp), intent(out) :: epoch
        character(len=:), allocatable :: path
        character(len=512) :: line
        integer :: u, status, n, k
        path = find_data_file('artemis1/'//name)
        if (len(path) == 0) then
            write(*, '(a)') 'artemis demo: no data/artemis1/'//name//' (run `pixi run artemis-data`)'
            error stop 1
        end if
        open(newunit=u, file=path, status='old', action='read')
        n = 0
        epoch = 0.0_wp
        do
            read(u, '(a)', iostat=status) line
            if (status /= 0) exit
            if (line(1:1) == '#') then
                if (index(line, 'epoch JD TDB') > 0) read(line(index(line, 'TDB') + 3:), *) epoch
                cycle
            end if
            n = n + 1
        end do
        allocate(t(n), s(6, n))
        rewind(u)
        k = 0
        do
            read(u, '(a)', iostat=status) line
            if (status /= 0) exit
            if (line(1:1) == '#') cycle
            k = k + 1
            read(line, *) t(k), s(:, k)
        end do
        close(u)
    end subroutine read_states

    !> The events (data/artemis1/events.txt): time and text.
    subroutine read_events()
        character(len=:), allocatable :: path
        character(len=512) :: line
        integer :: u, status, n, k, i
        path = find_data_file('artemis1/events.txt')
        allocate(app%events(0))
        if (len(path) == 0) return
        open(newunit=u, file=path, status='old', action='read')
        n = 0
        do
            read(u, '(a)', iostat=status) line
            if (status /= 0) exit
            if (line(1:1) /= '#' .and. len_trim(line) > 0) n = n + 1
        end do
        deallocate(app%events)
        allocate(app%events(n))
        rewind(u)
        k = 0
        do
            read(u, '(a)', iostat=status) line
            if (status /= 0) exit
            if (line(1:1) == '#' .or. len_trim(line) == 0) cycle
            k = k + 1
            i = index(adjustl(line), ' ')
            line = adjustl(line)
            read(line(1:i-1), *) app%events(k)%t
            app%events(k)%text = trim(adjustl(line(i:)))
        end do
        close(u)
    end subroutine read_events

    !> The time of the first event whose text has `word` (huge if none).
    real(wp) function event_time(word) result(t)
        character(len=*), intent(in) :: word
        integer :: k
        t = huge(1.0_wp)
        do k = 1, size(app%events)
            if (index(app%events(k)%text, word) > 0) then
                t = app%events(k)%t
                return
            end if
        end do
    end function event_time

    !> The data read and made into trajectories: Orion's (colored by phase),
    !> the Moon's ephemeris (positions; attitudes: x toward the Earth, z its
    !> orbit's normal), the Earth-Moon rotating frame's attitudes (x toward the
    !> Moon), the Earth's rotation (its rotation angle) and the Sun's positions.
    subroutine make_data()
        real(wp) :: epoch2, t_dro, t_return, axes(3, 3), n(3), jd_ut1, era, rgb(3)
        integer :: k
        call read_states('orion.txt', app%ot, app%os, app%epoch)
        call read_states('moon.txt', app%mt, app%ms, epoch2)
        call read_states('sun.txt', app%st, app%ss, epoch2)
        call read_events()
        app%span = app%ot(size(app%ot))
        t_dro = event_time('insertion')
        t_return = event_time('departure')
        app%orion = Trajectory()
        call app%orion%reserve(size(app%ot), colors=.true.)
        do k = 1, size(app%ot)
            rgb = outbound_color
            if (app%ot(k) >= t_dro) rgb = dro_color
            if (app%ot(k) >= t_return) rgb = return_color
            call app%orion%add(app%ot(k), pos=app%os(1:3, k), color=[rgb, 1.0_wp])
        end do
        app%moon_eph = Trajectory()
        app%rot_eph = Trajectory()
        app%earth_spin = Trajectory()
        call app%moon_eph%reserve(size(app%mt), attitudes=.true.)
        call app%rot_eph%reserve(size(app%mt), attitudes=.true.)
        call app%earth_spin%reserve(size(app%mt), attitudes=.true.)
        do k = 1, size(app%mt)
            n = cross(app%ms(1:3, k), app%ms(4:6, k))
            axes(:, 3) = n / norm2(n)
            axes(:, 1) = app%ms(1:3, k) / norm2(app%ms(1:3, k))
            axes(:, 2) = cross(axes(:, 3), axes(:, 1))
            call app%rot_eph%add(app%mt(k), pos=[0.0_wp, 0.0_wp, 0.0_wp], att=quat_from_matrix(axes))
            axes(:, 1) = -axes(:, 1)                        ! (the Moon: tidally locked, x toward the Earth)
            axes(:, 2) = -axes(:, 2)
            call app%moon_eph%add(app%mt(k), pos=app%ms(1:3, k), att=quat_from_matrix(axes))
            ! the Earth's rotation angle (UT1 ~ UTC; the equator of date ~ J2000's)
            jd_ut1 = app%epoch + (app%mt(k) - tdb_utc) / 86400.0_wp
            era = 2.0_wp * pi * modulo(0.7790572732640_wp + 1.00273781191135448_wp * (jd_ut1 - 2451545.0_wp), 1.0_wp)
            call app%earth_spin%add(app%mt(k), pos=[0.0_wp, 0.0_wp, 0.0_wp], &
                                    att=quat_from_axis_angle([0.0_wp, 0.0_wp, 1.0_wp], era))
        end do
        app%sun_eph = Trajectory()
        do k = 1, size(app%st)
            call app%sun_eph%add(app%st(k), pos=app%ss(1:3, k))
        end do
    end subroutine make_data

    ! ==================================================================
    ! the scene

    subroutine build_model()
        integer :: k
        logical :: ok
        call make_data()
        app%eci = ReferenceFrame('Earth-centered inertial')
        call app%eci%show_decorations(.false.)
        app%rot = ReferenceFrame('Earth-Moon rotating')
        call app%rot%show_decorations(.false.)
        call app%rot%follow(app%rot_eph, data=FOLLOW_ATTITUDE, mode=FOLLOW_LIMIT)
        app%moon_ci = ReferenceFrame('Moon-centered inertial')
        call app%moon_ci%show_decorations(.false.)
        call app%moon_ci%follow(app%moon_eph, data=FOLLOW_POSITION, mode=FOLLOW_LIMIT)

        app%earth = Sphere('Earth', radius=re)
        call app%earth%set_texture_map('2k_earth_daymap.jpg')
        call app%earth%set_night_texture_map('2k_earth_nightmap.jpg')
        call app%earth%follow(app%earth_spin, data=FOLLOW_ATTITUDE, mode=FOLLOW_LIMIT)
        call app%earth%show_axes(NO_AXES)
        call app%earth%show_axes_labels(NO_AXES)
        call app%earth%set_name_label_on_top(.true.)
        app%moon = Sphere('Moon', radius=rm, color=moon_color)
        call app%moon%set_texture_map('lroc_color_2k.jpg')
        call app%moon%follow(app%moon_eph, mode=FOLLOW_LIMIT)
        call app%moon%show_axes(NO_AXES)
        call app%moon%show_axes_labels(NO_AXES)
        call app%moon%set_name_label_on_top(.true.)
        ! the Sun: the scene's light, where it is
        app%sun = ReferenceFrame('Sun')
        call app%sun%show_decorations(.false.)
        call app%sun%follow(app%sun_eph, data=FOLLOW_POSITION, mode=FOLLOW_LIMIT)
        call app%sun%set_light_source_enabled(.true.)
        call app%sun%set_light_ambient([0.12_wp, 0.12_wp, 0.12_wp])
        call app%sun%set_light_diffuse([1.0_wp, 1.0_wp, 1.0_wp])

        ! Orion: a marker following its trajectory (inertial), named
        app%craft_marker = MarkerArtist(app%orion, markers=MARKER_START, size=11.0_wp, color=[1.0_wp, 1.0_wp, 1.0_wp])
        ok = app%craft_marker%set_x_data(DataSource(src=SOURCE_ZERO)) .and. &   ! (at the frame's origin, which
             app%craft_marker%set_y_data(DataSource(src=SOURCE_ZERO)) .and. &   !  follows Orion)
             app%craft_marker%set_z_data(DataSource(src=SOURCE_ZERO))
        app%craft = DrawableTrajectory('Orion', color=[1.0_wp, 1.0_wp, 1.0_wp])
        call app%craft%add_artist(app%craft_marker)
        call app%craft%show_axes(NO_AXES)
        call app%craft%show_axes_labels(NO_AXES)
        call app%craft%follow(app%orion, data=FOLLOW_POSITION, mode=FOLLOW_LIMIT)

        ! the trajectories (inertial data), drawn in the frame chosen
        app%orion_curve = CurveArtist(app%orion, color=outbound_color, width=2.0_wp)
        app%orion_path = DrawableTrajectory('Orion''s trajectory')
        call app%orion_path%add_artist(app%orion_curve)
        call app%orion_path%show_decorations(.false.)
        app%moon_curve = CurveArtist(app%moon_eph, color=moon_color, width=1.0_wp)
        app%moon_path = DrawableTrajectory('The Moon''s orbit')
        call app%moon_path%add_artist(app%moon_curve)
        call app%moon_path%show_decorations(.false.)

        ! the events: a marker and a label each (placed in the frame chosen)
        do k = 1, size(app%events)
            app%events(k)%point = Trajectory()
            call app%events(k)%point%add(app%events(k)%t, pos=[0.0_wp, 0.0_wp, 0.0_wp])
            app%events(k)%marker = MarkerArtist(app%events(k)%point, markers=MARKER_START, size=7.0_wp, &
                                                color=event_color)
            app%events(k)%frame = DrawableTrajectory(app%events(k)%text, color=[event_color, 0.9_wp])
            call app%events(k)%frame%add_artist(app%events(k)%marker)
            call app%events(k)%frame%show_axes(NO_AXES)
            call app%events(k)%frame%show_axes_labels(NO_AXES)
            call app%events(k)%frame%set_label_size(13)
            ! (the labels at the Moon each on their own side: both flybys pass 130 km up)
            if (index(app%events(k)%text, 'Outbound powered') > 0) &
                call app%events(k)%frame%set_label_alignment(ALIGN_RIGHT_BOTTOM)
            if (index(app%events(k)%text, 'Return powered') > 0) &
                call app%events(k)%frame%set_label_alignment(ALIGN_RIGHT_TOP)
            if (index(app%events(k)%text, 'Farthest') > 0) call app%events(k)%frame%set_label_alignment(ALIGN_LEFT_TOP)
        end do

        ! the ticked axes of the frame chosen
        app%ticks = CoordinateAxes('Axes', color=[0.6_wp, 0.65_wp, 0.8_wp, 0.7_wp])
        call app%ticks%show_axes(NO_AXES)
        call app%ticks%show_name_label(.false.)
        call app%ticks%set_label_size(13)
        call app%ticks%set_draw_axes(ior(X_AXIS, Y_AXIS))   ! (the views look down on the Moon's orbit)

        if (.not. (app%eci%add_child(app%earth) .and. app%eci%add_child(app%moon) .and. &
                   app%eci%add_child(app%rot) .and. app%eci%add_child(app%moon_ci) .and. &
                   app%eci%add_child(app%sun) .and. app%eci%add_child(app%craft))) &
            error stop 'artemis demo: could not build the scene'
        app%frames(1)%p => app%eci
        app%frames(2)%p => app%rot
        app%frames(3)%p => app%moon_ci
        app%frames(4)%p => app%moon
        app%frames(5)%p => app%craft
        app%shown_in%p => null()
        app%scene = FrameManager(app%eci)

        app%sky = SkySphere()
        if (.not. app%sky%set_star_data('Stars_HYGv3.txt', min_mag=-2.0_wp, max_mag=6.0_wp, max_stars=20000)) &
            write(*, '(a)') 'artemis demo: no star catalog'
        if (.not. app%sky%set_constellations(.true., 'inp_Constellation.txt', rgba=[0.3_wp, 0.4_wp, 0.65_wp, 0.3_wp])) &
            write(*, '(a)') 'artemis demo: no constellations'
        call apply_frame(app%start_frame)
        call apply_trails(.false.)
        call apply_labels(.true.)
    end subroutine build_model

    !> The trajectories, the events and the ticked axes moved to frame k
    !> (drawn in it, from their inertial data; as they are in the inertial
    !> frame).
    subroutine apply_frame(k)
        integer, intent(in) :: k                            !! the frame (1 to n_frames)
        class(ReferenceFrame), pointer :: f
        real(wp) :: m(4, 4), p(3)
        integer :: j
        f => app%frames(k)%p
        if (associated(app%shown_in%p)) then
            if (associated(app%shown_in%p, f)) return
            if (.not. (app%shown_in%p%remove_child(app%orion_path) .and. app%shown_in%p%remove_child(app%moon_path) &
                       .and. app%shown_in%p%remove_child(app%ticks))) write(*, '(a)') 'artemis demo: could not move'
            do j = 1, size(app%events)
                if (.not. app%shown_in%p%remove_child(app%events(j)%frame)) write(*, '(a)') 'artemis demo: could not move'
            end do
        end if
        if (.not. (f%add_child(app%orion_path) .and. f%add_child(app%moon_path) .and. f%add_child(app%ticks))) &
            write(*, '(a)') 'artemis demo: could not move'
        app%shown_in%p => f
        if (associated(f, app%eci)) then
            call app%orion_path%set_data_frame()
            call app%moon_path%set_data_frame()
        else
            call app%orion_path%set_data_frame(app%eci)
            call app%moon_path%set_data_frame(app%eci)
        end if
        ! each event where Orion was in this frame at its time
        do j = 1, size(app%events)
            m = rigid_inverse(f%world_matrix_at(app%events(j)%t))
            p = orion_at(app%events(j)%t)
            call app%events(j)%frame%set_position(matmul(m(1:3, 1:3), p) + m(1:3, 4))
            if (.not. f%add_child(app%events(j)%frame)) write(*, '(a)') 'artemis demo: could not move an event'
        end do
        call app%ticks%set_axis_length(tick_axes(1, k))
        call app%ticks%set_tick_spacing(tick_axes(2, k), tick_axes(3, k))
        call app%ticks%move_x_axis([0.0_wp, 0.0_wp, 0.0_wp], 1.03_wp * tick_axes(1, k))
        call app%ticks%move_y_axis([0.0_wp, 0.0_wp, 0.0_wp], 1.03_wp * tick_axes(1, k))
        call app%ticks%move_z_axis([0.0_wp, 0.0_wp, 0.0_wp], 1.03_wp * tick_axes(1, k))
        call app%hud_title%set_text('Artemis I:  '//trim(frame_names(k)))
    end subroutine apply_frame

    !> Orion's position at time t (ECI; linear between the minutes).
    function orion_at(t) result(p)
        real(wp), intent(in) :: t
        real(wp) :: p(3)
        integer :: i
        i = max(1, min(size(app%ot) - 1, int(t / (app%ot(2) - app%ot(1))) + 1))
        do while (i > 1 .and. app%ot(i) > t)
            i = i - 1
        end do
        do while (i < size(app%ot) - 1 .and. app%ot(i + 1) < t)
            i = i + 1
        end do
        p = app%os(1:3, i) + (app%os(1:3, i + 1) - app%os(1:3, i)) * &
            max(0.0_wp, min(1.0_wp, (t - app%ot(i)) / (app%ot(i + 1) - app%ot(i))))
    end function orion_at

    !> Trails (the last two days), or the whole mission.
    subroutine apply_trails(on)
        logical, intent(in) :: on
        if (on) then
            call app%orion_curve%set_trail(trail_days * 86400.0_wp, 0.0_wp, fade=0.6_wp)
            call app%moon_curve%set_trail(trail_days * 86400.0_wp, 0.0_wp, fade=0.6_wp)
        else
            call app%orion_curve%remove_trail()
            call app%moon_curve%remove_trail()
        end if
    end subroutine apply_trails

    !> Events' labels shown or hidden (their markers stay; the small
    !> trajectory correction burns' never labeled, to keep the picture clear).
    subroutine apply_labels(on)
        logical, intent(in) :: on
        integer :: k
        do k = 1, size(app%events)
            call app%events(k)%frame%show_name_label(on .and. index(app%events(k)%text, 'correction') == 0)
        end do
    end subroutine apply_labels

    ! ==================================================================
    ! the frames

    subroutine artemis_before_update(self, window, t)
        class(ArtemisHandler), intent(inout) :: self
        class(WindowProxy), intent(inout) :: window
        real(wp), intent(in) :: t
        if (t >= app%span .or. t < 0.0_wp) call window%set_time(0.0_wp)
    end subroutine artemis_before_update

    !> A julian date as 'YYYY-MM-DD hh:mm UTC'.
    function utc_text(jd) result(s)
        real(wp), intent(in) :: jd                          !! (UTC)
        character(len=:), allocatable :: s
        character(len=32) :: buf
        integer :: z, a, b, c, d, e, day, month, year, minutes, alpha
        real(wp) :: f
        z = int(jd + 0.5_wp)
        f = jd + 0.5_wp - z
        alpha = int((z - 1867216.25_wp) / 36524.25_wp)
        a = z + 1 + alpha - alpha / 4
        b = a + 1524
        c = int((b - 122.1_wp) / 365.25_wp)
        d = int(365.25_wp * c)
        e = int((b - d) / 30.6001_wp)
        day = b - d - int(30.6001_wp * e)
        month = merge(e - 1, e - 13, e < 14)
        year = merge(c - 4716, c - 4715, month > 2)
        minutes = min(1439, int(f * 1440.0_wp))
        write(buf, '(i4.4,a,i2.2,a,i2.2,a,i2.2,a,i2.2,a)') year, '-', month, '-', day, ' ', minutes / 60, ':', &
            mod(minutes, 60), ' UTC'
        s = trim(buf)
    end function utc_text

    !> The HUD: the date, the mission's elapsed time, Orion's distances and
    !> speed, the last event passed.
    subroutine update_hud()
        character(len=200) :: text
        real(wp) :: t, jd, met, p(3), v(3), dm
        integer :: i, k
        t = app%proxy%time()
        jd = app%epoch + (t - tdb_utc) / 86400.0_wp
        met = (jd - launch_jd_utc) * 86400.0_wp
        i = max(1, min(size(app%ot), int(t / (app%ot(2) - app%ot(1))) + 1))
        p = orion_at(t)
        v = app%os(4:6, i)
        k = max(1, min(size(app%mt), int(t / (app%mt(2) - app%mt(1))) + 1))
        dm = norm2(p - app%ms(1:3, k)) - rm
        write(text, '(a,a,i0,a,i2.2,a,i2.2,a,i0,a,i0,a,f6.3,a)') utc_text(jd), '    day ', int(met / 86400.0_wp), &
            ' ', int(mod(met, 86400.0_wp) / 3600.0_wp), 'h', int(mod(met, 3600.0_wp) / 60.0_wp), &
            'm    from the Earth''s center ', nint(norm2(p)), ' km    above the Moon ', nint(dm), ' km    speed ', &
            norm2(v), ' km/s'
        call app%hud_state%set_text(trim(text))
        text = ''
        do k = 1, size(app%events)
            if (app%events(k)%t <= t) text = app%events(k)%text
        end do
        call app%hud_event%set_text(trim(text))
        call follow_time()
    end subroutine update_hud

    !> The slider and its label at the current time (not while held).
    subroutine follow_time()
        character(len=32) :: text
        integer :: step
        step = nint(app%proxy%time() / 600.0_wp)
        write(text, '(a,f6.2)') 'day ', app%proxy%time() / 86400.0_wp
        call app%slider_time%setText(trim(text))
        if (app%holding_slider .or. app%slider%value() == step) return
        app%setting_slider = .true.
        call app%slider%setValue(step)
        app%setting_slider = .false.
    end subroutine follow_time

    subroutine build_window()
        integer :: k
        do k = 1, n_frames
            if (k == n_frames) then
                app%views(k) = View(app%eci, app%frames(k)%p, frame_type=ABSOLUTE_FRAME)
            else
                app%views(k) = View(app%eci, app%frames(k)%p)
            end if
            call app%views(k)%set_default_view_parameters( &
                eye=view_center(:, k) + view_distance(k) * view_eye(:, k) / norm2(view_eye(:, k)), &
                center=view_center(:, k), up=view_up(:, k))
        end do
        app%hud_title = HUDText('', position=[0.01_wp, 0.99_wp], alignment=ALIGN_LEFT_TOP, size=18.0_wp)
        app%hud_event = HUDText('', position=[0.01_wp, 0.95_wp], alignment=ALIGN_LEFT_TOP, size=15.0_wp, &
                                color=[event_color, 1.0_wp])
        app%hud_state = HUDText('', position=[0.01_wp, 0.01_wp], alignment=ALIGN_LEFT_BOTTOM, size=14.0_wp, &
                                color=[0.8_wp, 0.85_wp, 1.0_wp])
        app%window = QWindow()
        app%container = QWidget_createWindowContainer(app%window)
        call app%container%setMinimumSize(320, 240)
        app%events_filter = QtfEventFilter(app%window)
        call app%events_filter%exposeEvent%connect(on_expose)
        call app%events_filter%updateRequest%connect(on_update)
        call connect_view(app%view_link, app%events_filter, app%proxy, app%window)
    end subroutine build_window

    subroutine open_view()
        integer :: k
        if (.not. app%proxy%open(app%window%width(), app%window%height(), 'vkframes artemis', &
                                 native_window=app%window%winId(), pixel_ratio=app%window%devicePixelRatio())) then
            write(*, '(a)') 'artemis demo: could not open the view'
            return
        end if
        if (app%proxy%num_views() > 0) return
        call app%proxy%set_scene(app%scene)
        call app%proxy%set_event_handler(handler)
        call app%proxy%set_background_color([0.0_wp, 0.0_wp, 0.0_wp])
        call app%proxy%set_sky(app%sky)
        do k = 1, n_frames
            call app%proxy%add_view(app%views(k))
        end do
        call app%proxy%set_view_transition(1.0_wp)
        call app%proxy%select_view(app%frame_choice%currentIndex() + 1)
        call app%proxy%add_hud_text(app%hud_title)
        call app%proxy%add_hud_text(app%hud_event)
        call app%proxy%add_hud_text(app%hud_state)
        call app%proxy%set_time_scale(app%time_scale%value())
        call app%proxy%set_time(0.0_wp)
        call app%proxy%pause_time(app%paused)
    end subroutine open_view

    subroutine on_expose(data)
        class(*), intent(inout), optional :: data
        if (app%proxy%is_open() .or. .not. app%window%isExposed()) return
        call open_view()
        if (app%proxy%is_open()) call app%window%requestUpdate()
    end subroutine on_expose

    subroutine on_update(data)
        class(*), intent(inout), optional :: data
        if (app%window%devicePixelRatio() /= app%proxy%pixel_ratio()) &
            call app%proxy%resize(nint(app%window%width() * app%window%devicePixelRatio()), &
                                  nint(app%window%height() * app%window%devicePixelRatio()), &
                                  pixel_ratio=app%window%devicePixelRatio())
        call update_hud()
        if (.not. app%proxy%frame()) return
        app%frame = app%frame + 1
        if (allocated(app%export_file) .and. app%frame == 20) call export_page(app%export_file)
        if (app%options%after_frame(app%proxy, app%window, app%frame)) return
        call app%window%requestUpdate()
    end subroutine on_update

    ! ==================================================================
    ! controls

    subroutine on_frame(index, data)
        integer, intent(in) :: index
        class(*), intent(inout), optional :: data
        call apply_frame(index + 1)
        if (app%proxy%is_open()) call app%proxy%select_view(index + 1)
    end subroutine on_frame

    !> An event chosen: the time set to an hour before it.
    subroutine on_event(index, data)
        integer, intent(in) :: index                        !! 0: the prompt; then the events
        class(*), intent(inout), optional :: data
        if (index < 1 .or. index > size(app%events)) return
        if (app%proxy%is_open()) call app%proxy%set_time(max(0.0_wp, app%events(index)%t - 3600.0_wp))
    end subroutine on_event

    subroutine on_trails(checked, data)
        logical, intent(in) :: checked
        class(*), intent(inout), optional :: data
        call apply_trails(checked)
    end subroutine on_trails

    subroutine on_labels(checked, data)
        logical, intent(in) :: checked
        class(*), intent(inout), optional :: data
        call apply_labels(checked)
    end subroutine on_labels

    subroutine on_slider(value, data)
        integer, intent(in) :: value                        !! 10-minute steps
        class(*), intent(inout), optional :: data
        if (app%setting_slider) return
        if (app%proxy%is_open()) call app%proxy%set_time(600.0_wp * value)
    end subroutine on_slider

    subroutine on_slider_pressed(data)
        class(*), intent(inout), optional :: data
        app%holding_slider = .true.
    end subroutine on_slider_pressed

    subroutine on_slider_released(data)
        class(*), intent(inout), optional :: data
        app%holding_slider = .false.
    end subroutine on_slider_released

    subroutine on_time_scale(value, data)
        real(c_double), intent(in) :: value
        class(*), intent(inout), optional :: data
        if (app%proxy%is_open()) call app%proxy%set_time_scale(real(value, wp))
    end subroutine on_time_scale

    subroutine on_pause(data)
        class(*), intent(inout), optional :: data
        app%paused = .not. app%paused
        if (app%proxy%is_open()) call app%proxy%pause_time(app%paused)
        call app%pause%setText(trim(merge('Resume', 'Pause ', app%paused)))
    end subroutine on_pause

    subroutine on_reset_view(data)
        class(*), intent(inout), optional :: data
        type(View), pointer :: v
        v => app%proxy%current_view()
        call v%reset_view()
    end subroutine on_reset_view

    !> Export a web page: a file chosen, the mission written to it (the 25
    !> days animated, the frame chosen).
    subroutine on_export(data)
        class(*), intent(inout), optional :: data
        character(len=:), allocatable :: path
        path = QFileDialog_getSaveFileName('Export a web page', 'artemis1.html', 'Web pages (*.html)', app%main)
        if (len_trim(path) == 0) return
        if (index(path, '.htm', back=.true.) == 0) path = trim(path)//'.html'
        call export_page(trim(path))
    end subroutine on_export

    subroutine export_page(path)
        character(len=*), intent(in) :: path
        logical :: ok
        ! (the date and the last event: true only now; the page has its own clock)
        call app%hud_state%show(.false.)
        call app%hud_event%show(.false.)
        ok = app%proxy%export_web(path, animate=.true., t_start=0.0_wp, t_end=app%span, &
                                  title='Artemis I: '//trim(frame_names(app%frame_choice%currentIndex() + 1)))
        call app%hud_state%show(.true.)
        call app%hud_event%show(.true.)
        if (ok) then
            call app%export_status%setText('Wrote '//path)
        else
            call app%export_status%setText('Could not write '//path)
        end if
    end subroutine export_page

    subroutine on_close(event, data)
        type(QCloseEvent), intent(in) :: event
        class(*), intent(inout), optional :: data
        call app%proxy%close()
    end subroutine on_close

    subroutine build_ui()
        type(QDockWidget) :: dock
        type(QWidget) :: panel
        type(QFormLayout) :: form
        type(QPushButton) :: reset_view, export
        type(QHBoxLayout) :: buttons, bar
        type(QVBoxLayout) :: column
        type(QLabel) :: help
        character(len=110), allocatable :: items(:)
        integer :: k

        app%main = QMainWindow()
        call app%main%setWindowTitle('vkframes: Artemis I')
        call app%main%closeEvent%connect(on_close)
        call build_window()
        app%central = QWidget()
        column = QVBoxLayout(app%central)
        call column%addWidget(app%container, stretch=1)
        app%slider = QSlider(Qt_Horizontal)
        call app%slider%setRange(0, nint(app%span / 600.0_wp))
        call app%slider%setSingleStep(6)
        call app%slider%setPageStep(144)
        call app%slider%valueChanged%connect(on_slider)
        call app%slider%sliderPressed%connect(on_slider_pressed)
        call app%slider%sliderReleased%connect(on_slider_released)
        app%slider_time = QLabel('day 0.00')
        call app%slider_time%setMinimumWidth(80)
        bar = QHBoxLayout()
        call bar%addWidget(QLabel('Time'))
        call bar%addWidget(app%slider, stretch=1)
        call bar%addWidget(app%slider_time)
        call column%addLayout(bar)
        call app%main%setCentralWidget(app%central)

        panel = QWidget()
        form = QFormLayout(panel)
        app%frame_choice = QComboBox()
        call app%frame_choice%addItems(frame_names)
        call app%frame_choice%setCurrentIndex(app%start_frame - 1)
        call app%frame_choice%currentIndexChanged%connect(on_frame)
        call form%addRow('Frame', app%frame_choice)
        allocate(items(size(app%events) + 1))
        items(1) = 'Go to event...'
        do k = 1, size(app%events)
            items(k + 1) = utc_text(app%epoch + (app%events(k)%t - tdb_utc) / 86400.0_wp)//'  '//app%events(k)%text
        end do
        app%event_choice = QComboBox()
        call app%event_choice%addItems(items)
        call app%event_choice%currentIndexChanged%connect(on_event)
        call form%addRow('Events', app%event_choice)
        app%trails = QCheckBox('Trails (the last two days)')
        call app%trails%setChecked(.false.)
        call app%trails%toggled%connect(on_trails)
        call form%addRow('', app%trails)
        app%labels = QCheckBox('Event labels (the main events)')
        call app%labels%setChecked(.true.)
        call app%labels%toggled%connect(on_labels)
        call form%addRow('', app%labels)
        app%time_scale = QDoubleSpinBox()
        call app%time_scale%setRange(0.0_c_double, 1000000.0_c_double)
        call app%time_scale%setSingleStep(1000.0_c_double)
        call app%time_scale%setDecimals(0)
        call app%time_scale%setValue(10000.0_c_double)
        call app%time_scale%setSuffix(' x')
        call app%time_scale%valueChanged%connect(on_time_scale)
        call form%addRow('Time scale', app%time_scale)
        app%pause = QPushButton('Pause')
        call app%pause%clicked%connect(on_pause)
        reset_view = QPushButton('Reset view')
        call reset_view%clicked%connect(on_reset_view)
        buttons = QHBoxLayout()
        call buttons%addWidget(app%pause)
        call buttons%addWidget(reset_view)
        call form%addRow('', buttons)
        export = QPushButton('Export web page...')
        call export%clicked%connect(on_export)
        call form%addRow('', export)
        app%export_status = QLabel('')
        call app%export_status%setWordWrap(.true.)
        call form%addRow('', app%export_status)
        help = QLabel('<small>Orion''s flight from JPL Horizons, a point a minute: blue outbound, orange in ' // &
                      'the distant retrograde orbit, green on the way home. The events (burns with their ' // &
                      'delta-v, the 130 km lunar flybys, the farthest point) are found in the data. ' // &
                      'The trajectories are drawn in the frame chosen, each point where it was in that ' // &
                      'frame at its time; the camera rides with it. Left drag rotates, right drag zooms, ' // &
                      'middle drag pans; space resets the view.</small>')
        call help%setWordWrap(.true.)
        call form%addRow(help)
        call panel%setMinimumWidth(300)
        dock = QDockWidget('Artemis I', app%main)
        call dock%setWidget(panel)
        call app%main%addDockWidget(Qt_LeftDockWidgetArea, dock)
    end subroutine build_ui

end module artemis_demo

!> The Artemis I demo (see the module documentation).
program vkframes_artemis_demo

    use qt
    use artemis_demo
    implicit none

    type(QApplication) :: qapp
    character(len=1024) :: arg
    integer :: i, ierr

    qapp = QApplication()
    i = 1
    do while (i <= command_argument_count())
        if (app%options%take(i)) cycle
        call get_command_argument(i, arg)
        select case (arg)
        case ('--frame')
            call get_command_argument(i + 1, arg)
            read(arg, *) app%start_frame
            app%start_frame = max(1, min(n_frames, app%start_frame))
            i = i + 1
        case ('--export')
            call get_command_argument(i + 1, arg)
            app%export_file = trim(arg)
            i = i + 1
        end select
        i = i + 1
    end do

    call build_model()
    call build_ui()
    call app%options%start_quit_timer(app%main)
    call app%main%resize(1500, 900)
    call app%main%show()
    ierr = qapp%exec()

end program vkframes_artemis_demo
