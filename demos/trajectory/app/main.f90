!> vkframes in a qt-fortran GUI, drawing trajectory segments read from a JSON
!> file (default data/trajectory.json, found on VKFRAMES_DATA_PATH, which
!> demo.py sets), with an editor for each segment's style: each segment is a
!> Trajectory, drawn by a CurveArtist and a MarkerArtist in a
!> DrawableTrajectory of its own, with a small Sphere following it
!> (FOLLOW_LIMIT: it waits at the segment's start and stops at its end). The
!> central body, from the variables' frame (e.g. J2000-EARTH: the Earth,
!> textured), is a Sphere at the inertial root's center. The simulation time
!> runs over all the segments' times, shown by a time slider that can be
!> dragged; a legend (HUD text) names the segments in their colors.
!>
!> The dock's segment editor shows the style read from the file, for the
!> segment chosen; a change is drawn at once (the artists' and the body's
!> setters: no rebuild). File > Save and Save As write the file back with the
!> styles as edited: json-fortran keeps the whole file (json_file), and only
!> the style keys the editor sets are replaced (other keys, and the data, are
!> kept). Closing with unsaved changes asks to save them.
!>
!> The file holds an array of segments:
!>
!>     [ { "name": "Segment 1",                         (optional)
!>         "traj": [ {"var": "time", "units": "day", "data": [...]},
!>                   {"var": "Rx", "units": "km", "frame": "J2000-EARTH", "data": [...]},
!>                   {"var": "Ry", ...}, {"var": "Rz", ...} ],
!>         "style": { ... } },                           (optional)
!>       ... ]
!>
!> Time units: s, min, hour, day (or sec, minute, hr, d); position units: km,
!> m, au. Every `style` key is optional (the defaults in brackets):
!>
!>     "color":      [r, g, b], [r, g, b, a] (0 to 1), "#rrggbb", "#rrggbbaa" or a
!>                   name (white, red, ...) [a color from a palette, per segment]
!>     "width":      line width in points [2]
!>     "line_style": "solid", "dashed", "dotted" or "dash-dot" ["solid"]
!>     "visible":    shown at the start [true]
!>     "trace":      drawn up to the body only [false]
!>     "label":      the body's name label [true]
!>     "markers":    false (none), true (start and end) or an object [true]:
!>         "start", "end":  markers at the segment's ends [true]
!>         "interval":      also one every `interval` (the time's units) [none]
!>         "every":         or one every `every` data points [none]
!>         "shape":         disk, circle, square, plus, x, target, rose,
!>                          circle-pulse, disk-pulse or spiral ["disk"]
!>         "size":          in pixels [10]
!>         "color":         the intermediate markers [the line's color]
!>         "start_color", "end_color"  [green, red]
!>     "body":       false (none), true or an object [true]:
!>         "radius":        in the position's units [1% of the scene's size]
!>         "color":         [the line's color]
!>
!> Saved colors are written as "#rrggbb" (or "#rrggbbaa").
!>
!> Run with `pixi run -e qt trajectory-demo [-- file.json]` (from the vkframes
!> root). Options: `--capture file` saves frame `--capture-frame n` (default
!> 120) as an image; `--quit-after seconds` closes the demo; `--view n`
!> starts in view n (1: the whole scene, 2...: each segment's body). For
!> testing: `--test-edit` changes styles through the editor's widgets after
!> 1.5 s, `--save-as file` saves to `file` after 2.5 s.
module trajdemo

    use qt
    use vkframes
    use vkf_text, only: get_font
    use json_module, only: json_file, json_array, json_logical, json_string
    use iso_c_binding
    implicit none

    integer, parameter :: slider_steps = 1000                  !! the time slider's range
    real(wp), parameter :: palette(3, 8) = reshape([1.0_wp, 0.85_wp, 0.2_wp,  0.3_wp, 1.0_wp, 0.8_wp, &
                                                    1.0_wp, 0.4_wp, 0.4_wp,  0.5_wp, 0.7_wp, 1.0_wp, &
                                                    1.0_wp, 0.6_wp, 0.1_wp,  0.8_wp, 0.5_wp, 1.0_wp, &
                                                    0.6_wp, 1.0_wp, 0.3_wp,  1.0_wp, 0.5_wp, 0.8_wp], [3, 8])
    real(wp), parameter :: default_start_color(4) = [0.2_wp, 1.0_wp, 0.3_wp, 1.0_wp]
    real(wp), parameter :: default_end_color(4) = [1.0_wp, 0.25_wp, 0.2_wp, 1.0_wp]
    !> line styles (OpenGL stipples: 16 bits, bit 0 first, 2 window units each)
    character(len=8), parameter :: style_names(0:3) = [character(len=8) :: 'solid', 'dashed', 'dotted', 'dash-dot']
    integer, parameter :: style_bits(0:3) = [int(z'FFFF'), int(z'00FF'), int(z'3333'), int(z'18FF')]
    !> marker shapes, by MARKER_* constant
    character(len=12), parameter :: shape_names(0:9) = [character(len=12) :: 'disk', 'circle', 'square', 'plus', 'x', &
                                                        'target', 'rose', 'circle-pulse', 'disk-pulse', 'spiral']
    !> intermediate markers: none, every `interval` (time), every `every` points
    integer, parameter :: NO_INTERMEDIATE = 0, BY_INTERVAL = 1, BY_POINTS = 2
    !> the style keys the editor writes (replaced on saving; others are kept)
    character(len=10), parameter :: style_keys(8) = [character(len=10) :: 'color', 'width', 'line_style', 'visible', &
                                                     'trace', 'label', 'markers', 'body']

    !> One segment of the file, its style, and what draws it.
    type :: segment_t
        character(len=:), allocatable :: name
        integer :: index = 0                                   !! in the file's array
        logical :: valid = .false.                             !! it has times and positions
        type(Trajectory) :: traj
        type(DrawableTrajectory) :: frame                      !! draws the curve and markers (inertial)
        type(CurveArtist) :: curve
        type(MarkerArtist) :: markers
        type(Sphere) :: body                                   !! follows the trajectory
        type(HUDText) :: legend                                !! the name, in the segment's color
        character(len=:), allocatable :: frame_name            !! the positions' frame (e.g. J2000-EARTH)
        character(len=:), allocatable :: time_units, length_units   !! as in the file
        real(wp) :: time_factor = 1.0_wp                       !! s per time unit
        real(wp) :: length_factor = 1.0_wp                     !! km per length unit
        ! the style (see the module documentation)
        real(wp) :: color(4) = 1.0_wp
        real(wp) :: width = 2.0_wp
        integer :: line_style = 0                              !! index in style_names
        logical :: visible = .true., trace = .false., label = .true.
        logical :: markers_on = .true., marker_start = .true., marker_end = .true.
        integer :: intermediate = NO_INTERMEDIATE
        real(wp) :: interval = 0.0_wp                          !! in the file's time units
        integer :: every = 10
        integer :: shape = MARKER_DISK
        real(wp) :: marker_size = 10.0_wp
        real(wp) :: marker_color(4) = 1.0_wp, start_color(4) = 1.0_wp, end_color(4) = 1.0_wp
        logical :: body_on = .true.
        real(wp) :: radius = -1.0_wp                           !! km (< 0: the default, 1% of the scene's size)
        real(wp) :: body_color(4) = 1.0_wp
    end type segment_t

    !> The dock's segment editor: the style of segment `current`.
    type :: editor_t
        integer :: current = 0                                 !! in app%segments
        logical :: loading = .false.                           !! filling the widgets (not an edit)
        integer, allocatable :: segment_of(:)                  !! segment of each choice (0-based + 1)
        type(QComboBox) :: segment
        type(QPushButton) :: color, marker_color, start_color, end_color, body_color
        type(QDoubleSpinBox) :: width, interval, marker_size, radius
        type(QComboBox) :: line_style, intermediate, shape
        type(QCheckBox) :: visible, trace, label, marker_start, marker_end
        type(QSpinBox) :: every
        type(QGroupBox) :: markers, body
    end type editor_t

    !> The GUI and the scene.
    type :: app_t
        type(QMainWindow) :: main
        type(QStackedWidget) :: stack                          !! the center: a startup message, then the view
        type(QLabel) :: startup_message
        type(QWindow) :: window
        type(QWidget) :: container
        type(QtfEventFilter) :: events
        type(WindowProxy) :: proxy
        integer :: frame = 0
        type(QComboBox) :: view_choice
        type(QDoubleSpinBox) :: time_scale
        type(QCheckBox) :: pause_time, show_labels, show_hud
        type(QLabel) :: time_label
        type(QSlider) :: time_slider                           !! t0 to tf in slider_steps
        logical :: slider_held = .false.                       !! the user holds the slider's handle
        logical :: slider_setting = .false.                    !! the demo moves the slider (to the time)
        type(editor_t) :: editor
        type(HUDText) :: hud_view, hud_time
        ! the file (kept whole, to save it back) and the scene
        type(json_file) :: json
        logical :: modified = .false.                          !! styles changed since read or saved
        type(segment_t), allocatable :: segments(:)
        type(ReferenceFrame) :: root                           !! inertial, at the central body's center
        type(Sphere) :: central                                !! the central body (if known)
        logical :: has_central = .false.
        type(FrameManager) :: scene
        type(SkySphere) :: sky
        type(View), allocatable :: views(:)                    !! the whole scene, then each segment's body
        character(len=40), allocatable :: view_names(:)
        real(wp) :: t0 = 0.0_wp, tf = 1.0_wp                   !! all the segments' times (s)
        real(wp) :: extent = 1.0_wp                            !! the farthest position from the center (km)
        character(len=:), allocatable :: time_units            !! the first segment's, for the time shown
        real(wp) :: time_factor = 1.0_wp                       !! seconds per time unit
        ! options
        character(len=:), allocatable :: file, capture, save_as
        integer :: capture_frame = 120
        integer :: start_view = 1
        real(wp) :: quit_after = -1.0_wp
        logical :: test_edit = .false.
    end type app_t

    !> The application (module-level, so that it is a target: frames, artists
    !> and views keep pointers to each other).
    type(app_t), target, save :: app

contains

    ! ==================================================================
    ! reading the file

    !> Read the segments from `file` (json-fortran's json_file, by path:
    !> `[i].traj[k].data`), keeping the file's tree to save it back. Stops with
    !> a message if the file can't be read.
    subroutine read_segments(file)
        character(len=*), intent(in) :: file
        character(len=:), allocatable :: msg
        logical :: found, ok
        integer :: n, kind, i

        call app%json%initialize(compress_vectors=.true.)       ! (saved data arrays on one line)
        call app%json%load(filename=file)
        if (app%json%failed()) then
            call app%json%check_for_errors(ok, msg)
            write(*, '(a)') 'vkframes demo: could not read '//file//': '//msg
            error stop
        end if
        call app%json%info('', found, kind, n)
        if (kind /= json_array .or. n == 0) then
            write(*, '(a)') 'vkframes demo: '//file//' is not an array of segments'
            error stop
        end if
        allocate(app%segments(n))
        do i = 1, n
            call read_segment(app%json, i, app%segments(i))
        end do
        if (.not. any(app%segments%valid)) then
            write(*, '(a)') 'vkframes demo: no segment in '//file//' has times and positions'
            error stop
        end if
    end subroutine read_segments

    !> Segment i: its name, times and positions (in s and km), and its style.
    subroutine read_segment(json, i, s)
        type(json_file), intent(inout) :: json
        integer, intent(in) :: i
        type(segment_t), intent(inout) :: s
        character(len=:), allocatable :: seg, var, name, units
        real(wp), allocatable :: data(:), t(:), pos(:,:)
        logical :: found, have(0:3)
        integer :: k, n, nvar, kind, j

        s%index = i
        seg = '['//itoa(i)//']'
        s%name = get_string(json, seg//'.name', 'Segment '//itoa(i))
        call json%info(seg//'.traj', found, kind, nvar)
        if (.not. found .or. kind /= json_array) then
            write(*, '(a)') 'vkframes demo: '//s%name//' has no "traj" array: skipped'
            return
        end if

        ! the variables: time, Rx, Ry, Rz (others are ignored)
        have = .false.
        n = huge(n)
        s%time_units = 's'
        s%length_units = 'km'
        allocate(t(0), pos(3, 0))
        do k = 1, nvar
            var = seg//'.traj['//itoa(k)//']'
            name = lower(get_string(json, var//'.var', ''))
            units = lower(get_string(json, var//'.units', ''))
            select case (name)
            case ('time', 't')
                j = 0
            case ('rx', 'x')
                j = 1
            case ('ry', 'y')
                j = 2
            case ('rz', 'z')
                j = 3
            case default
                cycle
            end select
            call json%get(var//'.data', data, found)
            if (.not. found) then
                write(*, '(a)') 'vkframes demo: '//s%name//': no data for '//name
                cycle
            end if
            if (j == 0) then
                if (len(units) > 0) s%time_units = units
                s%time_factor = time_factor(units)
                t = data * s%time_factor
            else
                if (j == 1) then
                    if (len(units) > 0) s%length_units = units
                    s%length_factor = length_factor(units)
                end if
                if (.not. allocated(s%frame_name)) s%frame_name = get_string(json, var//'.frame', '')
                if (size(pos, 2) < size(data)) pos = reshape(pos, [3, size(data)], pad=[0.0_wp])
                pos(j, 1:size(data)) = data * length_factor(units)
            end if
            n = min(n, size(data))
            have(j) = .true.
        end do
        if (.not. all(have) .or. n == 0) then
            write(*, '(a)') 'vkframes demo: '//s%name//' needs time, Rx, Ry and Rz: skipped'
            return
        end if
        if (size(t) /= n .or. size(pos, 2) /= n) &
            write(*, '(a,i0,a)') 'vkframes demo: '//s%name//': variables of different lengths (using ', n, ' points)'
        s%traj = Trajectory()
        call s%traj%add(t(1:n), pos=pos(:, 1:n))
        s%valid = .true.
        if (.not. allocated(s%frame_name)) s%frame_name = ''
        if (.not. allocated(app%time_units)) then              ! (the time shown: in the first segment's units)
            app%time_units = s%time_units
            app%time_factor = s%time_factor
        end if

        call read_style(json, seg//'.style', i, s)
    end subroutine read_segment

    !> The segment's style (all optional): its line, markers and body.
    subroutine read_style(json, style, i, s)
        type(json_file), intent(inout) :: json
        character(len=*), intent(in) :: style                  !! its path
        integer, intent(in) :: i
        type(segment_t), intent(inout) :: s
        logical :: found
        integer :: kind

        s%color = get_color(json, style//'.color', [palette(:, modulo(i - 1, size(palette, 2)) + 1), 1.0_wp])
        s%width = get_real(json, style//'.width', 2.0_wp)
        s%line_style = name_index(get_string(json, style//'.line_style', 'solid'), style_names, 'line_style', s%name)
        s%visible = get_logical(json, style//'.visible', .true.)
        s%trace = get_logical(json, style//'.trace', .false.)
        s%label = get_logical(json, style//'.label', .true.)

        ! markers: false, true or an object
        call json%info(style//'.markers', found, kind)
        if (found .and. kind == json_logical) s%markers_on = get_logical(json, style//'.markers', .true.)
        s%marker_start = get_logical(json, style//'.markers.start', .true.)
        s%marker_end = get_logical(json, style//'.markers.end', .true.)
        s%interval = get_real(json, style//'.markers.interval', 0.0_wp)
        s%every = nint(get_real(json, style//'.markers.every', 0.0_wp))
        if (s%interval > 0.0_wp) then
            s%intermediate = BY_INTERVAL
        else if (s%every > 0) then
            s%intermediate = BY_POINTS
        end if
        if (s%every <= 0) s%every = 10                         ! (the editor's starting value)
        s%shape = name_index(get_string(json, style//'.markers.shape', 'disk'), shape_names, 'marker shape', s%name)
        s%marker_size = get_real(json, style//'.markers.size', 10.0_wp)
        s%marker_color = get_color(json, style//'.markers.color', s%color)
        s%start_color = get_color(json, style//'.markers.start_color', default_start_color)
        s%end_color = get_color(json, style//'.markers.end_color', default_end_color)

        ! the body: false, true or an object (its radius in the positions' units)
        call json%info(style//'.body', found, kind)
        if (found .and. kind == json_logical) s%body_on = get_logical(json, style//'.body', .true.)
        s%radius = get_real(json, style//'.body.radius', -1.0_wp)
        if (s%radius > 0.0_wp) s%radius = s%radius * s%length_factor
        s%body_color = get_color(json, style//'.body.color', s%color)
    end subroutine read_style

    function get_string(json, path, default) result(value)
        type(json_file), intent(inout) :: json
        character(len=*), intent(in) :: path, default
        character(len=:), allocatable :: value
        logical :: found
        call json%get(path, value, found)
        if (.not. found) value = default
    end function get_string

    real(wp) function get_real(json, path, default) result(value)
        type(json_file), intent(inout) :: json
        character(len=*), intent(in) :: path
        real(wp), intent(in) :: default
        logical :: found
        call json%get(path, value, found)
        if (.not. found) value = default
    end function get_real

    logical function get_logical(json, path, default) result(value)
        type(json_file), intent(inout) :: json
        character(len=*), intent(in) :: path
        logical, intent(in) :: default
        logical :: found
        call json%get(path, value, found)
        if (.not. found) value = default
    end function get_logical

    !> A color: [r, g, b(, a)], "#rrggbb(aa)" or a name; `default` if missing or not understood.
    function get_color(json, path, default) result(c)
        type(json_file), intent(inout) :: json
        character(len=*), intent(in) :: path
        real(wp), intent(in) :: default(4)
        real(wp) :: c(4)
        real(wp), allocatable :: v(:)
        character(len=:), allocatable :: s
        logical :: found
        integer :: kind
        c = default
        call json%info(path, found, kind)
        if (.not. found) return
        select case (kind)
        case (json_array)
            call json%get(path, v, found)
            if (found .and. (size(v) == 3 .or. size(v) == 4)) then
                c(1:size(v)) = max(0.0_wp, min(1.0_wp, v))
                if (size(v) == 3) c(4) = 1.0_wp
                return
            end if
        case (json_string)
            call json%get(path, s, found)
            if (parse_color(lower(s), c)) return
        end select
        c = default
        write(*, '(a)') 'vkframes demo: '//path//': not a color ([r, g, b], "#rrggbb" or a name)'
    end function get_color

    !> "#rrggbb", "#rrggbbaa" or a color name.
    logical function parse_color(s, c) result(ok)
        character(len=*), intent(in) :: s
        real(wp), intent(inout) :: c(4)
        integer :: k, v, ios
        ok = .true.
        if (len(s) > 0) then
            if (s(1:1) == '#' .and. (len(s) == 7 .or. len(s) == 9)) then
                c(4) = 1.0_wp
                do k = 1, (len(s) - 1) / 2
                    read(s(2*k:2*k+1), '(z2)', iostat=ios) v
                    if (ios /= 0) then
                        ok = .false.
                        return
                    end if
                    c(k) = real(v, wp) / 255.0_wp
                end do
                return
            end if
        end if
        select case (s)
        case ('white')
            c = [1.0_wp, 1.0_wp, 1.0_wp, 1.0_wp]
        case ('gray', 'grey')
            c = [0.6_wp, 0.6_wp, 0.6_wp, 1.0_wp]
        case ('red')
            c = [1.0_wp, 0.25_wp, 0.2_wp, 1.0_wp]
        case ('green')
            c = [0.2_wp, 1.0_wp, 0.3_wp, 1.0_wp]
        case ('blue')
            c = [0.35_wp, 0.55_wp, 1.0_wp, 1.0_wp]
        case ('yellow')
            c = [1.0_wp, 0.9_wp, 0.2_wp, 1.0_wp]
        case ('orange')
            c = [1.0_wp, 0.6_wp, 0.1_wp, 1.0_wp]
        case ('cyan')
            c = [0.2_wp, 1.0_wp, 1.0_wp, 1.0_wp]
        case ('magenta')
            c = [1.0_wp, 0.3_wp, 1.0_wp, 1.0_wp]
        case ('purple')
            c = [0.7_wp, 0.45_wp, 1.0_wp, 1.0_wp]
        case default
            ok = .false.
        end select
    end function parse_color

    !> "#rrggbb", or "#rrggbbaa" if not opaque.
    function color_hex(c) result(s)
        real(wp), intent(in) :: c(4)
        character(len=:), allocatable :: s
        character(len=9) :: buf
        integer :: n
        n = merge(3, 4, nint(255.0_wp * c(4)) == 255)
        write(buf, '(a,4z2.2)') '#', nint(255.0_wp * max(0.0_wp, min(1.0_wp, c(1:n))))
        s = trim(buf)
    end function color_hex

    !> The index (from 0) of `name` in `names` (0, with a message, if not there).
    integer function name_index(name, names, what, segment) result(k)
        character(len=*), intent(in) :: name, names(0:), what, segment
        do k = 0, ubound(names, 1)
            if (lower(name) == names(k)) return
            if (lower(name) == replace_char(names(k), '-', '_')) return
        end do
        k = 0
        write(*, '(a)') 'vkframes demo: '//segment//': unknown '//what//' "'//name//'" (using '//trim(names(0))//')'
    end function name_index

    pure function replace_char(s, a, b) result(r)
        character(len=*), intent(in) :: s
        character, intent(in) :: a, b
        character(len=len(s)) :: r
        integer :: k
        r = s
        do k = 1, len(s)
            if (r(k:k) == a) r(k:k) = b
        end do
    end function replace_char

    !> Seconds per time unit.
    real(wp) function time_factor(units)
        character(len=*), intent(in) :: units
        select case (units)
        case ('s', 'sec', 'secs', 'second', 'seconds', '')
            time_factor = 1.0_wp
        case ('min', 'mins', 'minute', 'minutes')
            time_factor = 60.0_wp
        case ('h', 'hr', 'hrs', 'hour', 'hours')
            time_factor = 3600.0_wp
        case ('d', 'day', 'days')
            time_factor = 86400.0_wp
        case default
            time_factor = 1.0_wp
            write(*, '(a)') 'vkframes demo: unknown time units "'//units//'" (taken as seconds)'
        end select
    end function time_factor

    !> km per length unit.
    real(wp) function length_factor(units)
        character(len=*), intent(in) :: units
        select case (units)
        case ('km', '')
            length_factor = 1.0_wp
        case ('m')
            length_factor = 1.0e-3_wp
        case ('au')
            length_factor = 1.495978707e8_wp
        case default
            length_factor = 1.0_wp
            write(*, '(a)') 'vkframes demo: unknown position units "'//units//'" (taken as km)'
        end select
    end function length_factor

    !> The central body's equatorial radius (km) for a frame name such as
    !> J2000-EARTH or EARTH (0 if unknown).
    real(wp) function body_radius(frame_name, name)
        character(len=*), intent(in) :: frame_name
        character(len=:), allocatable, intent(out) :: name
        integer :: k
        k = index(frame_name, '-', back=.true.)
        name = lower(frame_name(k+1:))
        select case (name)
        case ('earth')
            body_radius = 6378.137_wp
        case ('moon')
            body_radius = 1737.4_wp
        case ('mars')
            body_radius = 3396.19_wp
        case ('venus')
            body_radius = 6051.8_wp
        case ('mercury')
            body_radius = 2439.7_wp
        case ('jupiter')
            body_radius = 71492.0_wp
        case ('sun')
            body_radius = 695700.0_wp
        case default
            body_radius = 0.0_wp
        end select
        if (len(name) > 0) name = char(iachar(name(1:1)) - 32)//name(2:)       ! (Earth)
    end function body_radius

    pure function lower(s) result(l)
        character(len=*), intent(in) :: s
        character(len=len(s)) :: l
        integer :: k
        l = s
        do k = 1, len(s)
            if (s(k:k) >= 'A' .and. s(k:k) <= 'Z') l(k:k) = achar(iachar(s(k:k)) + 32)
        end do
    end function lower

    function itoa(i) result(s)
        integer, intent(in) :: i
        character(len=:), allocatable :: s
        character(len=12) :: buf
        write(buf, '(i0)') i
        s = trim(buf)
    end function itoa

    ! ==================================================================
    ! saving the file

    !> Write the file to `file`, with each segment's style as edited (the
    !> editor's keys replaced; other keys, and the data, as read).
    logical function save_file(file) result(ok)
        character(len=*), intent(in) :: file
        character(len=:), allocatable :: style, m, msg
        integer :: i, k

        do i = 1, size(app%segments)
            associate (s => app%segments(i))
            if (.not. s%valid) cycle
            style = '['//itoa(s%index)//'].style'
            do k = 1, size(style_keys)
                call app%json%remove(style//'.'//trim(style_keys(k)))
            end do
            call app%json%add(style//'.color', color_hex(s%color))
            call add_number(style//'.width', s%width)
            call app%json%add(style//'.line_style', trim(style_names(s%line_style)))
            call app%json%add(style//'.visible', s%visible)
            call app%json%add(style//'.trace', s%trace)
            call app%json%add(style//'.label', s%label)
            if (s%markers_on) then
                m = style//'.markers'
                call app%json%add(m//'.start', s%marker_start)
                call app%json%add(m//'.end', s%marker_end)
                if (s%intermediate == BY_INTERVAL) call add_number(m//'.interval', s%interval)
                if (s%intermediate == BY_POINTS) call app%json%add(m//'.every', s%every)
                call app%json%add(m//'.shape', trim(shape_names(s%shape)))
                call add_number(m//'.size', s%marker_size)
                call app%json%add(m//'.color', color_hex(s%marker_color))
                call app%json%add(m//'.start_color', color_hex(s%start_color))
                call app%json%add(m//'.end_color', color_hex(s%end_color))
            else
                call app%json%add(style//'.markers', .false.)
            end if
            if (s%body_on) then
                call add_number(style//'.body.radius', s%radius / s%length_factor)
                call app%json%add(style//'.body.color', color_hex(s%body_color))
            else
                call app%json%add(style//'.body', .false.)
            end if
            end associate
        end do
        if (.not. app%json%failed()) call app%json%print(file)
        ok = .not. app%json%failed()
        if (.not. ok) then
            call app%json%check_for_errors(ok, msg)
            call app%json%clear_exceptions()
            ok = .false.
            write(*, '(a)') 'vkframes demo: could not save '//file//': '//msg
            if (QMessageBox_warning('Save', 'Could not save '//file//': '//msg, parent=app%main) /= 0) continue
        end if
    end function save_file

    !> A number: an integer if it is one (2, not 0.2E+1).
    subroutine add_number(path, x)
        character(len=*), intent(in) :: path
        real(wp), intent(in) :: x
        if (x == aint(x) .and. abs(x) < 1.0e9_wp) then
            call app%json%add(path, nint(x))
        else
            call app%json%add(path, x)
        end if
    end subroutine add_number

    !> Styles changed (or saved: `modified` false): the window title shows it.
    subroutine set_modified(modified)
        logical, intent(in) :: modified
        app%modified = modified
        call app%main%setWindowTitle('vkframes: '//app%file//trim(merge(' *', '  ', modified)))
    end subroutine set_modified

    ! ==================================================================
    ! the scene

    !> The scene: the central body at the root's center, and each segment's
    !> curve, markers and body (all inertial); the views and the legend.
    subroutine build_model()
        character(len=:), allocatable :: body_name
        real(wp) :: r, t0, tf
        integer :: i, k, nv
        logical :: first

        call read_segments(app%file)

        ! the time range and the scene's size
        first = .true.
        app%extent = 0.0_wp
        do i = 1, size(app%segments)
            associate (s => app%segments(i))
            if (.not. s%valid) cycle
            if (s%traj%time_range(t0, tf)) then
                if (first) then
                    app%t0 = t0
                    app%tf = tf
                    first = .false.
                end if
                app%t0 = min(app%t0, t0)
                app%tf = max(app%tf, tf)
            end if
            do k = 1, s%traj%num_positions()
                app%extent = max(app%extent, norm2(s%traj%position(k)))
            end do
            end associate
        end do
        if (app%tf <= app%t0) app%tf = app%t0 + 1.0_wp
        if (app%extent <= 0.0_wp) app%extent = 1.0_wp

        ! the root (named for the frame) and the central body
        associate (s => app%segments(findloc(app%segments%valid, .true., 1)))
        if (len(s%frame_name) > 0) then
            app%root = ReferenceFrame(s%frame_name)
        else
            app%root = ReferenceFrame('Inertial')
        end if
        r = body_radius(s%frame_name, body_name)
        end associate
        call app%root%show_decorations(.false.)
        if (r > 0.0_wp) then
            app%has_central = .true.
            app%central = Sphere(body_name, radius=r, color=[0.5_wp, 0.5_wp, 0.55_wp])
            if (body_name == 'Earth') then
                call app%central%set_texture_map('2k_earth_daymap.jpg')     ! (data/, on VKFRAMES_DATA_PATH)
            end if
            if (.not. app%root%add_child(app%central)) write(*, '(a)') 'vkframes: add_child failed'
            app%extent = max(app%extent, r)
        end if

        ! the segments: their artists and bodies, styled as read
        nv = 1
        do i = 1, size(app%segments)
            associate (s => app%segments(i))
            if (.not. s%valid) cycle
            if (s%radius <= 0.0_wp) s%radius = round_2(0.01_wp * app%extent)
            s%curve = CurveArtist(s%traj, color=s%color, width=s%width)
            call s%curve%set_pattern(2, style_bits(s%line_style))
            s%markers = MarkerArtist(s%traj, size=s%marker_size)
            call s%markers%set_marker_shape(s%shape)
            call s%markers%set_marker_color(MARKER_INTERMEDIATE, s%marker_color)
            call s%markers%set_marker_color(MARKER_START, s%start_color)
            call s%markers%set_marker_color(MARKER_END, s%end_color)
            call apply_markers(s)
            call s%curve%set_trace_mode(s%trace)
            call s%markers%set_trace_mode(s%trace)
            s%frame = DrawableTrajectory(s%name)
            call s%frame%show_decorations(.false.)
            call s%frame%add_artist(s%curve)
            call s%frame%add_artist(s%markers)
            s%body = Sphere(s%name, radius=s%radius, color=s%body_color)
            call s%body%follow(s%traj, data=FOLLOW_POSITION, mode=FOLLOW_LIMIT)
            call s%body%show_axes(NO_AXES)
            call s%body%show_axes_labels(NO_AXES)
            if (.not. (app%root%add_child(s%frame) .and. app%root%add_child(s%body))) &
                write(*, '(a)') 'vkframes: add_child failed'
            s%legend = HUDText(s%name, position=[0.99_wp, 0.99_wp - 0.035_wp * real(nv - 1, wp)], &
                               alignment=ALIGN_RIGHT_TOP, size=16.0_wp, color=s%color(1:3))
            nv = nv + 1
            end associate
        end do
        call app%scene%set_frame(app%root)
        write(*, '(a)', advance='no') app%root%frame_string()

        ! the views: the whole scene, then each segment's body
        allocate(app%views(nv), app%view_names(nv))
        if (app%has_central) then
            app%views(1) = View(app%root, app%central, frame_type=ABSOLUTE_FRAME)
        else
            app%views(1) = View(app%root, app%root)
        end if
        call app%views(1)%set_default_view_distance(3.0_wp * app%extent)
        app%view_names(1) = 'All segments'
        k = 1
        do i = 1, size(app%segments)
            if (.not. app%segments(i)%valid) cycle
            k = k + 1
            app%views(k) = View(app%root, app%segments(i)%body, frame_type=ABSOLUTE_FRAME)
            call app%views(k)%set_default_view_distance(20.0_wp * app%segments(i)%radius)
            app%view_names(k) = app%segments(i)%name
        end do
        app%start_view = max(1, min(nv, app%start_view))
        app%hud_view = HUDText(trim(app%view_names(app%start_view)), position=[0.01_wp, 0.99_wp], &
                               alignment=ALIGN_LEFT_TOP, size=16.0_wp, color=[0.8_wp, 0.8_wp, 0.8_wp])
        app%hud_time = HUDText('', position=[0.01_wp, 0.01_wp], alignment=ALIGN_LEFT_BOTTOM, size=18.0_wp, &
                               color=[1.0_wp, 0.85_wp, 0.2_wp])

        if (.not. app%sky%set_star_data('Stars_HYGv3.txt', min_mag=-2.0_wp, max_mag=6.0_wp, max_stars=40000)) &
            write(*, '(a)') 'vkframes demo: no star catalog'
    end subroutine build_model

    !> x rounded to 2 significant digits (a default size, saved as typed).
    real(wp) function round_2(x)
        real(wp), intent(in) :: x
        real(wp) :: unit
        unit = 10.0_wp**(floor(log10(x)) - 1)
        round_2 = unit * anint(x / unit)
    end function round_2


    !> Which markers segment s shows, and where its intermediate ones are
    !> (at most 10000 of them, for a time interval).
    subroutine apply_markers(s)
        type(segment_t), intent(inout) :: s
        real(wp) :: t0, tf
        integer :: markers
        markers = 0
        if (s%markers_on) then
            if (s%marker_start) markers = ior(markers, MARKER_START)
            if (s%marker_end) markers = ior(markers, MARKER_END)
            select case (s%intermediate)
            case (BY_INTERVAL)
                if (s%traj%time_range(t0, tf) .and. s%interval * s%time_factor >= (tf - t0) / 10000.0_wp) then
                    markers = ior(markers, MARKER_INTERMEDIATE)
                    call s%markers%set_intermediate_type(INTERMEDIATE_TIME)
                    call s%markers%set_intermediate_spacing(s%interval * s%time_factor)
                end if
            case (BY_POINTS)
                markers = ior(markers, MARKER_INTERMEDIATE)
                call s%markers%set_intermediate_type(INTERMEDIATE_DATA)
                call s%markers%set_intermediate_spacing(real(max(1, s%every), wp))
            end select
        end if
        call s%markers%set_markers(markers)
    end subroutine apply_markers

    !> Show or hide segment s (its curve, markers, body and legend) as its style and the dock say.
    subroutine show_segment(s)
        type(segment_t), intent(inout) :: s
        call s%frame%show_contents(s%visible)
        call s%body%show_contents(s%visible .and. s%body_on)
        call s%body%show_name_label(s%visible .and. s%body_on .and. s%label .and. app%show_labels%isChecked())
        call s%legend%show(s%visible .and. app%show_hud%isChecked())
    end subroutine show_segment

    !> The time in seconds, and in the file's units if not seconds, for the HUD and the dock.
    function time_text(t) result(text)
        real(wp), intent(in) :: t
        character(len=:), allocatable :: text
        text = number_text(t)//' s'
        if (app%time_factor /= 1.0_wp) text = text//' ('//number_text(t / app%time_factor)//' '//app%time_units//')'
    end function time_text

    !> x with about 4 significant digits (fixed point between 0.001 and 1e6).
    function number_text(x) result(text)
        real(wp), intent(in) :: x
        character(len=:), allocatable :: text
        character(len=32) :: buf, fmt
        integer :: decimals
        if (x == 0.0_wp) then
            buf = '0'
        else if (abs(x) >= 1.0e-3_wp .and. abs(x) < 1.0e6_wp) then
            decimals = max(0, 3 - floor(log10(abs(x))))
            write(fmt, '(a,i0,a)') '(f0.', decimals, ')'
            write(buf, fmt) x
            if (buf(1:1) == '.') buf = '0'//buf
            if (buf(1:2) == '-.') buf = '-0'//buf(2:)
        else
            write(buf, '(es10.3)') x
        end if
        text = trim(adjustl(buf))
    end function number_text

    ! ==================================================================
    ! the window

    !> Open the view (after the main window is painted) and show the scene.
    subroutine open_view()
        integer :: k
        if (.not. app%proxy%open(app%window%width(), app%window%height(), 'vkframes', &
                                 native_window=app%window%winId(), pixel_ratio=app%window%devicePixelRatio())) then
            write(*, '(a)') 'vkframes: could not open the view'
            return
        end if
        call app%proxy%set_scene(app%scene)
        call app%proxy%set_background_color([0.0_wp, 0.0_wp, 0.0_wp])
        call app%proxy%set_sky(app%sky)
        do k = 1, size(app%views)
            call app%proxy%add_view(app%views(k))
        end do
        call app%proxy%select_view(app%start_view)
        call app%proxy%add_hud_text(app%hud_view)
        call app%proxy%add_hud_text(app%hud_time)
        do k = 1, size(app%segments)
            if (app%segments(k)%valid) call app%proxy%add_hud_text(app%segments(k)%legend)
        end do
        call app%proxy%set_time_range(app%t0, app%tf)
        call app%proxy%set_time_scale(app%time_scale%value())
        call app%proxy%set_time(app%t0)
    end subroutine open_view

    ! ==================================================================
    ! window events (GUI thread)

    subroutine on_expose(data)
        class(*), intent(inout), optional :: data
        if (app%window%isExposed() .and. app%proxy%is_open() .and. app%frame == 0) call app%window%requestUpdate()
    end subroutine on_expose

    !> Render a frame (time, frames, draw) and ask for the next one.
    subroutine on_update(data)
        class(*), intent(inout), optional :: data
        real(wp) :: t
        ! moved to a screen of another scale (or the display's scaling changed):
        ! Qt may send no resize, the window's size in window units being the same
        if (app%window%devicePixelRatio() /= app%proxy%pixel_ratio()) &
            call app%proxy%resize(nint(app%window%width() * app%window%devicePixelRatio()), &
                                  nint(app%window%height() * app%window%devicePixelRatio()), &
                                  pixel_ratio=app%window%devicePixelRatio())
        if (.not. app%proxy%frame()) return
        app%frame = app%frame + 1
        t = app%proxy%time()
        call app%time_label%setText('Time: '//time_text(t - app%t0))
        call app%hud_time%set_text('t = '//time_text(t - app%t0))
        if (.not. app%slider_held) then                    ! the slider follows the time (unless dragged)
            app%slider_setting = .true.
            call app%time_slider%setValue(nint(slider_steps * (t - app%t0) / (app%tf - app%t0)))
            app%slider_setting = .false.
        end if
        if (allocated(app%capture) .and. app%frame == app%capture_frame) then
            if (app%proxy%capture(app%capture)) write(*, '(a)') 'captured '//app%capture
        end if
        call app%window%requestUpdate()
    end subroutine on_update

    !> New size: rebuild the swapchain (framebuffer pixels).
    subroutine on_resize(event, data)
        type(QResizeEvent), intent(in) :: event
        class(*), intent(inout), optional :: data
        real(wp) :: dpr
        if (.not. app%proxy%is_open()) return
        dpr = app%window%devicePixelRatio()
        call app%proxy%resize(nint(event%width() * dpr), nint(event%height() * dpr), pixel_ratio=dpr)
        call app%window%requestUpdate()
    end subroutine on_resize

    !> VSG's mouse button number: 1 left, 2 middle, 3 right (0: other).
    integer function vsg_button(qt_button)
        integer, intent(in) :: qt_button
        select case (qt_button)
        case (Qt_LeftButton)
            vsg_button = 1
        case (Qt_MiddleButton)
            vsg_button = 2
        case (Qt_RightButton)
            vsg_button = 3
        case default
            vsg_button = 0
        end select
    end function vsg_button

    !> VSG's button mask (BUTTON_MASK_1/2/3 = 256/512/1024) for Qt's held buttons.
    integer function vsg_mask(qt_buttons)
        integer, intent(in) :: qt_buttons
        vsg_mask = 0
        if (iand(qt_buttons, Qt_LeftButton) /= 0) vsg_mask = ior(vsg_mask, 256)
        if (iand(qt_buttons, Qt_MiddleButton) /= 0) vsg_mask = ior(vsg_mask, 512)
        if (iand(qt_buttons, Qt_RightButton) /= 0) vsg_mask = ior(vsg_mask, 1024)
    end function vsg_mask

    !> Forward a mouse event (MOUSE_MOVE, MOUSE_PRESS or MOUSE_RELEASE), in framebuffer pixels.
    subroutine forward_mouse(kind, event)
        integer, intent(in) :: kind
        type(QMouseEvent), intent(in) :: event
        real(wp) :: dpr
        dpr = app%window%devicePixelRatio()
        call app%proxy%mouse(kind, nint(event%x() * dpr), nint(event%y() * dpr), vsg_mask(event%buttons()), &
                             vsg_button(event%button()))
        call event%accept()
    end subroutine forward_mouse

    subroutine on_move(event, data)
        type(QMouseEvent), intent(in) :: event
        class(*), intent(inout), optional :: data
        call forward_mouse(MOUSE_MOVE, event)
    end subroutine on_move

    subroutine on_press(event, data)
        type(QMouseEvent), intent(in) :: event
        class(*), intent(inout), optional :: data
        call forward_mouse(MOUSE_PRESS, event)
    end subroutine on_press

    subroutine on_release(event, data)
        type(QMouseEvent), intent(in) :: event
        class(*), intent(inout), optional :: data
        call forward_mouse(MOUSE_RELEASE, event)
    end subroutine on_release

    subroutine on_wheel(event, data)
        type(QWheelEvent), intent(in) :: event
        class(*), intent(inout), optional :: data
        if (event%angleDeltaY() == 0) return
        call app%proxy%scroll(merge(1.0_wp, -1.0_wp, event%angleDeltaY() > 0))
        call event%accept()
    end subroutine on_wheel

    !> VSG key symbol for a Qt key: ASCII for printable keys (lower case for the
    !> base symbol), X11-style codes for the others VSG's trackball uses; 0 if
    !> unmapped.
    subroutine vsg_key(event, symbol, modified_symbol, modifier)
        type(QKeyEvent), intent(in) :: event
        integer, intent(out) :: symbol, modified_symbol, modifier
        character(len=:), allocatable :: text
        integer :: key, mods
        key = event%key()
        mods = event%modifiers()
        modifier = 0
        if (iand(mods, Qt_ShiftModifier) /= 0) modifier = ior(modifier, 1)
        if (iand(mods, Qt_ControlModifier) /= 0) modifier = ior(modifier, 4)
        if (iand(mods, Qt_AltModifier) /= 0) modifier = ior(modifier, 8)
        if (iand(mods, Qt_MetaModifier) /= 0) modifier = ior(modifier, 128)
        select case (key)
        case (Qt_Key_Left)
            symbol = int(z'FF51')
        case (Qt_Key_Up)
            symbol = int(z'FF52')
        case (Qt_Key_Right)
            symbol = int(z'FF53')
        case (Qt_Key_Down)
            symbol = int(z'FF54')
        case (Qt_Key_Home)
            symbol = int(z'FF50')
        case (Qt_Key_Escape)
            symbol = int(z'FF1B')
        case (Qt_Key_Return)
            symbol = int(z'FF0D')
        case (Qt_Key_Space)
            symbol = 32
        case (Qt_Key_A:Qt_Key_Z)
            symbol = key - Qt_Key_A + iachar('a')
        case default
            symbol = 0
            text = event%text()
            if (len(text) == 1) then
                if (iachar(text) >= 32 .and. iachar(text) < 127) symbol = iachar(text)
            end if
        end select
        modified_symbol = symbol
        if (key >= Qt_Key_A .and. key <= Qt_Key_Z .and. iand(modifier, 1) /= 0) modified_symbol = key   ! upper case
    end subroutine vsg_key

    subroutine forward_key(pressed, event)
        logical, intent(in) :: pressed
        type(QKeyEvent), intent(in) :: event
        integer :: symbol, modified_symbol, modifier
        call vsg_key(event, symbol, modified_symbol, modifier)
        if (symbol == 0) return
        call app%proxy%key(pressed, symbol, modified_symbol, modifier)
        call event%accept()
    end subroutine forward_key

    subroutine on_key_press(event, data)
        type(QKeyEvent), intent(in) :: event
        class(*), intent(inout), optional :: data
        call forward_key(.true., event)
    end subroutine on_key_press

    subroutine on_key_release(event, data)
        type(QKeyEvent), intent(in) :: event
        class(*), intent(inout), optional :: data
        call forward_key(.false., event)
    end subroutine on_key_release

    ! ==================================================================
    ! controls (GUI thread)

    !> Show the chosen view (each view keeps its own camera).
    subroutine on_view(index, data)
        integer, intent(in) :: index
        class(*), intent(inout), optional :: data
        call app%proxy%select_view(index + 1)
        call app%hud_view%set_text(trim(app%view_names(index + 1)))
    end subroutine on_view

    subroutine on_reset_view(data)
        class(*), intent(inout), optional :: data
        type(View), pointer :: v
        v => app%proxy%current_view()
        call v%reset_view()
    end subroutine on_reset_view

    subroutine on_time_scale(value, data)
        real(c_double), intent(in) :: value
        class(*), intent(inout), optional :: data
        call app%proxy%set_time_scale(value)
    end subroutine on_time_scale

    subroutine on_pause_time(checked, data)
        logical, intent(in) :: checked
        class(*), intent(inout), optional :: data
        call app%proxy%pause_time(checked)
    end subroutine on_pause_time

    subroutine on_reset_time(data)
        class(*), intent(inout), optional :: data
        call app%proxy%set_time(app%t0)
    end subroutine on_reset_time

    !> The time slider moved (dragged, clicked or with the keys): set the time.
    !> (Not when the demo moves it to follow the time.)
    subroutine on_time_slider(value, data)
        integer, intent(in) :: value
        class(*), intent(inout), optional :: data
        if (app%slider_setting) return
        call app%proxy%set_time(app%t0 + (app%tf - app%t0) * real(value, wp) / slider_steps)
    end subroutine on_time_slider

    subroutine on_slider_pressed(data)
        class(*), intent(inout), optional :: data
        app%slider_held = .true.
    end subroutine on_slider_pressed

    subroutine on_slider_released(data)
        class(*), intent(inout), optional :: data
        app%slider_held = .false.
    end subroutine on_slider_released

    !> The bodies' name labels, or the HUD text (view, time, legend).
    subroutine on_show_labels(checked, data)
        logical, intent(in) :: checked
        class(*), intent(inout), optional :: data
        call refresh_segments()
    end subroutine on_show_labels

    subroutine on_show_hud(checked, data)
        logical, intent(in) :: checked
        class(*), intent(inout), optional :: data
        call app%hud_view%show(checked)
        call app%hud_time%show(checked)
        call refresh_segments()
    end subroutine on_show_hud

    subroutine refresh_segments()
        integer :: i
        do i = 1, size(app%segments)
            if (app%segments(i)%valid) call show_segment(app%segments(i))
        end do
    end subroutine refresh_segments

    ! ==================================================================
    ! the segment editor: each change is drawn at once, and marks the file modified

    !> The segment being edited.
    function edited() result(s)
        type(segment_t), pointer :: s
        s => app%segments(app%editor%current)
    end function edited

    !> Fill the editor's widgets with the style of the segment chosen.
    subroutine on_edit_segment(index, data)
        integer, intent(in) :: index
        class(*), intent(inout), optional :: data
        type(segment_t), pointer :: s
        type(editor_t), pointer :: e
        real(wp) :: t0, tf, step
        if (index < 0) return
        e => app%editor
        e%current = e%segment_of(index + 1)
        s => edited()
        e%loading = .true.
        call set_swatch(e%color, s%color)
        call e%width%setValue(real(s%width, c_double))
        call e%line_style%setCurrentIndex(s%line_style)
        call e%visible%setChecked(s%visible)
        call e%trace%setChecked(s%trace)
        call e%label%setChecked(s%label)
        call e%markers%setChecked(s%markers_on)
        call e%marker_start%setChecked(s%marker_start)
        call e%marker_end%setChecked(s%marker_end)
        call e%intermediate%setCurrentIndex(s%intermediate)
        ! (the interval in the segment's time units, stepping by about 1% of its span)
        call e%interval%setSuffix(' '//s%time_units)
        step = 1.0_wp
        if (s%traj%time_range(t0, tf)) then
            if (tf > t0) step = 10.0_wp**floor(log10((tf - t0) / s%time_factor / 100.0_wp))
        end if
        call e%interval%setSingleStep(real(step, c_double))
        call e%interval%setValue(real(s%interval, c_double))
        call e%every%setValue(s%every)
        call e%interval%setEnabled(s%intermediate == BY_INTERVAL)
        call e%every%setEnabled(s%intermediate == BY_POINTS)
        call e%shape%setCurrentIndex(s%shape)
        call e%marker_size%setValue(real(s%marker_size, c_double))
        call set_swatch(e%marker_color, s%marker_color)
        call set_swatch(e%start_color, s%start_color)
        call set_swatch(e%end_color, s%end_color)
        call e%body%setChecked(s%body_on)
        call e%radius%setSuffix(' '//s%length_units)
        call e%radius%setValue(real(s%radius / s%length_factor, c_double))
        call set_swatch(e%body_color, s%body_color)
        e%loading = .false.
    end subroutine on_edit_segment

    !> A color button: its color as background, and its hex code.
    subroutine set_swatch(button, c)
        type(QPushButton), intent(inout) :: button
        real(wp), intent(in) :: c(4)
        character(len=:), allocatable :: hex
        hex = color_hex(c)
        call button%setText(hex)
        call button%setStyleSheet('background-color: '//hex(1:7)//'; color: '// &
                                  trim(merge('black', 'white', dot_product(c(1:3), [0.3_wp, 0.59_wp, 0.11_wp]) > 0.5_wp))//';')
    end subroutine set_swatch

    !> Ask for a color (Qt's color dialog), starting at c; false if cancelled.
    logical function pick_color(c, title) result(picked)
        real(wp), intent(inout) :: c(4)
        character(len=*), intent(in) :: title
        type(QColor) :: initial, chosen
        initial%r = nint(255.0_wp * c(1))
        initial%g = nint(255.0_wp * c(2))
        initial%b = nint(255.0_wp * c(3))
        initial%a = nint(255.0_wp * c(4))
        chosen = QColorDialog_getColor(initial, title, app%main)
        picked = chosen%isValid()
        if (picked) c(1:3) = real([chosen%red(), chosen%green(), chosen%blue()], wp) / 255.0_wp
    end function pick_color

    !> An edit: the file now differs from what was read or saved.
    logical function editing()
        editing = .not. app%editor%loading .and. app%editor%current > 0
        if (editing) call set_modified(.true.)
    end function editing

    subroutine on_line_color(data)
        class(*), intent(inout), optional :: data
        type(segment_t), pointer :: s
        real(wp) :: c(4)
        if (app%editor%current == 0) return
        s => edited()
        c = s%color
        if (.not. pick_color(c, 'Line color: '//s%name)) return
        call set_line_color(c)
    end subroutine on_line_color

    !> The line's (and the legend's) color.
    subroutine set_line_color(c)
        real(wp), intent(in) :: c(4)
        type(segment_t), pointer :: s
        if (.not. editing()) return
        s => edited()
        s%color = c
        call s%curve%set_color(s%color)
        call s%legend%set_color(s%color(1:3))
        call set_swatch(app%editor%color, s%color)
    end subroutine set_line_color

    subroutine on_width(value, data)
        real(c_double), intent(in) :: value
        class(*), intent(inout), optional :: data
        type(segment_t), pointer :: s
        if (.not. editing()) return
        s => edited()
        s%width = value
        call s%curve%set_width(s%width)
    end subroutine on_width

    subroutine on_line_style(index, data)
        integer, intent(in) :: index
        class(*), intent(inout), optional :: data
        type(segment_t), pointer :: s
        if (.not. editing() .or. index < 0) return
        s => edited()
        s%line_style = index
        call s%curve%set_pattern(2, style_bits(s%line_style))
    end subroutine on_line_style

    subroutine on_visible(checked, data)
        logical, intent(in) :: checked
        class(*), intent(inout), optional :: data
        type(segment_t), pointer :: s
        if (.not. editing()) return
        s => edited()
        s%visible = checked
        call show_segment(s)
    end subroutine on_visible

    subroutine on_trace(checked, data)
        logical, intent(in) :: checked
        class(*), intent(inout), optional :: data
        type(segment_t), pointer :: s
        if (.not. editing()) return
        s => edited()
        s%trace = checked
        call s%curve%set_trace_mode(checked)
        call s%markers%set_trace_mode(checked)
    end subroutine on_trace

    subroutine on_label(checked, data)
        logical, intent(in) :: checked
        class(*), intent(inout), optional :: data
        type(segment_t), pointer :: s
        if (.not. editing()) return
        s => edited()
        s%label = checked
        call show_segment(s)
    end subroutine on_label

    subroutine on_markers(checked, data)
        logical, intent(in) :: checked
        class(*), intent(inout), optional :: data
        type(segment_t), pointer :: s
        if (.not. editing()) return
        s => edited()
        s%markers_on = checked
        call apply_markers(s)
    end subroutine on_markers

    subroutine on_marker_start(checked, data)
        logical, intent(in) :: checked
        class(*), intent(inout), optional :: data
        type(segment_t), pointer :: s
        if (.not. editing()) return
        s => edited()
        s%marker_start = checked
        call apply_markers(s)
    end subroutine on_marker_start

    subroutine on_marker_end(checked, data)
        logical, intent(in) :: checked
        class(*), intent(inout), optional :: data
        type(segment_t), pointer :: s
        if (.not. editing()) return
        s => edited()
        s%marker_end = checked
        call apply_markers(s)
    end subroutine on_marker_end

    subroutine on_intermediate(index, data)
        integer, intent(in) :: index
        class(*), intent(inout), optional :: data
        type(segment_t), pointer :: s
        call app%editor%interval%setEnabled(index == BY_INTERVAL)
        call app%editor%every%setEnabled(index == BY_POINTS)
        if (.not. editing() .or. index < 0) return
        s => edited()
        s%intermediate = index
        call apply_markers(s)
    end subroutine on_intermediate

    subroutine on_interval(value, data)
        real(c_double), intent(in) :: value
        class(*), intent(inout), optional :: data
        type(segment_t), pointer :: s
        if (.not. editing()) return
        s => edited()
        s%interval = value
        call apply_markers(s)
    end subroutine on_interval

    subroutine on_every(value, data)
        integer, intent(in) :: value
        class(*), intent(inout), optional :: data
        type(segment_t), pointer :: s
        if (.not. editing()) return
        s => edited()
        s%every = value
        call apply_markers(s)
    end subroutine on_every

    subroutine on_shape(index, data)
        integer, intent(in) :: index
        class(*), intent(inout), optional :: data
        type(segment_t), pointer :: s
        if (.not. editing() .or. index < 0) return
        s => edited()
        s%shape = index
        call s%markers%set_marker_shape(s%shape)
    end subroutine on_shape

    subroutine on_marker_size(value, data)
        real(c_double), intent(in) :: value
        class(*), intent(inout), optional :: data
        type(segment_t), pointer :: s
        if (.not. editing()) return
        s => edited()
        s%marker_size = value
        call s%markers%set_marker_size(s%marker_size)
    end subroutine on_marker_size

    subroutine on_marker_color(data)
        class(*), intent(inout), optional :: data
        type(segment_t), pointer :: s
        if (app%editor%current == 0) return
        s => edited()
        if (.not. pick_color(s%marker_color, 'Marker color: '//s%name)) return
        if (.not. editing()) return
        call s%markers%set_marker_color(MARKER_INTERMEDIATE, s%marker_color)
        call set_swatch(app%editor%marker_color, s%marker_color)
    end subroutine on_marker_color

    subroutine on_start_color(data)
        class(*), intent(inout), optional :: data
        type(segment_t), pointer :: s
        if (app%editor%current == 0) return
        s => edited()
        if (.not. pick_color(s%start_color, 'Start marker color: '//s%name)) return
        if (.not. editing()) return
        call s%markers%set_marker_color(MARKER_START, s%start_color)
        call set_swatch(app%editor%start_color, s%start_color)
    end subroutine on_start_color

    subroutine on_end_color(data)
        class(*), intent(inout), optional :: data
        type(segment_t), pointer :: s
        if (app%editor%current == 0) return
        s => edited()
        if (.not. pick_color(s%end_color, 'End marker color: '//s%name)) return
        if (.not. editing()) return
        call s%markers%set_marker_color(MARKER_END, s%end_color)
        call set_swatch(app%editor%end_color, s%end_color)
    end subroutine on_end_color

    subroutine on_body(checked, data)
        logical, intent(in) :: checked
        class(*), intent(inout), optional :: data
        type(segment_t), pointer :: s
        if (.not. editing()) return
        s => edited()
        s%body_on = checked
        call show_segment(s)
    end subroutine on_body

    !> The body's radius (the positions' units in the editor, km here).
    subroutine on_radius(value, data)
        real(c_double), intent(in) :: value
        class(*), intent(inout), optional :: data
        type(segment_t), pointer :: s
        if (.not. editing() .or. value <= 0.0_c_double) return
        s => edited()
        s%radius = value * s%length_factor
        call s%body%set_radius(s%radius)
    end subroutine on_radius

    subroutine on_body_color(data)
        class(*), intent(inout), optional :: data
        type(segment_t), pointer :: s
        real(wp) :: c(4)
        if (app%editor%current == 0) return
        s => edited()
        c = s%body_color
        if (.not. pick_color(c, 'Body color: '//s%name)) return
        call set_body_color(c)
    end subroutine on_body_color

    subroutine set_body_color(c)
        real(wp), intent(in) :: c(4)
        type(segment_t), pointer :: s
        if (.not. editing()) return
        s => edited()
        s%body_color = c
        call s%body%set_color(s%body_color)
        call set_swatch(app%editor%body_color, s%body_color)
    end subroutine set_body_color

    ! ==================================================================
    ! the file menu and lifetime

    subroutine on_save(data)
        class(*), intent(inout), optional :: data
        if (save_file(app%file)) call set_modified(.false.)
    end subroutine on_save

    !> Save to a file chosen (which becomes the file Save writes).
    subroutine on_save_as(data)
        class(*), intent(inout), optional :: data
        character(len=:), allocatable :: file
        file = QFileDialog_getSaveFileName('Save trajectory file as', app%file, 'JSON files (*.json);;All files (*)', &
                                           app%main)
        if (len(file) == 0) return
        call save_as(file)
    end subroutine on_save_as

    subroutine save_as(file)
        character(len=*), intent(in) :: file
        if (.not. save_file(file)) return
        app%file = file
        call set_modified(.false.)
        write(*, '(a)') 'saved '//file
    end subroutine save_as

    subroutine on_quit_action(data)
        class(*), intent(inout), optional :: data
        call app%main%close()
    end subroutine on_quit_action

    !> After the main window is first painted: the slow parts of the startup
    !> (reading the label font, creating the Vulkan device) while it shows a
    !> message, then the view in its place.
    subroutine on_startup(data)
        class(*), intent(inout), optional :: data
        type(c_ptr) :: font
        call show_startup_message('Loading fonts...')
        font = get_font('')                                  ! (read once, for all labels)
        call show_startup_message('Starting Vulkan...')
        call open_view()
        call app%stack%setCurrentIndex(1)
        if (app%proxy%is_open()) call app%window%requestUpdate()
    end subroutine on_startup

    !> The startup message, painted now (the GUI thread is busy until the view opens).
    subroutine show_startup_message(text)
        character(len=*), intent(in) :: text
        call app%startup_message%setText(text)
        call QApplication_processEvents()
    end subroutine show_startup_message

    !> --test-edit: change styles through the editor's widgets (their signals
    !> run the same slots as the user's edits; the colors as if picked).
    subroutine on_test_edit(data)
        class(*), intent(inout), optional :: data
        type(editor_t), pointer :: e
        e => app%editor
        call e%segment%setCurrentIndex(0)
        call e%width%setValue(5.0_c_double)
        call e%line_style%setCurrentIndex(2)                          ! dotted
        call set_line_color([0.2_wp, 0.6_wp, 1.0_wp, 1.0_wp])
        call e%intermediate%setCurrentIndex(BY_POINTS)
        call e%every%setValue(4)
        call e%shape%setCurrentIndex(MARKER_TARGET)
        call e%marker_size%setValue(18.0_c_double)
        call e%radius%setValue(e%radius%value() * 3.0_c_double)
        call set_body_color([1.0_wp, 0.2_wp, 0.8_wp, 1.0_wp])
        if (size(e%segment_of) > 1) then
            call e%segment%setCurrentIndex(1)
            call e%markers%setChecked(.false.)
            call e%trace%setChecked(.true.)
            call e%label%setChecked(.false.)
        end if
        write(*, '(a)') 'test edit done'
    end subroutine on_test_edit

    !> --save-as
    subroutine on_test_save(data)
        class(*), intent(inout), optional :: data
        call save_as(app%save_as)
    end subroutine on_test_save

    !> --quit-after
    subroutine on_quit(data)
        class(*), intent(inout), optional :: data
        call app%main%close()
    end subroutine on_quit

    !> The main window closes: with unsaved changes, save them, discard them or
    !> stay open; then close the view (waiting for the GPU) before Qt destroys
    !> its native window.
    subroutine on_close(event, data)
        type(QCloseEvent), intent(in) :: event
        class(*), intent(inout), optional :: data
        integer :: answer
        if (app%modified) then
            answer = QMessageBox_question('Unsaved changes', 'Save the style changes to '//app%file//'?', &
                                          ior(ior(QMessageBox_Save, QMessageBox_Discard), QMessageBox_Cancel), &
                                          QMessageBox_Save, app%main)
            if (answer == QMessageBox_Cancel) then
                call event%ignore()
                return
            end if
            if (answer == QMessageBox_Save) then
                if (.not. save_file(app%file)) then
                    call event%ignore()
                    return
                end if
            end if
        end if
        call app%proxy%close()
    end subroutine on_close

    ! ==================================================================
    ! construction

    !> The main window: the view in the center, a file menu, and controls in a
    !> dock (scrolling): the view, the time, the display, and the segment editor.
    subroutine build_ui()
        type(QDockWidget) :: dock
        type(QScrollArea) :: scroll
        type(QWidget) :: panel
        type(QVBoxLayout) :: column
        type(QGroupBox) :: group
        type(QFormLayout) :: form
        type(QPushButton) :: reset, reset_view
        type(QMenuBar) :: bar
        type(QMenu) :: menu
        type(QAction) :: action
        type(editor_t), pointer :: e
        integer :: i, n

        app%main = QMainWindow()
        call set_modified(.false.)
        call app%main%closeEvent%connect(on_close)

        ! File: Save, Save As, Quit
        bar = app%main%menuBar()
        menu = bar%addMenu('&File')
        action = QAction('&Save', app%main)
        call action%setShortcut('Ctrl+S')
        call action%triggered%connect(on_save)
        call menu%addAction(action)
        action = QAction('Save &As...', app%main)
        call action%setShortcut('Ctrl+Shift+S')
        call action%triggered%connect(on_save_as)
        call menu%addAction(action)
        call menu%addSeparator()
        action = QAction('&Quit', app%main)
        call action%setShortcut('Ctrl+Q')
        call action%triggered%connect(on_quit_action)
        call menu%addAction(action)

        ! the view's window (default surface type, as in the Qt demo)
        app%window = QWindow()
        app%container = QWidget_createWindowContainer(app%window)
        call app%container%setMinimumSize(320, 240)
        app%events = QtfEventFilter(app%window)
        call app%events%exposeEvent%connect(on_expose)
        call app%events%updateRequest%connect(on_update)
        call app%events%resizeEvent%connect(on_resize)
        call app%events%mousePressEvent%connect(on_press)
        call app%events%mouseReleaseEvent%connect(on_release)
        call app%events%mouseMoveEvent%connect(on_move)
        call app%events%wheelEvent%connect(on_wheel)
        call app%events%keyPressEvent%connect(on_key_press)
        call app%events%keyReleaseEvent%connect(on_key_release)
        app%stack = QStackedWidget()
        app%startup_message = QLabel('Starting...')
        call app%startup_message%setAlignment(Qt_AlignCenter)
        call app%startup_message%setStyleSheet('background-color: black; color: #cccccc; font-size: 16pt;')
        call app%stack%addWidget(app%startup_message)
        call app%stack%addWidget(app%container)
        call app%main%setCentralWidget(app%stack)

        panel = QWidget()
        column = QVBoxLayout(panel)

        ! the view
        group = QGroupBox('View')
        form = QFormLayout(group)
        app%view_choice = QComboBox()
        call app%view_choice%addItems(app%view_names)
        call app%view_choice%setCurrentIndex(app%start_view - 1)
        call app%view_choice%currentIndexChanged%connect(on_view)
        call form%addRow('View', app%view_choice)
        reset_view = QPushButton('Reset view')
        call reset_view%clicked%connect(on_reset_view)
        call form%addRow('', reset_view)
        app%show_labels = QCheckBox('Show labels')
        call app%show_labels%setChecked(.true.)
        call app%show_labels%toggled%connect(on_show_labels)
        call form%addRow('', app%show_labels)
        app%show_hud = QCheckBox('Show HUD')
        call app%show_hud%setChecked(.true.)
        call app%show_hud%toggled%connect(on_show_hud)
        call form%addRow('', app%show_hud)
        call column%addWidget(group)

        ! time: the whole range plays in about 30 s at the start
        group = QGroupBox('Time')
        form = QFormLayout(group)
        app%time_scale = QDoubleSpinBox()
        call app%time_scale%setRange(0.0_c_double, 1.0e9_c_double)
        call app%time_scale%setValue(real(max(1.0_wp, (app%tf - app%t0) / 30.0_wp), c_double))
        call app%time_scale%setSuffix(' x')
        call app%time_scale%valueChanged%connect(on_time_scale)
        call form%addRow('Time scale', app%time_scale)
        app%pause_time = QCheckBox('Pause time')
        call app%pause_time%toggled%connect(on_pause_time)
        call form%addRow('', app%pause_time)
        reset = QPushButton('Reset time')
        call reset%clicked%connect(on_reset_time)
        call form%addRow('', reset)
        app%time_label = QLabel('Time:')
        call form%addRow(app%time_label)
        app%time_slider = QSlider(Qt_Horizontal)
        call app%time_slider%setRange(0, slider_steps)
        call app%time_slider%setSingleStep(slider_steps / 100)
        call app%time_slider%setPageStep(slider_steps / 10)
        call app%time_slider%valueChanged%connect(on_time_slider)
        call app%time_slider%sliderPressed%connect(on_slider_pressed)
        call app%time_slider%sliderReleased%connect(on_slider_released)
        call form%addRow(app%time_slider)
        call column%addWidget(group)

        ! the segment editor: which segment, then its line, markers and body
        e => app%editor
        n = count(app%segments%valid)
        allocate(e%segment_of(n))
        e%segment_of = pack([(i, i = 1, size(app%segments))], app%segments%valid)
        group = QGroupBox('Segment')
        form = QFormLayout(group)
        e%segment = QComboBox()
        do i = 1, n
            call e%segment%addItem(app%segments(e%segment_of(i))%name)
        end do
        call form%addRow('Edit', e%segment)
        e%visible = QCheckBox('Visible')
        call e%visible%toggled%connect(on_visible)
        call form%addRow('', e%visible)
        e%color = QPushButton('')
        call e%color%clicked%connect(on_line_color)
        call form%addRow('Line color', e%color)
        e%width = QDoubleSpinBox()
        call e%width%setRange(0.5_c_double, 20.0_c_double)
        call e%width%setSingleStep(0.5_c_double)
        call e%width%setSuffix(' pt')
        call e%width%valueChanged%connect(on_width)
        call form%addRow('Line width', e%width)
        e%line_style = QComboBox()
        call e%line_style%addItems([character(len=8) :: 'Solid', 'Dashed', 'Dotted', 'Dash-dot'])
        call e%line_style%currentIndexChanged%connect(on_line_style)
        call form%addRow('Line style', e%line_style)
        e%trace = QCheckBox('Trace (up to the body)')
        call e%trace%toggled%connect(on_trace)
        call form%addRow('', e%trace)
        e%label = QCheckBox('Name label')
        call e%label%toggled%connect(on_label)
        call form%addRow('', e%label)
        call column%addWidget(group)

        e%markers = QGroupBox('Markers')
        call e%markers%setCheckable(.true.)
        call e%markers%toggled%connect(on_markers)
        form = QFormLayout(e%markers)
        e%marker_start = QCheckBox('At the start')
        call e%marker_start%toggled%connect(on_marker_start)
        call form%addRow('', e%marker_start)
        e%marker_end = QCheckBox('At the end')
        call e%marker_end%toggled%connect(on_marker_end)
        call form%addRow('', e%marker_end)
        e%intermediate = QComboBox()
        call e%intermediate%addItems([character(len=22) :: 'None', 'Every time interval', 'Every n points'])
        call e%intermediate%currentIndexChanged%connect(on_intermediate)
        call form%addRow('In between', e%intermediate)
        e%interval = QDoubleSpinBox()
        call e%interval%setDecimals(6)
        call e%interval%setRange(0.0_c_double, 1.0e12_c_double)
        call e%interval%valueChanged%connect(on_interval)
        call form%addRow('Interval', e%interval)
        e%every = QSpinBox()
        call e%every%setRange(1, 1000000)
        call e%every%setSuffix(' points')
        call e%every%valueChanged%connect(on_every)
        call form%addRow('Every', e%every)
        e%shape = QComboBox()
        call e%shape%addItems([character(len=14) :: 'Disk', 'Circle', 'Square', 'Plus', 'X', 'Target', 'Rose', &
                               'Pulsing circle', 'Pulsing disk', 'Spiral'])
        call e%shape%currentIndexChanged%connect(on_shape)
        call form%addRow('Shape', e%shape)
        e%marker_size = QDoubleSpinBox()
        call e%marker_size%setRange(1.0_c_double, 100.0_c_double)
        call e%marker_size%setSuffix(' px')
        call e%marker_size%valueChanged%connect(on_marker_size)
        call form%addRow('Size', e%marker_size)
        e%marker_color = QPushButton('')
        call e%marker_color%clicked%connect(on_marker_color)
        call form%addRow('Color', e%marker_color)
        e%start_color = QPushButton('')
        call e%start_color%clicked%connect(on_start_color)
        call form%addRow('Start color', e%start_color)
        e%end_color = QPushButton('')
        call e%end_color%clicked%connect(on_end_color)
        call form%addRow('End color', e%end_color)
        call column%addWidget(e%markers)

        e%body = QGroupBox('Body')
        call e%body%setCheckable(.true.)
        call e%body%toggled%connect(on_body)
        form = QFormLayout(e%body)
        e%radius = QDoubleSpinBox()
        call e%radius%setDecimals(3)
        call e%radius%setRange(1.0e-3_c_double, 1.0e12_c_double)
        call e%radius%valueChanged%connect(on_radius)
        call form%addRow('Radius', e%radius)
        e%body_color = QPushButton('')
        call e%body_color%clicked%connect(on_body_color)
        call form%addRow('Color', e%body_color)
        call column%addWidget(e%body)

        call column%addWidget(QLabel('<small>Left drag: rotate<br>Right drag: zoom<br>Middle drag: pan<br>' // &
                                     'Wheel: zoom</small>'))
        call column%addStretch()
        scroll = QScrollArea()
        call scroll%setWidgetResizable(.true.)
        call scroll%setWidget(panel)
        dock = QDockWidget('Controls', app%main)
        call dock%setWidget(scroll)
        call app%main%addDockWidget(Qt_LeftDockWidgetArea, dock)

        ! the first segment in the editor; the segments shown as their styles say
        call e%segment%currentIndexChanged%connect(on_edit_segment)
        call on_edit_segment(0)
        call refresh_segments()
    end subroutine build_ui

end module trajdemo

!> Trajectory segments from a JSON file, with a style editor, in a qt-fortran
!> GUI (see the module documentation).
program vkframes_trajectory_demo

    use qt
    use vkframes
    use trajdemo
    implicit none

    type(QApplication) :: qapp
    character(len=1024) :: arg
    integer :: i, ierr

    qapp = QApplication()

    app%file = ''
    i = 1
    do while (i <= command_argument_count())
        call get_command_argument(i, arg)
        select case (arg)
        case ('--capture')
            call get_command_argument(i + 1, arg)
            app%capture = trim(arg)
            i = i + 1
        case ('--capture-frame')
            call get_command_argument(i + 1, arg)
            read(arg, *) app%capture_frame
            i = i + 1
        case ('--view')
            call get_command_argument(i + 1, arg)
            read(arg, *) app%start_view
            i = i + 1
        case ('--quit-after')
            call get_command_argument(i + 1, arg)
            read(arg, *) app%quit_after
            i = i + 1
        case ('--test-edit')
            app%test_edit = .true.
        case ('--save-as')
            call get_command_argument(i + 1, arg)
            app%save_as = trim(arg)
            i = i + 1
        case default
            app%file = trim(arg)
        end select
        i = i + 1
    end do
    ! the file as given, or found by name on VKFRAMES_DATA_PATH (default data/trajectory.json)
    if (len(app%file) == 0) app%file = 'trajectory.json'
    if (len(find_data_file(app%file)) > 0) app%file = find_data_file(app%file)

    call build_model()
    call build_ui()
    if (app%test_edit) call QTimer_singleShot(1500, on_test_edit)
    if (allocated(app%save_as)) call QTimer_singleShot(2500, on_test_save)
    if (app%quit_after > 0) call QTimer_singleShot(int(1000 * app%quit_after), on_quit)
    call app%main%resize(1200, 800)
    call app%main%show()
    call QTimer_singleShot(0, on_startup)
    ierr = qapp%exec()

end program vkframes_trajectory_demo
