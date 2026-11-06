!> Orbits dragged from a palette onto the scene, in a qt-fortran GUI: the dock
!> lists orbit types (each with a picture of its shape about the Earth: a low
!> orbit, a sun-synchronous one, GPS, geostationary, a geostationary transfer
!> and Molniya); drag one onto the scene and it is made there.
!>
!> While an orbit type is dragged over the scene, a dashed preview of the
!> orbit follows the cursor; dropped, the orbit is made: its line, filled in
!> translucent (from the Earth's center: RibbonArtist), and a spacecraft
!> flying it, labeled. Where an orbit goes: its periapsis (for a circular
!> orbit, its starting point) where the cursor points, on the Earth if the
!> cursor is over it, else in the direction of the cursor's ray's closest
!> approach to the Earth, its plane as tilted as its type's inclination
!> (prograde); an inclined orbit only reaches latitudes up to its
!> inclination, so a point farther north or south is moved to that latitude.
!>
!> Drag and drop: the palette is a QListView of a QtfListModel whose rows can
!> be dragged (setDragEnabled); the scene's QWindow takes the drags through
!> its QtfEventFilter (dragEnterEvent, dragMoveEvent, dragLeaveEvent,
!> dropEvent), from the palette only (the model's isDragSource), the dragged
!> row in the drop's items. The window's position becomes a ray into the
!> scene with WindowProxy%cursor_ray (and ray_sphere on the Earth).
!>
!> Selecting and deleting: click an orbit (its line or its spacecraft) to
!> select it (WindowProxy%pick): it is highlighted (pulsing, arrows moving
!> along it); Delete (or Backspace) deletes it, Escape (or a click elsewhere)
!> deselects it. A drag still turns the camera: only a click (the mouse not
!> moved between press and release) selects.
!>
!> The dock also has the time scale, a check box for the orbits' planes (their
!> translucent fills), and buttons to delete the selected
!> orbit, the last one dropped, or all of them. Run with `pixi run -e qt palette-demo` (from the vkframes
!> root). Options: `--capture file` saves frame `--capture-frame n` (default
!> 120), `--quit-after seconds`, and, for tests, `--drop-test`: three orbit
!> types dragged and dropped with synthetic events (a preview shown on the
!> way), the orbits made printed; then the second orbit clicked (selected)
!> and the Delete key pressed, with synthetic mouse and key events, the
!> orbits left printed; then Escape (the view must go on), the planes hidden
!> and shown again, and the window closed.
module palette_demo

    use qt
    use vkframes
    use vkframes_qt, only: connect_view, ViewLink, DemoOptions
    use kepler
    use iso_c_binding

    implicit none

    integer, parameter :: max_orbits = 40                   !! the most orbits dropped (made once)
    integer, parameter :: n_points = 720                    !! points along an orbit
    integer, parameter :: icon_size = 72                    !! the palette's pictures (pixels)

    !> An orbit type of the palette.
    type :: OrbitType
        character(len=24) :: name = ''                      !! its name in the palette
        character(len=96) :: about = ''                     !! its tooltip
        real(wp) :: hp = 0.0_wp, ha = 0.0_wp                !! periapsis and apoapsis altitudes (km)
        real(wp) :: inc = 0.0_wp                            !! inclination (deg)
        real(wp) :: color(3) = 1.0_wp                       !! its line's color
    end type OrbitType

    integer, parameter :: n_types = 6
    type(OrbitType), parameter :: types(n_types) = [ &
        OrbitType('Low Earth orbit', 'Circular, 420 km, 51.6 deg (as the ISS): about 93 minutes', &
                  420.0_wp, 420.0_wp, 51.6_wp, [0.3_wp, 0.85_wp, 1.0_wp]), &
        OrbitType('Sun-synchronous', 'Circular, 700 km, 98.2 deg (retrograde, near-polar): about 99 minutes', &
                  700.0_wp, 700.0_wp, 98.2_wp, [0.45_wp, 1.0_wp, 0.5_wp]), &
        OrbitType('GPS (MEO)', 'Circular, 20,180 km, 55 deg: half a sidereal day', &
                  20180.0_wp, 20180.0_wp, 55.0_wp, [1.0_wp, 0.9_wp, 0.3_wp]), &
        OrbitType('Geostationary', 'Circular, 35,786 km, equatorial: one sidereal day', &
                  35786.0_wp, 35786.0_wp, 0.0_wp, [1.0_wp, 0.6_wp, 0.2_wp]), &
        OrbitType('Geostationary transfer', '250 by 35,786 km, 27 deg: about 10.5 hours', &
                  250.0_wp, 35786.0_wp, 27.0_wp, [1.0_wp, 0.4_wp, 0.9_wp]), &
        OrbitType('Molniya', '600 by 39,700 km, 63.4 deg: half a sidereal day', &
                  600.0_wp, 39700.0_wp, 63.4_wp, [1.0_wp, 0.35_wp, 0.3_wp])]

    !> An orbit dropped: its data, line, fill and spacecraft.
    type :: DroppedOrbit
        type(Trajectory) :: path
        type(CurveArtist) :: line
        type(RibbonArtist) :: fill
        type(DrawableTrajectory) :: frame
        type(Sphere) :: craft
        logical :: used = .false.                           !! in the scene
    end type DroppedOrbit

    !> Clicks select an orbit (picked), Delete deletes it.
    type, extends(WindowEventHandler) :: PaletteHandler
        real(wp) :: press(2) = 0.0_wp                       !! where the left button was pressed (-1 to 1, y up)
    contains
        procedure :: button_press => palette_button_press
        procedure :: button_release => palette_button_release
        procedure :: key_press => palette_key_press
    end type PaletteHandler

    !> The GUI and the scene.
    type :: app_t
        ! the window
        type(QMainWindow) :: main
        type(QWindow) :: window
        type(QWidget) :: container
        type(QtfEventFilter) :: events
        type(ViewLink) :: view_link                         !! the window's events to the proxy (connect_view)
        type(WindowProxy) :: proxy
        type(View) :: view
        integer :: frame = 0
        ! the palette and controls
        type(QListView) :: palette
        type(QtfListModel) :: palette_model
        type(QIcon) :: icons(n_types)
        type(QDoubleSpinBox) :: time_scale
        type(QCheckBox) :: show_planes                      !! the orbits' planes (their fills) shown
        type(QLabel) :: count_label
        type(HUDText) :: hud_help, hud_count
        ! the scene
        type(ReferenceFrame) :: root                        !! Earth-centered inertial (km)
        type(Sphere) :: earth
        type(DroppedOrbit), allocatable :: orbits(:)        !! (max_orbits): the slots, used or free
        integer :: order(max_orbits) = 0                    !! the slots in the scene, in the order dropped
        integer :: n_orbits = 0                             !! orbits in the scene (order(1:n_orbits))
        integer :: selected = 0                             !! the slot selected (0: none)
        type(Trajectory) :: preview                         !! the orbit dragged, where it would go
        type(CurveArtist) :: preview_line
        type(DrawableTrajectory) :: preview_frame
        type(SkySphere) :: sky
        type(FrameManager) :: scene
        ! options
        type(DemoOptions) :: options                        !! --capture, --capture-frame, --quit-after
        logical :: drop_test = .false.
    end type app_t

    type(app_t), target, save :: app
    type(PaletteHandler), target, save :: handler

contains

    ! ==================================================================
    ! the scene

    !> The scene: the Earth, the preview, the orbits to be dropped (made
    !> once, added to the scene as they are dropped), and the sky.
    subroutine build_model()
        integer :: k
        app%root = ReferenceFrame('ECI')
        call app%root%show_decorations(.false.)
        app%earth = Sphere('Earth', radius=earth_radius)
        call app%earth%set_texture_map('2k_earth_daymap.jpg')
        call app%earth%show_decorations(.false.)

        app%preview = Trajectory()
        call app%preview%reserve(n_points)
        app%preview_line = CurveArtist(app%preview, color=[1.0_wp, 1.0_wp, 1.0_wp, 0.8_wp], width=2.0_wp)
        call app%preview_line%set_pattern(4, int(z'00FF'))
        call app%preview_line%set_pickable(.false.)
        app%preview_frame = DrawableTrajectory('Preview')
        call app%preview_frame%show_decorations(.false.)
        call app%preview_frame%add_artist(app%preview_line)
        call app%preview_frame%show_contents(.false.)

        allocate(app%orbits(max_orbits))
        do k = 1, max_orbits
            associate (o => app%orbits(k))
                o%path = Trajectory()
                call o%path%reserve(n_points)
                o%line = CurveArtist(o%path, width=2.0_wp)
                o%fill = RibbonArtist(o%path)
                call o%fill%set_pickable(.false.)
                o%frame = DrawableTrajectory('Orbit')
                call o%frame%show_decorations(.false.)
                call o%frame%add_artist(o%fill)
                call o%frame%add_artist(o%line)
                o%craft = Sphere('Spacecraft', radius=250.0_wp)
                call o%craft%show_axes(NO_AXES)
                call o%craft%set_label_size(13)
                call o%craft%set_label_alignment(ALIGN_LEFT_CENTER)
            end associate
        end do

        if (.not. (app%root%add_child(app%earth) .and. app%root%add_child(app%preview_frame))) &
            error stop 'palette demo: could not build the scene'
        app%scene = FrameManager(app%root)

        app%sky = SkySphere()
        if (.not. app%sky%set_star_data('Stars_HYGv3.txt', min_mag=-2.0_wp, max_mag=6.0_wp, max_stars=20000)) &
            write(*, '(a)') 'palette demo: no star catalog'
    end subroutine build_model

    !> The orbit of type `kind` with its periapsis toward `anchor` (moved to
    !> the latitude the inclination reaches, if beyond it), one period of it
    !> into `path`.
    subroutine make_orbit(kind, anchor, path)
        integer, intent(in) :: kind                         !! the orbit type (1 to n_types)
        real(wp), intent(in) :: anchor(3)                   !! where the periapsis goes (any length but 0)
        type(Trajectory), intent(inout) :: path             !! the orbit (cleared first)
        real(wp) :: p(3), z(3), e1(3), e2(3), n(3), rp, ra, a, e, alpha, beta, inc, lat, lon, max_lat
        real(wp) :: r(3), v(3), t(n_points), pos(3, n_points)
        integer :: np
        logical :: cut0, cut1
        rp = earth_radius + types(kind)%hp
        ra = earth_radius + types(kind)%ha
        inc = types(kind)%inc * pi / 180.0_wp
        a = 0.5_wp * (rp + ra)
        e = (ra - rp) / (ra + rp)
        ! the periapsis: the anchor's longitude, its latitude within what the inclination reaches
        p = anchor / norm2(anchor)
        lat = asin(max(-1.0_wp, min(1.0_wp, p(3))))
        lon = atan2(p(2), p(1))
        max_lat = min(inc, pi - inc)
        lat = max(-max_lat, min(max_lat, lat))
        p = [cos(lat) * cos(lon), cos(lat) * sin(lon), sin(lat)]
        ! the orbit's normal: perpendicular to p, at the inclination to z (prograde: eastward at p)
        z = [0.0_wp, 0.0_wp, 1.0_wp]
        e1 = z - p(3) * p                                    ! (z's part perpendicular to p)
        if (norm2(e1) < 1.0e-9_wp) e1 = [1.0_wp, 0.0_wp, 0.0_wp]
        e1 = e1 / norm2(e1)
        e2 = cross(p, e1)                                    ! (east at p)
        alpha = max(-1.0_wp, min(1.0_wp, cos(inc) / max(norm2(z - p(3) * p), 1.0e-9_wp)))
        beta = sqrt(max(0.0_wp, 1.0_wp - alpha**2))
        n = alpha * e1 - beta * e2                          ! (n x p points east-ish: prograde)
        n = n / norm2(n)
        r = rp * p
        v = sqrt(mu * (1.0_wp + e) / rp) * cross(n, p)
        np = n_points
        call propagate(r, v, 0.0_wp, 2.0_wp * pi * sqrt(a**3 / mu), np, t, pos, cut0, cut1)
        call path%clear()
        call path%add(t(1:np), pos=pos(:, 1:np))
    end subroutine make_orbit

    !> Where the cursor at (x, y) points (window units from the top left): on
    !> the Earth if its ray meets it, else the ray's closest approach to the
    !> Earth's center. False before the view's first frame.
    logical function anchor_at(x, y, anchor) result(ok)
        real(c_double), intent(in) :: x, y                  !! the cursor (window units, from the top left)
        real(wp), intent(out) :: anchor(3)                  !! the point (root frame, km)
        real(wp) :: u, w, origin(3), dir(3), near, far, s
        anchor = [1.0_wp, 0.0_wp, 0.0_wp]
        u = 2.0_wp * real(x, wp) / max(1, app%window%width()) - 1.0_wp          ! (-1 to 1 across, y up)
        w = 1.0_wp - 2.0_wp * real(y, wp) / max(1, app%window%height())
        ok = app%proxy%cursor_ray(u, w, origin, dir)                            ! (false before the first frame)
        if (.not. ok) return
        if (ray_sphere(origin, dir, [0.0_wp, 0.0_wp, 0.0_wp], earth_radius, near, far)) then
            anchor = origin + near * dir
        else
            s = max(0.0_wp, -dot_product(origin, dir))
            anchor = origin + s * dir
            if (norm2(anchor) < 1.0e-6_wp) anchor = [1.0_wp, 0.0_wp, 0.0_wp]
        end if
    end function anchor_at

    !> Make an orbit of type `kind` toward `anchor`: added to the scene, with
    !> a spacecraft flying it.
    subroutine drop_orbit(kind, anchor)
        integer, intent(in) :: kind                         !! the orbit type (1 to n_types)
        real(wp), intent(in) :: anchor(3)                   !! where its periapsis goes
        character(len=16) :: number
        integer :: slot
        slot = findloc(app%orbits%used, .false., dim=1)
        if (slot == 0) then
            write(*, '(a,i0,a)') 'palette demo: at most ', max_orbits, ' orbits'
            return
        end if
        app%n_orbits = app%n_orbits + 1
        app%order(app%n_orbits) = slot
        associate (o => app%orbits(slot))
            o%used = .true.
            call make_orbit(kind, anchor, o%path)
            call o%line%set_color(types(kind)%color)
            call o%fill%set_color([types(kind)%color, 0.12_wp])
            write(number, '(i0)') slot
            call o%frame%set_name(trim(types(kind)%name))
            call o%craft%set_name('  '//trim(types(kind)%name)//' '//trim(number))
            call o%craft%set_color(types(kind)%color)
            call o%craft%follow(o%path)
            call show_plane(o, app%show_planes%isChecked())
            if (.not. (app%root%add_child(o%frame) .and. app%root%add_child(o%craft))) &
                write(*, '(a)') 'palette demo: could not add the orbit'
        end associate
        call update_count()
    end subroutine drop_orbit

    !> An orbit's plane (its fill) shown or not: its artist added to the
    !> orbit's frame or removed from it (not drawn at all).
    subroutine show_plane(o, shown)
        type(DroppedOrbit), intent(inout), target :: o      !! the orbit
        logical, intent(in) :: shown                        !! shown
        if (shown) then
            call o%frame%add_artist(o%fill)                 ! (nothing if already there)
        else
            call o%frame%remove_artist(o%fill)              ! (nothing if not there)
        end if
    end subroutine show_plane

    !> The number of orbits and the one selected, in the HUD and the dock.
    subroutine update_count()
        character(len=64) :: text
        character(len=:), allocatable :: selection
        write(text, '(i0,a)') app%n_orbits, merge(' orbit ', ' orbits', app%n_orbits == 1)
        selection = ''
        if (app%selected > 0) selection = ',  selected: '//trim(adjustl(app%orbits(app%selected)%craft%name()))// &
                                          ' (Delete deletes it, Escape deselects it)'
        call app%hud_count%set_text(trim(text)//selection)
        call app%count_label%setText(trim(text))
    end subroutine update_count

    !> Select orbit slot `slot` (0: none): highlighted, the one before not.
    subroutine select_orbit(slot)
        integer, value :: slot                              !! the slot (0: none; by value: app%selected may be passed)
        if (app%selected > 0) call app%orbits(app%selected)%line%set_highlight(HIGHLIGHT_NONE)
        app%selected = slot
        if (slot > 0) call app%orbits(slot)%line%set_highlight(HIGHLIGHT_PULSE + HIGHLIGHT_ARROWS, &
                                                               color=[1.0_wp, 1.0_wp, 1.0_wp])
        call update_count()
    end subroutine select_orbit

    !> Delete orbit slot `slot`: out of the scene, its slot free again.
    subroutine delete_orbit(slot)
        integer, value :: slot                              !! the slot (1 to max_orbits; by value: app%selected may be passed)
        integer :: k
        if (slot < 1 .or. slot > max_orbits) return
        if (.not. app%orbits(slot)%used) return
        if (app%selected == slot) call select_orbit(0)
        associate (o => app%orbits(slot))
            if (.not. (app%root%remove_child(o%frame) .and. app%root%remove_child(o%craft))) &
                write(*, '(a)') 'palette demo: could not delete the orbit'
            o%used = .false.
        end associate
        k = findloc(app%order(1:app%n_orbits), slot, dim=1)
        if (k > 0) then
            app%order(k:app%n_orbits - 1) = app%order(k + 1:app%n_orbits)
            app%n_orbits = app%n_orbits - 1
        end if
        call update_count()
    end subroutine delete_orbit

    ! ==================================================================
    ! selecting and deleting

    !> The left button pressed: where (a click selects on the release there).
    subroutine palette_button_press(self, window, x, y, button)
        class(PaletteHandler), intent(inout) :: self
        class(WindowProxy), intent(inout) :: window         !! the scene's window
        real(wp), intent(in) :: x, y                        !! -1 to 1 across the window, y up
        integer, intent(in) :: button                       !! MOUSE_LEFT, MOUSE_MIDDLE or MOUSE_RIGHT
        if (button == MOUSE_LEFT) self%press = [x, y]
    end subroutine palette_button_press

    !> The left button released where it was pressed (a click, not a drag of
    !> the camera): the orbit under it selected (its line or its spacecraft),
    !> or none.
    subroutine palette_button_release(self, window, x, y, button)
        class(PaletteHandler), intent(inout) :: self
        class(WindowProxy), intent(inout) :: window         !! the scene's window
        real(wp), intent(in) :: x, y                        !! -1 to 1 across the window, y up
        integer, intent(in) :: button                       !! MOUSE_LEFT, MOUSE_MIDDLE or MOUSE_RIGHT
        type(PickHit), allocatable :: hits(:)
        integer :: k, slot
        if (button /= MOUSE_LEFT .or. norm2([x, y] - self%press) > 0.01_wp) return
        call window%pick(x, y, hits, kinds=[PICK_CURVE, PICK_BODY], nearest=.true.)
        slot = 0
        if (size(hits) > 0) then
            do k = 1, max_orbits
                if (.not. app%orbits(k)%used) cycle
                if (associated(hits(1)%frame, app%orbits(k)%frame) .or. associated(hits(1)%frame, app%orbits(k)%craft)) &
                    slot = k
            end do
        end if
        call select_orbit(slot)
    end subroutine palette_button_release

    !> Delete (or Backspace): the selected orbit deleted; Escape: deselected.
    subroutine palette_key_press(self, window, key, modifiers, handled)
        class(PaletteHandler), intent(inout) :: self
        class(WindowProxy), intent(inout) :: window         !! the scene's window
        integer, intent(in) :: key, modifiers               !! a character code or KEY_*; MOD_* bits
        logical, intent(inout) :: handled                   !! (true: skip the built-in key)
        if (key == KEY_DELETE .or. key == KEY_BACKSPACE) then
            if (app%selected > 0) call delete_orbit(app%selected)
            handled = .true.
        else if (key == KEY_ESCAPE) then
            call select_orbit(0)
            handled = .true.
        end if
    end subroutine palette_key_press

    !> a x b.
    pure function cross(a, b) result(c)
        real(wp), intent(in) :: a(3)                        !! the first vector
        real(wp), intent(in) :: b(3)                        !! the second vector
        real(wp) :: c(3)
        c = [a(2) * b(3) - a(3) * b(2), a(3) * b(1) - a(1) * b(3), a(1) * b(2) - a(2) * b(1)]
    end function cross

    ! ==================================================================
    ! the palette

    !> The palette's pictures: each orbit's shape about the Earth (its plane
    !> face on, the periapsis at the left), the Earth to scale where it can be.
    subroutine paint_icons()
        type(QImage) :: image
        type(QPainter) :: painter
        type(QPixmap) :: pixmap
        real(c_double) :: x(181), y(181), scale, cx, cy, re, rp, ra, a, e, nu, r
        integer :: kind, k
        do kind = 1, n_types
            image = QImage(icon_size, icon_size)
            call image%fill(QColor(12, 16, 30))
            painter = QPainter(image)
            call painter%setRenderHint(QPainter_Antialiasing)
            rp = earth_radius + types(kind)%hp
            ra = earth_radius + types(kind)%ha
            a = 0.5_c_double * (rp + ra)
            e = (ra - rp) / (ra + rp)
            scale = 0.44_c_double * icon_size / a                ! (the orbit's width near the edges)
            cx = 0.5_c_double * icon_size - (a - rp) * scale     ! (the Earth at the focus, the orbit centered)
            cy = 0.5_c_double * icon_size
            re = max(3.0_c_double, earth_radius * scale)
            call painter%setPen(QPen(QColor(70, 120, 220), 1.0_c_double))
            call painter%setBrush(QBrush(QColor(40, 80, 170)))
            call painter%drawEllipse(cx - re, cy - re, 2.0_c_double * re, 2.0_c_double * re)
            do k = 1, 181
                nu = 2.0_c_double * pi * (k - 1) / 180.0_c_double
                r = a * (1.0_c_double - e**2) / (1.0_c_double + e * cos(nu))
                x(k) = cx - r * cos(nu) * scale                 ! (the periapsis at the left)
                y(k) = cy - r * sin(nu) * scale
            end do
            call painter%setPen(QPen(QColor(nint(255 * types(kind)%color(1)), nint(255 * types(kind)%color(2)), &
                                            nint(255 * types(kind)%color(3))), 2.0_c_double))
            call painter%drawPolyline(x, y)
            call painter%end()
            pixmap = QPixmap_fromImage(image)
            app%icons(kind) = QIcon(pixmap)
        end do
    end subroutine paint_icons

    !> A palette row: the type's name, picture and description.
    subroutine on_palette_data(row, column, request, data)
        integer, intent(in) :: row                          !! the row (0-based: the type, less one)
        integer, intent(in) :: column                       !! the column (0: the only one)
        type(QtfModelRequest), intent(inout) :: request     !! what is asked for, filled in here
        class(*), intent(inout), optional :: data           !! the connection's data (unused)
        if (row < 0 .or. row >= n_types) return
        call request%setText(trim(types(row + 1)%name))
        call request%setIcon(app%icons(row + 1))
        call request%setToolTip(trim(types(row + 1)%about)//' (drag it onto the scene)')
    end subroutine on_palette_data

    !> The orbit type dragged (from the palette), or 0.
    integer function dragged_type(event) result(kind)
        type(QDropEvent), intent(in) :: event               !! the drag or drop
        integer, allocatable :: rows(:)
        kind = 0
        if (.not. app%palette_model%isDragSource(event%mimeData())) return
        rows = event%items()
        if (size(rows) < 1) return
        if (rows(1) >= 0 .and. rows(1) < n_types) kind = rows(1) + 1
    end function dragged_type

    !> A drag entered the scene: taken if it is an orbit type from the palette.
    subroutine on_drag_enter(event, data)
        type(QDropEvent), intent(in) :: event               !! the drag
        class(*), intent(inout), optional :: data           !! the connection's data (unused)
        if (dragged_type(event) > 0) call event%acceptProposedAction()
    end subroutine on_drag_enter

    !> The drag moved over the scene: the preview of the orbit where it would go.
    subroutine on_drag_move(event, data)
        type(QDropEvent), intent(in) :: event               !! the drag
        class(*), intent(inout), optional :: data           !! the connection's data (unused)
        real(wp) :: anchor(3)
        integer :: kind
        kind = dragged_type(event)
        if (kind == 0) return
        call event%acceptProposedAction()
        if (.not. anchor_at(event%x(), event%y(), anchor)) return
        call make_orbit(kind, anchor, app%preview)
        call app%preview_line%set_color([types(kind)%color, 0.85_wp])
        call app%preview_frame%show_contents(.true.)
    end subroutine on_drag_move

    !> The drag left the scene: no preview.
    subroutine on_drag_leave(data)
        class(*), intent(inout), optional :: data           !! the connection's data (unused)
        call app%preview_frame%show_contents(.false.)
    end subroutine on_drag_leave

    !> An orbit type dropped on the scene: the orbit made there.
    subroutine on_drop(event, data)
        type(QDropEvent), intent(in) :: event               !! the drop
        class(*), intent(inout), optional :: data           !! the connection's data (unused)
        real(wp) :: anchor(3)
        integer :: kind
        call app%preview_frame%show_contents(.false.)
        kind = dragged_type(event)
        if (kind == 0) return
        if (.not. anchor_at(event%x(), event%y(), anchor)) return
        call event%acceptProposedAction()
        call drop_orbit(kind, anchor)
    end subroutine on_drop

    ! ==================================================================
    ! the window

    !> The view, the HUD texts, and the QWindow the scene is drawn in, with
    !> its events connected (and the drags and drops).
    subroutine build_window()
        app%view = View(app%root, app%root)
        call app%view%set_default_view_parameters(eye=[30000.0_wp, -110000.0_wp, 55000.0_wp], &
            center=[0.0_wp, 0.0_wp, 0.0_wp], up=[0.0_wp, 0.0_wp, 1.0_wp])
        app%hud_help = HUDText('Drag an orbit from the palette onto the scene', position=[0.01_wp, 0.99_wp], &
                               alignment=ALIGN_LEFT_TOP, size=17.0_wp, color=[0.95_wp, 0.95_wp, 0.95_wp])
        app%hud_count = HUDText('0 orbits', position=[0.01_wp, 0.01_wp], alignment=ALIGN_LEFT_BOTTOM, size=15.0_wp, &
                                color=[0.8_wp, 0.85_wp, 1.0_wp])
        app%window = QWindow()
        app%container = QWidget_createWindowContainer(app%window)
        call app%container%setMinimumSize(320, 240)
        app%events = QtfEventFilter(app%window)
        call app%events%exposeEvent%connect(on_expose)
        call app%events%updateRequest%connect(on_update)
        call app%events%dragEnterEvent%connect(on_drag_enter)
        call app%events%dragMoveEvent%connect(on_drag_move)
        call app%events%dragLeaveEvent%connect(on_drag_leave)
        call app%events%dropEvent%connect(on_drop)
        call connect_view(app%view_link, app%events, app%proxy, app%window)   ! (mouse, keys, resizes, the surface)
    end subroutine build_window

    !> Open the view (on the window's first expose) and show the scene.
    subroutine open_view()
        if (.not. app%proxy%open(app%window%width(), app%window%height(), 'vkframes palette', &
                                 native_window=app%window%winId(), pixel_ratio=app%window%devicePixelRatio())) then
            write(*, '(a)') 'palette demo: could not open the view'
            return
        end if
        if (app%proxy%num_views() > 0) return                    ! (opened again)
        call app%proxy%set_scene(app%scene)
        call app%proxy%set_background_color([0.0_wp, 0.0_wp, 0.0_wp])
        call app%proxy%set_sky(app%sky)
        call app%proxy%add_view(app%view)
        call app%proxy%set_event_handler(handler)
        call app%proxy%add_hud_text(app%hud_help)
        call app%proxy%add_hud_text(app%hud_count)
        call app%proxy%set_time_scale(app%time_scale%value())
    end subroutine open_view

    !> The window shown: the view opened (the first time), the frames started.
    subroutine on_expose(data)
        class(*), intent(inout), optional :: data           !! the connection's data (unused)
        if (app%proxy%is_open() .or. .not. app%window%isExposed()) return
        call open_view()
        if (app%proxy%is_open()) call app%window%requestUpdate()
    end subroutine on_expose

    !> Render a frame, ask for the next one.
    subroutine on_update(data)
        class(*), intent(inout), optional :: data           !! the connection's data (unused)
        if (app%window%devicePixelRatio() /= app%proxy%pixel_ratio()) &
            call app%proxy%resize(nint(app%window%width() * app%window%devicePixelRatio()), &
                                  nint(app%window%height() * app%window%devicePixelRatio()), &
                                  pixel_ratio=app%window%devicePixelRatio())
        if (.not. app%proxy%frame()) return
        app%frame = app%frame + 1
        if (app%drop_test) call drop_test_step()
        if (app%options%after_frame(app%proxy, app%window, app%frame)) return   ! (a capture; --recreate-surface)
        call app%window%requestUpdate()
    end subroutine on_update

    !> --drop-test: three orbit types dragged from the palette (their data as a
    !> drag from it carries) onto the scene with synthetic drag events: entered
    !> (frame 10), moved (a preview, 12), dropped (14) at points of the window;
    !> the orbits made printed (40).
    subroutine drop_test_step()
        integer, parameter :: kinds(3) = [4, 6, 1]          !! geostationary, Molniya, low
        real(c_double), parameter :: at(2, 3) = reshape([0.3_c_double, 0.5_c_double, 0.62_c_double, 0.35_c_double, &
                                                         0.52_c_double, 0.55_c_double], [2, 3])
        integer :: k, stage
        integer, save :: px, py
        real(wp) :: on(2)
        real(wp), allocatable :: pts(:)
        logical :: sent
        type(QMimeData) :: mime
        type(QDropEvent) :: ev
        real(c_double) :: x, y
        if (app%frame >= 10 .and. app%frame < 40) then
            k = (app%frame - 10) / 8 + 1                      ! (orbit k from frame 10 + 8 (k - 1))
            stage = mod(app%frame - 10, 8)
            if (k > 3 .or. stage > 4) return
            mime = app%palette_model%mimeData([kinds(k) - 1])
            x = at(1, k) * app%window%width()
            y = at(2, k) * app%window%height()
            select case (stage)
            case (0)
                ev = QDropEvent(QEvent_DragEnter, x, y, mime)
            case (2)
                ev = QDropEvent(QEvent_DragMove, x, y, mime)
            case (4)
                ev = QDropEvent(QEvent_Drop, x, y, mime)
            case default
                return
            end select
            sent = QCoreApplication_sendEvent(app%window, ev)
            if (stage == 2) write(*, '(a,a,a,l1)') 'drop test: ', trim(types(kinds(k))%name), &
                                                   ' dragged over the scene, previewed ', app%preview%num_times() > 0
            call ev%delete()
        else if (app%frame == 40) then
            write(*, '(a,i0,a)') 'drop test: ', app%n_orbits, ' orbits made'
            do k = 1, app%n_orbits
                write(*, '(a,i0,a,i0,a,f0.1,a)') 'drop test: orbit ', k, ', ', app%orbits(k)%path%num_times(), &
                    ' points, periapsis radius ', minval(radii(app%orbits(k)%path)), ' km'
            end do
        else if (app%frame == 42 .or. app%frame == 44) then
            ! a click on the second orbit's line (at its apoapsis: clear of the Earth)
            if (app%frame == 42) then
                pts = radii(app%orbits(2)%path)
                if (.not. app%proxy%screen_position(app%orbits(2)%path%point_at(app%orbits(2)%path%time( &
                        maxloc(pts, 1))), on)) error stop 'palette demo: the orbit is off the screen'
                px = nint(on(1) * app%window%width() * app%window%devicePixelRatio())
                py = nint((1.0_wp - on(2)) * app%window%height() * app%window%devicePixelRatio())
                call app%proxy%mouse(MOUSE_MOVE, px, py, 0, 0)
                call app%proxy%mouse(MOUSE_PRESS, px, py, 256, MOUSE_LEFT)
            else
                call app%proxy%mouse(MOUSE_RELEASE, px, py, 0, MOUSE_LEFT)
            end if
        else if (app%frame == 47) then
            write(*, '(a,i0,a)') 'drop test: clicked, slot ', app%selected, ' selected'
            call app%proxy%key(.true., KEY_DELETE, KEY_DELETE, 0)
            call app%proxy%key(.false., KEY_DELETE, KEY_DELETE, 0)
        else if (app%frame == 50) then
            write(*, '(a,i0,a,i0,a)') 'drop test: Delete pressed, ', app%n_orbits, ' orbits left, slot ', &
                                      app%selected, ' selected'
            call app%proxy%key(.true., KEY_ESCAPE, KEY_ESCAPE, 0)    ! (deselects; the view must go on)
            call app%proxy%key(.false., KEY_ESCAPE, KEY_ESCAPE, 0)
        else if (app%frame == 56) then
            write(*, '(a,l1)') 'drop test: Escape pressed, the view still running ', app%proxy%is_open()
            call app%show_planes%setChecked(.false.)
        else if (app%frame == 58) then
            write(*, '(a,i0,a)') 'drop test: planes hidden, ', app%orbits(app%order(1))%frame%num_artists(), &
                                 ' artist(s) per orbit'
            call app%show_planes%setChecked(.true.)
        else if (app%frame == 60) then
            write(*, '(a,i0,a)') 'drop test: planes shown, ', app%orbits(app%order(1))%frame%num_artists(), &
                                 ' artist(s) per orbit'
            call app%main%close()
        end if
    end subroutine drop_test_step

    !> The distances of a trajectory's points from the Earth's center (km).
    function radii(path) result(r)
        type(Trajectory), intent(in) :: path                !! the trajectory
        real(wp), allocatable :: r(:)
        real(wp), pointer :: pos(:,:)
        call path%positions(pos)
        allocate(r(size(pos, 2)))
        r = norm2(pos, dim=1)
    end function radii

    ! ==================================================================
    ! controls

    !> The time scale.
    subroutine on_time_scale(value, data)
        real(c_double), intent(in) :: value                 !! simulation seconds per second
        class(*), intent(inout), optional :: data           !! the connection's data (unused)
        if (app%proxy%is_open()) call app%proxy%set_time_scale(real(value, wp))
    end subroutine on_time_scale

    !> The last orbit dropped deleted.
    subroutine on_remove_last(data)
        class(*), intent(inout), optional :: data           !! the connection's data (unused)
        if (app%n_orbits > 0) call delete_orbit(app%order(app%n_orbits))
    end subroutine on_remove_last

    !> The orbits' planes shown or hidden.
    subroutine on_show_planes(checked, data)
        logical, intent(in) :: checked                      !! shown
        class(*), intent(inout), optional :: data           !! the connection's data (unused)
        integer :: k
        do k = 1, max_orbits
            call show_plane(app%orbits(k), checked)
        end do
    end subroutine on_show_planes

    !> The selected orbit deleted.
    subroutine on_delete_selected(data)
        class(*), intent(inout), optional :: data           !! the connection's data (unused)
        if (app%selected > 0) call delete_orbit(app%selected)
    end subroutine on_delete_selected

    !> All the orbits removed.
    subroutine on_clear(data)
        class(*), intent(inout), optional :: data           !! the connection's data (unused)
        do while (app%n_orbits > 0)
            call on_remove_last()
        end do
    end subroutine on_clear

    !> The view's camera reset.
    subroutine on_reset_view(data)
        class(*), intent(inout), optional :: data           !! the connection's data (unused)
        call app%view%reset_view()
    end subroutine on_reset_view

    !> The main window closes: close the view (waiting for the GPU) first.
    subroutine on_close(event, data)
        type(QCloseEvent), intent(in) :: event              !! the close (accepted)
        class(*), intent(inout), optional :: data           !! the connection's data (unused)
        call app%proxy%close()
    end subroutine on_close

    !> The main window: the scene in the middle, the dock (the palette, the
    !> time scale, the orbits' count and buttons) on the left.
    subroutine build_ui()
        type(QDockWidget) :: dock
        type(QWidget) :: panel
        type(QFormLayout) :: form
        type(QPushButton) :: delete_selected, remove_last, clear, reset_view
        type(QGridLayout) :: buttons
        type(QLabel) :: help

        app%main = QMainWindow()
        call app%main%setWindowTitle('vkframes: drag orbits from the palette onto the scene')
        call app%main%closeEvent%connect(on_close)
        call build_window()
        call app%main%setCentralWidget(app%container)

        panel = QWidget()
        form = QFormLayout(panel)

        ! the palette: orbit types, with pictures, dragged out
        call paint_icons()
        app%palette_model = QtfListModel(n_types)
        call app%palette_model%dataRequested%connect(on_palette_data)
        call app%palette_model%setDragEnabled(.true.)
        app%palette = QListView()
        call app%palette%setModel(app%palette_model)
        call app%palette%setIconSize(icon_size, icon_size)
        call app%palette%setDragEnabled(.true.)
        call app%palette%setDragDropMode(QAbstractItemView_DragOnly)
        call app%palette%setSpacing(4)
        call app%palette%setMinimumHeight((icon_size + 10) * n_types)
        call form%addRow(QLabel('<b>Orbits</b> (drag one onto the scene)'))
        call form%addRow(app%palette)

        app%time_scale = QDoubleSpinBox()
        call app%time_scale%setRange(0.0_c_double, 20000.0_c_double)
        call app%time_scale%setSingleStep(100.0_c_double)
        call app%time_scale%setDecimals(0)
        call app%time_scale%setValue(600.0_c_double)
        call app%time_scale%setSuffix(' x')
        call app%time_scale%valueChanged%connect(on_time_scale)
        call form%addRow('Time scale', app%time_scale)

        app%show_planes = QCheckBox('Orbit planes (filled in)')
        call app%show_planes%setChecked(.true.)
        call app%show_planes%toggled%connect(on_show_planes)
        call form%addRow('', app%show_planes)

        app%count_label = QLabel('0 orbits')
        call form%addRow('Dropped', app%count_label)
        delete_selected = QPushButton('Delete selected')
        call delete_selected%clicked%connect(on_delete_selected)
        remove_last = QPushButton('Remove last')
        call remove_last%clicked%connect(on_remove_last)
        clear = QPushButton('Clear')
        call clear%clicked%connect(on_clear)
        reset_view = QPushButton('Reset view')
        call reset_view%clicked%connect(on_reset_view)
        buttons = QGridLayout()                             ! (two by two: the dock can be narrow)
        call buttons%addWidget(delete_selected, 0, 0)
        call buttons%addWidget(remove_last, 0, 1)
        call buttons%addWidget(clear, 1, 0)
        call buttons%addWidget(reset_view, 1, 1)
        call form%addRow('', buttons)
        help = QLabel('<small>An orbit dropped has its periapsis (or, if circular, its start) ' // &
                      'where the cursor points: on the Earth, or toward the ray''s closest ' // &
                      'approach to it; the latitude only as far as its inclination reaches. ' // &
                      'Click an orbit to select it; Delete deletes it. ' // &
                      'Left drag rotates, right drag zooms, middle drag pans; space resets the ' // &
                      'view.</small>')
        call help%setWordWrap(.true.)
        call form%addRow(help)
        call panel%setMinimumWidth(240)                    ! (no maximum: the dock's edge drags it wider)
        dock = QDockWidget('Palette', app%main)
        call dock%setWidget(panel)
        call app%main%addDockWidget(Qt_LeftDockWidgetArea, dock)
    end subroutine build_ui

end module palette_demo

!> The orbit palette demo (see the module documentation).
program vkframes_palette_demo

    use qt
    use palette_demo
    implicit none

    type(QApplication) :: qapp
    character(len=1024) :: arg
    integer :: i, ierr

    qapp = QApplication()
    i = 1
    do while (i <= command_argument_count())
        if (app%options%take(i)) cycle                      ! (--capture, --quit-after, ...: vkframes_qt)
        call get_command_argument(i, arg)
        select case (arg)
        case ('--drop-test')
            app%drop_test = .true.
        end select
        i = i + 1
    end do

    call build_model()
    call build_ui()
    call app%options%start_quit_timer(app%main)
    call app%main%resize(1400, 860)
    call app%main%show()
    ierr = qapp%exec()

end program vkframes_palette_demo
