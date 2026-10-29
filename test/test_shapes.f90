!> Checks the reference shapes and lighting settings without a window: the
!> cones' visibility tests (polyhedral, elliptic, rectangular, distance
!> limits), make_cone_look_at, quat_from_matrix, the shapes' settings, bounds
!> and nodes (built and rebuilt, headless), and frame light sources and sphere
!> materials.
program test_shapes

    use, intrinsic :: iso_c_binding, only: c_null_ptr, c_associated
    use vkframes

    implicit none

    real(wp), parameter :: deg = pi / 180.0_wp
    integer :: failures

    failures = 0
    call check_quat_from_matrix()
    call check_cones()
    call check_look_at()
    call check_shapes()
    call check_polyhedra()
    call check_lights()

    if (failures > 0) then
        write(*, '(a,i0,a)') 'FAIL: ', failures, ' checks failed'
        error stop 1
    end if
    write(*, '(a)') 'PASS: shapes (axes, grid, plane, cones, polyhedra), lights, materials'

contains

    subroutine check(ok, what)
        logical, intent(in) :: ok
        character(len=*), intent(in) :: what
        if (.not. ok) then
            write(*, '(a)') '  failed: '//what
            failures = failures + 1
        end if
    end subroutine check

    subroutine check_quat_from_matrix()
        real(wp) :: q(4), r(4), err
        integer :: k
        err = 0.0_wp
        do k = 0, 35                                           ! rotations of every size, about changing axes
            q = quat_from_axis_angle([cos(0.3_wp * k), sin(0.7_wp * k), 0.5_wp], 10.0_wp * k * deg)
            r = quat_from_matrix(quat_to_matrix(q))
            err = max(err, min(maxval(abs(r - q)), maxval(abs(r + q))))      ! q and -q are the same rotation
        end do
        call check(err < 1.0e-12_wp, 'quat_from_matrix inverts quat_to_matrix')
    end subroutine check_quat_from_matrix

    !> A direction at cone angle `cone` and clock angle `clock` from the boresight (-z).
    pure function direction(cone, clock) result(p)
        real(wp), intent(in) :: cone, clock
        real(wp) :: p(3)
        p = [sin(cone) * cos(clock), sin(cone) * sin(clock), -cos(cone)]
    end function direction

    subroutine check_cones()
        type(PolyhedralCone) :: tri
        type(EllipticCone) :: ell
        type(RectangularCone) :: rect
        real(wp) :: a, b
        tri = PolyhedralCone('Triangle')                       ! vertices at clock 0, 120, 240 deg, cone 45 deg
        call check(tri%is_visible([0.0_wp, 0.0_wp, -1.0_wp]), 'polyhedral: on the boresight')
        call check(.not. tri%is_visible([0.0_wp, 0.0_wp, 1.0_wp]), 'polyhedral: behind the apex')
        call check(tri%is_visible(direction(40.0_wp * deg, 0.0_wp)), 'polyhedral: toward a vertex')
        call check(.not. tri%is_visible(direction(40.0_wp * deg, 60.0_wp * deg)), 'polyhedral: between vertices')
        call check(.not. tri%is_visible(2.0_wp * direction(10.0_wp * deg, 0.0_wp), max_distance=1.0_wp), &
                   'polyhedral: beyond max_distance')
        call check(.not. tri%is_visible(0.5_wp * direction(10.0_wp * deg, 0.0_wp), min_distance=1.0_wp), &
                   'polyhedral: before min_distance')

        ell = EllipticCone('Ellipse')                          ! a = 60 deg along x, b = 30 deg along y
        call ell%primary_angles(a, b)
        call check(abs(a - 60.0_wp * deg) < 1.0e-12_wp .and. abs(b - 30.0_wp * deg) < 1.0e-12_wp, 'elliptic: defaults')
        call check(ell%draw_mode() == ior(CONE_SIDES, CONE_BASE_OUTLINE), 'elliptic: default draw mode')
        call check(ell%is_visible(direction(55.0_wp * deg, 0.0_wp)), 'elliptic: inside along x')
        call check(.not. ell%is_visible(direction(35.0_wp * deg, 90.0_wp * deg)), 'elliptic: outside along y')
        call check(ell%is_visible(direction(25.0_wp * deg, 90.0_wp * deg)), 'elliptic: inside along y')

        rect = RectangularCone('Rectangle')                    ! 60 deg along x, 30 along y
        call check(rect%is_visible([tan(55.0_wp * deg), tan(25.0_wp * deg), -1.0_wp]), 'rectangular: inside')
        call check(.not. rect%is_visible([tan(55.0_wp * deg), tan(35.0_wp * deg), -1.0_wp]), 'rectangular: outside in y')
        call check(.not. rect%is_visible([tan(65.0_wp * deg), 0.0_wp, -1.0_wp]), 'rectangular: outside in x')
        call check(size(rect%clock_angles()) == 4, 'rectangular: 4 vertices')
    end subroutine check_cones

    subroutine check_look_at()
        type(PolyhedralCone) :: c
        real(wp) :: q(4), dir(3), up(3), y(3)
        dir = [1.0_wp, 2.0_wp, -0.5_wp]
        up = [0.0_wp, 0.0_wp, 1.0_wp]
        c = PolyhedralCone('Sensor')
        call c%make_cone_look_at([10.0_wp, 0.0_wp, 0.0_wp], dir, up)
        q = c%attitude()
        call check(all(c%position() == [10.0_wp, 0.0_wp, 0.0_wp]), 'look at: apex')
        call check(maxval(abs(quat_rotate(q, [0.0_wp, 0.0_wp, -1.0_wp]) - dir / norm2(dir))) < 1.0e-12_wp, &
                   'look at: boresight along dir')
        y = quat_rotate(q, [0.0_wp, 1.0_wp, 0.0_wp])
        call check(dot_product(y, up) > 0.0_wp .and. abs(dot_product(y, dir)) < 1.0e-12_wp, 'look at: +y toward up')
    end subroutine check_look_at

    subroutine check_shapes()
        type(CoordinateAxes), target :: axes
        type(LatLonGrid), target :: grid
        type(RadialPlane), target :: plane
        type(EllipticCone), target :: cone
        type(ReferenceFrame), target :: root
        type(Sphere), target :: later
        real(wp) :: c(3), r, rx, ry, rz, lat, lon, major, minor
        axes = CoordinateAxes('Axes', color=[1.0_wp, 1.0_wp, 0.0_wp])
        call axes%set_axis_length(4.0_wp)
        call axes%set_tick_spacing(1.0_wp, 0.5_wp)
        call axes%tick_spacing(major, minor)
        call check(axes%axis_length() == 4.0_wp .and. major == 1.0_wp .and. minor == 0.5_wp, 'axes: settings')
        call axes%bound(c, r)
        call check(r == 4.0_wp, 'axes: bound')
        call check(.not. axes%set_tick_image('does/not/exist.png') .and. axes%tick_shape() == MARKER_DISK, &
                   'axes: missing tick image')

        grid = LatLonGrid('Grid')
        call grid%set_parameters(6378.0_wp, 6378.0_wp, 6357.0_wp, 15.0_wp * deg, 20.0_wp * deg)
        call grid%parameters(rx, ry, rz, lat, lon)
        call check(rz == 6357.0_wp .and. abs(lat - 15.0_wp * deg) < 1.0e-15_wp, 'grid: parameters')
        call grid%bound(c, r)
        call check(r == 6378.0_wp, 'grid: bound')

        plane = RadialPlane('Plane')
        call plane%set_parameters(10.0_wp, 2.5_wp, 45.0_wp * deg)
        call plane%set_plane_color([0.0_wp, 0.0_wp, 1.0_wp, 0.3_wp])
        call check(all(plane%plane_color() == [0.0_wp, 0.0_wp, 1.0_wp, 0.3_wp]), 'plane: color')

        cone = EllipticCone('Cone', color=[0.0_wp, 1.0_wp, 0.0_wp, 0.4_wp])
        call cone%set_cone_length(5.0_wp)
        call check(all(cone%cone_color() == [0.0_wp, 1.0_wp, 0.0_wp, 0.4_wp]), 'cone: color from the frame')

        ! nodes: built (headless), then rebuilt after a change
        root = ReferenceFrame('Root')
        call check(root%add_child(axes) .and. root%add_child(grid) .and. root%add_child(plane) .and. &
                   root%add_child(cone), 'shapes in a tree')
        call root%realize(c_null_ptr)
        call axes%set_axis_length(5.0_wp)
        call grid%set_parameters(1.0_wp, 1.0_wp, 1.0_wp, 30.0_wp * deg, 30.0_wp * deg)
        call plane%set_parameters(3.0_wp, 1.0_wp, 30.0_wp * deg)
        call cone%set_primary_angles(20.0_wp * deg, 10.0_wp * deg)
        call check(cone%is_visible(direction(15.0_wp * deg, 0.0_wp)) .and. &
                   .not. cone%is_visible(direction(15.0_wp * deg, 90.0_wp * deg)), 'cone: new angles')
        call check(grid%frame_info() == 'LatLonGrid' .and. cone%frame_info() == 'EllipticCone', 'frame_info')

        ! a shown tree changing: a child added later is realized; axes moved
        later = Sphere('Later', radius=2.0_wp)
        call check(.not. c_associated(later%node()), 'a new frame has no node')
        call check(root%add_child(later), 'add_child to a shown tree')
        call check(c_associated(later%node()), 'a child added to a shown tree is realized')
        call later%move_x_axis([0.0_wp, 0.0_wp, 0.0_wp], 5.0_wp)
        call check(root%remove_child(later) .and. root%num_children() == 4, 'remove_child from a shown tree')
    end subroutine check_shapes

    !> Polyhedron: a cube's faces as polygons, a tetrahedron's as triangles,
    !> a tube as a grid (triangles, edges, bound).
    subroutine check_polyhedra()
        type(Polyhedron) :: cube, tet, tube
        real(wp) :: v(3, 8), c(3), r, grid(3, 6, 4)
        integer :: i, j
        v = reshape([0, 0, 0, 1, 0, 0, 1, 1, 0, 0, 1, 0, 0, 0, 1, 1, 0, 1, 1, 1, 1, 0, 1, 1] * 1.0_wp, [3, 8])
        cube = Polyhedron('Cube', color=[0.2_wp, 0.6_wp, 1.0_wp])
        call cube%set_faces(v, [1, 4, 3, 2, 5, 6, 7, 8, 1, 2, 6, 5, 2, 3, 7, 6, 3, 4, 8, 7, 4, 1, 5, 8], [4, 4, 4, 4, 4, 4])
        call check(cube%num_vertices() == 8 .and. cube%num_triangles() == 12 .and. cube%num_edges() == 12, &
                   'polyhedron: a cube (polygons: 12 triangles, 12 edges)')
        call cube%bound(c, r)
        call check(all(abs(c - 0.5_wp) < 1.0e-12_wp) .and. abs(r - sqrt(0.75_wp)) < 1.0e-12_wp, 'polyhedron: bound')
        call check(cube%draw_mode() == ior(POLY_FACES, POLY_EDGES) .and. cube%lit() .and. cube%two_sided() .and. &
                   .not. cube%smooth(), 'polyhedron: defaults')
        call cube%set_opacity(0.4_wp)
        call cube%set_vertex_colors(v)                          ! (rgb: the positions)
        call check(cube%has_vertex_colors() .and. abs(cube%opacity() - 0.4_wp) < 1.0e-12_wp, 'polyhedron: colors, opacity')
        call cube%set_faces(v, [1, 4, 3, 2], [4])
        call check(.not. cube%has_vertex_colors() .and. cube%num_triangles() == 2, 'polyhedron: faces again, colors dropped')

        tet = Polyhedron('Tetrahedron')
        call tet%set_triangles(v(:, [1, 2, 3, 5]), reshape([1, 3, 2, 1, 2, 4, 2, 3, 4, 3, 1, 4], [3, 4]))
        call check(tet%num_triangles() == 4 .and. tet%num_edges() == 6, 'polyhedron: a tetrahedron (triangles, 6 edges)')

        do j = 1, 4                                             ! 6 around, 4 along
            do i = 1, 6
                grid(:, i, j) = [cos(i * pi / 3.0_wp), sin(i * pi / 3.0_wp), real(j, wp)]
            end do
        end do
        tube = Polyhedron('Tube')
        call tube%set_grid(grid, closed_u=.true.)
        call check(tube%num_vertices() == 24 .and. tube%num_triangles() == 2 * 6 * 3 .and. &
                   tube%num_edges() == 6 * 3 + 4 * 6, 'polyhedron: a closed tube (grid)')
        call tube%set_grid(grid, closed_u=.true., lines=[3, 0])
        call check(tube%num_edges() == 2 * 3, 'polyhedron: a grid every 3rd line along it')
        call tube%set_grid(grid, lines=[0, 2])
        call check(tube%num_triangles() == 2 * 5 * 3 .and. tube%num_edges() == 3 * 5, 'polyhedron: an open grid')
    end subroutine check_polyhedra

    subroutine check_lights()
        type(ReferenceFrame), target :: root, sun
        type(Sphere), target :: earth
        real(wp) :: specular(4), shininess
        root = ReferenceFrame('Root')
        sun = ReferenceFrame('Sun')
        earth = Sphere('Earth')
        call check(root%add_child(earth) .and. root%add_child(sun), 'light tree')
        call check(.not. root%has_light_source(), 'no light source')
        call sun%set_light_source_enabled(.true.)
        call check(root%has_light_source(), 'a descendant has a light source')
        call check(all(sun%light_diffuse() == 0.8_wp) .and. all(sun%light_ambient() == 0.05_wp), 'light defaults')
        call sun%set_light_specular([1.0_wp, 0.9_wp, 0.8_wp])
        call check(all(sun%light_specular() == [1.0_wp, 0.9_wp, 0.8_wp]), 'light specular')
        call earth%set_material(specular=[0.5_wp, 0.5_wp, 0.5_wp], shininess=40.0_wp)
        call earth%material(specular=specular, shininess=shininess)
        call check(all(specular == [0.5_wp, 0.5_wp, 0.5_wp, 1.0_wp]) .and. shininess == 40.0_wp, 'sphere material')
        call check(earth%twilight() == 0.0_wp, 'twilight: none by default')
        call earth%set_twilight(0.2_wp)
        call check(earth%twilight() == 0.2_wp, 'twilight: set before shown')
        call root%realize(c_null_ptr)
        call earth%set_twilight(5.0_wp)                 ! (shown: its meshes' too)
        call check(earth%twilight() == 1.5_wp, 'twilight: at most 1.5 rad')
        call earth%set_twilight(-1.0_wp)
        call check(earth%twilight() == 0.0_wp, 'twilight: at least 0')
        call sun%set_light_source_enabled(.false.)
        call check(.not. root%has_light_source(), 'light source disabled')
    end subroutine check_lights

end program test_shapes
