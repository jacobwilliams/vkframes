!> Geometry generation for vkframes: meshes and lines computed in Fortran and
!> written into the rendering layer's buffers.
module vkf_geometry

    use, intrinsic :: iso_c_binding, only: c_ptr, c_int, c_double, c_float, c_int32_t, c_null_ptr, c_associated
    use vkf_layer, only: vkf_mesh_create, mesh_arrays

    implicit none

    private

    public :: ellipsoid_size, ellipsoid, ellipsoid_create, sphere_create
    public :: arrow_size, arrow, arrow_create

contains

    !> Number of vertices and triangle indices of `ellipsoid(radii, n_lon, n_lat, ...)`.
    pure subroutine ellipsoid_size(n_lon, n_lat, n_vertices, n_indices)
        integer, intent(in) :: n_lon  !! segments in longitude
        integer, intent(in) :: n_lat  !! segments in latitude
        integer, intent(out) :: n_vertices, n_indices
        n_vertices = (n_lon + 1) * (n_lat + 1)
        n_indices = 6 * n_lon * (n_lat - 1)
    end subroutine ellipsoid_size

    !> A triaxial ellipsoid with semi-axes `radii` (x, y, z) centered at the
    !> origin, for an equirectangular texture in geodetic coordinates.
    !>
    !> The vertex at geodetic latitude `lat` and longitude `lon` is the point
    !> whose surface normal is n = (cos lat cos lon, cos lat sin lon, sin lat):
    !> p = (a^2 nx, b^2 ny, c^2 nz) / sqrt(a^2 nx^2 + b^2 ny^2 + c^2 nz^2).
    !> So the normals are exact and texture rows are at geodetic latitude.
    !> (OpenFrames makes an ellipsoid by scaling a sphere, which skews the
    !> normals and maps the texture by parametric latitude.)
    !>
    !> Longitude 0 is on +x and the north pole on +z; texture u runs from
    !> longitude -180 (u = 0) to +180 (u = 1), v from the north pole (v = 0) to
    !> the south pole (v = 1). The seam column and pole rows are duplicated so
    !> the texture wraps; triangles are counter-clockwise seen from outside, and
    !> the degenerate triangles at the poles are left out. Indices are 0-based.
    !> Sizes are given by `ellipsoid_size`.
    pure subroutine ellipsoid(radii, n_lon, n_lat, xyz, normals, uv, indices)
        real(c_double), dimension(3), intent(in) :: radii
        integer, intent(in) :: n_lon  !! segments in longitude (>= 3)
        integer, intent(in) :: n_lat  !! segments in latitude (>= 2)
        real(c_float), dimension(:,:), intent(out) :: xyz      !! (3, n_vertices)
        real(c_float), dimension(:,:), intent(out) :: normals  !! (3, n_vertices)
        real(c_float), dimension(:,:), intent(out) :: uv       !! (2, n_vertices)
        integer(c_int32_t), dimension(:), intent(out) :: indices  !! (n_indices)

        real(c_double), parameter :: pi = acos(-1.0_c_double)
        real(c_double) :: lat, lon, n(3), r2(3), p(3)
        integer :: i, j, k, a, b, c, d

        r2 = radii**2
        do j = 0, n_lat
            lat = -0.5_c_double * pi + pi * real(j, c_double) / real(n_lat, c_double)
            do i = 0, n_lon
                lon = -pi + 2.0_c_double * pi * real(i, c_double) / real(n_lon, c_double)
                k = j * (n_lon + 1) + i + 1
                n = [cos(lat) * cos(lon), cos(lat) * sin(lon), sin(lat)]
                p = r2 * n
                xyz(:, k) = real(p / sqrt(dot_product(p, n)), c_float)
                normals(:, k) = real(n, c_float)
                uv(:, k) = real([real(i, c_double) / real(n_lon, c_double), &
                                 1.0_c_double - real(j, c_double) / real(n_lat, c_double)], c_float)
            end do
        end do

        ! quad (i, j): a = (i, j), b = (i+1, j), c = (i+1, j+1), d = (i, j+1);
        ! triangles (a, b, c) and (a, c, d), except (a, b, c) on the south row
        ! and (a, c, d) on the north row, which are degenerate
        k = 0
        do j = 0, n_lat - 1
            do i = 0, n_lon - 1
                a = j * (n_lon + 1) + i
                b = a + 1
                d = a + n_lon + 1
                c = d + 1
                if (j > 0) then
                    indices(k+1:k+3) = int([a, b, c], c_int32_t)
                    k = k + 3
                end if
                if (j < n_lat - 1) then
                    indices(k+1:k+3) = int([a, c, d], c_int32_t)
                    k = k + 3
                end if
            end do
        end do
    end subroutine ellipsoid

    !> An ellipsoid mesh node with semi-axes `radii` (x, y, z) and color (`rgba`,
    !> multiplying the texture), with an optional equirectangular texture and
    !> night texture (image handles; the night texture shows where the light
    !> is weak). `n_lon` and `n_lat` default to 128 and 64 segments. Null on
    !> failure.
    function ellipsoid_create(radii, rgba, texture, n_lon, n_lat, night_texture) result(mesh)
        real(c_double), dimension(3), intent(in) :: radii
        real(c_float), dimension(4), intent(in) :: rgba
        type(c_ptr), intent(in), optional :: texture, night_texture
        integer, intent(in), optional :: n_lon, n_lat
        type(c_ptr) :: mesh
        real(c_float), dimension(:,:), pointer :: xyz, normals, uv
        integer(c_int32_t), dimension(:), pointer :: indices
        type(c_ptr) :: tex, night
        integer :: nx, ny, n_vertices, n_indices
        nx = 128
        ny = 64
        if (present(n_lon)) nx = n_lon
        if (present(n_lat)) ny = n_lat
        tex = c_null_ptr
        if (present(texture)) tex = texture
        night = c_null_ptr
        if (present(night_texture)) night = night_texture
        call ellipsoid_size(nx, ny, n_vertices, n_indices)
        mesh = vkf_mesh_create(int(n_vertices, c_int), int(n_indices, c_int), rgba, tex, night, 0_c_int)
        if (.not. c_associated(mesh)) return
        call mesh_arrays(mesh, n_vertices, n_indices, xyz, normals, uv, indices)
        call ellipsoid(radii, nx, ny, xyz, normals, uv, indices)
    end function ellipsoid_create

    !> A sphere mesh node: `ellipsoid_create` with equal semi-axes.
    function sphere_create(radius, rgba, texture, n_lon, n_lat) result(mesh)
        real(c_double), intent(in) :: radius
        real(c_float), dimension(4), intent(in) :: rgba
        type(c_ptr), intent(in), optional :: texture
        integer, intent(in), optional :: n_lon, n_lat
        type(c_ptr) :: mesh
        mesh = ellipsoid_create([radius, radius, radius], rgba, texture, n_lon, n_lat)
    end function sphere_create

    !> Number of vertices and triangle indices of `arrow(..., n_segments, ...)`.
    pure subroutine arrow_size(n_segments, n_vertices, n_indices)
        integer, intent(in) :: n_segments
        integer, intent(out) :: n_vertices, n_indices
        n_vertices = 6 * (n_segments + 1) + 2
        n_indices = 15 * n_segments
    end subroutine arrow_size

    !> An arrow (OpenFrames' Vector: a cylinder body and a cone head) from
    !> `base` along `direction`: body of length `body_length` and radius
    !> `body_radius`, then a head of length `head_length` and base radius
    !> `head_radius`, closed at the bottom of the body and of the head, with
    !> `n_segments` around. Triangles are counter-clockwise seen from outside;
    !> indices are 0-based; uv is zero. Sizes are given by `arrow_size`.
    subroutine arrow(base, direction, body_length, head_length, body_radius, head_radius, n_segments, &
                          xyz, normals, uv, indices)
        real(c_double), intent(in) :: base(3), direction(3)
        real(c_double), intent(in) :: body_length, head_length, body_radius, head_radius
        integer, intent(in) :: n_segments
        real(c_float), dimension(:,:), intent(out) :: xyz, normals, uv
        integer(c_int32_t), dimension(:), intent(out) :: indices
        real(c_double), parameter :: pi = acos(-1.0_c_double)
        real(c_double) :: w(3), u(3), v(3), r(3), a, cone_normal(3)
        integer :: i, n, nv, ni, body0, cap0, head0, apex0, disk0

        n = n_segments
        w = direction / norm2(direction)
        u = cross(w, [1.0_c_double, 0.0_c_double, 0.0_c_double])
        if (norm2(u) < 1.0e-6_c_double) u = cross(w, [0.0_c_double, 1.0_c_double, 0.0_c_double])
        u = u / norm2(u)
        v = cross(w, u)
        uv = 0.0_c_float
        nv = 0
        ni = 0

        ! body side: rings at the base and the top of the body
        body0 = nv
        do i = 0, n
            r = radial(i)
            call put(base + body_radius * r, r)
            call put(base + body_radius * r + body_length * w, r)
        end do
        ! body bottom cap (facing -w): center, then a ring
        cap0 = nv
        call put(base, -w)
        do i = 0, n
            call put(base + body_radius * radial(i), -w)
        end do
        ! head side: ring at the head's base, and the apex for each segment
        head0 = nv
        do i = 0, n
            r = radial(i)
            cone_normal = r * head_length + w * head_radius
            call put(base + body_length * w + head_radius * r, cone_normal / norm2(cone_normal))
        end do
        apex0 = nv
        do i = 0, n
            a = 2.0_c_double * pi * (real(i, c_double) + 0.5_c_double) / real(n, c_double)
            r = cos(a) * u + sin(a) * v
            cone_normal = r * head_length + w * head_radius
            call put(base + (body_length + head_length) * w, cone_normal / norm2(cone_normal))
        end do
        ! head base disk (facing -w): center, then a ring
        disk0 = nv
        call put(base + body_length * w, -w)
        do i = 0, n
            call put(base + body_length * w + head_radius * radial(i), -w)
        end do

        do i = 0, n - 1
            ! body side: (a_i, a_i+1, b_i+1) and (a_i, b_i+1, b_i)
            call tri(body0 + 2*i, body0 + 2*(i+1), body0 + 2*(i+1) + 1)
            call tri(body0 + 2*i, body0 + 2*(i+1) + 1, body0 + 2*i + 1)
            ! caps facing -w: (center, ring_i+1, ring_i)
            call tri(cap0, cap0 + 1 + i + 1, cap0 + 1 + i)
            call tri(disk0, disk0 + 1 + i + 1, disk0 + 1 + i)
            ! head side: (ring_i, ring_i+1, apex_i)
            call tri(head0 + i, head0 + i + 1, apex0 + i)
        end do

    contains

        pure function radial(k) result(rad)
            integer, intent(in) :: k
            real(c_double) :: rad(3), ang
            ang = 2.0_c_double * pi * real(k, c_double) / real(n, c_double)
            rad = cos(ang) * u + sin(ang) * v
        end function radial

        subroutine put(p, nrm)
            real(c_double), intent(in) :: p(3), nrm(3)
            nv = nv + 1
            xyz(:, nv) = real(p, c_float)
            normals(:, nv) = real(nrm, c_float)
        end subroutine put

        subroutine tri(i1, i2, i3)
            integer, intent(in) :: i1, i2, i3
            indices(ni+1:ni+3) = int([i1, i2, i3], c_int32_t)
            ni = ni + 3
        end subroutine tri

    end subroutine arrow

    !> An arrow mesh node (see `arrow`) of color `rgba`, with 24 segments by
    !> default. Null on failure.
    function arrow_create(base, direction, body_length, head_length, body_radius, head_radius, rgba, n_segments) &
            result(mesh)
        real(c_double), intent(in) :: base(3), direction(3)
        real(c_double), intent(in) :: body_length, head_length, body_radius, head_radius
        real(c_float), dimension(4), intent(in) :: rgba
        integer, intent(in), optional :: n_segments
        type(c_ptr) :: mesh
        real(c_float), dimension(:,:), pointer :: xyz, normals, uv
        integer(c_int32_t), dimension(:), pointer :: indices
        integer :: n, n_vertices, n_indices
        n = 24
        if (present(n_segments)) n = max(3, n_segments)
        call arrow_size(n, n_vertices, n_indices)
        mesh = vkf_mesh_create(int(n_vertices, c_int), int(n_indices, c_int), rgba, c_null_ptr, c_null_ptr, 0_c_int)
        if (.not. c_associated(mesh)) return
        call mesh_arrays(mesh, n_vertices, n_indices, xyz, normals, uv, indices)
        call arrow(base, direction, body_length, head_length, body_radius, head_radius, n, xyz, normals, uv, indices)
    end function arrow_create

    pure function cross(a, b) result(c)
        real(c_double), intent(in) :: a(3), b(3)
        real(c_double) :: c(3)
        c = [a(2) * b(3) - a(3) * b(2), a(3) * b(1) - a(1) * b(3), a(1) * b(2) - a(2) * b(1)]
    end function cross

end module vkf_geometry
