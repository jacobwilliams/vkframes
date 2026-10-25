!> Geometry generation for vkframes: meshes and lines computed in Fortran and
!> written into the rendering layer's buffers.
module vkf_geometry

    use, intrinsic :: iso_c_binding, only: c_ptr, c_int, c_double, c_float, c_int32_t, c_null_ptr, c_associated
    use vkf_layer, only: vkf_mesh_create, mesh_arrays

    implicit none

    private

    public :: ellipsoid_size, ellipsoid, ellipsoid_create, sphere_create

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
    !> multiplying the texture), with an optional equirectangular texture (an
    !> image handle). `n_lon` and `n_lat` default to 128 and 64 segments.
    !> Null on failure.
    function ellipsoid_create(radii, rgba, texture, n_lon, n_lat) result(mesh)
        real(c_double), dimension(3), intent(in) :: radii
        real(c_float), dimension(4), intent(in) :: rgba
        type(c_ptr), intent(in), optional :: texture
        integer, intent(in), optional :: n_lon, n_lat
        type(c_ptr) :: mesh
        real(c_float), dimension(:,:), pointer :: xyz, normals, uv
        integer(c_int32_t), dimension(:), pointer :: indices
        type(c_ptr) :: tex
        integer :: nx, ny, n_vertices, n_indices
        nx = 128
        ny = 64
        if (present(n_lon)) nx = n_lon
        if (present(n_lat)) ny = n_lat
        tex = c_null_ptr
        if (present(texture)) tex = texture
        call ellipsoid_size(nx, ny, n_vertices, n_indices)
        mesh = vkf_mesh_create(int(n_vertices, c_int), int(n_indices, c_int), rgba, tex)
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

end module vkf_geometry
