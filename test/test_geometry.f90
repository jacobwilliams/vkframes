!> Checks the ellipsoid mesh (vkf_geometry), without a window:
!> vertices on the surface, normals along the surface gradient, texture rows at
!> geodetic latitude (WGS84, checked with the closed form), and triangles
!> counter-clockwise seen from outside.
program test_geometry

    use, intrinsic :: iso_c_binding, only: c_double, c_float, c_int32_t
    use vkf_geometry, only: ellipsoid_size, ellipsoid

    implicit none

    integer, parameter :: wp = c_double
    real(wp), parameter :: pi = acos(-1.0_wp)
    real(wp), parameter :: wgs84(3) = [6378137.0_wp, 6378137.0_wp, 6356752.314245_wp]
    real(wp), parameter :: triaxial(3) = [3.0_wp, 2.0_wp, 1.0_wp]
    integer :: failures

    failures = 0
    call check_ellipsoid('sphere', [1.0_wp, 1.0_wp, 1.0_wp], 24, 12)
    call check_ellipsoid('WGS84', wgs84, 128, 64)
    call check_ellipsoid('triaxial', triaxial, 36, 18)
    call check_geodetic_latitude(128, 64)

    if (failures > 0) then
        write(*, '(a,i0,a)') 'FAIL: ', failures, ' checks failed'
        error stop 1
    end if
    write(*, '(a)') 'PASS: ellipsoid geometry'

contains

    subroutine expect(ok, what)
        logical, intent(in) :: ok
        character(len=*), intent(in) :: what
        if (.not. ok) then
            write(*, '(a)') '  failed: '//what
            failures = failures + 1
        end if
    end subroutine expect

    subroutine make(radii, n_lon, n_lat, xyz, normals, uv, indices)
        real(wp), intent(in) :: radii(3)
        integer, intent(in) :: n_lon, n_lat
        real(c_float), allocatable, intent(out) :: xyz(:,:), normals(:,:), uv(:,:)
        integer(c_int32_t), allocatable, intent(out) :: indices(:)
        integer :: n_vertices, n_indices
        call ellipsoid_size(n_lon, n_lat, n_vertices, n_indices)
        allocate(xyz(3, n_vertices), normals(3, n_vertices), uv(2, n_vertices), indices(n_indices))
        call ellipsoid(radii, n_lon, n_lat, xyz, normals, uv, indices)
    end subroutine make

    subroutine check_ellipsoid(name, radii, n_lon, n_lat)
        character(len=*), intent(in) :: name
        real(wp), intent(in) :: radii(3)
        integer, intent(in) :: n_lon, n_lat
        real(c_float), allocatable :: xyz(:,:), normals(:,:), uv(:,:)
        integer(c_int32_t), allocatable :: indices(:)
        real(wp) :: p(3), n(3), g(3), e1(3), e2(3), surface_err, normal_err, winding
        integer :: k, t, ia, ib, ic

        call make(radii, n_lon, n_lat, xyz, normals, uv, indices)
        write(*, '(a,i0,a,i0,a)') name//': ', size(xyz, 2), ' vertices, ', size(indices) / 3, ' triangles'

        surface_err = 0.0_wp
        normal_err = 0.0_wp
        do k = 1, size(xyz, 2)
            p = real(xyz(:, k), wp)
            n = real(normals(:, k), wp)
            surface_err = max(surface_err, abs(sum((p / radii)**2) - 1.0_wp))
            g = p / radii**2                           ! surface gradient
            if (norm2(g) > 0.0_wp) normal_err = max(normal_err, norm2(cross(n, g / norm2(g))))
            call expect(abs(norm2(n) - 1.0_wp) < 1.0e-6_wp, name//': unit normal')
        end do
        write(*, '(a,es10.2,a,es10.2)') '  max |surface eq - 1| =', surface_err, ', max normal angle =', normal_err
        call expect(surface_err < 1.0e-6_wp, name//': vertices on the surface')
        call expect(normal_err < 1.0e-6_wp, name//': normals along the gradient')

        call expect(all(indices >= 0 .and. indices < size(xyz, 2)), name//': indices in range')
        do t = 1, size(indices), 3
            ia = indices(t) + 1
            ib = indices(t + 1) + 1
            ic = indices(t + 2) + 1
            e1 = real(xyz(:, ib) - xyz(:, ia), wp)
            e2 = real(xyz(:, ic) - xyz(:, ia), wp)
            ! outward: same side as the outward normal at the first vertex
            winding = dot_product(cross(e1, e2), real(normals(:, ia) + normals(:, ib) + normals(:, ic), wp))
            if (winding <= 0.0_wp) then
                call expect(.false., name//': counter-clockwise triangle seen from outside')
                exit
            end if
        end do
    end subroutine check_ellipsoid

    !> The geodetic latitude of each WGS84 vertex, from the closed form
    !> tan(lat) = z / ((1 - e^2) rho), matches its texture row.
    subroutine check_geodetic_latitude(n_lon, n_lat)
        integer, intent(in) :: n_lon, n_lat
        real(c_float), allocatable :: xyz(:,:), normals(:,:), uv(:,:)
        integer(c_int32_t), allocatable :: indices(:)
        real(wp) :: e2, p(3), lat, lat_row, err
        integer :: k
        call make(wgs84, n_lon, n_lat, xyz, normals, uv, indices)
        e2 = 1.0_wp - (wgs84(3) / wgs84(1))**2
        err = 0.0_wp
        do k = 1, size(xyz, 2)
            p = real(xyz(:, k), wp)
            lat_row = 0.5_wp * pi - pi * real(uv(2, k), wp)   ! v = 0 at the north pole
            if (abs(lat_row) > 0.5_wp * pi - 1.0e-6_wp) cycle  ! poles: longitude undefined
            lat = atan2(p(3), (1.0_wp - e2) * norm2(p(1:2)))
            err = max(err, abs(lat - lat_row))
        end do
        write(*, '(a,es10.2,a)') 'WGS84: max geodetic latitude error =', err, ' rad'
        call expect(err < 1.0e-6_wp, 'WGS84: texture rows at geodetic latitude')
    end subroutine check_geodetic_latitude

    pure function cross(a, b) result(c)
        real(wp), intent(in) :: a(3), b(3)
        real(wp) :: c(3)
        c = [a(2) * b(3) - a(3) * b(2), a(3) * b(1) - a(1) * b(3), a(1) * b(2) - a(2) * b(1)]
    end function cross

end program test_geometry
