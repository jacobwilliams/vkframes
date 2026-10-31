!> Checks SkySphere's star catalog reading (no window), against the HYG v3
!> catalog in data/ (OpenFrames' Stars/Stars_HYGv3.txt): the stars in a
!> magnitude range, the limit on their number, Sirius' direction, sizes
!> growing with brightness, a catalog with right ascension in degrees and no
!> color index, and invalid settings or files.
program test_sky

    use vkframes
    use vkf_checks, only: check, report

    implicit none

    character(len=*), parameter :: catalog = 'data/Stars_HYGv3.txt'
    type(SkySphere) :: sky
    real(wp) :: dir(3), rgb(3), size, size1, ra, dec, m(2)
    integer :: i, unit
    logical :: sizes_ok


    ! magnitudes -2 to 6: 5068 stars of the catalog (counted with awk)
    call check(sky%set_star_data(catalog, min_mag=-2.0_wp, max_mag=6.0_wp, max_stars=40000), 'read the catalog')
    write(*, '(a,i0,a)') 'HYG v3: ', sky%num_stars(), ' stars of magnitude -2 to 6'
    call check(sky%num_stars() == 5068, 'stars in the magnitude range')
    m = sky%star_magnitude_range()
    call check(m(1) >= -2.0_wp .and. m(2) <= 6.0_wp, 'magnitudes in the range')

    ! the first star is Sirius: ra 6.752481 h, dec -16.716116 deg
    ra = 6.752481_wp / 24.0_wp * 2.0_wp * pi
    dec = -16.716116_wp * pi / 180.0_wp
    call sky%star(1, dir, rgb, size1)
    call check(maxval(abs(dir - [cos(ra) * cos(dec), sin(ra) * cos(dec), sin(dec)])) < 1.0e-6_wp, 'Sirius direction')
    call check(all(rgb > 0.0_wp .and. rgb <= 1.0_wp), 'Sirius color')

    ! sizes between the limits, the brightest (Sirius) the largest
    sizes_ok = .true.
    do i = 1, sky%num_stars()
        call sky%star(i, dir, rgb, size)
        if (size < 1.0_wp - 1.0e-6_wp .or. size > size1 + 1.0e-6_wp .or. abs(norm2(dir) - 1.0_wp) > 1.0e-6_wp) &
            sizes_ok = .false.
    end do
    call check(sizes_ok, 'sizes from 1 up to the brightest star, unit directions')

    ! at most max_stars, in catalog order
    call check(sky%set_star_data(catalog, max_mag=8.0_wp, max_stars=1000) .and. sky%num_stars() == 1000, 'max_stars')

    ! right ascension in degrees ("ra_360.0"), no color index
    open(newunit=unit, file='build/test_sky_catalog.txt', status='replace', action='write')
    write(unit, '(a)') 'ra_360.0'//achar(9)//'dec'//achar(9)//'mag'//achar(9)//'ci'//achar(9)//'proper'
    write(unit, '(a)') '90.0'//achar(9)//'0.0'//achar(9)//'1.0'
    write(unit, '(a)') '0.0'//achar(9)//'90.0'//achar(9)//'9.5'        ! too dim: left out
    close(unit)
    call check(sky%set_star_data('build/test_sky_catalog.txt', min_mag=-2.0_wp, max_mag=8.0_wp), 'degrees catalog')
    call sky%star(1, dir, rgb, size)
    call check(sky%num_stars() == 1 .and. maxval(abs(dir - [0.0_wp, 1.0_wp, 0.0_wp])) < 1.0e-6_wp, &
               'right ascension in degrees')

    ! invalid settings and files
    call check(.not. sky%set_star_data(catalog, min_mag=5.0_wp, max_mag=1.0_wp), 'min_mag > max_mag rejected')
    call check(.not. sky%set_star_data(catalog, min_dim_ratio=1.5_wp), 'dim ratio > 1 rejected')
    call check(.not. sky%set_star_data('does/not/exist.txt'), 'missing catalog')
    call check(.not. sky%set_texture_map('does/not/exist.jpg') .and. sky%texture_map() == '', 'missing texture')
    call check(sky%draw_mode() == SKY_TEXTURE + SKY_STARS, 'default draw mode')

    call report('SkySphere star catalog')

end program test_sky
