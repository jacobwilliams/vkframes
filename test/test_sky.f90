!> Checks SkySphere's star catalog reading (no window), against the HYG v3
!> catalog in data/ (OpenFrames' Stars/Stars_HYGv3.txt): the stars in a
!> magnitude range, the limit on their number, Sirius' direction, sizes
!> growing with brightness, a catalog with right ascension in degrees and no
!> color index, the named stars (a name of two words) and their settings, the
!> constellations (data/inp_Constellation.txt) and their settings, and invalid
!> settings or files.
program test_sky

    use vkframes
    use vkf_checks, only: check, report

    implicit none

    character(len=*), parameter :: catalog = 'data/Stars_HYGv3.txt'
    type(SkySphere) :: sky
    real(wp) :: dir(3), rgb(3), size, size1, ra, dec, m(2)
    integer :: i, unit, which, first, last
    real(wp) :: a(3), b(3)
    logical :: sizes_ok
    character(len=:), allocatable :: name


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

    ! named stars: 129 of magnitude -2 to 6 (awk), Sirius first; Rigil Kentaurus (two words) fourth
    write(*, '(a,i0,a)') 'HYG v3: ', sky%num_star_names(), ' named stars of magnitude -2 to 6'
    call check(sky%num_star_names() == 129, 'named stars')
    call sky%star_name(1, name, size, which)
    call check(name == 'Sirius' .and. which == 1 .and. abs(size + 1.44_wp) < 1.0e-9_wp, 'Sirius named')
    call sky%star_name(4, name, size, which)
    call check(name == 'Rigil Kentaurus', 'a name of two words')
    call check(.not. sky%star_names() .and. sky%num_star_names_drawn() == 0, 'names not drawn by default')
    call check(sky%set_star_names(.true., max_mag=2.0_wp) .and. sky%star_names(), 'names drawn')
    call check(sky%num_star_names_drawn() == 0, 'no names drawn before the sky is shown')
    call check(.not. sky%set_star_names(.true., size=0.0_wp), 'name size 0 rejected')

    ! constellations: 87 of 763 lines (awk), Andromeda first, Orion's first line from Chi2 Ori
    call check(.not. sky%constellations() .and. sky%num_constellations() == 0, 'no constellations by default')
    call check(sky%set_constellations(.true., file='data/inp_Constellation.txt'), 'read the constellations')
    write(*, '(a,i0,a,i0,a)') 'constellations: ', sky%num_constellations(), ' of ', sky%num_constellation_lines(), ' lines'
    call check(sky%num_constellations() == 87 .and. sky%num_constellation_lines() == 763, 'constellations and lines')
    call check(sky%constellation_name(1) == 'Andromeda' .and. sky%constellation_name(87) == 'Vulpecula', &
               'constellation names')
    call sky%constellation_lines(87, first, last)
    call check(first == 762 .and. last == 763, "a constellation's lines")
    do i = 1, sky%num_constellations()
        if (sky%constellation_name(i) == 'Orion') exit
    end do
    call sky%constellation_lines(i, first, last)
    call sky%constellation_line(first, a, b)
    ra = 6.0649_wp / 24.0_wp * 2.0_wp * pi
    dec = 20.1337_wp * pi / 180.0_wp
    call check(maxval(abs(a - [cos(ra) * cos(dec), sin(ra) * cos(dec), sin(dec)])) < 1.0e-6_wp .and. &
               abs(norm2(b) - 1.0_wp) < 1.0e-6_wp, "Orion's first line")
    call check(.not. sky%set_constellations(.true., width=0.0_wp), 'constellation width 0 rejected')
    call check(.not. sky%set_constellations(.true., file='does/not/exist.txt') .and. &
               sky%num_constellation_lines() == 763, 'missing constellation file: those read kept')
    call check(sky%set_constellations(.false.) .and. .not. sky%constellations(), 'constellations hidden')

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
    call check(sky%num_star_names() == 0, 'no names without a color index')

    ! invalid settings and files
    call check(.not. sky%set_star_data(catalog, min_mag=5.0_wp, max_mag=1.0_wp), 'min_mag > max_mag rejected')
    call check(.not. sky%set_star_data(catalog, min_dim_ratio=1.5_wp), 'dim ratio > 1 rejected')
    call check(.not. sky%set_star_data('does/not/exist.txt'), 'missing catalog')
    call check(.not. sky%set_texture_map('does/not/exist.jpg') .and. sky%texture_map() == '', 'missing texture')
    call check(sky%draw_mode() == SKY_TEXTURE + SKY_STARS, 'default draw mode')

    call report('SkySphere star catalog')

end program test_sky
