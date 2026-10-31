!> Checks ShadowCones without a window: the Earth's shadow in sunlight (km),
!> the radii taken from the Spheres; the umbra's length d R / (Rs - R); the
!> umbra's and penumbra's edges tangent to both the Earth and the Sun; the
!> shadow at the Earth, pointing away from the Sun however the Earth turns,
!> and following it along its orbit (reshaped for the new distance); the
!> regions drawn as set, the length limit cutting the umbra; a body larger
!> than its light: no apex, no antumbra.
program test_shadow

    use, intrinsic :: iso_c_binding, only: c_null_ptr
    use vkframes
    use vkf_checks, only: check, report

    implicit none

    real(wp), parameter :: rs = 696000.0_wp, re = 6378.137_wp
    real(wp), parameter :: au = 1.495978707e8_wp
    type(ReferenceFrame), target :: root
    type(Sphere), target :: sun, earth
    type(ShadowCones), target :: shadow
    type(Trajectory), target :: orbit
    type(FrameManager) :: scene
    real(wp) :: m(4,4), d, z0, r0, z1, r1, apex
    logical :: drawn
    integer :: seen


    ! the Sun at the origin; the Earth (turning) along an orbit; its shadow its child
    sun = Sphere('Sun', radius=rs)
    earth = Sphere('Earth', radius=re)
    call earth%set_sphere_scale([1.0_wp, 1.0_wp, 0.99665_wp])
    orbit = Trajectory()
    call orbit%add(0.0_wp, pos=[au, 0.0_wp, 0.0_wp], att=quat_from_axis_angle([0.0_wp, 0.0_wp, 1.0_wp], 0.5_wp * pi))
    call orbit%add(1.0_wp, pos=[0.0_wp, 1.02_wp * au, 0.0_wp], att=quat_from_axis_angle([1.0_wp, 0.0_wp, 0.0_wp], 1.0_wp))
    call orbit%add(2.0_wp, pos=[-au, 0.0_wp, 0.0_wp])               ! (followed in a loop: t = 1 inside it)
    call earth%follow(orbit)
    shadow = ShadowCones('Earth shadow', body=earth, light=sun)
    root = ReferenceFrame('Root')
    call check(root%add_child(sun) .and. root%add_child(earth) .and. earth%add_child(shadow), 'tree')
    call check(abs(shadow%body_radius() - re) < 1.0e-9_wp .and. shadow%light_radius() == rs, &
               "radii: the Spheres' (equatorial)")
    call check(shadow%frame_info() == 'ShadowCones' .and. .not. shadow%name_label_shown(), 'info, no name label')

    call root%realize(c_null_ptr)
    seen = -1
    call scene%set_frame(root)
    call scene%update(0.0_wp, seen)

    ! at the Earth, its +z axis away from the Sun, though the Earth is turned
    m = shadow%world_matrix()
    call check(maxval(abs(m(1:3, 4) - [au, 0.0_wp, 0.0_wp])) < 1.0e-6_wp, 'at the Earth')
    call check(maxval(abs(m(1:3, 3) - [1.0_wp, 0.0_wp, 0.0_wp])) < 1.0e-12_wp, 'pointing away from the Sun')
    call check(abs(shadow%distance() - au) < 1.0e-6_wp, 'distance')

    ! the umbra: d R / (Rs - R) long (about 1.38 million km), edges tangent to both
    apex = au * re / (rs - re)
    call check(abs(shadow%umbra_length() - apex) < 1.0e-6_wp * apex .and. abs(apex - 1.3836e6_wp) < 1.0e3_wp, &
               'umbra length')
    call shadow%region_extent(SHADOW_UMBRA, drawn, z0, r0, z1, r1)
    call check(drawn .and. abs(z1 - apex) < 1.0e-6_wp * apex .and. r1 == 0.0_wp, 'umbra: to its apex')
    call check(tangent(z0, r0, z1, r1, 0.0_wp, re) .and. tangent(z0, r0, z1, r1, -au, rs), &
               'umbra: tangent to the Earth and the Sun')
    call shadow%region_extent(SHADOW_ANTUMBRA, drawn, z0, r0, z1, r1)
    call check(drawn .and. abs(z0 - apex) < 1.0e-6_wp * apex .and. abs(z1 - 2.0_wp * apex) < 1.0e-6_wp * apex .and. &
               tangent(z0, r0, z1, r1, 0.0_wp, re), 'antumbra: from the apex, the default length')
    call shadow%region_extent(SHADOW_PENUMBRA, drawn, z0, r0, z1, r1)
    call check(drawn .and. z0 < 0.0_wp .and. r1 > r0 .and. tangent(z0, r0, z1, r1, 0.0_wp, re) .and. &
               tangent(z0, r0, z1, r1, -au, rs), 'penumbra: widening, tangent to the Earth and the Sun')
    call check(abs(shadow%drawn_length() - 2.0_wp * apex) < 1.0e-6_wp * apex, 'drawn length')

    ! along the orbit: moved, turned and reshaped
    call scene%update(1.0_wp, seen)
    d = 1.02_wp * au
    m = shadow%world_matrix()
    call check(maxval(abs(m(1:3, 4) - [0.0_wp, d, 0.0_wp])) < 1.0e-6_wp .and. &
               maxval(abs(m(1:3, 3) - [0.0_wp, 1.0_wp, 0.0_wp])) < 1.0e-12_wp, 'follows the Earth')
    call check(abs(shadow%umbra_length() - d * re / (rs - re)) < 1.0e-6_wp * apex, 'reshaped for the distance')

    ! the regions drawn, and the length limit
    call shadow%set_regions(SHADOW_UMBRA)
    call shadow%set_length(1.0e6_wp)
    call scene%update(1.0_wp, seen)
    call shadow%region_extent(SHADOW_UMBRA, drawn, z0, r0, z1, r1)
    call check(drawn .and. z1 == 1.0e6_wp .and. r1 > 0.0_wp .and. tangent(z0, r0, z1, r1, 0.0_wp, re), &
               'length: the umbra cut')
    call shadow%region_extent(SHADOW_PENUMBRA, drawn)
    call check(.not. drawn, 'regions: the penumbra not drawn')
    call shadow%region_extent(SHADOW_ANTUMBRA, drawn)
    call check(.not. drawn, 'antumbra: none before the apex')
    call shadow%set_region_color(SHADOW_UMBRA, [0.0_wp, 1.0_wp, 0.0_wp])
    call check(all(shadow%region_color(SHADOW_UMBRA) == [0.0_wp, 1.0_wp, 0.0_wp, 0.25_wp]), 'region color')

    ! a body larger than its light: the umbra widens, no apex, no antumbra
    call shadow%set_regions(SHADOW_ALL)
    call shadow%set_length(0.0_wp)
    call shadow%set_light_radius(0.5_wp * re)
    call scene%update(1.0_wp, seen)
    call shadow%region_extent(SHADOW_UMBRA, drawn, z0, r0, z1, r1)
    call check(drawn .and. r1 > r0 .and. abs(z1 - 100.0_wp * re) < 1.0e-9_wp * re, 'larger body: the umbra widens')
    call shadow%region_extent(SHADOW_ANTUMBRA, drawn)
    call check(.not. drawn .and. shadow%umbra_length() == huge(1.0_wp), 'larger body: no antumbra')

    call report('ShadowCones')

contains

    !> Whether the line through (z0, r0) and (z1, r1) (in the axis' plane) is
    !> tangent to a sphere at z = zc of radius rc.
    logical function tangent(z0, r0, z1, r1, zc, rc)
        real(wp), intent(in) :: z0, r0, z1, r1, zc, rc
        real(wp) :: dz, dr, dist
        dz = z1 - z0
        dr = r1 - r0
        dist = abs(dz * (0.0_wp - r0) - dr * (zc - z0)) / hypot(dz, dr)
        tangent = abs(dist - rc) < 1.0e-6_wp * rc
    end function tangent

end program test_shadow
