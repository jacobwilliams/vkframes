!> Picking geometry: what is under a point of a window (WindowProxy%pick).
!>
!> A PickCamera is the camera and the cursor of a pick, in the scene's root
!> frame: the eye and its axes (right, up, forward), the projection, the
!> viewport's size and the cursor's position (-1 to 1, y up, as the window's
!> events). Frames test their contents against it (ReferenceFrame%pick and
!> its extensions):
!> * points and line segments on the screen, within a tolerance in pixels
!>   (`point_pick`, `segment_pick`: the closest point of a segment, its
!>   fraction along the segment in the scene, perspective-correct, and its
!>   depth), for trajectories, markers and frames' origins;
!> * a ray from the eye through the cursor against an ellipsoid (`ray`,
!>   `ray_ellipsoid`), for bodies;
!> * the ray against a plane, a sphere and a line (`ray_plane`,
!>   `ray_sphere`, `ray_line`), for dragging (WindowProxy%cursor_ray, and
!>   vkf_handles' constraints);
!> * bounding spheres on the screen (`sphere_near`), to skip whole chunks of
!>   a long trajectory.
!> Depths are distances along the view direction (perspective) or from the
!> eye's plane (orthographic); points closer than `near` are behind the eye.
module vkf_pick

    use vkf_math, only: wp, pi, cross

    implicit none

    private

    !> The camera and cursor of a pick (see the module documentation).
    type, public :: PickCamera
        real(wp) :: eye(3) = 0.0_wp                         !! the eye (root frame)
        real(wp) :: right(3) = [1.0_wp, 0.0_wp, 0.0_wp]     !! the camera's right axis (unit vector, root frame)
        real(wp) :: up(3) = [0.0_wp, 0.0_wp, 1.0_wp]        !! the camera's up axis (unit vector, root frame)
        real(wp) :: forward(3) = [0.0_wp, 1.0_wp, 0.0_wp]   !! the view direction (unit vector, root frame)
        logical :: perspective = .true.                     !! (true: perspective; false: orthographic)
        real(wp) :: tan_y = 0.0_wp                          !! tan(vertical field of view / 2)
        real(wp) :: aspect = 1.0_wp                         !! width / height
        real(wp) :: ortho(4) = [-1.0_wp, 1.0_wp, -1.0_wp, 1.0_wp]   !! (orthographic: left, right, bottom, top)
        real(wp) :: half_w = 1.0_wp, half_h = 1.0_wp        !! half the viewport (framebuffer pixels)
        real(wp) :: x = 0.0_wp, y = 0.0_wp                  !! the cursor (-1 to 1, y up)
        real(wp) :: tolerance = 6.0_wp                      !! framebuffer pixels
        real(wp) :: pixel_ratio = 1.0_wp                    !! framebuffer pixels per window unit (for line widths, marker sizes)
        real(wp) :: near = 0.0_wp                           !! the smallest depth in front of the eye
        integer :: kinds = -1                               !! the kinds of hits wanted: bit k for kind k (all)
        real(wp) :: max_depth = huge(1.0_wp)                !! hits deeper than this aren't wanted (nearest only)
    end type PickCamera

    public :: pick_camera, project, point_pick, segment_pick, sphere_near, ray, ray_ellipsoid, wants
    public :: ray_plane, ray_sphere, ray_line

contains

    !> The camera of a pick: eye, center and up (root frame), a perspective
    !> projection (`fovy`, degrees, and `aspect`) or an orthographic one
    !> (`ortho`: left, right, bottom, top), a viewport of `width` x `height`
    !> framebuffer pixels, the cursor (x, y: -1 to 1, y up) and the tolerance
    !> (framebuffer pixels).
    pure function pick_camera(eye, center, up, perspective, fovy, aspect, ortho, width, height, x, y, tolerance) &
            result(cam)
        real(wp), intent(in) :: eye(3), center(3), up(3)    !! the camera (root frame)
        real(wp), intent(in) :: fovy                        !! vertical field of view (degrees; perspective)
        real(wp), intent(in) :: aspect                      !! width / height (perspective)
        real(wp), intent(in) :: ortho(4)                    !! left, right, bottom, top (orthographic)
        real(wp), intent(in) :: x, y                        !! the cursor (-1 to 1, y up)
        real(wp), intent(in) :: tolerance                   !! (framebuffer pixels)
        logical, intent(in) :: perspective                  !! (true: perspective; false: orthographic)
        integer, intent(in) :: width, height                !! the viewport (framebuffer pixels)
        type(PickCamera) :: cam
        real(wp) :: f(3), r(3), d
        f = center - eye
        d = norm2(f)
        if (d > 0.0_wp) f = f / d
        r = cross(f, up)
        if (norm2(r) > 0.0_wp) r = r / norm2(r)
        cam%eye = eye
        cam%forward = f
        cam%right = r
        cam%up = cross(r, f)
        cam%perspective = perspective
        cam%tan_y = tan(0.5_wp * fovy * pi / 180.0_wp)
        cam%aspect = aspect
        cam%ortho = ortho
        cam%half_w = 0.5_wp * max(1, width)
        cam%half_h = 0.5_wp * max(1, height)
        cam%x = x
        cam%y = y
        cam%tolerance = tolerance
        cam%near = 1.0e-6_wp * max(d, tiny(1.0_wp))          ! (as the renderer's near plane)
    end function pick_camera

    !> Hits of `kind` (vkf_frames' PICK_*) are wanted.
    pure logical function wants(cam, kind)
        type(PickCamera), intent(in) :: cam
        integer, intent(in) :: kind                         !! a PICK_* constant of vkf_frames
        wants = btest(cam%kinds, kind)
    end function wants

    !> The screen offset (framebuffer pixels) of point p (root frame) from the
    !> cursor, and its depth; false if it is behind the eye.
    logical function project(cam, p, sx, sy, depth) result(visible)
        type(PickCamera), intent(in) :: cam
        real(wp), intent(in) :: p(3)
        real(wp), intent(out) :: sx, sy                     !! offset from the cursor (framebuffer pixels, y up)
        real(wp), intent(out) :: depth                      !! (scene units)
        real(wp) :: v(3), nx, ny
        v = p - cam%eye
        depth = dot_product(v, cam%forward)
        sx = huge(1.0_wp)
        sy = huge(1.0_wp)
        visible = .false.
        if (cam%perspective) then
            if (depth <= cam%near) return
            nx = dot_product(v, cam%right) / (depth * cam%tan_y * cam%aspect)
            ny = dot_product(v, cam%up) / (depth * cam%tan_y)
        else
            nx = (2.0_wp * dot_product(v, cam%right) - (cam%ortho(1) + cam%ortho(2))) / (cam%ortho(2) - cam%ortho(1))
            ny = (2.0_wp * dot_product(v, cam%up) - (cam%ortho(3) + cam%ortho(4))) / (cam%ortho(4) - cam%ortho(3))
        end if
        sx = (nx - cam%x) * cam%half_w
        sy = (ny - cam%y) * cam%half_h
        visible = .true.
    end function project

    !> Point p (root frame) is within the tolerance (plus `extra` pixels, e.g.
    !> half a marker's size) of the cursor: its distance from it (pixels) and
    !> its depth.
    logical function point_pick(cam, p, pixels, depth, extra) result(hit)
        type(PickCamera), intent(in) :: cam
        real(wp), intent(in) :: p(3)
        real(wp), intent(out) :: pixels                     !! distance from the cursor (framebuffer pixels)
        real(wp), intent(out) :: depth                      !! (scene units)
        real(wp), intent(in), optional :: extra             !! added to the tolerance (framebuffer pixels)
        real(wp) :: sx, sy, reach
        hit = .false.
        pixels = huge(1.0_wp)
        if (.not. project(cam, p, sx, sy, depth)) return
        pixels = hypot(sx, sy)
        reach = cam%tolerance
        if (present(extra)) reach = reach + extra
        hit = pixels <= reach .and. depth <= cam%max_depth
    end function point_pick

    !> Segment a-b (root frame) passes within the tolerance (plus `extra`
    !> pixels, e.g. half the line's width) of the cursor: the distance
    !> (pixels) of its closest point, the fraction `s` of the way from a to b
    !> of that point in the scene (perspective-correct), and its depth. The
    !> part behind the eye is cut off.
    logical function segment_pick(cam, a, b, pixels, s, depth, extra) result(hit)
        type(PickCamera), intent(in) :: cam
        real(wp), intent(in) :: a(3), b(3)
        real(wp), intent(out) :: pixels                     !! distance from the cursor (framebuffer pixels)
        real(wp), intent(out) :: s                          !! fraction of the way from a to b (0 to 1)
        real(wp), intent(out) :: depth                      !! (scene units)
        real(wp), intent(in), optional :: extra             !! added to the tolerance (framebuffer pixels)
        real(wp) :: da, db, s0, s1, p0(3), p1(3), x0, y0, x1, y1, z0, z1, dx, dy, t, len2, u
        hit = .false.
        pixels = huge(1.0_wp)
        s = 0.0_wp
        depth = huge(1.0_wp)
        ! the part in front of the eye: s0 .. s1 of a-b
        s0 = 0.0_wp
        s1 = 1.0_wp
        if (cam%perspective) then
            da = dot_product(a - cam%eye, cam%forward)
            db = dot_product(b - cam%eye, cam%forward)
            if (da <= cam%near .and. db <= cam%near) return
            if (da <= cam%near) s0 = (cam%near - da) / (db - da) * 1.000001_wp
            if (db <= cam%near) s1 = (da - cam%near) / (da - db) * 0.999999_wp
        end if
        p0 = a + s0 * (b - a)
        p1 = a + s1 * (b - a)
        if (.not. project(cam, p0, x0, y0, z0)) return
        if (.not. project(cam, p1, x1, y1, z1)) return
        ! the closest point on the screen segment (the cursor is at 0, 0)
        dx = x1 - x0
        dy = y1 - y0
        len2 = dx * dx + dy * dy
        t = 0.0_wp
        if (len2 > 0.0_wp) t = max(0.0_wp, min(1.0_wp, -(x0 * dx + y0 * dy) / len2))
        pixels = hypot(x0 + t * dx, y0 + t * dy)
        ! back to the scene: linear on the screen is not linear in depth
        u = t
        if (cam%perspective) u = t * z0 / (t * z0 + (1.0_wp - t) * z1)
        s = s0 + u * (s1 - s0)
        depth = z0 + u * (z1 - z0)
        hit = pixels <= cam%tolerance
        if (present(extra)) hit = pixels <= cam%tolerance + extra
        hit = hit .and. depth <= cam%max_depth
    end function segment_pick

    !> A sphere (root frame: center c, radius r) might be within the tolerance
    !> (plus `extra` pixels) of the cursor on the screen (false only if it
    !> surely isn't: to skip what it bounds).
    logical function sphere_near(cam, c, r, extra) result(near)
        type(PickCamera), intent(in) :: cam
        real(wp), intent(in) :: c(3), r
        real(wp), intent(in), optional :: extra             !! added to the tolerance (framebuffer pixels)
        real(wp) :: sx, sy, depth, scale, reach
        near = .true.
        if (dot_product(c - cam%eye, cam%forward) - r > cam%max_depth) then   ! (all deeper than a hit already found)
            near = .false.
            return
        end if
        if (cam%perspective) then
            depth = dot_product(c - cam%eye, cam%forward)
            if (depth <= r + cam%near) then                     ! (the eye inside it, or close: can't tell)
                near = depth > -r
                return
            end if
            if (.not. project(cam, c, sx, sy, depth)) return
            scale = cam%half_h / (cam%tan_y * (depth - r))      ! pixels per scene unit at its nearest
        else
            if (.not. project(cam, c, sx, sy, depth)) return
            scale = 2.0_wp * cam%half_h / (cam%ortho(4) - cam%ortho(3))
        end if
        reach = cam%tolerance
        if (present(extra)) reach = reach + extra
        near = hypot(sx, sy) <= r * scale + reach
    end function sphere_near

    !> The ray from the eye through the cursor (root frame; `dir` a unit vector).
    pure subroutine ray(cam, origin, dir)
        type(PickCamera), intent(in) :: cam
        real(wp), intent(out) :: origin(3), dir(3)
        if (cam%perspective) then
            origin = cam%eye
            dir = cam%forward + cam%x * cam%tan_y * cam%aspect * cam%right + cam%y * cam%tan_y * cam%up
            dir = dir / norm2(dir)
        else
            origin = cam%eye + cam%right * (cam%ortho(1) + 0.5_wp * (cam%x + 1.0_wp) * (cam%ortho(2) - cam%ortho(1))) &
                             + cam%up * (cam%ortho(3) + 0.5_wp * (cam%y + 1.0_wp) * (cam%ortho(4) - cam%ortho(3)))
            dir = cam%forward
        end if
    end subroutine ray

    !> Where the ray (origin, unit dir) first meets the ellipsoid centered at
    !> `center` with semi-axes `radii` along the columns of `axes` (unit
    !> vectors): the distance along the ray, the point, and the point in the
    !> ellipsoid's axes (from its center). False if it misses (or the
    !> ellipsoid is behind).
    logical function ray_ellipsoid(origin, dir, center, axes, radii, distance, point, local) result(hit)
        real(wp), intent(in) :: origin(3), dir(3)           !! the ray (dir: a unit vector)
        real(wp), intent(in) :: center(3)                   !! the ellipsoid's center
        real(wp), intent(in) :: axes(3,3)                   !! its axes (columns: unit vectors)
        real(wp), intent(in) :: radii(3)                    !! its semi-axes along them
        real(wp), intent(out) :: distance                   !! along the ray (huge() if missed)
        real(wp), intent(out) :: point(3)                   !! the point met
        real(wp), intent(out) :: local(3)                   !! the point in the ellipsoid's axes, from its center
        real(wp) :: o(3), d(3), qa, qb, qc, disc, t
        hit = .false.
        distance = huge(1.0_wp)
        point = 0.0_wp
        local = 0.0_wp
        if (any(radii <= 0.0_wp)) return
        o = matmul(transpose(axes), origin - center) / radii    ! (the unit sphere)
        d = matmul(transpose(axes), dir) / radii
        qa = dot_product(d, d)
        qb = 2.0_wp * dot_product(o, d)
        qc = dot_product(o, o) - 1.0_wp
        disc = qb * qb - 4.0_wp * qa * qc
        if (disc < 0.0_wp .or. qa <= 0.0_wp) return
        t = (-qb - sqrt(disc)) / (2.0_wp * qa)
        if (t < 0.0_wp) t = (-qb + sqrt(disc)) / (2.0_wp * qa)  ! (the eye inside it: where the ray leaves)
        if (t < 0.0_wp) return
        distance = t
        point = origin + t * dir
        local = (o + t * d) * radii
        hit = .true.
    end function ray_ellipsoid

    !> Where the ray (origin, unit dir) meets the plane through `point` with
    !> normal `normal`: the distance along the ray and the point. False if the
    !> ray is parallel to the plane (within 1e-6 of it, as a cosine) or the
    !> plane is behind.
    logical function ray_plane(origin, dir, point, normal, distance, hit_point) result(hit)
        real(wp), intent(in) :: origin(3), dir(3)           !! the ray (dir: a unit vector)
        real(wp), intent(in) :: point(3), normal(3)         !! the plane (normal: any length but 0)
        real(wp), intent(out) :: distance                   !! along the ray (huge() if missed)
        real(wp), intent(out) :: hit_point(3)               !! the point met
        real(wp) :: n(3), d
        hit = .false.
        distance = huge(1.0_wp)
        hit_point = 0.0_wp
        if (norm2(normal) <= 0.0_wp) return
        n = normal / norm2(normal)
        d = dot_product(dir, n)
        if (abs(d) < 1.0e-6_wp) return
        distance = dot_product(point - origin, n) / d
        if (distance < 0.0_wp) then
            distance = huge(1.0_wp)
            return
        end if
        hit_point = origin + distance * dir
        hit = .true.
    end function ray_plane

    !> Where the ray (origin, unit dir) meets the sphere of `center` and
    !> `radius`: the distances along the ray of where it enters and leaves
    !> (`near` <= `far`; `near` < 0 if the origin is inside). False if it
    !> misses, or the sphere is behind.
    logical function ray_sphere(origin, dir, center, radius, near, far) result(hit)
        real(wp), intent(in) :: origin(3), dir(3)           !! the ray (dir: a unit vector)
        real(wp), intent(in) :: center(3)                   !! the sphere's center
        real(wp), intent(in) :: radius                      !! its radius (> 0)
        real(wp), intent(out) :: near, far                  !! distances along the ray (huge() if missed)
        real(wp) :: o(3), b, c, disc
        hit = .false.
        near = huge(1.0_wp)
        far = huge(1.0_wp)
        if (radius <= 0.0_wp) return
        o = origin - center
        b = dot_product(o, dir)
        c = dot_product(o, o) - radius * radius
        disc = b * b - c
        if (disc < 0.0_wp) return
        if (-b + sqrt(disc) < 0.0_wp) return
        near = -b - sqrt(disc)
        far = -b + sqrt(disc)
        hit = .true.
    end function ray_sphere

    !> The point of the line through `point` along `axis` closest to the ray
    !> (origin, unit dir): its parameter `s` (the point is point + s * axis,
    !> axis normalized) and the point. False if the ray and the line are
    !> parallel (within 1e-6 of it, as a sine squared), or the closest point
    !> of the ray is behind its origin.
    logical function ray_line(origin, dir, point, axis, s, line_point) result(hit)
        real(wp), intent(in) :: origin(3), dir(3)           !! the ray (dir: a unit vector)
        real(wp), intent(in) :: point(3), axis(3)           !! the line (axis: any length but 0)
        real(wp), intent(out) :: s                          !! along the line (scene units, from `point`)
        real(wp), intent(out) :: line_point(3)              !! the point of the line
        real(wp) :: u(3), w(3), b, d, e, denom
        hit = .false.
        s = 0.0_wp
        line_point = point
        if (norm2(axis) <= 0.0_wp) return
        u = axis / norm2(axis)
        w = point - origin
        b = dot_product(u, dir)
        d = dot_product(u, w)
        e = dot_product(dir, w)
        denom = 1.0_wp - b * b
        if (denom < 1.0e-6_wp) return
        s = (b * e - d) / denom
        if ((e + b * s) < 0.0_wp) return                    ! (the ray's closest point: origin + (e + b s) dir)
        line_point = point + s * u
        hit = .true.
    end function ray_line

end module vkf_pick
