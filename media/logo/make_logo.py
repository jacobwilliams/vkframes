"""Generate the vkframes logo (vkframes-logo.svg, next to this script), and
vkframes-logo-dark.svg: the same in the dark-background colors always (for
renderers without prefers-color-scheme, e.g. ffmpeg's librsvg: the video
watermark of example/record_video.f90).

The mark: a half-lit body with an orbit traced from its start (a green start
marker) up to the spacecraft, the plane swept so far (translucent), and the
spacecraft's own frame (x, y, z axes, tilted with the orbit), as vkframes
draws them (CurveArtist in trace mode, RibbonArtist, MarkerArtist). The
wordmark: "vk" in Vulkan red and "frames" in Fortran purple, in lighter tints
on dark backgrounds (prefers-color-scheme).

    pixi run logo
"""
import math
import os

HERE = os.path.dirname(os.path.abspath(__file__))
OUT = os.path.join(HERE, "vkframes-logo.svg")
OUT_DARK = os.path.join(HERE, "vkframes-logo-dark.svg")

# the axes (x, y, z), the orbit and spacecraft, the outlines
RED, GREEN, BLUE = "#e5484d", "#2fb170", "#3e7bfa"
GOLD = "#f5b83d"
INK, INK_DARK = "#14213d", "#e6ecf5"
# the wordmark: on light backgrounds, and lighter on dark ones
VULKAN_RED, VULKAN_RED_DARK = "#a41e22", "#e5484d"
FORTRAN_PURPLE, FORTRAN_PURPLE_DARK = "#734f96", "#a98bd0"

# the orbit: center, semi-axes, tilt (degrees), and the part traced (degrees
# of the parametric angle, from the start marker to the spacecraft)
CX, CY, RX, RY, ROT = 128, 132, 112, 40, -22
T_START, T_SPACECRAFT = 150, 380

STYLE = f"""<style>
  .ink {{ stroke: {INK}; }}
  .vk {{ fill: {VULKAN_RED}; }}
  .ft {{ fill: {FORTRAN_PURPLE}; }}
  @media (prefers-color-scheme: dark) {{
    .ink {{ stroke: {INK_DARK}; }}
    .vk {{ fill: {VULKAN_RED_DARK}; }}
    .ft {{ fill: {FORTRAN_PURPLE_DARK}; }}
  }}
</style>"""
STYLE_DARK = f"""<style>
  .ink {{ stroke: {INK_DARK}; }}
  .vk {{ fill: {VULKAN_RED_DARK}; }}
  .ft {{ fill: {FORTRAN_PURPLE_DARK}; }}
</style>"""


def f(x):
    return f"{x:.1f}"


def ellipse_point(t_deg):
    """The orbit's point at parametric angle t (degrees)."""
    t, r = math.radians(t_deg), math.radians(ROT)
    x, y = RX * math.cos(t), RY * math.sin(t)
    return CX + x * math.cos(r) - y * math.sin(r), CY + x * math.sin(r) + y * math.cos(r)


def arc(t0, t1, n=64):
    """The orbit from t0 to t1 (degrees), as path points."""
    return [ellipse_point(t0 + (t1 - t0) * k / n) for k in range(n + 1)]


def path(points):
    return "M " + " L ".join(f"{f(x)} {f(y)}" for x, y in points)


def arrow(ox, oy, dx, dy, length, color, width, head, head_w):
    """A line from (ox, oy) along unit (dx, dy), with a triangular head."""
    tx, ty = ox + dx * length, oy + dy * length
    bx, by = tx - dx * head, ty - dy * head
    px, py = -dy, dx
    return (f'<line x1="{f(ox)}" y1="{f(oy)}" x2="{f(bx)}" y2="{f(by)}" stroke="{color}" '
            f'stroke-width="{width}" stroke-linecap="round"/>'
            f'<polygon points="{f(tx)},{f(ty)} {f(bx + px * head_w)},{f(by + py * head_w)} '
            f'{f(bx - px * head_w)},{f(by - py * head_w)}" fill="{color}"/>')


def logo(style=STYLE):
    parts = ['<defs><clipPath id="body"><circle cx="128" cy="132" r="44"/></clipPath></defs>']
    # the plane swept so far, and the orbit traced behind the body
    fan = arc(T_START, T_SPACECRAFT)
    parts.append(f'<path d="M {CX} {CY} L {path(fan)[2:]} Z" fill="{GOLD}" opacity="0.18"/>')
    parts.append(f'<path d="{path(arc(T_START, 360))}" fill="none" stroke="{GOLD}" stroke-width="6" '
                 'stroke-linecap="round"/>')
    # the body: night side, day side (lit from the right), a highlight
    parts.append('<g clip-path="url(#body)"><rect x="80" y="84" width="96" height="96" fill="#1b2b4b"/>'
                 f'<ellipse cx="150" cy="132" rx="44" ry="60" fill="{BLUE}"/>'
                 '<ellipse cx="160" cy="118" rx="26" ry="30" fill="#7fa8ff" opacity="0.6"/></g>')
    # the orbit in front of the body, up to the spacecraft, and its radius vector
    parts.append(f'<path d="{path(arc(360, T_SPACECRAFT))}" fill="none" stroke="{GOLD}" stroke-width="6" '
                 'stroke-linecap="round"/>')
    sx, sy = ellipse_point(T_SPACECRAFT)
    parts.append(f'<line x1="{CX}" y1="{CY}" x2="{f(sx)}" y2="{f(sy)}" stroke="{GOLD}" stroke-width="2.5" '
                 'opacity="0.7"/>')
    # the start marker
    bx, by = ellipse_point(T_START)
    parts.append(f'<circle cx="{f(bx)}" cy="{f(by)}" r="8" fill="{GREEN}" class="ink" stroke-width="3"/>')
    # the spacecraft's frame: isometric x (lower left), y (lower right), z (up), tilted with the orbit
    c, s, r = math.cos(math.radians(30)), math.sin(math.radians(30)), math.radians(ROT)
    for (dx, dy), length, color in zip(((-c, s), (c, s), (0.0, -1.0)), (44, 44, 54), (RED, GREEN, BLUE)):
        ux, uy = dx * math.cos(r) - dy * math.sin(r), dx * math.sin(r) + dy * math.cos(r)
        parts.append(arrow(sx, sy, ux, uy, length, color, width=5.5, head=14, head_w=7.5))
    parts.append(f'<circle cx="{f(sx)}" cy="{f(sy)}" r="9" fill="{GOLD}" class="ink" stroke-width="3"/>')
    # the wordmark
    family = "Inter, 'Helvetica Neue', Helvetica, Arial, sans-serif"
    parts.append(f'<text x="294" y="162" font-family="{family}" font-size="96" letter-spacing="-1">'
                 '<tspan class="vk" font-weight="700">vk</tspan>'
                 '<tspan class="ft" font-weight="500">frames</tspan></text>')
    body = "\n".join(parts)
    return ('<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 786 256" width="786" height="256" '
            'role="img" aria-label="vkframes">\n<title>vkframes</title>\n'
            f'{style}\n{body}\n</svg>\n')


if __name__ == "__main__":
    for out, style in ((OUT, STYLE), (OUT_DARK, STYLE_DARK)):
        with open(out, "w") as fh:
            fh.write(logo(style))
        print("wrote", out)
