#version 450

// the variant (see line.vert): plain (VKF_LINE_PLAIN), dashed (VKF_LINE_DASHED) or full
#if !defined(VKF_LINE_PLAIN) && !defined(VKF_LINE_DASHED)
#define VKF_LINE_FULL
#endif

// Lines (see line.vert): the style's color, with the dash pattern's gaps
// discarded. The pattern has two sizes (2^n and 2^(n+1) scene units); each
// pixel's signed distance to the nearest dash edge (in pixels: the pattern's
// bits scaled by how fast the dash position changes on screen) is blended
// between them, and its coverage is a one-pixel anti-aliased edge about it:
// so as the size changes (zooming) dashes merge and split smoothly, always
// solid, instead of cross-fading into half-transparent ones.
// Highlighted with arrows: the quad is a band, a translucent glow of the
// highlight color, with the line (dashed, if it is) in its middle and
// chevrons of the highlight color over both, blended between their two
// spacings as the dashes; drawn in two passes (pass, pc.lod.w: see make_line
// in src/vsg/vkf_layer.cpp): the line, writing depth, then the glow and the
// chevrons, writing none, with the translucent surfaces (sorted by depth),
// so the faint glow doesn't cut them away. A trail (see line.vert): each pixel in the past's
// or the future's color and dash pattern, faded toward the cutoffs, and none
// beyond them.

layout(set = 0, binding = 1) uniform Style {
    vec4 color;
    vec4 params;            // z = dash pattern (16 bits; 65535 or 0: solid)
    vec4 count;
    vec4 highlight;         // highlight color (rgba)
    vec4 flow;
    vec4 pulse;
    vec4 trail0;            // z = direction (0: no trail), w = color flags (1 past, 2 future: replacing the line's)
    vec4 trail1;            // the past's cutoff, its fade's start, the future's fade's start, its cutoff
    vec4 trail2;            // the past's color
    vec4 trail3;            // the future's color
    vec4 trail4;            // the past's dash pattern and factor, the future's
} style;

#ifdef VKF_LINE_PLAIN
// a plain line (line.vert's VKF_LINE_PLAIN): its color, nothing discarded
layout(location = 0) in vec4 fragColor;
layout(location = 0) out vec4 outColor;

void main()
{
    outColor = fragColor;
}
#else
layout(location = 0) in vec4 fragColor;
layout(location = 1) noperspective in vec3 dash;    // along the line, in periods 2^n and 2^(n+1);
                                                    // z = the weight of 2^(n+1)
#ifdef VKF_LINE_FULL
layout(location = 2) noperspective in vec3 flow;    // as dash, for the arrows (periods of their spacing), moved
layout(location = 3) flat in vec2 flowPixels;       // pixels per period, of each size
layout(location = 4) noperspective in float across; // -1 to 1 across the quad
layout(location = 5) flat in vec4 highlight;        // x = pulse (0 to 1), y = the line's half width,
                                                    // z = the quad's (pixels), w = highlight flags
layout(location = 6) in float trailDistance;        // from the current time's, in the direction of time
layout(location = 7) noperspective in vec3 dash2;   // (trails) the future's dashes
layout(location = 8) flat in int pass;              // 0 the line, 1 a highlighted line's glow and arrows
#endif
layout(location = 0) out vec4 outColor;

// The signed distance (in bits; > 0 in a dash) from position x (0 to 16, in
// bits of the 16-bit `pattern`, repeating) to the nearest change of the
// pattern: on to off or off to on.
float bitDistance(uint pattern, float x)
{
    int b = clamp(int(floor(x)), 0, 15);
    uint on = (pattern >> uint(b)) & 1u;
    float ahead = float(b + 1) - x, behind = x - float(b);
    for (int k = 1; k < 16; ++k) {
        if (((pattern >> uint((b + k) & 15)) & 1u) != on) break;
        ahead += 1.0;
    }
    for (int k = 1; k < 16; ++k) {
        if (((pattern >> uint((b - k) & 15)) & 1u) != on) break;
        behind += 1.0;
    }
    float d = min(ahead, behind);
    return (on == 1u) ? d : -d;
}

// The coverage of a dash pattern at dash positions d (its two sizes, and the
// second's weight), `rate` the screen-space rate of d.xy (periods per pixel):
// the edge distances in pixels blended (crisp where one size shows, gaps
// closing or opening as they change), anti-aliased over a pixel; where the
// pattern's bits are under a pixel (fixed-length dashes, far away), its
// average instead (a fainter solid line, not a flicker).
float dashCoverage(uint pattern, vec3 d, vec2 rate)
{
    if (pattern == 0xFFFFu || pattern == 0u) return 1.0;
    vec2 pixelsPerBit = 1.0 / max(16.0 * rate, vec2(1.0e-6));
    float w = smoothstep(0.3, 0.7, d.z);
    float d0 = bitDistance(pattern, 16.0 * fract(d.x)) * pixelsPerBit.x;
    float d1 = bitDistance(pattern, 16.0 * fract(d.y)) * pixelsPerBit.y;
    float coverage = clamp(0.5 + mix(d0, d1, w), 0.0, 1.0);
    float average = float(bitCount(pattern)) / 16.0;
    return mix(average, coverage, smoothstep(0.25, 0.75, mix(pixelsPerBit.x, pixelsPerBit.y, w)));
}

// The screen-space rate of change of dash positions (periods per pixel, along
// the line), for each size. (Derivatives: taken before any discard.)
vec2 dashRate(vec3 d)
{
    return vec2(length(vec2(dFdx(d.x), dFdy(d.x))), length(vec2(dFdx(d.y), dFdy(d.y))));
}

// 0 at a to 1 at b (a step if they are the same)
float ramp(float a, float b, float x)
{
    return (abs(b - a) > 1.0e-6) ? smoothstep(min(a, b), max(a, b), x) : step(a, x);
}

#ifdef VKF_LINE_DASHED
// a dashed line (line.vert's VKF_LINE_DASHED: no highlight or trail)
void main()
{
    vec2 rate = dashRate(dash);                                  // (before any discard)
    float coverage = dashCoverage(uint(style.params.z) & 0xFFFFu, dash, rate);
    if (coverage <= 0.0) discard;
    outColor = vec4(fragColor.rgb, fragColor.a * coverage);
}
#else
void main()
{
    // (in uniform control flow, before any discard: the style is uniform; none for solid lines)
    vec2 rate = vec2(0.0), rate2 = vec2(0.0);
    uint pattern = uint(style.params.z) & 0xFFFFu;
    if ((pattern != 0xFFFFu && pattern != 0u) || style.trail0.z != 0.0) {
        rate = dashRate(dash);
        rate2 = dashRate(dash2);
    }
    vec4 base = fragColor;
    float coverage, fade = 1.0;
    if (style.trail0.z != 0.0) {                                 // a trail: the past's or the future's style
        float r = trailDistance;
        if (r < style.trail1.x || r > style.trail1.w) discard;  // (beyond the cutoffs)
        int flags = int(style.trail0.w + 0.5);
        if (r <= 0.0) {
            fade = ramp(style.trail1.x, style.trail1.y, r);
            base = ((flags & 1) != 0) ? style.trail2 : vec4(base.rgb, base.a * style.trail2.a);
            coverage = dashCoverage(uint(style.trail4.x) & 0xFFFFu, dash, rate);
        } else {
            fade = 1.0 - ramp(style.trail1.z, style.trail1.w, r);
            base = ((flags & 2) != 0) ? style.trail3 : vec4(base.rgb, base.a * style.trail3.a);
            coverage = dashCoverage(uint(style.trail4.z) & 0xFFFFu, dash2, rate2);
        }
    } else {
        coverage = dashCoverage(uint(style.params.z) & 0xFFFFu, dash, rate);
    }
    if ((int(highlight.w + 0.5) & 2) == 0) {
        if (pass != 0 || coverage <= 0.0 || fade <= 0.0) discard;
        outColor = vec4(base.rgb, base.a * coverage * fade);
        return;
    }

    // the band: r pixels from its middle, out to h
    float h = max(highlight.z, 1.0);
    float r = abs(across) * h;
    float glow = style.highlight.a * (0.25 + 0.15 * highlight.x) * (1.0 - smoothstep(0.3 * h, h, r));
    float line = clamp(highlight.y - r + 0.5, 0.0, 1.0) * coverage;
    // chevrons: tip in the middle of each period, arms going back to the band's edges
    vec2 g = mod(fract(flow.xy) * flowPixels + 1.2 * r, flowPixels);
    float thickness = max(1.5, 0.35 * h);
    vec2 on = clamp(0.5 * thickness - abs(g - 0.5 * flowPixels) + 0.5, 0.0, 1.0);
    float arrow = mix(on.x, on.y, smoothstep(0.3, 0.7, flow.z)) * (1.0 - smoothstep(0.75 * h, h, r));

    // two passes: the line (writing depth: it hides what's behind it), then
    // the glow and the arrows (writing none, after the translucent surfaces
    // are sorted in: where faint, the glow doesn't hide them)
    if (pass == 0) {
        float a = base.a * line * fade;
        if (a < 0.01) discard;
        outColor = vec4(base.rgb, a);
        return;
    }
    vec4 c = vec4(style.highlight.rgb, glow * (1.0 - line));     // (the glow about the line, not over it)
    c.a = max(c.a, arrow * style.highlight.a) * fade;
    if (c.a < 0.01) discard;
    outColor = c;
}
#endif
#endif
