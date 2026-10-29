#version 450

// Lines (see line.vert): the style's color, with the dash pattern's gaps
// discarded; blended between its two sizes (2^n and 2^(n+1) scene units).
// Highlighted with arrows: the quad is a band, a translucent glow of the
// highlight color, with the line (dashed, if it is) in its middle and
// chevrons of the highlight color over both, blended between their two
// spacings as the dashes. A trail (see line.vert): each pixel in the past's
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

layout(location = 0) in vec4 fragColor;
layout(location = 1) noperspective in vec3 dash;    // along the line, in periods 2^n and 2^(n+1);
                                                    // z = the weight of 2^(n+1)
layout(location = 2) noperspective in vec3 flow;    // as dash, for the arrows (periods of their spacing), moved
layout(location = 3) flat in vec2 flowPixels;       // pixels per period, of each size
layout(location = 4) noperspective in float across; // -1 to 1 across the quad
layout(location = 5) flat in vec4 highlight;        // x = pulse (0 to 1), y = the line's half width,
                                                    // z = the quad's (pixels), w = highlight flags
layout(location = 6) in float trailDistance;        // from the current time's, in the direction of time
layout(location = 7) noperspective in vec3 dash2;   // (trails) the future's dashes
layout(location = 0) out vec4 outColor;

// The coverage of a dash pattern at dash positions d (1 where solid).
float dashCoverage(uint pattern, vec3 d)
{
    if (pattern == 0xFFFFu || pattern == 0u) return 1.0;
    uvec2 bit = min(uvec2(floor(16.0 * fract(d.xy))), uvec2(15u));
    float on0 = float((pattern >> bit.x) & 1u);
    float on1 = float((pattern >> bit.y) & 1u);
    return mix(on0, on1, smoothstep(0.3, 0.7, d.z));             // crisp but near the middle
}

// 0 at a to 1 at b (a step if they are the same)
float ramp(float a, float b, float x)
{
    return (abs(b - a) > 1.0e-6) ? smoothstep(min(a, b), max(a, b), x) : step(a, x);
}

void main()
{
    vec4 base = fragColor;
    float coverage, fade = 1.0;
    if (style.trail0.z != 0.0) {                                 // a trail: the past's or the future's style
        float r = trailDistance;
        if (r < style.trail1.x || r > style.trail1.w) discard;  // (beyond the cutoffs)
        int flags = int(style.trail0.w + 0.5);
        if (r <= 0.0) {
            fade = ramp(style.trail1.x, style.trail1.y, r);
            base = ((flags & 1) != 0) ? style.trail2 : vec4(base.rgb, base.a * style.trail2.a);
            coverage = dashCoverage(uint(style.trail4.x) & 0xFFFFu, dash);
        } else {
            fade = 1.0 - ramp(style.trail1.z, style.trail1.w, r);
            base = ((flags & 2) != 0) ? style.trail3 : vec4(base.rgb, base.a * style.trail3.a);
            coverage = dashCoverage(uint(style.trail4.z) & 0xFFFFu, dash2);
        }
    } else {
        coverage = dashCoverage(uint(style.params.z) & 0xFFFFu, dash);
    }
    if ((int(highlight.w + 0.5) & 2) == 0) {
        if (coverage <= 0.0 || fade <= 0.0) discard;
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

    vec4 c = vec4(style.highlight.rgb, glow);
    c = mix(c, base, line);
    c.rgb = mix(c.rgb, style.highlight.rgb, arrow);
    c.a = max(c.a, arrow * style.highlight.a) * fade;
    if (c.a < 0.01) discard;
    outColor = c;
}
