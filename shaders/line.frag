#version 450

// Lines (see line.vert): the style's color, with the dash pattern's gaps
// discarded; blended between its two sizes (2^n and 2^(n+1) scene units).
// Highlighted with arrows: the quad is a band, a translucent glow of the
// highlight color, with the line (dashed, if it is) in its middle and
// chevrons of the highlight color over both, blended between their two
// spacings as the dashes.

layout(set = 0, binding = 1) uniform Style {
    vec4 color;
    vec4 params;            // z = dash pattern (16 bits; 65535 or 0: solid)
    vec4 count;
    vec4 highlight;         // highlight color (rgba)
} style;

layout(location = 0) in vec4 fragColor;
layout(location = 1) noperspective in vec3 dash;    // along the line, in periods 2^n and 2^(n+1);
                                                    // z = the weight of 2^(n+1)
layout(location = 2) noperspective in vec3 flow;    // as dash, for the arrows (periods of their spacing), moved
layout(location = 3) flat in vec2 flowPixels;       // pixels per period, of each size
layout(location = 4) noperspective in float across; // -1 to 1 across the quad
layout(location = 5) flat in vec4 highlight;        // x = pulse (0 to 1), y = the line's half width,
                                                    // z = the quad's (pixels), w = highlight flags
layout(location = 0) out vec4 outColor;

void main()
{
    float coverage = 1.0;
    uint pattern = uint(style.params.z) & 0xFFFFu;
    if (pattern != 0xFFFFu && pattern != 0u) {
        uvec2 bit = min(uvec2(floor(16.0 * fract(dash.xy))), uvec2(15u));
        float on0 = float((pattern >> bit.x) & 1u);
        float on1 = float((pattern >> bit.y) & 1u);
        coverage = mix(on0, on1, smoothstep(0.3, 0.7, dash.z));     // crisp but near the middle
    }
    if ((int(highlight.w + 0.5) & 2) == 0) {
        if (coverage <= 0.0) discard;
        outColor = vec4(fragColor.rgb, fragColor.a * coverage);
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
    c = mix(c, fragColor, line);
    c.rgb = mix(c.rgb, style.highlight.rgb, arrow);
    c.a = max(c.a, arrow * style.highlight.a);
    if (c.a < 0.01) discard;
    outColor = c;
}
