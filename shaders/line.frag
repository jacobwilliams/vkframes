#version 450

// Lines (see line.vert): the style's color, with the dash pattern's gaps
// discarded; blended between its two sizes (2^n and 2^(n+1) scene units).

layout(set = 0, binding = 1) uniform Style {
    vec4 color;
    vec4 params;            // z = dash pattern (16 bits; 65535 or 0: solid)
} style;

layout(location = 0) in vec4 fragColor;
layout(location = 1) noperspective in vec3 dash;    // along the line, in periods 2^n and 2^(n+1);
                                                    // z = the weight of 2^(n+1)
layout(location = 0) out vec4 outColor;

void main()
{
    outColor = fragColor;
    uint pattern = uint(style.params.z) & 0xFFFFu;
    if (pattern != 0xFFFFu && pattern != 0u) {
        uvec2 bit = min(uvec2(floor(16.0 * fract(dash.xy))), uvec2(15u));
        float on0 = float((pattern >> bit.x) & 1u);
        float on1 = float((pattern >> bit.y) & 1u);
        float coverage = mix(on0, on1, smoothstep(0.3, 0.7, dash.z));   // crisp but near the middle
        if (coverage <= 0.0) discard;
        outColor.a *= coverage;
    }
}
