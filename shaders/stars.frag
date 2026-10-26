#version 450

// A star: its color, fading out from half its radius to its edge
// (OpenFrames' SkySphere fragment shader), premultiplied by that alpha.

layout(location = 0) in vec3 fragColor;
layout(location = 1) in vec2 pointCoord;
layout(location = 0) out vec4 outColor;

const float r_cutoff = 0.25;
const float x = -0.5 / (r_cutoff - 0.5);
const float y = 0.5 / (r_cutoff - 0.5);

void main()
{
    vec2 v = pointCoord - vec2(0.5);
    float alpha = clamp(mix(x, y, length(v)), 0.0, 1.0);
    if (alpha <= 0.0) discard;
    outColor = vec4(fragColor * alpha, alpha);
}
