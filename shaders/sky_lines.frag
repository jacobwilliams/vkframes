#version 450

// A line on the sky: its color, its edges antialiased over a pixel.

layout(set = 0, binding = 1) uniform Sky {
    mat4 attitude;
    vec4 params;
    vec4 color;
} sky;

layout(location = 0) in float across;
layout(location = 1) flat in float halfWidth;
layout(location = 0) out vec4 outColor;

void main()
{
    float alpha = sky.color.a * clamp(halfWidth + 0.5 - abs(across), 0.0, 1.0);
    if (alpha <= 0.0) discard;
    outColor = vec4(sky.color.rgb, alpha);
}
