#version 450

// Sky texture along the view ray (see sky.vert). The texture's u wraps at
// right ascension 180 degrees: its derivatives are taken from whichever of u
// and u + 1/2 is continuous there, so the mipmap level has no seam.

layout(set = 0, binding = 2) uniform sampler2D image;

layout(location = 0) in vec3 ray;
layout(location = 0) out vec4 outColor;

const float PI = 3.1415926536;

void main()
{
    vec3 d = normalize(ray);
    vec2 uv = vec2(atan(d.y, d.x) / (2.0 * PI) + 0.5, 0.5 - asin(clamp(d.z, -1.0, 1.0)) / PI);
    vec2 uv2 = vec2(fract(uv.x + 0.5), uv.y);
    vec2 dx = dFdx(uv), dy = dFdy(uv);
    vec2 dx2 = dFdx(uv2), dy2 = dFdy(uv2);
    if (abs(dx2.x) < abs(dx.x)) dx = dx2;
    if (abs(dy2.x) < abs(dy.x)) dy = dy2;
    outColor = vec4(textureGrad(image, uv, dx, dy).rgb, 1.0);
}
