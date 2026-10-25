#version 450

// Marker shapes, from OpenFrames' marker shaders (MarkerArtist.cpp's disk and
// image, Demos/Shaders/Marker_*.frag), selected by style.params.y:
//   0 disk, 1 circle, 2 square, 3 plus, 4 X, 5 target, 6 rose (4 petals),
//   7 pulsing circle, 8 pulsing disk, 9 rotating spiral (animated at 1 Hz),
//   10 image (binding 2; fragments with alpha < 0.05 discarded)
// pointCoord is gl_PointCoord's equivalent (0 to 1 across the marker, y down).

layout(set = 0, binding = 1) uniform Style {
    vec4 color;
    vec4 params;
} style;

layout(set = 0, binding = 2) uniform sampler2D image;

layout(location = 0) in vec4 fragColor;
layout(location = 1) in vec2 pointCoord;
layout(location = 2) in float time;
layout(location = 0) out vec4 outColor;

const float PI = 3.1415926536;
const float TWO_PI = 6.2831853072;

void main()
{
    vec2 v = pointCoord - vec2(0.5);
    float dist2 = dot(v, v);
    vec4 color = fragColor;
    int shape = int(style.params.y + 0.5);

    if (shape == 1) {                              // circle
        if (dist2 > 0.25 || dist2 < 0.16) discard;
    } else if (shape == 2) {                       // square
        if (pointCoord.x > 0.1 && pointCoord.x < 0.9 && pointCoord.y > 0.1 && pointCoord.y < 0.9) discard;
    } else if (shape == 3) {                       // plus
        vec2 d = abs(v);
        if (d.x > 0.1 && d.y > 0.1) discard;
    } else if (shape == 4) {                       // X
        vec2 d = vec2(abs(pointCoord.y - pointCoord.x), abs(pointCoord.y + pointCoord.x - 1.0));
        if (d.x > 0.1 && d.y > 0.1) discard;
    } else if (shape == 5) {                       // target
        if (dist2 > 0.25 || (dist2 < 0.16 && dist2 > 0.04)) discard;
    } else if (shape == 6) {                       // rose: r = |sin(theta) cos(theta)|
        float r = length(v);
        if (r > 0.001 && r - abs(v.x * v.y) / (r * r) > 0.0) discard;
    } else if (shape == 7) {                       // pulsing circle
        float rad2 = 0.0625 + (0.25 - 0.0625) * abs(sin(PI * time));
        if (dist2 > rad2 || dist2 < rad2 - 0.09) discard;
    } else if (shape == 8) {                       // pulsing disk
        float d = length(v);
        if (d > 0.5) discard;
        d = mod(d - time, 0.5);
        color.a *= smoothstep(0.0, 0.25, d) - smoothstep(0.25, 0.5, d);
    } else if (shape == 9) {                       // rotating spiral
        const float FOUR_PI_INV = 0.07957747154;
        const float R_EPS = 0.1;
        float d = length(v);
        float theta1 = mod(atan(v.y, v.x) + TWO_PI * time, TWO_PI);
        float r1 = theta1 * FOUR_PI_INV * (1.0 - R_EPS);
        float theta2 = mod(theta1 + PI, TWO_PI);
        float r2 = theta2 * FOUR_PI_INV * (1.0 - R_EPS);
        float arm1 = smoothstep(r1 - R_EPS, r1, d) - smoothstep(r1, r1 + R_EPS, d);
        float arm2 = smoothstep(r2 - R_EPS, r2, d) - smoothstep(r2, r2 + R_EPS, d);
        color.a *= max(arm1, arm2);
    } else if (shape == 10) {                      // image
        vec4 t = texture(image, pointCoord);
        if (t.a < 0.05) discard;
        color *= t;
    } else {                                       // disk
        if (dist2 > 0.25) discard;
    }
    if (color.a <= 0.0) discard;
    outColor = color;
}
