#version 450

// Lines on the sky at infinity (constellations): a screen-space quad (6
// vertices) per segment between two directions in the sky's frame (turned by
// the sky's attitude, then the view's rotation), of the style's width (window
// units) and an antialiasing pixel each side. A segment is clipped to in front
// of the eye (a great circle's arc projects to a straight line, so the
// segment's chord is exact); none in an orthographic view (as the stars).
// Drawn at the far plane (reverse-Z: depth 0) without writing depth, so the
// scene is in front.

layout(push_constant) uniform PushConstants {
    mat4 projection;
    mat4 modelView;         // with its translation zeroed (EyeRelative)
    vec4 eyeHigh;
    vec4 eyeLow;
    vec4 viewport;          // xy = size in framebuffer pixels, z = framebuffer pixels per window unit
} pc;

layout(std430, set = 0, binding = 0) readonly buffer Lines {
    vec4 p[];               // line i: p[2i] = (first direction, -), p[2i+1] = (second direction, -)
} lines;

layout(set = 0, binding = 1) uniform Sky {
    mat4 attitude;          // sky frame to the root frame
    vec4 params;            // x = width in window units
    vec4 color;
} sky;

layout(location = 0) out float across;          // framebuffer pixels from the line's center
layout(location = 1) flat out float halfWidth;  // the line's half width (framebuffer pixels)

out gl_PerVertex {
    vec4 gl_Position;
};

// per vertex: (0 the first end, 1 the second; side -1 or 1)
const vec2 corners[6] = vec2[](vec2(0.0, -1.0), vec2(1.0, -1.0), vec2(0.0, 1.0),
                               vec2(0.0, 1.0), vec2(1.0, -1.0), vec2(1.0, 1.0));

void main()
{
    int line = gl_VertexIndex / 6;
    vec2 corner = corners[gl_VertexIndex % 6];
    mat3 turn = mat3(pc.modelView) * mat3(sky.attitude);
    vec3 a = turn * lines.p[2 * line].xyz;
    vec3 b = turn * lines.p[2 * line + 1].xyz;
    const float near = 1.0e-3;                      // in front of the eye: z <= -near
    vec4 pa = vec4(0.0), pb = vec4(0.0);
    if (a.z <= -near || b.z <= -near)
    {
        if (a.z > -near) a = mix(a, b, (-near - a.z) / (b.z - a.z));
        if (b.z > -near) b = mix(b, a, (-near - b.z) / (a.z - b.z));
        pa = pc.projection * vec4(a, 0.0);          // directions: at infinity
        pb = pc.projection * vec4(b, 0.0);
    }
    if (pa.w <= 0.0 || pb.w <= 0.0)
    {
        gl_Position = vec4(0.0, 0.0, 0.0, -1.0);   // behind the eye (or an orthographic view)
        across = 0.0;
        halfWidth = 0.0;
        return;
    }
    vec2 halfSize = 0.5 * max(pc.viewport.xy, vec2(1.0));
    vec2 sa = pa.xy / pa.w;
    vec2 sb = pb.xy / pb.w;
    vec2 d = (sb - sa) * halfSize;                  // framebuffer pixels
    vec2 n = length(d) > 0.0 ? normalize(vec2(-d.y, d.x)) : vec2(0.0, 1.0);
    halfWidth = 0.5 * sky.params.x * pc.viewport.z;
    float extent = halfWidth + 1.0;                 // (a pixel more for the antialiased edge)
    across = corner.y * extent;
    vec2 s = corner.x < 0.5 ? sa : sb;
    gl_Position = vec4(s + n * across / halfSize, 0.0, 1.0);   // z 0: the far plane
}
