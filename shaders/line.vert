#version 450

// Thick lines: each segment is drawn as a screen-space quad (two triangles, 6
// vertices) of the style's width. A strip joins points i and i+1; with
// separate segments (style.params.y = 1) segment i joins points 2i and 2i+1.
// There are no vertex buffers: the points come from a storage buffer indexed
// by gl_VertexIndex, so a strip of n points is one draw of 6*(n-1) vertices,
// and n/2 separate segments one of 3*n vertices.
// Segments are extended by half the width at each end (square caps), which
// covers the gaps at the joins.
//
// Points are drawn relative to the eye (RTE) to keep double precision far from
// the origin (see EyeRelative in src/vsg/vkf_layer.cpp): each point is a (high,
// low) float pair, the eye position in the same frame is pushed as a pair, and
// the model-view matrix has its translation zeroed. The subtraction is
// OpenFrames' (TrajectoryArtist.cpp), a double-single difference that carries
// the rounding error of each step.

layout(push_constant) uniform PushConstants {
    mat4 projection;
    mat4 modelView;         // with its translation zeroed
    vec4 eyeHigh;           // eye position in the points' frame, (high, low) pair
    vec4 eyeLow;
    vec4 viewport;          // xy = size of the view in framebuffer pixels,
                            // z = framebuffer pixels per window unit (2 on Retina)
} pc;

layout(std430, set = 0, binding = 0) readonly buffer Points {
    float p[];              // point i: p[6i .. 6i+2] = xyz high parts, p[6i+3 .. 6i+5] = xyz low parts
} points;

layout(set = 0, binding = 1) uniform Style {
    vec4 color;
    vec4 params;            // x = width in window units (logical pixels), y = 1 for separate segments
} style;

layout(location = 0) out vec4 fragColor;

out gl_PerVertex {
    vec4 gl_Position;
};

// (along the segment: 0 = start, 1 = end; across: -1 or +1)
const vec2 corners[6] = vec2[](vec2(0.0, -1.0), vec2(1.0, -1.0), vec2(0.0, 1.0),
                               vec2(0.0, 1.0), vec2(1.0, -1.0), vec2(1.0, 1.0));

const float W_MIN = 1.0e-6;

// Position of point i relative to the eye: (high + low) - (eyeHigh + eyeLow)
// in double-single arithmetic.
vec3 eyeRelative(int i)
{
    int k = 6 * i;
    vec3 high = vec3(points.p[k], points.p[k + 1], points.p[k + 2]);
    vec3 low = vec3(points.p[k + 3], points.p[k + 4], points.p[k + 5]);

    // low part of point - eye, and its rounding error
    precise vec3 t1 = low - pc.eyeLow.xyz;
    precise vec3 e = t1 - low;

    // high part of point - eye, including the error of the low part
    precise vec3 t2 = ((-pc.eyeLow.xyz - e) + (low - (t1 - e))) + high - pc.eyeHigh.xyz;

    // sum of the low and high parts, and its rounding error
    precise vec3 diffHigh = t1 + t2;
    precise vec3 diffLow = t2 - (diffHigh - t1);
    precise vec3 r = diffHigh + diffLow;
    return r;
}

void main()
{
    int segment = gl_VertexIndex / 6;
    vec2 corner = corners[gl_VertexIndex % 6];

    mat4 mvp = pc.projection * pc.modelView;
    int first = (style.params.y > 0.5) ? 2 * segment : segment;
    vec4 a = mvp * vec4(eyeRelative(first), 1.0);
    vec4 b = mvp * vec4(eyeRelative(first + 1), 1.0);

    // clip the segment to the part in front of the eye (w > 0)
    if (a.w < W_MIN && b.w < W_MIN) {
        gl_Position = vec4(0.0, 0.0, 0.0, -1.0);   // whole segment behind: outside the clip volume
        fragColor = vec4(0.0);
        return;
    }
    if (a.w < W_MIN) a = mix(a, b, (W_MIN - a.w) / (b.w - a.w));
    if (b.w < W_MIN) b = mix(b, a, (W_MIN - b.w) / (a.w - b.w));

    // direction and normal of the segment in pixels
    vec2 halfSize = 0.5 * pc.viewport.xy;
    vec2 sa = (a.xy / a.w) * halfSize;
    vec2 sb = (b.xy / b.w) * halfSize;
    vec2 d = sb - sa;
    float len = length(d);
    vec2 dir = (len > 1.0e-6) ? d / len : vec2(1.0, 0.0);
    vec2 normal = vec2(-dir.y, dir.x);

    float halfWidth = 0.5 * style.params.x * pc.viewport.z;
    vec4 pos = (corner.x == 0.0) ? a : b;
    vec2 offset = halfWidth * (corner.y * normal + (2.0 * corner.x - 1.0) * dir);
    pos.xy += (offset / halfSize) * pos.w;

    gl_Position = pos;
    fragColor = style.color;
}
