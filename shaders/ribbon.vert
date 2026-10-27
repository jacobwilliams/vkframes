#version 450

// Ribbons: a translucent surface through pairs of points (a start and an end
// at each trajectory point: e.g. the origin and the position, sweeping out an
// orbit's plane). Pair q is points 2q and 2q+1; the quad between pairs q and
// q+1 is two triangles (6 vertices), so n pairs are one draw of 6*(n-1)
// vertices, with no vertex buffers: the points come from the storage buffer,
// indexed by gl_VertexIndex. A quad whose two starts are the same point (a
// fan from the origin) has one empty triangle, so the surface is drawn once.
//
// The buffers are a line's (see line.vert): points as (high, low) float
// pairs, drawn relative to the eye (RTE) to keep double precision, so the
// ribbon's edge lies on the curve through the same points; the style's color,
// times each point's color if colored (style.count.y = 1), blended across
// each quad.

layout(push_constant) uniform PushConstants {
    mat4 projection;
    mat4 modelView;         // with its translation zeroed
    vec4 eyeHigh;           // eye position in the points' frame, (high, low) pair
    vec4 eyeLow;
    vec4 viewport;
    ivec4 lod;
} pc;

layout(std430, set = 0, binding = 0) readonly buffer Points {
    vec4 p[];               // point i: p[2i] = (xyz, unused) high parts, p[2i+1] low parts
} points;

layout(set = 0, binding = 1) uniform Style {
    vec4 color;
    vec4 params;
    vec4 count;             // x = points written, y = 1 for a color per point (colors), else 0
} style;

layout(std430, set = 0, binding = 3) readonly buffer Colors {
    vec4 c[];               // point i's color (if style.count.y = 1), times style.color
} colors;

layout(location = 0) out vec4 fragColor;

out gl_PerVertex {
    vec4 gl_Position;
};

// the quad's corners: 0 = pair q's start, 1 = its end, 2 = pair q+1's start, 3 = its end
const int corners[6] = int[](0, 1, 3, 0, 3, 2);

// Position of point i relative to the eye: (high + low) - (eyeHigh + eyeLow)
// in double-single arithmetic (as line.vert).
vec3 eyeRelative(int i)
{
    vec3 high = points.p[2 * i].xyz;
    vec3 low = points.p[2 * i + 1].xyz;
    precise vec3 t1 = low - pc.eyeLow.xyz;
    precise vec3 e = t1 - low;
    precise vec3 t2 = ((-pc.eyeLow.xyz - e) + (low - (t1 - e))) + high - pc.eyeHigh.xyz;
    precise vec3 diffHigh = t1 + t2;
    precise vec3 diffLow = t2 - (diffHigh - t1);
    precise vec3 r = diffHigh + diffLow;
    return r;
}

void main()
{
    int quad = gl_VertexIndex / 6;
    int i = 2 * quad + corners[gl_VertexIndex % 6];
    gl_Position = pc.projection * pc.modelView * vec4(eyeRelative(i), 1.0);
    fragColor = (style.count.y > 0.5) ? style.color * colors.c[i] : style.color;
}
