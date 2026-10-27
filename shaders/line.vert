#version 450

// Thick lines: each segment is drawn as a screen-space quad (two triangles, 6
// vertices) of the style's width. A strip joins points i and i+1; with
// separate segments (style.params.y = 1) segment i joins points 2i and 2i+1.
// There are no vertex buffers: the points come from a storage buffer indexed
// by gl_VertexIndex, so a strip of n points is one draw of 6*(n-1) vertices,
// and n/2 separate segments one of 3*n vertices.
// Joins: in a strip, consecutive segments meet in a mitre (both quads end on
// the line bisecting the turn), so that they neither leave a gap nor overlap
// (overlapping caps would draw the many short segments of a dense trajectory
// about twice). Where there is no neighbouring point (a strip's ends, the ends
// of a buffer, separate segments), where the segment is clipped at the eye, or
// where the turn is sharp (over 120 degrees: the mitre would be longer than
// twice the half width), the segment is extended by half the width instead
// (a square cap), which covers the join. A neighbour is used only if it was
// written (style.count.x points in the buffer).
//
// Dash patterns (OpenGL's line stipple: 16 bits, each covering `factor`
// window units, bit 0 first) are drawn by line.frag from `dash`, the position
// along the line in pattern periods of two sizes: powers of two in scene
// units, 2^n and 2^(n+1), the nearest below and above the pattern's length on
// screen there (n = floor(lambda), lambda = log2(pattern length / pixels per
// scene unit) per segment). line.frag blends the two by fract(lambda) (only
// from 0.3 to 0.7, so that the dashes are mostly crisp), so they change size
// smoothly along the line and while zooming, from 0.7 to 1.4 times the
// pattern's length on screen. The position comes from each point's distance
// along the strip (from its start; written with the points by Fortran, as a
// high/low pair), so the dashes run on across the joins, and fract(s / 2^n)
// of a float s is exact: they stay precise however far along a long
// trajectory. Separate segments start their pattern at their first point, as
// GL_LINES does.
//
// Level of detail (strips): the strip may be drawn at a stride (pc.lod.x):
// segment i joins points i*stride and the next multiple, or the last point
// drawn (pc.lod.y points), so that it always ends there. EyeRelative (in
// src/vsg/vkf_layer.cpp) picks the stride per view, from the bounds Fortran
// computes of how far the points skipped are from the segments drawn.
//
// Colors: the style's, or (lines made by vkf_line_create_colored) each
// point's times the style's, blended along each segment from its first
// point's to its second's (OpenFrames' CustomLineSegments).
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
    ivec4 lod;              // x = stride (strips), y = points drawn
} pc;

layout(std430, set = 0, binding = 0) readonly buffer Points {
    vec4 p[];               // point i: p[2i] = (xyz, distance along the strip) high parts, p[2i+1] low parts
} points;

layout(set = 0, binding = 1) uniform Style {
    vec4 color;
    vec4 params;            // x = width in window units (logical pixels), y = 1 for separate segments,
                            // z = dash pattern (16 bits; 65535 or 0: solid), w = window units per bit
    vec4 count;             // x = points written in the buffer (set by vkf_line_points_changed),
                            // y = 1 for a color per point (colors), else 0
} style;

layout(std430, set = 0, binding = 3) readonly buffer Colors {
    vec4 c[];               // point i's color (if style.count.y = 1: else one, white), times style.color
} colors;

layout(location = 0) out vec4 fragColor;
layout(location = 1) noperspective out vec3 dash;   // along the line, in periods 2^n and 2^(n+1);
                                                    // z = the weight of 2^(n+1)

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
    vec3 high = points.p[2 * i].xyz;
    vec3 low = points.p[2 * i + 1].xyz;

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

// The dash positions (in periods 2^n and 2^(n+1)) and the weight of the
// second at a vertex of the segment from point `first` to `second`: at its
// drawn start (ta) or end (tb), moved `along` pixels along the segment (a cap
// or a mitre); screenLength is the drawn part's length in pixels.
vec3 dashPosition(int first, int second, float end, float ta, float tb, float screenLength, float along)
{
    vec2 sa = vec2(0.0), sb;                       // distances along the strip (high, low)
    if (style.params.y > 0.5) {
        sb = vec2(length(eyeRelative(second) - eyeRelative(first)), 0.0);
    } else {
        sa = vec2(points.p[2 * first].w, points.p[2 * first + 1].w);
        sb = vec2(points.p[2 * second].w, points.p[2 * second + 1].w);
    }
    float sceneLength = (sb.x - sa.x) + (sb.y - sa.y);
    float drawn = (tb - ta) * sceneLength;
    float period = 16.0 * max(style.params.w, 1.0e-3) * pc.viewport.z;    // pixels
    // lambda: log2 of the period's length here in scene units (large if the segment shows no length)
    float lambda = 126.0;
    if (drawn > 0.0 && screenLength > 1.0e-6)
        lambda = clamp(log2(period * drawn / screenLength), -125.0, 126.0);
    float n = floor(lambda);
    vec2 scale = vec2(exp2(-n), exp2(-n - 1.0));
    vec2 start = fract(sa.x * scale) + fract(sa.y * scale);            // exact: powers of two
    vec2 span = sceneLength * scale;                                   // the whole segment, in periods
    vec2 perPixel = (screenLength > 1.0e-6) ? (tb - ta) * span / screenLength : vec2(0.0);
    vec2 d = ((end == 0.0) ? start + ta * span : start + tb * span) + along * perPixel;
    return vec3(d, lambda - n);
}

void main()
{
    int segment = gl_VertexIndex / 6;
    vec2 corner = corners[gl_VertexIndex % 6];

    mat4 mvp = pc.projection * pc.modelView;
    bool separate = style.params.y > 0.5;
    int stride = separate ? 1 : max(pc.lod.x, 1);
    int last = (pc.lod.y > 0) ? pc.lod.y - 1 : 0x7fffffff;  // the last point drawn
    int first = separate ? 2 * segment : segment * stride;
    int second = separate ? first + 1 : min(first + stride, last);
    vec4 a = mvp * vec4(eyeRelative(first), 1.0);
    vec4 b = mvp * vec4(eyeRelative(second), 1.0);

    // clip the segment to the part in front of the eye (w > 0); a separate
    // segment of no length (its two points the same: e.g. a thrust vector
    // where there is no thrust) draws nothing, not a cap's square
    bool empty = separate && points.p[2 * first] == points.p[2 * second] && points.p[2 * first + 1] == points.p[2 * second + 1];
    if ((a.w < W_MIN && b.w < W_MIN) || empty) {
        gl_Position = vec4(0.0, 0.0, 0.0, -1.0);   // whole segment behind: outside the clip volume
        fragColor = vec4(0.0);
        dash = vec3(0.0);
        return;
    }
    float ta = 0.0, tb = 1.0;                      // the part drawn, as fractions of the segment
    if (a.w < W_MIN) { ta = (W_MIN - a.w) / (b.w - a.w); a = mix(a, b, ta); }
    if (b.w < W_MIN) { tb = 1.0 - (W_MIN - b.w) / (a.w - b.w); b = mix(b, a, 1.0 - tb); }
    // (the clip coordinates are linear in the scene's, so ta and tb are fractions of its length too)

    // direction and normal of the segment in pixels
    vec2 halfSize = 0.5 * pc.viewport.xy;
    vec2 sa = (a.xy / a.w) * halfSize;
    vec2 sb = (b.xy / b.w) * halfSize;
    vec2 d = sb - sa;
    float len = length(d);
    vec2 dir = (len > 1.0e-6) ? d / len : vec2(1.0, 0.0);
    vec2 normal = vec2(-dir.y, dir.x);

    float halfWidth = 0.5 * style.params.x * pc.viewport.z;
    bool atEnd = corner.x == 1.0;
    vec4 pos = atEnd ? b : a;
    // the join at this end: a square cap, or (strips) a mitre with the neighbouring segment
    vec2 offset = halfWidth * (corner.y * normal + (2.0 * corner.x - 1.0) * dir);
    float along = atEnd ? halfWidth : -halfWidth;      // (pixels beyond the end point, along the segment)
    // (at the last point drawn, the next one: trace mode's tail continues there)
    int neighbour = atEnd ? ((second >= last) ? second + 1 : min(second + stride, last)) : first - stride;
    bool clipped = atEnd ? (tb < 1.0) : (ta > 0.0);
    if (!separate && neighbour >= 0 && neighbour < int(style.count.x) && !clipped && len > 1.0e-6) {
        vec4 c = mvp * vec4(eyeRelative(neighbour), 1.0);
        if (c.w >= W_MIN) {
            vec2 sc = (c.xy / c.w) * halfSize;
            vec2 e = atEnd ? sc - sb : sa - sc;        // the neighbouring segment, in the line's direction
            vec2 t = dir + e / max(length(e), 1.0e-6);
            if (length(e) > 1.0e-6 && length(t) > 1.0e-3) {
                t = normalize(t);                      // along the bisector of the turn
                vec2 m = vec2(-t.y, t.x);
                float k = dot(m, normal);              // cos(half the turn)
                if (k > 0.5) {                         // (turn under 120 degrees)
                    offset = corner.y * (halfWidth / k) * m;
                    along = dot(offset, dir);
                }
            }
        }
    }
    pos.xy += (offset / halfSize) * pos.w;

    gl_Position = pos;
    fragColor = (style.count.y > 0.5) ? style.color * colors.c[atEnd ? second : first] : style.color;
    dash = vec3(0.0);
    uint pattern = uint(style.params.z) & 0xFFFFu;
    if (pattern != 0xFFFFu && pattern != 0u)
        dash = dashPosition(first, second, corner.x, ta, tb, len, along);
}
