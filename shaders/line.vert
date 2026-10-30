#version 450

// the variant: plain (VKF_LINE_PLAIN), dashed (VKF_LINE_DASHED) or full
#if !defined(VKF_LINE_PLAIN) && !defined(VKF_LINE_DASHED)
#define VKF_LINE_FULL
#endif

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
// scene unit) per segment; or, as given per line in pc.dashLevels (see
// dash_levels in src/vsg/vkf_layer.cpp: one size for the whole line, so its
// dashes all have one length, morphing over a fraction of a second when it
// changes), the old and the new size and the morph's progress). line.frag
// blends the two by fract(lambda) (from
// 0.3 to 0.7: one size only most of the time), blending each pixel's
// distance to the nearest dash edge rather than its coverage, so the dashes
// stay solid while they change size, smoothly along the line and while
// zooming, from 0.7 to 1.4 times the pattern's length on screen. The position comes from each point's distance
// along the strip (from its start; written with the points by Fortran, as a
// high/low pair), so the dashes run on across the joins, and fract(s / 2^n)
// of a float s is exact: they stay precise however far along a long
// trajectory. Separate segments start their pattern at their first point, as
// GL_LINES does. Fixed-length dashes (pc.dashLevels.w = 2: fixed_dashes)
// instead have one period per line in scene units that never changes, as
// paint on the line (zooming in, they grow on screen), taken about a pivot
// near the view's center for precision.
//
// Level of detail (strips): the strip may be drawn at a stride (pc.lod.x)
// from its first point drawn (pc.lod.z, 0-based: a trace of decreasing
// times starts part-way) to its last (pc.lod.y points): segment 0 joins the
// first point and the next multiple of the stride, segment i the multiples
// after (the points the bounds are computed for), the last ending at the
// last point drawn, so that it always starts and ends there. EyeRelative (in
// src/vsg/vkf_layer.cpp) picks the stride per view, from the bounds Fortran
// computes of how far the points skipped are from the segments drawn.
//
// Highlight (style.flow.x: flags 1 pulse, 2 arrows; e.g. a selected
// trajectory): drawn by the GPU from the time pushed (pc.viewport.w), so
// nothing is uploaded each frame. Pulse: the line widens (up to 1.5 times)
// and its color turns towards the highlight color, and back, every
// style.pulse.x seconds. Arrows: the quad is drawn as wide as the band
// (style.flow.w window units, if wider than the line), and line.frag draws a
// translucent glow in it and chevrons pointing along the line, from its first
// point to its last (a trajectory's initial time to its final), every
// style.flow.y window units, moving style.flow.z window units per second.
// Their positions come from the same distance along the line as the dashes',
// in periods of the spacing (two sizes, blended), less the distance moved
// (in periods, the same for both, so that they meet where the sizes change),
// so they run on across the joins; their spacing and speed on screen stay
// within 0.7 to 1.4 times those given.
//
// Screen-locked dashes (style.count.z = 1; vkf_line_set_dash_mode): as
// OpenGL's line stipple, a constant length in pixels, following the line on
// screen (crawling along it as the view changes): the position is each
// segment's first point's distance along the strip on screen (computed per
// view and frame on the CPU: ScreenArcs in src/vsg/vkf_layer.cpp, set 1) plus
// the pixels along the segment, in pattern periods (one size: no blend).
// Separate segments start their pattern at each segment, as GL_LINES does.
//
// Trails (style.trail0.z = 1 or -1: a fading trail): the part of a strip
// within a time window about the current time, in two styles: the past
// (before the current time) and the future. Fortran gives the distance along
// the strip at the current time (trail0.xy, a high/low pair) and, as
// distances from it in the direction of time (trail0.z: the sign of the
// distance's change with time), the past's cutoff and where its fade starts,
// where the future's fade starts and its cutoff (trail1). Each vertex gets
// its distance from the current time's (trailDistance: exact far along the
// strip, from the high/low pairs); line.frag fades and cuts each pixel by it
// and draws it in the past's or the future's color and dash pattern (trail2
// to trail4; the future's dashes from dash2, for its own factor).
//
// Colors: the style's, or (lines made by vkf_line_create_colored) each
// point's times the style's, blended along each segment from its first
// point's to its second's (OpenFrames' CustomLineSegments).
//
// Variants: plain lines (compiled with VKF_LINE_PLAIN: vkf_shaders::
// line_plain_vert, with line_plain_frag): no dashes, highlight or trail, so
// only the color is passed to line.frag; dashed lines (VKF_LINE_DASHED:
// line_dashed_vert and _frag): dashes (any layout) but no highlight or trail,
// the color and `dash` passed; the full shaders for the rest (highlights,
// trails). The C++ layer draws a line with the leanest its style allows
// (EyeRelative, line_variant): with fewer outputs and registers dense lines
// draw about twice as fast on Apple's GPU (measured: 1 million points, every
// one drawn, solid: 108 against 219 frames per second).
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
    ivec4 lod;              // x = stride (strips), y = points drawn (up to), z = the first drawn (0-based),
                            // w = the pass: 0 the line, 1 a highlighted line's glow and arrows (see line.frag)
    uvec4 dashLevels;       // the sizes per line (w = 1: given; else per segment) of the dashes (x), the
                            // trail's future's (y) and the arrows (z): the old size + 128, the new + 128
                            // (8 bits each), the morph's progress (16 bits); w = 2: fixed length, as
    vec4 fixedDash0;        // the pivot's distance along the strip (high, low), the dashes' phase and
    vec4 fixedDash1;        // periods per scene unit; the future's, the arrows' (see fixed_dashes)
} pc;

layout(std430, set = 0, binding = 0) readonly buffer Points {
    vec4 p[];               // point i: p[2i] = (xyz, distance along the strip) high parts, p[2i+1] low parts
} points;

layout(set = 0, binding = 1) uniform Style {
    vec4 color;
    vec4 params;            // x = width in window units (logical pixels), y = 1 for separate segments,
                            // z = dash pattern (16 bits; 65535 or 0: solid), w = window units per bit
    vec4 count;             // x = points written in the buffer (set by vkf_line_points_changed),
                            // y = 1 for a color per point (colors), else 0, z = 1 for screen-locked dashes
    vec4 highlight;         // highlight color (rgba)
    vec4 flow;              // x = highlight flags (1 pulse, 2 arrows), y = arrow spacing (window units),
                            // z = arrow speed (window units per second), w = band width (window units)
    vec4 pulse;             // x = pulse period (s)
    vec4 trail0;            // x, y = the distance at the current time (high, low), z = direction (0: no trail),
                            // w = color flags (1: the past's color replaces the line's, 2: the future's)
    vec4 trail1;            // the past's cutoff, its fade's start, the future's fade's start, its cutoff
    vec4 trail2;            // the past's color (rgba)
    vec4 trail3;            // the future's color (rgba)
    vec4 trail4;            // the past's dash pattern and factor, the future's
} style;

layout(std430, set = 1, binding = 0) readonly buffer ScreenArcs {
    float arc[];            // (screen-locked dashes) point i's distance along the strip on screen (pixels)
} screen;

layout(std430, set = 0, binding = 3) readonly buffer Colors {
    vec4 c[];               // point i's color (if style.count.y = 1: else one, white), times style.color
} colors;

layout(location = 0) out vec4 fragColor;
#ifndef VKF_LINE_PLAIN
layout(location = 1) noperspective out vec3 dash;   // along the line, in periods 2^n and 2^(n+1);
                                                    // z = the weight of 2^(n+1)
#endif
#ifdef VKF_LINE_FULL
layout(location = 2) noperspective out vec3 flow;   // as dash, for the arrows (periods of their spacing), moved
layout(location = 3) flat out vec2 flowPixels;      // pixels per period, of each size
layout(location = 4) noperspective out float across;    // -1 to 1 across the quad
layout(location = 5) flat out vec4 highlight;       // x = pulse (0 to 1), y = the line's half width,
                                                    // z = the quad's (pixels), w = highlight flags
layout(location = 6) out float trailDistance;       // from the current time's, in the direction of time
layout(location = 7) noperspective out vec3 dash2;  // (trails) the future's dashes, as dash
layout(location = 8) flat out int pass;              // pc.lod.w
#endif

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
// or a mitre); screenLength is the drawn part's length in pixels, period the
// pattern's (pixels), level its sizes per line (packed: see pc.dashLevels; used
// if pc.dashLevels.w = 1), fixedDash its phase and periods per scene unit
// (used if pc.dashLevels.w = 2: d = phase + (s - pivot) * perUnit, one size).
// perPixel: periods per pixel along the segment.
vec3 dashPosition(int first, int second, float end, float ta, float tb, float screenLength, float along,
                  float period, uint level, vec2 fixedDash, out vec2 perPixel)
{
    vec2 sa = vec2(0.0), sb;                       // distances along the strip (high, low)
    if (style.params.y > 0.5) {
        sb = vec2(length(eyeRelative(second) - eyeRelative(first)), 0.0);
    } else {
        sa = vec2(points.p[2 * first].w, points.p[2 * first + 1].w);
        sb = vec2(points.p[2 * second].w, points.p[2 * second + 1].w);
    }
    float sceneLength = (sb.x - sa.x) + (sb.y - sa.y);
    if (pc.dashLevels.w == 2u) {                                       // a fixed length, about the pivot
        float from = (sa.x - pc.fixedDash0.x) + (sa.y - pc.fixedDash0.y);
        perPixel = vec2((screenLength > 1.0e-6) ? (tb - ta) * sceneLength * fixedDash.y / screenLength : 0.0);
        float d = fixedDash.x + (from + ((end == 0.0) ? ta : tb) * sceneLength) * fixedDash.y + along * perPixel.x;
        return vec3(d, d, 0.0);
    }
    float drawn = (tb - ta) * sceneLength;
    // lambda: log2 of the period's length here in scene units (large if the segment shows no length)
    float lambda = 126.0;
    if (drawn > 0.0 && screenLength > 1.0e-6)
        lambda = clamp(log2(period * drawn / screenLength), -125.0, 126.0);
    float n = floor(lambda);
    vec2 scale = vec2(exp2(-n), exp2(-n - 1.0));
    float weight = lambda - n;
    if (pc.dashLevels.w == 1u) {                                       // one size per line: old to new
        scale = vec2(exp2(-float(int(level & 0xFFu) - 128)), exp2(-float(int((level >> 8) & 0xFFu) - 128)));
        weight = float(level >> 16) / 65535.0;
    }
    vec2 start = fract(sa.x * scale) + fract(sa.y * scale);            // exact: powers of two
    vec2 span = sceneLength * scale;                                   // the whole segment, in periods
    perPixel = (screenLength > 1.0e-6) ? (tb - ta) * span / screenLength : vec2(0.0);
    vec2 d = ((end == 0.0) ? start + ta * span : start + tb * span) + along * perPixel;
    return vec3(d, weight);
}

void main()
{
    int segment = gl_VertexIndex / 6;
    vec2 corner = corners[gl_VertexIndex % 6];

    mat4 mvp = pc.projection * pc.modelView;
    bool separate = style.params.y > 0.5;
    int stride = separate ? 1 : max(pc.lod.x, 1);
    int last = (pc.lod.y > 0) ? pc.lod.y - 1 : 0x7fffffff;  // the last point drawn
    int from = separate ? 0 : pc.lod.z;                     // the first point drawn
    int q = from / stride;
    int first = separate ? 2 * segment : ((segment == 0) ? from : (q + segment) * stride);
    int second = separate ? first + 1 : min((q + segment + 1) * stride, last);
    vec4 a = mvp * vec4(eyeRelative(first), 1.0);
    vec4 b = mvp * vec4(eyeRelative(second), 1.0);

    // clip the segment to the part in front of the eye (w > 0); a separate
    // segment of no length (its two points the same: e.g. a thrust vector
    // where there is no thrust) draws nothing, not a cap's square
    bool empty = false;
    if (separate)                                  // (the style's, for all vertices: a strip reads nothing more)
        empty = points.p[2 * first] == points.p[2 * second] && points.p[2 * first + 1] == points.p[2 * second + 1];
    if ((a.w < W_MIN && b.w < W_MIN) || empty) {
        gl_Position = vec4(0.0, 0.0, 0.0, -1.0);   // whole segment behind: outside the clip volume
        fragColor = vec4(0.0);
#ifndef VKF_LINE_PLAIN
        dash = vec3(0.0);
#endif
#ifdef VKF_LINE_FULL
        flow = vec3(0.0);
        flowPixels = vec2(0.0);
        across = 0.0;
        highlight = vec4(0.0);
        trailDistance = 0.0;
        dash2 = vec3(0.0);
        pass = 0;
#endif
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

    // highlight: the pulse (0 to 1), the line's half width, the quad's (the band's if wider)
#ifndef VKF_LINE_FULL
    const int flags = 0;                           // (plain, dashed: no highlight)
    const float pulse = 0.0;
#else
    float time = pc.viewport.w;
    int flags = int(style.flow.x + 0.5);
    float pulse = ((flags & 1) != 0) ? 0.5 - 0.5 * cos(6.2831853 * time / max(style.pulse.x, 0.05)) : 0.0;
#endif
    float lineHalfWidth = 0.5 * style.params.x * pc.viewport.z * (1.0 + 0.5 * pulse);
    float halfWidth = ((flags & 2) != 0) ? max(lineHalfWidth, 0.5 * style.flow.w * pc.viewport.z) : lineHalfWidth;
    bool atEnd = corner.x == 1.0;
    vec4 pos = atEnd ? b : a;
    // the join at this end: a square cap, or (strips) a mitre with the neighbouring segment
    vec2 offset = halfWidth * (corner.y * normal + (2.0 * corner.x - 1.0) * dir);
    float along = atEnd ? halfWidth : -halfWidth;      // (pixels beyond the end point, along the segment)
    // (at the last point drawn, the next one: trace mode's tail continues there)
    int previous = (segment == 0) ? -1 : ((segment == 1) ? from : first - stride);  // (none before the first drawn)
    int neighbour = atEnd ? ((second >= last) ? second + 1 : min(second + stride, last)) : previous;
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
#ifndef VKF_LINE_PLAIN
    vec2 perPixel;
    dash = vec3(0.0);
#ifdef VKF_LINE_FULL
    pass = pc.lod.w;
    fragColor.rgb = mix(fragColor.rgb, style.highlight.rgb, 0.5 * pulse);
    dash2 = vec3(0.0);
    trailDistance = 0.0;
    bool trail = style.trail0.z != 0.0 && !separate;
#else
    const bool trail = false;                      // (dashed: no trail)
#endif
    uint pattern = uint(trail ? style.trail4.x : style.params.z) & 0xFFFFu;
    float factor = trail ? style.trail4.y : style.params.w;
    bool screenDashes = style.count.z > 0.5;
    // (screen-locked: pixels along the strip on screen at this vertex; the
    // buffer read only then, and guarded: the dummy set's is short)
    float pixels = 0.0;
    if (screenDashes)
        pixels = ((separate || first >= screen.arc.length()) ? 0.0 : screen.arc[first]) + (atEnd ? len : 0.0) + along;
    if (pattern != 0xFFFFu && pattern != 0u) {
        float period = 16.0 * max(factor, 1.0e-3) * pc.viewport.z;
        if (screenDashes)
            dash = vec3(vec2(pixels / period), 0.0);
        else
            dash = dashPosition(first, second, corner.x, ta, tb, len, along, period, pc.dashLevels.x, pc.fixedDash0.zw, perPixel);
    }
#ifdef VKF_LINE_FULL
    if (trail) {
        uint pattern2 = uint(style.trail4.z) & 0xFFFFu;
        if (pattern2 != 0xFFFFu && pattern2 != 0u) {
            float period2 = 16.0 * max(style.trail4.w, 1.0e-3) * pc.viewport.z;
            if (screenDashes)
                dash2 = vec3(vec2(pixels / period2), 0.0);
            else
                dash2 = dashPosition(first, second, corner.x, ta, tb, len, along, period2, pc.dashLevels.y, pc.fixedDash1.xy, perPixel);
        }
        // the distance from the current time's at this end of the drawn part (high and low parts apart: exact)
        float s1 = (points.p[2 * first].w - style.trail0.x) + (points.p[2 * first + 1].w - style.trail0.y);
        float s2 = (points.p[2 * second].w - style.trail0.x) + (points.p[2 * second + 1].w - style.trail0.y);
        trailDistance = style.trail0.z * mix(s1, s2, atEnd ? tb : ta);
    }
    flow = vec3(0.0);
    flowPixels = vec2(0.0);
    if ((flags & 2) != 0) {
        float spacing = max(style.flow.y, 1.0) * pc.viewport.z;                   // pixels
        flow = dashPosition(first, second, corner.x, ta, tb, len, along, spacing, pc.dashLevels.z, pc.fixedDash1.zw, perPixel);
        flow.xy -= fract(time * style.flow.z * pc.viewport.z / spacing);           // (periods moved)
        flowPixels = vec2(perPixel.x > 0.0 ? 1.0 / perPixel.x : 1.0e9, perPixel.y > 0.0 ? 1.0 / perPixel.y : 1.0e9);
    }
    across = corner.y;
    highlight = vec4(pulse, lineHalfWidth, halfWidth, float(flags));
#endif
#endif
}
