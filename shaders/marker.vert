#version 450

// Markers: a screen-space square (two triangles, 6 vertices) centered on each
// point, of the style's size, drawn by marker.frag in the style's shape (as
// OpenFrames' point sprites, but not limited by the device's point sizes).
// The points come from a storage buffer indexed by gl_VertexIndex (as in
// line.vert: n markers are one draw of 6*n vertices), drawn relative to the
// eye for double precision (see line.vert).
//
// Colors: the style's, or (markers made colored, style.params.w = 1) each
// marker's times the style's.
//
// Auto-attenuation (OpenFrames' MarkerArtist::computeAttenuation): with a
// radius R > 0, a marker at eye distance d is drawn at size * min(1, sqrt(R/d)),
// so markers shrink as the camera moves away beyond R (the size of the
// trajectory) and never grow past their size.

layout(push_constant) uniform PushConstants {
    mat4 projection;
    mat4 modelView;         // with its translation zeroed
    vec4 eyeHigh;           // eye position in the points' frame, (high, low) pair
    vec4 eyeLow;
    vec4 viewport;          // xy = size of the view in framebuffer pixels,
                            // z = framebuffer pixels per window unit, w = time (s)
} pc;

layout(std430, set = 0, binding = 0) readonly buffer Points {
    vec4 p[];               // point i: p[2i].xyz = high parts, p[2i+1].xyz = low parts (w: see line.vert)
} points;

layout(set = 0, binding = 1) uniform Style {
    vec4 color;
    vec4 params;            // x = size in window units, y = shape (marker.frag),
                            // z = attenuation radius R (0: none), w = 1 for a color per marker
} style;

layout(std430, set = 0, binding = 3) readonly buffer Colors {
    vec4 c[];               // marker i's color (if style.params.w = 1: else one, white)
} colors;

layout(location = 0) out vec4 fragColor;
layout(location = 1) out vec2 pointCoord;   // as gl_PointCoord: 0 to 1, y down
layout(location = 2) out float time;

out gl_PerVertex {
    vec4 gl_Position;
};

const vec2 corners[6] = vec2[](vec2(-1.0, -1.0), vec2(1.0, -1.0), vec2(-1.0, 1.0),
                               vec2(-1.0, 1.0), vec2(1.0, -1.0), vec2(1.0, 1.0));

// Position of point i relative to the eye, in double-single arithmetic (see line.vert).
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
    int marker = gl_VertexIndex / 6;
    vec2 corner = corners[gl_VertexIndex % 6];

    vec3 rel = eyeRelative(marker);
    vec4 pos = pc.projection * pc.modelView * vec4(rel, 1.0);
    if (pos.w <= 0.0) {
        gl_Position = vec4(0.0, 0.0, 0.0, -1.0);   // behind the eye: outside the clip volume
        fragColor = vec4(0.0);
        pointCoord = vec2(0.0);
        time = 0.0;
        return;
    }

    float size = style.params.x * pc.viewport.z;
    if (style.params.z > 0.0) {
        float d = length((pc.modelView * vec4(rel, 0.0)).xyz);
        if (d > style.params.z) size *= sqrt(style.params.z / d);
    }
    vec2 halfSize = 0.5 * pc.viewport.xy;
    pos.xy += (0.5 * size * corner / halfSize) * pos.w;

    gl_Position = pos;
    fragColor = (style.params.w > 0.5) ? style.color * colors.c[marker] : style.color;
    pointCoord = 0.5 * corner + 0.5;               // Vulkan's y is down, as gl_PointCoord's
    time = pc.viewport.w;
}
