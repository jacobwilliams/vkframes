#version 450

// Stars at infinity (OpenFrames' SkySphere star field): a screen-space square
// (6 vertices) per star, of the star's size, in the direction of the star in
// the sky's frame (turned by the sky's attitude, then the view's rotation).
// Drawn at the far plane (reverse-Z: depth 0) without writing depth, so the
// scene is in front; blended with what is behind by taking the brighter
// (OpenFrames' RGBA_MAX), so stars show over a sky texture.

layout(push_constant) uniform PushConstants {
    mat4 projection;
    mat4 modelView;         // with its translation zeroed (EyeRelative)
    vec4 eyeHigh;
    vec4 eyeLow;
    vec4 viewport;          // xy = size in framebuffer pixels, z = framebuffer pixels per window unit
} pc;

layout(std430, set = 0, binding = 0) readonly buffer Stars {
    vec4 s[];               // star i: s[2i] = (direction, size in window units), s[2i+1] = (rgb, -)
} stars;

layout(set = 0, binding = 1) uniform Sky {
    mat4 attitude;          // sky frame to the root frame
    vec4 params;
} sky;

layout(location = 0) out vec3 fragColor;
layout(location = 1) out vec2 pointCoord;

out gl_PerVertex {
    vec4 gl_Position;
};

const vec2 corners[6] = vec2[](vec2(-1.0, -1.0), vec2(1.0, -1.0), vec2(-1.0, 1.0),
                               vec2(-1.0, 1.0), vec2(1.0, -1.0), vec2(1.0, 1.0));

void main()
{
    int star = gl_VertexIndex / 6;
    vec2 corner = corners[gl_VertexIndex % 6];
    vec4 a = stars.s[2 * star];
    vec3 view = mat3(pc.modelView) * (mat3(sky.attitude) * a.xyz);
    vec4 pos = pc.projection * vec4(view, 0.0);    // a direction: at infinity
    if (pos.w <= 0.0) {
        gl_Position = vec4(0.0, 0.0, 0.0, -1.0);   // behind the eye (or an orthographic view)
        fragColor = vec3(0.0);
        pointCoord = vec2(0.0);
        return;
    }
    vec2 halfSize = 0.5 * pc.viewport.xy;
    pos.xy += (0.5 * a.w * pc.viewport.z * corner / halfSize) * pos.w;
    pos.z = 0.0;                                   // the far plane
    gl_Position = pos;
    fragColor = stars.s[2 * star + 1].rgb;
    pointCoord = 0.5 * corner + 0.5;
}
