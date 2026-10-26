#version 450

// Sky texture: an equirectangular image (as a Sphere's texture: u = lon/2pi +
// 1/2, v = 1/2 - lat/pi, here right ascension and declination) seen from
// inside, at infinity. One triangle covers the viewport; each corner's view
// ray is turned into the sky's frame (the view's rotation, without its
// translation, then the sky's attitude), and sky.frag samples the image along
// the interpolated ray. Drawn at the far plane (reverse-Z: depth 0) without
// writing depth, so the scene is in front.

layout(push_constant) uniform PushConstants {
    mat4 projection;
    mat4 modelView;         // with its translation zeroed (EyeRelative)
    vec4 eyeHigh;
    vec4 eyeLow;
    vec4 viewport;
} pc;

layout(set = 0, binding = 1) uniform Sky {
    mat4 attitude;          // sky frame to the root frame
    vec4 params;
} sky;

layout(location = 0) out vec3 ray;      // in the sky's frame

out gl_PerVertex {
    vec4 gl_Position;
};

void main()
{
    vec2 ndc = vec2((gl_VertexIndex << 1) & 2, gl_VertexIndex & 2) * 2.0 - 1.0;
    vec4 p = inverse(pc.projection) * vec4(ndc, 0.5, 1.0);   // a point on the ray, in view coordinates
    vec3 view = p.xyz / p.w;
    vec3 world = transpose(mat3(pc.modelView)) * view;      // the rotation's inverse
    ray = transpose(mat3(sky.attitude)) * world;
    gl_Position = vec4(ndc, 0.0, 1.0);
}
