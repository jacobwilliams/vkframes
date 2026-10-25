#version 450

// Labels: billboard text (always facing the screen) sized in pixels, for frame
// names and axis labels. Adapted from VulkanSceneGraph's text.vert (MIT
// license, Copyright (c) Robert Osfield), CPU-layout billboard variant: vsg::Text
// lays out the glyphs (with a character size of 1) and this shader scales them
// in eye space, using the size of the view being drawn (pushed at offset 128 by
// the label node in src/vsg/vkf_layer.cpp), so a label is right in every window.
//
// inCenterAndAutoScaleDistance.w selects the size (OpenFrames' character size
// modes for its labels):
//   w > 0: a fixed screen size of w window units (logical pixels), as
//          OpenFrames' name labels (SCREEN_COORDS);
//   w < 0: an object size of -w (scene units), but no larger on screen than
//          inOutlineWidth window units, as OpenFrames' axis labels
//          (OBJECT_COORDS_WITH_MAXIMUM_SCREEN_SIZE). Labels draw no outline.

layout(push_constant) uniform PushConstants {
    mat4 projection;
    mat4 modelview;
    vec4 viewport;          // xy = size of the view in framebuffer pixels,
                            // z = framebuffer pixels per window unit (2 on Retina)
} pc;

layout(location = 0) in vec3 inPosition;
layout(location = 1) in vec4 inColor;
layout(location = 2) in vec4 inOutlineColor;
layout(location = 3) in float inOutlineWidth;
layout(location = 4) in vec3 inTexCoord;
layout(location = 5) in vec4 inCenterAndAutoScaleDistance;

layout(location = 0) out vec4 fragColor;
layout(location = 1) out vec4 outlineColor;
layout(location = 2) out float outlineWidth;
layout(location = 3) out vec2 fragTexCoord;

out gl_PerVertex {
    vec4 gl_Position;
};

void main()
{
    vec4 center_eye = pc.modelview * vec4(inCenterAndAutoScaleDistance.xyz, 1.0);

    // eye-space units per framebuffer pixel at the label's depth (w is the
    // distance for a perspective projection, 1 for an orthographic one)
    float w = (pc.projection * center_eye).w;
    float units_per_pixel = 2.0 * abs(w) / (abs(pc.projection[1][1]) * max(pc.viewport.y, 1.0));

    float size = inCenterAndAutoScaleDistance.w;
    float scale;
    if (size > 0.0)
        scale = size * pc.viewport.z * units_per_pixel;
    else
        scale = min(-size, inOutlineWidth * pc.viewport.z * units_per_pixel);

    gl_Position = pc.projection * (center_eye + vec4(inPosition.xy * scale, inPosition.z * scale, 0.0));
    gl_Position.z += inTexCoord.z * 0.0001;

    fragColor = inColor;
    outlineColor = inOutlineColor;
    outlineWidth = 0.0;
    fragTexCoord = inTexCoord.xy;
}
