#version 450

// Labels: billboard text (always facing the screen) sized in pixels, for frame
// names, axis labels and HUD text. Adapted from VulkanSceneGraph's text.vert
// (MIT license, Copyright (c) Robert Osfield), GPU-layout billboard variant:
// vsg::Text with GpuLayoutTechnique keeps the glyph indices and the layout in
// uniform buffers (so the text can change after it is shown) with a character
// size of 1, and this shader places and scales the glyphs in eye space using
// the size of the view being drawn (pushed at offset 128 by the label node in
// src/vsg/vkf_layer.cpp), so a label is right in every window.
//
// textLayout.position.w selects the size (OpenFrames' character size modes):
//   w > 0: a fixed screen size of w window units (logical pixels), as
//          OpenFrames' name labels and HUD text (SCREEN_COORDS);
//   w < 0: an object size of -w (scene units), but no larger on screen than
//          textLayout.outlineWidth window units, as OpenFrames' axis labels
//          (OBJECT_COORDS_WITH_MAXIMUM_SCREEN_SIZE). Labels draw no outline.

#define GLYPH_DIMENSIONS 0
#define GLYPH_BEARINGS 1
#define GLYPH_UVRECT 2

layout(constant_id = 0) const uint numTextIndices = 256;

layout(set = 0, binding = 1) uniform sampler2D glyphMetricsSampler;

layout(set = 1, binding = 0) uniform TextLayout {
    vec4 position;          // xyz = position, w = size (see above)
    vec4 horizontal;        // xyz = direction, w = horizontal alignment offset
    vec4 vertical;          // xyz = direction, w = vertical alignment offset
    vec4 color;
    vec4 outlineColor;
    float outlineWidth;     // the maximum size of object-sized labels
} textLayout;

layout(set = 1, binding = 1) uniform TextIndices {
    uvec4 glyph_index[numTextIndices];
} text;

layout(push_constant) uniform PushConstants {
    mat4 projection;
    mat4 modelview;
    vec4 viewport;          // xy = size of the view in framebuffer pixels,
                            // z = framebuffer pixels per window unit (2 on Retina),
                            // w = 1 for HUD labels (drawn on top of the scene)
} pc;

layout(location = 0) in vec3 inPosition;

layout(location = 0) out vec4 fragColor;
layout(location = 1) out vec4 outlineColor;
layout(location = 2) out float outlineWidth;
layout(location = 3) out vec2 fragTexCoord;

out gl_PerVertex {
    vec4 gl_Position;
};

void main()
{
    // the glyph's position in the text (VSG's GPU layout)
    float horiAdvance = 0.0;
    float vertAdvance = 0.0;
    for (uint i = 0; i < gl_InstanceIndex; ++i)
    {
        uint glyph_index = text.glyph_index[i / 4][i % 4];
        if (glyph_index == 0)
        {
            vertAdvance -= 1.0;     // a newline
            horiAdvance = 0.0;
        }
        else
        {
            horiAdvance += texture(glyphMetricsSampler, vec2(GLYPH_DIMENSIONS, glyph_index))[2];
        }
    }
    uint glyph_index = text.glyph_index[gl_InstanceIndex / 4][gl_InstanceIndex % 4];
    vec4 dimensions = texture(glyphMetricsSampler, vec2(GLYPH_DIMENSIONS, glyph_index));
    vec4 bearings = texture(glyphMetricsSampler, vec2(GLYPH_BEARINGS, glyph_index));
    vec4 uv_rec = texture(glyphMetricsSampler, vec2(GLYPH_UVRECT, glyph_index));
    vec3 pos = textLayout.horizontal.xyz * (horiAdvance + textLayout.horizontal.w + bearings.x + inPosition.x * dimensions.x) +
               textLayout.vertical.xyz * (vertAdvance + textLayout.vertical.w + bearings.y + (inPosition.y - 1.0) * dimensions.y);

    vec4 center_eye = pc.modelview * vec4(textLayout.position.xyz, 1.0);

    // eye-space units per framebuffer pixel at the label's depth, across and up
    // (w is the distance for a perspective projection, 1 for an orthographic
    // one; the two differ for a non-square projection such as the HUD's)
    float w = (pc.projection * center_eye).w;
    vec2 units_per_pixel = 2.0 * abs(w) / (vec2(abs(pc.projection[0][0]), abs(pc.projection[1][1])) *
                                           max(pc.viewport.xy, vec2(1.0)));

    float size = textLayout.position.w;
    vec2 scale;
    if (size > 0.0)
        scale = size * pc.viewport.z * units_per_pixel;
    else
        scale = vec2(min(-size, textLayout.outlineWidth * pc.viewport.z * units_per_pixel.y));

    gl_Position = pc.projection * (center_eye + vec4(pos.xy * scale, pos.z * scale.y, 0.0));
    gl_Position.z += inPosition.z * 0.0001;
    if (pc.viewport.w > 0.5) gl_Position.z = gl_Position.w;   // HUD: at the near plane (reverse depth)

    fragColor = textLayout.color;
    outlineColor = textLayout.outlineColor;
    outlineWidth = 0.0;
    fragTexCoord = vec2(mix(uv_rec[0], uv_rec[2], inPosition.x), mix(uv_rec[1], uv_rec[3], inPosition.y));
}
