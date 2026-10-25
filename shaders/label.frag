#version 450

// Labels: glyphs from the font's signed-distance-field atlas. VulkanSceneGraph's
// text.frag (MIT license, Copyright (c) Robert Osfield), without outlines.

layout(binding = 0) uniform sampler2D texSampler;

layout(location = 0) in vec4 fragColor;
layout(location = 1) in vec4 outlineColor;
layout(location = 2) in float outlineWidth;
layout(location = 3) in vec2 fragTexCoord;

layout(location = 0) out vec4 outColor;

float glyph_alpha(vec2 texcoord, vec2 dx, vec2 dy)
{
    float lod = textureQueryLod(texSampler, texcoord).x;
    float innerCutOff = 0.0;
    if (lod > 0.0) innerCutOff = lod * 0.03;

    float distance_from_edge = textureGrad(texSampler, texcoord, dx, dy).r;
    float d_distance_dx = dFdx(distance_from_edge);
    float d_distance_dy = dFdy(distance_from_edge);
    float delta = sqrt(d_distance_dx * d_distance_dx + d_distance_dy * d_distance_dy);

    float min_distance_from_edge = distance_from_edge - delta + innerCutOff;
    float max_distance_from_edge = distance_from_edge + delta;

    if (min_distance_from_edge >= 0.0) return 1.0;
    if (max_distance_from_edge >= 0.0) return max_distance_from_edge / (max_distance_from_edge - min_distance_from_edge);
    return 0.0;
}

float sampled_glyph_alpha_grid(vec2 texcoord)
{
    float lod = textureQueryLod(texSampler, texcoord).x;
    vec2 dx = dFdx(texcoord);
    vec2 dy = dFdy(texcoord);
    if (lod <= 0.0) return glyph_alpha(texcoord, dx, dy);

    float area = length(dx) * length(dy);
    float average_side = sqrt(area) / (1.0 + lod);
    float num_x = ceil(length(dx) / average_side);
    float num_y = ceil(length(dy) / average_side);
    vec2 interval_dx = dx / num_x;
    vec2 interval_dy = dy / num_y;

    float total_alpha = 0.0;
    vec2 tc_row_start = texcoord - dx * 0.5 - dy * 0.5;
    for (float r = 0; r < num_y; ++r)
    {
        vec2 tc = tc_row_start;
        tc_row_start = tc_row_start + interval_dy;
        for (float c = 0; c < num_x; ++c)
        {
            total_alpha = total_alpha + glyph_alpha(tc, interval_dx, interval_dy);
            tc = tc + interval_dx;
        }
    }
    return total_alpha / (num_x * num_y);
}

void main()
{
    outColor = vec4(fragColor.rgb, fragColor.a * sampled_glyph_alpha_grid(fragTexCoord));
    if (outColor.a == 0.0) discard;
}
