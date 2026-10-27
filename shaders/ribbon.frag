#version 450

// Ribbons (see ribbon.vert): the color, blended over the scene.

layout(location = 0) in vec4 fragColor;
layout(location = 0) out vec4 outColor;

void main()
{
    outColor = fragColor;
}
