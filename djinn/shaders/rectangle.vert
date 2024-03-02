#version 410 core
layout (location = 0) in vec2 pos;
layout (location = 1) in vec3 color;

out vec3 outColor;

void main()
{
    gl_Position = vec4(pos.x, pos.y, 0.0, 1.0);
    outColor = color;
}
