#version 410 core
layout (location = 0) in vec2 aPos;
layout (location = 1) in vec2 aTexCoord;

out vec2 texCoord;

uniform mat4 projection;

void main()
{
    gl_Position = projection * vec4(aPos.x, aPos.y,  1.0, 1.0);
    texCoord = aTexCoord;
}