#version 410 core
out vec4 FragColor;

in vec2 texCoord;

uniform sampler2D fontAtlas;

void main()
{
    vec4 c = texture(fontAtlas, texCoord);
    FragColor = vec4(c.r, c.r, c.r, c.r);
    // FragColor = vec4(1.0);
}