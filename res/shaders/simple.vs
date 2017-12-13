#version 330
#extension GL_ARB_explicit_uniform_location : require

layout (location = 0) in vec3 coord;
layout (location = 1) in vec2 texCoord;

layout (location = 0) uniform mat4 mvp;

out vec2 TexCoord;

void main(void) {
    gl_Position = mvp * vec4(coord, 1.0);
    TexCoord = texCoord;
}
