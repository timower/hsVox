#version 330
#extension GL_ARB_explicit_uniform_location : require

in vec2 TexCoord;

uniform sampler2D tex1;

void main(void) {
    gl_FragColor = texture(tex1, TexCoord);
}
