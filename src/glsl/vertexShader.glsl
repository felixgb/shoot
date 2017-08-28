attribute vec3 coord;
attribute vec3 color;
varying vec3 f_color;
uniform mat4 projection;
uniform mat4 scale;

uniform mat4 transform;

varying vec4 position_in_view_space;

void main(void) {
    gl_Position = transform * gl_Vertex;
    f_color = color;
}
