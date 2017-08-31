#version 330 core
layout (location = 0) in vec3 position;
layout (location = 1) in vec3 normal;

uniform mat4 model;
uniform mat4 view;
uniform mat4 projection;
uniform vec3 lightPos;

out vec3 surfaceNormal;
out vec3 toLight;

void main()
{
    vec4 worldPos = model * vec4(position, 1.0f);
    gl_Position = projection * view * worldPos;

    surfaceNormal = (model * vec4(normal, 0.0)).xyz;
    toLight = lightPos - worldPos.xyz;
}
