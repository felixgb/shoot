#version 330 core

in vec3 surfaceNormal;
in vec3 toLight;

uniform vec3 lightColor;

out vec4 outColor;

void main()
{
    vec3 unitNormal = normalize(surfaceNormal);
    vec3 unitLightVector = normalize(toLight);

    // how bright this pixel should be
    float nDot = dot(unitNormal, unitLightVector);
    float brightness = max(nDot, 0.0);

    vec3 diffuse = brightness * lightColor;

    outColor = vec4(diffuse, 1.0f);
}
