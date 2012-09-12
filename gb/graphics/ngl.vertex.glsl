#version 330

layout(location=0) in vec4 in_Position;
layout(location=1) in vec4 in_Color;
layout(location=2) in uvec4 in_TexCoord;
layout(location=3) in vec3 in_Transforms;

uniform int TextureAtlasWidth;
uniform int TextureAtlasHeight;

out VertexData {
  vec4 Color;
  vec4 TexCoord;
  float Rotation;
} vertexData;

void main(void)
{
  gl_Position.xy = in_Position.xy;
  gl_Position.z = in_Position.z * in_Transforms.x;
  gl_Position.w = in_Position.w * in_Transforms.y;
  vertexData.Rotation = in_Transforms.z;
  vertexData.Color = in_Color;

  vertexData.TexCoord =
    vec4(float(in_TexCoord.x) / float(TextureAtlasWidth),
         float(in_TexCoord.y) / float(TextureAtlasHeight),
         float(in_TexCoord.z) / float(TextureAtlasWidth),
         float(in_TexCoord.w) / float(TextureAtlasHeight));
}
