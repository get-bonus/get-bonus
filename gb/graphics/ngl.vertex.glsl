#version 330
 
layout(location=0) in vec4 in_Position;
layout(location=1) in vec4 in_Color;
layout(location=2) in int in_TexIndex;
layout(location=3) in vec3 in_Transforms;

uniform vec4 TextureAtlasIndex[~a];

out VertexData {
  vec4 Color;
  vec4 TexCoord;
  float cosA;
  float sinA;
} vertexData;
 
void main(void)
{
  gl_Position.xy = in_Position.xy;
  gl_Position.z = in_Position.z * in_Transforms.x;
  gl_Position.w = in_Position.w * in_Transforms.y;
  vertexData.cosA = cos(in_Transforms.z);
  vertexData.sinA = sin(in_Transforms.z);
  vertexData.Color = in_Color;
  vertexData.TexCoord = TextureAtlasIndex[in_TexIndex];
}
