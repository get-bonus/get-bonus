#version 330
 
layout(location=0) in vec4 in_Position;
layout(location=1) in vec4 in_Color;
//layout(location=2) in int in_TexIndex;
//layout(location=3) in vec3 in_Transforms;

uniform vec4 TextureAtlasIndex[~a];

/*
out VertexData {
  vec4 Color;
  vec4 TexCoord;
  float Rotation;
} vertexData;
*/

out vec4 Color;

void main(void)
{
  /*  gl_Position.xy = in_Position.xy;
  gl_Position.z = in_Position.z * in_Transforms.x;
  gl_Position.w = in_Position.w * in_Transforms.y;
  vertexData.Rotation = in_Transforms.z;
  vertexData.Color = in_Color;
  vertexData.TexCoord = TextureAtlasIndex[in_TexIndex];
  */

  gl_Position = in_Position;
  Color = in_Color;
}
