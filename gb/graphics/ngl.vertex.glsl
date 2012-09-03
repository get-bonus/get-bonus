#version 330
 
layout(location=0) in vec4 in_Position;
layout(location=1) in vec4 in_Color;
layout(location=2) in vec4 in_TexCoord;
out VertexData {
  vec4 Color;
  vec4 TexCoord;
} vertexData;
 
void main(void)
{
  gl_Position = in_Position;
  vertexData.Color = in_Color;
  vertexData.TexCoord = in_TexCoord;
}
