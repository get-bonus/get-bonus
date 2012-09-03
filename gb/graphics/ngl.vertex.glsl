#version 330
 
layout(location=0) in vec4 in_Position;
layout(location=1) in vec4 in_Color;
layout(location=2) in float in_TexIndex;

// XXX Change this two to something else, hopefully set by the program
uniform vec4 TextureAtlasIndex[4];

out VertexData {
  vec4 Color;
  vec4 TexCoord;
} vertexData;
 
void main(void)
{
  gl_Position = in_Position;
  vertexData.Color = in_Color;
  //vertexData.TexCoord = vec4(0.0, 0.0, 0.0, 0.0);
  //vertexData.TexCoord = vec4(0.0, 0.0, 1.0, 1.0);
  //vertexData.TexCoord = TextureAtlasIndex[0];
  //vertexData.TexCoord = TextureAtlasIndex[1];
  vertexData.TexCoord = TextureAtlasIndex[int(in_TexIndex)];
}
