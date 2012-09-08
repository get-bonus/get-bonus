#version 330
 
layout(location=0) in vec4 in_Position;
layout(location=1) in vec4 in_Color;
layout(location=2) in vec2 in_TexCoord;
//layout(location=2) in int in_TexIndex;
layout(location=3) in vec3 in_Transforms;

uniform vec4 TextureAtlasIndex[~a];

/*
out VertexData {
  vec4 Color;
  vec4 TexCoord;
  float Rotation;
} vertexData;
*/

out vec4 Color;
out vec2 TexCoord;

void main(void)
{
  /*  gl_Position.xy = in_Position.xy;
  gl_Position.z = in_Position.z * in_Transforms.x;
  gl_Position.w = in_Position.w * in_Transforms.y;
  vertexData.Rotation = in_Transforms.z;
  vertexData.Color = in_Color;
  vertexData.TexCoord = TextureAtlasIndex[in_TexIndex];
  */

  float Angle = in_Transforms.z;
  mat4 RotationMatrix = mat4( cos( Angle ), -sin( Angle ), 0.0, 0.0,
                              sin( Angle ),  cos( Angle ), 0.0, 0.0,
                                       0.0,           0.0, 1.0, 0.0,
                                       0.0,           0.0, 0.0, 1.0 );


  vec4 pos_pre = vec4(in_Position.xy, 0.0, 1.0);

  gl_Position = pos_pre * RotationMatrix;

  Color = in_Color;
  TexCoord = in_TexCoord;
}
