#version 330
 
layout(location=0) in vec4 in_Position;
layout(location=1) in vec4 in_Color;
layout(location=2) in vec2 in_TexCoord;
//layout(location=2) in int in_TexIndex;
layout(location=3) in vec3 in_Transforms;
layout(location=4) in int in_Corner;

uniform vec4 TextureAtlasIndex[~a];

float hw_factor[4] = float[]( -1, +1, -1, +1 );
float hh_factor[4] = float[]( +1, +1, -1, -1 );
float Tw_factor[4] = float[](  0,  1,  0,  1 );
float Th_factor[4] = float[](  0,  0,  1,  1 );

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
  mat2 RotationMatrix = mat2( cos( Angle ), -sin( Angle ),
                              sin( Angle ),  cos( Angle ) );

  vec4 pos = in_Position;
  float x = pos.x;
  float y = pos.y;
  float hw = pos.z;
  float hh = pos.w;
  float mx = in_Transforms.x;
  float my = in_Transforms.y;

  vec2 pos_2d_pre = vec2(x + hw * mx, y + hh * my);
  vec2 pos_2d = pos_2d_pre * RotationMatrix;

  gl_Position = vec4(pos_2d, 0.0, 1.0);

  Color = in_Color;
  TexCoord = in_TexCoord;
}
