#version 330
 
layout(location=0) in vec4 in_Position;
layout(location=1) in vec4 in_Color;
layout(location=2) in vec2 in_TexCoord;
layout(location=3) in vec3 in_Transforms;

out vec4 Color;
out vec2 TexCoord;

void main(void)
{
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
