#version 330
 
layout(location=0) in vec4 in_Position;
layout(location=1) in vec4 in_Color;
layout(location=2) in int in_TexIndex;
layout(location=3) in vec3 in_Transforms;
layout(location=4) in int in_WhichCorner;

uniform vec4 TextureAtlasIndex[~a];
uniform ivec2 CornerMapping_Pos[4];
uniform ivec2 CornerMapping_Tex[4];

out vec4 Color;
out vec2 TexCoord;

void main(void)
{
  int WhichCorner = in_WhichCorner;

  float x = in_Position.x;
  float y = in_Position.y;
  float hw = in_Position.z * in_Transforms.x;
  float hh = in_Position.w * in_Transforms.y;

  float Angle = in_Transforms.z;

  mat4 RotationMatrix = mat4( cos( Angle ), -sin( Angle ), 0.0, 0.0,
                              sin( Angle ),  cos( Angle ), 0.0, 0.0,
                                       0.0,           0.0, 1.0, 0.0,
                                       0.0,           0.0, 0.0, 1.0 );

  gl_Position = vec4(x + CornerMapping_Pos[WhichCorner].x * hw,
                     y + CornerMapping_Pos[WhichCorner].y * hh,
                     0.0, 1.0) * RotationMatrix;
  
  Color = in_Color;

  vec4 Tpos = TextureAtlasIndex[in_TexIndex];
  float Tllx = Tpos.x;
  float Tlly = Tpos.y;
  float Tw = Tpos.z;
  float Th = Tpos.w;

  TexCoord = vec2(Tllx + CornerMapping_Tex[WhichCorner].x * Tw,
                  Tlly + CornerMapping_Tex[WhichCorner].y * Th);
}
