#version 130

in vec4 in_Position;
in vec4 in_Color;
in vec4 in_TexCoord;
in vec3 in_Transforms;
in vec2 in_VertexSpecification;
in float in_Palette;

uniform float ViewportWidth;
uniform float ViewportHeight;

out vec4 Color;
out vec2 TexCoord;
out float Palette;

mat4 glRotate( float angle, float x, float y, float z ) {
  float c = cos(angle);
  float s = sin(angle);
  return mat4( x*x*(1-c) + c, x*y*(1-c) - z*s, x*z*(1-c) + y*s, 0.0,
               y*x*(1-c) + z*s, y*y*(1-c) + c, y*z*(1-c) - x*s, 0.0,
               x*z*(1-c) - y*s, y*z*(1-c) + x*s, z*z*(1-c)+c, 0.0,
               0.0, 0.0, 0.0, 1.0);
}

mat4 glOrtho( float left, float right, float bottom, float top, float nearVal, float farVal ) {
  float t_x = - (right + left) / (right - left);
  float t_y = - (top + bottom) / (top - bottom);
  float t_z = - (farVal + nearVal) / (farVal - nearVal);
  return mat4( 2.0 / right - left, 0.0, 0.0, t_x,
               0.0, 2.0 / top - bottom, 0.0, t_y,
               0.0, 0.0, -2 / farVal - nearVal, t_z,
               0.0, 0.0, 0.0, 1.0 );
}

mat4 glTranslate( float x, float y, float z ) {
  return mat4(1.0, 0.0, 0.0, x,
              0.0, 1.0, 0.0, y,
              0.0, 0.0, 1.0, z,
              0.0, 0.0, 0.0, 1.0);
}

void main(void)
{
  // Vertex Shader
  vec4 the_gl_Position;

  the_gl_Position.xy = in_Position.xy;
  the_gl_Position.z = in_Position.z * in_Transforms.x;
  the_gl_Position.w = in_Position.w * in_Transforms.y;

  float mid_Rotation = in_Transforms.z;
  vec4 mid_Color = in_Color;
  vec4 mid_TexCoord = in_TexCoord;

  // Geometry Shader
  mat4 ViewportMatrix = glOrtho(0.0, ViewportWidth,
                                0.0, ViewportHeight,
                                1.0, -1.0);

  int i = 0;

  vec4 pos = the_gl_Position;

  float x = pos.x;
  float y = pos.y;
  mat4 TranslateMatrix = glTranslate(x, y, 0.0);

  float Angle = mid_Rotation;
  mat4 RotationMatrix = glRotate(Angle, 0.0, 0.0, 1.0);

  mat4 ZeMatrix = RotationMatrix * TranslateMatrix * ViewportMatrix;

  float hw = pos.z;
  float hh = pos.w;
  vec4 vcorner = vec4(in_VertexSpecification.x * hw,
                      in_VertexSpecification.y * hh,
                      0.0, 1.0) * ZeMatrix;

  vec4 Tpos = mid_TexCoord;
  float Tllx = Tpos.x;
  float Tlly = Tpos.y;
  float Tw = Tpos.z;
  float Th = Tpos.w;

  vec2 tcorner = vec2(Tllx + ((in_VertexSpecification.x + 1.0)/2.0) * Tw,
                      Tlly + ((in_VertexSpecification.y - 1.0)/-2.0) * Th);

  Color = mid_Color;
  gl_Position = vcorner;
  TexCoord = tcorner;
  Palette = in_Palette;
}
