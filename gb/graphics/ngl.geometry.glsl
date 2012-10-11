#version 330
#extension GL_EXT_geometry_shader4 : enable

layout (points) in;
layout (triangle_strip, max_vertices=4) out;

in VertexData {
  vec4 Color;
  vec4 TexCoord;
  float Rotation;
} vertexData[1];

uniform float ViewportWidth;
uniform float ViewportHeight;

out vec4 Color;
out vec2 TexCoord;

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

void main()
{
  mat4 ViewportMatrix = glOrtho(0.0, ViewportWidth,
                                0.0, ViewportHeight,
                                1.0, -1.0);

  int i = 0;

  vec4 pos = gl_in[i].gl_Position;

  float x = pos.x;
  float y = pos.y;
  mat4 TranslateMatrix = glTranslate(x, y, 0.0);

  float Angle = vertexData[i].Rotation;
  mat4 RotationMatrix = glRotate(Angle, 0.0, 0.0, 1.0);

  mat4 ZeMatrix = RotationMatrix * TranslateMatrix * ViewportMatrix;

  float hw = pos.z;
  float hh = pos.w;
  vec4 ul = vec4(- hw, + hh, 0.0, 1.0) * ZeMatrix;
  vec4 ur = vec4(+ hw, + hh, 0.0, 1.0) * ZeMatrix;
  vec4 ll = vec4(- hw, - hh, 0.0, 1.0) * ZeMatrix;
  vec4 lr = vec4(+ hw, - hh, 0.0, 1.0) * ZeMatrix;

  vec4 Tpos = vertexData[i].TexCoord;
  float Tllx = Tpos.x;
  float Tlly = Tpos.y;
  float Tw = Tpos.z;
  float Th = Tpos.w;

  vec2 Tll = vec2(Tllx, Tlly);
  vec2 Tul = vec2(Tllx, Tlly + Th);
  vec2 Tur = vec2(Tllx + Tw, Tlly + Th);
  vec2 Tlr = vec2(Tllx + Tw, Tlly);

  Color = vertexData[i].Color;

  gl_Position = ul;
  TexCoord = Tll;
  EmitVertex();

  gl_Position = ur;
  TexCoord = Tlr;
  EmitVertex();

  gl_Position = ll;
  TexCoord = Tul;
  EmitVertex();

  gl_Position = lr;
  TexCoord = Tur;
  EmitVertex();

  EndPrimitive();
}
