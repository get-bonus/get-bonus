#version 330
#extension GL_EXT_geometry_shader4 : enable
 
layout (points) in;
layout (triangle_strip, max_vertices=4) out;

in VertexData {
  vec4 Color;
  vec4 TexCoord;
  float Rotation;
} vertexData[];

out vec4 Color;
out vec2 TexCoord;

void main()
{
  for(int i = 0; i < gl_VerticesIn; i++)
  {
    vec4 pos = gl_in[i].gl_Position;
    float x = pos.x;
    float y = pos.y;
    float hw = pos.z;
    float hh = pos.w;

    float Angle = vertexData[i].Rotation;
    mat4 RotationMatrix = mat4( cos( Angle ), -sin( Angle ), 0.0, 0.0,
                                sin( Angle ),  cos( Angle ), 0.0, 0.0,
                                         0.0,           0.0, 1.0, 0.0,
                                         0.0,           0.0, 0.0, 1.0 );

    vec4 ul = vec4(x - hw, y + hh, 0.0, 1.0) * RotationMatrix;
    vec4 ur = vec4(x + hw, y + hh, 0.0, 1.0) * RotationMatrix;
    vec4 ll = vec4(x - hw, y - hh, 0.0, 1.0) * RotationMatrix;
    vec4 lr = vec4(x + hw, y - hh, 0.0, 1.0) * RotationMatrix;

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
}
