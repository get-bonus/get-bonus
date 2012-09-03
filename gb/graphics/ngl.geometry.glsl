#version 330
#extension GL_EXT_geometry_shader4 : enable
 
layout (points) in;
layout (triangle_strip, max_vertices=4) out;

in VertexData {
  vec4 Color;
  vec4 TexCoord;
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

    vec4 ul = vec4(x - hw, y + hh, 0.0, 1.0);
    vec4 ur = vec4(x + hw, y + hh, 0.0, 1.0);
    vec4 ll = vec4(x - hw, y - hh, 0.0, 1.0);
    vec4 lr = vec4(x + hw, y - hh, 0.0, 1.0);

    vec4 Tpos = vertexData[i].TexCoord;
    float Tx = Tpos.x;
    float Ty = Tpos.y;
    float Thw = Tpos.z;
    float Thh = Tpos.w;

    vec2 Tul = vec2(Tx - Thw, Ty + Thh);
    vec2 Tur = vec2(Tx + Thw, Ty + Thh);
    vec2 Tll = vec2(Tx - Thw, Ty - Thh);
    vec2 Tlr = vec2(Tx + Thw, Ty - Thh);

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
