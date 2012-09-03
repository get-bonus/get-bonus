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

    Color = vertexData[i].Color;
    TexCoord = vertexData[i].TexCoord.xy;

    gl_Position = ul;
    EmitVertex();

    gl_Position = ur;
    EmitVertex();

    gl_Position = ll;
    EmitVertex();

    gl_Position = lr;
    EmitVertex();

    EndPrimitive();

  }
}
