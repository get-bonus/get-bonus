#version 330
#extension GL_EXT_geometry_shader4 : enable
 
layout (points) in;
layout (triangle_strip, max_vertices=4) out;

in VertexData {
  vec4 Color;
  vec4 TexCoord;
  float cosA;
  float sinA;
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

    float cosA = vertexData[i].cosA;
    float sinA = vertexData[i].sinA;
    float left = x - hw;
    float right = x + hw;
    float upper = y + hh;
    float lower = y - hh;

    float leftS = left * sinA;
    float leftC = left * cosA;
    float rightS = right * sinA;
    float rightC = right * cosA;

    float lowerS = lower * sinA;
    float lowerC = lower * cosA;
    float upperS = upper * sinA;
    float upperC = upper * cosA;

    vec2 ul = vec2(leftC - upperS, leftS + upperC);
    //vec2 ur = vec2(rightC - upperS, rightS + upperC);
    vec2 ll = vec2(leftC - lowerS, leftS + lowerC);
    //vec2 lr = vec2(rightC - lowerS, rightS + lowerC);

    vec4 Tpos = vertexData[i].TexCoord;
    float Tw = Tpos.z;
    float Th = Tpos.w;

    Color = vertexData[i].Color;

    gl_Position = vec4(ul, 0.0, 1.0);
    TexCoord = Tpos.xy;
    EmitVertex();

    gl_Position += vec4(rightC - leftC, rightS - leftS, 0.0, 0.0);
    //gl_Position = vec4(ur, 0.0, 1.0);
    TexCoord += vec2(Tw, 0);
    EmitVertex();

    gl_Position = vec4(ll, 0.0, 1.0);
    TexCoord += vec2(-Tw, Th);
    EmitVertex();

    gl_Position += vec4(rightC - leftC, rightS - leftS, 0.0, 0.0);
    //gl_Position = vec4(lr, 0.0, 1.0);
    TexCoord += vec2(Tw, 0);
    EmitVertex();
  }
}
