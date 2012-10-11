#version 330

uniform sampler2D TextureAtlas; 
uniform int TextureAtlasSize;

in vec4 Color;
in vec2 TexCoord;
out vec4 out_Color;
 
void main(void)
{
  // XXX Would it be faster to use texelFetch with integer coords?

  // XXX Do proper blending, allow color to set the alpha, etcs
  vec2 TexCoord_uv = vec2(floor(TexCoord.x)+0.5, floor(TexCoord.y)+0.5) / TextureAtlasSize;
  out_Color = Color + texture2D(TextureAtlas, TexCoord_uv);
}
