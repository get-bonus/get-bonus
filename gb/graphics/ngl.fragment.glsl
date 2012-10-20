#version 330

uniform sampler2D TextureAtlas; 
uniform int TextureAtlasSize;

in vec4 Color;
in vec2 TexCoord;
out vec4 out_Color;
 
void main(void)
{
  // XXX Would it be faster to use texelFetch with integer coords?

  // I changed from floor to ceil to fix the menu, but I'm worried
  // that I will eventually find some that need to be balanced on the
  // other side. Maybe I should preserve the original pixel boundaries
  // coordinates better and round towards them?
  vec2 TexCoord_uv = vec2(ceil(TexCoord.x)+0.5, ceil(TexCoord.y)+0.5) / TextureAtlasSize;
  // Blurry version:
  // vec2 TexCoord_uv = TexCoord / TextureAtlasSize;

  // XXX Do proper blending, allow color to set the alpha, etcs
  out_Color = Color + texture2D(TextureAtlas, TexCoord_uv);
}
