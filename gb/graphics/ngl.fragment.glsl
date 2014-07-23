#version 330

uniform sampler2D SpriteAtlasTex; 
uniform sampler2D PaletteAtlasTex;

// None of these are "flat" because they are the same on all vertices
// anyways, except TexCoord which needs to be interpolated across the
// triangle.
in vec4 Color;
in vec2 TexCoord;
in float Palette;
out vec4 out_Color;

float blurry_mess ( float v ) {
  return v;
}

float works_for_menu ( float v ) {
  return ceil(v)+0.5;
}

float works_for_tennis_bg ( float v ) {
  return round(v)+0.5;
}

// XXX This is strange, because before I changed menu's resolution,
// floor was broken on it.
float works_for_tennis_bg_and_menu ( float v ) {
  return floor(v)+0.5;
}

float clampit ( float v ) {
  //return works_for_menu(v);
  return works_for_tennis_bg_and_menu(v);
}
 
void main(void)
{
  ivec2 TexCoord_uv = ivec2(clampit(TexCoord.x), clampit(TexCoord.y));
  vec4 SpriteColor = texelFetch(SpriteAtlasTex, TexCoord_uv, 0);

  float PaletteOffset = SpriteColor.r * 256;
  ivec2 PalCoord_uv = ivec2( PaletteOffset, Palette );
  vec4 PaletteColor = texelFetch(PaletteAtlasTex, PalCoord_uv, 0 );
  
  // XXX Do proper blending, allow color to set the alpha, etcs
  out_Color = Color + PaletteColor;
}
