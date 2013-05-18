#version 130

uniform sampler2D TextureAtlas; 
uniform int TextureAtlasSize;

in vec4 Color;
in vec2 TexCoord;
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
  // XXX Would it be faster to use texelFetch with integer coords?

  vec2 TexCoord_uv = 
    vec2(clampit(TexCoord.x), clampit(TexCoord.y))
    / TextureAtlasSize;

  // XXX Do proper blending, allow color to set the alpha, etcs
  out_Color = Color + texture2D(TextureAtlas, TexCoord_uv);
}
