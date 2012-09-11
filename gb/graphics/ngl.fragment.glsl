#version 330

uniform sampler2D TextureAtlas; 

in vec4 Color;
in vec2 TexCoord;
out vec4 out_Color;
 
void main(void)
{
  // XXX Do proper blending, allow color to set the alpha, etcs
  out_Color = Color + texture2D(TextureAtlas, TexCoord);
}
