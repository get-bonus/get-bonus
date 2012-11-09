#lang racket/base
(require ffi/vector
         ffi/cvector
         ffi/unsafe
         gb/graphics/gl-util
         racket/match
         stephanh/RacketGL1/rgl)

(define-shader-source fragment-source "crt.fragment.glsl")
(define-shader-source vertex-source "crt.vertex.glsl")

;; This width and height is based on the SNES, which was 256x239. The
;; smallest 16:9 rectangle that this fits in is 432x243, which is
;; crt-scale 27, but this makes it so that we have an odd number in
;; various places. So, we'll use crt-scale 28, or 448x252. This makes
;; the GBIES basically a "widescreen" SNES.
(define crt-scale 28)
(define crt-width (* crt-scale 16))
(define crt-height (* crt-scale 9))

;; FBO stuff based on: http://www.songho.ca/opengl/gl_fbo.html

;; shader stuff based on
;; :bsnes_v085-source/bsnes/ruby/video/opengl.hpp

(define (make-draw-on-crt actual-screen-width actual-screen-height)
  (define texture-width crt-width)
  (define texture-height crt-height)

  (define scale
    (* 1.
       (min (quotient actual-screen-width crt-width)
            (quotient actual-screen-height crt-height))))

  (define screen-width (* scale crt-width))
  (define screen-height (* scale crt-height))

  (define inset-left (/ (- actual-screen-width screen-width) 2.))
  (define inset-right (+ inset-left screen-width))
  (define inset-bottom (/ (- actual-screen-height screen-height) 2.))
  (define inset-top (+ inset-bottom screen-height))

  (glEnable GL_TEXTURE_2D)

  (define myTexture (u32vector-ref (glGenTextures 1) 0))

  (glBindTexture GL_TEXTURE_2D myTexture)
  (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_NEAREST)
  (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_NEAREST)
  (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S GL_CLAMP)
  (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T GL_CLAMP)
  (glTexImage2D
   GL_TEXTURE_2D 0 GL_RGBA8 texture-width texture-height 0
   GL_RGBA GL_UNSIGNED_BYTE
   0)
  (glBindTexture GL_TEXTURE_2D 0)

  (define myRB (u32vector-ref (glGenRenderbuffers 1) 0))

  (glBindRenderbuffer GL_RENDERBUFFER myRB)
  (glRenderbufferStorage GL_RENDERBUFFER
                         GL_DEPTH_COMPONENT24
                         texture-width texture-height)
  (glBindRenderbuffer GL_RENDERBUFFER 0)

  (define myFBO (u32vector-ref (glGenFramebuffers 1) 0))

  (glBindFramebuffer GL_FRAMEBUFFER myFBO)
  (glFramebufferTexture2D
   GL_DRAW_FRAMEBUFFER
   GL_COLOR_ATTACHMENT0
   GL_TEXTURE_2D myTexture 0)

  (glFramebufferRenderbuffer
   GL_FRAMEBUFFER
   GL_DEPTH_ATTACHMENT
   GL_RENDERBUFFER myRB)

  (match (glCheckFramebufferStatus GL_FRAMEBUFFER)
    [(== GL_FRAMEBUFFER_COMPLETE)
     (void)]
    [x
     (eprintf "FBO creation failed: ~v\n" x)
     (exit 1)])

  (glBindFramebuffer GL_FRAMEBUFFER 0)

  (glDisable GL_TEXTURE_2D)

  (define shader_program (glCreateProgram))

  (define&compile-shader fragment_shader
    GL_FRAGMENT_SHADER
    shader_program fragment-source)

  (define&compile-shader vertex_shader
    GL_VERTEX_SHADER
    shader_program vertex-source)

  (glLinkProgram shader_program)
  (print-shader-log glGetProgramInfoLog 'Program shader_program)

  (glUseProgram shader_program)

  (glUniform2fv
   (glGetUniformLocation shader_program "rubyInputSize")
   1
   (f32vector (* 1. crt-width) (* 1. crt-height)))
  (glUniform2fv
   (glGetUniformLocation shader_program "rubyOutputSize")
   1
   (f32vector (* 1. screen-width) (* 1. screen-height)))
  (glUniform2fv
   (glGetUniformLocation shader_program "rubyTextureSize")
   1
   (f32vector (* 1. texture-width) (* 1. texture-height)))

  (glUseProgram 0)

  (define (draw-on-crt do-the-drawing)
    (glBindFramebuffer GL_FRAMEBUFFER myFBO)

    (glPushAttrib GL_CURRENT_BIT)
    (glPushMatrix)
    (glViewport 0 0 crt-width crt-height)
    (do-the-drawing)
    (glPopMatrix)
    (glPopAttrib)

    (glBindFramebuffer GL_FRAMEBUFFER 0)

    (glUseProgram shader_program)
    (glClearColor 0. 0. 0. 1.)
    (glClear (bitwise-ior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))
    (glMatrixMode GL_PROJECTION)
    (glLoadIdentity)
    (glEnable GL_TEXTURE_2D)
    (glMatrixMode GL_MODELVIEW)
    (glLoadIdentity)
    (glOrtho 0 actual-screen-width 0 actual-screen-height 0. -10.)
    (glViewport 0 0 actual-screen-width actual-screen-height)
    (glBindTexture GL_TEXTURE_2D myTexture)
    (glBegin GL_QUADS)
    (glTexCoord2i 0 0) (glVertex2f inset-left inset-bottom)
    (glTexCoord2i 1 0) (glVertex2f inset-right inset-bottom)
    (glTexCoord2i 1 1) (glVertex2f inset-right inset-top)
    (glTexCoord2i 0 1) (glVertex2f inset-left inset-top)
    (glEnd)
    (glBindTexture GL_TEXTURE_2D 0)
    (glDisable GL_TEXTURE_2D)

    (glUseProgram 0))

  draw-on-crt)

(provide crt-height
         crt-width
         make-draw-on-crt)
