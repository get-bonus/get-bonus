#lang racket/base
(require ffi/vector
         ffi/cvector
         ffi/unsafe
         gb/graphics/gl-util
         racket/match
         (planet stephanh/RacketGL/rgl))

(define-shader-source fragment-source "crt.fragment.glsl")
(define-shader-source vertex-source "crt.vertex.glsl")

;; http://www.songho.ca/opengl/gl_fbo.html
(define myFBO #f)
(define myTexture #f)
(define myRB #f)
;; shader stuff based on bsnes_v085-source/bsnes/ruby/video/opengl.hpp
(define shader_program #f)

;; This width and height is based on the SNES, which was 256x239. The
;; smallest 16:9 rectangle that this fits in is 432x243, so we'll use
;; that. This makes the GBIES basically a "widescreen" SNES.
(define crt-width 432)
(define crt-height 243)

(define texture-width crt-width)
(define texture-height crt-height)
(define (draw-on-crt screen-width screen-height do-the-drawing)
  (unless myFBO
    (glEnable GL_TEXTURE_2D)

    (unless myTexture
      (set! myTexture (u32vector-ref (glGenTextures 1) 0))

      (glBindTexture GL_TEXTURE_2D myTexture)
      (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_LINEAR)
      (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_LINEAR)
      (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S GL_CLAMP)
      (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T GL_CLAMP)
      (glTexImage2D
       GL_TEXTURE_2D 0 GL_RGBA8 texture-width texture-height 0
       GL_RGBA GL_UNSIGNED_BYTE
       0)
      (glBindTexture GL_TEXTURE_2D 0))

    (unless myRB
      (set! myRB (u32vector-ref (glGenRenderbuffers 1) 0))

      (glBindRenderbuffer GL_RENDERBUFFER myRB)
      (glRenderbufferStorage GL_RENDERBUFFER
                             GL_DEPTH_COMPONENT24
                             texture-width texture-height)
      (glBindRenderbuffer GL_RENDERBUFFER 0))

    (set! myFBO (u32vector-ref (glGenFramebuffers 1) 0))

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

    (glDisable GL_TEXTURE_2D))

  (glBindFramebuffer GL_FRAMEBUFFER myFBO)

  (glPushAttrib GL_CURRENT_BIT)
  (glPushMatrix)
  (glViewport 0 0 crt-width crt-height)
  (do-the-drawing)
  (glPopMatrix)
  (glPopAttrib)

  (glBindFramebuffer GL_FRAMEBUFFER 0)

  (unless shader_program
    (set! shader_program (glCreateProgram))

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

    (glUseProgram 0))

  (glUseProgram shader_program)
  (glClearColor 0. 0. 0. 1.)
  (glClear (bitwise-ior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))
  (glMatrixMode GL_PROJECTION)
  (glLoadIdentity)
  (glEnable GL_TEXTURE_2D)
  (glMatrixMode GL_MODELVIEW)
  (glLoadIdentity)
  (glOrtho 0 screen-width 0 screen-height 0. -10.)
  (glViewport 0 0 screen-width screen-height)
  (glBindTexture GL_TEXTURE_2D myTexture)
  (glBegin GL_QUADS)
  (glTexCoord2i 0 0) (glVertex2i 0 0)
  (glTexCoord2i 1 0) (glVertex2i screen-width 0)
  (glTexCoord2i 1 1) (glVertex2i screen-width screen-height)
  (glTexCoord2i 0 1) (glVertex2i 0 screen-height)
  (glEnd)
  (glBindTexture GL_TEXTURE_2D 0)
  (glDisable GL_TEXTURE_2D)

  (glUseProgram 0))

(provide crt-height
         crt-width
         draw-on-crt)
