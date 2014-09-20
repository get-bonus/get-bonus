#lang racket/base
(require ffi/vector
         ffi/cvector
         ffi/unsafe/cvector
         ffi/unsafe
         gb/graphics/gl-util
         racket/match
         opengl)
(module+ test
  (require rackunit))

(define-syntax-rule (log* e ...) (begin (log e) ...))
(define-syntax-rule (log e) (begin (printf "~v\n" `e) e))

(define-shader-source fragment-source "crt.fragment.glsl")
(define-shader-source vertex-source "crt.vertex.glsl")

;; This width and height is based on the SNES, which was 256x239. The
;; smallest 16:9 rectangle that this fits in is 432x243, which is
;; crt-scale 27, but this makes it so that we have an odd number in
;; various places. So, we'll use crt-scale 28, or 448x252. This makes
;; the GBIES basically a "widescreen" SNES.
;;
;; But, it is good to have the resolution always divisible by 8, 16,
;; and 32, which are common sprite sizes. (Just 16 would probably be
;; okay, but that is smaller than the SNES in height.)
;;
;; But, the small SNES size was 256x224, which is very close to scale
;; 25 or scale 26, which would be nice and pure
(define crt-scale 32)
(define crt-width (* crt-scale 16))
(define crt-height (* crt-scale 9))
;; XXX what text terminal dimensions does this give?

;; xxx 400x240 is what shovel knight does

;; FBO stuff based on: http://www.songho.ca/opengl/gl_fbo.html

;; shader stuff based on
;; :bsnes_v085-source/bsnes/ruby/video/opengl.hpp

;; We want to find how to scale the CRT to the real screen, but it is
;; important to only use powers of two in the decimals and only up to
;; 2^5
(define (quotient* x y)
  (define-values (q r) (quotient/remainder x y))
  (define (recur r i max-i)
    (cond
      [(= i max-i)
       0]
      [else
       (define d (expt 2 (* -1 i)))
       (define dy (* d y))
       (cond
         [(> dy r)
          (recur r (add1 i) max-i)]
         [else
          (+ d (recur (- r dy) (add1 i) max-i))])]))
  (+ q (recur r 1 5)))
(module+ test
  (define-syntax-rule (check-1q name x y e-r)
    (begin
      (define a-r (quotient* x y))
      (check-= a-r e-r 0
               (format "~a: ~a vs ~a"
                       name
                       (exact->inexact a-r)
                       (exact->inexact e-r)))))
  (define-syntax-rule (check-q* name (w h) (e-ws e-hs))
    (begin
      (check-1q (format "~a width(~a)" name w) w crt-width e-ws)
      (check-1q (format "~a height(~a)" name h) h crt-height e-hs)))

  (define ws 1)
  (define hs 1)

  (check-q* "PS Vita"
            (960 544)
            ((+ 1 1/2 1/4 1/8)
             (+ 1 1/2 1/4 1/8)))
  (check-q* "iPhone 4"
            (960 640)
            ((+ 1 1/2 1/4 1/8)
             (+ 2 1/8 1/16)))
  (check-q* "Normal laptop"
            (1024 640)
            (2
             (+ 2 1/8 1/16)))
  (check-q* "iPhone 5"
            (1136 640)
            ((+ 2 1/8 1/16)
             (+ 2 1/8 1/16)))
  (check-q* "720p"
            (1280 720)
            ((+ 2 1/2)
             (+ 2 1/2)))
  (check-q* "1080p"
            (1920 1080)
            ((+ 3 1/2 1/4)
             (+ 3 1/2 1/4)))
  (check-q* "MacBook Pro Retina, Arch"
            (1440 900)
            ((+ 2 1/2 1/4 1/16)
             (+ 3 1/8))))

(define (make-draw-on-crt actual-screen-width actual-screen-height)
  (eprintf "You are using OpenGL ~a\n"
           (gl-version))

  (define texture-width crt-width)
  (define texture-height crt-height)

  (define scale
    (* 1.
       (min (quotient* actual-screen-width crt-width)
            (quotient* actual-screen-height crt-height))))

  (define screen-width (* scale crt-width))
  (define screen-height (* scale crt-height))

  (define inset-left (/ (- actual-screen-width screen-width) 2.))
  (define inset-right (+ inset-left screen-width))
  (define inset-bottom (/ (- actual-screen-height screen-height) 2.))
  (define inset-top (+ inset-bottom screen-height))

  (define myTexture (u32vector-ref (glGenTextures 1) 0))

  (glBindTexture GL_TEXTURE_2D myTexture)
  (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_NEAREST)
  (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_NEAREST)
  (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S GL_CLAMP_TO_EDGE)
  (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T GL_CLAMP_TO_EDGE)
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

  (define shader_program (glCreateProgram))
  (glBindAttribLocation shader_program 0 "iTexCoordPos")

  (define&compile-shader fragment_shader
    GL_FRAGMENT_SHADER
    shader_program fragment-source)

  (define&compile-shader vertex_shader
    GL_VERTEX_SHADER
    shader_program vertex-source)

  (glLinkProgram shader_program)
  (print-shader-log glGetProgramInfoLog 'Program shader_program)

  (glUseProgram shader_program)

  (glUniform1i 
   (glGetUniformLocation shader_program "rubyTexture")
   0)
  (glUniform2fv
   (glGetUniformLocation shader_program "rubyInputSize")
   1
   (f32vector (* 1. crt-width) (* 1. crt-height)))
  (glUniform2fv
   (glGetUniformLocation shader_program "rubyOutputSize")
   1
   ;; xxx this might have to be without actual-
   (f32vector (* 1. actual-screen-width) (* 1. actual-screen-height)))
  (glUniform2fv
   (glGetUniformLocation shader_program "rubyTextureSize")
   1
   (f32vector (* 1. texture-width) (* 1. texture-height)))

  (glUseProgram 0)

  ;; xxx turn this entirely into a shader
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

  (define VaoId (u32vector-ref (glGenVertexArrays 1) 0))
  (glBindVertexArray VaoId)
  (define VboId (u32vector-ref (glGenBuffers 1) 0))
  (glBindBuffer GL_ARRAY_BUFFER VboId)

  (define DataWidth 4)
  (define DataSize 4)
  (define DataCount 6)
  (glVertexAttribPointer 0 DataSize GL_FLOAT #f 0 0)
  (glEnableVertexAttribArray 0)

  (glBufferData GL_ARRAY_BUFFER (* DataCount DataWidth DataSize) #f GL_STATIC_DRAW)

  (define DataVec
    (make-cvector*
     (glMapBufferRange
      GL_ARRAY_BUFFER
      0 
      (* DataCount DataSize)
      GL_MAP_WRITE_BIT)
     _float
     (* DataWidth
        DataSize
        DataCount)))
  (define (cvector-set*! vec k . vs)
    (for ([v (in-list vs)]
          [i (in-naturals)])
      (cvector-set! vec (+ k i) v)))
  (cvector-set*! DataVec 0
                 0.0 0.0 inset-left inset-bottom
                 1.0 0.0 inset-right inset-bottom
                 1.0 1.0 inset-right inset-top
                 
                 0.0 1.0 inset-left inset-top
                 1.0 1.0 inset-right inset-top
                 0.0 0.0 inset-left inset-bottom)
  (glUnmapBuffer GL_ARRAY_BUFFER)
  (set! DataVec #f)

  (glBindBuffer GL_ARRAY_BUFFER 0)
  (glBindVertexArray 0)

  (define (new-draw-on-crt do-the-drawing)
    (glBindFramebuffer GL_FRAMEBUFFER myFBO)
    (glViewport 0 0 crt-width crt-height)
    (do-the-drawing)
    (glBindFramebuffer GL_FRAMEBUFFER 0)

    (glBindVertexArray VaoId)
    (glEnableVertexAttribArray 0)
    (glUseProgram shader_program)
    (glClearColor 0. 0. 0. 0.)
    (glClear (bitwise-ior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))
    (glViewport 0 0 actual-screen-width actual-screen-height)
    (glActiveTexture GL_TEXTURE0)
    (glBindTexture GL_TEXTURE_2D myTexture)
    (glDrawArrays GL_TRIANGLES 0 DataCount)

    (glActiveTexture GL_TEXTURE0)
    (glBindTexture GL_TEXTURE_2D 0)
    (glUseProgram 0)
    (glDisableVertexAttribArray 0)
    (glBindVertexArray 0))

  (define (fake-draw-on-crt do-the-drawing)
    (do-the-drawing))

  new-draw-on-crt)

(provide crt-height
         crt-width
         make-draw-on-crt)
