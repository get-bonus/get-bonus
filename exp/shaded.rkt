#lang racket/gui
(require ffi/vector
         ffi/cvector
         ffi/unsafe
         racket/runtime-path
         xml
         (planet stephanh/RacketGL/rgl))

(define (cdata-string* c)
  (define s (cdata-string c))
  (substring s 9 (- (string-length s) 3)))

(module+ test
  (require rackunit)
  (check-equal? (cdata-string* (cdata #f #f "<![CDATA[‹content›]]>"))
                "‹content›"))

(module+ main
  (define-runtime-path resources "../resources")
  (define shader-path
    (build-path resources "CRT-Geom-Interlaced"
                "single-pass" "crt-geom-interlaced-curved.shader"))

  (require racket/pretty)

  (match-define
   (list
    'shader
    '((language "GLSL")) "\r\n    "
    (list 'vertex '() vertex-cdata)
    "\r\n    "
    (list 'fragment '() fragment-cdata)
    "\r\n")
   (xml->xexpr
    (document-element
     (call-with-input-file shader-path read-xml))))

  (define fragment_source (cdata-string* fragment-cdata))
  (define vertex_source (cdata-string* vertex-cdata)))

(module+ main
  (define frame
    (new frame% [label "Example"]))

  ;; http://www.songho.ca/opengl/gl_fbo.html
  (define myFBO #f)
  (define myTexture #f)
  (define myRB #f)
  ;; shader stuff based on bsnes_v085-source/bsnes/ruby/video/opengl.hpp
  (define shader_program #f)
  (define this-canvas%
    (class canvas%
      (define/override (on-size width height)
        (define dc
          (send this get-dc))
        (define gc
          (send dc get-gl-context))
        (send gc
              call-as-current
              (λ ()
                (define the-w 432)
                (define the-h 243)

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
                     GL_TEXTURE_2D 0 GL_RGBA8 the-w the-h 0
                     GL_RGBA GL_UNSIGNED_BYTE
                     0)
                    (glBindTexture GL_TEXTURE_2D 0))

                  (unless myRB
                    (set! myRB (u32vector-ref (glGenRenderbuffers 1) 0))

                    (glBindRenderbuffer GL_RENDERBUFFER myRB)
                    (glRenderbufferStorage GL_RENDERBUFFER
                                           GL_DEPTH_COMPONENT24
                                           the-w the-h)
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
                     (printf "good: ~v\n" (list myFBO myTexture myRB))]
                    [x
                     (printf "bad: ~v\n" x)
                     (exit 1)])

                  (glBindFramebuffer GL_FRAMEBUFFER 0)

                  (glDisable GL_TEXTURE_2D))

                (glBindFramebuffer GL_FRAMEBUFFER myFBO)

                (glPushAttrib GL_CURRENT_BIT)
                (glPushMatrix)
                (glClearColor 1. 1. 1. 1.)
                (glClear (bitwise-ior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))
                (glViewport 0 0 the-w the-h)
                (glMatrixMode GL_PROJECTION)
                (glLoadIdentity)
                (glOrtho 0 the-w 0 the-h 0. -10.)
                (glMatrixMode GL_MODELVIEW)
                (glLoadIdentity)
                (glEnable GL_DEPTH_TEST)
                ;; Otherwise we won't see the red square
                (glDepthFunc GL_LEQUAL)

              ;;; THE ACTUAL DRAWING
                ;; Red is big
                (glPushMatrix)
                (glTranslatef 0. 0. 10.)
                (glColor4f 1. 0. 0. 1.)
                (glRectf 0. 0. (/ the-w 2.) (/ the-h 2.))
                (glPopMatrix)

                ;; Blue is small
                (glPushMatrix)
                (glTranslatef 0. 0. 0.)
                (glColor4f 0. 0. 1. 1.)
                (glRectf 0. 0. (/ the-w 8.) (/ the-h 8.))
                (glPopMatrix)

                ;; Green is medium
                (glPushMatrix)
                (glTranslatef 0. 0. 5.)
                (glColor4f 0. 1. 0. 1.)
                (glRectf 0. 0. (/ the-w 4.) (/ the-h 4.))
                (glPopMatrix)

                ;; THE ACTUAL DRAWING DONE
                (glPopMatrix)
                (glPopAttrib)

                (glBindFramebuffer GL_FRAMEBUFFER 0)

                (unless shader_program
                  (set! shader_program (glCreateProgram))

                  (define fragment_shader (glCreateShader GL_FRAGMENT_SHADER))
                  (glShaderSource fragment_shader 1 (vector fragment_source)
                                  (s32vector (string-length fragment_source)))
                  (glCompileShader fragment_shader)

                  (define (check-shader-compilation which fragment_shader)
                    (define shader-vec (s32vector 0))
                    (glGetShaderiv fragment_shader GL_COMPILE_STATUS shader-vec)
                    (unless (= 1 (s32vector-ref shader-vec 0))
                      (define status (s32vector-ref shader-vec 0))
                      (glGetShaderiv fragment_shader GL_INFO_LOG_LENGTH shader-vec)
                      (define max-length (s32vector-ref shader-vec 0))
                      (define-values (len log-bs)
                        (glGetShaderInfoLog fragment_shader max-length))
                      (eprintf "~a failed to compile: ~a\n"
                               which log-bs)
                      (exit 1)))

                  (check-shader-compilation "fragment shader" fragment_shader)

                  (glAttachShader shader_program fragment_shader)

                  (define vertex_shader (glCreateShader GL_VERTEX_SHADER))
                  (glShaderSource vertex_shader 1 (vector vertex_source)
                                  (s32vector (string-length vertex_source)))
                  (glCompileShader vertex_shader)
                  (check-shader-compilation "vertex shader" vertex_shader)

                  (glAttachShader shader_program vertex_shader)

                  (glLinkProgram shader_program))

                (glUseProgram shader_program)
                (glUniform2fv
                 (glGetUniformLocation shader_program "rubyInputSize")
                 1
                 (f32vector (* 1. the-w) (* 1. the-h)))
                (glUniform2fv
                 (glGetUniformLocation shader_program "rubyOutputSize")
                 1
                 (f32vector (* 1. width) (* 1. height)))
                (glUniform2fv
                 (glGetUniformLocation shader_program "rubyTextureSize")
                 1
                 (f32vector (* 1. the-w) (* 1. the-h)))
                (glClearColor 0. 0. 0. 1.)
                (glClear (bitwise-ior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))
                (glMatrixMode GL_PROJECTION)
                (glLoadIdentity)
                (glEnable GL_TEXTURE_2D)
                (glEnable GL_BLEND)
                (glMatrixMode GL_MODELVIEW)
                (glLoadIdentity)
                (glOrtho 0 width 0 height 0. -10.)
                (glViewport 0 0 width height)
                (glTexEnvi GL_TEXTURE_ENV GL_TEXTURE_ENV_MODE GL_MODULATE)
                (glBindTexture GL_TEXTURE_2D myTexture)
                (glBegin GL_QUADS)
                (glTexCoord2i 0 0) (glVertex2i 0 0)
                (glTexCoord2i 1 0) (glVertex2i width 0)
                (glTexCoord2i 1 1) (glVertex2i width height)
                (glTexCoord2i 0 1) (glVertex2i 0 height)
                (glEnd)
                (glBindTexture GL_TEXTURE_2D 0)
                (glDisable GL_TEXTURE_2D)
                (glFlush)

                (glUseProgram 0)

                (send gc swap-buffers))))

      (super-new)))

  (define config
    (new gl-config%))
  (send config set-double-buffered #t)

  (define c
    (new this-canvas%
         [parent frame]
         [gl-config config]
         [style '(gl no-autoclear)]))

  (send frame show #t))
