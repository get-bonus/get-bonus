#lang racket/gui
(require ffi/vector
         ffi/cvector
         ffi/unsafe
         (planet stephanh/RacketGL/rgl))

(module+ main
  (define frame
    (new frame% [label "Example"]))

  ;; http://www.songho.ca/opengl/gl_fbo.html
  (define myFBO #f)
  (define myTexture #f)
  (define myRB #f)
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
                (glPopAttrib)

                (glBindFramebuffer GL_FRAMEBUFFER 0)

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
