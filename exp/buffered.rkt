#lang racket/gui
(require sgl
         sgl/gl
         sgl/gl-vectors
         ffi/vector
         ffi/cvector
         ffi/unsafe
         (prefix-in rgl: (planet stephanh/RacketGL/rgl)))

(module+ main
  (define frame
    (new frame% [label "Example"]))

  (define myFBO #f)
  (define myTexture #f)
  (define myRB #f)
  (define myTextureBuffer #f)
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
                  (unless myTexture
                    (set! myTexture (gl-vector-ref (glGenTextures 1) 0))

                    (glBindTexture GL_TEXTURE_2D myTexture)
                    (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_LINEAR)
                    (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_LINEAR)
                    (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S GL_CLAMP)
                    (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T GL_CLAMP)
                    (rgl:glTexImage2D
                     GL_TEXTURE_2D 0 GL_RGBA the-w the-h 0
                     GL_RGBA GL_UNSIGNED_BYTE
                     0)
                    (glBindTexture GL_TEXTURE_2D 0))

                  (unless myRB
                    (set! myRB (u32vector-ref (rgl:glGenRenderbuffers 1) 0))

                    (rgl:glBindRenderbuffer rgl:GL_RENDERBUFFER myRB)
                    (rgl:glRenderbufferStorage rgl:GL_RENDERBUFFER
                                               rgl:GL_DEPTH_COMPONENT
                                               the-w the-h)
                    (rgl:glBindRenderbuffer rgl:GL_RENDERBUFFER 0))

                  (set! myFBO (u32vector-ref (rgl:glGenFramebuffers 1) 0))

                  (rgl:glBindFramebuffer rgl:GL_FRAMEBUFFER myFBO)
                  (rgl:glFramebufferTexture2D
                   rgl:GL_FRAMEBUFFER
                   rgl:GL_COLOR_ATTACHMENT0
                   rgl:GL_TEXTURE_2D myTexture 0)
                  (rgl:glFramebufferRenderbuffer
                   rgl:GL_FRAMEBUFFER
                   rgl:GL_DEPTH_ATTACHMENT
                   rgl:GL_RENDERBUFFER
                   myRB)

                  (match (rgl:glCheckFramebufferStatus rgl:GL_FRAMEBUFFER)
                    [(== rgl:GL_FRAMEBUFFER_COMPLETE)
                     (printf "good: ~v\n" (list myFBO myTexture myRB))]
                    [x
                     (printf "bad: ~v\n" x)
                     (exit 1)])

                  (rgl:glBindFramebuffer rgl:GL_FRAMEBUFFER 0))

                (rgl:glBindFramebuffer rgl:GL_FRAMEBUFFER myFBO)

                (gl-push-attrib)
                (gl-push-matrix)
                (glClearColor 1. 1. 1. 1.)
                (gl-clear 'color-buffer-bit 'depth-buffer-bit)
                (gl-viewport 0 0 the-w the-h)
                (gl-matrix-mode 'projection)
                (gl-load-identity)
                ;; The trick: http://mlucassmith.tumblr.com/post/10869898438/why-i-hate-glortho-and-all-its-offspring
                (glOrtho 0 the-w 0 the-h 0. -10.)
                (gl-matrix-mode 'modelview)
                (gl-load-identity)
                (gl-disable 'texture-2d)
                (gl-disable 'blend)
                (gl-enable 'depth-test)
                ;; Otherwise we won't see the red square
                (gl-depth-func 'lequal)

              ;;; THE ACTUAL DRAWING
                ;; Red is big
                (gl-push-matrix)
                (gl-translate 0. 0. 10.)
                (glColor4f 1. 0. 0. 1.)
                (glRectf 0 0 (/ the-w 2) (/ the-h 2))
                (gl-pop-matrix)

                ;; Blue is small
                (gl-push-matrix)
                (gl-translate 0. 0. 0.)
                (glColor4f 0. 0. 1. 1.)
                (glRectf 0 0 (/ the-w 8) (/ the-h 8))
                (gl-pop-matrix)

                ;; Green is medium
                (gl-push-matrix)
                (gl-translate 0. 0. 5.)
                (glColor4f 0. 1. 0. 1.)
                (glRectf 0 0 (/ the-w 4) (/ the-h 4))
                (gl-pop-matrix)

                ;; THE ACTUAL DRAWING DONE
                (gl-pop-matrix)
                (gl-pop-attrib)

                (rgl:glBindFramebuffer rgl:GL_FRAMEBUFFER 0)

                (glClearColor 0. 0. 0. 1.)
                (gl-clear 'color-buffer-bit 'depth-buffer-bit)
                (gl-matrix-mode 'projection)
                (gl-load-identity)
                (gl-enable 'texture-2d)
                (gl-enable 'blend)
                (gl-matrix-mode 'modelview)
                (gl-load-identity)
                (glOrtho 0 width 0 height 0. -10.)
                (gl-viewport 0 0 width height)
                (glTexEnvf GL_TEXTURE_ENV GL_TEXTURE_ENV_MODE GL_MODULATE)
                (glBindTexture GL_TEXTURE_2D myTexture)
                (gl-begin 'quads)
                (gl-tex-coord 0 0) (gl-vertex 0 0)
                (gl-tex-coord 1 0) (gl-vertex width 0)
                (gl-tex-coord 1 1) (gl-vertex width height)
                (gl-tex-coord 0 1) (gl-vertex 0 height)
                (gl-end)
                (glBindTexture GL_TEXTURE_2D 0)
                (gl-disable 'texture-2d)

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
