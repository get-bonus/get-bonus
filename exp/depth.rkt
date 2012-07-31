#lang racket/gui
(require sgl
         sgl/gl)

(define frame
  (new frame% [label "Example"]))

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
              (gl-viewport 0 0 width height)
              (gl-matrix-mode 'projection)
              (gl-load-identity)
              (gl-enable 'depth-test)

              (gl-matrix-mode 'modelview)
              (gl-load-identity)
              (gl-push-matrix)
              ;; The trick: http://mlucassmith.tumblr.com/post/10869898438/why-i-hate-glortho-and-all-its-offspring
              (glOrtho 0 width 0 height 0. -11.)

              (gl-push-attrib)
              (glClearColor 0. 0. 0. 1.)
              (gl-clear 'color-buffer-bit 'depth-buffer-bit)

              (gl-push-matrix)
              (gl-translate 0. 0. 5.)
              (glColor4f 0. 1. 0. 1.)
              (glRectf 0 0 (/ width 4) (/ height 4))
              (gl-pop-matrix)

              (gl-push-matrix)
              (gl-translate 0. 0. 0.)
              (glColor4f 0. 0. 1. 1.)
              (glRectf 0 0 (/ width 8) (/ height 8))
              (gl-pop-matrix)

              (gl-push-matrix)
              (gl-translate 0. 0. 10.)
              (glColor4f 1. 0. 0. 1.)
              (glRectf 0 0 (/ width 2) (/ height 2))
              (gl-pop-matrix)

              (gl-pop-attrib)

              (gl-pop-matrix)

              (send gc swap-buffers)
              (gl-flush))))

    (super-new)))

(define config
  (new gl-config%))
(send config set-double-buffered #t)

(define c
  (new this-canvas%
       [parent frame]
       [gl-config config]
       [style '(gl no-autoclear)]))

(send frame show #t)
