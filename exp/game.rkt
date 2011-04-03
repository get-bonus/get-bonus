#lang racket/gui
(require racket/runtime-path
         (prefix-in gl: "gl.rkt")
         "fullscreen.rkt"
         "joystick.rkt")

(define-runtime-path resource-path "../resources")
(define sprite-sheet-text (gl:texture (build-path resource-path "SMB-Tiles.png")))
(define tile-texture (gl:sprite-sheet sprite-sheet-text 16 1))

(define PX 8)
(define PY 4.5)

(define the-canvas
  (make-fullscreen-canvas/ratio 
   "Example"
   16 9 
   (λ (c c-scale)
     (define dc (send c get-dc))
     (define glctx (send dc get-gl-context))
     (send glctx call-as-current
           (λ () 
             (gl:draw 
              (* 2 16) (* 2 9) 16 9 PX PY
              (gl:background
               255 255 0 0
               (gl:color 1 1 1 1
                         (gl:translate 0 0
                                       (tile-texture 0 1))
                         (gl:translate 8 4.5
                                       (gl:texture/scale sprite-sheet-text 1 1))
                         (gl:translate 10 7
                                       (gl:scale 0.05 0.05
                                                 sprite-sheet-text))
                         (gl:translate 16 9
                                       (tile-texture 1 0)))
               (gl:color 1 1 1 1
                         (gl:translate PX PY
                                       (tile-texture 0 0)))))
             (send glctx swap-buffers))))
   (λ (k)
     (void))))

(define RATE 1/60)

(thread
 (λ ()
   (define jss (get-all-joystick-snapshot-thunks))
   (let loop ()
     (for ([js (in-list jss)]
           [i (in-naturals)])
       (define s (js))
       (set! PX
             (+ PX
                (* RATE 16/2 (mvector-ref (joystick-state-sticks s) 0 0 0))))
       (set! PY
             (+ PY
                (* RATE 9/2 (mvector-ref (joystick-state-sticks s) 0 0 1)))))
     (send the-canvas refresh-now)
     (sleep RATE)
     (loop))))
