#lang racket/gui
(require racket/runtime-path
         (prefix-in gl: "gl.rkt")
         "sprites.rkt"
         "mvector.rkt"
         "fullscreen.rkt"
         "joystick.rkt")

(define-runtime-path resource-path "../resources")

(define map-text 
  (gl:path->texture (build-path resource-path "IMB" "mapsheet.png")))
(define map-sprites
  (sprite-sheet/row-major map-text 16))
(define width 320)
(define height 40)
(define map-bytes
  (file->bytes (build-path resource-path "IMB" "out.lvl")))

(define PX 8)
(define PY 4.5)

(define the-background
  (gl:for*/gl 
   ([r (in-range height)]
    [c (in-range width)])
   (define b
     (bytes-ref map-bytes
                (+ (* height c) r)))
   (if (zero? b)
       gl:blank
       (gl:translate c (- height r 1)
                     (map-sprites b)))))

(define the-canvas
  (make-fullscreen-canvas/ratio 
   "Example"
   16 9 
   (λ (c)
     (define dc (send c get-dc))
     (define glctx (send dc get-gl-context))
     (send glctx call-as-current
           (λ () 
             (gl:draw 
              ; Show whole map
              ;width height (* 16 20) (* 9 20)
              width height (* 16 4) (* 9 4)
              PX PY
              (gl:background
               255 255 255 0
               the-background
               (gl:translate PX PY
                             (map-sprites 5))))
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
