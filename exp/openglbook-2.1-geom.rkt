#lang racket/base
(require "../r.rkt"
         racket/class
         racket/runtime-path
         gb/gui/fullscreen
         gb/graphics/ngl)

(define-runtime-path texture-atlas-path "../r.png")

(module+ main
  (define Frame 0)

  (define HowManySprites (* 4 512))
  (define objects
    (for/list ([i (in-range HowManySprites)])
      (sprite-info
       (random-in -1.0 1.0)
       (random-in -1.0 1.0)
       (random)
       (random)

       (random)
       (random)
       (random)
       (random)

       (list-ref
        (list none
              tennis/bg
              tennis/ball
              tennis/paddle)
        (random 4))

       (random)
       (random)
       (random))))

  (define draw #f)

  (define-values
    (the-frame the-canvas)
    (make-fullscreen-canvas/ratio
     ""
     16 9
     (λ (c)
       (define dc (send c get-dc))
       (define glctx (send dc get-gl-context))

       (send glctx call-as-current
             (λ ()
               (unless draw
                 (set! draw
                       (make-draw
                        texture-atlas-path
                        texture-atlas-width texture-atlas-height
                        (send c get-width)
                        (send c get-height))))

               (set! Frame (modulo (add1 Frame) 60))

               (when (zero? Frame)
                 (set! objects
                       (for/list ([o (in-list objects)])
                         (struct-copy sprite-info o
                                      [theta
                                       (add1 (sprite-info-theta o))]))))

               (draw objects)
               (send glctx swap-buffers))))
     (λ (k)
       (void))))

  (let loop ()
    (define before
      (current-inexact-milliseconds))
    (define next-time
      (+ before (* 1/60 1000)))
    (send the-canvas refresh-now)
    (define after
      (current-inexact-milliseconds))
    (send the-frame set-label
          (format "~a FPS"
                  (real->decimal-string
                   (/ 1 (/ (- after before) 1000))
                   2)))
    (sync (alarm-evt next-time))
    (loop)))

(define (random-in lo hi)
  (define rng (- hi lo))
  (+ lo (* (random) (+ rng 1))))
