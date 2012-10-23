#lang racket/base
(require racket/class
         racket/match
         racket/runtime-path
         gb/gui/fullscreen
         gb/graphics/r
         gb/graphics/string
         gb/graphics/ngl
         gb/graphics/ngli)

(define-runtime-path texture-atlas-path "../r.png")

(define modern/12/string
  (make-string-factory (make-char-factory modern 12)))

(module+ main
  (define Frame 0)

  (define HowManySprites (* 4 512))
  (define objects

    (list
     (transform #:d 4.0 6.0 #:rgba 1.0 0.0 0.0 1.0
                (rectangle 0.5 0.5))
     (transform #:d 12.0 6.0 #:rgba 0.0 1.0 0.0 1.0 #:m 1.0 1.0
                (rectangle 0.5 0.5))
     (transform #:d 4.0 3.0 #:rgba 0.0 0.0 1.0 1.0 #:m 0.5 0.5
                (rectangle 0.5 0.5))
     (transform #:d 12.0 3.0 #:rgba 0.0 0.0 0.0 1.0 #:m 2.0 2.0
                (rectangle 0.5 0.5)))

    #;
    (transform
     #:d 8.0 4.5
     (modern/12/string "TEST" #:hw 0.5 #:hh 0.5))

    #;

    (list
    (transform
    #:d 8.0 4.5
    (rectangle 0.5 0.5
    fonts/modern/12/T))
    (transform
    #:d 9.0 4.5
    (rectangle 0.5 0.5
    fonts/modern/12/E))

    (transform
    #:d 10.0 4.5
    (rectangle 0.5 0.5
    fonts/modern/12/S))

    (transform
    #:d 11.0 4.5
    (rectangle 0.5 0.5
    fonts/modern/12/T)))
    #;
    (for/list ([i (in-range HowManySprites)])
    (transform
    #:d
    (random-in 5.0 10.0)
    (random-in 3.0  6.0)
    #:rgba
    (random) (random)
    (random) (random)
    #:m
    (random) (random)
    #:rot
    (random)

    (rectangle
    (random) (random)
    (list-ref
    (list none
    tennis/bg
    tennis/ball
    tennis/paddle)
    (random 4))))))

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
                        texture-atlas-size
                        16.0 9.0)))

               (set! Frame (modulo (add1 Frame) 60))

               #;
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
