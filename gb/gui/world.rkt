#lang racket/base
(require racket/runtime-path
         racket/contract
         racket/list
         racket/format
         gb/gui/fullscreen
         gb/input/keyboard
         gb/data/psn
         gb/audio/3s
         gb/graphics/crt
         gb/graphics/ngl-main
         gb/input/controller)

(define RATE 1/60)

(define nested? (make-parameter #f))
(define current-controllers (make-parameter (λ () empty)))
(define current-update-canvas (make-parameter #f))

(define (big-bang initial-world
                  #:tick tick
                  #:sound-scale [sound-scale 1.0]
                  #:listener [world->listener (λ (w) (psn 0. 0.))]
                  #:done? [done? (λ (w) #f)])
  (if (nested?)
    (dynamic-wind
        pause-last-sound
        (λ ()
          (nested-big-bang initial-world tick sound-scale
                           world->listener done?))
        unpause-last-sound)
    (outer-big-bang initial-world tick sound-scale
                    world->listener done?)))

(define current-sound-ctxt (make-parameter #f))
(define current-sound (make-parameter #f))
(define (pause-last-sound)
  (sound-pause! (current-sound)))
(define (unpause-last-sound)
  (sound-unpause! (current-sound)))

(define (nested-big-bang initial-world tick sound-scale
                         world->listener done?)
  (define st
    (initial-system-state
     (current-sound-ctxt)
     world->listener))
  (dynamic-wind
      void
      (λ ()
        (let loop ([frame 0]
                   [w initial-world])
          (define next-time
            (+ (current-inexact-milliseconds) (* RATE 1000)))
          (parameterize ([current-sound st])
            (define-values (wp cmd ss)
              (parameterize ([current-frame frame])
                (call-with-continuation-barrier
                 (λ ()
                   (tick w
                         (controller-monitor-state
                          (current-controllers)))))))
            ((current-update-canvas) cmd)
            (cond
              [(done? wp)
               wp]
              [else
               ;; XXX This is implies that we could switch sounds while
               ;; rendering the next sound... which is bad.
               (define stp
                 (render-sound sound-scale st ss wp))
               (set! st stp)
               (sync (alarm-evt next-time))
               (loop (add1 frame) wp)]))))
      (λ ()
        (sound-destroy! st))))

(define-runtime-path texture-atlas-path "../../r.png")

(define current-frame
  (make-parameter 0))
(define (outer-big-bang initial-world tick sound-scale
                        world->listener done?)
  (define km
    (keyboard-monitor))
  (define cm
    (make-controller-monitor
     #:keyboard km))

  (define last-sprites #f)
  (define draw-on-crt #f)
  (define draw-sprites #f)
  (define frame-time 1000.00)
  (define-values
    (set-label! refresh! done-sema)
    (make-fullscreen-canvas
     (λ (w h done!)
       (define start (current-inexact-milliseconds))
       (unless draw-on-crt
         (set! draw-on-crt
               (make-draw-on-crt w h)))
       (unless draw-sprites
         (set! draw-sprites
               (make-draw texture-atlas-path
                          texture-atlas-size
                          (* 1.0 crt-width)
                          (* 1.0 crt-height))))
       (when last-sprites
         (draw-on-crt (λ () (draw-sprites last-sprites))))
       (done!)
       (define stop (current-inexact-milliseconds))
       (set! frame-time (- stop start)))
     (λ (k)
       (keyboard-monitor-submit! km k))))

  (define (this-update-canvas cmd)
    (set! last-sprites cmd)
    (set-label!
     (~a "Frame time: "
         (~r frame-time
             #:min-width 5
             #:precision 1)
         "; "
         "FPS: "
         (~r (/ 1000 frame-time)
             #:min-width 7
             #:precision 2)))
    (refresh!))

  (define the-ctxt (make-sound-context))
  (dynamic-wind
      void
      (λ ()
        (parameterize
            ([current-sound-ctxt the-ctxt]
             [nested? #t]
             [current-controllers cm]
             [current-update-canvas this-update-canvas])
          (nested-big-bang initial-world tick sound-scale
                           world->listener done?)))
      (λ ()
        (sound-context-destroy! the-ctxt)
        (semaphore-post done-sema))))

(provide/contract
 [RATE number?]
 [current-frame (-> exact-nonnegative-integer?)]
 [big-bang
  (->* (any/c
        #:tick (-> any/c (listof controller?)
                   (values any/c sprite-tree/c sound-scape/c)))
       (#:sound-scale real?
                      #:listener (-> any/c psn?)
                      #:done? (-> any/c boolean?))
       any/c)])
