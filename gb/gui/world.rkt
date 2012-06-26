#lang racket/base
(require racket/gui/base
         racket/contract
         racket/class
         racket/list
         gb/gui/fullscreen
         gb/input/keyboard
         gb/data/psn
         gb/audio/3s
         gb/data/ltq
         (prefix-in gl: gb/graphics/gl)
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

(define current-sound (make-parameter #f))
(define (pause-last-sound)
  (sound-pause! (current-sound)))
(define (unpause-last-sound)
  (sound-unpause! (current-sound)))

(define (nested-big-bang initial-world tick sound-scale
                         world->listener done?)
  (let loop ([w initial-world]
             [st (initial-system-state world->listener)])
    (define next-time
      (+ (current-inexact-milliseconds) (* RATE 1000)))
    (parameterize ([current-sound st])
      (define-values (wp cmd ss)
        (tick w (controller-monitor-state (current-controllers))))
      ((current-update-canvas) cmd)
      (if (done? wp)
        (let ()
          (sound-destroy! st)
          wp)
        (let ()
          ;; XXX This is implies that we could switch sounds while
          ;; rendering the next sound... which is bad.
          (define stp
            (render-sound sound-scale st ss wp))
          (sync (alarm-evt next-time))
          (loop wp stp))))))

(define current-rate-finder (make-parameter (λ () (error 'current-rate "Not in big-bang"))))
(define (current-rate)
  ((current-rate-finder)))
(define (outer-big-bang initial-world tick sound-scale
                        world->listener done?)
  (define km
    (keyboard-monitor))
  (define cm
    (make-controller-monitor 
     #:keyboard km))

  (define last-cmd #f)
  (define-values
    (the-frame the-canvas)
    (make-fullscreen-canvas/ratio
     ""
     16 9
     (λ (c)
       (define dc (send c get-dc))
       (define glctx (send dc get-gl-context))
       (unless glctx
         (error 'loop "Could not initialize OpenGL!")
         ;; XXX should bring down the whole thing
         (exit 1))
       (send glctx call-as-current
             (λ ()
               (when last-cmd (gl:draw last-cmd))
               (send glctx swap-buffers))))
     (λ (k)
       (keyboard-monitor-submit! km k))))

  (define frame-ltq (ltq (/ (* 2 60) RATE)))
  (define (this-update-canvas cmd)
    (ltq-add! frame-ltq)
    (set! last-cmd cmd)
    (send the-canvas refresh-now))

  (define done-ch (make-channel))
  (define ticker
    (thread
     (λ ()
       (channel-put
        done-ch
        (parameterize
            ([nested? #t]
             [current-rate-finder
              (λ ()
                (with-handlers ([exn:fail? (λ (x) 0.)])
                  (exact->inexact
                   (/ (ltq-count frame-ltq)
                      (- (current-seconds)
                         (ltq-min frame-ltq))))))]
             [current-controllers cm]
             [current-update-canvas this-update-canvas])
          (nested-big-bang initial-world tick sound-scale
                           world->listener done?))))))

  (begin0
    (yield done-ch)
    (send the-frame show #f)))

(provide/contract
 [RATE number?]
 [current-rate (-> number?)]
 [big-bang
  (->* (any/c
        #:tick (-> any/c (listof controller?)
                   (values any/c gl:cmd? sound-scape/c)))
       (#:sound-scale real?
                      #:listener (-> any/c psn?)
                      #:done? (-> any/c boolean?))
       any/c)])
