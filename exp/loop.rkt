#lang racket/base
(require racket/gui/base
         racket/contract
         racket/class
         "mapping.rkt"
         "joystick.rkt"
         "fullscreen.rkt"
         "keyboard.rkt"
         "psn.rkt"
         "3s.rkt"
         "ltq.rkt"
         (prefix-in gl: "gl.rkt")
         "controller.rkt")

(define RATE 1/60)

(define nested? (make-parameter #f))
(define current-controllers (make-parameter #f))
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
        (tick w 
              (map (λ (c) (c)) (current-controllers))))
      ((current-update-canvas) cmd)
      (if (done? wp)
          (let ()
            (sound-destroy! st)
            wp)
          (let ()
            ; XXX This is implies that we could switch sounds while rendering the next sound... which is bad.
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
  
  (define last-cmd #f)
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
               (when last-cmd (gl:draw last-cmd))
               (send glctx swap-buffers))))
     (λ (k)
       (keyboard-monitor-submit! km k))))
  
  (define cs 
    (cons (keyboard-monitor->controller-snapshot km)
          (map joystick-snapshot->controller-snapshot
               (get-all-joystick-snapshot-thunks))))
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
             [current-controllers cs]
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