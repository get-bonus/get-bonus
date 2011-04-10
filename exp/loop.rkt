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
         (prefix-in gl: "gl.rkt")
         "controller.rkt")

(define RATE 1/60)

; XXX nested big-bangs
; XXX have the final one kill the canvas, etc.

(define (big-bang initial-world
                  #:tick tick
                  #:listener [world->listener (λ (w) (psn 0. 0.))]
                  #:done? [done? (λ (w) #f)])
  (define km
    (keyboard-monitor))
  
  (define last-cmd #f)
  (define the-canvas
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
  
  (let loop ([w initial-world]
             [st (initial-system-state world->listener)])
    
    (define-values (wp cmd ss)
      (tick w 
            (map (λ (c) (c)) cs)))
    (set! last-cmd cmd)
    (send the-canvas refresh-now)
    (define stp
      (render-sound st ss wp))
    (if (done? wp)
        wp
        (begin (sleep/yield RATE)
               (loop wp stp)))))

(provide/contract
 [big-bang
  (->* (any/c
        #:tick (-> any/c (listof controller?)
            (values any/c gl:focused? sound-scape/c)))
       (#:listener (-> any/c psn?)
        #:done? (-> any/c boolean?))
       any/c)])