#lang racket/base
(require racket/contract
         racket/set
         "controller.rkt"
         "keyboard.rkt"
         "joystick.rkt"
         "mvector.rkt")

(define (joystick-snapshot->controller-snapshot js)
  (λ ()
    (define s (js))
    ; XXX Specialized for NES
    (controller
     (make-rectangular (mvector-ref (joystick-state-sticks s) 0 0 0)
                       (mvector-ref (joystick-state-sticks s) 0 0 1))
     0
     0
     #t #t ; a b
     #f #f
     #t #t ; select start
     #f #f
     #f #f)))

(define (keyboard-monitor->controller-snapshot km)
  (λ ()
    (define ks (keyboard-monitor-snapshot km))
    (define (on? k)
      (set-member? ks k))
    (controller
     (make-rectangular 
      (cond
        [(on? 'left) -1]
        [(on? 'right) 1]
        [else 0])
      (cond
        [(on? 'up) 1]
        [(on? 'down) -1]
        [else 0]))
     0 0
     (on? #\x) (on? #\c)
     (on? #\s) (on? #\d)
     (on? #\space) (on? #\return)
     (on? #\z) (on? #\a)
     (on? #\v) (on? #\f))))

(provide/contract
 [joystick-snapshot->controller-snapshot
  (-> (-> joystick-state?)
      (-> controller?))]
 [keyboard-monitor->controller-snapshot
  (-> keyboard-monitor?
      (-> controller?))])
