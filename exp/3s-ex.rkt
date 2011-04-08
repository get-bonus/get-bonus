#lang racket/base
(require racket/list
         "3s.rkt"
         "psn.rkt")

(define (big-sound w lpf tick)
  (let loop ([w w] [st (initial-system-state lpf)])
    (define-values (w* s) (tick w))
    (define st* (render-sound st s w))
    (sleep 1)
    (loop w* st*)))

(require racket/runtime-path)
(define-runtime-path resource-path "../resources")

(define bgm 
  (path->audio 
   (build-path resource-path 
               "SMB-1-1.mp3")))
(define jump-se
  (path->audio
   (build-path resource-path 
               "SMB-SE-Jump.wav")))

(big-sound 0 
           (λ (w) (psn 0.0 0.0))
           (λ (w)
             (cond
               [(zero? w)
                (values (add1 w)
                        (list (background (λ (w) bgm) #:gain 0.8)
                              (sound-on jump-se
                                        #:looping? #t
                                        (λ (w) (+ (psn -5.0 0.0)
                                                  (modulo w 11))))))]
               [(w . >= . 30)
                (error 'done)]
               [else
                (values (add1 w)
                        empty)])))