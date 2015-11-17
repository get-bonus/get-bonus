#lang racket/base
(require gb/data/mvector
         3s
         gb/data/psn
         gb/gui/world
         gb/lib/math
         gb/meta
         dos/win
         racket/contract
         racket/function
         racket/list
         racket/match
         racket/math)

(define (os-sound-reader k d)
  (λ (w)
    (win-env-read1 w k d)))

;; XXX remove width/height and maybe sound-scale since it is
;; standardized as crt-*
(define (big-bang/os width height center-pos
                     #:sound-scale [sound-scale width]
                     main-t)
  (big-bang
   (win-mbr main-t)
   #:sound-scale sound-scale
   #:tick
   (λ (w cs)
     (define w+cs (win-env-replace w 'controller cs))
     (define new-w (win-boot w+cs))
     (define gl-list
       (sort (win-env-read new-w 'graphics)
             < #:key car))
     (values new-w
             (map cdr gl-list)
             (win-env-read new-w 'sound)))
   #:listener
   (λ (w)
     ;; XXX
     center-pos)
   #:done?
   (λ (w)
     (win-env-read1 w 'done? #f))
   #:return
   (λ (w)
     (win-env-read1 w 'return #f))))

(provide
 (all-from-out dos/win)
 RATE
 current-frame
 call-with-gb
 (contract-out
  [big-bang/os
   (->* (number? number? psn? (-> any/c any))
        (#:sound-scale number?)
        any)]
  [os-sound-reader
   (-> symbol? any/c
       (-> win? any/c))]))
