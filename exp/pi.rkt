#lang typed/racket/base
(require math/number-theory
         math/base)

(module+ main
  (define-values (_a1)
    (for/fold:
     ([8Pi : Integer -8])
     ([i : Integer (in-range 50)])

     (define 8i 
       (+ 8 8Pi))

     (: E (Integer -> Real))
     (define (E k)
       (/ 1 (+ 8i k)))
     
     (define pi_i
       (+ (* +4 (E 1))
          (* -2 (E 4))
          (* -1 (E 5))
          (* -1 (E 6))))

     (printf "~s " (number->string pi_i))

     (values 8i)))
  (newline)
  (exit 1)

  (define-values (_a _b _c _d)
    (for/fold:
     ([8Pi : Integer -8]
      [16^Pi : Real 1/16]
      [10^Pi : Real 1/10]
      [pi_Pi : Real 0])
     ([i : Integer (in-range 500)])

     (define 8i 
       (+ 8 8Pi))

     (: E (Integer -> Real))
     (define (E k)
       (/ 1 (+ 8i k)))

     (define 16^i 
       (* 16^Pi 16))
     (define pi_i_minus_pi_Pi
       (/ (+ (* +4 (E 1))
             (* -2 (E 4))
             (* -1 (E 5))
             (* -1 (E 6)))
          16^i))
     (define pi_i
       (+ pi_Pi pi_i_minus_pi_Pi))

     (define 10^i
       (* 10 10^Pi))

     (printf "~a"
             (remainder
              (exact-truncate
               (* pi_i 10^i))
              10))

     (values 8i
             16^i
             10^i
             pi_i)))
  (newline))
