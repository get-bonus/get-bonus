#lang typed/racket/base
(require math/number-theory
         math/base)

(module+ main
  (void
   (for/fold:
    : Real
    ([pi_Pi : Real 0])
    ([i : Integer (in-range 50)])

    (: E (Integer -> Real))
    (define (E k)
      (/ 1 (+ (* 8 i) k)))

    (define pi_i_minus_pi_Pi
      (/ (+ (* +4 (E 1))
            (* -2 (E 4))
            (* -1 (E 5))
            (* -1 (E 6)))
         (expt 16 i)))
    (define pi_i 
      (+ pi_Pi pi_i_minus_pi_Pi))

    (printf "~a"
            (remainder
             (exact-truncate
              (* pi_i (expt 10 i)))
             10))

    pi_i))

  (newline))
