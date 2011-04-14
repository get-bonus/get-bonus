#lang racket/base
(require racket/contract
         racket/math)

(struct controller
        (dpad
         l-stick
         r-stick
         a b
         x y
         select start
         l1 l2
         r1 r2)
        #:transparent)

(define (stick-state? n)
  (and (inexact? n)
       (<= -1 (real-part n) 1)
       (<= -1 (imag-part n) 1)))

(define stick-x real-part)
(define stick-y imag-part)

(provide/contract
 [stick-state? contract?]
 [stick-x (-> stick-state? (between/c -1 1))]
 [stick-y (-> stick-state? (between/c -1 1))]
 [struct controller
         ([dpad stick-state?]
          [l-stick stick-state?]
          [r-stick stick-state?]
          [a boolean?] [b boolean?]
          [x boolean?] [y boolean?]
          [select boolean?] [start boolean?]
          [l1 boolean?] [l2 boolean?]
          [r1 boolean?] [r2 boolean?])])