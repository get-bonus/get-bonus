#lang racket/base
(require racket/contract)

(define (random-between min max)
  (+ min (random (- max min))))

(define (list-ref/random l)
  (list-ref l (random (length l))))

(provide/contract
 [random-between
  (-> exact-nonnegative-integer? exact-nonnegative-integer?
      exact-nonnegative-integer?)]
 [list-ref/random
  (-> list? any/c)])