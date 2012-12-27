#lang racket/base
(require racket/contract
         gb/lib/godel
         gb/meta)

(define (random-between min max)
  (+ min (random (- max min))))

(define (list-ref/random l)
  (list-ref l (random (length l))))

(define prng-state-spec (vector 1 1 1 1 1 1))
(define (random-generate)
  (define prng (make-pseudo-random-generator))
  (define state (pseudo-random-generator->vector prng))
  (define level (godel-encode prng-state-spec state))
  level)

(define (random-start inner-start)
  (λ (level)
    (define state (godel-decode prng-state-spec level))
    (define prng (vector->pseudo-random-generator state))
    (parameterize ([current-pseudo-random-generator prng])
      (inner-start))))

(provide/contract
 [random-generate
  (-> level?)]
 [random-start
  (-> (-> score?)
      (-> level? score?))]
 [random-between
  (-> exact-nonnegative-integer? exact-nonnegative-integer?
      exact-nonnegative-integer?)]
 [list-ref/random
  (-> list? any/c)])
