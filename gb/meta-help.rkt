#lang racket/base
(require racket/contract
         math/base
         data/enumerate
         gb/meta)

(provide/contract
 [godel-generate
  (-> enum? (-> any/c)
      (-> level?))]
 [godel-start
  (-> enum? (-> any/c result/c)
      start/c)]
 [random-godel-generate
  (-> enum?
      (-> level?))]
 [random-generate
  (-> level?)]
 [random-start
  (-> (-> score?)
      (-> level? score?))])

(define (godel-generate s f)
  (λ () (to-nat s (f))))

(define (random-godel-generate s)
  (λ ()
    (define K (size s))
    (if (= +inf.0 K)
        (random-bits 32)
        (random-natural K))))

(define (godel-start s f)
  (λ (n) (f (from-nat s n))))

(define prng-first-three 4294967086)
(define prng-last-three 4294944442)
;; XXX Missing constraint that one of each is not 0
(define prng-state/e
  (vec/e (below/e prng-first-three)
         (below/e prng-first-three)
         (below/e prng-first-three)
         (below/e prng-last-three)
         (below/e prng-last-three)
         (below/e prng-last-three)))
(define random-generate
  (godel-generate
   prng-state/e
   (λ ()
     (define prng (make-pseudo-random-generator))
     (pseudo-random-generator->vector prng))))
(define (random-start inner-start)
  (godel-start
   prng-state/e
   (λ (state)
     (define prng (vector->pseudo-random-generator state))
     (parameterize ([current-pseudo-random-generator prng])
       (inner-start)))))
