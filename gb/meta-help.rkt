#lang racket/base
(require racket/contract
         gb/lib/godel
         gb/meta)

(provide/contract
 [godel-generate
  (-> (spec/c any/c) (-> any/c)
      (-> level?))]
 [godel-start
  (-> (spec/c any/c) (-> any/c result/c)
      start/c)]
 [random-godel-generate
  (-> (spec/c any/c)
      (-> level?))]
 [random-generate
  (-> level?)]
 [random-start
  (-> (-> score?)
      (-> level? score?))])

(define (godel-generate s f)
  (λ () (encode s (f))))

(define (random-godel-generate s)
  (λ () (random (spec-k s))))

(define (godel-start s f)
  (λ (n) (f (decode s n))))

(define prng-first-three 4294967086)
(define prng-last-three 4294944442)
;; XXX Missing constraint that one of each is not 0
(define prng-state/s 
  (hetero-vector/s
     (vector (nat-range/s prng-first-three)
             (nat-range/s prng-first-three)
             (nat-range/s prng-first-three)
             (nat-range/s prng-last-three)
             (nat-range/s prng-last-three)
             (nat-range/s prng-last-three))))
(define random-generate
  (godel-generate
   prng-state/s
   (λ ()
     (define prng (make-pseudo-random-generator))
     (pseudo-random-generator->vector prng))))
(define (random-start inner-start)
  (godel-start
   prng-state/s
   (λ (state)
     (define prng (vector->pseudo-random-generator state))
     (parameterize ([current-pseudo-random-generator prng])
       (inner-start)))))
