#lang racket/base
(require data/heap
         racket/match
         racket/contract)

(struct ltq (max count h) #:mutable)

(define (make-ltq max)
  (ltq max 0 (make-heap <=)))

(define (ltq-add! l)
  (match-define (ltq max ct h) l)
  (heap-add! h (current-seconds))
  (set-ltq-count! l (add1 ct))
  (when ((ltq-count l) . > . max)
    (set-ltq-count! l max)
    (heap-remove-min! h)))

(define (ltq-min l)
  (heap-min (ltq-h l)))

(provide/contract
 [ltq? contract?]
 [rename make-ltq ltq
         (-> exact-nonnegative-integer? ltq?)]
 [ltq-add! (-> ltq? void)]
 [ltq-min (-> ltq? exact-nonnegative-integer?)]
 [ltq-count (-> ltq? exact-nonnegative-integer?)])