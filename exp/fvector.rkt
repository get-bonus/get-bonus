#lang racket/base
(require racket/contract
         racket/match)
; XXX Do a lit review and find something better.
; XXX This is bad because it doesn't use the fact that the size of the vector is fixed.

(struct fvector (ht))
(define (make-fvector n)
  (fvector (hasheq)))

(define mt-hasheq (hasheq))
(define (fvector-update f n u a)
  (match-define (fvector ht) f)
  (hash-update ht n u a))

(define (fvector-ref f n a)
  (match-define (fvector ht) f)
  (hash-ref ht n a))

(provide/contract
 [fvector? contract?]
 [rename make-fvector fvector
         (-> exact-nonnegative-integer?
             fvector?)]
 [fvector-update
  (-> fvector? 
      exact-nonnegative-integer?
      (-> any/c any/c) any/c
      any/c)]
 [fvector-ref
  (-> fvector?
      exact-nonnegative-integer?
      any/c
      any/c)])