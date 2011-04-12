#lang racket/base
(require racket/match
         racket/contract)
; This is a functional matrix. 
; XXX Do a lit review and find something better.
; XXX This is bad because it doesn't use the fact that the size of the matrix is fixed.

(struct fmatrix (ht))
(define (make-fmatrix rs cs)
  (fmatrix (hasheq)))

(define mt-hasheq (hasheq))
(define (fmatrix-update f r c u a)
  (match-define (fmatrix ht) f)
  (hash-update ht r
               (λ (r-ht)
                 (hash-update r-ht c u a))
               mt-hasheq))

(define (fmatrix-ref f r c a)
  (match-define (fmatrix ht) f)
  (hash-ref (hash-ref ht r mt-hasheq) c a))

(provide/contract
 [fmatrix? contract?]
 [rename make-fmatrix fmatrix
         (-> exact-nonnegative-integer? exact-nonnegative-integer?
             fmatrix?)]
 [fmatrix-update
  (-> fmatrix? 
      exact-nonnegative-integer? exact-nonnegative-integer?
      (-> any/c any/c) any/c
      any/c)]
 [fmatrix-ref
  (-> fmatrix?
      exact-nonnegative-integer? exact-nonnegative-integer?
      any/c
      any/c)])