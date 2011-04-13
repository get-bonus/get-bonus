#lang racket/base
(require "fvector.rkt"
         racket/match
         racket/contract)
; This is a functional matrix.

; XXX Maybe use row-column vector rather than nested?
(struct fmatrix (fv mt))
(define (make-fmatrix rs cs)
  (fmatrix (fvector rs) (fvector cs)))

(define mt-fvector (hasheq))
(define (fmatrix-update f r c u a)
  (match-define (fmatrix fv mt-fvector) f)
  (fmatrix (fvector-update fv r
                           (λ (rv)
                             (fvector-update rv c u a))
                           mt-fvector)
           mt-fvector))

(define (fmatrix-ref f r c a)
  (match-define (fmatrix fv mt-fvector) f)
  (fvector-ref (fvector-ref fv r mt-fvector) c a))

(provide/contract
 [fmatrix? contract?]
 [rename make-fmatrix fmatrix
         (-> exact-nonnegative-integer? exact-nonnegative-integer?
             fmatrix?)]
 [fmatrix-update
  (-> fmatrix? 
      exact-nonnegative-integer? exact-nonnegative-integer?
      (-> any/c any/c) any/c
      fmatrix?)]
 [fmatrix-ref
  (-> fmatrix?
      exact-nonnegative-integer? exact-nonnegative-integer?
      any/c
      any/c)])