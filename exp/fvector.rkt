#lang racket/base
(require (prefix-in skal: "../lib/untyped/skewbinaryrandomaccesslist.rkt")
         racket/contract
         racket/match)
; XXX This is bad because it doesn't use the fact that the size of the vector is fixed.

(struct fvector (s))
(define (make-fvector n)
  (fvector 
   (skal:make-list n #f)))

(define (fvector-update f n u a)
  (match-define (fvector s) f)
  (define e (skal:list-ref s n))
  (skal:list-set s n e
            (u (or e a))))

(define (fvector-ref f n a)
  (match-define (fvector s) f)
  (or (skal:list-ref s n) a))

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