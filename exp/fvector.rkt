#lang s-exp "tr-cheat.rkt"
(require (prefix-in skal: "../lib/skal.rkt")
         racket/contract
         racket/match)
; XXX This is bad because it doesn't use the fact that the size of the vector is fixed.

(struct fvector (s))
(define (make-fvector n)
  (fvector 
   (skal:make-list n #f)))

(define (fvector-update f n u a)
  (match-define (fvector s) f)
  ; XXX Implement update in skal
  (define e (skal:list-ref s n))
  (fvector
   (skal:list-set s n 
                  (u (or e a)))))

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
      fvector?)]
 [fvector-ref
  (-> fvector?
      exact-nonnegative-integer?
      any/c
      any/c)])