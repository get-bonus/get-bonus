#lang s-exp "tr-cheat.rkt"
(require (prefix-in skal: "../lib/skal.rkt")
         tests/eli-tester
         racket/contract
         racket/match)
; XXX This is bad because it doesn't use the fact that the size of the vector is fixed.

(struct fvector (s))
(define (make-fvector n)
  (fvector 
   (skal:make-list n #f)))

(define (fvector-update f n u a)
  (match-define (fvector s) f)
  (fvector
   (skal:list-update s n
                     (λ (e)
                       (u (or e a))))))

(define (fvector-ref f n a)
  (match-define (fvector s) f)
  (or (skal:list-ref s n) a))

(test
 (fvector-ref (make-fvector 4) 1 #f) => #f
 (fvector-ref (fvector-update (make-fvector 4) 1 add1 0) 1 #f) => 1)

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