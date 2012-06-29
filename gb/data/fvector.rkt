#lang racket/base
(require (planet dvanhorn/fector:1:1/fast)
         tests/eli-tester)

(define (fvector n)
  (make-fector n #f))
(define (fvector-update f n u a)
  (fector-set f n
              (u (or (fector-ref f n) a))))
(define (fvector-ref f n a)
  (or (fector-ref f n) a))
(define (fvector-set f n v)
  (fector-set f n v))

(define make-fvector fvector)
(module+ test
  (test
   (fvector-ref (make-fvector 4) 1 #f) => #f
   (fvector-ref (fvector-update (make-fvector 4) 1 add1 0) 1 #f) => 1))

(provide
 fvector
 fvector-update
 fvector-ref
 fvector-set)

#;(provide/contract
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
     any/c)]
[fvector-set
 (-> fvector?
     exact-nonnegative-integer?
     any/c
     fvector?)])
