#lang s-exp "tr-cheat.rkt"
(require tests/stress
         (prefix-in ra: (planet dvanhorn/ralist:3:2))
         "../lib/skal.rkt")

(define (build-hash N f)
  (for/hasheq ([i (in-range N)])
    (values i (f i))))

(define (bench build set ref N M)
  (define fv (build N (λ (i) i)))
  (for/fold ([s fv])
    ([i (in-range M)])
    (if (zero? (random 2))
        (set s (random N) i)
        (begin (ref s (random N))
               s)))
  (void))

(define (build-fvec N f)
  f)
(define (fvec-ref f i)
  (f i))
(define (fvec-set f i v)
  (λ (x)
    (if (= i x)
        v
        (f x))))

(define N 10000)
(define M 10000)
(stress 100
        ["fvec"
         (bench build-fvec fvec-set fvec-ref N M)]
        ["hasheq"
         (bench build-hash hash-set hash-ref N M)]
        ["sbal"
         (bench build-list list-set list-ref N M)]
        ["ralist"
         (bench ra:build-list ra:list-set ra:list-ref N M)])