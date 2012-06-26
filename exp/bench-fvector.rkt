#lang s-exp gb/lib/tr-cheat
(require tests/stress
         (prefix-in ra: (planet dvanhorn/ralist:3:5))
         (prefix-in fec:slow: (planet dvanhorn/fector:1:1/main))
         (prefix-in fec:fast: (planet dvanhorn/fector:1:1/fast))
         gb/lib/skal)

(define (make-hash N e)
  (for/hasheq ([i (in-range N)])
    (values i e)))

(define (bench make set ref N M)
  (define fv (make N #f))
  (for/fold ([s fv])
    ([i (in-range M)])
    (if (zero? (random 2))
        (set s (random N) i)
        (begin (ref s (random N))
               s)))
  (void))

(define (make-fvec N e)
  (λ (x) e))
(define (fvec-ref f i)
  (f i))
(define (fvec-set f i v)
  (λ (x)
    (if (= i x)
        v
        (f x))))

(define N 10000)
(define M 10000)
(stress 20
        ["fec:fast"
         (bench fec:fast:make-fector fec:fast:fector-set fec:fast:fector-ref N M)]
        ["fec:slow"
         (bench fec:slow:make-fector fec:slow:fector-set fec:slow:fector-ref N M)]
        ["fvec"
         (bench make-fvec fvec-set fvec-ref N M)]
        ["hasheq"
         (bench make-hash hash-set hash-ref N M)]
        ["sbal"
         (bench make-list list-set list-ref N M)]
        ["ralist"
         (bench ra:make-list ra:list-set ra:list-ref N M)])
