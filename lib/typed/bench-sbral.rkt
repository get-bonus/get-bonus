#lang racket/base
(require tests/stress
         "skewbinaryrandomaccesslist.ss"
         #;"bench-sbral-ty.rkt")

(define (bench-ht N M)
  (define ht
    (for/hasheq ([i (in-range N)])
      (values i i)))
  (for/fold ([ht ht])
    ([i (in-range M)])
    (hash-set ht (random N) i))
  (void))

(define (bench-sbal N M)
  (define s
    (build-list N (λ (i) i)))
  (for/fold ([s s])
    ([i (in-range M)])
    (list-set s (random N) i))
  (void))

(define N 10000)
(define M 10000)
(stress 100
        ["hasheq"
         (bench-ht N M)]
        ["sbal"
         (bench-sbal N M)]
        #;["sbal-ty"
         (bench-sbal-ty N M)])