#lang typed/racket/base
(require "skewbinaryrandomaccesslist.ss")

(: bench-sbal-ty : Natural Natural -> Void)
(define (bench-sbal-ty N M)
  (define s
    (build-list N (λ (i) i)))
  (for/fold ([s s])
    ([i (in-range M)])
    (list-set s (random N) i))
  (void))

(provide bench-sbal-ty)
