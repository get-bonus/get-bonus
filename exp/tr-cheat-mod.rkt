#lang typed/racket/base

(provide f)

(: f (Float -> Float))
(define (f x)
  (+ x 1.))