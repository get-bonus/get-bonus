#lang racket/base
(require (for-syntax racket/base
                     syntax/parse)
         ffi/vector)

(define-syntax (define-texture stx)
  (syntax-parse stx
    [(_ texture:id Tllx:expr Tlly:expr Tw:expr Th:expr)
     (syntax/loc stx
       (begin
         (define texture
           (u32vector Tllx Tlly Tw Th))
         (provide texture)))]))

(provide define-texture)
