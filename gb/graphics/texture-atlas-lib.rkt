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

(define (texture x y w h)
  (u32vector x y w h))
(define (texture-width v)
  (u32vector-ref v 2))
(define (texture-height v)
  (u32vector-ref v 3))

(provide define-texture
         texture
         texture-width
         texture-height)
