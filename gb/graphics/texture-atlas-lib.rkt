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
           (f32vector Tllx Tlly Tw Th))
         (provide texture)))]))

(define (texture x y w h)
  (f32vector x y w h))
;; XXX also 4 long
(define texture? f32vector?)
(define (texture-x v)
  (f32vector-ref v 0))
(define (texture-y v)
  (f32vector-ref v 1))
(define (texture-width v)
  (f32vector-ref v 2))
(define (texture-height v)
  (f32vector-ref v 3))

(provide define-texture
         texture
         texture?
         texture-x
         texture-y
         texture-width
         texture-height)
