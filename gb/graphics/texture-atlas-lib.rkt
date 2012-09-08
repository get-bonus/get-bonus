#lang racket/base
(require (for-syntax racket/base
                     syntax/parse)
         racket/match
         racket/contract
         ffi/vector
         gb/lib/ffi/vector)

(struct texture-atlas (size vector [next-sprite #:mutable]))

(define (texture-atlas/size size)
  (texture-atlas 
   size
   (make-f32vector (* 4 size))
   0))

(define (insert-sprite! the-texture-atlas Tllx Tlly Tw Th)
  (match-define (texture-atlas _ vec last-sprite)
                the-texture-atlas)
  (f32vector-set!* vec (* 4 last-sprite)
                   Tllx Tlly Tw Th)
  (set-texture-atlas-next-sprite! the-texture-atlas (add1 last-sprite))
  last-sprite)

(define-syntax (define-sprite stx)
  (syntax-parse stx
    [(_ sprite:id Ta:id Tllx:expr Tlly:expr Tw:expr Th:expr)
     (syntax/loc stx
       (begin
         (define sprite
           (insert-sprite! Ta Tllx Tlly Tw Th))
         (provide sprite)))]))

(provide define-sprite)
(provide/contract
 [texture-atlas?
  (-> any/c
      boolean?)]
 [texture-atlas/size
  (-> exact-nonnegative-integer?
      texture-atlas?)]
 [texture-atlas-size
  (-> texture-atlas?
      exact-nonnegative-integer?)]
 [texture-atlas-vector
  (-> texture-atlas?
      f32vector?)])
