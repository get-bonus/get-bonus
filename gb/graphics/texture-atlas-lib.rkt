#lang racket/base
(require (for-syntax racket/base
                     syntax/parse)
         racket/match
         racket/contract
         ffi/vector
         gb/lib/ffi/vector)

(struct texture-atlas (size vector [next-texture #:mutable]))

(define (texture-atlas/size size)
  (texture-atlas 
   size
   (make-u32vector (* 4 size))
   0))

(define (insert-texture! the-texture-atlas Tllx Tlly Tw Th)
  (match-define (texture-atlas _ vec last-texture)
                the-texture-atlas)
  (vector-set!* u32vector-set!
                vec (* 4 last-texture)
                Tllx Tlly Tw Th)
  (set-texture-atlas-next-texture! the-texture-atlas (add1 last-texture))
  last-texture)

(define-syntax (define-texture stx)
  (syntax-parse stx
    [(_ texture:id Ta:id Tllx:expr Tlly:expr Tw:expr Th:expr)
     (syntax/loc stx
       (begin
         (define texture
           (insert-texture! Ta Tllx Tlly Tw Th))
         (provide texture)))]))

(provide define-texture)
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
      u32vector?)])
