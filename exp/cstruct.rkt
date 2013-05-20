#lang racket/base
(require ffi/unsafe
         ffi/unsafe/cvector
         (for-syntax racket/base
                     racket/syntax))

(define-cstruct _sprite/int32s
  ([x _int32]
   [y _int32]
   [hw _int32]
   [hh _int32]))

(define-cstruct _sprite
  ([int32s _sprite/int32s]
   [r _byte]
   [g _byte]
   [b _byte]
   [a _byte]
   [tex _uint8]
   [pal _uint8]
   [mx _float]
   [my _float]
   [theta _float]))

(define (sprite-x s)
  (sprite/int32s-x (sprite-int32s s)))
(define (set-sprite-x! s v)
  (set-sprite/int32s-x! (sprite-int32s s) v))

(define (memory-bandwidth _struct)
  ;; 1 GB / s at 60 FPS
  (real->decimal-string
   (/ (* 1024 1024 1024 1/60)
      (ctype-sizeof _struct))))

(module+ main
  (ctype-sizeof _sprite)
  (ctype->layout _sprite)
  (memory-bandwidth _sprite))

(define how-many-sprites 512)

(module+ main
  (define the-ptr (malloc _sprite how-many-sprites))

  (define sprites (make-cvector* the-ptr _sprite how-many-sprites))
  (cvector-length sprites)

  (define a-sprite (cvector-ref sprites (/ how-many-sprites 2)))
  (printf "Before First: ~a\n" (sprite-x a-sprite))
  (set-sprite-x! a-sprite 0)

  (define a-sprite2 (cvector-ref sprites (/ how-many-sprites 2)))
  (printf "Before Second: ~a\n" (sprite-x a-sprite2))

  (define a-sprite3
    (make-sprite (make-sprite/int32s 1 2 3 4)
                 5 6 7 8 9 10 11.0 12.0 13.0))
  (cvector-set! sprites (/ how-many-sprites 2) a-sprite3)
  (printf "After First: ~a\n" (sprite-x a-sprite))
  (printf "After Second: ~a\n" (sprite-x a-sprite2)))

;; XXX look at http://developer.apple.com/library/ios/#documentation/3ddrawing/conceptual/opengles_programmingguide/TechniquesforWorkingwithVertexData/TechniquesforWorkingwithVertexData.html
