#lang racket/base
(require racket/draw
         racket/class)

(define base-c (make-object color% #xFD #xF6 #xE3 1))
(define outline-c (make-object color% #xFF #x14 #x93 1))
(define all-white-c (make-object color% 255 255 255 1))
(define all-black-c (make-object color% 0 0 0 1))
(define all-transparent-c (make-object color% 0 0 0 0))

(define (clamp lo v hi)
  (max lo (min v hi)))

(define (scale-and-center c dc bg-c w h inner)
  (send dc set-background base-c)
  (send dc clear)
  (define it (send dc get-transformation))
  (send dc set-smoothing 'unsmoothed)

  (define cw (send c get-width))
  (define ch (send c get-height))
  (define (floor* x)
    (if (< x 1)
      x
      (floor x)))
  (define the-scale
    (floor* (min (/ cw w) (/ ch h))))
  (send dc translate
        (/ (- cw (* w the-scale)) 2)
        (/ (- ch (* h the-scale)) 2))

  (send dc set-scale the-scale the-scale)

  (send dc set-pen bg-c 0 'solid)
  (send dc set-brush bg-c 'solid)
  (send dc draw-rectangle 0 0 w h)

  (inner)

  (send dc set-transformation it))

(provide (all-defined-out))
