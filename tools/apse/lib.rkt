#lang racket/base
(require racket/draw
         racket/format
         racket/match
         racket/class)
(module+ test
  (require rackunit))

(define base-c (make-object color% #xFD #xF6 #xE3 1))
(define outline-c (make-object color% #xFF #x14 #x93 1))
(define all-white-c (make-object color% 255 255 255 1))
(define all-black-c (make-object color% 0 0 0 1))
(define all-transparent-c (make-object color% 0 0 0 0))

(define-syntax-rule (push! l e ...)
  (set! l (list* e ... l)))

(define (byte-xy-offset w h x y)
    (+ (* y w) x))

(define (pixels-color pxs w h x y)
  (define xy-offset (+ (* 4 y w) (* 4 x)))
  (define a (bytes-ref pxs (+ xy-offset 0)))
  (define r (bytes-ref pxs (+ xy-offset 1)))
  (define g (bytes-ref pxs (+ xy-offset 2)))
  (define b (bytes-ref pxs (+ xy-offset 3)))
  (vector a r g b))

(define (clamp lo v hi)
  (max lo (min v hi)))

(define (color-key? c)
  (for/or ([some-c (in-string "0123456789")]
           [i (in-naturals)]
           #:when (eq? c some-c))
    i))
(define (shifted-color-key? c)
  (for/or ([some-c (in-string ")!@#$%^&*(")]
           [i (in-naturals)]
           #:when (eq? c some-c))
    i))

(define (color%->hex c)
  (apply string-append
         "#"
         (for/list ([b (in-list (list (inexact->exact (* 255 (send c alpha)))
                                      (send c red)
                                      (send c green)
                                      (send c blue)))])
           (~a (number->string b 16)
               #:min-width 2
               #:pad-string "0"))))
(define (color-hex? s)
  (and (string? s)
       (= (string-length s) 9)
       (match s
         [(regexp #rx"^#(..)(..)(..)(..)$" (list _ as rs gs bs))
          (define a (string->number as 16))
          (define r (string->number rs 16))
          (define g (string->number gs 16))
          (define b (string->number bs 16))
          (and a r g b (vector a r g b))]
         [_
          #f])))
(module+ test
  (check-equal? (color-hex? #f) #f)
  (check-equal? (color-hex? "12345678") #f)
  (check-equal? (color-hex? "1234567890") #f)
  (check-equal? (color-hex? "123456789") #f)
  (check-equal? (color-hex? "#abcdefgh") #f)
  (check-equal? (color-hex? "#ffffffff") (vector 255 255 255 255)))

(define (read-color-hex minibuffer-read prompt)
  (minibuffer-read prompt
                   #:valid-char?
                   (λ (c)
                     (or (char=? #\# c)
                         (string->number (string c) 16)))
                   #:accept-predicate? color-hex?))

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
