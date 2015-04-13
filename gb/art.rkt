#lang racket/base
(require mode-lambda
         gfx/color
         apse)

(define sd (make-sprite-db))

(define GB-SNES-SCALE 26)
(define W (* GB-SNES-SCALE 16))
(define H (* GB-SNES-SCALE 9))

(define cw-slots (* 3 7))
(define color-schemes (polygon-idxs 7 cw-slots))
(define (add-cw! CW fmt)
  (for ([c (in-list CW)]
        [i (in-naturals)])
    (define n (string->symbol (format fmt i)))
    (add-palette! sd n (color->palette c))))
(add-palette! sd 'grayscale (color->palette GRAY))
(add-cw! (color-wheel cw-slots) "hi~a")
(add-cw! (color-wheel cw-slots #:s 0.67 #:b 0.6) "med~a")
(add-apse-palette! sd)

(define-sprite sd 'block
  $$$$$$$$
  $qqqqqq$
  $zaaaaq$
  $zaaaaq$
  $zaaaaq$
  $zaaaaq$
  $zzzzzq$
  $$$$$$$$)

(module+ apse
  (with-apse-params [sd W H]
    (apse-sprite 'block 'hi0)))
