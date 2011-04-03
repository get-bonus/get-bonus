#lang racket

(require 2htdp/image)
(define sheet (bitmap "../resources/SMB-Tiles.png"))

sheet 

(define small-tile-margin 1)
(define small-tile-size 16)
(define (small-tile r c)
  (crop (* (+ small-tile-margin small-tile-size) c)
        (* (+ small-tile-margin small-tile-size) r)
        small-tile-size
        small-tile-size
        sheet))

#;(beside (scale 5 (small-tile 0 0))
        (scale 5 (small-tile 0 1)))

(beside (small-tile 1 0)
               (small-tile 1 1)
               (small-tile 1 2))

#;(scale 5
         (beside (small-tile 1 0)
               (small-tile 1 1)
               (small-tile 1 2)))