#lang racket/base
(require "gl.rkt"
         racket/contract)

(define (sprite-sheet/grid t size [margin 0])
  (λ (r c)
    (define w (texture-w t))
    (define h (texture-h t))
    (define x (* (+ margin size) c))
    (define y (* (+ margin size) r))
    (texture t 
             1 1
             (/ x w) (/ y h) 
             (/ size w) (/ size h))))

(provide/contract
 [sprite-sheet/grid
  ((texture? real?) (real?) . ->* . 
            (integer? integer? . -> .  cmd?))])