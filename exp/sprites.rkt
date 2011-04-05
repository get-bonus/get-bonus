#lang racket/base
(require "gl.rkt"
         racket/contract)

(define (sprite-sheet/grid t size [margin 0])
  (define w (texture-w t))
  (define h (texture-h t))
  (λ (r c)
    (define x (* (+ margin size) c))
    (define y (* (+ margin size) r))
    (texture t 
             1 1
             (/ x w) (/ y h) 
             (/ size w) (/ size h))))

(define (sprite-sheet/array t size)
  (define w (texture-w t))
  (define h (texture-h t))
  (define cols (/ w size))
  (define rows (/ h size))
  (define ss/g (sprite-sheet/grid t size))
  (λ (i)
    (define r (quotient i rows))
    (define c (remainder i cols))
    (ss/g r c)))

(provide/contract
 [sprite-sheet/grid
  ((texture? real?) (real?) . ->* . 
            (integer? integer? . -> .  cmd?))]
 [sprite-sheet/array
  (texture? real? . -> . (integer? . -> . cmd?))])