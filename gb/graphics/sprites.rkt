#lang racket/base
(require gb/graphics/gl
         racket/contract)

(define (sprite-sheet/grid t size [margin 0])
  (define w (texture-w t))
  (define h (texture-h t))
  (define m (+ margin size))
  (define tw (/ size w))
  (define th (/ size h))
  (λ (r c)
    (define x (* m c))
    (define y (* m r))
    (texture t 
             1 1
             (/ x w) (/ y h) 
             tw th)))

(define (make-sprite-sheet/array quotient remainder)
  (λ (t size)
    (define w (texture-w t))
    (define h (texture-h t))
    (define cols (/ w size))
    (define rows (/ h size))
    (define ss/g (sprite-sheet/grid t size))
    (λ (i)
      (define r (quotient i rows))
      (define c (remainder i cols))
      (ss/g r c))))

(define sprite-sheet/row-major
  (make-sprite-sheet/array quotient remainder))
(define sprite-sheet/column-major
  (make-sprite-sheet/array remainder quotient))

(provide/contract
 [sprite-sheet/grid
  ((texture? real?) (real?) . ->* . 
            (integer? integer? . -> .  cmd?))]
 [sprite-sheet/row-major
  (texture? real? . -> . (integer? . -> . cmd?))]
 [sprite-sheet/column-major
  (texture? real? . -> . (integer? . -> . cmd?))])
