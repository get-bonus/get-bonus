#lang racket/base
(require racket/contract
         racket/draw
         racket/class
         "psn.rkt"
         "gl.rkt")

(define (center-texture-at p t)
  (translate
   (- (psn-x p) (/ (texture-dw t) 2))
   (- (psn-y p) (texture-dh t))
   (texture t)))

(define (texture/px t 
                    [w (texture-dw t)] [h (texture-dh t)]
                    [tx1 0] [ty1 0]
                    [tw (texture-w t)] [th (texture-h t)])
  (define atw (texture-w t))
  (define ath (texture-h t))
  (texture t w h
           (/ tx1 atw) (/ ty1 ath)
           (/ tw atw) (/ th ath)))

(define (color/% c . cs)
  (apply color
         (exact->inexact (/ (send c red) 255))
         (exact->inexact (/ (send c green) 255))
         (exact->inexact (/ (send c blue) 255))
         (- 1. (send c alpha)) cs))
(define (background/% c . cs)
  (apply background 
         (exact->inexact (/ (send c red) 255))
         (exact->inexact (/ (send c green) 255))
         (exact->inexact (/ (send c blue) 255))
         (- 1. (send c alpha)) cs))

(provide/contract
 [color/% (((is-a?/c color%)) () #:rest (listof cmd?) . ->* . cmd?)]
 [background/% (((is-a?/c color%)) () #:rest (listof cmd?) . ->* . cmd?)]
 [center-texture-at
  (psn? texture? . -> . cmd?)]
 [texture/px 
  ((texture?) 
   (real? real? integer? integer? integer? integer?)
   . ->* . cmd?)])