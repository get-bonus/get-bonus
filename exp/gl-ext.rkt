#lang racket/base
(require racket/contract
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

(provide/contract
 [center-texture-at
  (psn? texture? . -> . cmd?)]
 [texture/px 
  ((texture?) 
   (real? real? integer? integer? integer? integer?)
   . ->* . cmd?)])