#lang racket/base
(require racket/contract)

(define (list-ref/random l)
  (list-ref l (random (length l))))

(provide/contract
 [list-ref/random
  (-> list? any/c)])