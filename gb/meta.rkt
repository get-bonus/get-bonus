#lang racket/base
(require racket/contract)

(struct game-info (id name desc version generate start) #:transparent)

(define level? exact-nonnegative-integer?)
(define score? real?)

(define result/c
  score?)

(define start/c
  (-> level? result/c))

(provide
 (contract-out
  [level? flat-contract?]
  [score? flat-contract?]
  [result/c contract?]
  [start/c contract?]
  [struct game-info
          ([id symbol?]
           [name string?]
           [desc (listof string?)]
           [version exact-nonnegative-integer?]
           [generate (-> level?)]
           [start start/c])]))
