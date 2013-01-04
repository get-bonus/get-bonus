#lang racket/base
(require racket/contract)

(struct game-info (id name version generate start) #:transparent)

(define level? exact-nonnegative-integer?)
(define score? real?)

(provide
 (contract-out
  [level? flat-contract?]
  [score? flat-contract?]
  [struct game-info
          ([id symbol?]
           [name string?]
           [version exact-nonnegative-integer?]
           [generate (-> level?)]
           [start (-> level? score?)])]))
