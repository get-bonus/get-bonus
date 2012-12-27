#lang racket/base
(require racket/contract)

(struct game-info (name version generate start) #:transparent)

(define level? exact-nonnegative-integer?)
(define score? exact-nonnegative-integer?)

(provide
 (contract-out
  [level? flat-contract?]
  [score? flat-contract?]
  [struct game-info
          ([name string?]
           [version exact-nonnegative-integer?]
           [generate (-> level?)]
           [start (-> level? score?)])]))

