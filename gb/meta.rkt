#lang racket/base
(require racket/contract)

(struct game-info (name start) #:transparent)

(provide
 (contract-out
  [struct game-info
          ([name string?]
           ;; XXX Add some functions for loading a save or trying to
           ;; get a higher score, etc.

           ;; XXX Change to return some sort of score/achievement/etc data      
           [start (-> any)])]))

