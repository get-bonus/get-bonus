#lang racket/base
(require "psn.rkt"
         racket/contract)

(define (clamp bot x top)
  (max bot (min x top)))

(define (cardinate p)
  (if (= 0. (psn-x p))
      p
      (psn (psn-x p) 0.)))

(provide/contract
 [cardinate (-> psn?
                psn?)]
 [clamp (-> number? number? number?
            number?)])