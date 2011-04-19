#lang racket/base
(require racket/contract)

(define (clamp bot x top)
  (max bot (min x top)))

(provide/contract
 [clamp (-> number? number? number?
            number?)])