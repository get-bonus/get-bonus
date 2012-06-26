#lang racket/base
(require racket/match
         racket/contract)

(define psn? inexact?)
(define psn make-rectangular)
(define psn-x (compose exact->inexact real-part))
(define psn-y (compose exact->inexact imag-part))

(define-match-expander psn*
  (syntax-rules ()
    [(_ x y)
     (and (app psn-x x)
          (app psn-y y))]))

(provide
 psn*)
(provide/contract
 [psn? contract?]
 [psn (inexact? inexact? . -> . psn?)]
 [psn-x (psn? . -> . inexact?)]
 [psn-y (psn? . -> . inexact?)])