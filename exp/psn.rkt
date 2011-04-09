#lang racket/base
(require racket/contract)

(define psn? inexact?)
(define psn make-rectangular)
(define psn-x (compose exact->inexact real-part))
(define psn-y (compose exact->inexact imag-part))

(provide/contract
 [psn? contract?]
 [psn (inexact? inexact? . -> . psn?)]
 [psn-x (psn? . -> . inexact?)]
 [psn-y (psn? . -> . inexact?)])