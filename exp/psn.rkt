#lang racket/base
(require racket/contract)

(define psn? inexact?)
(define psn make-rectangular)
(define psn-x real-part)
(define psn-y imag-part)

(provide/contract
 [psn? contract?]
 [psn (inexact? inexact? . -> . psn?)]
 [psn-x (psn? . -> . inexact?)]
 [psn-y (psn? . -> . inexact?)])