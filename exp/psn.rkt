#lang racket/base
(define psn? complex?)
(define psn make-rectangular)
(define psn-x real-part)
(define psn-y imag-part)

(provide/contract
 [psn? contract?]
 [psn (number? number? . -> . psn?)]
 [psn-x (psn? . -> . number?)]
 [psn-y (psn? . -> . number?)])