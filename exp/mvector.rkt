#lang racket/base

(define-syntax mvector-ref
  (syntax-rules ()
    [(_ v i) (vector-ref v i)]
    [(_ v i j ...) (mvector-ref (vector-ref v i) j ...)]))
(define-syntax mvector-set!
  (syntax-rules ()
    [(_ v i e) (vector-set! v i e)]
    [(_ v i j ... e) (mvector-set! (vector-ref v i) j ... e)]))

(provide mvector-ref
         mvector-set!)