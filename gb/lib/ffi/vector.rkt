#lang racket/base
(require (for-syntax racket/base
                     syntax/parse)
         ffi/vector)

(define-syntax (f32vector-set!* stx)
  (syntax-parse stx
    [(_ vec:id offset:expr element:expr ...)
     (with-syntax
         ([(element-i ...)
           (for/list ([i (in-range (length (syntax->list #'(element ...))))])
             (datum->syntax #'vec i))])
       (syntax/loc stx
         (let ([offset-v offset])
           (f32vector-set! vec (+ offset-v element-i) element)
           ...)))]))

(provide f32vector-set!*)
