#lang racket/base
(require (for-syntax racket/base
                     unstable/syntax
                     syntax/stx
                     racket/syntax
                     syntax/parse)
         ffi/unsafe
         ffi/unsafe/define)

(define-syntax-rule (fake id ...)
  (begin
    (define-syntax (id stx)
      (syntax-case stx ()
        [(_ . b)
         (syntax/loc stx
           (error 'id "not yet implemented: ~e" (list . b)))]
        [b
         (syntax/loc stx
           (error 'id "not yet implemented: ~e" 'b))]))
    ...
    (provide id ...)))

(define-syntax-rule (define* id v)
  (begin (define id v)
         (provide id)))

(define-syntax-rule (define-cpointer-type* id ...)
  (begin (define-cpointer-type id)
         ...))

(define-syntax (define-enumy stx)
  (syntax-parse stx
    [(_ (~seq name:id (~datum =) val:expr) ...)
     (syntax/loc stx
       (begin (define* name val)
              ...))]))

(define-syntax (define-#define stx)
  (syntax-parse stx
    [(_ (~seq (~datum define) name:id val:expr) ...)
     (syntax/loc stx
       (begin (define* name val)
              ...))]))

(define-syntax-rule (dup f (a ...) ...)
  (begin (f a ...)
         ...))
(define-syntax (d stx)
  (syntax-parse stx
    [(_ (ffi [fun (in ...) (out ...)]
             ...)
        ...)
     (with-syntax ([(define-ffi ...)
                    (stx-map (λ (f) (format-id f "define-~a" f))
                             #'(ffi ...))])
       (syntax/loc stx
         (begin
           (dup define-ffi [fun (_fun in ... -> out ...)] ...)
           ...
           (provide fun ... ...))))]))

(provide (all-defined-out))
