#lang racket/base
(require (for-syntax racket/base
                     racket/syntax))

(define-syntax (lagrange stx)
  (syntax-case stx ()
    [(_ (x_k y_k) ...)
     (with-syntax*
      ([the-x (generate-temporary)]
       [(l_k ...)
        (for/list ([x_j (in-list (syntax->list #'(x_k ...)))])
          (quasisyntax/loc x_j
            (* #,@(for/list ([x_m (in-list (syntax->list #'(x_k ...)))]
                             #:unless (equal? x_j x_m))
                    (quasisyntax/loc x_m
                      (/ (- the-x #,x_m)
                         (- #,x_j #,x_m)))))))])
      (syntax/loc stx
        (λ (the-x)
          (+ (* y_k l_k) ...))))]))

(provide lagrange)
