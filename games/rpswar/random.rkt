#lang racket/base
(require (for-syntax racket/base
                     racket/list
                     racket/syntax
                     unstable/syntax))

(define (random-list-ref l)
  (list-ref l (random (length l))))

(define-syntax (random-case stx)
  (syntax-case stx ()
    [(random-case [num-expr body ...] ...)
     (with-syntax*
         ([(num-id ...) (generate-temporaries #'(num-expr ...))]
          [(total-expr ...)
           (reverse
            (let loop ([l (reverse (syntax->list #'(num-id ...)))])
              (if (empty? l)
                  empty
                  (cons #`(+ #,@l)
                        (loop (rest l))))))])
       (syntax/loc stx
         (let ([num-id num-expr]
               ...)
           (let ([num-id total-expr]
                 ...)
             (let ([choice (random)])
               (cond 
                 [(<= choice num-id) body ...]
                 ...))))))]))

(provide random-list-ref
         random-case)