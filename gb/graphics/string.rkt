#lang racket/base
(require (for-syntax racket/base
                     racket/syntax
                     gb/graphics/font-lib
                     syntax/parse)
         gb/graphics/r
         gb/graphics/ngli
         gb/graphics/texture-atlas-lib         
         gb/graphics/font-lib
         racket/list
         racket/match)

(define-syntax (make-char-factory stx)
  (syntax-parse stx
    [(_ family:id size:nat)
     (define (format-char char)
       (format-id #'none "fonts/~a/~a/~a"
                  #'family (syntax->datum #'size) char))
     (with-syntax
         ([(chars ...)
           (for/list ([i (in-range CHAR-START (add1 CHAR-END))])
             (format-char i))])
       (quasisyntax/loc stx
         (lambda (char)
           (define int (- (char->integer char) CHAR-START))
           (vector-ref (vector chars ...) int))))]))

(define (make-string-factory char-factory)
  (λ (some-string
      #:hw [hw #f]
      #:hh [hh #f])
    (define maker
      (if (and hw hh)
        (λ (tex) (rectangle hw hh tex))
        sprite))
    (define tex-offset
      (if (and hw hh)
        (λ (tex) (* 2.0 hw))
        texture-width))
    (define-values
      (tot-offset l)
      (for/fold ([offset 0.0]
                 [l empty])
          ([c (in-string some-string)])
        (define tex (char-factory c))
        (define this-offset (tex-offset tex))
        (values (+ offset this-offset)
                (cons (transform #:dx offset
                                 (maker tex))
                      l))))
    l))

(provide make-string-factory
         make-char-factory)
