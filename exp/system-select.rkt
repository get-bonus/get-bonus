#lang racket/base
(require (for-syntax racket/base
                     syntax/parse))
(define-syntax (system-select stx)
  (syntax-parse stx
    [(_ [((~datum unix)) unix-file]
        [((~datum macosx)) macosx-file])
     (with-syntax
         ([the-type-file
           (case (system-type 'os)
             [(unix) #'unix-file]
             [(macosx) #'macosx-file])])
     (syntax/loc stx
       (begin (require the-type-file)
              (provide (all-from-out the-type-file)))))]))

(provide system-select)
