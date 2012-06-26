#lang racket/base
(require (for-syntax racket/base
                     typed-racket/utils/tc-utils))

(define-syntax (module-begin stx)
  (syntax-case stx ()
    [(_ . e)
     #`(#%plain-module-begin
        (begin-for-syntax
          (set-box! typed-context? #t))
        . e)]))

(provide 
 (rename-out
  [module-begin #%module-begin])
 (except-out (all-from-out racket/base) #%module-begin))
