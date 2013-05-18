#lang racket/base
(require (for-syntax racket/base
                     racket/syntax
                     gb/graphics/font-lib
                     syntax/parse)
         gb/graphics/r
         gb/graphics/ngl
         gb/graphics/ngli
         gb/graphics/texture-atlas-lib         
         gb/graphics/font-lib
         racket/contract
         racket/list
         racket/match)

(define (make-string-factory tex:font)
  (λ (some-string
      #:tint? [tint? #f]
      #:hw [hw #f]
      #:hh [hh #f])
    (define maker
      (cond
        [(and hw hh)
         (λ (tex) (rectangle hw hh tex))]
        [tint?
         sprite/tint]
        [else 
         sprite]))
    (define tex-offset
      (if (and hw hh)
        (λ (tex) (* 2.0 hw))
        texture-width))
    (define-values
      (tot-offset l)
      (for/fold ([offset 0.0]
                 [l empty])
          ([c (in-string some-string)])
        (define tex (texturev-ref tex:font (char->integer c)))
        (define this-offset (tex-offset tex))
        (values (+ offset this-offset)
                (cons (transform #:dx offset
                                 (maker tex))
                      l))))
    l))

(provide 
 (contract-out
  [make-string-factory
   (-> texturev?
       (->* (string?)
            (#:hw flonum? #:hh flonum? #:tint? boolean?)
            sprite-tree/c))]))
