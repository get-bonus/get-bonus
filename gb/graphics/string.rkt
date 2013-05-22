#lang racket/base
(require gb/graphics/r
         gb/graphics/ngl
         gb/graphics/ngli
         racket/contract
         racket/list
         racket/match)

(define (make-string-factory tex:font [pal 0])
  (λ (some-string
      #:tint? [tint? #f]
      #:hw [hw #f]
      #:hh [hh #f])
    (define maker
      (cond
        [(and hw hh)
         (λ (tex i pal) (rectangle hw hh tex i))]
        [tint?
         sprite/tint]
        [else
         sprite]))
    (define tex-offset
      (if (and hw hh)
        (* 2.0 hw)
        (sprited-width tex:font)))

    (for/list ([c (in-string some-string)]
               [i (in-naturals)])
      (transform #:dx (* i tex-offset)
                 (maker tex:font (char->integer c) pal)))))

(provide
 (contract-out
  [make-string-factory
   (->* (sprited?) (palette?)
        (->* (string?)
             ;; xxx flonum?
             (#:hw flonum? #:hh flonum? #:tint? boolean?)
             sprite-tree/c))]))
