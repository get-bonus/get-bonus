#lang racket/base
(require gb/graphics/ngl
         gb/graphics/texture-atlas-lib
         (for-syntax racket/base
                     syntax/parse))

(define current-dx (make-parameter 0.0))
(define current-dy (make-parameter 0.0))
(define current-mx (make-parameter 1.0))
(define current-my (make-parameter 1.0))
(define current-theta (make-parameter 0.0))
(define current-r (make-parameter 0.0))
(define current-g (make-parameter 0.0))
(define current-b (make-parameter 0.0))
(define current-a (make-parameter 0.0))

(define none (texture 0 0 0 0))

(define (rectangle hw hh [tex none])
  (sprite-info (current-dx) (current-dy)
               hw hh
               (current-r) (current-g) (current-b) (current-a)
               tex
               (current-mx) (current-my)
               (current-theta)))
(define (sprite* r g b a tex)
  (sprite-info (current-dx) (current-dy)
               (* 0.5 (texture-width tex)) (* 0.5 (texture-height tex))
               r g b a
               tex
               (current-mx) (current-my)
               (current-theta)))
(define (sprite tex)
  (sprite* 0.0 0.0 0.0 0.0 tex))
(define (sprite/tint tex)
  (sprite* (current-r) (current-g) (current-b) (current-a) tex))

(define-syntax (transform stx)
  (syntax-parse stx
    ;; Color
    [(_ #:rgba r:expr g:expr b:expr a:expr . more:expr)
     (syntax/loc stx
       (transform #:r r #:g g #:b b #:a a . more))]
    [(_ #:r r:expr . more:expr)
     (syntax/loc stx
       (parameterize ([current-r r])
         (transform . more)))]
    [(_ #:g g:expr . more:expr)
     (syntax/loc stx
       (parameterize ([current-g g])
         (transform . more)))]
    [(_ #:b b:expr . more:expr)
     (syntax/loc stx
       (parameterize ([current-b b])
         (transform . more)))]
    [(_ #:a a:expr . more:expr)
     (syntax/loc stx
       (parameterize ([current-a a])
         (transform . more)))]    
    ;; Rotation
    [(_ #:rot theta:expr . more:expr)
     (syntax/loc stx
       (parameterize ([current-theta (+ (current-theta) theta)])
         (transform . more)))]
    ;; Translation
    [(_ #:d dx:expr dy:expr . more:expr)
     (syntax/loc stx
       (transform #:dx dx #:dy dy . more))]
    [(_ #:dx dx:expr . more:expr)
     (syntax/loc stx
       (parameterize ([current-dx (+ (current-dx) dx)])
         (transform . more)))]
    [(_ #:dy dy:expr . more:expr)
     (syntax/loc stx
       (parameterize ([current-dy (+ (current-dy) dy)])
         (transform . more)))]
    ;; Scaling
    [(_ #:m mx:expr my:expr . more:expr)
     (syntax/loc stx
       (transform #:mx mx #:my my . more))]
    [(_ #:mx mx:expr . more:expr)
     (syntax/loc stx
       (parameterize ([current-mx (+ (current-mx) mx)])
         (transform . more)))]
    [(_ #:my my:expr . more:expr)
     (syntax/loc stx
       (parameterize ([current-my (+ (current-my) my)])
         (transform . more)))]
    ;; Done
    [(_ . body:expr)
     (syntax/loc stx
       (let () . body))]))

(provide sprite
         sprite/tint
         rectangle
         transform)
