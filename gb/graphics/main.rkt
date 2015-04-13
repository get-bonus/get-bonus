#lang racket/base
(require mode-lambda
         racket/contract
         racket/list
         racket/match
         (for-syntax racket/base
                     syntax/parse))

(define crt-scale 32)
(define crt-width (* crt-scale 16))
(define crt-height (* crt-scale 9))

(define gb-sd
  (make-sprite-db))

(define gb-csd
  (compile-sprite-db gb-sd))

(define (sprited-height s)
  (error 'sprited-height))
(define (sprited-width s)
  (error 'sprited-width))
(define (sprited-ref s i)
  (error 'sprited-ref))

(define current-dx (make-parameter 0.0))
(define current-dy (make-parameter 0.0))
(define current-mx (make-parameter 1.0))
(define current-my (make-parameter 1.0))
(define current-theta (make-parameter 0.0))
(define current-r (make-parameter 0))
(define current-g (make-parameter 0))
(define current-b (make-parameter 0))
(define current-a (make-parameter 0))

(define (rectangle hw hh [spr #f] [i #f] [pal 'pal:grayscale])
  (sprite (current-dx) (current-dy)
          hw hh
          (current-r) (current-g) (current-b) (current-a)
          (if (and spr i)
              (sprited-ref spr i)
              0)
          pal
          (current-mx) (current-my)
          (current-theta)))
(define (!sprite* r g b a spr i pal)
  (sprite (current-dx) (current-dy)
          (* 0.5 (sprited-width spr)) (* 0.5 (sprited-height spr))
          r g b a
          (sprited-ref spr i) pal
          (current-mx) (current-my)
          (current-theta)))
(define (!sprite tex i pal)
  (!sprite* 0 0 0 0 tex i pal))
(define (!sprite/tint tex i pal)
  (!sprite* (current-r) (current-g) (current-b) (current-a) tex i pal))

(define-syntax (transform stx)
  (syntax-parse stx
    ;; Color
    [(_ #:irgbv irgbv:expr . more:expr)
     (syntax/loc stx
       ;; xxx u8vector
       (let* ([irgbv-v irgbv]
              [r (vector-ref irgbv-v 0)]
              [g (vector-ref irgbv-v 1)]
              [b (vector-ref irgbv-v 2)])
         (transform #:r r #:g g #:b b . more)))]
    [(_ #:rgb r:expr g:expr b:expr . more:expr)
     (syntax/loc stx
       (transform #:r r #:g g #:b b . more))]
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
    [(_ #:mxy mxy:expr . more:expr)
     (syntax/loc stx
       (let ([mxy-v mxy])
         (transform #:mx mxy-v #:my mxy-v . more)))]
    [(_ #:mx mx:expr . more:expr)
     (syntax/loc stx
       (parameterize ([current-mx (* (current-mx) mx)])
         (transform . more)))]
    [(_ #:my my:expr . more:expr)
     (syntax/loc stx
       (parameterize ([current-my (* (current-my) my)])
         (transform . more)))]
    ;; Done
    [(_ . body:expr)
     (syntax/loc stx
       (let () . body))]))

(define (make-string-factory tex:font [pal 'pal:grayscale])
  (λ (some-string
      #:tint? [tint? #f]
      #:hw [hw #f]
      #:hh [hh #f])
    (define maker
      (cond
        [(and hw hh)
         (λ (tex i pal)
           (rectangle hw hh tex i))]
        [tint?
         !sprite/tint]
        [else
         !sprite]))
    (define tex-offset
      (if (and hw hh)
          (* 2.0 hw)
          (sprited-width tex:font)))

    (for/list ([c (in-string some-string)]
               [i (in-naturals)])
      (transform #:dx (* i tex-offset)
                 (maker tex:font (char->integer c) pal)))))

(define sprite-tree #f)

(provide
 (all-defined-out))
