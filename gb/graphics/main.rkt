#lang racket/base
(require mode-lambda
         racket/contract
         racket/runtime-path
         racket/list
         racket/match
         gb/lib/fstree
         (for-syntax racket/base
                     syntax/parse))

(define crt-scale 32)
(define crt-width (* crt-scale 16))
(define crt-height (* crt-scale 9))

(define gb-sd (make-sprite-db))
(define sprite-tree (make-fstree))

(define-runtime-path sos.tgz-p "../../sos.tgz")
(let ()
  (local-require file/untar
                 file/gunzip
                 racket/path)
  (define sos-dir-p (path-replace-suffix sos.tgz-p #""))
  (unless (directory-exists? sos-dir-p)
    (define-values (in-bytes out-bytes) (make-pipe))
    (call-with-input-file sos.tgz-p
      (λ (in-file)
        (gunzip-through-ports in-file out-bytes)))
    (close-output-port out-bytes)
    (untar in-bytes
           #:dest (path-only sos-dir-p)))
  (for ([f (in-list (directory-list sos-dir-p))])
    (define ns
      (regexp-replace*
       (regexp-quote ".")
       (regexp-replace #rx".png$" (path->string f) "")
       "/"))
    (define n (string->symbol (format "spr:~a" ns)))
    (local-require racket/string)
    (with-handlers ([exn:fail? void])
      ;; xxx really gross
      (fstree-insert! sprite-tree
                      (string-join (reverse (rest (reverse (string-split ns "/")))) "/")
                      (string->symbol (string-join (reverse (rest (reverse (string-split (symbol->string n) "/")))) "/"))))
    (add-sprite!/file gb-sd n (build-path sos-dir-p f))))

(let ()
  (local-require gfx/color)
  (add-palette! gb-sd 'pal:grayscale
                (list TRANSPARENT BLACK BLACK
                      BLACK BLACK BLACK
                      BLACK BLACK BLACK
                      BLACK BLACK BLACK
                      BLACK BLACK BLACK))
  (add-palette! gb-sd 'pal:white
                (list TRANSPARENT WHITE WHITE
                      WHITE WHITE WHITE
                      WHITE WHITE WHITE
                      WHITE WHITE WHITE
                      WHITE WHITE WHITE)))

(define gb-csd (compile-sprite-db gb-sd))

(define (sprited-height s)
  (sprite-height gb-csd (sprited-ref s 0)))
(define (sprited-width s)
  (sprite-width gb-csd (sprited-ref s 0)))
(define (sprited-ref s i)
  (define id
    (string->symbol
     (format "~a/~a" s i)))
  (or (sprite-idx gb-csd id)
      (error 'sprited-ref "Can't find sprite ~v" id)))

(define (palette-ref pal)
  (or (palette-idx gb-csd pal)
      (error 'palette-ref "Can't find palette ~v" pal)))

(define current-dx (make-parameter 0.0))
(define current-dy (make-parameter 0.0))
(define current-mx (make-parameter 1.0))
(define current-my (make-parameter 1.0))
(define current-theta (make-parameter 0.0))
(define current-r (make-parameter 0))
(define current-g (make-parameter 0))
(define current-b (make-parameter 0))
(define current-a (make-parameter 255))

(define (rectangle hw hh [spr #f] [i #f] [pal #f])
  (sprite (current-dx) (current-dy)
          (if (and spr i)
              (sprited-ref spr i)
              ;; xxx i think this should be some default sprite
              0)
          ;; xxx I think these should turn into mx/my some how
          ;; hw hh
          #:r (current-r) #:g (current-g) #:b (current-b)
          #:a (exact->inexact (/ (current-a) 255))
          #:pal-idx (or (and pal (palette-ref pal)) 0)
          #:mx (current-mx) #:my (current-my)
          #:theta (current-theta)))
(define (!sprite* r g b a spr i pal)
  (sprite (current-dx) (current-dy) (sprited-ref spr i)
          #:r r #:g g #:b b #:a (exact->inexact (/ a 255))
          #:pal-idx (or (and pal (palette-ref pal)) 0)
          #:mx (current-mx) #:my (current-my)
          #:theta (current-theta)))
(define (!sprite tex i pal)
  (!sprite* 0 0 0 255 tex i pal))
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

(define (make-string-factory tex:font [pal #f])
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
         (λ (tex i pal)
           (!sprite* 255 255 255 255 tex i pal))]))
    (define tex-offset
      (if (and hw hh)
          (* 2.0 hw)
          (sprited-width tex:font)))

    (for/list ([c (in-string some-string)]
               [i (in-naturals)])
      (transform #:dx (* i tex-offset)
                 (maker tex:font (char->integer c) pal)))))

(provide
 (all-defined-out))
