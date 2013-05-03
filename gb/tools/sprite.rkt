#lang racket/base
(require racket/cmdline
         racket/file
         racket/contract
         racket/path
         racket/draw
         racket/list
         rackunit
         racket/class
         racket/stxparam
         (for-syntax racket/base
                     syntax/parse))
(module+ test
  (require rackunit))

(define (make-write-sprite/main width-sb height-sb)
  (command-line
   #:program "sprite"
   #:args (orig-pth dest-manifest dest-root)
   (make-write-sprite
    width-sb height-sb
    orig-pth dest-manifest dest-root)))

(define (make-write-sprite
         width-sb height-sb
         orig-pth the-dest-manifest the-dest-root)
  (when (file-exists? the-dest-manifest)
    (delete-file the-dest-manifest))
  (define the-src-bm
    (make-object bitmap% orig-pth 'png/alpha #f #t))
  (unless (= width-sb (send the-src-bm get-width))
    (error 'write-sprite "Width should be ~e but is ~e"
           width-sb (send the-src-bm get-width)))
  (unless (= height-sb (send the-src-bm get-height))
    (error 'write-sprite "Height should be ~e but is ~e"
           height-sb (send the-src-bm get-height)))

  (λ (name lx ly w h)
    (define new-bm (make-bitmap w h))
    (define bm-dc (new bitmap-dc% [bitmap new-bm]))

    (send bm-dc draw-bitmap-section
          the-src-bm 0 0
          lx ly w h)

    (define sprite-name
      (format "~a.png" name))
    (define sprite-pth
      (build-path the-dest-root sprite-name))
    (make-directory* (path-only sprite-pth))

    (send new-bm save-file sprite-pth 'png 100)

    (write-to-file sprite-name the-dest-manifest #:exists 'append)))
(module+ test
  (define a-wh 40)
  (define a.png (make-temporary-file "~a.png"))

  (define a-bm (make-bitmap a-wh a-wh))
  (define a-bm-dc (send a-bm make-dc))
  (define (set-color c)
    (send a-bm-dc set-pen c 1 'solid)
    (send a-bm-dc set-brush c 'solid))
  (set-color "red")
  (send a-bm-dc draw-rectangle  0  0 (/ a-wh 2) (/ a-wh 2))
  (set-color "blue")
  (send a-bm-dc draw-rectangle  0 (/ a-wh 2) (/ a-wh 2) (/ a-wh 2))
  (set-color "green")
  (send a-bm-dc draw-rectangle (/ a-wh 2)  0 (/ a-wh 2) (/ a-wh 2))
  (set-color "black")
  (send a-bm-dc draw-rectangle (/ a-wh 2) (/ a-wh 2) (/ a-wh 2) (/ a-wh 2))
  (void (send a-bm save-file a.png 'png 100))

  (define a.rktd (make-temporary-file "~a.rktd"))
  (define a-root (make-temporary-file "~a.output" 'directory))
  (for* ([dw (in-list '(-1 0 +1))]
         [dh (in-list '(-1 0 +1))]
         #:unless (and (zero? dw) (zero? dh)))
    (check-exn exn:fail?
               (λ ()
                 (make-write-sprite (+ dw a-wh) (+ dh a-wh)
                                    a.png a.rktd a-root))))
  (define a-write-sprite
    (make-write-sprite a-wh a-wh a.png a.rktd a-root))

  (define (check-bitmap check?)
    (define this-bm
      (make-object bitmap%
                   (build-path a-root
                               (last (file->list a.rktd)))
                   'png/alpha #f #t))
    (check-equal? (send this-bm get-width) (/ a-wh 2))
    (check-equal? (send this-bm get-height) (/ a-wh 2))
    (define this-bm-dc (send this-bm make-dc))
    (define c (make-object color%))
    (for* ([x (in-range (/ a-wh 2))]
           [y (in-range (/ a-wh 2))])
      (send this-bm-dc get-pixel x y c)
      (check? (send c red) (send c green) (send c blue) (send c alpha))))

  (a-write-sprite   "red"  0  0 (/ a-wh 2) (/ a-wh 2))
  (check-equal? (file->list a.rktd)
                (list "red.png"))
  (check-bitmap (λ (r g b a)
                  (check-equal? r 255)
                  (check-equal? g   0)
                  (check-equal? b   0)
                  (check-equal? a 1.0)))

  (a-write-sprite  "blue"  0 (/ a-wh 2) (/ a-wh 2) (/ a-wh 2))
  (check-equal? (file->list a.rktd)
                (list "red.png" "blue.png"))
  (check-bitmap (λ (r g b a)
                  (check-equal? r   0)
                  (check-equal? g   0)
                  (check-equal? b 255)
                  (check-equal? a 1.0)))

  (a-write-sprite "green" (/ a-wh 2)  0 (/ a-wh 2) (/ a-wh 2))
  (check-equal? (file->list a.rktd)
                (list "red.png" "blue.png" "green.png"))
  (check-bitmap (λ (r g b a)
                  (check-equal? r   0)
                  (check-equal? g 255)
                  (check-equal? b   0)
                  (check-equal? a 1.0)))

  (a-write-sprite "black" (/ a-wh 2) (/ a-wh 2) (/ a-wh 2) (/ a-wh 2))
  (check-equal? (file->list a.rktd)
                (list "red.png" "blue.png" "green.png" "black.png"))
  (check-bitmap (λ (r g b a)
                  (check-equal? r   0)
                  (check-equal? g   0)
                  (check-equal? b   0)
                  (check-equal? a 1.0))))

(define (valid-name? s)
  (not (regexp-match #rx"[^a-zA-Z0-9_/\\-]" s)))
(module+ test
  (check-true (valid-name? "maze/left"))
  (check-false (valid-name? "maze/left!")))

(define-check (check-within min v max)
  (unless (<= min v)
    (fail-check))
  (unless (<= v max)
    (fail-check)))
(module+ test
  (check-within 0 5 10))

(define (make-test-sprite width-sb height-sb)
  (λ (name lx ly w h)
    (check-pred valid-name? name)
    (check-within 0 lx width-sb)
    (check-within 0 (+ lx w) width-sb)
    (check-within 0 ly height-sb)
    (check-within 0 (+ ly h) height-sb)))
(module+ test
  (define test-sprite (make-test-sprite a-wh a-wh))
  (test-sprite "red"    0  0 (/ a-wh 2) (/ a-wh 2))
  (test-sprite "blue"   0 (/ a-wh 2) (/ a-wh 2) (/ a-wh 2))
  (test-sprite "green" (/ a-wh 2)  0 (/ a-wh 2) (/ a-wh 2))
  (test-sprite "black" (/ a-wh 2) (/ a-wh 2) (/ a-wh 2) (/ a-wh 2)))

(define-syntax-parameter sprite
  (λ (stx) (raise-syntax-error 'sprite "Only allowed inside sprite" stx)))

(define sprite/c
  (-> string?
      exact-nonnegative-integer?
      exact-nonnegative-integer?
      exact-nonnegative-integer?
      exact-nonnegative-integer?
      void))

(define-syntax (module-begin stx)
  (syntax-parse stx
    [(_ width:nat height:nat . body)
     (syntax/loc stx
       (#%module-begin
        (module+ main
          (define write-sprite
            (contract sprite/c (make-write-sprite/main width height)
                      'sprite-spec 'sprite-lib
                      'write-sprite #'write-sprite))
          (syntax-parameterize
              ([sprite (make-rename-transformer #'write-sprite)])
            . body))
        (module+ test
          (define test-sprite
            (contract sprite/c (make-test-sprite width height)
                      'sprite-spec 'sprite-lib
                      'test-sprite #'test-sprite))
          (syntax-parameterize
              ([sprite (make-rename-transformer #'test-sprite)])
            . body))))]))

(provide sprite
         (rename-out [module-begin #%module-begin])
         (except-out (all-from-out racket/base) #%module-begin))
