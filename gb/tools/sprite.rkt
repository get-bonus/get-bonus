#lang racket/base
(require racket/cmdline
         racket/file
         racket/path
         racket/draw
         rackunit
         racket/class
         racket/stxparam
         (for-syntax racket/base
                     syntax/parse))

(define (make-write-sprite width-sb height-sb)
  (command-line
   #:program "sprite"
   #:args (orig-pth dest-manifest dest-root)
   (when (file-exists? dest-manifest)
     (delete-file dest-manifest))
   (define the-src-bm
     (make-object bitmap% orig-pth 'png/alpha #f #t))
   (unless (= width-sb (send the-src-bm get-width))
     (error 'write-sprite "Width should be ~e but is ~e"
            width-sb (send the-src-bm get-width)))
   (unless (= height-sb (send the-src-bm get-height))
     (error 'write-sprite "Height should be ~e but is ~e"
            height-sb (send the-src-bm get-height)))
   (define the-dest-root dest-root)
   (define the-manifest dest-manifest)

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

     (write-to-file sprite-name the-manifest #:exists 'append))))

(define (valid-name? s)
  (and (string? s)
       (not (regexp-match #rx"[^a-zA-Z0-9_/\\-]" s))))

(define-check (check-within min v max)
  (unless (<= min v)
    (fail-check))
  (unless (< v max)
    (fail-check)))

(define (make-test-sprite width-sb height-sb)
  (λ (name lx ly w h)
    (check-pred valid-name? name)
    (check-within 0 lx width-sb)
    (check-within 0 (+ lx w) width-sb)
    (check-within 0 ly height-sb)
    (check-within 0 (+ ly h) height-sb)))

(define-syntax-parameter sprite
  (λ (stx) (raise-syntax-error 'sprite "Only allowed inside sprite" stx)))

(define-syntax (module-begin stx)
  (syntax-parse stx
    [(_ width:nat height:nat body ...)
     (syntax/loc stx
       (#%module-begin
        (module+ main
          (define write-sprite
            (make-write-sprite width height))
          (syntax-parameterize
              ([sprite (make-rename-transformer #'write-sprite)])
            body ...))
        (module+ test
          (define test-sprite
            (make-test-sprite width height))
          (syntax-parameterize
              ([sprite (make-rename-transformer #'test-sprite)])
            body ...))))]))

(provide sprite
         (rename-out [module-begin #%module-begin])
         (except-out (all-from-out racket/base) #%module-begin))
