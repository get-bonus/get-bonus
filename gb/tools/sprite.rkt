#lang racket/base
(require racket/cmdline
         racket/file
         racket/path
         racket/draw
         racket/class)

(define the-src-bm #f)
(define the-manifest #f)
(define the-dest-root #f)

(command-line #:program "sprite"
              #:args (orig-pth dest-manifest dest-root)
              (when (file-exists? dest-manifest)
                (delete-file dest-manifest))
              (set! the-src-bm
                    (make-object bitmap% orig-pth 'png/alpha #f #t))
              (set! the-dest-root dest-root)              
              (set! the-manifest dest-manifest))

(define (sprite name lx ly w h)
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

  (write-to-file sprite-name the-manifest #:exists 'append))

(provide sprite
         (all-from-out racket/base))
