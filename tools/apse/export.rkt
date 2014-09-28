#lang racket/base
(require racket/path
         racket/class
         racket/file
         racket/pretty
         racket/match
         racket/draw
         racket/function
         racket/list
         gb/lib/korf-bin
         gb/lib/math
         gb/lib/gzip
         gb/lib/fstree
         "lib.rkt"
         "db.rkt")

(define (prep-path n)
  (regexp-replace* #rx"/" n "."))

(define (export db ep)
  (define palettes (db-palettes db))
  (define palette-depth 16)
  (for ([pn (in-list palettes)])
    (define pal-bm
      (make-object bitmap% palette-depth 1 #f #t))
    (define pal-bm-dc (new bitmap-dc% [bitmap pal-bm]))
    (define p (load-palette db pn))
    (define n (palette-name p))
    (define pal-p (build-path ep "pals" (prep-path (format "~a.png" n))))
    (for ([c (in-vector (palette-color%s p))]
          [x (in-naturals)])
      (send pal-bm-dc set-pixel x 0 c))
    (send pal-bm save-file pal-p 'png 100))

  (define sprites (map (curry load-sprite db) (db-sprites db)))
  (define pn "grayscale")
  (define p (load-palette db pn))
  (define cs (palette-color%s p))
  (for ([s (in-list sprites)])
    (define w (sprite-width s))
    (define h (sprite-height s))
    (define n (sprite-name s))
    (for ([img (in-vector (sprite-images s))]
          [i (in-naturals)])
      (define spr-bm
        (make-object bitmap% w h #f #t))
      (define spr-bm-dc (new bitmap-dc% [bitmap spr-bm]))
      (define spr-p (build-path ep "sprs" (prep-path (format "~a.~a.png" n i))))
      (for* ([x (in-range w)]
             [y (in-range h)])
        (define palette-val
          (bytes-ref img
                     (byte-xy-offset w h x y)))
        (define c (vector-ref cs palette-val))
        (send spr-bm-dc set-pixel x y c))
      (send spr-bm save-file spr-p 'png 100))))

(module+ main
  (require racket/cmdline)
  (command-line
   #:program "export"
   #:args (db-path export-path)
   (export (load-db db-path) export-path)))
