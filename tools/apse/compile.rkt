#lang racket/base
(require racket/path
         racket/class
         racket/pretty
         racket/match
         racket/draw
         racket/function
         racket/list
         gb/lib/korf-bin
         "lib.rkt"
         "db.rkt")

(define (compile db atlas-p pal-p idx-p)
  ;; Make the atlas
  (define atlas-indexes
    (let ()
      (define sprites (map (curry load-sprite db) (db-sprites db)))
      (define images
        (flatten
         (for/list ([s (in-list sprites)])
           (for/list ([img (in-vector (sprite-images s))]
                      [i (in-naturals)])
             (vector s i img)))))

      (define-values
        (tex-size places)
        (pack (λ (s*i) (sprite-width (vector-ref s*i 0)))
              (λ (s*i) (sprite-height (vector-ref s*i 0)))
              images))

      (define atlas-bm (make-object bitmap% tex-size tex-size #f #t))
      (define atlas-bm-dc (new bitmap-dc% [bitmap atlas-bm]))

      (begin0
        (append
         (list ";; sprite info")
         (list `(define-sprite-atlas-size ,tex-size))
         (list ";; sprite images")
         (list `(define-texture
                  none
                  0.0 0.0 0.0 0.0))
         (for/list ([pl (in-list places)])
           (match-define (placement ax ay (vector s i img)) pl)
           (define w (sprite-width s))
           (define h (sprite-height s))
           (define bm (make-object bitmap% w h #f #t))
           (define new-img (make-bytes (* 4 w h) 0))
           (for* ([x (in-range w)]
                  [y (in-range h)])
             (define new-offset (+ (* 4 y w) (* 4 x)))
             (define palette-val
               (bytes-ref img
                          (byte-xy-offset w h x y)))
             (bytes-set! new-img
                         (+ new-offset 1)
                         palette-val)
             (unless (zero? palette-val)
               (bytes-set! new-img
                           (+ new-offset 0)
                           255)))
           (send bm set-argb-pixels 0 0 w h new-img)
           (send atlas-bm-dc draw-bitmap bm ax ay)
           `(define-sprite-image ,(sprite-name s) ,i
              ,(* 1.0 ax) ,(* 1.0 ay)
              ,(* 1.0 w) ,(* 1.0 h)))
         (list ";; sprites")
         (for/list ([s (in-list sprites)])
           `(define-sprite ,(sprite-name s)
              ,(vector-length (sprite-images s)))))

        (send atlas-bm save-file atlas-p 'png 100))))

  ;; Make the palette
  (define palette-indexes
    (let ()
      (define palettes (db-palettes db))

      (define pal-bm (make-object bitmap% 16 (length palettes) #f #t))
      (define pal-bm-dc (new bitmap-dc% [bitmap pal-bm]))

      (begin0
        (append
         (list ";; palette info")
         (list `(define-palette-atlas-size ,(length palettes) 16))
         (list ";; palettes")
         (for/list ([pn (in-list palettes)]
                    [y (in-naturals)])
           (define p (load-palette db pn))
           (for ([c (in-vector (palette-color%s p))]
                 [x (in-naturals)])
             (send pal-bm-dc set-pixel x y c))
           `(define-palette ,(palette-name p) ,y)))

        (send pal-bm save-file pal-p 'png 100))))

  ;; Make index
  (let ()
    (with-output-to-file idx-p
      #:exists 'replace
      (λ ()
        (printf "#lang racket/base\n")
        (pretty-display
         `(require gb/graphics/texture-atlas-lib))
        (newline)
        (for-each pretty-display atlas-indexes)
        (newline)
        (for-each pretty-display palette-indexes)
        (newline)
        (pretty-display
         `(provide (all-defined-out)))))))

(module+ main
  (require racket/cmdline)
  (command-line #:program "apse"
                #:args (db-path atlas-path pal-path index-path)
                (compile (load-db db-path) atlas-path pal-path index-path)))
