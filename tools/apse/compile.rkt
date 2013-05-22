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

(define (compile db atlas-p pal-p idx-p idx-bin-p)
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
      (define how-many-places (add1 (length places)))

      (define atlas-bin
        (make-bytes (* tex-size tex-size)))

      (define index-values 4)
      (define index-bytes-per-value 4)
      (define index-bin
        (make-bytes (* index-values index-bytes-per-value
                       how-many-places)))

      (define sprite->img-placements
        (hash-set
         (for/hasheq
          ([s (in-list sprites)])
          (values s
                  (make-vector
                   (vector-length (sprite-images s)) 0)))
         'none #f))
      (for ([pl (in-list places)]
            [pi (in-naturals 1)])
        (match-define (placement ax ay (vector s i img)) pl)
        (define ps (hash-ref sprite->img-placements s))
        (vector-set! ps i (vector pi ax ay)))

      (begin0
        (append
         (list ";; sprite info")
         (list `(define-sprite-atlas ,how-many-places ,tex-size))
         (list ";; sprites")
         (for/list ([(e vs) (in-hash sprite->img-placements)])
           (match e
             ['none
              `(define-sprite none 0 0 (0))]
             [(? sprite? s)
              (define w (sprite-width s))
              (define h (sprite-height s))
              (define pis
                (for/list ([v (in-vector vs)]
                           [i (in-naturals)]
                           [img (in-vector (sprite-images s))])
                  (match-define (vector pi ax ay) v)

                  (for* ([x (in-range w)]
                         [y (in-range h)])
                    (define palette-val
                      (bytes-ref img
                                 (byte-xy-offset w h x y)))

                    (bytes-set! atlas-bin
                                (+ (* tex-size (+ ay y)) (+ ax x))
                                palette-val))

                  (for ([v (in-list (list ax ay w h))]
                        [o (in-naturals)])
                    (real->floating-point-bytes
                     v index-bytes-per-value
                     (system-big-endian?) index-bin
                     (+ (* index-values
                           index-bytes-per-value
                           pi)
                        (* index-bytes-per-value o))))

                  pi))

              `(define-sprite ,(sprite-name s) ,w ,h ,pis)])))

        (display-to-file (gzip-bytes atlas-bin)
                         #:exists 'replace
                         atlas-p)
        (display-to-file (gzip-bytes index-bin)
                         #:exists 'replace
                         idx-bin-p))))  

  ;; Make the palette
  (define palette-indexes
    (let ()
      (define palettes (db-palettes db))
      (define palette-depth 16)
      (define pal-bm
        (make-object bitmap% palette-depth (length palettes) #f #t))
      (define pal-bm-dc (new bitmap-dc% [bitmap pal-bm]))

      (begin0
        (append
         (list ";; palette info")
         (list `(define-palette-atlas
                  ,(length palettes)
                  ,palette-depth))
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
        (for-each displayln atlas-indexes)
        (newline)
        (for-each pretty-display palette-indexes)))))

(module+ main
  (require racket/cmdline)
  (command-line
   #:program "apse"
   #:args (db-path atlas-path pal-path index-path idx-bin-p)
   (compile (load-db db-path) atlas-path pal-path index-path idx-bin-p)))
