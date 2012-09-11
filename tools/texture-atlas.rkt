#lang racket/base
(require racket/runtime-path
         racket/path
         racket/file
         racket/match
         racket/class
         racket/draw)

(define-runtime-path r "../r")

(module+ main
  (define MAX-W 150)
  (define MAX-H 150)

  (define sizes '(8 10 12 14 16 20))
  (define families '(decorative roman script swiss modern))

  (for* ([size (in-list sizes)]
         [family (in-list families)])
    (define f
      (make-font #:size size
                 #:family family
                 #:smoothing 'unsmoothed))
    (define font-dir
      (build-path r
                  "fonts"
                  (symbol->string family)
                  (number->string size)))

    (unless (directory-exists? font-dir)
      (make-directory* font-dir)

      (define chars
        "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz")

      (define char-dcs
        (for/list ([char (in-string chars)])
          (define char-s (string char))

          (define target (make-bitmap MAX-W MAX-H #t))
          (define dc (new bitmap-dc% [bitmap target]))

          (send dc set-brush "white" 'transparent)
          (send dc erase)
          (send dc set-brush "black" 'transparent)
          (send dc draw-text char-s (/ MAX-W 2) (/ MAX-H 2))

          dc))

      (define char-color% (make-object color%))
      (define-values
        (char-min-x char-max-x char-min-y char-max-y)
        (for*/fold
            ([char-min-x MAX-W]
             [char-max-x 0]
             [char-min-y MAX-H]
             [char-max-y 0])
            ([char-dc (in-list char-dcs)]
             [x (in-range MAX-W)]
             #:unless (< char-min-x x char-max-x)
             [y (in-range MAX-H)]
             #:unless (< char-min-y y char-max-y))
          (send char-dc get-pixel x y char-color%)
          (match (send char-color% red)
            [255
             (values char-min-x char-max-x char-min-y char-max-y)]
            [0
             (values (min x char-min-x)
                     (max x char-max-x)
                     (min y char-min-y)
                     (max y char-max-y))])))

      (define char-w (- char-max-x char-min-x))
      (define char-h (- char-max-y char-min-y))

      (for ([char (in-string chars)]
            [char-src-dc (in-list char-dcs)])
        (define char-s (string char))
        (define char-file
          (build-path font-dir
                      (format "~a.png" char-s)))

        (define char-bitmap (make-bitmap char-w char-h #t))
        (define char-dc (new bitmap-dc% [bitmap char-bitmap]))

        (send char-dc set-brush "white" 'transparent)
        (send char-dc erase)

        (send char-dc draw-bitmap-section
              (send char-src-dc get-bitmap)
              0 0
              char-min-x char-min-y
              char-w char-h)

        (printf "~a\n" char-file)
        (send char-bitmap save-file
              char-file
              'png
              100)))))

(define-runtime-path atlas.png "../r.png")
(define-runtime-path atlas.rkt "../r.rkt")

(module+ main
  (define pngs (find-files (λ (x) (equal? #"png" (filename-extension x))) r))

  ;; Here's a source for doing this better than I do it here:
  ;; http://clb.demon.fi/projects/rectangle-bin-packing

  (define how-many (length pngs))
  (define shelves (inexact->exact (ceiling (sqrt how-many))))

  (require racket/pretty)

  (define-values (install! tot-w tot-h)
    (for/fold ([install! void]
               [max-w 0]
               [h 0])
        ([shelf-i (in-range shelves)])
      (define-values
        (shelf-install! shelf-w shelf-h)
        (for/fold ([shelf-install! void]
                   [w 0]
                   [max-h 0])
            ([p (in-list pngs)]
             [i (in-naturals)]
             #:when (= shelf-i (modulo i shelves)))
          (define p-id
            (string->symbol
             (regexp-replace
              #rx".png$"
              (path->string
               (find-relative-path (simple-form-path r)
                                   (simple-form-path p)))
              "")))
          (define bm (make-object bitmap% p 'png/alpha))
          (define bm-w (send bm get-width))
          (define bm-h (send bm get-height))
          (values
           (λ (bm-dc)
             (send bm-dc draw-bitmap
                   bm w h)
             #;(pretty-display
              `(define-texture
                 ,p-id
                 the-texture-atlas
                 ,w ,h ,bm-w ,bm-h))
             (pretty-display
              `(define-texture
                 ,p-id
                 the-texture-atlas
                 (exact->inexact (/ ,w ,tot-w))
                 (exact->inexact (/ ,h ,tot-h))
                 (exact->inexact (/ ,bm-w ,tot-w))
                 (exact->inexact (/ ,bm-h ,tot-h))))
             (shelf-install! bm-dc))
           (+ w bm-w)
           (max max-h bm-h))))
      (values
       (λ (bm-dc)
         (shelf-install! bm-dc)
         (install! bm-dc))
       (max max-w shelf-w)
       (+ h shelf-h))))

  (define atlas-bm (make-object bitmap% tot-w tot-h #f #t))
  (define atlas-bm-dc (new bitmap-dc% [bitmap atlas-bm]))

  (with-output-to-file atlas.rkt
    #:exists 'replace
    (λ ()
      (printf "#lang racket/base\n")
      (pretty-display
       `(require gb/graphics/texture-atlas-lib))
      (pretty-display
       `(define the-texture-atlas
          (texture-atlas/size (add1 ,how-many))))
      (pretty-display
       `(define texture-atlas-width
          ,tot-w))
      (pretty-display
       `(define texture-atlas-height
          ,tot-h))
      (pretty-display
       `(provide the-texture-atlas
                 texture-atlas-width
                 texture-atlas-height))
      (printf "\n")

      (pretty-display
       `(define-texture
          none
          the-texture-atlas
          0.0 0.0 0.0 0.0))
      
      (install! atlas-bm-dc)))

  (send atlas-bm save-file
        atlas.png
        'png
        100))
