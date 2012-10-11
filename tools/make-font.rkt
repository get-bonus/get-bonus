#lang racket/base
(require racket/runtime-path
         racket/path
         racket/file
         racket/match
         racket/class
         racket/draw
         racket/cmdline)

(module+ main
  (command-line #:program "make-font"
                #:args (r family-str size-str chars)
                (main r (string->symbol family-str) (string->number size-str) chars)))

(define (main r family size chars)
  (define MAX-W 150)
  (define MAX-H 150)

  (define f
    (make-font #:size size
               #:family family
               #:smoothing 'unsmoothed))
  (define font-dir
    (build-path r
                "fonts"
                (symbol->string family)
                (number->string size)))

  (make-directory* font-dir)

  (define char-dcs
    (for/list ([char (in-string chars)])
      (define char-s (string char))

      (define target (make-bitmap MAX-W MAX-H #t))
      (define dc (new bitmap-dc% [bitmap target]))

      (send dc set-brush "white" 'transparent)
      (send dc erase)
      (send dc set-brush "black" 'transparent)
      (send dc set-font f)
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
         [y (in-range MAX-H)])
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

    (send char-bitmap save-file
          char-file
          'png
          100)))
