#lang racket/base
(require racket/match
         racket/file
         racket/list
         racket/class
         racket/draw)

(define (display-sprite db-path sprite output-path)
  (define sprite-dir (build-path db-path "sprites" (format "~a.spr" sprite)))
  (match-define (cons w h) (file->value (build-path sprite-dir "meta")))

  (define (image->bm image-i palette)
    (define bm (make-object bitmap% w h #f #t))
    (define bm-dc (send bm make-dc))

    (define paletted-pixels
      (file->bytes (build-path sprite-dir
                               (format "~a.img" image-i))))    

    (for* ([x (in-range w)]
           [y (in-range h)])
      (define p (bytes-ref paletted-pixels (+ (* y w) x)))
      (match-define (vector a r g b) (vector-ref palette p))
      (send bm-dc set-pixel x y (make-object color% r g b (/ a 255))))

    bm)

  (define palettes (file->value (build-path sprite-dir "palettes")))
  (define palette-name (first palettes))
  (define palette
      (file->value (build-path db-path "palettes" 
                               (format "~a.pal" palette-name))))

  (define the-bm (image->bm 0 palette))
  (send the-bm save-file output-path 'png))

(module+ main
  (require racket/cmdline)
  (command-line #:program "display"
                #:args (db-path sprite output-path)
                (display-sprite db-path sprite output-path)))
