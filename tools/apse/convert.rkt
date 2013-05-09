#lang racket/base
(require racket/function
         racket/draw
         racket/file
         racket/path
         racket/format
         data/gvector
         racket/class)

(define (convert db-path png-path)
  (define new-bm
    (make-object bitmap% png-path 'png/alpha #f #t))
  (define w (send new-bm get-width))
  (define h (send new-bm get-height))
  (define pixels (make-bytes (* w h 4)))
  (send new-bm get-argb-pixels 0 0 w h pixels)

  (define new-pixels (make-bytes (* w h)))
  (define palette (make-gvector))
  (define color->palette (make-hash))

  (define (palette-lookup! c)
    (hash-ref! color->palette c
               (λ ()
                 (gvector-add! palette c)
                 (sub1 (gvector-count palette)))))
  ;; Transparent
  (palette-lookup! (vector   0   0   0   0))
  ;; Black
  (palette-lookup! (vector 255   0   0   0))

  (for* ([x (in-range w)]
         [y (in-range h)])
    (define xy-offset (+ (* 4 y w) (* 4 x)))
    (define a (bytes-ref pixels (+ xy-offset 0)))
    (define r (bytes-ref pixels (+ xy-offset 1)))
    (define g (bytes-ref pixels (+ xy-offset 2)))
    (define b (bytes-ref pixels (+ xy-offset 3)))
    (define c (vector a r g b))
    (bytes-set! new-pixels
                (+ (* y w) x)
                (palette-lookup! c)))

  (define how-many (gvector-count palette))

  (when (<= how-many 10)
    (define sprite-dir
      (build-path db-path "sprites"
                  (path-replace-suffix (file-name-from-path png-path)
                                       #".spr")))
    (make-directory* sprite-dir)

    (write-to-file
     (cons w h)
     (build-path sprite-dir "meta"))

    (display-to-file
     new-pixels
     (build-path sprite-dir "0.img"))

    (define palette-id
      (path->string
       (path-replace-suffix (file-name-from-path png-path)
                            #"")))
    (define palette-file
      (path-replace-suffix (file-name-from-path png-path)
                            #".pal"))
    (make-directory* (build-path db-path "palettes"))
    (write-to-file
     (gvector->vector palette)
     (build-path db-path "palettes" palette-file))

    (write-to-file
     (list palette-id)
     (build-path sprite-dir "palettes"))))

(module+ main
  (require racket/cmdline)
  (command-line #:program "convert"
                #:args (database . pngs)
                (for-each (curry convert database) pngs)))
