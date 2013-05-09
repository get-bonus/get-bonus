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
                  (file-name-from-path png-path)))
    (make-directory* sprite-dir)

    (with-output-to-file
        (build-path sprite-dir "meta")
      (λ ()
        (write (cons w h))))

    (with-output-to-file
        (build-path sprite-dir "0.img")
      (λ ()
        (write-bytes new-pixels)))

    (with-output-to-file
        (build-path sprite-dir "palettes")
      (λ ()
        (write (list (gvector->vector palette)))))))

(module+ main
  (require racket/cmdline)
  (command-line #:program "convert"
                #:args (database . pngs)
                (for-each (curry convert database) pngs)))
