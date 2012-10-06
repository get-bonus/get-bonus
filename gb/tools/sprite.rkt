#lang racket/base
(require racket/cmdline
         racket/file
         racket/path
         racket/draw
         racket/class)

(define the-src-bm #f)
(define the-src-bm-dc #f)
(define the-manifest #f)
(define the-dest-root #f)
(define the-free-dest-root #f)

(command-line #:program "sprite"
              #:args (orig-pth dest-manifest dest-root free-dest-root)
              (when (file-exists? dest-manifest)
                (delete-file dest-manifest))
              (set! the-src-bm
                    (make-object bitmap% orig-pth 'png/alpha #f #t))
              (set! the-src-bm-dc
                    (new bitmap-dc% [bitmap the-src-bm]))
              (set! the-dest-root dest-root)
              (unless (equal? dest-root free-dest-root)
                (set! the-free-dest-root free-dest-root))
              (set! the-manifest dest-manifest))

(define (sprite name lx ly w h)
  (define new-bm (make-bitmap w h))
  (define bm-dc (new bitmap-dc% [bitmap new-bm]))

  (send bm-dc draw-bitmap-section
        the-src-bm 0 0
        lx ly w h)

  (define sprite-pth
    (build-path the-dest-root (format "~a.png" name)))
  (make-directory* (path-only sprite-pth))

  (send new-bm save-file sprite-pth 'png 100)

  (when the-free-dest-root
    (define new-free-bm (make-bitmap w h))
    (define free-bm-dc (new bitmap-dc% [bitmap new-free-bm]))

    (define color-weight (/ 1 (* w h)))

    (define a-color (make-object color%))
    (define-values
      (total-red total-green total-blue count)
      (for*/fold ([total-red 0]
                  [total-green 0]
                  [total-blue 0]
                  [count 0])
          ([x (in-range w)]
           [y (in-range h)])
        (send the-src-bm-dc get-pixel (+ lx x) (+ ly y) a-color)
        (if (= 1.0 (send a-color alpha))
          (values (+ total-red (send a-color red))
                  (+ total-green (send a-color green))
                  (+ total-blue (send a-color blue))
                  (+ count 1))
          (values total-red
                  total-green
                  total-blue
                  count))))

    (define (to-byte tot)
      (inexact->exact 
       (round
        (exact->inexact 
         (/ tot count)))))
    
    (define average-color
      (make-object color% 
                   (to-byte total-red)
                   (to-byte total-green)
                   (to-byte total-blue)))
    (send free-bm-dc set-pen
          average-color 1 'solid)
    (send free-bm-dc set-brush
          average-color 'solid)

    (send free-bm-dc draw-rectangle
          0 0 w h)
    #;
    (for* ([x (in-range w)]
           [y (in-range h)])
      (send the-src-bm-dc get-pixel (+ lx x) (+ ly y) a-color)
      (when (= 1.0 (send a-color alpha))
        (send free-bm-dc draw-point
              x y)))

    (define free-sprite-pth
      (build-path the-free-dest-root (format "~a.png" name)))
    (make-directory* (path-only free-sprite-pth))

    (send new-free-bm save-file free-sprite-pth 'png 100))

  (write-to-file (path->bytes sprite-pth) the-manifest #:exists 'append))

(provide sprite
         (all-from-out racket/base))
