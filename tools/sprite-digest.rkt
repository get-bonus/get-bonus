#lang racket/base
(require racket/cmdline
         racket/file
         racket/path
         racket/draw
         racket/class)

(define (digest orig-pth free-sprite-pth)
  (define new-bm
    (make-object bitmap% orig-pth 'png/alpha #f #t))
  (define bm-dc (new bitmap-dc% [bitmap new-bm]))
  (define w (send new-bm get-width))
  (define h (send new-bm get-height))
  (define new-free-bm (make-bitmap w h))
  (define free-bm-dc (new bitmap-dc% [bitmap new-free-bm]))

  (define color-weight (/ 1 (* w h)))

  (define a-color (make-object color%))

  (define some-pixels (make-bytes 10))
  (define (alpha? x y)
    (send bm-dc get-argb-pixels x y 1 1 some-pixels #t #f)
    (zero? (bytes-ref some-pixels 0)))

  (define-values
    (total-red total-green total-blue count)
    (for*/fold ([total-red 0]
                [total-green 0]
                [total-blue 0]
                [count 0])
        ([x (in-range w)]
         [y (in-range h)])
      (send bm-dc get-pixel x y a-color)
      (if (alpha? x y)
        (values total-red
                total-green
                total-blue
                count)
        (values (+ total-red (send a-color red))
                (+ total-green (send a-color green))
                (+ total-blue (send a-color blue))
                (+ count 1)))))

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

  (for* ([x (in-range w)]
         [y (in-range h)])
    (send bm-dc get-pixel x y a-color)
    (unless (alpha? x y)
      (send free-bm-dc set-pixel
            x y average-color)))

  (make-directory* (path-only free-sprite-pth))

  (unless (send new-free-bm save-file free-sprite-pth 'png 100)
    (error 'sprite-digest "Failed to save ~e" free-sprite-pth)))

(command-line #:program "sprite-digest"
              #:args (orig-pth digest-pth)
              (digest orig-pth digest-pth))
