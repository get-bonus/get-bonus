#lang racket/gui
(require racket/runtime-path
         gb/lib/component)

(define-runtime-path resource-path "../resources")

(module+ main
  (define frame (new frame% [label "Sprite Sheet Tool"]))

  ;; Get the file
  (define p
    (parameterize ([current-directory resource-path])
      (get-file "Please select a sprite sheet to inspect"
                frame
                resource-path
                #f #f empty
                (list (list "PNGs" "*.png")))))
  (define bm (make-object bitmap% p 'png/alpha #f #t))

  (define d
    (new dialog% [label "Computing sprites..."]
         [parent frame]))
  (define m
    (new message%
         [label "Computing sprites..."]
         [parent d]))
  (send d show-without-yield)

  ;; XXX Have the user select the background color
  ;; XXX Have the user name the sprites, tag them, and sequence them
  ;; XXX Save the sprite dictionary
  ;; XXX Start from an existing sprite dictionary
  ;; XXX Add keyboard shortcuts
  ;; XXX Maybe put in hit shapes

  ;; Get the sprites
  (define objs (components bm (λ (p) (zero? (pixel-a p)))))
  (define total-sprites (vector-length objs))
  (send d show #f)

  ;; Display the sprites
  (define vp
    (new vertical-panel%
         [parent frame]
         [style '(vscroll)]
         [min-width 800]
         [min-height 600]
         [stretchable-width 800]
         [stretchable-height 600]))

  (define svp
    (new vertical-panel%
         [parent vp]
         [style '(vscroll)]
         [min-width 800]
         [min-height 600]
         [stretchable-width 800]
         [stretchable-height 600]))

  (for ([s (in-vector objs)])
    (match-define (aabb x1 y1 x2 y2) s)
    (define w (- x2 x1 -1))
    (define h (- y2 y1 -1))

    (define hp
      (new horizontal-panel% [parent svp]))
    (define c
      (new canvas%
           [parent hp]
           [style '(no-autoclear)]
           [stretchable-width w]
           [stretchable-height h]
           [min-width w]
           [min-height h]))
    (define cdc (send c get-dc))
    (send cdc draw-bitmap-section
          bm 0 0
          x1 y1 w h)
    (new text-field%
         [label #f]
         [parent hp]))

  (new message%
       [label (format "Total sprites is ~a\n" total-sprites)]
       [parent vp])

  (send frame show #t))
