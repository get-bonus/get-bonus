#lang racket/gui

(define-values (w h) (get-display-size #t))
(define-values (x y) (get-display-left-top-inset #t))

(define frame 
  (new frame% 
       [label "Example"]
       [x 0] [y (* -1 2 y)]
       [width w] [height h]
       [style '(hide-menu-bar no-resize-border no-caption no-system-menu)]))

(define (max-in-ratio rw rh w h)
  (define scale
    (min (/ w rw)
         (/ h rh)))
  (define cw (* rw scale))
  (define ch (* rh scale))
  (values
   cw ch
   (/ (- w cw) 2)
   (/ (- h ch) 2)))

(define-values (cw ch hm vm) 
  (max-in-ratio 16 9 w h))

(define vert-pane
  (new vertical-pane%
       [parent frame]
       [min-width w]
       [min-height h]))

(define (make-vertical-border)
  (new canvas%
       [parent vert-pane]
       [min-width w]
       [min-height vm]
       [paint-callback
        (λ (c dc)
          (send dc set-background "black")
          (send dc clear))]))

(define top-border (make-vertical-border))

(define horiz-pane
  (new horizontal-pane%
       [parent vert-pane]
       [min-width w]
       [min-height ch]))

(define (make-horizontal-border)
  (new canvas%
       [parent horiz-pane]
       [min-height ch]
       [min-width hm]
       [paint-callback
        (λ (c dc)
          (send dc set-background "blue")
          (send dc clear))]))

(define left-border (make-horizontal-border))
(define canvas
  (new canvas%
       [parent horiz-pane]
       [min-width cw]
       [min-height ch]
       [horiz-margin hm]
       [paint-callback
        (λ (c dc)
          (send dc set-background "white")
          
          (send dc clear)
          
          (send dc set-pen "red" 10 'solid)
          (send dc draw-point 0 0)
          (send dc draw-point cw ch))]))
(define right-border (make-horizontal-border))
(define bot-border (make-vertical-border))

(send frame show #t)