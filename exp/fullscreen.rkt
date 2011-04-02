#lang racket/gui
(require "joystick.rkt")

(define (make-fullscreen-canvas/ratio LABEL W H ON-PAINT ON-CHAR)
  (define-values (w h) (get-display-size #t))
  (define-values (x y) (get-display-left-top-inset #t))
  
  (define c-scale
    (min (/ w W)
         (/ h H)))
  (define cw (* W c-scale))
  (define ch (* H c-scale))
  (define hm (/ (- w cw) 2))
  (define vm (/ (- h ch) 2))
  
  (define frame 
    (new frame% 
         [label LABEL]
         [x 0] [y (* -1 2 y)]
         [width w] [height h]
         [style '(hide-menu-bar no-resize-border no-caption no-system-menu)]))
  
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
            (send dc set-background "black")
            (send dc clear))]))
  
  (define left-border (make-horizontal-border))
  (define this-canvas%
    (class canvas% 
      (define/override (on-paint)
        (send (send this get-dc) set-scale c-scale c-scale)
        (ON-PAINT this))
      (define/override (on-char k)
        (ON-CHAR k))
      
      (super-new)))
  (define canvas
    (new this-canvas%
         [parent horiz-pane]
         [min-width cw]
         [min-height ch]
         [horiz-margin hm]
         [style '(gl)]))
  (define right-border (make-horizontal-border))
  (define bot-border (make-vertical-border))
  
  (send frame show #t))

(thread
 (λ ()
   (define jss (get-all-joystick-snapshot-thunks))
   (let loop ()
     (for ([js (in-list jss)]
           [i (in-naturals)])
       (printf "~a: ~a\n" i (js)))
     (sleep 1)
     (loop))))

(make-fullscreen-canvas/ratio 
 "Example"
 16 9 
 (λ (c)
   (define dc (send c get-dc))
   (send dc set-background "white")
   
   (send dc clear)
   
   (send dc set-pen "red" 1 'solid)
   (send dc draw-point 0 0)
   (send dc draw-point 16 9))
 (λ (k)
   (void)))

