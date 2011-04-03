#lang racket/gui
(require (prefix-in gl: "gl.rkt")
         "joystick.rkt")

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
        (ON-PAINT this c-scale))
      (define/override (on-char k)
        (ON-CHAR k))
      
      (super-new)))
  (define canvas
    (new this-canvas%
         [parent horiz-pane]
         [min-width cw]
         [min-height ch]
         [horiz-margin hm]
         [style '(gl no-autoclear)]))
  (define right-border (make-horizontal-border))
  (define bot-border (make-vertical-border))
  
  (send frame show #t)
  
  canvas)

(define PX 8)
(define PY 4.5)

(define the-canvas
  (make-fullscreen-canvas/ratio 
   "Example"
   16 9 
   (λ (c c-scale)
     (define dc (send c get-dc))
     (define glctx (send dc get-gl-context))
     (send glctx call-as-current
           (λ () 
             (gl:draw 
              (* 2 16) (* 2 9) 16 9 PX PY
              (gl:background
               255 255 0 0
               (gl:color 0 0 255 0
                         (gl:translate PX PY
                                       (gl:scale 1 1
                                                 (gl:circle))))
               (gl:color 255 0 0 0
                         (gl:translate 0 0
                                       (gl:scale 1 1
                                                 (gl:circle)))
                         (gl:translate 16 9
                                       (gl:scale 1 1
                                                 (gl:circle))))))
             (send glctx swap-buffers)))
     #;(begin
         (send dc set-background "white")     
         (send dc clear)     
         
         (send dc set-pen "blue" 1 'solid)
         (send dc draw-point PX PY)
         
         (send dc set-pen "red" 1 'solid)
         (send dc draw-point 0 0)
         (send dc draw-point 16 9)))
   (λ (k)
     (void))))

(define RATE 1/60)

(thread
 (λ ()
   (define jss (get-all-joystick-snapshot-thunks))
   (let loop ()
     (for ([js (in-list jss)]
           [i (in-naturals)])
       (define s (js))
       (set! PX
             (+ PX
                (* RATE 16/2 (mvector-ref (joystick-state-sticks s) 0 0 0))))
       (set! PY
             (+ PY
                (* RATE 9/2 (mvector-ref (joystick-state-sticks s) 0 0 1)))))
     (send the-canvas refresh-now)
     (sleep RATE)
     (loop))))