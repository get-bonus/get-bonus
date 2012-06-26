#lang racket/gui
(require racket/runtime-path
         (prefix-in gl: gb/graphics/gl)
         gb/lib/component
         gb/graphics/sprites)
(define-runtime-path resource-path "../resources")
(current-directory resource-path)

(define canvas-w 800)
(define canvas-h 600)

(define current-sprite 0)
(define total-sprites 0)
(define (next-sprite! b e) 
  (set! current-sprite (modulo (add1 current-sprite) total-sprites))
  (send progress set-value current-sprite)
  (select-sprite!))
(define (select-sprite!)
  (define cdc (send c get-dc))
  (match-define (aabb x1 y1 x2 y2) (vector-ref objs current-sprite))
  
  (define tw (gl:texture-w tex))
  (define th (gl:texture-h tex))
  (define w (- x2 x1 -1))
  (define h (- y2 y1 -1))
  
  (define ratio (/ canvas-w canvas-h))
  (define w* (max w 100))
  (define h* (* (/ h w) w*))
  
  (define glctx (send cdc get-gl-context))
  (send glctx call-as-current
        (λ () 
          (gl:draw 
           w* (* h* ratio)
           w* (* h* ratio)
           0 0
           (gl:background 255 0 255 0
                          (gl:texture tex w h (/ x1 tw) (/ y1 th) (/ w tw) (/ h th))))
          (send glctx swap-buffers)))
  (send cdc clear))
  
(define frame (new frame% [label "Sprite Sheet Tool"]))
(define c
  (new canvas%
       [parent frame]
       [style '(gl no-autoclear)]
       [min-width canvas-w]
       [min-height canvas-h]))
(define hp (new horizontal-pane% [parent frame]))
(define progress
  (new gauge% [label #f] [range 1] [parent hp]))
(define next 
  (new button% [parent hp] [label "Next sprite"]
       [callback next-sprite!]))
(send frame show #t)

(define p
  (get-file "Please select a sprite sheet to inspect"
            frame
            resource-path
            #f #f empty
            (list (list "PNGs" "*.png"))))
(define tex (gl:path->texture p))
(define bm (make-object bitmap% p 'png/alpha #f #t))

(define d 
  (new dialog% [label "Computing sprites..."]
       [parent frame]))
(define m 
  (new message% 
       [label "Computing sprites..."]
       [parent d]))
(send d show-without-yield)

; XXX Have the user select the background color
; XXX Have the user name the sprites, tag them, and sequence them
; XXX Save the sprite dictionary
; XXX Start from an existing sprite dictionary
; XXX Add keyboard shortcuts
; XXX Maybe put in hit shapes

(define objs (components bm (λ (p) (zero? (pixel-a p)))))
(set! total-sprites (vector-length objs))
(send progress set-range total-sprites)
(send d show #f)

(select-sprite!)
