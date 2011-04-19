#lang racket/gui
(require racket/runtime-path
         "../../exp/loop.rkt"
         (prefix-in gl: 
                    (combine-in "../../exp/gl.rkt"
                                "../../exp/gl-ext.rkt"))
         "../../exp/sprites.rkt"
         "../../exp/mvector.rkt"
         "../../exp/fullscreen.rkt"
         "../../exp/keyboard.rkt"
         "../../exp/mapping.rkt"
         "../../exp/controller.rkt"
         "../../exp/joystick.rkt"
         "../../exp/3s.rkt"
         "../../exp/psn.rkt"
         (prefix-in cd: "../../exp/cd-narrow.rkt"))

(define (rad->deg r)
  (* r (/ 180 pi)))

; XXX refer to game as ハングリーマン

(define-runtime-path resource-path "r")
(define-syntax-rule (define-sound id f)
  (define id (path->audio (build-path resource-path f))))
(define-syntax-rule (define-texture id f)
  (define id (gl:path->texture (build-path resource-path f))))
             
(define width 29)
(define height 32)
(define center-pos
  (psn (/ width 2.) (/ height 2.)))

(define layout
  (vector  
   1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
   1 0 0 0 0 0 0 0 0 0 0 0 0 0 1
   1 0 0 0 0 0 0 0 0 0 0 0 0 0 1
   1 0 0 1 1 1 0 0 1 1 1 1 0 0 1
   1 0 0 1 1 1 0 0 1 1 1 1 0 0 1
   1 0 0 0 0 0 0 0 0 0 0 0 0 0 0
   1 0 0 0 0 0 0 0 0 0 0 0 0 0 0
   1 0 0 1 1 1 0 0 1 0 0 1 1 1 1
   1 0 0 0 0 0 0 0 1 0 0 0 0 0 1
   1 0 0 0 0 0 0 0 1 0 0 0 0 0 1
   1 1 1 1 1 1 0 0 1 1 1 1 0 0 1
   1 1 1 1 1 1 0 0 1 0 0 0 0 0 0
   1 1 1 1 1 1 0 0 1 0 0 0 0 0 0
   1 1 1 1 1 1 0 0 1 0 0 1 1 1 2 ; The gate
   0 0 0 0 0 0 0 0 0 0 0 1 0 0 0
   0 0 0 0 0 0 0 0 0 0 0 1 0 0 0
   1 1 1 1 1 1 0 0 1 0 0 1 1 1 1
   1 1 1 1 1 1 0 0 1 0 0 0 0 0 0
   1 1 1 1 1 1 0 0 1 0 0 0 0 0 0
   1 1 1 1 1 1 0 0 1 0 0 1 1 1 1
   1 0 0 0 0 0 0 0 0 0 0 0 0 0 1
   1 0 0 0 0 0 0 0 0 0 0 0 0 0 1
   1 0 0 1 1 1 0 0 1 1 1 1 0 0 1
   1 0 0 0 0 1 0 0 0 0 0 0 0 0 0
   1 0 0 0 0 1 0 0 0 0 0 0 0 0 0
   1 1 1 0 0 1 0 0 1 0 0 1 1 1 1
   1 0 0 0 0 0 0 0 1 0 0 0 0 0 1
   1 0 0 0 0 0 0 0 1 0 0 0 0 0 1
   1 0 0 1 1 1 1 1 1 1 1 1 0 0 1
   1 0 0 0 0 0 0 0 0 0 0 0 0 0 0
   1 0 0 0 0 0 0 0 0 0 0 0 0 0 0
   1 1 1 1 1 1 1 1 1 1 1 1 1 1 1))

; XXX make layouts widescreen (56 width?)
; XXX randomly generate layouts
; XXX look at http://media.giantbomb.com/uploads/0/1450/1620957-30786cedx_screenshot03_super.jpg
; XXX turn the layout into a nice graphic with rounded tiles, etc
; XXX turn the layout into a set of collision shapes
; XXX place the ghosts and pellets into the layout
; XXX have ghosts spawn and move around (look at pac-man wiki page)
; XXX run around the map
; XXX get points
; XXX kill ghosts / be killed
; XXX render ui
; XXX increase speed with time/score
; XXX add fruits
; XXX add music / sound effects
; XXX respawn pellets / change layout on left/right when pellets gone on other side
; XXX stationary ghosts that awaken
; XXX ghost train
; XXX bomb

(define-texture sprites-t "pacman.png")

(define (rate how-many how-often t)
  (modulo (floor (/ t how-often)) how-many))
  
(define (player-animation n)
  (gl:translate 
   -1 -1
   (gl:texture/px sprites-t
                  2 2
                  (+ 3 (* 15 (rate 3 10 n))) 90
                  14 14)))

(define mid-point 
  (floor (/ width 2)))
(define whole-map
  (gl:color 
   0 0 255 0
   (gl:for*/gl
    ([c (in-range width)]
     [r (in-range height)])
    (define vc
      (if (c . <= . mid-point)
          c
          (- width c 1)))
    (gl:translate 
     c (- height r 1)
     (if (= 1 (vector-ref layout (+ (* r (add1 mid-point)) vc)))
         (gl:rectangle 1. 1.)
         gl:blank)))))

(struct player (pos dir))
(struct ghost ())
(struct game-st (frame objs))

(define speed
  (* 3. RATE))

(big-bang
   (game-st 0 
            (hasheq
             'chaser (ghost)
             'ambusher (ghost)
             'fickle (ghost)
             'stupid (ghost)
             'player (player (psn 14.5 8.) 0.)))
   #:sound-scale
   (/ width 2.)
   #:tick
   (λ (w cs)
     (match-define (cons c _) cs)
     (match-define (game-st frame objs) w)
     (define frame-n (add1 frame))
     (define objs-n
       (for/hasheq ([(k v) (in-hash objs)])
         (match v
           [(ghost)
            ; XXX move
            (values k v)]
           [(player p dir)
            (values k 
                    ; XXX wrap around screen
                    ; XXX collision detection
                    (player (+ p (* (controller-dpad c) speed))
                            (angle (controller-dpad c))))])))
     (values 
      (game-st frame-n objs-n)
      (gl:focus 
       width height width height
       (psn-x center-pos) (psn-y center-pos)
       (gl:background 
        0 0 0 0
        whole-map
        (gl:for/gl
         ([(k v) (in-hash objs-n)])
         (match v
           [(ghost)
            ; XXX
            gl:blank]
           [(player p dir)
            ; XXX have animation frame stick if you haven't moved
            (gl:translate 
             (psn-x p) (psn-y p)
             (gl:rotate
              (rad->deg dir)
              (player-animation frame-n)))]))))
      empty))
   #:listener
   (λ (w) center-pos)
   #:done?
   (λ (w) #f))