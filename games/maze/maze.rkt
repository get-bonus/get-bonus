#lang racket/gui
(require racket/runtime-path
         tests/eli-tester
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
         "../../exp/math.rkt"
         (prefix-in cd: 
                    (combine-in "../../exp/cd-narrow.rkt"
                                "../../exp/cd-broad.rkt")))

(define (rad->deg r)
  (* r (/ 180 pi)))
(define (sequence-not-empty? s)
  (for/or ([e s]) #t))

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
   1 1 1 1 1 1 0 0 1 0 0 1 1 2 2 ; The gate
   0 0 0 0 0 0 0 0 0 0 0 1 3 3 3 ; The jail
   0 0 0 0 0 0 0 0 0 0 0 1 3 3 3
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
; XXX place pellets into the layout
; XXX have ghosts move around (look at pac-man wiki page)
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

(define (ghost-animation n frame-n dir)
  (define dir-n
    (match dir
      ['right 0]
      ['left 1]
      ['up 2]
      ['down 3]))
  (gl:translate 
   -1 -1
   (gl:texture/px sprites-t
                  2 2
                  (+ 3 (* 17 (+ (rate 2 10 frame-n) (* 2 dir-n)))) (+ 125 (* 18 n))
                  14 12)))
  
(define (player-animation n)
  (gl:translate 
   -1 -1
   (gl:texture/px sprites-t
                  2 2
                  (+ 3 (* 15 (rate 3 10 n))) 90
                  14 14)))
(define player-r .99)

(define mid-point 
  (floor (/ width 2)))
(define (layout-ref r c)
  (define vc
    (if (c . <= . mid-point)
        c
        (- width c 1)))
  (vector-ref layout (+ (* r (add1 mid-point)) vc)))

(define whole-map
  (gl:color 
   0 0 255 0
   (gl:for*/gl
    ([c (in-range width)]
     [r (in-range height)])
    (define x c)
    (define y (- height r 1))
    (gl:translate 
     x y
     (if (= 1 (layout-ref r c))
         (gl:rectangle 1. 1.)
         gl:blank)))))
(define map-space
  (for*/fold ([s (cd:space width height 1. 1.)])
    ([c (in-range width)]
     [r (in-range height)])
    (define x c)
    (define y (- height r 1))
    (define cx (+ x .5))
    (define cy (+ y .5))
    (if (= 1 (layout-ref r c))
        (cd:space-insert s (cd:aabb (psn cx cy) .5 .5) 'map)
        s)))

(define (wrap-at top n)
  (cond
    [(n . < . 0)
     (+ top n)]
    [(top . < . n)
     (- n top)]
    [else
     n]))
(test
 (wrap-at width 5) => 5
 (wrap-at width -1) => (- width 1)
 (wrap-at width (+ width 1)) => 1)

(define (wrap w h p)
  (psn (wrap-at w (psn-x p))
       (wrap-at h (psn-y p))))

(define jail-pos
  (psn 14.5 17.))

(struct player (pos dir next-dir))
(struct ghost (n pos dir))
(struct game-st (frame objs))

(define speed
  (* 5. RATE))

(big-bang
   (game-st 0 
            (hasheq
             'chaser (ghost 0 (+ jail-pos (psn 0. 3.)) 'right)
             'ambusher (ghost 1 jail-pos 'left)
             'fickle (ghost 2 (- jail-pos 1.5) 'up)
             'stupid (ghost 3 (+ jail-pos 1.5) 'down)
             'player (player (psn 14.5 8.) (* .5 pi) (* .5 pi))))
   #:sound-scale
   (/ width 2.)
   #:tick
   (λ (w cs)
     (match-define (cons c _) cs)
     (match-define (game-st frame objs) w)
     (define frame-n (add1 frame))
     (define objs:post-movement
       (for/hasheq ([(k v) (in-hash objs)])
         (values
          k
          (match v
            [(? ghost?)
             ; XXX move
             v]
            [(player p dir next-dir)
             (define stick (controller-dpad c))
             (define next-dir-n 
               ; If the stick is stable, then don't change the direction
               (if (= stick 0.+0.i)
                   next-dir
                   (angle (cardinate stick))))
             ; The coorridors used to feel too "tight" and easy to get stuck on an edge, but I think this got fixed
             (define (try-direction p mdir)
               (define mp (wrap width height (+ p (make-polar speed mdir))))
               (if (sequence-not-empty? (cd:space-collisions? map-space (cd:aabb mp player-r player-r)))
                   p
                   mp))
             (define np (try-direction p next-dir-n))
             ; Don't change the direction if we couldn't move in it
             (define actual-dir
               (if (= np p)
                   dir
                   next-dir-n))
             (define nnp
               (if (= np p)
                   (try-direction p dir)
                   np))
             (player nnp actual-dir next-dir-n)]))))
     (define objs:post-cd
       objs:post-movement)     
     (define objs:final
       objs:post-cd)
     (values 
      (game-st frame-n objs:final)
      (gl:focus 
       width height width height
       (psn-x center-pos) (psn-y center-pos)
       (gl:background 
        0 0 0 0
        whole-map
        (gl:for/gl
         ([v (in-hash-values objs:final)])
         (match v
           [(ghost n p dir)
            ; XXX dead mode
            (gl:translate 
             (psn-x p) (psn-y p)
             (ghost-animation n frame-n dir))]
           [(player p dir _)
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