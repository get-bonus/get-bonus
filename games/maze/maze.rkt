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

(define (angle-direction a)
  (cond
    [(= 0 a) 'right]
    [(= (/ pi 2) a) 'up]
    [(= pi a) 'left]
    [else 'down]))

(define jail-pos
  (psn 14.5 17.))

(struct player (pos dir next-dir))
(struct ghost (n pos dir))
(struct game-st (frame objs))

(define speed
  (* 5. RATE))

(require racket/package)
(define-package floyd-warshall (find-direction)
  (define (->i x)
    (inexact->exact (floor x)))
  (define (x*y->i x y)
    (+ (* y width) x))
  (define (psn->i p)
    (match-define (psn* (app ->i x) (app ->i y)) p)
    (x*y->i x y))
  (define (i->psn i)
    (define-values (y x) (quotient/remainder i width))
    (psn (exact->inexact y) (exact->inexact x)))
    
  (define (matrix rows cols df)
    (make-vector (* rows cols) df))
  (define (matrix-ref rows cols m r c)
    (vector-ref m (+ (* r cols) c)))
  (define (matrix-set! rows cols m r c v)
    (vector-set! m (+ (* r cols) c) v))
    
  (define n (* width height))
  (define path (matrix n n +inf.0))
  (define next (matrix n n #f))
  
  ; XXX Incorporate walled-ness
  (printf "Initializing matrix...\n")
  (for* ([x (in-range width)]
         [y (in-range height)])
    (printf "~a,~a\n" x y)
    (define i (x*y->i x y))
    (define-syntax-rule
      (try dx dy)
      (let ()
        (define xp (+ x dx))
        (when (<= 0 xp (sub1 width))
          (define yp (+ y dy))
          (when (<= 0 yp (sub1 height))
            (unless (= 1 (layout-ref yp xp))
              (matrix-set! n n path i (x*y->i xp yp) 1))))))
    (try 1 0) (try -1 0)
    (try 0 1) (try 0 -1))
  
  (printf "Populating matrix...\n")
  (for* ([k (in-range n)])
    (define kp (i->psn k))
    (for ([i (in-range n)])
      (define i->k (matrix-ref n n path i k))
      (define ip (i->psn i))
      (for ([j (in-range n)])
        (define i->k->j 
          (+ i->k
             (matrix-ref n n path k j)))
        (when (i->k->j . < . (matrix-ref n n path i j))
          (matrix-set! n n path i j i->k->j)
          (matrix-set! n n next i j 
                       (angle (- ip kp)))))))
  
  (define (find-direction p0 p1)
    (matrix-ref n n next (psn->i p0) (psn->i p1))))
(open-package floyd-warshall)

(define (posn-in-dir p mdir)
  (wrap width height (+ p (make-polar speed mdir))))
(define (try-direction p mdir)
               (define mp (posn-in-dir p mdir))
               (if (sequence-not-empty? (cd:space-collisions? map-space (cd:aabb mp player-r player-r)))
                   p
                   mp))

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
            [(ghost n p dir)
             (define target
               (match k
                 ['chaser (player-pos (hash-ref objs 'player))]
                 ['ambusher (- (player-pos (hash-ref objs 'player)) 1.)]
                 ['fickle (+ (player-pos (hash-ref objs 'player)) (psn 0. 1.))]
                 ['stupid (psn (* (random) width) (* (random) height))]))
             (define na
               (find-direction p target))
             (define np (try-direction p na))
             (define ndir (angle-direction na))
             (ghost n np ndir)]
            [(player p dir next-dir)
             (define stick (controller-dpad c))
             (define next-dir-n 
               ; If the stick is stable, then don't change the direction
               (if (= stick 0.+0.i)
                   next-dir
                   (angle (cardinate stick))))
             ; The coorridors used to feel too "tight" and easy to get stuck on an edge, but I think this got fixed
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