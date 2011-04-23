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
         "../../exp/fmatrix.rkt"
         (only-in "../../exp/path-finding.rkt"
                  manhattan-distance)
         (prefix-in cd: 
                    (combine-in "../../exp/cd-narrow.rkt"
                                "../../exp/cd-broad.rkt")))

(define (rad->deg r)
  (* r (/ 180 pi)))
(define (sequence-not-empty? s)
  (for/or ([e s]) #t))


(define title
  (gl:string->texture "ハングリーマン"
                      #:size 50 #:family 'decorative))

(define-runtime-path resource-path "r")
(define-syntax-rule (define-sound id f)
  (define id (path->audio (build-path resource-path f))))
(define-syntax-rule (define-texture id f)
  (define id (gl:path->texture (build-path resource-path f))))
             
(define width 28)
(define height 31)
(define center-pos
  (psn (/ width 2.) (/ height 2.)))

; Much enlightenment from http://gameinternals.com/post/2072558330/understanding-pac-man-ghost-behavior
(define-runtime-path default-map "default.map")
(match-define 
 (list wall hall gate jail)
 (bytes->list #"1023"))
(define (path->layout p)
  (apply bytes-append (rest (file->bytes-lines p))))
(define layout (path->layout default-map))

; XXX make layouts widescreen (56 width?)
; XXX ghosts/pacman are the wrong size
; XXX randomly generate layouts --- ensure that every 0 has at least 2 adjacents 0, but prefer to not have more than 2, start from edges -- I want a "braid" maze like http://www.astrolog.org/labyrnth/algrithm.htm http://www.astrolog.org/labyrnth/sample/blindaly.gif https://github.com/jamis/theseus
; XXX look at http://media.giantbomb.com/uploads/0/1450/1620957-30786cedx_screenshot03_super.jpg
; XXX turn the layout into a nice graphic with rounded walls, wide tunnels, etc
; XXX show points
; XXX kill ghosts / be killed
; XXX render ui
; XXX increase speed with time/score
; XXX add fruits
; XXX add music / sound effects
; XXX respawn pellets / change layout on left/right when pellets gone on other side
; XXX stationary ghosts that awaken
; XXX ghost train
; XXX bomb
; XXX fruit appears after 70 dots and 170 dots
; XXX each level has 240 dots and 4 powerups
; XXX move slower and escape in frightened mode
; XXX decrease frightened time with time/score
; XXX chaser starts outside house
; XXX ambusher exits immediately
; XXX fickle exits after 30 dots
; XXX stupid leaves after 1/3 of the dots

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
   -.5 -.5
   (gl:texture/px 
    sprites-t
    1 1
    (+ 3 (* 17 (+ (rate 2 10 frame-n) (* 2 dir-n))))
    (+ 125 (* 18 n))
    14 12)))
  
(define (player-animation n)
  (gl:translate 
   -.5 -.5
   (gl:texture/px sprites-t
                  1 1
                  (+ 3 (* 15 (rate 3 10 n))) 90
                  14 14)))
(define player-r .499)

(define pellet-r (/ player-r 6))
(define pellet-img
  (gl:scale pellet-r pellet-r
            (gl:circle)))

(define mid-point 
  (/ width 2))
(define (layout-ref r c)
  (define vc
    (if (c . < . mid-point)
        c
        (- width c 1)))
  (bytes-ref layout (+ (* r  mid-point) vc)))
(define (r->y r)
  (- height r 1))
(define (y->r y)
  (- height (+ y 1)))
(define (layout-ref/xy x y)
  (layout-ref (y->r y) x))

(test
 (for ([rand (in-range height)])
   (test
    (r->y (y->r rand)) => rand
    (y->r (r->y rand)) => rand)))

(define whole-map
  (gl:color 
   0. 0. 1. 0.
   (gl:for*/gl
    ([c (in-range width)]
     [r (in-range height)])
    (define x c)
    (define y (r->y r))
    (gl:translate 
     (+ x .5) (+ y .5)
     (if (equal? wall (layout-ref r c))
         (gl:translate -.5 -.5 (gl:rectangle 1. 1.))
         gl:blank)))))
(define map-space
  (for*/fold ([s (cd:space width height 1. 1.)])
    ([c (in-range width)]
     [r (in-range height)])
    (define x c)
    (define y (- height r 1))
    (define cx (+ x .5))
    (define cy (+ y .5))
    (if (equal? wall (layout-ref r c))
        (cd:space-insert s (cd:aabb (psn cx cy) .5 .5) 'map)
        s)))

(define (wrap-at top n)
  (cond
    [(n . < . 0)
     (+ top n)]
    [(top . <= . n)
     (- n top)]
    [else
     n]))
(test
 (wrap-at width 5) => 5
 (wrap-at width -1) => (- width 1)
 (wrap-at width width) => 0
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
  (psn 14.5 16.5))

(define speed
  (* 5. RATE))

(define right 0)
(define left pi)
(define up (* .5 pi))
(define down (* 1.5 pi))

(define (->i x)
  (inexact->exact
   (floor x)))
(define (pos->cell p)
  (match-define (psn* (app ->i x0) (app ->i y0)) p)
  (cons x0 y0))

(test 
 (pos->cell (psn 14.5 7.)) => (cons 14 7)
 (pos->cell (psn 13.9 6.)) => (cons 13 6))

(define reverse-direction
  (match-lambda
    ['left 'right]
    ['right 'left]
    ['up 'down]
    ['down 'up]))

(define (cell-neighbors/no-reverse c last-cell)
  (match-define (cons x y) c)
  (define-syntax-rule
    (try [ndir nx* ny*]
         ...)
    (append
     (let* ([nx (wrap-at width nx*)]
            [ny (wrap-at height ny*)]
            [nc (cons nx* ny*)])
     (if (or (equal? last-cell nc)
             (not (= hall (layout-ref/xy nx ny))))
         empty
         (list nc)))
     ...))
  (try [left (sub1 x) y]
       [up x (add1 y)]
       [right (add1 x) y]
       [down x (sub1 y)]))

(test
 (layout-ref/xy 2 1) => hall
 (layout-ref/xy 1 1) => hall
 (wrap-at width 28) => 0
 (wrap-at height 16) => 16
 (layout-ref/xy 0 16) => hall
 (cell-neighbors/no-reverse (cons 27 16) (cons 26 16)) => (list (cons 28 16))
 (cell-neighbors/no-reverse (cons 28 16) (cons 27 16)) => (list (cons 29 16))
 (cell-neighbors/no-reverse (cons 0 16) (cons 1 16)) => (list (cons -1 16))
 (cell-neighbors/no-reverse (cons 10 19) (cons 11 19)) => (list (cons 9 19))
 (cell-neighbors/no-reverse (cons 9 19) (cons 10 19)) => (list (cons 9 18))
 (cell-neighbors/no-reverse (cons 9 19) (cons 9 20)) => (list (cons 10 19) (cons 9 18))
 (cell-neighbors/no-reverse (cons 1 1) (cons 2 1)) => (list (cons 1 2))
 (cell-neighbors/no-reverse (cons 2 1) (cons 3 1)) => (list (cons 1 1)))

(define (pos->cell-distance p c)
  (manhattan-distance (pos->cell p) c))
(define (pos->pos-distance p1 p2)
  (pos->cell-distance p1 (pos->cell p2)))

(define (movement-vector p0 c)
  (match-define (psn* (app ->i x0) (app ->i y0)) p0)
  (match-define (cons x1 y1) c)
  (define dir
    (- (psn (exact->inexact x1) (exact->inexact y1))
       (psn (exact->inexact x0) (exact->inexact y0))))
  (define (min* x y)
    (min (abs x) y))
  (match dir
    [(app psn-y 0.)
     (define dist-to-center
       (- (+ .5 y0) (psn-y p0)))
     (if (= 0. dist-to-center)
         (make-polar speed 
                     (if (> (psn-x dir) 0)
                         right
                         left))
         (make-polar 
          (min* dist-to-center speed)
          (if (dist-to-center . < . 0)
              down
              up)))]
    [(app psn-x 0.)
     (define dist-to-center
       (- (+ .5 x0) (psn-x p0)))
     (if (= 0. dist-to-center)
         (make-polar speed 
                     (if (> (psn-y dir) 0)
                         up
                         down))
         (make-polar 
          (min* dist-to-center speed)
          (if (dist-to-center . < . 0)
              left
              right)))]))

(define (posn-in-dir p mdir)
  (posn->v p (make-polar speed mdir)))
(define (posn->v p v)
  (wrap width height (+ p v)))
(define (try-direction p mdir)
  (try-move p (posn-in-dir p mdir)))
(define (try-move p mp)
  (if (sequence-not-empty? 
       (cd:space-collisions? 
        map-space (cd:aabb mp player-r player-r)))
      p
      mp))

; Returns the first without calling the measure
(define (argmin* m l)
  (if (pair? (rest l))
      (argmin m l)
      (first l)))

(define outside-jail
  (+ jail-pos (psn 0. 3.)))
(define outside-jail-right-of
  (pos->cell
   (+ outside-jail 1.)))
(define TIME-TO-SCATTER (/ 7 RATE)) ; 7 seconds
(define TIME-TO-CHASE (/ 20 RATE)) ; 20 seconds

(struct static (count fmatrix display))
(define (layout->static-objs layout)
  (define-values
    (count fm)
    (for*/fold ([ct 0] [fm (fmatrix width height)])
      ([x (in-range width)]
       [y (in-range height)])
      ; XXX Don't put them close to the ghost house?
      (if (= hall (layout-ref/xy x y))
          (values (add1 ct) (fmatrix-set fm x y 'pellet))
          (values ct fm))))
  (static count fm (static-fm->display fm)))
(define (static-fm->display fm)
  (gl:color/% 
   (make-object color% 255 161 69)
   (gl:for*/gl      
    ([x (in-range width)]
     [y (in-range height)])
    (match (fmatrix-ref fm x y #f)
      ['pellet
       (gl:translate (+ x .5) (+ y .5) pellet-img)]
      [#f
       gl:blank]))))
(define (static-ref st x y)
  (fmatrix-ref (static-fmatrix st) x y #f))
(define (static-chomp st x y)
  (match-define (static c fm _) st)
  (define fm-n
    (fmatrix-set fm x y #f))
  (static (sub1 c) fm-n 
          (static-fm->display fm-n)))

(struct game-st (frame score static-objs dyn-objs))
(struct player (pos dir next-dir))
(struct ghost (n pos target dir last-cell scatter? frames-to-switch))
(define (make-ghost n)
  (ghost n outside-jail #f 'left outside-jail-right-of #t TIME-TO-SCATTER))

; XXX Force it to be in-accessible?
(define (scatter-tile)
  (psn (* (random) width)
       (* (random) height)))

(define pellet-pts 10)

(big-bang
   (game-st 0 0 (layout->static-objs layout)
            (hasheq
             'chaser (make-ghost 0)
             'ambusher (make-ghost 1)
             'fickle (make-ghost 2)
             'stupid (make-ghost 3)
             'player (player (psn 13.5 7.5) (* .5 pi) (* .5 pi))))
   #:sound-scale
   (/ width 2.)
   #:tick
   (λ (w cs)
     (define c (last cs))
     (match-define (game-st frame score st dyn-objs) w)
     (define frame-n (add1 frame))
     (define dyn-objs:post-movement
       (for/hasheq ([(k v) (in-hash dyn-objs)])
         (values
          k
          (match v
            [(struct* ghost 
                      ([pos p]
                       [last-cell lc]
                       [target l-target]
                       [scatter? scatter?] 
                       [frames-to-switch switch-n]))
             (define n-switch-n* (sub1 switch-n))
             (define-values (n-scatter? n-switch-n) 
               (if (zero? n-switch-n*)
                   (if scatter?
                       (values #f TIME-TO-CHASE)
                       (values #t TIME-TO-SCATTER))
                   (values scatter? n-switch-n*)))
             (define same-mode?
               (equal? scatter? n-scatter?))
             (define c (pos->cell p))
             (define nps*
               (cell-neighbors/no-reverse c lc))
             (define nps
               ; This makes them allowed, but not obligated, to switch directions.
               (if (not same-mode?)
                   (list* lc nps*)
                   nps*))
             (define pp (player-pos (hash-ref dyn-objs 'player)))
             (define target
               (if (and l-target same-mode?
                        (if (not n-scatter?) (= (length nps) 1) #t))
                   l-target
                   (if n-scatter?
                       (scatter-tile)
                       (match k
                         ['chaser 
                          pp]
                         ['ambusher
                          (+ pp
                             (make-polar 4 (player-dir (hash-ref dyn-objs 'player))))]
                         ['fickle
                          (define v
                            (- pp
                               (ghost-pos (hash-ref dyn-objs 'ambusher))))
                          (+ pp (make-polar (* 2 (magnitude v)) (angle v)))]
                         ['stupid
                          (if (<= (pos->pos-distance pp p) 8)
                              ; XXX This can keep him target the spot where the player WAS
                              l-target #;(scatter-tile)
                              pp)]))))
             (define next-cell
               (argmin* (curry pos->cell-distance target)
                        nps))
             (define mv
               (movement-vector p next-cell))
             (define np
               (posn->v p mv))
             (define ndir 
               (angle-direction (angle mv)))
             (struct-copy ghost v
                          [target target]
                          [scatter? n-scatter?]
                          [frames-to-switch n-switch-n]
                          [last-cell 
                           (if (equal? c (pos->cell np))
                               lc
                               c)]
                          [pos np]
                          [dir ndir])]
            [(player p dir next-dir)
             (define stick (controller-dpad c))
             (define next-dir-n 
               ; If the stick is stable, 
               ; then don't change the direction
               (if (= stick 0.+0.i)
                   next-dir
                   (angle (cardinate stick))))
             ; The coorridors used to feel too "tight" 
             ; and easy to get stuck on an edge, but I 
             ; think this got fixed
             (define np (try-direction p next-dir-n))
             ; Don't change the direction if we couldn't
             ; move in it
             (define actual-dir
               (if (= np p)
                   dir
                   next-dir-n))
             (define nnp
               (if (= np p)
                   (try-direction p dir)
                   np))
             (player nnp actual-dir next-dir-n)]))))
     (define-values
       (score-n st-n)
       (let ()
         (match-define (cons x y) (pos->cell (player-pos (hash-ref dyn-objs:post-movement 'player))))
         (match (static-ref st x y)
           ['pellet
            (values (+ score pellet-pts)
                    (static-chomp st x y))]
           [#f
            (values score st)])))
     (define dyn-objs:final
       dyn-objs:post-movement)
     (values 
      (game-st frame-n score-n st-n dyn-objs:final)
      ; XXX Render UI
      ; Put this whole thing in the center of the screen by
      ; using translate and adding a scale to focus?
      (gl:focus 
       width (+ height 3) width (+ height 3) 0 0
       (gl:background 
        0. 0. 0. 0.
        (gl:color
         1. 1. 1. 1.
         (gl:center-texture-at 
          (psn (/ width 2.) (+ height 2.5))
          title))
        (gl:translate
         0 1
         (gl:seqn
          whole-map
          (static-display st)
          (gl:for/gl
           ([v (in-hash-values dyn-objs:final)])
           (match v
             [(struct* ghost ([n n] [pos p] [target tp] [dir dir]))
              ; XXX dead mode
              (gl:seqn
               (gl:translate 
                (psn-x p) (psn-y p)
                (ghost-animation n frame-n dir))
               (gl:translate
                (- (psn-x tp) .5) (- (psn-y tp) .5)
                (gl:color/%
                 (match n
                   [0 (make-object color% 169 16 0)]
                   [1 (make-object color% 215 182 247)]
                   [2 (make-object color% 60 189 255)]
                   [3 (make-object color% 230 93 16)])
                 (gl:rectangle 1. 1. 'outline))))]
             [(player p dir _)
              (gl:translate 
               (psn-x p) (psn-y p)
               (gl:rotate
                (rad->deg dir)
                (player-animation frame-n)))]))
          #;(gl:color
             255 255 255 0
             ; Draw horizontal lines
             (gl:for/gl
              ([y (in-range (add1 height))])
              (gl:line 0 y width y))
             ; Draw vertical lines
             (gl:for/gl
              ([x (in-range (add1 width))])
              (gl:line x 0 x height)))))))
      empty))
   #:listener
   (λ (w) center-pos)
   #:done?
   (λ (w) #f))