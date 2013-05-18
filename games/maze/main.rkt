#lang racket/base
(require racket/runtime-path
         racket/math
         racket/match
         racket/file
         racket/list
         racket/function
         tests/eli-tester
         gb/gui/world
         gb/gui/os
         gb/graphics/ngl-main
         gb/data/mvector
         gb/gui/os
         gb/input/controller
         gb/audio/3s
         gb/data/psn
         gb/lib/math
         math/base
         gb/lib/random
         gb/data/fmatrix
         gb/meta
         gb/meta-help
         (only-in gb/ai/path-finding
                  manhattan-distance)
         (for-syntax racket/base
                     racket/syntax)
         (prefix-in cd:
                    (combine-in gb/physics/cd-narrow
                                gb/physics/cd-broad)))

;; https://s3.amazonaws.com/data.tumblr.com/tumblr_m01c27aYQ91qbw2q1o1_1280.jpg?AWSAccessKeyId=AKIAJ6IHWSU3BX3X7X3Q&Expires=1331014634&Signature=MsCEbwNiXX7J5h%2B%2BeCdDjW11mmc%3D

(define (sequence-not-empty? s)
  (for/or ([e s]) #t))

(define char-height
  (texture-height tex:sos/font:0))
(define char-width
  (texture-width tex:sos/font:0))
(define string->sprites
  (make-string-factory spr:sos/font))

(define-runtime-path resource-path "r")
(define-syntax-rule (define-sound id f)
  (define id (path->audio (build-path resource-path f))))

(define-sound se:crunch "crunch.wav")
(define-sound se:bgm "bgm.ogg")
;; XXX cut this better
(define-sound se:power-up "power-up.ogg")

(require games/maze/map)
(define scale 16)

(define center-pos
  (psn (/ width 2.) (/ height 2.)))

;; Much enlightenment from http://gameinternals.com/post/2072558330/understanding-pac-man-ghost-behavior

;; XXX ghosts/pacman are the wrong size
;; XXX turn the layout into a nice graphic with rounded walls, wide tunnels, etc
;; XXX increase speed with time/score
;; XXX stationary ghosts that awaken
;; XXX ghost train
;; XXX bomb

(define (rate how-many how-often t)
  (modulo (floor (/ t how-often)) how-many))

(define (ghost-animation n frame-n dir)
  (define dir-n
    (match dir
      ['right 0]
      ['left 1]
      ['up 2]
      ['down 3]))
  (ghost-sprite n dir-n frame-n))

(define-syntax (ghost-match stx)
  (syntax-case stx ()
    [(_ ghost-e set-e frame-e)
     (with-syntax
         ([((ghost-n set-n frame-n maze/ghost/ghost-n/set-n/frame-n) ...)
           (for*/list ([ghost-n (in-range 5)]
                       [set-n (in-range 4)]
                       [frame-n (in-range 2)])
             (list ghost-n set-n frame-n
                   #'tex:sos/fauna/snake:0
                   #;
                   (format-id stx "maze/ghost/~a/~a/~a"
                              ghost-n set-n frame-n)))])
       (syntax/loc stx
         (let ([ghost-v ghost-e]
               [set-v set-e]
               [frame-v frame-e])
           (cond
             [(and (= ghost-v ghost-n)
                   (= set-v set-n)
                   (= frame-v frame-n))
              maze/ghost/ghost-n/set-n/frame-n]
             ...))))]))

(define (ghost-sprite which-ghost which-set frame-n)
  (transform
   #:d (* scale -.5) (* scale -.5)
   (rectangle
    (* scale 0.5) (* scale 0.5)
    (ghost-match which-ghost which-set (rate 2 10 frame-n)))))

(define (scared-ghost-animation frame-n warning?)
  (ghost-sprite
   4
   (if (and warning? (even? frame-n)) 1 0)
   frame-n))

(define (player-animation n)
  (transform
   #:d (* scale -.5) (* scale -.5)
   (rectangle
    (* scale 0.5) (* scale 0.5)
    tex:sos/character/cat:0
    #;
    (match (rate 3 10 n)
      [0 maze/player/0]
      [1 maze/player/1]
      [2 maze/player/2]))))
(define player-r .499)

(define pellet-r (/ player-r 6))
(define (pellet-img)
  (rectangle (* scale pellet-r) (* scale pellet-r)))
(define (power-up-img)
  (rectangle (* scale 2 pellet-r) (* scale 2 pellet-r)))
(define (fruit-img)
  (let ()
    (define s (* 3 pellet-r))
    (transform
     #:d (* scale (- (* s .5))) (* scale (- (* s .5)))
     (rectangle (* scale s) (* scale s)))))

(define (quad-power-up-cell q)
  (locate-cell q power-up))
(define (quad-fruit-cell q)
  (locate-cell q fruit))

(define (locate-cell/static st q-id v)
  (locate-cell (hash-ref (static-quads st) q-id) ghost-entry))

(define (xy->quad*r*c x y)
  (define r (y->r y))
  (define c x)
  (define vc
    (if (c . < . h-width)
      c
      (- width c 1)))
  (define vr
    (if (r . < . h-height)
      r
      (- height r 1)))
  (define
    quad
    (cond
      [(and (r . < . h-height) (c . < . h-width))
       'sw]
      [(and (r . < . h-height) (c . >= . h-width))
       'se]
      [(and (r . >= . h-height) (c . < . h-width))
       'nw]
      [(and (r . >= . h-height) (c . >= . h-width))
       'ne]))
  (values quad vr vc))

(define (quad*cell->psn q r*c)
  (match-define (cons r c) r*c)
  (define-values
    (x y)
    (match q
      ['sw (values            c         (- r 1))]
      ['nw (values            c  (- height r 1))]
      ['se (values (- width c 1)        (- r 1))]
      ['ne (values (- width c 1) (- height r 1))]))
  (psn (exact->inexact (+ x .5)) (exact->inexact (+ y .5))))

(define (r->y r)
  (- height r 1))
(define (y->r y)
  (- height (+ y 1)))

(module+ test
  (test
   (for ([rand (in-range height)])
     (test
      (r->y (y->r rand)) => rand
      (y->r (r->y rand)) => rand))))

(define (wrap-at top n)
  (cond
    [(n . < . 0)
     (+ top n)]
    [(top . <= . n)
     (- n top)]
    [else
     n]))
(module+ test
  (test
   (wrap-at width 5) => 5
   (wrap-at width -1) => (- width 1)
   (wrap-at width width) => 0
   (wrap-at width (+ width 1)) => 1))

(define (wrap w h p)
  (psn (wrap-at w (psn-x p))
       (wrap-at h (psn-y p))))

(define (angle-direction a)
  (cond
    [(= right a) 'right]
    [(=    up a) 'up]
    [(=  left a) 'left]
    [else        'down]
    ;; [else (error 'angle-direction "bad direction: ~e" a)]
    ))

(define INIT-SPEED
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

(module+ test
  (test
   (pos->cell (psn 14.5 7.)) => (cons 14 7)
   (pos->cell (psn 13.9 6.)) => (cons 13 6)))

(define reverse-direction
  (match-lambda
   ['left 'right]
   ['right 'left]
   ['up 'down]
   ['down 'up]))

(define (cell-neighbors/no-reverse st c last-cell)
  (match-define (cons x y) c)
  (define-syntax-rule
    (try [ndir nx* ny*]
         ...)
    (append
     (let* ([nx (wrap-at width nx*)]
            [ny (wrap-at height ny*)]
            [nc (cons nx* ny*)])
       (if (or (equal? last-cell nc)
               (= wall (static-map-ref st nx ny)))
         empty
         (list nc)))
     ...))
  (try [left (sub1 x) y]
       [up x (add1 y)]
       [right (add1 x) y]
       [down x (sub1 y)]))

(define (pos->cell-distance p c)
  (manhattan-distance (pos->cell p) c))
(define (pos->pos-distance p1 p2)
  (pos->cell-distance p1 (pos->cell p2)))

(define (movement-vector speed p0 c)
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

(define (posn-in-dir speed p mdir)
  (posn->v p (make-polar speed mdir)))
(define (posn->v p v)
  (wrap width height (+ p v)))
(define (try-direction st speed p mdir)
  (define mp (posn-in-dir speed p mdir))
  (if (sequence-not-empty?
       (cd:space-collisions?
        (static-map-space st)
        (cd:aabb mp player-r player-r)))
    p
    mp))

;; Returns the first without calling the measure
(define (argmin* m l)
  (if (pair? (rest l))
    (argmin m l)
    (first l)))

(define TIME-TO-POWER-WARNING (/ 2 RATE))
(define TIME-TO-POWER (/ 7 RATE))
(define TIME-TO-SCATTER (/ 4.5 RATE))
(define TIME-TO-CHASE (/ 30 RATE))

(define QUADS '(nw ne se sw))
(define quad->dx*dy
  (match-lambda
   ['nw (values 0 h-height)]
   ['ne (values h-width h-height)]
   ['sw (values 0 0)]
   ['se (values h-width 0)]))
(struct static (quads
                map-display map-space
                quad->objs objs-display))
(define (static-map-ref st x y)
  (match-define (struct* static ([quads quads])) st)
  (define-values (q r c) (xy->quad*r*c x y))
  (quad-ref (hash-ref quads q) r c))
(define (place-power-up the-q qo)
  (match-define (quad-objs pellet-count r*c->obj) qo)
  (match-define (cons r c) (quad-power-up-cell the-q))
  (quad-objs (add1 pellet-count)
             (fmatrix-set r*c->obj r c 'power-up)))
(define (place-fruit the-q qo)
  (match-define (quad-objs pellet-count r*c->obj) qo)
  (match-define (cons r c) (quad-fruit-cell the-q))
  (quad-objs (add1 pellet-count)
             (fmatrix-set r*c->obj r c 'fruit)))

(define (quads->space qs)
  (for*/fold ([s (cd:space width height 1. 1.)])
      ([x (in-range width)]
       [y (in-range height)])
    (define-values (q r c) (xy->quad*r*c x y))
    (define cx (+ x .5))
    (define cy (+ y .5))
    (if (equal? wall (quad-ref (hash-ref qs q) r c))
      (cd:space-insert s (cd:aabb (psn cx cy) .5 .5) 'map)
      s)))

(define (quads->display qs)
  (transform
   #:rgba 0. 0. 1. 1.
   (for*/list
       ([x (in-range width)]
        [y (in-range height)])
     (define-values (q r c) (xy->quad*r*c x y))
     (if (equal? wall (quad-ref (hash-ref qs q) r c))
       (transform
        #:d (* scale x) (* scale y)
        (rectangle (* scale 0.5) (* scale 0.5)))
       empty))))

(struct quad-objs (pellet-count r*c->obj))
(define (populate-quad q [old-q #f] [old-qo #f])
  (define-values
    (pc fm)
    (for*/fold ([ct 0] [fm (fmatrix h-height h-width)])
        ([r (in-range h-height)]
         [c (in-range h-width)])
      (cond
        [(= hall (quad-ref q r c))
         (values (add1 ct)
                 (fmatrix-set fm r c 'pellet))]
        [else
         (values ct fm)])))
  (define im1 (quad-objs pc fm))
  (define im2 (place-power-up q im1))
  (define im3
    (if (and old-q
             (let ()
               (define old-fruit-cell (quad-fruit-cell old-q))
               (fmatrix-ref (quad-objs-r*c->obj old-qo)
                            (car old-fruit-cell) (cdr old-fruit-cell)
                            #f)))
      (place-fruit q im2)
      im2))
  im3)

(define (make-static maze-seq)
  (define quads
    (hasheq 'nw (sequence-first maze-seq)
            'ne (sequence-first maze-seq)
            'sw (sequence-first maze-seq)
            'se (sequence-first maze-seq)))
  (define map-display
    (quads->display quads))
  (define map-space
    (quads->space quads))

  (define objs
    (for/hasheq ([(n q) (in-hash quads)])
                (values n (populate-quad q))))

  (static quads map-display map-space
          objs (quad-objs->display objs)))

(define (quad-objs->display os)
  (transform
   #:irgbv (vector 255 161 69) #:a 1.0
   (for*/list
       ([x (in-range width)]
        [y (in-range height)])
     (define-values (q r c) (xy->quad*r*c x y))
     (match-define (quad-objs _ fm) (hash-ref os q))
     (transform
      #:d (* scale x) (* scale y)
      (match (fmatrix-ref fm r c #f)
        ['pellet (pellet-img)]
        ['power-up (power-up-img)]
        ['fruit (fruit-img)]
        [#f
         empty])))))

(define opposite-quad
  (match-lambda
   ['nw 'se]
   ['ne 'sw]
   ['se 'nw]
   ['sw 'ne]))

(define (static-chomp maze-seq st x y)
  (match-define (struct* static
                         ([quads quads]
                          [quad->objs quad->objs]))
                st)
  (define-values (q r c) (xy->quad*r*c x y))
  (match-define (quad-objs qc fm) (hash-ref quad->objs q))

  (define obj
    (fmatrix-ref fm r c #f))
  (if obj
    (let ()
      (define fm-n
        (fmatrix-set fm r c #f))
      (define qc-n (sub1 qc))
      (define im
        (hash-set quad->objs q
                  (quad-objs qc-n fm-n)))

      (define quad->objs-p
        (if (zero? qc-n)
          (let ()
            (define oq (opposite-quad q))
            (hash-update im oq (curry place-fruit (hash-ref quads oq))))
          im))
      (if (eq? obj 'fruit)
        (let ()
          (define oq (opposite-quad q))
          (define nq
            (sequence-first maze-seq))
          (define quads-n
            (hash-set quads oq nq))
          (define old-objs
            (hash-ref quad->objs-p oq))
          (define quad->objs-n
            (hash-set quad->objs-p oq
                      (populate-quad nq (hash-ref quads oq) old-objs)))
          (values (struct-copy
                   static st
                   [quads quads-n]
                   [map-display (quads->display quads-n)]
                   [map-space (quads->space quads-n)]
                   [quad->objs quad->objs-n]
                   [objs-display
                    (quad-objs->display
                     quad->objs-n)])
                  obj))
        (values (struct-copy static st
                             [quad->objs quad->objs-p]
                             [objs-display
                              (quad-objs->display
                               quad->objs-p)])
                obj)))
    (values st obj)))

(define (scatter-tile)
  (psn (round (* (random) width))
       (round (* (random) height))))

;; XXX score multiplier
(define pellet-pts 10)
(define power-up-pts 50)
(define fruit-pts 100)
(define ghost-pts 200)

(define ghost-return 40)

;; XXX Add a slow start-up clock for beginning of game and
;;     after death

(define ((ghost ai-n init-timer))
  (define ai-sym
    (match ai-n
      [0 'chaser]
      [1 'ambusher]
      [2 'fickle]
      [3 'stupid]))
  (define ghost-qid
    (match ai-n
       [0 'nw]
       [1 'ne]
       [2 'sw]
       [3 'se]))
  (define ghost-entry-cell
    (locate-cell/static 
     (os/read* 'static)
     ghost-qid ghost-entry))
  (define outside-jail
    (quad*cell->psn
     ghost-qid
     ghost-entry-cell))
  (define outside-jail-right-of
    (pos->cell
     (+ outside-jail 1.)))
  (define (ghost-graphics pos l-target dir power-left-n)
    (cons
     (transform
      #:d (* scale (psn-x pos)) (* scale (psn-y pos))
      (if (zero? power-left-n)
        (ghost-animation ai-n (current-frame) dir)
        (scared-ghost-animation
         (current-frame)
         (power-left-n . <= . TIME-TO-POWER-WARNING))))
     (transform
      #:d (* scale (- (psn-x l-target) .5))
          (* scale (- (psn-y l-target) .5))
      #:a 1.0
      #:irgbv
      (match ai-n
        [0 (vector 169 16 0)]
        [1 (vector 215 182 247)]
        [2 (vector 60 189 255)]
        [3 (vector 230 93 16)])
      (rectangle (* scale 0.5) (* scale 0.5)))))
  (let wait-loop ([dot-timer init-timer])
    (unless (dot-timer . <= . 0)
      (define event (os/read* 'event #f))
      (os/write
       (list
        (cons 'graphics
              (cons 0
                    (if (even? (current-frame))
                      empty
                      (ghost-graphics outside-jail outside-jail 'left
                                      (os/read* 'power-left)))))))
      (wait-loop (if (eq? event 'pellet)
                   ;; XXX Add a sound effect when the activate?
                   (max 0 (sub1 dot-timer))
                   dot-timer))))
  (let loop ([pos outside-jail]
             [l-target (scatter-tile)]
             [dir 'left]
             [lc outside-jail-right-of]
             [scatter? #t]
             [switch-n TIME-TO-SCATTER])
    (define power-left-n (os/read* 'power-left))
    (define frightened? (not (zero? power-left-n)))
    (define speed
      (if frightened?
        (/ INIT-SPEED 2.)
        INIT-SPEED))
    (define c (pos->cell pos))
    (define n-switch-n*
      (if frightened?
        ;; Don't count frightened time on clocks
        switch-n
        (sub1 switch-n)))
    (define-values (n-scatter? n-switch-n)
      (if (zero? n-switch-n*)
        (if scatter?
          (values #f TIME-TO-CHASE)
          (values #t TIME-TO-SCATTER))
        (values scatter? n-switch-n*)))
    (define same-mode?
      (equal? scatter? n-scatter?))
    (define st (os/read* 'static))
    (define nps*
      (cell-neighbors/no-reverse st c lc))
    (define nps
      ;; This makes them allowed, but not obligated,
      ;; to switch directions.
      (if (not same-mode?)
        (list* lc nps*)
        nps*))
    (define pp
      (os/read* 'player-pos))
    (define target
      (cond
        [(or frightened?
             (and l-target same-mode?
                  (if (not n-scatter?) (= (length nps) 1) #t)))
         l-target]
        [n-scatter?
         (scatter-tile)]
        [else
         (match ai-n
           [0
            pp]
           [1
            (+ pp
               (make-polar
                4
                (os/read* 'player-dir)))]
           [2
            (define v
              (- pp
                 (os/read* 'ambusher pp)))
            (+ pp (make-polar (* 2 (magnitude v))
                              (angle v)))]
           [3
            (if (<= (pos->pos-distance pp pos) 8)
              l-target
              pp)])]))
    (define next-cell
      (argmin* (curry pos->cell-distance target)
               nps))
    (define mv
      (movement-vector speed pos next-cell))
    (define np
      (posn->v pos mv))
    (define ndir
      (angle-direction (angle mv)))
    (define p-cell
      (pos->cell
       (os/read* 'player-pos)))
    (define g-cell
      (pos->cell pos))
    (define-values (events death?)
      (if (equal?
           p-cell
           g-cell)
        (if (power-left-n . > . 0)
          ;; XXX reset dot-timer to ghost-return and jail
          (values (list (cons 'dp2 ghost-pts)) #t)
          ;; XXX Add sound effect
          (values (list (cons 'lives-p -1)) #f))
        (values empty #f)))
    (os/write
     (append
      events
      (list
       (cons ai-sym pos)
       (cons 'graphics
             (cons
              1.0
              (ghost-graphics pos l-target dir power-left-n))))))
    (if death?
      (os/exit ai-n)
      (loop np target ndir
            (if (equal? c (pos->cell np))
              lc
              c)
            n-scatter?
            n-switch-n))))

(define (player)
  (define player-entry-cell
    (locate-cell/static (os/read* 'static) 'sw player-entry))
  (let loop ([p (quad*cell->psn 'sw player-entry-cell)]
             [dir up]
             [next-dir up])
    (define speed INIT-SPEED)
    (define c (os/read* 'controller))
    (define st (os/read* 'static))
    (define stick (controller-ldpad c))
    (define next-dir-n
      ;; If the stick is stable, then don't change the direction
      (if (= stick 0.+0.i)
        next-dir
        (angle (cardinate stick))))
    ;; The coorridors used to feel too "tight" and easy to get stuck
    ;; on an edge, but I think this got fixed
    (define np (try-direction st speed p next-dir-n))
    ;; Don't change the direction if we couldn't move in it
    (define actual-dir
      (if (= np p)
        dir
        next-dir-n))
    (define nnp
      (if (= np p)
        (try-direction st speed p dir)
        np))
    (os/write
     (list
      (cons 'player-pos nnp)
      (cons 'player-dir actual-dir)
      (cons 'graphics
            (cons
             0.0
             (transform
              #:d (* scale (psn-x nnp)) (* scale (psn-y nnp))
              #:rot actual-dir
              (player-animation (current-frame)))))))
    (loop nnp actual-dir next-dir-n)))

(define (game-start maze-seq)
  (big-bang/os
   width (+ height 2) center-pos
   #:sound-scale (/ width 2.)
   (λ ()
     (define init-st (make-static maze-seq))
     (os/write
      (list (cons 'static init-st)
            (cons 'player-pos 0.)
            (cons 'power-left 0)
            (cons 'sound
                  (background (λ (w) se:bgm)
                              #:gain 0.5
                              #:pause-f
                              (compose positive?
                                       (os-sound-reader 'power-left 0))))
            (cons 'sound
                  (background (λ (w) se:power-up)
                              #:gain 1.0
                              #:pause-f
                              (compose zero?
                                       (os-sound-reader 'power-left 0))))))
     (os/thread player)
     (os/write (list (cons 'static init-st)
                     (cons 'power-left 0)))
     (let loop ([score 0]
                [lives 1]
                [power-left 0]
                [st init-st]
                [next-ghost 0]
                [dots-to-ghost 0])
       (define c (os/read* 'controller))
       (define power-left-p (max 0 (sub1 power-left)))
       (match-define
        (cons x y)
        (pos->cell (os/read* 'player-pos)))
       (define-values
         (st-n event)
         (static-chomp maze-seq st x y))
       (define dp1
         (match event
           ['pellet pellet-pts]
           ['power-up power-up-pts]
           ['fruit fruit-pts]
           [#f 0]))
       (define power-left-n
         (if (eq? event 'power-up)
           (+ power-left-p TIME-TO-POWER)
           power-left-p))
       (define lives-p (+ lives (sum (os/read 'lives-p))))
       (define dp2 (sum (os/read 'dp2)))
       (define score-n (+ score dp1 dp2))
       (define-values
         (next-ghost-n dots-to-ghost-n)
         (cond
           [(zero? dots-to-ghost)
            (os/thread (ghost next-ghost 10))
            (values (modulo (add1 next-ghost) 4)
                    (- ghost-return 10))]
           [(eq? event 'pellet)
            (values next-ghost (sub1 dots-to-ghost))]
           [else
            (values next-ghost dots-to-ghost)]))
       (os/write
        (list*
         (cons 'event event)
         (cons 'done?
               (zero? lives-p))
         (cons 'return
               score-n)
         (cons 'graphics
               (cons
                10.
                (list
                 (transform
                  #:rgba 1. 1. 1. 1.
                  (transform
                   #:d (* scale (/ width 2.)) (* scale (+ height 0.5))
                   (string->sprites
                    (format "~a" score-n))))
                 (static-objs-display st)
                 (static-map-display st)
                 (transform #:a 1.0
                            #:dx (* scale (/ width 2.))
                            #:dy (* scale (/ height 2.))
                            (rectangle (* scale (/ width 2.))
                                       (* scale (/ height 2.)))))))
         (cons 'static
               st-n)
         (cons 'power-left
               power-left-n)
         (if (eq? event 'pellet)
           (list (cons 'sound (sound-at se:crunch center-pos #:gain 0.8)))
           empty)))
       (loop
        score-n lives-p
        power-left-n st-n
        next-ghost-n dots-to-ghost-n)))))

(require gb/lib/godel
         gb/lib/godel-seq)

(define maze-seq/s
  (infinite-sequence/s maze/s))

(define game
  (game-info 'maze "Maze"
             (list "Avoid ghosts while collecting points in a randomly-generated maze. Eat a power pellet to turn the tables and eat the ghosts. After you clear each quadrant, eat the fruit to respawn it and continue."
                   "Compare to Pac-Man(R) by Namco (1980)")
             0
             (random-godel-generate maze-seq/s)
             (godel-start maze-seq/s game-start)))

(provide game)
