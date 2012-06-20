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
         "../../exp/random.rkt"
         "../../exp/fmatrix.rkt"
         (only-in "../../exp/path-finding.rkt"
                  manhattan-distance)
         (prefix-in cd:
                    (combine-in "../../exp/cd-narrow.rkt"
                                "../../exp/cd-broad.rkt")))

;; https://s3.amazonaws.com/data.tumblr.com/tumblr_m01c27aYQ91qbw2q1o1_1280.jpg?AWSAccessKeyId=AKIAJ6IHWSU3BX3X7X3Q&Expires=1331014634&Signature=MsCEbwNiXX7J5h%2B%2BeCdDjW11mmc%3D

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

(define-sound se:crunch "crunch.wav")
(define-sound se:bgm "bgm.ogg")
;; XXX cut this better
(define-sound se:power-up "power-up.ogg")

(define-runtime-path template-map "template.map")
(match-define
 (list hall wall power-up fruit ghost-entry player-entry conn)
 (bytes->list #"0123456"))
(define (path->quadrant p)
  (define lines (file->bytes-lines p))
  (values (* 2 (bytes-length (first lines)))
          (* 2 (length lines))
          (apply bytes-append lines)))
(define-values (width height quad:template)
  (path->quadrant template-map))

;; XXX incorporate ghost position (so that I don't put a ghost in a wall if I'm switching the state of a quad)
(define (generate-quad)
  (define new-quad (bytes-copy quad:template))
  (define cells
    (for*/list ([r (in-range h-height)]
                [c (in-range h-width)])
      (cons r c)))
  (define (cell-neighbors r*c)
    (match-define (cons r c) r*c)
    (list (cons r (sub1 c))
          (cons (add1 r) c)
          (cons r (add1 c))
          (cons (sub1 r) c)))
  (define (inside-maze? r*c)
    (match-define (cons r c) r*c)
    (and (<= 0 r (sub1 h-height))
         (<= 0 c (sub1 h-width))))
  (define (quad-cell-ref r*c)
    (match-define (cons r c) r*c)
    (if (inside-maze? r*c)
      (quad-ref new-quad r c)
      hall))
  (define (original-wall? c)
    (= wall (quad-ref quad:template (car c) (cdr c))))
  (define (not-wall? c)
    (not (= wall (quad-cell-ref c))))
  (define (non-wall-neighbors cn)
    (filter not-wall?
            (cell-neighbors cn)))

  ;; For every cell, consider turning it into a wall if
  ;; all its non-wall neighbors have more than 2 exits
  ;; i.e. doing so does not create a dead-end.
  (for ([r*c (in-list (shuffle cells))])
    (when (= hall (quad-cell-ref r*c))
      (define cns (non-wall-neighbors r*c))
      (when
          (for/and ([cn (in-list cns)])
            (define how-many-halls
              (length (non-wall-neighbors cn)))
            (how-many-halls . > . 2))
        (match-define (cons r c) r*c)
        (bytes-set! new-quad (r*c->i r c) wall))))

  (define seen? (make-hash))
  (define (visit r*c)
    (match-define (cons r c) r*c)
    (unless (hash-has-key? seen? r*c)
      (hash-set! seen? r*c #t)
      (for-each visit
                (filter inside-maze?
                        (non-wall-neighbors r*c)))))
  (visit player-entry-cell)

  (let ()
    (local-require data/heap
                   unstable/function)
    (define (dig-until-seen c0)
      (define h
        (make-heap (λ (c1 c2)
                     (<= (manhattan-distance c1 c0)
                         (manhattan-distance c2 c0)))))
      (define came-from (make-hash))
      (define (add-neighbors! c)
        (define ns
          (filter (conjoin (λ (c) (not (hash-has-key? came-from c)))
                           inside-maze?
                           (negate original-wall?))
                  (cell-neighbors c)))
        (for-each (λ (nc) (hash-set! came-from nc c)) ns)
        (heap-add-all! h ns))
      (hash-set! came-from c0 #f)
      (heap-add! h c0)
      (define last-c
        (let/ec done
          (let loop ()
            (define c (heap-min h))
            (heap-remove-min! h)
            (if (hash-has-key? seen? c)
              (done c)
              (begin
                (add-neighbors! c)
                (loop))))))
      (let loop ([c last-c])
        (define next-c (hash-ref came-from c #f))
        (when (= wall (quad-cell-ref c))
          (match-define (cons nr nc) c)
          (bytes-set! new-quad (r*c->i nr nc) hall))
        (when next-c
          (loop next-c))))

    (for ([r*c (in-list (shuffle cells))])
      (unless (= wall (quad-cell-ref r*c))
        (unless (hash-has-key? seen? r*c)
          (dig-until-seen r*c)
          (visit r*c)))))

  ;; Turn all the "conn" blocks into "hall".
  ;; We had them different in the template to protect them from
  ;; being walled.
  (for ([r*c (in-list cells)])
    (when (= conn (quad-cell-ref r*c))
      (bytes-set! new-quad (r*c->i (car r*c) (cdr r*c)) hall)))
  new-quad)

(define center-pos
  (psn (/ width 2.) (/ height 2.)))

;; Much enlightenment from http://gameinternals.com/post/2072558330/understanding-pac-man-ghost-behavior

;; XXX time limit?
;; XXX ghosts/pacman are the wrong size
;; XXX turn the layout into a nice graphic with rounded walls, wide tunnels, etc
;; XXX increase speed with time/score
;; XXX stationary ghosts that awaken
;; XXX ghost train
;; XXX bomb

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
  (ghost-sprite n dir-n frame-n))

(define (ghost-sprite which-ghost which-set frame-n)
  (gl:translate
   -.5 -.5
   (gl:texture/px
    sprites-t
    1 1
    (+ 3 (* 17 (+ (rate 2 10 frame-n) (* 2 which-set))))
    (+ 125 (* 18 which-ghost))
    14 12)))

(define (scared-ghost-animation frame-n warning?)
  (ghost-sprite
   4
   (if (and warning? (even? frame-n)) 1 0)
   frame-n))

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
(define power-up-img
  (gl:scale (* pellet-r 2) (* 2 pellet-r)
            (gl:circle)))
(define fruit-img
  (let ()
    (define s (* 3 pellet-r))
    (gl:translate
     (- (* s .5)) (- (* s .5))
     (gl:rectangle s s))))

(define h-width
  (/ width 2))
(define h-height
  (/ height 2))
(define (r*c->i vr vc)
  (+ (* vr h-width) vc))
(define (quad-ref quad vr vc)
  (bytes-ref quad (r*c->i vr vc)))

(define (locate-cell quad value)
  (for*/or ([r (in-range h-height)]
            [c (in-range h-width)]
            #:when (= (quad-ref quad r c) value))
    (cons r c)))
(define power-up-cell (locate-cell quad:template power-up))
(define fruit-cell (locate-cell quad:template fruit))
(define ghost-entry-cell (locate-cell quad:template ghost-entry))
(define player-entry-cell (locate-cell quad:template player-entry))

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
      ['sw (values c r)]
      ['nw (values c (- height r 1))]
      ['se (values (- width c 1) r)]
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
    [(= 0 a) 'right]
    [(= (/ pi 2) a) 'up]
    [(= pi a) 'left]
    [else 'down]))

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
(define (static-display st)
  (gl:seqn (static-map-display st)
           (static-objs-display st)))
(define (place-power-up q)
  (match-define (quad-objs pellet-count r*c->obj) q)
  (match-define (cons r c) power-up-cell)
  (quad-objs (add1 pellet-count)
             (fmatrix-set r*c->obj r c 'power-up)))
(define (place-fruit q)
  (match-define (quad-objs pellet-count r*c->obj) q)
  (match-define (cons r c) fruit-cell)
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
  (gl:color
   0. 0. 1. 0.
   (gl:for*/gl
    ([x (in-range width)]
     [y (in-range height)])
    (define-values (q r c) (xy->quad*r*c x y))
    (gl:translate
     (+ x .5) (+ y .5)
     (if (equal? wall (quad-ref (hash-ref qs q) r c))
       (gl:translate -.5 -.5 (gl:rectangle 1. 1.))
       gl:blank)))))

(struct quad-objs (pellet-count r*c->obj))
(define (populate-quad q [old-q #f])
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
  (define im2 (place-power-up im1))
  (define im3
    (if (and old-q (fmatrix-ref (quad-objs-r*c->obj old-q)
                                (car fruit-cell) (cdr fruit-cell)
                                #f))
      (place-fruit im2)
      im2))
  im3)

(define (make-static)
  (define quads
    (hasheq 'nw (generate-quad)
            'ne (generate-quad)
            'sw (generate-quad)
            'se (generate-quad)))
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
  (gl:color/%
   (make-object color% 255 161 69)
   (gl:for*/gl
    ([x (in-range width)]
     [y (in-range height)])
    (define-values (q r c) (xy->quad*r*c x y))
    (match-define (quad-objs _ fm) (hash-ref os q))
    (gl:translate
     (+ x .5) (+ y .5)
     (match (fmatrix-ref fm r c #f)
       ['pellet pellet-img]
       ['power-up power-up-img]
       ['fruit fruit-img]
       [#f
        gl:blank])))))

(define opposite-quad
  (match-lambda
   ['nw 'se]
   ['ne 'sw]
   ['se 'nw]
   ['sw 'ne]))

(define (static-chomp st x y)
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
            (hash-update im oq place-fruit))
          im))
      (if (eq? obj 'fruit)
        (let ()
          (define oq (opposite-quad q))
          (define nq
            (generate-quad))
          (define quads-n
            (hash-set quads oq nq))
          (define old-objs
            (hash-ref quad->objs-p oq))
          (define quad->objs-n
            (hash-set quad->objs-p oq
                      (populate-quad nq old-objs)))
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

(struct game-st (frame
                 score lives next-extend
                 power-left
                 static-objs dyn-objs))
(struct player (pos dir next-dir))
(struct ghost
        (n pos target dir last-cell
           scatter? frames-to-switch dot-timer))
(define (make-ghost n init-timer)
  (define outside-jail
    (quad*cell->psn
     (match n
       [0 'nw]
       [1 'ne]
       [2 'sw]
       [3 'se])
     ghost-entry-cell))
  (define outside-jail-right-of
    (pos->cell
     (+ outside-jail 1.)))
  (ghost n outside-jail (scatter-tile)
         'left outside-jail-right-of #t
         TIME-TO-SCATTER init-timer))

(define (scatter-tile)
  (psn (* (random) width)
       (* (random) height)))

(define (update-objs objs f)
  (for/hasheq ([(k v) (in-hash objs)])
              (values k (f v))))

;; XXX score multiplier
(define pellet-pts 10)
(define power-up-pts 50)
(define fruit-pts 100)
(define ghost-pts 200)
(define extend-pts 3000)

(define ghost-return 40)

;; XXX Add a slow start-up clock for beginning of game and
;;     after death
(define init-objs
  (hasheq
   'chaser (make-ghost 0 0)
   'ambusher (make-ghost 1 40)
   'fickle (make-ghost 2 80)
   'stupid (make-ghost 3 160)
   'player (player (quad*cell->psn 'sw player-entry-cell)
                   (* .5 pi) (* .5 pi))))

(big-bang
 (game-st 0 0 30 extend-pts 0
          (make-static) init-objs)
 #:sound-scale
 (/ width 2.)
 #:tick
 (λ (w cs)
   (define c (last cs))
   (match-define
    (game-st frame
             score lives next-extend
             power-left
             st dyn-objs)
    w)
   (define power-left-p (max 0 (sub1 power-left)))
   (define frame-n (add1 frame))
   (define frightened? (not (zero? power-left-p)))
   (define dyn-objs:post-movement
     (update-objs
      dyn-objs
      (match-lambda
       [(and v
             (struct* ghost
                      ([n n]
                       [pos p]
                       [last-cell lc]
                       [target l-target]
                       [scatter? scatter?]
                       [dot-timer 0]
                       [frames-to-switch switch-n])))
        (define speed
          (if frightened?
            (/ INIT-SPEED 2.)
            INIT-SPEED))
        (define c (pos->cell p))
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
        (define nps*
          (cell-neighbors/no-reverse st c lc))
        (define nps
          ;; This makes them allowed, but not obligated,
          ;; to switch directions.
          (if (not same-mode?)
            (list* lc nps*)
            nps*))
        (define pp
          (player-pos (hash-ref dyn-objs 'player)))
        (define target
          (cond
            [(or frightened?
                 (and l-target same-mode?
                      (if (not n-scatter?) (= (length nps) 1) #t)))
             l-target]
            [n-scatter?
             (scatter-tile)]
            [else
             (match n
               [0
                pp]
               [1
                (+ pp
                   (make-polar
                    4
                    (player-dir
                     (hash-ref dyn-objs 'player))))]
               [2
                (define v
                  (- pp
                     (ghost-pos
                      (hash-ref dyn-objs 'ambusher))))
                (+ pp (make-polar (* 2 (magnitude v))
                                  (angle v)))]
               [3
                (if (<= (pos->pos-distance pp p) 8)
                  l-target
                  pp)])]))
        (define next-cell
          (argmin* (curry pos->cell-distance target)
                   nps))
        (define mv
          (movement-vector speed p next-cell))
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
        (define speed INIT-SPEED)
        (define stick (controller-dpad c))
        (define next-dir-n
          ;; If the stick is stable,
          ;; then don't change the direction
          (if (= stick 0.+0.i)
            next-dir
            (angle (cardinate stick))))
        ;; The coorridors used to feel too "tight"
        ;; and easy to get stuck on an edge, but I
        ;; think this got fixed
        (define np (try-direction st speed p next-dir-n))
        ;; Don't change the direction if we couldn't
        ;; move in it
        (define actual-dir
          (if (= np p)
            dir
            next-dir-n))
        (define nnp
          (if (= np p)
            (try-direction st speed p dir)
            np))
        (player nnp actual-dir next-dir-n)]
       [v v])))
   (define-values
     (dp1 st-n event)
     (let ()
       (match-define
        (cons x y)
        (pos->cell (player-pos
                    (hash-ref dyn-objs:post-movement 'player))))
       (define-values
         (st-n chomped?)
         (static-chomp st x y))
       (match chomped?
         ['pellet
          (values pellet-pts st-n 'pellet)]
         ['power-up
          (values power-up-pts st-n 'power-up)]
         ['fruit
          (values fruit-pts st-n 'fruit)]
         [#f
          (values 0 st-n #f)])))
   (define power-left-n
     (if (eq? event 'power-up)
       (+ power-left-p TIME-TO-POWER)
       power-left-p))
   (define dyn-objs:post-chomp
     (if (eq? event 'pellet)
       (update-objs
        dyn-objs:post-movement
        (match-lambda
         ;; XXX Add a sound effect when the activate?
         [(and v (struct* ghost ([dot-timer dt])))
          (struct-copy ghost v
                       [dot-timer (max 0 (sub1 dt))])]
         [v v]))
       dyn-objs:post-movement))
   (define-values
     (lives-p dp2 dyn-objs:post-death)
     (let ()
       (define p-cell
         (pos->cell
          (player-pos
           (hash-ref dyn-objs:post-chomp 'player))))
       (if (power-left-n . > . 0)
         (let ()
           (define-values
             (dp2 dyn-objs:post-killing)
             (for/fold
                 ([dp2 0]
                  [do (hasheq)])
                 ([(k v) (in-hash dyn-objs:post-chomp)])
               (match v
                 [(struct* ghost
                           ([n n]
                            [dot-timer 0]
                            [pos (app pos->cell g-cell)]))
                  (if (equal? p-cell g-cell)
                    (values
                     (+ dp2 ghost-pts)
                     (hash-set do k
                               (make-ghost n ghost-return)))
                    (values
                     dp2
                     (hash-set do k v)))]
                 [_
                  (values
                   dp2
                   (hash-set do k v))])))
           (values lives dp2 dyn-objs:post-killing))
         (if (for/or ([v (in-hash-values dyn-objs:post-chomp)]
                      #:when (ghost? v)
                      #:when (zero? (ghost-dot-timer v)))
               (equal?
                p-cell
                (pos->cell (ghost-pos v))))
           ;; XXX Add sound effect
           (values (sub1 lives) 0 init-objs)
           (values lives 0 dyn-objs:post-chomp)))))
   (define score-n (+ score dp1 dp2))
   (define next-extend-p (- next-extend dp1 dp2))
   (define-values
     (lives-n next-extend-n)
     (if (next-extend-p . <= . 0)
       ;; XXX Add sound effect
       (values (add1 lives-p) extend-pts)
       (values lives-p next-extend-p)))
   (define dyn-objs:final
     dyn-objs:post-death)
   (values
    (game-st frame-n score-n lives-n
             next-extend-n power-left-n
             st-n dyn-objs:final)
    (gl:focus
     (+ width 2) (+ height 3) (+ width 2) (+ height 3) 0 0
     (gl:translate
      1. 1.
      (gl:background
       0. 0. 0. 0.
       (gl:color
        1. 1. 1. 1.
        (gl:center-texture-at
         (psn (/ width 2.) (+ height 1.5))
         title)
        (gl:translate
         (* width 3/4) (+ height 0.5)
         (gl:texture
          (gl:string->texture
           #:size 50
           (format "FPS: ~a"
                   (real->decimal-string
                    (current-rate) 1)))))
        (gl:translate
         0. (+ height 0.5)
         (gl:texture
          (gl:string->texture
           #:size 50
           (format "Score: ~a    Lives: ~a"
                   score-n lives-n)))))
       (gl:seqn
        (static-display st)
        (gl:for/gl
         ([v (in-hash-values dyn-objs:final)])
         (match v
           [(struct* ghost
                     ([n n]
                      [pos p]
                      [target tp]
                      [dir dir]
                      [dot-timer dt]))
            (cond
              [(or (zero? dt)
                   (and (dt . <= . 10) (even? frame)))
               (gl:seqn
                (gl:translate
                 (psn-x p) (psn-y p)
                 (if (zero? power-left-n)
                   (ghost-animation n frame-n dir)
                   (scared-ghost-animation
                    frame-n
                    (power-left-n . <= . TIME-TO-POWER-WARNING))))
                (gl:translate
                 (- (psn-x tp) .5) (- (psn-y tp) .5)
                 (gl:color/%
                  (match n
                    [0 (make-object color% 169 16 0)]
                    [1 (make-object color% 215 182 247)]
                    [2 (make-object color% 60 189 255)]
                    [3 (make-object color% 230 93 16)])
                  (gl:rectangle 1. 1. 'outline))))]
              [else
               gl:blank])]
           [(player p dir _)
            (gl:translate
             (psn-x p) (psn-y p)
             (gl:rotate
              (rad->deg dir)
              (player-animation frame-n)))]))))))
    (append
     (if (eq? event 'pellet)
       (list (sound-at se:crunch center-pos #:gain 0.8))
       empty)
     (if (zero? frame)
       (list (background (λ (w) se:bgm)
                         #:gain 0.5
                         #:pause-f
                         (λ (w)
                           (not (zero? (game-st-power-left w)))))
             (background (λ (w) se:power-up)
                         #:gain 1.0
                         #:pause-f
                         (λ (w)
                           (zero? (game-st-power-left w)))))
       empty))))
 #:listener
 (λ (w) center-pos)
 #:done?
 (λ (w)
   (zero? (game-st-lives w))))
