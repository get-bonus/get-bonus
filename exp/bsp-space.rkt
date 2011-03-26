#lang racket/base
(require racket/function
         racket/match
         tests/eli-tester)

(struct posn (x y) #:transparent)
(struct segment (s e) #:transparent)
(struct space (ul w h) #:transparent)
(define (space-ur s)
  (match-define (space (and ul (posn x y)) w h) s)
  (struct-copy posn ul
               [x (+ x w)]))
(define (space-ll s)
  (match-define (space (and ul (posn x y)) w h) s)
  (struct-copy posn ul
               [y (+ y h)]))
(define (space-lr s)
  (match-define (space (and ul (posn x y)) w h) s)
  (struct-copy posn ul
               [x (+ x w)]
               [y (+ y h)]))

(test
 (space-ul (space (posn 1 2) 3 4)) => (posn 1 2)
 (space-ur (space (posn 1 2) 3 4)) => (posn 4 2)
 (space-ll (space (posn 1 2) 3 4)) => (posn 1 6)
 (space-lr (space (posn 1 2) 3 4)) => (posn 4 6))

(define (segment-transpose s)
  (match-define (segment (posn x1 y1) (posn x2 y2)) s)
  (segment (posn y1 x1) (posn y2 x2)))
(define (space-transpose s)
  (match-define (space (posn x y) w h) s)
  (space (posn y x) h w))

(test
 (segment-transpose (segment (posn 1 2) (posn 3 4)))
 => (segment (posn 2 1) (posn 4 3))
 
 (space-transpose (space (posn 1 2) 3 4))
 => (space (posn 2 1) 4 3)
 (let ([r (space (posn (random 10) (random 10))
                 (random 10) (random 10))])
   (test
    (space-transpose (space-transpose r)) => r)))

(struct dungeon (w h))
(struct dun:split dungeon (connection left right))
(struct dun:split:vert dun:split (x))
(struct dun:split:horiz dun:split (y))
(struct dun:room dungeon (space))

(define max-room-width (make-parameter 25))
(define (min-room-width) (* (max-room-width) (min-room-percentage)))
(define max-room-height (make-parameter 25))
(define (min-room-height) (* (max-room-height) (min-room-percentage)))
(define min-room-percentage (make-parameter 0.5))

(define (random-in-range min max)
  (define size (inexact->exact (floor (- max min))))
  (if (size . <= . 0)
      min
      (+ min (random size))))

(define (posn-left? p1 p2)
  (<= (posn-x p1) (posn-x p2)))
(define posn-right?
  (negate posn-left?))
(define (posn-above? p1 p2)
  (<= (posn-y p1) (posn-y p2)))
(define posn-below?
  (negate posn-above?))

(test
 (posn-left? (posn 0 0) (posn 1 0))
 (posn-right? (posn 1 0) (posn 0 0))
 (posn-above? (posn 0 0) (posn 0 1))
 (posn-below? (posn 0 1) (posn 0 0)))

(define (posn+ p1 p2)
  (posn (+ (posn-x p1) (posn-x p2))
        (+ (posn-y p1) (posn-y p2))))

(test
 (posn+ (posn 1 1) (posn 2 2)) => (posn 3 3))

(define (space-adjust p s)
  (struct-copy space s
               [ul (posn+ p (space-ul s))]))

(test
 (space-adjust (posn 1 1) (space (posn 0 0) 5 5))
 => (space (posn 1 1) 5 5))

(define (min-of <= x y)
  (if (<= x y)
      x
      y))

(test
 (min-of posn-left?
         (posn 0 0)
         (posn 1 1))
 =>
 (posn 0 0))

(define (bsp-min <= t)
  (match t
    [(dun:room _ _ s)
     s]
    [(dun:split:vert _ _ _ l r x)
     (min-of <=
             (bsp-min <= l)
             (space-adjust (posn x 0) (bsp-min <= r)))]
    [(dun:split:horiz _ _ _ l r y)
     (min-of <=
             (bsp-min <= l)
             (space-adjust (posn 0 y) (bsp-min <= r)))]))

(define (space-lift <= corner)
  (λ (x y)
    (<= (corner x) (corner y))))

(test
 (bsp-min (space-lift posn-left? space-ul)
          (dun:split:vert 10 10 #f 
                          (dun:room 5 10 (space (posn 0 0) 5 10))
                          (dun:room 5 10 (space (posn 0 0) 5 10))
                          5))
 =>
 (space (posn 0 0) 5 10)
 
 (bsp-min (space-lift posn-right? space-ur)
          (dun:split:vert 10 10 #f 
                          (dun:room 5 10 (space (posn 0 0) 5 10))
                          (dun:room 5 10 (space (posn 0 0) 5 10))
                          5))
 =>
 (space (posn 5 0) 5 10)
 
 (bsp-min (space-lift posn-right? space-ur)
          (dun:split:horiz 20 20 #f
                           (dun:split:vert 10 10 #f 
                                           (dun:room 5 10 (space (posn 0 0) 5 10))
                                           (dun:room 5 10 (space (posn 0 0) 5 10))
                                           5)
                           (dun:split:vert 10 10 #f 
                                           (dun:room 5 10 (space (posn 0 0) 5 10))
                                           (dun:room 5 10 (space (posn 0 0) 5 10))
                                           5)
                           10))
 =>
 (space (posn 5 10) 5 10)
 
 (bsp-min (space-lift posn-above? space-ul)
          (dun:split:horiz 10 10 #f 
                           (dun:room 10 5 (space (posn 0 0) 10 5))
                           (dun:room 10 5 (space (posn 0 0) 10 5))
                           5))
 =>
 (space (posn 0 0) 10 5)
 
 (bsp-min (space-lift posn-below? space-ll)
          (dun:split:horiz 10 10 #f 
                           (dun:room 10 5 (space (posn 0 0) 10 5))
                           (dun:room 10 5 (space (posn 0 0) 10 5))
                           5))
 =>
 (space (posn 0 5) 10 5)
 
 (bsp-min (space-lift posn-below? space-ll)
          (dun:split:vert 20 20 #f
                          (dun:split:horiz 10 10 #f 
                                           (dun:room 10 5 (space (posn 0 0) 10 5))
                                           (dun:room 10 5 (space (posn 0 0) 10 5))
                                           5)
                          
                          (dun:split:horiz 10 10 #f 
                                           (dun:room 10 5 (space (posn 0 0) 10 5))
                                           (dun:room 10 5 (space (posn 0 0) 10 5))
                                           5)
                          10))
 =>
 (space (posn 10 5) 10 5))

(define (horiz-connect-spaces dis sl sr)
  (segment
   ; Pick a point on the bottom wall of sl
   (posn+
    (space-ll sl)
    (posn (random-in-range 0 (space-w sl)) 0))
   
   ; Pick a point on the top wall of sr
   (posn+ (space-ul sr)
          (posn (random-in-range 0 (space-w sr))
                dis))))

(define (horiz-connect dis l r)
  (define sl (bsp-min (space-lift posn-below? space-ll) l))
  (define sr (bsp-min (space-lift posn-above? space-ul) r))
  (horiz-connect-spaces dis sl sr))

(define (vert-connect dis l r)  
  (define sl (bsp-min (space-lift posn-right? space-ur) l))
  (define sr (bsp-min (space-lift posn-left? space-ul) r))
  (segment-transpose
   (horiz-connect-spaces dis (space-transpose sl) (space-transpose sr))))

(define (random-dungeon w h)
  (define (vertical)
    (define split-w
      (random-in-range (min-room-width)
                       (- w (min-room-width))))
    (define left
      (random-dungeon split-w h))
    (define right
      (random-dungeon (- w split-w) h))
    (dun:split:vert w h (vert-connect split-w left right) left right split-w))
  (define (horizontal)
    (define split-h
      (random-in-range (min-room-height)
                       (- h (min-room-height))))
    (define left
      (random-dungeon w split-h))
    (define right
      (random-dungeon w (- h split-h)))
    (dun:split:horiz w h (horiz-connect split-h left right) left right split-h))
  (define (room)
    (define rw (random-in-range (* w (min-room-percentage)) w))
    (define rh (random-in-range (* h (min-room-percentage)) h))
    (dun:room 
     w h
     (space
      (posn (random-in-range 0 (- w rw))
            (random-in-range 0 (- h rh)))
      rw rh)))
  (define can-vertical?
    (not (w . <= . (max-room-width))))
  (define can-horizontal?
    (not (h . <= . (max-room-height))))
  (cond
    [(and can-horizontal? can-vertical?)
     (case (random 2)
       [(0) (horizontal)]
       [(1) (vertical)])]
    [can-horizontal? (horizontal)]
    [can-vertical? (vertical)]
    [else (room)]))

(require 2htdp/image)

(define (render-space s c i)
  (match-define (space (posn x y) cw ch) s)
  (place-image/align
   (rectangle cw ch "solid" c)
   x y
   "left" "top"
   i))
(define (render-segment s c i)
  (match s
    [(segment (posn x1 y1) (posn x2 y2))
     (scene+line i x1 y1 x2 y2 c)]
    [#f
     i]))

(define render
  (match-lambda
    [(dun:split:vert _ h s left right y)
     (render-segment
      s "black"
      (beside/align 
       "top"
       (render left)
       (render right)))]
    [(dun:split:horiz w _ s left right x)
     (render-segment
      s "black"
      (above/align
       "left"
       (render left)
       (render right)))]
    [(dun:room w h s)
     (render-space
      s "blue"
      (rectangle w h "solid" "white"))]))

; XXX Prefer straight cooridors
; XXX Find the closest rooms in two sections, rather than rightmost/leftmost
(scale 2 
       (parameterize ([max-room-width 50]
                      [max-room-height 50])
         (render (random-dungeon 200 200))))
