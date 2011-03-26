#lang racket/base

(struct posn (x y) #:transparent)
(struct space (ul w h))
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

(struct dungeon (w h))
(struct dun:split dungeon (connection left right))
(struct dun:split:vert dun:split ())
(struct dun:split:horiz dun:split ())
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
(define (posn-above? p1 p2)
  (<= (posn-y p1) (posn-y p2)))
           
(define (horiz-connect dis l r)
  (cond
    [(not (and (dun:room? l) (dun:room? r)))
     ; XXX Find the right most of l and the left most of r
     (space (posn 0 0) 0 0)]
    [else
     (define sl (dun:room-space l))
     (define sr (dun:room-space r))
     (space (posn (posn-x (space-ur sl))
                  ; XXX This will not work if there is no overlap.
                  (random-in-range
                   (max (posn-y (space-ur sl))
                        (posn-y (space-ul sr)))
                   (min (posn-y (space-lr sl))
                        (posn-y (space-ll sr)))))
            (+ (- dis
                  (posn-x (space-ur sl)))
               (posn-x (space-ul sr)))
            ; XXX This 1 should be based on the character size
            1)]))

(define (vert-connect dis l r)
  (cond
    [(not (and (dun:room? l) (dun:room? r)))
     ; XXX Find the down most of l and the top most of r
     (space (posn 0 0) 0 0)]
    [else
     (define sl (dun:room-space l))
     (define sr (dun:room-space r))
     (space (posn ; XXX This will not work if there is no overlap.
                  (random-in-range
                   (max (posn-x (space-ll sl))
                        (posn-x (space-ul sr)))
                   (min (posn-x (space-lr sl))
                        (posn-x (space-ur sr))))
                  (posn-y (space-ll sl)))
            ; XXX This 1 should be based on the character size
            1
            (+ (- dis
                  (posn-y (space-ll sl)))
               (posn-y (space-ul sr))))]))

(define (random-dungeon w h)
  (define (horizontal)
    (define split-w
      (random-in-range (min-room-width)
                       (- w (min-room-width))))
    (define left
      (random-dungeon split-w h))
    (define right
      (random-dungeon (- w split-w) h))
    (dun:split:horiz w h (horiz-connect split-w left right) left right))
  (define (vertical)
    (define split-h
      (random-in-range (min-room-height)
                       (- h (min-room-height))))
    (define left
      (random-dungeon w split-h))
    (define right
      (random-dungeon w (- h split-h)))
    (dun:split:vert w h (vert-connect split-h left right) left right))
  (define (room)
    (define rw (random-in-range (* w (min-room-percentage)) w))
    (define rh (random-in-range (* h (min-room-percentage)) h))
    (dun:room 
     w h
     (space
      (posn (random-in-range 0 (- w rw))
            (random-in-range 0 (- h rh)))
      rw rh)))
  (define can-horizontal?
    (not (w . <= . (max-room-width))))
  (define can-vertical?
    (not (h . <= . (max-room-height))))
  (cond
    [(and can-horizontal? can-vertical?)
     (case (random 2)
       [(0) (horizontal)]
       [(1) (vertical)])]
    [can-horizontal? (horizontal)]
    [can-vertical? (vertical)]
    [else (room)]))

(require racket/match
         2htdp/image)

(define (render-space s c i)
  (match-define (space (posn x y) cw ch) s)
  (place-image/align
   (rectangle cw ch "solid" c)
   x y
   "left" "top"
   i))
  
(define render
  (match-lambda
    [(dun:split:horiz _ _ s left right)
     (render-space
      s "black"
      (beside/align 
       "top"
       (render left)
       (render right)))]
    [(dun:split:vert _ _ s left right)
     (render-space
      s "black"
      (above/align
       "left"
       (render left)
       (render right)))]
    [(dun:room w h s)
     (render-space
      s "blue"
      (rectangle w h "solid" "white"))]))

(scale 5 (render (random-dungeon 100 100)))
