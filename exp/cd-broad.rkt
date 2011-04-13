#lang racket/base
; Based on http://www.metanetsoftware.com/technique/tutorialB.html
(require racket/generator
         racket/sequence
         racket/contract
         racket/function
         racket/match
         unstable/contract
         tests/eli-tester
         "psn.rkt"
         "cd-narrow.rkt")

; Right now, the space is a grid made of cells such that there are
; (w / cell-w) x (h / cell-h) cells. An object, abstracted to an
; aabb is stored in all the cells that it resides in.

; A good cell size, is therefore, the maximum size of objects that reside
; in the space.

; It would be interesting to see if other data-structures, such as discussed on 
; http://en.wikipedia.org/wiki/Spatial_index would be better.
(require racket/set
         "fmatrix.rkt")
(struct space (cw ch fm))
(define (make-space w h cell-w cell-h)
  (define cols (ceiling (/ w cell-w)))
  (define rows (ceiling (/ h cell-h)))
  (space
   cell-w cell-h
   (fmatrix rows cols)))
(define (space-row g y)
  (inexact->exact (floor (/ y (space-ch g)))))
(define (space-col g x)
  (inexact->exact (floor (/ x (space-cw g)))))
  
(define (space-update g r c f a)
  (fmatrix-update (space-fm g) r c f a))
(define (space-ref g r c)
  (fmatrix-ref (space-fm g) r c mt-set))

(define (in-space g s)
  (define r (shape->aabb s))
  (define ul (aabb-ul r))
  (define ur (aabb-ur r))
  (define ll (aabb-ll r))
  (in-generator
   (for* ([row (in-range (space-row g (psn-y ul))
                         (add1 (space-row g (psn-y ll))))]
          [col (in-range (space-col g (psn-x ul))
                         (add1 (space-col g (psn-x ur))))])
     ; XXX Remove cons
     (yield (cons row col)))))

(struct iobj (shape obj))

(define mt-set (set))
(define (space-insert g shape obj)
  (define i (iobj shape obj))
  (for/fold ([g g])
    ([r*c (in-space g shape)])
    (match-define (cons r c) r*c)
    (space-update g row col
                    (curryr set-add i)
                    mt-set)))
(define (set-filter f s)
  (for/set ([e (in-set s)]
            #:when (not (f e)))
           e))
(define (space-remove g shape obj)
  (for/fold ([g g])
    ([r*c (in-space g shape)])
    (match-define (cons r c) r*c)   
    (space-update g row col
                  (curry set-filter (compose (curry equal? obj) iobj-obj))
                  mt-set)))

; A collision means that the shape (for which this code assigns no semantics) has 
; collided with the object with a certain separating vector.
(struct collision (v o2))

(define ((make-space-collisions colliding? collision) g shape)
  (in-generator
   (for* ([(row col) (in-space g shape)]
          [io (in-set (space-ref g row col))])
     (match-define (iobj o-shape obj) io)
     (define v (colliding? shape o-shape))
     (when v
       (yield (collision v obj))))))

(define space-collisions
  (make-space-collisions 
   (λ (s1 s2) (shape-vs-shape s1 s2 #:depth? #t))
   collision))

; These functions just return the object... no vector.
(define space-collisions?
  (make-space-collisions 
   (λ (s1 s2) (shape-vs-shape s1 s2))
   (λ (v o) o)))

(define (in-space/ray g start end)
  (match-define (psn* sx sy) start)
  (define x0 (space-col g sx))
  (define y0 (space-row g sy))
  (match-define (psn* ex ey) end)
  (define x1 (space-col g ex))
  (define y1 (space-row g ey))
  (define dx (abs (- x1 x0)))
  (define dy (abs (- y1 y0)))
  (define x x0)
  (define y y0)
  (define n (+ 1 dx dy))
  (define x-inc
    (if (x1 . > . x0)
        1
        -1))
  (define y-inc
    (if (y1 . > . y0)
        1
        -1))
  (define error
    (- dx dy))
  (set! dx (* 2 dx))
  (set! dy (* 2 dy))
  
  (in-generator
   (for ([i (in-range n)])
     ; XXX Remove extra cons
     (yield (cons x y))
     
     (if (error . > . 0)
         (begin (set! x (+ x x-inc))
                (set! error (- error dy)))
         (begin (set! y (+ y y-inc))
                (set! error (+ error dx)))))))

(let ()
  (test
   (sequence->list (in-space/ray (make-space 10 10 1 1) (psn 0. 0.) (psn 10. 10.)))
   =>
   '((0 . 0) (0 . 1) (1 . 1) (1 . 2) (2 . 2) (2 . 3) (3 . 3) (3 . 4) (4 . 4) (4 . 5)
             (5 . 5) (5 . 6) (6 . 6) (6 . 7) (7 . 7) (7 . 8) (8 . 8) (8 . 9) (9 . 9)
             (9 . 10) (10 . 10))))

(define (space-ray g start end)
  (in-generator
   (for ([r*c (in-space/ray g start end)])
     (match-define (cons r c) r*c)
     (for ([io (in-set (space-ref g row col))])
       (match-define (iobj o-shape obj) io)
       (when (shape-vs-line o-shape start end)
         (yield obj))))))

(provide/contract
 [space? contract?]
 [collision? contract?]
 [rename
  make-space space 
  (-> real? real?
      real? real?
      space?)]
 [space-insert
  (-> space?
      shape/c any/c
      space?)]
 [space-remove
  (-> space?
      shape/c any/c
      space?)]
 [space-collisions?
  (-> space?
      shape/c
      (sequence/c any/c))]
 [space-collisions
  (-> space?
      shape/c
      (sequence/c collision?))]
 [space-ray
  (-> space? 
      psn? psn?
      (sequence/c collision?))])

; XXX Write tests