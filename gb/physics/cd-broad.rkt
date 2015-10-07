#lang racket/base
; Based on http://www.metanetsoftware.com/technique/tutorialB.html
(require racket/generator
         racket/sequence
         racket/contract
         racket/function
         racket/match
         tests/eli-tester
         gb/lib/math
         gb/data/psn
         gb/physics/cd-narrow)

; Right now, the space is a grid made of cells such that there are
; (w / cell-w) x (h / cell-h) cells. An object, abstracted to an
; aabb is stored in all the cells that it resides in.

; A good cell size, is therefore, the maximum size of objects that reside
; in the space.

; It would be interesting to see if other data-structures, such as discussed on 
; http://en.wikipedia.org/wiki/Spatial_index would be better.
(require racket/set
         gb/data/fmatrix)
(struct space (rows cols cw ch fm))
(define (make-space w h cell-w cell-h)
  (define cols (inexact->exact (ceiling (/ w cell-w))))
  (define rows (inexact->exact (ceiling (/ h cell-h))))
  (space
   rows cols
   cell-w cell-h
   (fmatrix (add1 rows) (add1 cols))))
(define (space-row g y)
  (clamp 0
         (inexact->exact (floor (/ y (space-ch g))))
         (space-rows g)))
(define (space-col g x)
  (clamp 0
         (inexact->exact (floor (/ x (space-cw g))))
         (space-cols g)))
  
(define (space-update g r c f a)
  (struct-copy space g
               [fm (fmatrix-update (space-fm g) r c f a)]))
(define (space-ref g r c)
  (fmatrix-ref (space-fm g) r c mt-set))

(define (in-space g s)
  (define r (shape->aabb s))
  (define ul (aabb-ul r))
  (define ur (aabb-ur r))
  (define ll (aabb-ll r))
  (in-generator
   (for* ([row (in-range (space-row g (psn-y ll))
                         (add1 (space-row g (psn-y ul))))]
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
    (match-define (cons row col) r*c)
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
    (match-define (cons row col) r*c) 
    (space-update g row col
                  (curry set-filter (compose (curry equal? obj) iobj-obj))
                  mt-set)))

; A collision means that the shape (for which this code assigns no semantics) has 
; collided with the object with a certain separating vector.
(struct collision (v o2) #:transparent)

(define ((make-space-collisions colliding? collision) g shape)
  (define seen? (make-hasheq))
  (in-generator
   (for ([r*c (in-space g shape)])
     (match-define (cons row col) r*c)
     (for ([io (in-set (space-ref g row col))]
           #:when (not (hash-has-key? seen? io)))
       (hash-set! seen? io #t)
       (match-define (iobj o-shape obj) io)
       (define v (colliding? shape o-shape))
       (when v
         (yield (collision v obj)))))))

(define space-collisions
  (make-space-collisions 
   (λ (s1 s2) (shape-vs-shape s1 s2 #:depth? #t))
   collision))

; These functions just return the object... no vector.
(define space-collisions?
  (make-space-collisions 
   (λ (s1 s2) (shape-vs-shape s1 s2))
   (λ (v o) o)))

; Based on http://playtechs.blogspot.com/2007/03/raytracing-on-grid.html
(define (in-space/ray g start end)
  (match-define (psn* sx sy) start)
  (define x0 (space-col g sx))
  (define y0 (space-row g sy))
  (match-define (psn* ex ey) end)
  (define x1 (space-col g ex))
  (define y1 (space-row g ey))
  (define dx (abs (- x1 x0)))
  (define dy (abs (- y1 y0)))
  (define n (+ 1 dx dy))
  (define x-inc
    (if (x1 . > . x0)
        1
        -1))
  (define y-inc
    (if (y1 . > . y0)
        1
        -1))
  
  (in-generator
   (for/fold ([x x0] [y y0] [error (- dx dy)])
     ([i (in-range n)])
     ; XXX Remove extra cons
     (yield (cons x y))
     
     (if (error . > . 0)
         (values (+ x x-inc)
                 y
                 (- error (* 2 dy)))
         (values x
                 (+ y y-inc)
                 (+ error (* 2 dx)))))))

(module+ test
  (test
   (sequence->list (in-space/ray (make-space 4 5 1 1) (psn .5 .5) (psn 2.5 3.5)))
   =>
   '((0 . 0) (0 . 1) (1 . 1) (1 . 2) (2 . 2) (2 . 3))
   
   (sequence->list (in-space/ray (make-space 10 10 1 1) (psn 0. 0.) (psn 10. 10.)))
   =>
   '((0 . 0) (0 . 1) (1 . 1) (1 . 2) (2 . 2) (2 . 3) (3 . 3) (3 . 4) (4 . 4) (4 . 5)
             (5 . 5) (5 . 6) (6 . 6) (6 . 7) (7 . 7) (7 . 8) (8 . 8) (8 . 9) (9 . 9)
             (9 . 10) (10 . 10))))

(define (space-ray g start end)
  (define seen? (make-hasheq))
  (in-generator
   (for ([r*c (in-space/ray g start end)])
     (match-define (cons row col) r*c)
     (for ([io (in-set (space-ref g row col))]
           #:when (not (hash-has-key? seen? io)))
       (hash-set! seen? io #t)
       (match-define (iobj o-shape obj) io)
       (when (shape-vs-line o-shape start end)
         (yield obj))))))

(provide/contract
 [space? contract?]
 [struct collision
         ([v psn?]
          [o2 any/c])]
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

(module+ test
  (define s
    (space-insert (make-space 10 10 1 1)
                  (circle (psn 1. 1.) .5) 
                  'circle))
  (test
   (space-row s 10) => 10
   (space-row s 0) => 0
   (space-row s 5) => 5
   (space-col s 10) => 10
   (space-col s 0) => 0
   (space-col s 5) => 5
   
   (sequence->list
    (space-collisions?
     (space-insert (make-space 10 10 1 1)
                   (aabb (psn 1.5 1.5) .5 .5)
                   'box)
     (aabb (psn 1.5 1.25) .5 .5)))
   =>
   (list 'box)
   
   (sequence->list
    (space-collisions?
     (space-insert (make-space 10 10 1 1)
                   (aabb (psn 3.5 3.5) .5 .5)
                   'box)
     (aabb (psn 1.5 1.25) .5 .5)))
   =>
   (list)
   
   (sequence->list
    (space-collisions
     (space-insert (make-space 10 10 1 1)
                   (aabb (psn 1.5 1.5) .5 .5)
                   'box)
     (aabb (psn 1.5 1.25) .5 .5)))
   =>
   (list (collision -0.0+0.75i 'box))
   
   (sequence->list
    (space-collisions?
     (space-insert (make-space 10 10 1 1)
                   (circle (psn 1. 1.) 1.)
                   'circle)
     (circle (psn 1. 1.) 1.)))
   =>
   (list 'circle)
   
   (sequence->list
    (space-collisions
     (space-insert (make-space 10 10 1 1)
                   (circle (psn 1. 1.) 1.)
                   'circle)
     (circle (psn 1. 1.) 1.)))
   =>
   (list (collision -1.0+1.2246467991473532e-16i 'circle))
   
   (sequence->list
    (space-collisions?
     (space-insert (make-space 10 10 1 1)
                   (circle (psn 1. 1.) 1.)
                   'circle)
     (aabb (psn 2. 2.) .5 .5)))
   =>
   (list 'circle)
   
   (sequence->list
    (space-collisions?
     (space-insert (make-space 10 10 1 1)
                   (aabb (psn 2. 2.) .5 .5)
                   'box)
     (circle (psn 1. 1.) 1.)))
   =>
   (list 'box)
   
   (sequence->list
    (space-ray 
     (space-insert (make-space 10 10 1 1)
                   (circle (psn 1. 1.) .5) 
                   'circle)
     (psn 0. 0.) (psn 1. 1.)))
   =>
   (list 'circle)
   
   (sequence->list
    (space-ray
     (space-insert (make-space 10 10 1 1)
                   (aabb (psn .5 .5) .5 .5)
                   'box)
     (psn 0. 0.) (psn 1. 1.)))
   =>
   (list 'box)))
