#lang racket/base
; 2d Collision Detection - Narrow Phase
; based on http://www.metanetsoftware.com/technique/tutorialA.html
(require (for-syntax racket/base)
         racket/math
         racket/match
         racket/contract
         "psn.rkt"
         tests/eli-tester)

; XXX They have ideas about collision response

;; Basic stuff
(define (complex-abs v)
  (psn (abs (psn-x v))
       (abs (psn-y v))))
  
(define (distance p1 p2)
  (- p1 p2))

(define (length-sqr v)
  (+ (sqr (psn-x v)) (sqr (psn-y v))))
(define (length v)
  (sqrt (length-sqr v)))

(define (normalize v)
  (define len (length v))
  (psn (/ (psn-x v) len)
       (/ (psn-y v) len)))

(define (dot-product a b)
  (+ (* (psn-x a) (psn-x b))
     (* (psn-y a) (psn-y b))))

(define (projection a b)
  (define dp (dot-product a b))
  (define n
    (/ dp (+ (sqr (psn-x b)) (sqr (psn-y b)))))
  (psn (* n (psn-x b))
       (* n (psn-y b))))

(define (right-hand-normal a)
  (psn (- (psn-y a)) (psn-x a)))
(define (left-hand-normal a)
  (- (right-hand-normal a)))

(test
 (right-hand-normal (psn 3.0 -1.0)) => (psn 1.0 3.0)
 (left-hand-normal (psn 3.0 -1.0)) => (psn -1.0 -3.0))

(define (perproduct a b)
  (dot-product a (right-hand-normal b)))

(define x-axis (psn 1. 0.))
(define y-axis (psn 0. 1.))

(define (overlap red blue green)
  (- (+ (length red) (length blue))
     (length green)))

(test
 (overlap (psn 2. 0.)
          (psn 1. 0.)
          (psn 4. 0.))
 =>
 -1.
 
 (overlap (psn 1. 0.)
          (psn 2. 0.)
          (psn 4. 0.))
 =>
 -1.
 
 (overlap (psn 1. 0.)
          (psn 2. 0.)
          (psn 3. 0.))
 =>
 0.
 
 (overlap (psn 1. 0.)
          (psn 2. 0.)
          (psn 2. 0.))
 =>
 1.)

;; Separating Axes Theorem
(define (adjust-projection-vector p1 p2 s)
  (let*
      ([s
        (if
         ; If o1 is to the left of o2, then we need to subtract the vector from it
         (and (number? s) (<= (psn-x p1) (psn-x p2)))
         (- s)
         s)]
       [s
        (if
         ; If o1 is below o2, then we need to subtract the vector from it
         (and (number? s) (<= (psn-y p1) (psn-y p2)))
         (conjugate s)
         s)])
    s))

(define-syntax (separating-axes stx)
  (syntax-case stx ()
    [(_
      separating-axes**
      o1p o2p o1v o2v
      [axis ...])
     (with-syntax ([(axis-v ...)
                    (generate-temporaries #'(axis ...))])
       (syntax/loc stx
         (let* ([o1p-v o1p]
                [o2p-v o2p]
                [d-v (distance o1p-v o2p-v)]
                [o1v-v o1v]
                [o2v-v o2v]
                [axis-v axis]
                ...)
           (adjust-projection-vector
            o1p-v o2p-v
            (separating-axes*
             separating-axes**
             d-v o1v-v o2v-v
             [axis-v ...]
             [])))))]))

(define-syntax separating-axes*
  (syntax-rules ()
    [(_ separating-axes** dv o1v o2v [] [(axis axis-overlap) ...])
     (separating-axes** [(axis axis-overlap) ...])]
    [(_ separating-axes** dv o1v o2v [axis0 axis1 ...] [(axis axis-overlap) ...])
     (let* ([o1v-axis0 (projection o1v axis0)]
            [o2v-axis0 (projection o2v axis0)]
            [dist-axis0 (projection dv axis0)]
            [axis0-overlap
             (overlap o1v-axis0 o2v-axis0 dist-axis0)])
       (if (0 . < . axis0-overlap)
           (separating-axes* 
            separating-axes**
            dv o1v o2v 
            [axis1 ...]
            [(axis0 axis0-overlap)
             (axis axis-overlap) ...])
           #f))]))

(define-syntax-rule (separating-axes**? . _)
  #t)

(define-syntax separating-axes**
  (syntax-rules ()
    [(_ [])
     #f]
    [(_ [(axis0 axis0-overlap)
         (axis1 axis1-overlap)
         ...])
     (if (and (<= (abs axis0-overlap) (abs axis1-overlap))
              ...)
         (* axis0-overlap axis0)
         (separating-axes** [(axis1 axis1-overlap)
                             ...]))]))

;; AABBs
(struct aabb (p xw yw))
(define aabb-ul 
  (match-lambda
    [(aabb p xw yw)
     (- p (psn xw (- yw)))]))
(define aabb-ur
  (match-lambda
    [(aabb p xw yw)
     (+ p (psn xw yw))]))
(define aabb-ll 
  (match-lambda
    [(aabb p xw yw)
     (- p (psn xw yw))]))
(define aabb-lr
  (match-lambda
    [(aabb p xw yw)
     (+ p (psn xw (- yw)))]))
     
(define (aabb-v o1)
  (match-define (aabb p1 xw1 yw1) o1)
  (psn xw1 yw1))

; aabb-vs-aabb : aabb aabb -> psn
; Returns the smallest vector to add to o1 so they no longer collide
(define (aabb-vs-aabb o1 o2)
  (separating-axes
   separating-axes**
   (aabb-p o1) (aabb-p o2)
   (aabb-v o1) (aabb-v o2)
   [x-axis y-axis]))

(define (aabb-vs-aabb? o1 o2)
  (separating-axes
   separating-axes**?
   (aabb-p o1) (aabb-p o2)
   (aabb-v o1) (aabb-v o2)
   [x-axis y-axis]))

(define (test-aabb-vs-aabb o1 o2 ans)
  (define s (aabb-vs-aabb o1 o2))
  (test s => ans
        (aabb-vs-aabb? o1 o2) => (if ans #t #f)))

(test
 ; The unit square vs the two-unit square
 ; Clearly these overlap and we need to push them apart by 1.5 in some direction
 (test-aabb-vs-aabb (aabb (psn 0. 0.) .5 .5)
                    (aabb (psn 0. 0.) 1. 1.)
                    (psn -0. 1.5))
 
 (test-aabb-vs-aabb (aabb (psn 0. 0.) 1. 1.)
                    (aabb (psn 0. 0.) .5 .5)
                    (psn -0. 1.5))
 
 ; Two unit squares, one at (.5,.5) and one at (1.5,.5)
 ; They shouldn't overlap
 (projection (psn 1. 1.) x-axis) => (psn 1. 0.)
 (projection (psn 2. 1.) x-axis) => (psn 2. 0.)
 (distance (psn .5 .5) (psn 1.5 .5)) => (psn -1. .0)
 (overlap .5 .5 1.) => 0.
 (test-aabb-vs-aabb (aabb (psn .5 .5) .5 .5)
                    (aabb (psn 1.5 .5) .5 .5)
                    #f)
 
 ; Two unit squares, one at (.5,.5) and one at (1.5,1.5)
 ; They shouldn't overlap
 (test-aabb-vs-aabb (aabb (psn .5 .5) .5 .5)
                    (aabb (psn 1.5 1.5) .5 .5)
                    #f) 
 
 ; Two unit squares, one at (.5,.5) and one at (3.,3.)
 ; They shouldn't overlap
 (test-aabb-vs-aabb (aabb (psn .5 .5) .5 .5)
                    (aabb (psn 3. 3.) .5 .5)
                    #f)
 
 ; Two unit squares, one at (.5, .5) and one at (.5, .25)
 ; Should be pushed down/up by .75
 (overlap (projection (psn .5 .5) y-axis)
          (projection (psn .5 .5) y-axis)
          .25)
 => .75
 (projection (distance (psn .5 .5) (psn .5 .25)) y-axis) => (psn 0. .25)
 (test-aabb-vs-aabb (aabb (psn .5 .5) .5 .5)
                    (aabb (psn .5 .25) .5 .5)
                    (psn -.0 -.75))
 (test-aabb-vs-aabb (aabb (psn .5 .25) .5 .5)
                    (aabb (psn .5 .5) .5 .5)
                    (psn -.0 .75))
 
 ; Two unit squares, one at (.5, .5) and one at (.25, .5)
 ; Should be pushed left/right by .75
 (test-aabb-vs-aabb (aabb (psn .5 .5) .5 .5)
                    (aabb (psn .25 .5) .5 .5)
                    (psn .75 -.0))
 (test-aabb-vs-aabb (aabb (psn .25 .5) .5 .5)
                    (aabb (psn .5 .5) .5 .5)
                    (psn -.75 .0))
 
 ; A unit square at (.5,.5) and a 2x1 rectangle at (1.5,.5)
 (test-aabb-vs-aabb (aabb (psn .5 .5) .5 .5)
                    (aabb (psn 1.5 .5) 1. .5)
                    (psn -.5 .0)))

; Based on http://stackoverflow.com/questions/99353/how-to-test-if-a-line-segment-intersects-an-axis-aligned-rectange-in-2d
(define-syntax all-same?
  (syntax-rules ()
    [(_) #t]
    [(_ last) #t]
    [(_ last e r ...)
     (let ([fst last]
           [snd e])
       (and (= fst snd)
            (all-same? snd r ...)))]))
(define (aabb-vs-line s start end)
  (match-define (psn* x1 y1) start)
  (match-define (psn* x2 y2) end)
  (define (F p)
    (match-define (psn* x y) p)
    (sgn
     (+ (* (- y2 y1) x)
        (* (- x1 x2) y)
        (- (* x2 y1)
           (* x1 y2)))))
  (and
   (not
    (all-same?
     (F (aabb-ul s))
     (F (aabb-ur s))
     (F (aabb-ll s))
     (F (aabb-lr s))))
   (not
    (or
     (let ()
       (match-define (psn* xTR yTR) (aabb-ur s))
       (or
        (and (x1 . > . xTR) (x2 . > . xTR) 'right)
        (and (y1 . > . yTR) (y2 . > . yTR) 'above)))
     (let ()
       (match-define (psn* xBL yBL) (aabb-ll s))
       (or
        (and (x1 . < . xBL) (x2 . < . xBL) 'left)
        (and (y1 . < . yBL) (y2 . < . yBL) 'below)))))))
(test
 (aabb-vs-line (aabb (psn .5 .5) .5 .5) (psn 0. 0.) (psn 1. 1.))
 (aabb-vs-line (aabb (psn 1. 1.) .5 .5) (psn 0. 0.) (psn 1. 1.))
 (aabb-vs-line (aabb (psn 10. 10.) .5 .5) (psn 0. 0.) (psn 1. 1.)) => #f
 (aabb-vs-line (aabb (psn 1. 1.) .5 .5) (psn 0. 0.) (psn .5 .5))
 (aabb-vs-line (aabb (psn 1. 1.) .5 .5) (psn 0. 0.) (psn .25 .25)) => #f)

; XXX I didn't really understand how to do the other things in the article and the code was a bit too opaque for me. I should support them eventually.

;; Circles
(struct circle (p r))

(define (circle-vs-circle? c1 c2)
  (match-define (circle p1 r1) c1)
  (match-define (circle p2 r2) c2)
  (define min-distance (+ r1 r2))
  (define delta (- p2 p1))
  (define dist-sq (length-sqr delta))
  (dist-sq . < . (sqr min-distance)))

(define (circle-vs-circle c1 c2)
  (match-define (circle p1 r1) c1)
  (match-define (circle p2 r2) c2)
  (define min-distance (+ r1 r2))
  (define delta (- p2 p1))
  (if (zero? delta)
      (make-polar (max r1 r2)
                  pi)
      (let ()
        (define dist-sq (length delta))
        (if (dist-sq . < . min-distance)
            (* (- dist-sq min-distance)
               (normalize delta))
            #f))))

(define (test-circle-vs-circle c1 c2)
  (define ? (circle-vs-circle? c1 c2))
  (define v (circle-vs-circle c1 c2))
  (if ? (test v) (test v => #f))
  v)

(test
 (magnitude
  (test-circle-vs-circle 
   (circle (psn 0. 0.) 1.)
   (circle (psn 0. 0.) 1.)))
 =>
 1.
 
 (test-circle-vs-circle
  (circle (psn 0. 0.) 1.)
  (circle (psn 1. 1.) 1.))
 
 (test-circle-vs-circle
  (circle (psn 0. 0.) 1.)
  (circle (psn 0. 1.) 1.))
 
 (test-circle-vs-circle
  (circle (psn 0. 0.) 1.)
  (circle (psn 2. 2.) 1.))
 =>
 #f)

; Based on http://doswa.com/blog/2009/07/13/circle-segment-intersectioncollision/
(define (circle-vs-line c seg_a seg_b)
  (match-define (circle c_pos c_rad) c)
  (define seg_v (- seg_b seg_a))
  (define pt_v (- c_pos seg_a))
  (define len-proj_v (dot-product pt_v (normalize seg_v)))
  (define closest 
    (cond
      [(len-proj_v . < . 0)
       seg_a]
      [(len-proj_v . > . (length seg_v))
       seg_b]
      [else
       (define proj_v
         (* len-proj_v (normalize seg_v)))
       (+ seg_a proj_v)]))
  (define dist_v (- c_pos closest))
  (define len-dist_v (length dist_v))
  (len-dist_v . < .  c_rad))
(test
 (circle-vs-line (circle (psn 10. 10.) .5) (psn 0. 0.) (psn 1. 1.)) => #f
 (circle-vs-line (circle (psn 1. 1.) .5) (psn 0. 0.) (psn 1. 1.)) => #t
 (circle-vs-line (circle (psn 10. 10.) 5.) (psn 0. 0.) (psn 10. 10.)) => #t)

;; Collisions
(define (circle->aabb c)
  (match-define (circle p r) c)
  (aabb p r r))

(define (shape-vs-shape s1 s2 #:depth? [depth? #f])
  (cond
    [(and (aabb? s1) (aabb? s2))
     (if depth?
         (aabb-vs-aabb s1 s2)
         (aabb-vs-aabb? s1 s2))]
    [(and (circle? s1) (circle? s2))
     (if depth?
         (circle-vs-circle s1 s2)
         (circle-vs-circle? s1 s2))]
    [(circle? s1)
     (shape-vs-shape (circle->aabb s1) s2 #:depth? depth?)]
    [(circle? s2)
     (shape-vs-shape s1 (circle->aabb s2) #:depth? depth?)]))

(test
 (shape-vs-shape
  (circle (psn 0. 0.) 1.)
  (aabb (psn 1. 1.) .5 .5))
 
 (shape-vs-shape
  (circle (psn 5. 5.) 1.)
  (aabb (psn 1. 1.) .5 .5))
 =>
 #f)

(define shape/c
  (or/c circle? aabb?))

(define (shape->aabb s)
  (if (circle? s)
      (circle->aabb s)
      s))

(define (shape-vs-line s start end)
  (cond
    [(circle? s)
     (circle-vs-line s start end)]
    [(aabb? s)
     (aabb-vs-line s start end)]))

(provide/contract
 [struct aabb ([p psn?] [xw real?] [yw real?])]
 [aabb-ul (-> aabb? psn?)]
 [aabb-ur (-> aabb? psn?)]
 [aabb-ll (-> aabb? psn?)]
 [struct circle ([p psn?] [r real?])]
 [shape/c contract?]
 [shape->aabb
  (-> shape/c aabb?)]
 [shape-vs-shape
  (->* (shape/c shape/c)
       (#:depth? boolean?)
       (or/c #t #f psn?))]
 [shape-vs-line
  (-> shape/c psn? psn?
      boolean?)])
