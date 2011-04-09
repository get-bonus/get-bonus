#lang racket/base
; 2d Collision Detection - Narrow Phase
; based on http://www.metanetsoftware.com/technique/tutorialA.html
(require (for-syntax racket/base)
         racket/math
         racket/match
         "psn.rkt"
         tests/eli-tester)

; XXX They have ideas about collision response

;; Basic stuff
(define (complex-abs v)
  (psn (abs (psn-x v))
       (abs (psn-y v))))
  
(define (distance p1 p2)
  (- p1 p2))

(define (length v)
  (sqrt (+ (sqr (psn-x v)) (sqr (psn-y v)))))

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
         (and s (<= (psn-x p1) (psn-x p2)))
         (- s)
         s)]
       [s
        (if
         ; If o1 is below o2, then we need to subtract the vector from it
         (and s (<= (psn-y p1) (psn-y p2)))
         (conjugate s)
         s)])
    s))

(define-syntax (separating-axes stx)
  (syntax-case stx ()
    [(_
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
             d-v o1v-v o2v-v
             [axis-v ...]
             [])))))]))

(define-syntax separating-axes*
  (syntax-rules ()
    [(_ dv o1v o2v [] [(axis axis-overlap) ...])
     (separating-axes** [(axis axis-overlap) ...])]
    [(_ dv o1v o2v [axis0 axis1 ...] [(axis axis-overlap) ...])
     (let* ([o1v-axis0 (projection o1v axis0)]
            [o2v-axis0 (projection o2v axis0)]
            [dist-axis0 (projection dv axis0)]
            [axis0-overlap
             (overlap o1v-axis0 o2v-axis0 dist-axis0)])
       (if (0 . < . axis0-overlap)
           (separating-axes* dv o1v o2v 
                             [axis1 ...]
                             [(axis0 axis0-overlap)
                              (axis axis-overlap) ...])
           #f))]))

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
(define (aabb-v o1)
  (match-define (aabb p1 xw1 yw1) o1)
  (psn xw1 yw1))

; aabb-vs-aabb : aabb aabb -> psn
; Returns the smallest vector to add to o1 so they no longer collide
(define (aabb-vs-aabb o1 o2)
  (separating-axes
   (aabb-p o1) (aabb-p o2)
   (aabb-v o1) (aabb-v o2)
   [x-axis y-axis]))

#;(define (aabb-vs-aabb o1 o2)
  (define d (distance (aabb-p o1) (aabb-p o2)))
  (define x-overlap
    (overlap (projection (aabb-v o1) x-axis)
             (projection (aabb-v o2) x-axis)
             (projection d x-axis)))
  (if (0 . < . x-overlap)
      (let ()
        (define y-overlap
          (overlap (projection (aabb-v o1) y-axis)
                   (projection (aabb-v o2) y-axis)
                   (projection d y-axis)))
        (if (0 . < . y-overlap)
            (if (x-overlap . <= . y-overlap)
                (* x-overlap x-axis)
                (* y-overlap y-axis))
            #f))
        #f))

(test
 ; The unit square vs the two-unit square
 ; Clearly these overlap and we need to push them apart by 1.5 in some direction
 (aabb-vs-aabb (aabb (psn 0. 0.) .5 .5)
               (aabb (psn 0. 0.) 1. 1.))
 =>
 (psn -0. 1.5)
 (aabb-vs-aabb (aabb (psn 0. 0.) 1. 1.)
               (aabb (psn 0. 0.) .5 .5))
 =>
 (psn -0. 1.5)

 ; Two unit squares, one at (.5,.5) and one at (1.5,.5)
 ; They shouldn't overlap
 (projection (psn 1. 1.) x-axis) => (psn 1. 0.)
 (projection (psn 2. 1.) x-axis) => (psn 2. 0.)
 (distance (psn .5 .5) (psn 1.5 .5)) => (psn -1. .0)
 (overlap .5 .5 1.) => 0.
 (aabb-vs-aabb (aabb (psn .5 .5) .5 .5)
               (aabb (psn 1.5 .5) .5 .5))
 =>
 #f
 
 ; Two unit squares, one at (.5,.5) and one at (1.5,1.5)
 ; They shouldn't overlap
 (aabb-vs-aabb (aabb (psn .5 .5) .5 .5)
               (aabb (psn 1.5 1.5) .5 .5))
 =>
 #f 
 
 ; Two unit squares, one at (.5,.5) and one at (3.,3.)
 ; They shouldn't overlap
 (aabb-vs-aabb (aabb (psn .5 .5) .5 .5)
               (aabb (psn 3. 3.) .5 .5))
 =>
 #f
 
 ; Two unit squares, one at (.5, .5) and one at (.5, .25)
 ; Should be pushed down/up by .75
 (overlap (projection (psn .5 .5) y-axis)
          (projection (psn .5 .5) y-axis)
          .25)
 => .75
 (projection (distance (psn .5 .5) (psn .5 .25)) y-axis) => (psn 0. .25)
 (aabb-vs-aabb (aabb (psn .5 .5) .5 .5)
               (aabb (psn .5 .25) .5 .5))
 =>
 (psn -.0 -.75)
 (aabb-vs-aabb (aabb (psn .5 .25) .5 .5)
               (aabb (psn .5 .5) .5 .5))
 =>
 (psn -.0 .75)
 
 ; Two unit squares, one at (.5, .5) and one at (.25, .5)
 ; Should be pushed left/right by .75
 (aabb-vs-aabb (aabb (psn .5 .5) .5 .5)
               (aabb (psn .25 .5) .5 .5))
 =>
 (psn .75 -.0)
 (aabb-vs-aabb (aabb (psn .25 .5) .5 .5)
               (aabb (psn .5 .5) .5 .5))
 =>
 (psn -.75 .0)
 
 ; A unit square at (.5,.5) and a 2x1 rectangle at (1.5,.5)
 (aabb-vs-aabb (aabb (psn .5 .5) .5 .5)
               (aabb (psn 1.5 .5) 1. .5))
 =>
 (psn -.5 .0))

