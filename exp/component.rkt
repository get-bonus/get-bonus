#lang racket/base
; Thanks to Bryan Morse!
(require racket/set
         racket/function
         racket/match
         racket/class
         racket/list
         racket/gui/base)

(struct matrix* (w v))
(define (matrix w h)
  (matrix* w (make-vector (* w h) #f)))
(define (matrix-set! m x y e)
  (match-define (matrix* w v) m)
  (vector-set! v (+ (* y w) x) e))
(define (matrix-ref m x y)
  (match-define (matrix* w v) m)
  (vector-ref v (+ (* y w) x)))

(struct pixel (r g b a))
(define (argb->matrix bs w h)
  (define m (matrix w h))
  (for* ([x (in-range w)]
         [y (in-range h)])
    (define offset (+ (* 4 y w) (* 4 x)))
    (matrix-set! m x y
                 (pixel (bytes-ref bs (+ offset 1))
                        (bytes-ref bs (+ offset 2))
                        (bytes-ref bs (+ offset 3))
                        (bytes-ref bs (+ offset 0)))))
  m)

(struct aabb (x1 y1 x2 y2) #:transparent)

(define (components bm background?)
  (define w (send bm get-width))
  (define h (send bm get-height))
  (define argb (make-bytes (* 4 w h) 0))
  (send bm get-argb-pixels 0 0 w h argb #f)
  (define pixels (argb->matrix argb w h))
  (define labeling (matrix w h))
  (define (matrix-ref* m x y)
    (and (<= 0 x (sub1 w))
         (<= 0 y (sub1 h))
         (matrix-ref m x y)))
  
  (define labels (make-hasheq))
  (define label-i 0)
  (define (new-label)
    (define s label-i)
    (set! label-i (add1 label-i))
    (hash-set! labels s s)
    s)
    
  (define (set-choose l)
    (apply min l))
  (define (neighbor-labels x y)
    (for*/fold ([ls empty])
      ([dx (in-list (list -1 0 +1))]
       [dy (in-list (list -1 0 +1))])
      (define l (matrix-ref* labeling (+ dx x) (+ dy y)))
      (if l
          (cons l ls)
          ls)))
  
  (define (is-equivalent! f e)
    (define old (hash-ref labels e))
    (hash-set! labels e f)
    (unless (eq? old e)
      (is-equivalent! f old)))
  
  (for* ([x (in-range w)]
         [y (in-range h)])
    (define p (matrix-ref pixels x y))
    (unless (background? p)
      (define ls (cons (new-label) (neighbor-labels x y)))
      (define l (set-choose ls))
      (matrix-set! labeling x y l)
      (for-each (λ (e) (is-equivalent! l e)) ls)))
  
  (define objs (make-hasheq))
  (define (expand-obj x y s)
    (define r (hash-ref labels s))
    (if (eq? r s)
        (hash-update! objs r
                  (λ (a)
                    (match-define (aabb x1 y1 x2 y2) a)
                    (aabb (min x1 x)
                          (min y1 y)
                          (max x2 x)
                          (max y2 y)))
                  (aabb x y x y))
        (expand-obj x y r)))
  
  (for* ([x (in-range w)]
         [y (in-range h)])
    (define l (matrix-ref labeling x y))
    (when l
      (expand-obj x y l)))
  
  (list->vector
   (for/list ([o (in-hash-values objs)])
     o)))

; XXX contracts
(provide (all-defined-out))