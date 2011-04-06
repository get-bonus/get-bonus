#lang racket/base
; Thanks to Bryan Morse!
(require racket/set
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
    (if (empty? l)
        #f
        (apply min l)))
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
  (define (equivalent! f ls)
    (for ([e (in-list ls)])
      (is-equivalent! f e)))
    
  (for* ([x (in-range w)]
         [y (in-range h)])
    (define p (matrix-ref pixels x y))
    (unless (background? p)
      (define ls (neighbor-labels x y))
      (define l (set-choose ls))
      (if l
          (begin
            (matrix-set! labeling x y l)
            (equivalent! l ls))
          (matrix-set! labeling x y (new-label)))))
  
  (define new-labels (make-hasheq))
  (define new-label-i 0)
  (define (get-new-label r)
    (hash-ref! new-labels r
               (λ ()
                 (begin0 new-label-i
                         (set! new-label-i (add1 new-label-i))))))
    
  (define (label-fixpoint s)
    (define r (hash-ref labels s))
    (if (eq? r s)
        (get-new-label r)
        (label-fixpoint r)))
  
  (define relabeling (matrix w h))
  (for* ([x (in-range w)]
         [y (in-range h)])
    (define l (matrix-ref labeling x y))
    (when l
      (matrix-set! relabeling x y
                   (label-fixpoint l))))
  
  ; XXX As I go through the relabeling, find the min and max x and y for each label
  ;     those are the sprites
  
  relabeling)

; Usage
(require racket/runtime-path)
(define-runtime-path resource-path "../resources")
(define p (build-path resource-path "generalsprites.png"))
(define p-l (build-path resource-path "generalsprites-labeled.png"))
(define bm (make-object bitmap% p 'png/alpha #f #t))
(define w (send bm get-width))
(define h (send bm get-height))
  
(define l (components bm (λ (p) (zero? (pixel-a p)))))

(define nbm (make-object bitmap% w h))
(define argb (make-bytes (* 4 w h) 0))

(for* ([x (in-range w)]
       [y (in-range h)])
  (define n (matrix-ref l x y))
  (when n
    (define offset (+ (* 4 y w) (* 4 x)))
    (bytes-set! argb (+ offset 0) 1)
    (bytes-set! argb (+ offset 1) (modulo n 255))
    (bytes-set! argb (+ offset 2) (modulo (expt n 2) 255))
    (bytes-set! argb (+ offset 3) (modulo (expt n 3) 255))))

(send nbm set-argb-pixels 0 0 w h argb #f)
(send nbm save-file p-l 'png 100)
