
#lang racket/gui

#|

Corresponds to the affine transformation:


f(x,y) = |a b| | x | + | c |
         |c d| | y |   | d |

but used with probability p

|#
(struct affine/p (a b c d e f p))

(define (draw-ifs dc dx dy h factor affines iterations)
  (unless (= 1 (apply + (map affine/p-p affines)))
    (error 'draw "probabilities didn't sum to 1"))
  (let loop ([x 0]
             [y 0]
             [i iterations])
    (unless (zero? i)
      (send dc draw-point (+ dx (* factor x)) (- h (+ dy (* y factor))))
      (define next (pick-transformation affines))
      (define-values (nx ny) (apply-transformation next x y))
      (loop nx ny (- i 1)))))

(define (pick-transformation transformations)
  (let loop ([v (random)]
             [transformations transformations])
    (cond
      [(null? transformations) (error 'pick-transformation "ack")]
      [else
       (define transformation (car transformations))
       (cond
         [(<= v (affine/p-p transformation))
          transformation]
         [else
          (loop (- v (affine/p-p transformation))
                (cdr transformations))])])))

(define (apply-transformation an-affine/p x y)
  (match-define (affine/p a b c d e f p) an-affine/p)
  (values (+ (* a x) (* b y) e)
          (+ (* c x) (* d y) f)))

;; adjusts the Barnsley fern IFS based on the arguments
;; (which are expected to be nats
(define (mk/steps x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16)
  (define (x xi v)
    (cond
      [(<= v 0) (+ v (* xi -0.01))]
      [else (+ v (* xi 0.01))]))
  (list (affine/p 0 0 0 (x x1 0.16) 0 0 #e0.01)
        (affine/p (x x2 0.85) 0.04 (x x3 -0.04) (x x4 0.85) 0 (x x5 1.6) #e0.85)
        (affine/p (x x6 0.2) (x x7 -0.26) (x x8 0.23) (x x9 0.22) 0 (x x10 1.6) #e0.07)
        (affine/p (x x11 -0.15) (x x12 0.28) (x x13 0.26) (x x14 0.24) 0 (x x15 0.44) #e0.07)))

;; adjusts the Barnsley fern IFS (by calling mk/steps) 
;; based on the bits in 'n'. Take the number's binary
;; representation, chop it up into chunks of size 16
;; and collect offsets to pass to mk/steps based on that.
(define (mk/n n)
  (let loop ([n n]
             [x1 0]
             [x2 0]
             [x3 0]
             [x4 0]
             [x5 0]
             [x6 0]
             [x7 0]
             [x8 0]
             [x9 0]
             [x10 0]
             [x11 0]
             [x12 0]
             [x13 0]
             [x14 0]
             [x15 0]
             [x16 0])
    (cond
      [(<= n 0)
       (mk/steps x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16)]
      [else
       (loop (- n 16) ;(arithmetic-shift n -16)
             (+ x1 (p n 0))
             (+ x2 (p n 1))
             (+ x3 (p n 2))
             (+ x4 (p n 3))
             (+ x5 (p n 4))
             (+ x6 (p n 5))
             (+ x7 (p n 6))
             (+ x8 (p n 7))
             (+ x9 (p n 8))
             (+ x10 (p n 9))
             (+ x11 (p n 10))
             (+ x12 (p n 11))
             (+ x13 (p n 12))
             (+ x14 (p n 13))
             (+ x15 (p n 14))
             (+ x16 (p n 15)))])))

(define (p n m) 
  (if (< n m)
      0
      1)
  #;
  (if (bitwise-bit-set? n m) 1 0))

;; drawing coordinates to make the fern not look horrible
(define (draw-into-bitmap bdc cw ch margin which)
  (draw-ifs bdc
        (+ margin (/ (- cw margin margin) 2)) 
        margin
        ch
        (min (/ (- cw margin margin) 5) (/ (- ch margin margin) 10))
        (mk/n which)
        10000))

(define bitmap-to-draw #f)

;; run a thread to compute the various drawing
;; syncronize with the eventspace handling thread
;; to hand off a new bitmap to draw
(void
 (thread
  (λ () 
    (define size 200)
    (define current-bitmap (make-bitmap (* size 3/2) (* size 3/2)))
    (define other-bitmap (make-bitmap (* size 3/2) (* size 3/2)))
    (define bdc (make-object bitmap-dc% current-bitmap))
    (send bdc set-smoothing 'aligned)
    (send bdc set-pen "forestgreen" 1 'solid)
    (define sema (make-semaphore 0))
    (let loop ([which 0])
      (send bdc set-bitmap current-bitmap)
      (random-seed 0)
      (send bdc clear)
      (draw-into-bitmap bdc size size 4 which)
      (send bdc set-bitmap #f)
      
      (queue-callback
       (λ ()
         (set! bitmap-to-draw current-bitmap)
         (queue-callback (λ () (send c refresh)) #f)
         (semaphore-post sema)))
      (semaphore-wait sema)
      (let ([t current-bitmap])
        (set! current-bitmap other-bitmap)
        (set! other-bitmap current-bitmap))
      (loop (+ which 1))))))


(define f (new frame% [label ""] [width 200] [height 200]))
(define c (new canvas% 
               [parent f]
               [paint-callback 
                (λ (c dc)
                  (when bitmap-to-draw
                    (send dc draw-bitmap bitmap-to-draw 0 0)))]))
(send f show #t)
