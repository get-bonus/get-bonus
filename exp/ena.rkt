#lang racket/base
(require racket/match
         racket/function
         racket/list
         racket/math
         gb/lib/godel
         2htdp/image
         2htdp/universe)

(define (enumerate&animate spec f stop? width height draw)
  (define (time->world t)
    (cons t (decode spec t)))
  (big-bang
   (time->world 0)
   [on-tick
    (λ (t*s)
      (time->world (f (car t*s))))]
   [stop-when
    (λ (t*s)
      (stop? (cdr t*s)))]
   [to-draw
    (λ (t*s)
      (draw (cdr t*s)))]))

(module+ main
  (struct graph (states state->nexts))

  (define graph/s
    (wrap/s (inf*k-bind/s
             nat/s
             (λ (how-many-states)
               (flist/s how-many-states
                        (nat-range/s
                                  how-many-states))))
            (match-lambda
             [(cons how-many-states
                    (list pointed-to-by ...))
              (graph how-many-states
                     (for/fold ([ht (for/hash 
                                        ([d (in-range how-many-states)])
                                      (values d empty))])
                         ([d (in-range how-many-states)]
                          [s (in-list pointed-to-by)])
                       (hash-update ht s (curry cons d))))])
            error))

  (define (go! N)
    (define the-scale 50)

    (define (draw-graph f)
      (define states (graph-states f))
      (define axis/s (nat-range/s (ceiling (sqrt states))))
      (define xy/s (cons/s axis/s axis/s))

      (define (state->color s)
        "black")
      (define r (* the-scale N 1/3))
      (define a (* the-scale N 1/2))
      (define b a)
      (define (state->xy s)
        (define t (* 2 pi (/ s states)))
        (define sx (+ a (* r (cos t))))
        (define sy (+ b (* r (sin t))))
        (values (- sx 0.5) (- sy 0.5)))

      (for/fold ([img (rectangle (* the-scale N) (* the-scale N)
                                 "solid" "white")])
          ([(s ns) (in-hash (graph-state->nexts f))])
        (define-values (sx sy) (state->xy s))
        (for/fold ([img (place-image
                         (circle (/ r states) "solid" (state->color s))
                         sx sy img)])
            ([d (in-list ns)])
          (define-values (dx dy) (state->xy d))
          (add-line img
                    sx sy
                    dx dy
                    (make-pen "red" 1
                              "solid" "round" "round")))))

    (enumerate&animate
     graph/s
     (λ (n)
       ;; This is too slow
       (add1 n)
       ;; This is mesmerizing, but a little fast
       (+ (* n 2) 1)
       ;; Doesn't feel much different than above
       (+ (inexact->exact (floor (* n (+ 1 (random))))) 1))
     (λ (f) #f)
     (* the-scale N) (* the-scale N)
     draw-graph))

  (go! 5))
