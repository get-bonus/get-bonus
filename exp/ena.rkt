#lang racket/base
(require racket/match
         gb/lib/godel
         2htdp/image
         2htdp/universe)

(define (enumerate&animate spec stop? width height draw)
  (define (time->world t)
    (printf "~e\n" t)
    (cons t (decode spec t)))
  (big-bang
   (time->world 0)
   [on-tick
    (λ (t*s)
      (time->world (add1 (car t*s))))]
   [stop-when
    (λ (t*s)
      (stop? (cdr t*s)))]
   [to-draw
    (λ (t*s)
      (draw (cdr t*s)))]))

(module+ main
  (require games/rpswar/fst)

  (define (go! N)
    (define the-scale 50)

    (define (draw-fst f)
      (define states (fst-states f))
      (define axis/s (nat-range/s (ceiling (sqrt states))))
      (define xy/s (cons/s axis/s axis/s))

      (define (char->color c)
        (match c
          ['r "red"]
          ['b "blue"]
          ['g "green"]))
      (define (state->color s)
        (char->color (hash-ref (fst-state->output f) s)))
      (define (state->xy s)
        (match-define (cons sx sy) (decode xy/s s))
        (values (* the-scale (+ sx 0.5)) (* the-scale (+ sy 0.5))))

      (for/fold ([img (rectangle (* the-scale N) (* the-scale N)
                                 "solid" "white")])
          ([(s delta) (in-hash (fst-delta f))])
        (define-values (sx sy) (state->xy s))
        (for/fold ([img (place-image 
                         (circle (/ the-scale 4) "solid" (state->color s))
                         sx sy img)])
            ([(o d) (in-hash delta)])
          (define-values (dx dy) (state->xy d))
          (add-line img
                    sx sy
                    dx dy
                    (make-pen (char->color o) 1
                              "solid" "round" "round")))))

    (enumerate&animate
     (fst/s '(r g b)
            '(r g b))
     (λ (f)
       (> (sqrt (fst-states f)) N))
     (* the-scale N) (* the-scale N)
     draw-fst))

  (go! 5))
