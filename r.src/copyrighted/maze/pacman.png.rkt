#lang s-exp gb/tools/sprite

(define (player which-frame)
  (sprite (format "maze/player/~a" which-frame)
          (+ 3 (* 15 which-frame))
          90
          14 14))

(for ([i (in-range 3)])
  (player i))

(define (ghost which-ghost which-set which-frame)
  (sprite (format "maze/ghost/~a/~a/~a" which-ghost which-set which-frame)
          (+ 3 (* 17 (+ which-frame (* 2 which-set))))
          (+ 125 (* 18 which-ghost))
          14 12))

(for* ([which-ghost (in-range 5)]
       [which-set (in-range 4)]
       [which-frame (in-range 2)])
  (ghost which-ghost which-set which-frame))
