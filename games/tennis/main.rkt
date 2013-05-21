#lang racket/base
(require racket/runtime-path
         racket/match
         racket/function
         racket/math
         racket/list
         gb/gui/world
         gb/gui/os
         gb/graphics/ngl-main
         gb/data/mvector
         gb/input/controller
         gb/audio/3s
         gb/lib/math
         gb/lib/random
         gb/data/psn
         gb/meta
         gb/meta-help
         (prefix-in cd: gb/physics/cd-narrow))

(define-runtime-path resource-path "r")

(define-syntax-rule (define-sound id f)
  (define id (path->audio (build-path resource-path f))))

(define-sound se:applause "applause.wav")
(define-sound se:bgm "bgm.ogg")
(define-sound se:bump-lhs "bump-lhs.ogg")
(define-sound se:bump-rhs "bump-rhs.ogg")
(define-sound se:bump-wall "bump-wall.wav")

(define width crt-width)
(define height crt-height)

(define width-h (/ width 2.))
(define center-pos
  (psn width-h (/ height 2.)))
(define speed
  (* (/ (sqrt (+ (sqr width) (sqr height)))
        4.0)
     RATE))
(define initial-ball-speed
  speed)

(define blocks-in-a-paddle 4)

(define tennis/paddle
  (sprited-ref spr:sos/font 111))
(define tennis/ball
  (sprited-ref spr:sos/face/smile 0))

(define paddle-w
  (sprited-width spr:sos/font))
(define paddle-hw
  (/ paddle-w 2.0))
(define paddle-h
  (* blocks-in-a-paddle (sprited-height spr:sos/font)))
(define paddle-hh
  (/ paddle-h 2.0))

(define min-paddle-y
  paddle-hh)
(define max-paddle-y
  (- height paddle-hh))

(define (lhs-paddle)
  (define block-h
    (/ paddle-h blocks-in-a-paddle))
  (for/list ([i (in-range blocks-in-a-paddle)])
    (transform
     #:dy (* block-h i)
     (sprite tennis/paddle))))

(define ball-scale
  (/ 1.0 2.0))
(define ball-hh
  (* ball-scale (/ (sprited-height spr:sos/face/smile) 2.0)))
(define ball-r ball-hh)
(define ball-hw
  (* ball-scale (/ (sprited-width spr:sos/face/smile) 2.0)))

(define (ball-sprite)
  (transform
   #:d (- ball-r) (- ball-r)
   #:mxy ball-scale
   (sprite tennis/ball)))

(define (bgm)
  ;; xxx make a background out of overworld bricks
  empty)

(define lhs-x
  (- (/ width 32.0) paddle-hw))

(define serve-dist
  (* 1.5 (+ ball-hw paddle-hw)))
(define ball-start-pos
  (psn (+ lhs-x serve-dist) (/ height 2.)))

(define (player-paddle)
  (let loop ([lhs-y (/ height 2.0)])
    (define lhs-dy
      (controller-ldpad-y (os/read* 'controller)))
    (define lhs-y-n
      (clamp
       min-paddle-y
       (+ lhs-y (* lhs-dy speed))
       max-paddle-y))
    (os/write
     (list
      (cons 'lhs-y lhs-y-n)
      (cons 'graphics
            (cons 0
                  (transform #:d
                             (+ lhs-x paddle-hw)
                             (- lhs-y-n paddle-hh)
                             (lhs-paddle))))))
    (loop lhs-y-n)))

(define (ball-bounce dir mx my [dy 0.])
  (define p (make-polar 1.0 dir))
  (angle
   (make-rectangular (* mx (real-part p))
                     (+ ;;(* -1 dy) ;;; XXX see comment below
                      (* my (imag-part p))))))

(define ((ball dir-seq initial-ball-speed))
  (let loop ([taps 0]
             [ball-pos ball-start-pos]
             [ball-dir (sequence-first dir-seq)])
    (define (ball-speed taps)
      (* (+ 1 (* .1 taps)) initial-ball-speed))
    (define (ball-in-dir ball-pos dir)
      (+ ball-pos (make-polar (ball-speed taps) dir)))
    (define lhs-y-n (os/read* 'lhs-y (/ height 2.0)))
    (define lhs-shape
      (cd:aabb (psn (+ lhs-x paddle-hw) lhs-y-n)
               paddle-hw paddle-hh))
    (define ball-shape
      (cd:aabb ball-pos ball-hw ball-hh))
    (define-values
      (ball-pos-n ball-dir-n sounds score?)
      (cond
        [; The ball has bounced off the lhs
         (cd:shape-vs-shape ball-shape lhs-shape)
         (define ball-pos-pushed
           (psn (max (+ lhs-x (* 4 paddle-hw))
                     (psn-x ball-pos))
                (psn-y ball-pos)))
         (values ball-pos-pushed
                 ;; XXX Sometimes the ball can bend too far. I need to
                 ;; tone this done a little.
                 (ball-bounce ball-dir -1.0 1.0
                              (/ (- lhs-y-n (psn-y ball-pos)) paddle-h))
                 (list (cons 'sound (sound-at se:bump-lhs ball-pos)))
                 (add1 taps))]
        ;; The ball has moved to the left of the lhs paddle
        [((psn-x ball-pos) . <= . lhs-x)
         (values ball-start-pos (sequence-first dir-seq)
                 empty 'right)]
        ;; The ball hit a horizontal wall
        [(or
          ;; The ball hit the top
          ((psn-y ball-pos) . >= . height)
          ;; The ball hit the bot
          ((psn-y ball-pos) . <= . 0))
         (values ball-pos
                 (ball-bounce ball-dir 1.0 -1.0)
                 (list (cons 'sound (sound-at se:bump-wall ball-pos)))
                 #f)]
        ;; The ball hit the right vertical wall
        [((psn-x ball-pos) . >= . width)
         (values ball-pos
                 (ball-bounce ball-dir -1.0 1.0)
                 (list (cons 'sound (sound-at se:bump-wall ball-pos)))
                 #f)]
        ;; The ball is inside the frame
        [else
         (values ball-pos ball-dir empty #f)]))
    (define taps-n
      (if (number? score?)
        (add1 taps)
        taps))
    (define new-ball?
      (if (= taps taps-n)
        #f
        (if (zero? (modulo taps-n 5))
          (begin (os/thread
                  (ball dir-seq
                        (ball-speed (/ taps 2))))
                 #t)
          #f)))
    (os/write
     (list*
      (cons 'score? score?)
      (cons 'graphics
            (cons 0
                  (transform #:d (psn-x ball-pos-n) (psn-y ball-pos-n)
                             #:rot ball-dir-n
                             (ball-sprite))))
      (append
       sounds
       (if new-ball?
         (list (cons 'sound (sound-at se:applause center-pos)))
         empty))))
    (unless (eq? score? 'right)
      (loop taps-n
            (ball-in-dir ball-pos-n ball-dir-n)
            ball-dir-n))))

(define char-height
  (sprited-height spr:sos/font))
(define char-width
  (sprited-width spr:sos/font))
(define string->sprites
  (make-string-factory spr:sos/font))

(require racket/generator)
(define (game-start dir-seq)
  (big-bang/os
   width height center-pos
   #:sound-scale width-h
   (λ ()
     (os/thread player-paddle)
     (os/thread (ball dir-seq initial-ball-speed))
     (os/write
      (list
       (cons 'sound (background (λ (w) se:bgm) #:gain 0.1))
       (cons 'listener center-pos)))
     (let loop ([score 0])
       (define score?s (os/read 'score?))
       (define balls (length score?s))
       (define score-n (+ score (apply + (filter number? score?s))))
       (os/write
        (list
         (cons 'done? (zero? balls))
         (cons 'return score-n)
         (cons 'graphics
               (cons
                10.0
                (cons
                 (let ()
                   (define score-s
                     (format "~a" score-n))
                   (define score-h
                     char-height)
                   (define score-w
                     (* char-width (string-length score-s)))
                   (transform
                    #:d
                    (- (psn-x center-pos)
                       (/ score-w 2.0))
                    (- height score-h)
                    (cons
                     (string->sprites
                      score-s)
                     (transform
                      #:rgba 1. 1. 1. 1.
                      (string->sprites
                       #:tint? #t
                       (make-string (string-length score-s) #\space))))))
                 (bgm))))))
       (loop score-n)))))

(require gb/lib/godel
         gb/lib/godel-seq)

(define 0..90->radians
  (compose1 degrees->radians (λ (n) (- n 45))))

(define radians->0..90
  (compose1 (λ (n) (+ n 45)) radians->degrees))

(define tennis/s
  (wrap/s
   (nat-range/s 91)
   0..90->radians
   radians->0..90))

(module+ test
  (for ([i (in-naturals)]
        [x (in-range 10)])
    (printf "~a. ~v\n" i (decode tennis/s i))))

(define tennis-seq/s
  (infinite-sequence/s tennis/s))

(define game
  (game-info 'tennis "Tennis!" 
             (list "Bounce the ball against the back wall by moving the paddle up and down. As you hit the ball more times, it moves faster and spawns child balls that start slightly slower. Your score is a function of how many times you've bounced a ball."
                   "Compare to Pong(R) by Atari (1972)")
             0
             (random-godel-generate tennis-seq/s)
             (godel-start tennis-seq/s game-start)))

(provide game)
