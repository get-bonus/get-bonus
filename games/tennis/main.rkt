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
         gb/gui/fullscreen
         gb/input/controller
         gb/audio/3s
         gb/lib/math
         gb/data/psn
         gb/meta
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

(define paddle-w
  (texture-width tennis/paddle))
(define paddle-hw
  (/ paddle-w 2.0))
(define paddle-h
  (* blocks-in-a-paddle (texture-height tennis/paddle)))
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
  (* ball-scale (/ (texture-height tennis/ball) 2.0)))
(define ball-r ball-hh)
(define ball-hw 
  (* ball-scale (/ (texture-width tennis/ball) 2.0)))

(define (ball-sprite)
  (transform
   #:d (- ball-r) (- ball-r)
   (rectangle ball-hw ball-hh
              tennis/ball)))

(define (bgm)
  (transform
   #:d (/ width 2.0) (/ height 2.0)
   (sprite tennis/bg)))

(define lhs-x
  (- (/ width 32.0) paddle-hw))

(define (between lo hi)
  (+ lo (* (random) (- hi lo))))
(define serve-dist
  (* 1.5 (+ ball-hw paddle-hw)))
(define ball-start-pos
  (psn (+ lhs-x serve-dist) (/ height 2.)))
(define (ball-start-dir)
  (* (if (zero? (random 2))
       -1.
       1.)
     (between 0 (/ pi 4.))))

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

(define ((ball initial-ball-speed))
  (let loop ([taps 0]
             [ball-pos ball-start-pos]
             [ball-dir (ball-start-dir)])
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
         (values ball-start-pos (ball-start-dir)
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
          (begin (os/thread (ball (ball-speed (/ taps 2))))
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

(define modern-12-char
  (make-char-factory modern 12))
(define char-height
  (texture-height (modern-12-char #\a)))
(define char-width
  (texture-width (modern-12-char #\a)))
(define string->sprites
  (make-string-factory modern-12-char))

(define (game-start)
  (big-bang/os
   width height center-pos
   #:sound-scale width-h
   (λ ()
     (os/thread player-paddle)
     (os/thread (ball initial-ball-speed))
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

(define game
  (game-info "Tennis!"
             game-start))

(provide game)
