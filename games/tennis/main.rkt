#lang racket/base
(require racket/runtime-path
         racket/match
         racket/function
         racket/math
         racket/list
         gb/gui/world
         gb/gui/os
         (prefix-in gl:
                    (combine-in gb/graphics/gl
                                gb/graphics/gl-ext))
         gb/graphics/sprites
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

(define width 16.)
(define width-h (/ width 2.))
(define height 9.)
(define center-pos
  (psn width-h (/ height 2.)))
(define speed
  (* 4.5 RATE))
(define initial-ball-speed
  speed)

(define paddle-w
  .5)
(define paddle-hw
  (/ paddle-w 2))
(define paddle-h
  (/ height 9))
(define paddle-hh
  (/ paddle-h 2))

(define min-paddle-y
  paddle-hh)
(define max-paddle-y
  (- height paddle-hh))

(define paddle-blocks
  (gl:path->texture (build-path resource-path "tetrispiecess.png")))
(define blocks-in-a-paddle 5)
(define (stack n gap cmd)
  (gl:for/gl ([i (in-range n)])
             (gl:translate 0. (* gap i) cmd)))

(define (paddle-at px)
  (define block-h
    (/ paddle-h blocks-in-a-paddle))
  (define block
    (gl:texture/px paddle-blocks
                   paddle-w block-h
                   px 44
                   10 10))
  (stack blocks-in-a-paddle block-h
         block))

(define lhs-paddle
  (paddle-at 4))

(define ball-r .25)
(define ball-hw (* 1.5 ball-r))
(define ball-hh ball-r)

(define ball-sprites
  (gl:path->texture
   (build-path resource-path "ryu.png")))
(define gl:ball
  (gl:translate
   (- ball-r) (- ball-r)
   (gl:texture/px
    ball-sprites
    (* 2 ball-hw) (* 2 ball-hh)
    484 683
    36 24)))

(define bgm-img
  (gl:path->texture
   (build-path resource-path "potosvillage.png")))
(define bgm
  (gl:texture/px
   bgm-img
   width height
   315 265
   336 189))

(define lhs-x
  (- .5 paddle-hw))

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

;; XXX Make the ball whoosh
;; XXX Make scores have calls

(define (player-paddle)
  (let loop ([lhs-y 4.5])
    (define lhs-dy
      (controller-dpad-y (os/read* 'controller)))
    (define lhs-y-n
      (clamp
       min-paddle-y
       (+ lhs-y (* lhs-dy speed))
       max-paddle-y))
    (os/write
     (list
      (cons 'lhs-y lhs-y-n)
      (cons 'graphics
            (gl:translate lhs-x (- lhs-y-n paddle-hh)
                          lhs-paddle))))
    (loop lhs-y-n)))

(define (ball-bounce dir mx my [dy 0.])
  (define p (make-polar 1.0 dir))
  (angle
   (make-rectangular (* mx (real-part p))
                     (+ ;;(* -1 dy) ;;; see comment below
                      (* my (imag-part p))))))

(define ((ball initial-ball-speed))
  (let loop ([taps 0]
             [ball-pos ball-start-pos]
             [ball-dir (ball-start-dir)])
    (define (ball-speed taps)
      (* (+ 1 (* .1 taps)) initial-ball-speed))
    (define (ball-in-dir ball-pos dir)
      (+ ball-pos (make-polar (ball-speed taps) dir)))
    (define lhs-y-n (os/read* 'lhs-y 4.5))
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
            (gl:translate (psn-x ball-pos-n) (psn-y ball-pos-n)
                          (gl:rotate (* (/ 180 pi) ball-dir-n)
                                     gl:ball)))
      (append
       sounds
       (if new-ball?
         (list (cons 'sound (sound-at se:applause center-pos)))
         empty))))
    (unless (eq? score? 'right)
      (loop taps-n
            (ball-in-dir ball-pos-n ball-dir-n)
            ball-dir-n))))

(define (game-start)
  (big-bang/os
   width height center-pos
   #:sound-scale width-h
   (λ ()
     (os/thread player-paddle)
     (os/thread (ball initial-ball-speed))
     (os/write
      (list
       ;; XXX fix sound lambda
       (cons 'sound (background (λ (w) se:bgm) #:gain 0.1))
       (cons 'listener center-pos)))
     (let loop ([score 0])
       (define score?s (os/read 'score?))
       (define balls (length score?s))
       (define score-n (+ score (apply + (filter number? score?s))))
       (os/write
        (list
         (cons 'done? (zero? balls))
         (cons 'graphics/first
               (gl:seqn
                bgm
                (let ()
                  (printf "FPS: ~a\n"
                          (real->decimal-string
                           (current-rate) 1))
                  (define score-t
                    (gl:string->texture
                     #:size 30
                     (format "~a" score-n)))
                  (gl:translate
                   (- (psn-x center-pos) (/ (gl:texture-dw score-t) 2))
                   (- height (gl:texture-dh score-t))
                   (gl:seqn
                    (gl:color 1. 1. 1. 0.
                              (gl:rectangle (gl:texture-dw score-t)
                                            (gl:texture-dh score-t)))
                    (gl:color 0. 0. 0. 1.
                              (gl:texture score-t)))))))))
       (loop score-n)))))

(define game
  (game-info "Tennis!"
             game-start))

(provide game)
