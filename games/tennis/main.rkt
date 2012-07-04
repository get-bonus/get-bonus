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
(define ball-speed
  (* 2. speed))

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
(define rhs-paddle
  (paddle-at 70))

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
(define rhs-x
  (- width .5 paddle-hw))

(define frame-top
  (cd:aabb (+ center-pos (psn 0. height))
           width-h (/ height 2.)))
(define frame-bot
  (cd:aabb (- center-pos (psn 0. height))
           width-h (/ height 2.)))

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

(define (won? at-least over lhs rhs)
  (and ((max lhs rhs) . >= . at-least)
       ((abs (- lhs rhs)) . >= . over)))

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

(define (ai-paddle)
  (let loop ([rhs-y 4.5])
    (define ball-pos
      (os/read* 'ball-pos ball-start-pos))
    (define rhs-dy
      (clamp -1.
             (/ (- (psn-y ball-pos) rhs-y) speed)
             1.))
    (define rhs-y-n
      (clamp
       min-paddle-y
       (+ rhs-y (* rhs-dy speed))
       max-paddle-y))
    (os/write
     (list
      (cons 'rhs-y rhs-y-n)
      (cons 'graphics
            (gl:translate rhs-x (- rhs-y-n paddle-hh)
                          rhs-paddle))))
    (loop rhs-y-n)))

(define (psn-clamp-x p min-x max-x)
  (psn (min max-x (max min-x (psn-x p))) (psn-y p)))

(define (ball-bounce dir mx my [dy 0.])
  (define p (make-polar 1.0 dir))
  (angle
   (make-rectangular (* mx (real-part p))
                     (+ (* -1. dy) (* my (imag-part p))))))

(define (ball)
  (let loop ([serving? #t]
             [ball-pos ball-start-pos]
             [ball-dir (ball-start-dir)])
    (define (ball-in-dir ball-pos dir)
      (+ ball-pos (make-polar ball-speed dir)))
    (define lhs-y-n (os/read* 'lhs-y 4.5))
    (define lhs-shape
      (cd:aabb (psn (+ lhs-x paddle-hw) lhs-y-n)
               paddle-hw paddle-hh))
    (define rhs-y-n (os/read* 'rhs-y 4.5))
    (define rhs-shape
      (cd:aabb (psn (+ rhs-x paddle-hw) rhs-y-n)
               paddle-hw paddle-hh))
    (define ball-pos-m ball-pos)
    (define ball-shape
      (cd:aabb ball-pos-m ball-hw ball-hh))
    (define-values
      (ball-pos-n ball-dir-n serving?-p sounds score?)
      (cond
        ;; The ball has moved to the left of the lhs paddle
        [((psn-x ball-pos-m) . < . lhs-x)
         (values ball-start-pos (ball-start-dir)
                 #t empty 'right)]
        ;; The ball has moved to the right of the rhs paddle
        [((psn-x ball-pos-m) . > . rhs-x)
         (values ball-start-pos (ball-start-dir)
                 #t empty 'left)]
        ;; We're serving
        [serving?
         (values ball-pos-m ball-dir #f
                 (list (cons 'sound (sound-at se:bump-lhs ball-pos-m)))
                 #f)]
        [; The ball hit the top
         (cd:shape-vs-shape ball-shape frame-top)
         (values ball-pos-m
                 (ball-bounce ball-dir 1.0 -1.0)
                 #f
                 (list (cons 'sound (sound-at se:bump-wall ball-pos-m)))
                 #f)]
        [; The ball hit the bot
         (cd:shape-vs-shape ball-shape frame-bot)
         (values ball-pos-m
                 (ball-bounce ball-dir 1.0 -1.0)
                 #f
                 (list (cons 'sound (sound-at se:bump-wall ball-pos-m)))
                 #f)]
        [; The ball has bounced off the lhs
         (cd:shape-vs-shape ball-shape lhs-shape)
         (values ball-pos-m
                 (ball-bounce ball-dir -1.0
                              (+ 1.0) (/ (- lhs-y-n (psn-y ball-pos-m)) paddle-hh))
                 #f
                 (list (cons 'sound (sound-at se:bump-lhs ball-pos-m)))
                 #f)]
        [; The ball has bounced off the rhs
         (cd:shape-vs-shape ball-shape rhs-shape)
         (values ball-pos-m
                 (ball-bounce ball-dir -1.0
                              (+ 1.0) (/ (- rhs-y-n (psn-y ball-pos-m)) paddle-hh))
                 #f
                 (list (cons 'sound (sound-at se:bump-rhs ball-pos-m)))
                 #f)]
        ;; The ball is inside the frame
        [else
         (values ball-pos-m ball-dir #f empty #f)]))
    (os/write
     (list*
      (cons 'score? score?)
      (cons 'ball-pos
            ball-pos-n)
      (cons 'graphics
            (gl:translate (psn-x ball-pos-n) (psn-y ball-pos-n)
                          (gl:rotate (* (/ 180 pi) ball-dir-n)
                                     gl:ball)))
      sounds))
    (loop serving?-p 
          ;;ball-pos-n
          (ball-in-dir ball-pos-n ball-dir-n)
          ball-dir-n)))

(define (game-start)
  (big-bang/os
   width height center-pos
   #:sound-scale width-h
   (λ ()
     (os/thread player-paddle)
     (os/thread ai-paddle)
     (os/thread ball)
     (os/write
      (list
       ;; XXX fix sound lambda
       (cons 'sound (background (λ (w) se:bgm) #:gain 0.1))
       (cons 'listener center-pos)))
     (let loop ([lhs-score 0]
                [rhs-score 0])
       (define score? (os/read* 'score?))
       (define-syntax-rule (define-score lhs-score-n left lhs-score)
         (define lhs-score-n
           (if (eq? 'left score?) (add1 lhs-score) lhs-score)))
       (define-score lhs-score-n left lhs-score)
       (define-score rhs-score-n right rhs-score)
       (os/write
        (append
         (list
          (cons 'done?
                (won? 4 2 lhs-score rhs-score))
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
                      (format "(~a:~a)" lhs-score-n rhs-score-n)))
                   (gl:translate
                    (- (psn-x center-pos) (/ (gl:texture-dw score-t) 2))
                    (- height (gl:texture-dh score-t))
                    (gl:seqn
                     (gl:color 1. 1. 1. 0.
                               (gl:rectangle (gl:texture-dw score-t)
                                             (gl:texture-dh score-t)))
                     (gl:color 0. 0. 0. 1.
                               (gl:texture score-t))))))))
         (if score?
           (list (cons 'sound (sound-at se:applause center-pos)))
           empty)))
       (loop lhs-score-n
             rhs-score-n)))))

(define game
  (game-info "Tennis!"
             game-start))

(provide game)
