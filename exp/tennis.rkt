#lang racket/gui
(require racket/runtime-path
         "loop.rkt"
         (prefix-in gl: "gl.rkt")
         "sprites.rkt"
         "mvector.rkt"
         "fullscreen.rkt"
         "keyboard.rkt"
         "mapping.rkt"
         "controller.rkt"
         "joystick.rkt"
         "3s.rkt"
         "psn.rkt"
         (prefix-in cd: "cd-narrow.rkt"))

(define-runtime-path resource-path "../resources")

; XXX Get different music/sound effects
(define bgm 
  (path->audio 
   (build-path resource-path 
               "SMB-1-1.mp3")))
(define jump-se
  (path->audio
   (build-path resource-path 
               "SMB-SE-Jump.wav")))

(define width 16.)
(define height 9.)
(define center-pos
  (psn (/ width 2.) (/ height 2.)))
(define speed 
  (* 4. RATE))

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

(define paddle
  (gl:rectangle paddle-w paddle-h))

(define ball-r .25)
(define ball
  (gl:color 0 255 0 0
            (gl:scale ball-r ball-r (gl:circle))))

(define lhs-x 
  (- .5 (/ paddle-w 2)))
(define rhs-x 
  (- width .5 (/ paddle-w 2)))

(struct world (frame lhs-score rhs-score
                     lhs-y
                     ball-pos ball-dir
                     rhs-y))

(define frame-shape
  (cd:aabb center-pos (/ width 2.) (/ height 2.)))

(define (clamp bot x top)
  (max bot (min x top)))

(define (random-dir)
  (* (random) 2. pi))

(big-bang
 (world 0
        0 0
        4.5
        center-pos
        (random-dir)
        4.5)
 #:tick
 (λ (w cs)
   (match-define (world 
                  frame lhs-score rhs-score
                  lhs-y
                  ball-pos ball-dir
                  rhs-y)
                 w)
   (match-define 
    (list (app controller-dpad
               (app psn-y
                    lhs-dy))
          (app controller-dpad
               (app psn-y
                    rhs-dy)))
    (if (= (length cs) 2)
        cs
        (list (first cs)
              (controller (psn 0. 
                               ; XXX This is too hard
                               ; Goes towards the ball's y position
                               (* 1. (sgn (- (psn-y ball-pos) rhs-y))))
                          0. 0.
                          #f #f #f #f 
                          #f #f #f #f #f #f))))
   
   (define lhs-y-n
     (clamp
      min-paddle-y
      (+ lhs-y (* lhs-dy speed))
      max-paddle-y))
   (define rhs-y-n
     (clamp
      min-paddle-y
     (+ rhs-y (* rhs-dy speed))
      max-paddle-y))
   (define ball-pos-m
     (+ ball-pos (make-polar speed ball-dir)))
   
   (define ball-shape
     (cd:circle ball-pos-m ball-r))
   (define lhs-shape
     (cd:aabb (psn lhs-x lhs-y-n) paddle-hw paddle-hh))
   (define rhs-shape
     (cd:aabb (psn rhs-x rhs-y-n) paddle-hw paddle-hh))
   
   (define-values
     (ball-pos-n ball-dir-n)
     (cond
       [(or 
         ; The ball has bounced outside
         (not (cd:shape-vs-shape ball-shape frame-shape))
         ; The ball has bounced off the lhs
         (cd:shape-vs-shape ball-shape lhs-shape)
         ; The ball has bounced off the rhs
         (cd:shape-vs-shape ball-shape rhs-shape))
        (values ball-pos
                ; XXX Bounce better
                (+ ball-dir pi (* (random) 1/2 pi)))]
       ; The ball is inside the frame
       [else
        (values ball-pos-m ball-dir)]))
   
   ; XXX Maybe I should implement serving?
   (define-values
     (ball-pos-p ball-dir-p lhs-score-n rhs-score-n)
     (cond
       ; The ball has moved to the left of the lhs paddle
       [((psn-x ball-pos-n) . < . lhs-x)
        (values center-pos (random-dir) lhs-score (add1 rhs-score))]
       ; The ball has moved to the right of the rhs paddle
       [((psn-x ball-pos-n) . > . rhs-x)
        (values center-pos (random-dir) (add1 lhs-score) rhs-score)]
       [else
        (values ball-pos-n ball-dir-n lhs-score rhs-score)]))
   
   (values 
    (world 
     (add1 frame)
     lhs-score-n rhs-score-n
     lhs-y-n
     ball-pos-p ball-dir-p
     rhs-y-n)
    (gl:focus 
     width height width height
     (psn-x center-pos) (psn-y center-pos)
     (gl:background
      255 255 255 0
      ; XXX Place the scores better
      (gl:translate (* width 1/4) (* height 8/9)
                    (gl:text #:size 30 (format "~a" lhs-score-n)))
      (gl:translate (* width 3/4) (* height 8/9)
                    (gl:text #:size 30 (format "~a" rhs-score-n)))
      (gl:translate lhs-x (- lhs-y paddle-hh)
                    (gl:color 255 0 0 0
                              paddle))
      (gl:translate rhs-x (- rhs-y paddle-hh)
                    (gl:color 0 0 255 0
                              paddle))
      ; XXX Animate the ball
      (gl:translate (psn-x ball-pos-p) (psn-y ball-pos-p)
                    ball)))
    ; XXX Make the ball whoosh
    ; XXX Make collisions ping
    ; XXX Make scores have calls
    ; XXX Make scores have a winging sound
    (if (zero? frame)
        (list (background (λ (w) bgm) #:gain 0.8))
        empty)))
 #:listener
 (λ (w) center-pos)
 #:done?
 ; XXX Look at score
 (λ (w) #f))
