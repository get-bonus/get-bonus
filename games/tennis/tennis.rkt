#lang racket/gui
(require racket/runtime-path
         "../../exp/loop.rkt"
         (prefix-in gl: 
                    (combine-in "../../exp/gl.rkt"
                                "../../exp/gl-ext.rkt"))
         "../../exp/sprites.rkt"
         "../../exp/mvector.rkt"
         "../../exp/fullscreen.rkt"
         "../../exp/keyboard.rkt"
         "../../exp/mapping.rkt"
         "../../exp/controller.rkt"
         "../../exp/joystick.rkt"
         "../../exp/3s.rkt"
         "../../exp/psn.rkt"
         (prefix-in cd: "../../exp/cd-narrow.rkt"))

(define-runtime-path resource-path "r")

(define-syntax-rule (define-sound id f)
  (define id (path->audio (build-path resource-path f))))

(define-sound se:applause "applause.wav")
(define-sound se:bgm "bgm.mp3")
(define-sound se:bump-lhs "bump-lhs.mp3")
(define-sound se:bump-rhs "bump-rhs.mp3")
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
(define ball
  (gl:translate (- ball-r) (- ball-r)
                (gl:texture/px ball-sprites
                               (* 2 ball-hw) (* 2 ball-hh)
                               484 683
                               36 24)))

(define bgm-img
  (gl:path->texture 
   (build-path resource-path "potosvillage.png")))
(define bgm
  (gl:texture/px bgm-img
                 width height
                 315 265
                 336 189))

(define lhs-x 
  (- .5 paddle-hw))
(define rhs-x 
  (- width .5 paddle-hw))

(struct game-st 
        (frame
         server
         lhs-score rhs-score
         lhs-y
         ball-pos ball-dir ball-target
         rhs-y))

(define frame-top
  (cd:aabb (+ center-pos (psn 0. height))
           width-h (/ height 2.)))
(define frame-bot
  (cd:aabb (- center-pos (psn 0. height))
           width-h (/ height 2.)))

(define (clamp bot x top)
  (max bot (min x top)))

(define (between lo hi)
  (+ lo (* (random) (- hi lo))))
(define (random-dir t)
  (case t
    [(left) 
     (between (* 2/3 pi) (* 4/3 pi))]
    [(right)
     (between (* 5/3 pi) (* 7/3 pi))]))
(define serve-dist
  (* 1.2 (+ ball-hw paddle-hw)))
(define (start-pos lhs-y rhs-y server)
  (case server
    [(right) 
     (psn (- rhs-x serve-dist) rhs-y)]
    [(left) 
     (psn (+ lhs-x serve-dist) lhs-y)]))
(define start-dir
  (match-lambda
    ['left 0.]
    ['right pi]))
(define opposite
  (match-lambda
    ['left 'right]
    ['right 'left]))

(define (game first-server)
  (big-bang
   (game-st 0
            first-server
            0 0
            4.5
            (start-pos 4.5 4.5 first-server)
            (start-dir first-server) (opposite first-server)
            4.5)
   #:sound-scale
   width-h
   #:tick
   (λ (w cs)
     (match-define (game-st 
                    frame server
                    lhs-score rhs-score
                    lhs-y
                    ball-pos ball-dir ball-tar
                    rhs-y)
                   w)
     (match-define 
      (list (and
             (app controller-a
                  lhs-serve?)
             (app controller-dpad
                  (app psn-y
                       lhs-dy)))
            (and
             (app controller-a
                  rhs-serve?)
             (app controller-dpad
                  (app psn-y
                       rhs-dy))))
      (if (= (length cs) 2)
          cs
          (list
           (first cs)
           (controller 
            (psn 0. 
                 ; Goes towards the ball's y position
                 (clamp -1.
                        (/ (- (psn-y ball-pos) rhs-y) speed)
                        1.))
            0. 0.
            ; Serves immediately
            (if (eq? server 'right)
                #t
                #f)
            #f #f #f 
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
     (define (ball-in-dir dir)
       (+ ball-pos (make-polar ball-speed dir)))
     (define ball-pos-m
       (if server
           (start-pos lhs-y-n rhs-y-n server)
           (ball-in-dir ball-dir)))
     
     (define ball-shape
       (cd:aabb ball-pos-m ball-hw ball-hh))
     (define lhs-shape
       (cd:aabb (psn (+ lhs-x paddle-hw) lhs-y-n)
                paddle-hw paddle-hh))
     (define rhs-shape
       (cd:aabb (psn (+ rhs-x paddle-hw) rhs-y-n)
                paddle-hw paddle-hh))
     
     (define-values
       (ball-pos-n+ ball-dir-n ball-tar-n server-p sounds)
       (cond
         [server
          (cond
            [(and (eq? server 'right) rhs-serve?)
             (values ball-pos-m ball-dir ball-tar #f
                     (list (sound-at se:bump-rhs ball-pos-m)))]
            [(and (eq? server 'left) lhs-serve?)
             (values ball-pos-m ball-dir ball-tar #f
                     (list (sound-at se:bump-lhs ball-pos-m)))]
            [else
             (values ball-pos-m ball-dir ball-tar server empty)])]
         [; The ball hit the top
          (cd:shape-vs-shape ball-shape frame-top)
          (values ball-pos
                  (case ball-tar
                    [(left) (between 3.2 4.2)]
                    [(right) (between 5.2 6.2)])
                  ball-tar #f
                  (list (sound-at se:bump-wall ball-pos-m)))]
         [; The ball hit the bot
          (cd:shape-vs-shape ball-shape frame-bot)
          (values ball-pos
                  (case ball-tar
                    [(left) (between 2.1 3.0)]
                    [(right) (between 0.2 1.1)])
                  ball-tar #f
                  (list (sound-at se:bump-wall ball-pos-m)))]
         [; The ball has bounced off the lhs
          (cd:shape-vs-shape ball-shape lhs-shape)
          (values ball-pos
                  (random-dir 'right) 'right #f
                  (list (sound-at se:bump-lhs ball-pos-m)))]
         [; The ball has bounced off the rhs
          (cd:shape-vs-shape ball-shape rhs-shape)
          (values ball-pos
                  (random-dir 'left) 'left #f
                  (list (sound-at se:bump-rhs ball-pos-m)))]
         ; The ball is inside the frame
         [else
          (values ball-pos-m ball-dir ball-tar #f empty)]))
     (define ball-pos-n
       (if (= ball-dir-n ball-dir)
           ball-pos-n+
           (ball-in-dir ball-dir-n)))
     
     (define-values
       (ball-pos-p ball-dir-p ball-tar-p
                   server-n lhs-score-n rhs-score-n score?)
       (cond
         ; The ball has moved to the left of the lhs paddle
         [((psn-x ball-pos-n) . < . lhs-x)
          (values (start-pos lhs-y rhs-y 'right) (start-dir 'left)
                  'right 'left lhs-score (add1 rhs-score) #t)]
         ; The ball has moved to the right of the rhs paddle
         [((psn-x ball-pos-n) . > . rhs-x)
          (values (start-pos lhs-y rhs-y 'left) (start-dir 'left)
                  'left 'right (add1 lhs-score) rhs-score #t)]
         [else
          (values ball-pos-n ball-dir-n ball-tar-n
                  server-p lhs-score rhs-score #f)]))
     
     (values 
      (game-st 
       (add1 frame) server-n
       lhs-score-n rhs-score-n
       lhs-y-n
       ball-pos-p ball-dir-p ball-tar-p
       rhs-y-n)
      (gl:focus 
       width height width height
       (psn-x center-pos) (psn-y center-pos)
       (gl:seqn
        bgm
        (let ()
          (define score-t
            (gl:string->texture 
             #:size 30 
             (format "~a : ~a" lhs-score-n rhs-score-n)))
          (gl:translate
           (- (psn-x center-pos) (/ (gl:texture-dw score-t) 2))
           (- height (gl:texture-dh score-t))
           (gl:seqn
            (gl:color 255 255 255 0
                      (gl:rectangle (gl:texture-dw score-t)
                                    (gl:texture-dh score-t)))
            (gl:texture score-t))))
        ; XXX Add a collision animation
        (gl:translate lhs-x (- lhs-y-n paddle-hh)
                      lhs-paddle)
        (gl:translate rhs-x (- rhs-y-n paddle-hh)
                      rhs-paddle)
        (gl:translate (psn-x ball-pos-p) (psn-y ball-pos-p)
                      (gl:rotate (* (/ 180 pi) ball-dir-p)
                                 ball))))
      ; XXX Make the ball whoosh
      ; XXX Make scores have calls
      (append
       sounds
       (if score?
           (list (sound-at se:applause center-pos))
           empty)
       (if (zero? frame)
           (list (background (λ (w) se:bgm) #:gain 0.1))
           empty))))
   #:listener
   (λ (w) center-pos)
   #:done?
   (λ (w)
     (match-define (game-st 
                    frame server
                    lhs-score rhs-score
                    lhs-y
                    ball-pos ball-dir ball-tar
                    rhs-y)
                   w)
     (and ((max lhs-score rhs-score) . >= . 4)
          ((abs (- lhs-score rhs-score)) . >= . 2)))))

; XXX change games to have a single server
; XXX implement sets (win at least 6 games and 2 more than oppt)
; XXX implement matches (best of three sets)

(struct GAME (frame bgm-started? last-game))

(define (text s)
  (gl:string->texture #:size 45 s))

(define-sound se:title "title.mp3")
(big-bang
 (GAME 0 #f #f)
 #:tick
 (λ (w cs)
   (match-define (GAME frame bgm-started? last-game) w)
   (define start?
     (ormap controller-start cs))
   (define last-game-n
     ; XXX Maybe have a select sound like http://www.freesound.org/samplesViewSingle.php?id=87035
     (if start?
         (game
          (case (random 2)
            [(0) 'left]
            [(1) 'right]))
         last-game))
   
   (values 
    (GAME (add1 frame) #t last-game-n)
    (gl:background
     255 255 255 0
     (gl:focus 
      16 9 16 9 0 0
      (gl:seqn
       (gl:center-texture-at
        (psn 8. 6.5)
        (text "Tennis!"))
       (match last-game-n
         [#f
          gl:blank]
         [(and (app game-st-lhs-score lhs)
               (app game-st-rhs-score rhs))
          (gl:center-texture-at
           (psn 8. 4.5)
           (text
            (if (lhs . > . rhs)
                "Player 1 won!"
                "Player 2 won!")))])
       (if (zero? (modulo frame 10))
           gl:blank
           (gl:center-texture-at
            (psn 8. 2.5)
            (text "Press START"))))))
    (if bgm-started?
        empty
        (list (background (λ (w) se:title)))))))