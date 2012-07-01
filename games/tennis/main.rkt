#lang racket/base
(require racket/runtime-path
         racket/match
         racket/function
         racket/math
         racket/list
         gb/gui/world
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
         serving?
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

(define (won? at-least over lhs rhs)
  (and ((max lhs rhs) . >= . at-least)
       ((abs (- lhs rhs)) . >= . over)))

;; XXX Make the ball whoosh
;; XXX Make scores have calls

(define (game-start)
  ;; OS library
  (define kernel-prompt-tag (make-continuation-prompt-tag 'kernel))
  (define (run-process-until-syscall p)
    (call-with-continuation-barrier
     (λ ()
       (call-with-continuation-prompt
        (λ ()
          (p)
          (error 'kernel "Process did not run to system call"))
        kernel-prompt-tag
        (λ (x) x)))))
  (define (trap-syscall k->syscall)
    ;; First we capture our context back to the OS
    (call-with-current-continuation
     (λ (k)
       ;; Then we abort, give it to the OS, along with a syscall
       ;; specification
       (abort-current-continuation kernel-prompt-tag (k->syscall k)))
     kernel-prompt-tag))

  (define-syntax-rule
    (define-syscall (call k call-arg ...) state-args
      body ...)
    (define call
      (let ()
        (struct call (k call-arg ...)
                #:property prop:procedure
                (λ (the-struct . state-args)
                  (match-define (call k call-arg ...) the-struct)
                  body ...)
                #:transparent)
        (λ (call-arg ...)
          (trap-syscall
           (λ (k)
             (call k call-arg ...)))))))

  (define-syntax-rule
    (define-syscalls state-args
      [call-spec . body]
      ...)
    (begin
      (define-syscall call-spec state-args
        . body)
      ...))

  (define (snoc* beginning . end)
    (append beginning end))

  (struct process (pid k) #:transparent)
  (struct os (cur-heap next-heap cur-procs next-procs))

  (define-syscalls (pid current)
    [(os/read k id)
     (match-define (os cur-h next-h cur-ps next-ps) current)
     (os cur-h next-h
         (snoc* cur-ps
                (process pid (λ () (k (hash-ref cur-h id empty)))))
         next-ps)]
    [(os/write k id*vals)
     (match-define (os cur-h next-h cur-ps next-ps) current)
     (define next-h-n
       (for/fold ([next-h next-h])
           ([id*val (in-list id*vals)])
         (match-define (cons id val) id*val)
         (hash-update next-h id (curry cons val) (λ () empty))))
     (os cur-h next-h-n cur-ps
         (snoc* next-ps
                (process pid (λ () (k (void))))))]
    [(os/thread k t)
     (match-define (os cur-h next-h cur-ps next-ps) current)
     (define t-pid (gensym 'pid))
     (os cur-h next-h
         (snoc* cur-ps
                (process pid (λ () (k t-pid)))
                (process t-pid t))
         next-ps)])

  (define (os/read* k [def #f])
    (match (os/read k)
      [(list v)
       v]
      [else
       (if def
         def
         (error 'os/read* "~e does not have one value, has ~e" k else))]))

  (define boot
    (match-lambda
     [(os cur-h next-h (list) next-ps)
      (os next-h (hasheq) next-ps (list))]
     [(os cur-h next-h (list* (process pid now) cur-ps) next-ps)
      (define syscall (run-process-until-syscall now))
      (boot (syscall pid 
                     (os cur-h next-h cur-ps next-ps)))]))

  (define (big-bang/os #:sound-scale sound-scale
                       main-t)
    (big-bang
     (os (hasheq) (hasheq)
         (list (process (gensym 'pid) main-t))
         empty)
     #:sound-scale sound-scale
     #:tick
     (λ (w cs)
       (match-define (os cur-h next-h cur-ps next-ps) w)
       (define new-w 
         (boot
          (os (hash-set cur-h 'controller (list (first cs)))
              next-h cur-ps next-ps)))
       (match-define (os new-cur-h _ _ _) new-w)
       (values new-w
               (gl:focus
                width height width height
                (psn-x center-pos) (psn-y center-pos)
                (gl:background 0. 0. 0. 1.0
                               (apply gl:seqn
                                      (append
                                       (hash-ref new-cur-h 'graphics/first empty)
                                       (hash-ref new-cur-h 'graphics empty)))))
               (hash-ref new-cur-h 'sound empty)))
     #:listener
     (λ (w)
       ;; XXX
       center-pos)
     #:done?
     (λ (w)
       (match-define (os cur-h _ _ _) w)
       (first (hash-ref cur-h 'done? (list #f))))))

  ;; Use

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
        (os/read* 'ball-pos (start-pos 4.5 4.5 server)))
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

  (define (ball)
    (let loop ([serving? #t]
               [ball-pos (start-pos 4.5 4.5 server)]
               [ball-dir (start-dir server)]
               [ball-tar (opposite server)])
      (define (ball-in-dir dir)
        (+ ball-pos (make-polar ball-speed dir)))
      (define (ball-bounce dir mx my [dy 0.])
        (define p (make-polar 1.0 dir))
        (angle 
         (make-rectangular (* mx (real-part p))
                           (+ (* -1. dy) (* my (imag-part p))))))
      (define lhs-y-n (os/read* 'lhs-y 4.5))
      (define lhs-shape
        (cd:aabb (psn (+ lhs-x paddle-hw) lhs-y-n)
                 paddle-hw paddle-hh))
      (define rhs-y-n (os/read* 'rhs-y 4.5))
      (define rhs-shape
        (cd:aabb (psn (+ rhs-x paddle-hw) rhs-y-n)
                 paddle-hw paddle-hh))
      (define rhs-serve? #t)
      (define lhs-serve? #t)
      (define ball-pos-m
        (if serving?
          (start-pos lhs-y-n rhs-y-n server)
          (ball-in-dir ball-dir)))
      (define ball-shape
        (cd:aabb ball-pos-m ball-hw ball-hh))
      (define-values
        (ball-pos-n+ ball-dir-n ball-tar-n serving?-p sounds)
        (cond
          [serving?
           (cond
             [(and (eq? server 'right) rhs-serve?)
              (values ball-pos-m ball-dir ball-tar #f
                      (list (cons 'sound (sound-at se:bump-rhs ball-pos-m))))]
             [(and (eq? server 'left) lhs-serve?)
              (values ball-pos-m ball-dir ball-tar #f
                      (list (cons 'sound (sound-at se:bump-lhs ball-pos-m))))]
             [else
              (values ball-pos-m ball-dir ball-tar serving? empty)])]
          [; The ball hit the top
           (cd:shape-vs-shape ball-shape frame-top)
           (values ball-pos
                   (ball-bounce ball-dir 1.0 -1.0)
                   ball-tar #f
                   (list (cons 'sound (sound-at se:bump-wall ball-pos-m))))]
          [; The ball hit the bot
           (cd:shape-vs-shape ball-shape frame-bot)
           (values ball-pos
                   (ball-bounce ball-dir 1.0 -1.0)
                   ball-tar #f
                   (list (cons 'sound (sound-at se:bump-wall ball-pos-m))))]
          [; The ball has bounced off the lhs
           (cd:shape-vs-shape ball-shape lhs-shape)
           (values ball-pos 
                   (ball-bounce ball-dir -1.0 
                                (+ 1.0) (/ (- lhs-y-n (psn-y ball-pos-m)) paddle-hh))                   
                   'right #f
                   (list (cons 'sound (sound-at se:bump-lhs ball-pos-m))))]
          [; The ball has bounced off the rhs
           (cd:shape-vs-shape ball-shape rhs-shape)
           (values ball-pos
                   (ball-bounce ball-dir -1.0 
                                (+ 1.0) (/ (- rhs-y-n (psn-y ball-pos-m)) paddle-hh))
                   #;(ball-bounce ball-dir -1.0 1.0)
                   'left #f
                   (list (cons 'sound (sound-at se:bump-rhs ball-pos-m))))]
          ;; The ball is inside the frame
          [else
           (values ball-pos-m ball-dir ball-tar #f empty)]))
      (define ball-pos-n
        (if (= ball-dir-n ball-dir)
          ball-pos-n+          
          ;; XXX Don't know if this is right spot... something weird
          ;; happened once
          (psn-clamp-x 
           (ball-in-dir ball-dir-n)
           lhs-x rhs-x)))
      (define-values
        (ball-pos-p ball-dir-p ball-tar-p
                    serving?-n score?)
        (cond
          ;; The ball has moved to the left of the lhs paddle
          [((psn-x ball-pos-n) . < . lhs-x)
           (values (start-pos lhs-y-n rhs-y-n server) (start-dir server)
                   (opposite server) #t 'right)]
          ;; The ball has moved to the right of the rhs paddle
          [((psn-x ball-pos-n) . > . rhs-x)
           (values (start-pos lhs-y-n rhs-y-n server) (start-dir server)
                   (opposite server) #t 'left)]
          [else
           (values ball-pos-n ball-dir-n ball-tar-n
                   serving?-p #f)]))
      (os/write
       (list*
        (cons 'score? score?)
        (cons 'ball-pos
              ball-pos-p)
        (cons 'graphics
              (gl:translate (psn-x ball-pos-p) (psn-y ball-pos-p)
                            (gl:rotate (* (/ 180 pi) ball-dir-p)
                                       gl:ball)))
        sounds))
      (loop serving?-p ball-pos-p ball-dir-p ball-tar-p)))

  (big-bang/os
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

(define server 'left)

(define game
  (game-info "Tennis!"
             game-start))

(provide game)
