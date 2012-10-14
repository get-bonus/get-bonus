#lang racket/base
(require (for-syntax racket/base)
         racket/runtime-path
         racket/list
         racket/match
         gb/audio/3s
         gb/data/psn
         gb/gui/world
         gb/meta
         gb/input/controller
         gb/graphics/r
         gb/graphics/ngl-main)

(begin-for-syntax
  (require racket/runtime-path)
  (define-runtime-path games-path "../games"))

(define-syntax (static-games stx)
  (syntax-case stx ()
    [(_)
     (with-syntax
         ([((game-code game/main) ...)
           (for/list ([g (in-list (directory-list games-path))])
             (list (path->string g)
                   (path->string (build-path "../games" g "main.rkt"))))])
       (syntax/loc stx
         (make-hash
          (list
           (cons game-code
                 (let ()
                   (local-require (only-in game/main game))
                   game))
           ...))))]))

(define-runtime-path texture-atlas-path "../r.png")

(define game-code->info
  (static-games))
(define games
  (hash-values game-code->info))

(struct game-st (bgm? input-delay pos))

(define-runtime-path resource-path "../games/tennis/r")
(define-syntax-rule (define-sound id f)
  (define id (path->audio (build-path resource-path f))))
(define-sound se:bgm "title.ogg")

(define width 16.0)
(define height 9.0)
(define center-pos
  (psn 8.0 4.5))

(define (snoc l x)
  (append l (list x)))

(define (go)
  (define string->sprites
    (make-string-factory (make-char-factory modern 12)))

  (define (pointer)
    (string->sprites "->" #:hw 0.5 #:hh 0.5))
  (define (menu pos)
    (cons
     (transform
      #:dy (+ 0.5 (* 1.0 pos))
      (pointer))
     (for/list ([g (in-list games)]
                [i (in-naturals)])
       (transform
        #:d 2.0 (+ 0.5 (* 1.0 i))
        (string->sprites (game-info-name g) #:hw 0.5 #:hh 0.5)))))

  (define draw #f)

  (big-bang
   (game-st #f 0 0)
   #:sound-scale
   width
   #:tick
   (λ (w cs)
     (match-define (game-st bgm? delay pos) w)
     (match-define (list* c _) cs)
     (define mod
       (cond
         [(controller-up c)   +1]
         [(controller-down c) -1]
         [else                 0]))
     (define-values
       (delay+ pos+)
       (cond
         [(positive? delay)
          (values (sub1 delay) pos)]
         [(zero? mod)
          (values 0 pos)]
         [else
          (values 8 ;; How many frames to wait for more input
                  (modulo (+ pos mod)
                          (length games)))]))
     (when (controller-start c)
       ((game-info-start (list-ref games pos+))))
     (values
      (game-st #t delay+ pos+)
      (λ ()
        (unless draw
          (set! draw (make-draw texture-atlas-path
                                texture-atlas-size
                                16.0 9.0)))

        (draw (menu pos+)))
      (if bgm?
        empty
        (list (background (λ (w) se:bgm) #:gain 0.1)))))
   #:listener
   (λ (w)
     center-pos)
   #:done?
   (λ (w)
     #f)))

(module+ main
  (require racket/cmdline)

  (command-line
   #:program "get-bonus"
   #:args maybe-game
   (match maybe-game
     [(list)
      (go)]
     [(list some-game)
      (define gi
        (hash-ref game-code->info some-game
                  (λ ()
                    (error 'get-bonus "We know nothing about the game ~e"
                           some-game))))
      ((game-info-start gi))])))
