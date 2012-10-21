#lang racket/base
(require (for-syntax racket/base)
         racket/runtime-path
         racket/list
         racket/match
         gb/audio/3s
         gb/data/psn
         gb/gui/os
         gb/meta
         gb/input/controller
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

(define game-code->info
  (static-games))
(define games
  (hash-values game-code->info))

(define-runtime-path resource-path "../games/tennis/r")
(define-syntax-rule (define-sound id f)
  (define id (path->audio (build-path resource-path f))))
(define-sound se:bgm "title.ogg")

(define width crt-width)
(define height crt-height)
(define center-pos
  (psn (/ width 2.0)
       (/ height 2.0)))

(define (snoc l x)
  (append l (list x)))

(define (go)
  (define modern-12-char
    (make-char-factory modern 12))
  (define char-height
    (texture-height (modern-12-char #\a)))
  (define char-width
    (texture-width (modern-12-char #\a)))
  (define string->sprites
    (make-string-factory modern-12-char))

  (define (menu-entry-height pos)
    (+ (/ char-height 2.0) (* char-height pos)))
  (define menu
    (for/list ([g (in-list games)]
               [i (in-naturals)])
      (transform
       #:d (* 2.0 char-width) (menu-entry-height i)
       (string->sprites (game-info-name g)))))

  (big-bang/os
   width height center-pos
   #:sound-scale width
   (λ ()
     (os/write
      (list (cons 'sound (background (λ (w) se:bgm) #:gain 0.1))))

     (let loop ([delay 0]
                [pos 0])
       (define c (os/read* 'controller))
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

       (os/write
        (list
         (cons 'graphics
               (cons 0
                     (cons
                      menu
                      (transform
                       #:dy (menu-entry-height pos)
                       (string->sprites ">>")))))))

       (loop delay+ pos+)))))

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
