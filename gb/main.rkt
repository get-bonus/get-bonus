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
         (prefix-in gl:
                    (combine-in gb/graphics/gl
                                gb/graphics/gl-ext)))

(begin-for-syntax
  (require racket/runtime-path)
  (define-runtime-path games-path "../games"))

(define-syntax (static-games stx)
  (syntax-case stx ()
    [(_)
     (with-syntax
         ([(game/main ...)
           (for/list ([g (in-list (directory-list games-path))])
             (path->string (build-path "../games" g "main.rkt")))])
       (syntax/loc stx
         (list (let ()
                 (local-require (only-in game/main game))
                 game)
               ...)))]))

(define games
  (static-games))

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
  (define gl:pointer
    (gl:texture
     (gl:string->texture #:size 60 "→")))
  (define (gl:menu pos)
    (gl:color
     1. 1. 1. 1.
     (gl:above
      (apply gl:above
             (for/list ([g (in-list games)]
                        [i (in-naturals)])
               (define t
                 (gl:translate
                  1.0 0.0
                  (gl:texture
                   (gl:string->texture #:size 30 (game-info-name g)))))
               (if (= pos i)
                 (gl:seqn gl:pointer t)
                 t))))))

  (big-bang
   (game-st #f 0 0)
   #:sound-scale
   width
   #:tick
   (λ (w cs)
     (match-define (game-st bgm? delay pos) w)
     (match-define (list* c _) cs)
     (define mod
       (match (controller-dpad-y c)
         [(? positive?) -1]
         [(? negative?) +1]
         [_              0]))
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
      (gl:focus
       width height width height
       (psn-x center-pos) (psn-y center-pos)
       (gl:background
        0. 0. 0. 1.
        (gl:menu pos+)))
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
  (go))
