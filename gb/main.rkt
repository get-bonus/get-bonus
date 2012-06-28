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

;; Before and after are like a zipper representing where we are in the
;; menu.
(struct game-st (bgm? before after))

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
  (define (gl:menu-part games)
    (apply gl:above
           (for/list ([g (in-list games)])
             (gl:texture
              (gl:string->texture #:size 30 (game-info-name g))))))
  (define gl:pointer
          (gl:texture
           (gl:string->texture #:size 60 "→")))
  (define (gl:menu before after)
    (gl:color
     1. 1. 1. 1.
     (gl:above
      (gl:translate
       1.0 0.0
       (gl:menu-part before))
      (gl:seqn
       gl:pointer
       (gl:translate
        1.0 0.0
        (gl:menu-part after))))))

  (big-bang
   (game-st #f empty games)
   #:sound-scale
   width
   #:tick
   (λ (w cs)
     ;; XXX Errors: (1) cursor moves too fast (2) can end up on an
     ;; empty spot (3) starts at the bottom, rather than the top
     (match-define (game-st bgm? before after) w)
     (match-define (list* c _) cs)
     (define-values (before+ after+)
       (match (controller-dpad-y c)
         [(? negative?)
          (match after
            [(list)
             (values empty before)]
            [(list* last after+)
             ;; XXX Slow
             (values (snoc before last) after+)])]
         [(? positive?)
          (cond
            [(empty? before)
             (values after empty)]
            [else
             ;; XXX Slow!!!!
             (match-define (cons next before+/r) (reverse before))
             (define before+ (reverse before+/r))
             (values before+ (cons next after))])]
         [_
          (values before after)]))
     (values
      (game-st #t before+ after+)
      (gl:focus
       width height width height
       (psn-x center-pos) (psn-y center-pos)
       (gl:background 
        0. 0. 0. 1.
        (gl:menu before+ after+)))
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
