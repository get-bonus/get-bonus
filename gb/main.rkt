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
         gb/graphics/ngl-main
         gb/lib/srs
         gb/sys/menu)

(begin-for-syntax
  (require racket/runtime-path)
  (define-runtime-path games-path "../games"))

(define-syntax (static-games stx)
  (syntax-case stx ()
    [(_)
     (with-syntax
         ([((game-code game/main) ...)
           (for/list ([g (in-list (directory-list games-path))])
             (list (string->symbol (path->string g))
                   (path->string (build-path "../games" g "main.rkt"))))])
       (syntax/loc stx
         (make-immutable-hash
          (list
           (cons 'game-code
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

(define current-srs (make-parameter #f))

(define (play-game gi)
  (match-define (game-info id name version generate start)
                gi)
  (define the-card
    (or (for/or ([c (in-list (srs-cards (current-srs)))])
          (match (card-data c)
            [(vector (== id) (== version) c-level)
             c]
            [_
             #f]))
        (srs-generate! (current-srs) id
                       current-inexact-milliseconds)))
  (play-card the-card))

(define (play-card the-card)
  (match (card-id the-card)
    [(? symbol? id)
     (play-card
      (srs-generate! (current-srs) id
                     current-inexact-milliseconds))]
    [_
     (match-define (vector id _ level) (card-data the-card))
     (match-define (game-info _ name version generate start)
                   (hash-ref game-code->info id))
     (define start-time (current-inexact-milliseconds))
     (define score (start level))
     (define end-time (current-inexact-milliseconds))
     (printf "~a,~a -> ~a\n"
             version level score)
     (srs-card-attempt! (current-srs) the-card
                        ;; XXX The replay would go in this #f's spot
                        (attempt start-time end-time score #f))
     (void)]))

(define (go)
  (big-bang/os
   width height center-pos
   (λ ()
     (define main
       (menu:music
        (background (λ (w) se:bgm) #:gain 0.1)
        (menu:modal
         (list
          (cons
           "Games"
           (menu:list
            (for/list ([g (in-list games)])
              ;; XXX Display more info (game like, last play, etc)
              (menu:option (game-info-name g) (λ () (play-game g))))))
          (cons
           "Cards"
           (menu:list
            ;; XXX make this refresh when you come back from a game
            (for/list ([c (in-list (srs-cards (current-srs)))])
              ;; XXX Display more info (data, history, etc)
              (menu:option (format "~v: ~v" (card-id c) (card-data c))
                           (λ () (play-card c))))))))))

     (render-menu main))))

(module+ main
  (require racket/cmdline)

  (define-runtime-path srs-path "../srs.db")
  (define the-srs (srs srs-path))

  (for ([gi (in-list games)])
    (match-define (game-info id _ version generate _) gi)
    (set-srs-generator! the-srs id
                        (λ () (vector id version (generate)))))

  (command-line
   #:program "get-bonus"
   #:args maybe-game
   (parameterize ([current-srs the-srs])
     (match maybe-game
       [(list)
        (go)]
       [(list (app string->symbol some-game))
        (define gi
          (hash-ref game-code->info some-game
                    (λ ()
                      (error 'get-bonus "We know nothing about the game ~e"
                             some-game))))
        (play-game gi)]))))
