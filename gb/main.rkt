#lang racket/base
(require (for-syntax racket/base)
         racket/runtime-path
         racket/list
         racket/match
         racket/date
         math/base
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

(struct ldata (game-id game-version level) #:prefab)
(struct replay (prng-state input) #:prefab)
(struct adata (play-session replay) #:prefab)

(define current-srs (make-parameter #f))

(define (game-cards id)
  (for/list ([c (in-list (srs-cards (current-srs)))]
             #:when (recent-card? c)
             #:when (and (ldata? (card-data c))
                         (equal? id (ldata-game-id (card-data c)))))
    c))

(define (recent-card? c)
  (match* ((card-id c) (card-data c))
    [(_ #f)
     #t]
    [(_ (ldata id version _))
     (equal? version (game-info-version (hash-ref game-code->info id)))]
    [(_ _)
     #f]))

(define (card-game c)
  (match* ((card-id c) (card-data c))
    [((? symbol? id) #f)
     id]
    [(_ (ldata id version _))
     id]))

(define (play-game gi)
  (match-define (game-info id name desc version generate start)
                gi)
  (define gcs (game-cards id))
  (define the-card
    (or (and (not (empty? gcs))
             (first gcs))
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
     (match-define (ldata id _ level) (card-data the-card))
     (match-define (game-info _ name _ version generate start)
                   (hash-ref game-code->info id))
     (define start-time (current-inexact-milliseconds))
     (define score (start level))
     (define end-time (current-inexact-milliseconds))
     (printf "~a,~a -> ~a\n"
             version level score)
     (srs-card-attempt!
      (current-srs) the-card
      ;; XXX The replay would go in this #f's spot
      (attempt start-time end-time score
               (adata
                (current-play-session)
                #f)))
     (void)]))

(define (attempt-length a)
  (- (attempt-end a) (attempt-start a)))
(define (stats-string l #:fmt [fmt real->decimal-string])
  (let/ec return
    (when (empty? l)
      (return "N/A"))
    (define min.v (apply min l))
    (define max.v (apply max l))
    (define mean.v (/ (apply + l) (length l)))
    (define median.v (list-ref (sort l <) (floor (/ (length l) 2))))
    (apply format "[~a/~a/~a/~a]"
           (map fmt
                (list min.v
                      median.v
                      mean.v
                      max.v)))))

(define (time-format ot)
  (define-values
    (t tu)
    (let/ec return
      (for/fold ([t ot]
                 [tu "ms"])
          ([n*u (in-list '((1000 "s") (60 "m") (60 "h")
                           (24 "d") (7 "w")
                           (52 "y")))])
        (match-define (list n u) n*u)
        (if (>= t n)
          (values (/ t n) u)
          (return t tu)))))
  (format "~a~a"
          (real->decimal-string t)
          tu))

(define (history->info-screen-list history)
  (list
   (format "    A: ~a"
           (length history))
   (format "Total: ~a"
           (time-format
            (sum (map attempt-length
                      history))))
   (format "    S: ~a"
           (stats-string (map attempt-score
                              history)))
   (format "    T: ~a"
           (stats-string (map attempt-length
                              history)
                         #:fmt time-format))
   (format "    L: ~a"
           (parameterize ([date-display-format
                           'iso-8601])
             (define attempts
               (sort history
                     <
                     #:key attempt-start))
             (cond
               [(empty? attempts)
                "N/A"]
               [else
                (define last (first attempts))
                (format "~a - ~a"
                        (adata-play-session (attempt-data last))
                        (date->string
                         (seconds->date
                          (/ (attempt-start last) 1000))
                         #t))])))))

(require gb/lib/meta-q)
(define (id->game-info-display id)
  (define g (hash-ref game-code->info id))
  (append (meta-q 48 (list* 
                      (format "~a - ~a"
                              (game-info-name g)
                              (first (game-info-desc g)))
                      (rest (game-info-desc g))))
          (list "")))

(define (go)
  (big-bang/os
   width height center-pos
   (λ (env)
     (win-write
      'sound (background (λ (w) se:bgm) #:gain 0.1))

     (define main
       (list
        (λ ()
          (define (attempt-this-session? a)
            (equal? (current-play-session)
                    (adata-play-session (attempt-data a))))
          (define these-cards
            (filter (λ (c)
                      (not (empty? (filter attempt-this-session? (card-history c)))))
                    (srs-cards (current-srs))))
          (define these-attempts
            (filter attempt-this-session?
                    (append-map card-history these-cards)))
          (menu:status
           (format "S: ~a G: ~a A: ~a T: ~a (Total: ~a)"
                   (current-play-session)
                   (length
                    (remove-duplicates
                     (map card-game
                          these-cards)))
                   (length
                    these-attempts)
                   (time-format
                    (sum
                     (map attempt-length
                          these-attempts)))
                   (time-format
                    (sum
                     (map attempt-length
                          (append-map card-history
                                      (srs-cards (current-srs)))))))))
        (menu:top
         'top
         (list
          (menu:option
           "Cards"
           (λ ()
             (menu:list
              'cards
              (for/list ([c (in-list (srs-cards (current-srs)))]
                         #:when (recent-card? c))
                (match-define (card id sort-score data history) c)
                (menu:option
                 (format "~a" id)
                 (λ ()
                   (list
                    (menu:status "Play the card?")
                    (menu:info
                     (append
                      (id->game-info-display
                       (match data
                         [#f id]
                         [(ldata game _ _) game]))
                     (list*
                      (format "    S: ~a"
                              (real->decimal-string sort-score))
                      (history->info-screen-list history))))
                    (menu:action (λ () (play-card c))))))))))
          (menu:option
           "Games"
           (λ ()
             (menu:list
              'games
              (for/list ([g (in-list games)])
                (menu:option
                 (game-info-name g)
                 (λ ()
                   (define history
                     (append-map card-history
                                 (game-cards
                                  (game-info-id g))))

                   (list (menu:status "Play the game?")
                         (menu:info 
                          (append
                           (id->game-info-display
                            (game-info-id g))
                           (history->info-screen-list history)))
                         (menu:action (λ () (play-game g))))))))))))))

     (render-menu #:back (λ () (win-exit 'done? #t))
                  main))))

(define current-play-session (make-parameter #f))

(module+ main
  (require racket/cmdline
           racket/file)

  (define-runtime-path user "../user")

  (define the-srs (srs (build-path user "srs.db")))

  (define play-session-pth (build-path user "play.session"))
  (define play-session
    (if (file-exists? play-session-pth)
      (add1 (file->value play-session-pth))
      0))
  (write-to-file play-session play-session-pth #:exists 'replace)

  (for ([gi (in-list games)])
    (match-define (game-info id _ _ version generate _) gi)
    (set-srs-generator! the-srs id
                        (λ () (values (ldata id version (generate))
                                      (adata play-session #f)))))

  (command-line
   #:program "get-bonus"
   #:args maybe-game
   (parameterize ([current-srs the-srs]
                  [current-play-session play-session])
     (call-with-gb
      (λ ()
        (match maybe-game
          [(list)
           (go)]
          [(list (app string->symbol some-game))
           (define gi
             (hash-ref game-code->info some-game
                       (λ ()
                         (error 'get-bonus "We know nothing about the game ~e"
                                some-game))))
           (play-game gi)]))))))
