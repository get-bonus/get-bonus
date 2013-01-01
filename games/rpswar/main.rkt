#lang racket/base
(require racket/runtime-path
         racket/match
         racket/list
         gb/gui/os
         gb/graphics/ngl-main
         gb/data/mvector
         gb/input/keyboard
         gb/input/controller
         gb/audio/3s
         gb/lib/math
         gb/lib/random
         gb/data/psn
         gb/meta
         gb/sys/menu
         (prefix-in cd: gb/physics/cd-narrow)
         "fst.rkt"
         "graph.rkt")

(struct a-match (match-number ai-spec) #:transparent)
(struct start a-match () #:transparent)
(struct round a-match (round-number wins ai-state) #:transparent)
(struct user-input round () #:transparent)
(struct computer-input round (user-input) #:transparent)
(struct resolve round (user-input computer-input) #:transparent)
(struct resolved round (user-input computer-input outcome) #:transparent)
(struct end round () #:transparent)

(define rps-outcome
  (match-lambda*
   ['(r s) 'user]
   ['(s r) 'computer]
   ['(r p) 'computer]
   ['(p r) 'user]
   ['(p s) 'computer]
   ['(s p) 'user]
   [_ 'draw]))

(define rps->string
  (match-lambda
   ['r "Rock"]
   ['p "Paper"]
   ['s "Scissors"]))

(define graphs (make-weak-hash))
(define (fst-graph* fst)
  (hash-ref! graphs fst (λ () (fst-graph fst))))

(define (state->menu return w)
  (define same w)
  (define (string next fmt . args)
    (menu:option (apply format fmt args)
                 (λ () (return next))))
  (match w
    [(start match# ai)
     (define next (user-input match# ai 1 0 (fst-start ai)))
     (menu:list (list (string next "Match: ~a" match#)
                      (string next "Fight!")))]
    [(user-input match# ai round# wins state)
     (define (next ui)
       (computer-input match# ai round# wins state ui))
     (menu:list (list* (string same "Match: ~a" match#)
                       (string same "Round: ~a" round#)
                       (string same "Ratio: ~a/~a" wins round#)
                       (string same "What will you throw down?")
                       (for/list ([ui (in-list '(r p s))])
                         (string (next ui) (rps->string ui)))))]
    [(computer-input match# ai round# wins state ui)
     (define next
       (resolve match# ai round# wins state ui (fst-output ai state)))
     (menu:list (list  (string next "Match: ~a" match#)
                       (string next "Round: ~a" round#)
                       (string next "Ratio: ~a/~a" wins round#)
                       (string next "You threw down ~a" (rps->string ui))))]
    [(resolve match# ai round# wins state ui ci)
     (define next
       (resolved match# ai round# wins state ui ci (rps-outcome ui ci)))
     (menu:list (list (string next "Match: ~a" match#)
                      (string next "Round: ~a" round#)
                      (string next "Ratio: ~a/~a" wins round#)
                      (string next "You threw down ~a" (rps->string ui))
                      (string next "The AI threw down ~a" (rps->string ci))))]
    [(resolved match# ai round# wins state ui ci outcome)
     (define total-wins
       (if (eq? 'user outcome)
         (add1 wins)
         wins))
     (define next
       (if ((ceiling (/ round# 2)) . < . total-wins)
         (end match# ai round# total-wins state)
         (let ()
           (define next-state
             (fst-next ai state ui))
           (user-input match# ai
                       (if (eq? outcome 'draw)
                         round#
                         (add1 round#))
                       total-wins next-state))))
     (menu:list (list (string next "Match: ~a" match#)
                      (string next "Round: ~a" round#)
                      (string next "Ratio: ~a/~a" wins round#)
                      (match outcome
                        ['user (string next "You won the round!")]
                        ['computer (string next "The AI won the round!")]
                        ['draw (string next "Draw")])))]
    [(end match# _ round# wins _)
     (define next same)
     (menu:list (list (string next "Match: ~a" match#)
                      (string next "Round: ~a" round#)
                      (string next "Ratio: ~a/~a" wins round#)
                      (string next "You won the match!")))]))

(define (game-start)
  (big-bang/os
   crt-width crt-height (psn (/ crt-width 2.) (/ crt-height 2.0))
   #:sound-scale (/ crt-width 2.)
   (λ ()
     (define start-fst (random-one-state-fst '(r p s) '(r p s)))

     (define final-ai
       (let loop ([ai start-fst])
         (if (zero? (random 2))
           ai
           (loop (mutate-fst ai)))))

     (let loop ([s (start 1 final-ai)])
       (define ns
         (let/ec return
           (render-menu (state->menu return s))))

       (when (end? ns)
         (os/write
          (list
           (cons 'done?
                 #t)
           (cons 'return
                 (/ (round-wins ns)
                    (round-round-number ns))))))

       (loop ns)))))

(define game
  (game-info 'rpswar "Rock-Paper-Scissors Warrior"
             1 random-generate
             (random-start game-start)))

(provide game)
