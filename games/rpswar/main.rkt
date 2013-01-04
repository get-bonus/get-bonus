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
         math/base
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
  (define (string next text status [auto #f])
    (menu:option
     text
     (λ ()
       (list* (menu:status status)
              (menu:action (λ () (return next)))
              (if auto
                (list (menu:auto 'next (λ () (return next))))
                empty)))))
  (define (next-menu next)
    (menu:list (list (string next "Next" "Advance to next message." 'next))))
  (match w
    [(start match# ai)
     (define next (user-input match# ai 1 0 (fst-start ai)))
     (list
      (menu:info (list (format "Match: ~a" match#)
                       (format "Fight!")))
      (next-menu next))]
    [(user-input match# ai round# wins state)
     (define (next ui)
       (computer-input match# ai round# wins state ui))
     (list
      (menu:info (list (format "Match: ~a" match#)
                       (format "Round: ~a" round#)
                       (format "Ratio: ~a/~a" wins round#)
                       ""
                       (format-fst ai state)
                       ""
                       (format "What will you throw down?")))
      (menu:list (for/list ([ui (in-list '(r p s))])
                   (string (next ui)
                           (rps->string ui)
                           (format "Throw down a ~a"
                                   (rps->string ui))))))]
    [(computer-input match# ai round# wins state ui)
     (define next
       (resolve match# ai round# wins state ui (fst-output ai state)))
     (list
      (menu:info (list (format "Match: ~a" match#)
                       (format "Round: ~a" round#)
                       (format "Ratio: ~a/~a" wins round#)
                       (format "You threw down ~a" (rps->string ui))))
      (next-menu next))]
    [(resolve match# ai round# wins state ui ci)
     (define next
       (resolved match# ai round# wins state ui ci
                 (rps-outcome ui ci)))
     (list
      (menu:info (list (format "Match: ~a" match#)
                       (format "Round: ~a" round#)
                       (format "Ratio: ~a/~a" wins round#)
                       (format "You threw down ~a" (rps->string ui))
                       (format "The AI threw down ~a" (rps->string ci))))
      (next-menu next))]
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
     (list
      (menu:info (list (format "Match: ~a" match#)
                       (format "Round: ~a" round#)
                       (format "Ratio: ~a/~a" wins round#)
                       (match outcome
                         ['user (format "You won the round!")]
                         ['computer (format "The AI won the round!")]
                         ['draw (format "Draw")])))
      (next-menu next))]
    [(end match# _ round# wins _)
     (list
      (menu:info (list (format "Match: ~a" match#)
                       (format "Round: ~a" round#)
                       (format "Ratio: ~a/~a" wins round#)
                       (format "You won the match!")))
      (next-menu same))]))

(define (repeat-n n f a)
  (if (zero? n)
    a
    (repeat-n (sub1 n) f (f a))))

(define (game-start)
  (big-bang/os
   crt-width crt-height (psn (/ crt-width 2.) (/ crt-height 2.0))
   #:sound-scale (/ crt-width 2.)
   (λ ()
     (define start-fst (random-one-state-fst '(r p s) '(r p s)))

     (define final-ai
       (let loop ([ai start-fst])
         (define next-ai (repeat-n (random-integer 1 10) mutate-fst ai))
         (if (zero? (random 2))
           next-ai
           (loop next-ai))))

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
  (game-info 'rpswar "RPS War"
             2 random-generate
             (random-start game-start)))

(provide game)
