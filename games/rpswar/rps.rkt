#lang racket

#|

Press any key to advance the game.
Press r for Rock.
Press p for Paper.
Press s for Scissors.

|#

(require "fst.rkt"
         "graph.rkt"
         2htdp/universe
         2htdp/image)

(struct a-match (match-number ai-spec) #:transparent)
(struct start a-match () #:transparent)
(struct round a-match (round-number wins ai-state) #:transparent)
(struct user-input round () #:transparent)
(struct computer-input round (user-input) #:transparent)
(struct resolve round (user-input computer-input) #:transparent)
(struct resolved round (user-input computer-input outcome) #:transparent)
(struct end round () #:transparent)

(define start-fst (random-one-state-fst '(r p s) '(r p s)))

(define rps-outcome
  (match-lambda*
    ['(r s) 'user]
    ['(s r) 'computer]
    ['(r p) 'computer]
    ['(p r) 'user]
    ['(p s) 'computer]
    ['(s p) 'user]
    [_ 'draw]))

(define tick
  (match-lambda
    [(start match# ai)
     (user-input match# ai 1 0 (fst-start ai))]
    [(computer-input match# ai round# wins state ui)
     (resolve match# ai round# wins state ui (fst-output ai state))]
    [(resolve match# ai round# wins state ui ci)
     (resolved match# ai round# wins state ui ci (rps-outcome ui ci))]
    [(resolved match# ai round# wins state ui ci outcome)
     (define total-wins
       (if (eq? 'user outcome)
           (add1 wins)
           wins))
     (if ((ceiling (/ round# 2)) . < . total-wins)
         (end match# ai round# total-wins state)
         (let ()
           (define next-state
             (fst-next ai state ui))
           (user-input match# ai
                       (if (eq? outcome 'draw)
                           round#
                           (add1 round#)) 
                       total-wins next-state)))]
    [(end match# ai _ _ _)
     (start (add1 match#) (mutate-fst ai))]))

(define (input w ke)
  (match w
    [(user-input match# ai round# wins state)
     (define ui
       (match ke
         ["r" 'r]
         ["s" 's]
         ["p" 'p]
         [_ #f]))
     (if ui
         (computer-input match# ai round# wins state ui)
         w)]
    [_ (tick w)]))

(define (string s . a)
  (text (apply format s a) 36 "black"))
(define rps->string 
  (match-lambda
    ['r "Rock"]
    ['p "Paper"]
    ['s "Scissors"]))

(define (render w)
  (above
   (match w
    [(start match# _)
     (above (string "Match: ~a" match#)
            (string "Fight!"))]
    [(user-input match# _ round# wins _)
     (above (string "Match: ~a" match#)
            (string "Round: ~a" round#)
            (string "Ratio: ~a/~a" wins round#)
            (string "What will you throw down?"))]
    [(computer-input match# _ round# wins _ ui)
     (above (string "Match: ~a" match#)
            (string "Round: ~a" round#)
            (string "Ratio: ~a/~a" wins round#)
            (string "You threw down ~a" (rps->string ui)))]
    [(resolve match# _ round# wins _ ui ci)
     (above (string "Match: ~a" match#)
            (string "Round: ~a" round#)
            (string "Ratio: ~a/~a" wins round#)
            (string "You threw down ~a" (rps->string ui))
            (string "The AI threw down ~a" (rps->string ci)))]
    [(resolved match# _ round# wins _ _ _ outcome)
     (above (string "Match: ~a" match#)
            (string "Round: ~a" round#)
            (string "Ratio: ~a/~a" wins round#)
            (match outcome
              ['user (string "You won the round!")]
              ['computer (string "The AI won the round!")]
              ['draw (string "Draw")]))]
    [(end match# _ round# wins _)
     (above (string "Match: ~a" match#)
            (string "Round: ~a" round#)
            (string "Ratio: ~a/~a" wins round#)
            (string "You won the match!"))])
   (fst-graph* (a-match-ai-spec w))))

(define graphs (make-weak-hash))
(define (fst-graph* fst)
  (hash-ref! graphs fst (λ () (fst-graph fst))))

(big-bang (start 1 start-fst)
          (on-key input)
          (to-draw render 800 600))
