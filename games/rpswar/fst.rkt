#lang racket/base
(require racket/match
         data/enumerate
         data/enumerate/lib
         "random.rkt")

(struct fst (states input-alpha output-alpha start delta state->output) #:transparent)

(define (fst/e input output)
  (map/e #:contract fst?
         (match-lambda
           [(cons how-many-states
                  (vector start
                          (list (list next ...) ...)
                          (list output-v ...)))
            (fst how-many-states input output start
                 (for/hash ([s (in-range how-many-states)]
                            [ns (in-list next)])
                   (values s
                           (for/hash ([i (in-list input)]
                                      [n (in-list ns)])
                             (values i n))))
                 (for/hash ([s (in-range how-many-states)]
                            [ov (in-list output-v)])
                   (values s ov)))])
         (match-lambda
           [(fst how-many-states (== input) (== output)
                 start delta state->output)
            (cons how-many-states
                  (vector start
                          (for/list ([s (in-range how-many-states)])
                            (define input->next (hash-ref delta s))
                            (for/list ([i (in-list input)])
                              (hash-ref input->next i)))
                          (for/list ([s (in-range how-many-states)])
                            (hash-ref state->output s))))])
         (dep/e
          #:f-range-finite? #t
          natural/e
          (λ (how-many-states)
            (vector/e (below/e how-many-states)
                      (listof-n/e (listof-n/e (below/e
                                               how-many-states)
                                              (length input))
                                  how-many-states)
                      (listof-n/e (apply fin/e output)
                                  how-many-states))))))

(module+ test
  (define alpha '(r p s))
  (define ai
    (for/fold ([ai (random-one-state-fst alpha alpha)])
              ([i (in-range 20)])
      (mutate-fst ai)))

  (define ai/e fst/e)

  (define n (to-nat (ai/e alpha alpha) ai))
  (printf "[ ~v ] = ~v\n" ai n)
  (define v (from-nat (ai/e alpha alpha) n))
  (printf "[ ~v ]^-1 = ~v\n" n v)
  (printf "~v\n" (equal? v ai))

  (require racket/pretty
           racket/runtime-path
           racket/file
           gb/lib/math)
  (define-runtime-path here ".")

  (pretty-print
   (from-nat (ai/e alpha alpha)
             (bytes->integer (file->bytes (build-path here "fst.rkt"))))))

(define (format-fst f current)
  (format "~a of ~a"
          current
          (fst-states f)))

(define (random-state how-many-states)
  (random how-many-states))

(define (random-one-state-fst input output)
  (define state_0 0)
  (fst 1 input output state_0
       (hash state_0
             (for/hash ([i (in-list input)])
               (values i state_0)))
       (hash state_0
             (random-list-ref output))))

(define (mutate-fst f)
  (match-define (fst states input-alpha output-alpha start delta state->output) f)
  (define (change-output state->output random-state)
    (hash-set state->output random-state
              (random-list-ref output-alpha)))
  (define (change-transition delta from-state to-state)
    (hash-update delta from-state
                 (λ (input->next)
                   (hash-set input->next
                             (random-list-ref input-alpha)
                             to-state))
                 (hash)))
  (random-case
   [1/2
    ;; Add a new state
    (define new-state states)
    (fst (add1 new-state) input-alpha output-alpha start
         ;; Linked to by a random existing state
         (change-transition
          ;; with random transitions to other stats
          (hash-set delta new-state
                    (for/hash ([i (in-list input-alpha)])
                      (values i (random-state new-state))))
          (random-state states)
          new-state)
         ;; and a random output
         (change-output state->output new-state))]
   ;; Add a transition
   [1/6
    (fst states input-alpha output-alpha start
         (change-transition delta (random-state states)
                            (random-state states))
         state->output)]
   ;; Change an output
   [1/6
    (fst states input-alpha output-alpha start
         delta
         (change-output state->output (random-state states)))]
   ;; Change the start state
   [1/6
    (fst states input-alpha output-alpha
         (random-state states)
         delta state->output)]))

(define (fst-output f s)
  (hash-ref (fst-state->output f) s))

(define (fst-next f s i)
  (hash-ref (hash-ref (fst-delta f) s) i))

(provide (all-defined-out))
