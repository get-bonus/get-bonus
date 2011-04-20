#lang racket/base
(require racket/list
         racket/contract
         racket/math
         racket/match
         data/heap
         tests/eli-tester)

(define-syntax-rule
  (while c e ...)
  (let loop ()
    (when c
      e ...
      (loop))))

(define (heap-empty? h)
  (zero? (heap-count h)))

; A graph is a 
;  - cache : start*goal -> (or/c node #f)
;  - node->neighbors : (node -> listof node)
;  - estimate : (node node -> integer)
(struct graph (cache node->neighbors estimate))
(define (make-graph n->n e)
  (graph (make-hash) n->n e))

; A node is anything equal?-able

; shortest-path : graph node node -> maybe direction
; Given a graph, finds the next node to move to along the shortest
; path from start to end

; XXX This cache is bad because it does not "link up" with other
;     found paths from before
(define (A* g start goal)
  (match-define (graph cache node->neighbors estimate) g)
  (cond
    [(hash-ref cache (cons start goal) #f)
     => (λ (ans) ans)]
    [else
     (let/ec return    
       (when (equal? start goal)
         (hash-set! cache (cons start goal) goal)
         (return goal))
       
       (define g-score (make-hash))
       (hash-set! g-score start 0)
       (define h-score (make-hash))
       (hash-set! h-score start (estimate start goal))
       (define f-score (make-hash))
       (hash-set! f-score start (hash-ref h-score start))
       
       ; XXX Is it safe that this can change after the insertion?
       ;     Can it?
       (define (f-score-<= a b)
         (<= 
          (hash-ref f-score a)
          (hash-ref f-score b)))
       
       (define closed-set (make-hash))
       (define open-set (make-hash))
       (define open-queue (make-heap f-score-<=))    
       
       (hash-set! open-set start #t)
       (heap-add! open-queue start)
       (define came-from (make-hash))
       (define (reconstruct current-node)
         (define next-node (hash-ref came-from current-node))
         (hash-ref! cache (cons next-node goal) current-node)
         (if (equal? next-node start)
             current-node
             (reconstruct next-node)))
       
       (while 
        (not (heap-empty? open-queue))
        (define x (heap-min open-queue))
        (heap-remove-min! open-queue)
        (hash-remove! open-set x)
        (when (equal? x goal)
          (define last-node (reconstruct goal))
          (return last-node))
        (hash-set! closed-set x #t)
        (for ([y (in-list (node->neighbors x))])
          (when (not (hash-has-key? closed-set y))
            (define tentative-g-score 
              (add1 
               (hash-ref
                g-score x
                (λ ()
                  (error 'g-score "No g-score for x = ~a" x)))))
            (define tentative-is-better? #t)
            (define add-to-open-set? #f)
            (cond
              [(not (hash-has-key? open-set y))
               (set! add-to-open-set? #t)
               (set! tentative-is-better? #t)]
              [(< tentative-g-score
                  (hash-ref 
                   g-score y
                   (λ ()
                     (error 'g-score "No g-score for y = ~a" y))))
               (set! tentative-is-better? #t)]
              [else
               (set! tentative-is-better? #f)])
            (when tentative-is-better?
              (hash-set! came-from y x)
              (hash-set! g-score y tentative-g-score)
              (hash-set! h-score y (estimate y goal))
              (hash-set! f-score y 
                         (+ (hash-ref g-score y)
                            (hash-ref h-score y)))
              (when add-to-open-set?
                (hash-set! open-set y #t)
                (heap-add! open-queue y))))))
       
       #f)]))

(define shortest-path A*)

(provide/contract
 [graph? contract?]
 [rename
  make-graph graph
  (-> (-> any/c 
          (listof any/c))
      (-> any/c any/c
          number?)
      graph?)]
 [shortest-path
  (-> graph? any/c any/c
      (or/c #f any/c))])

; A simple macro for encoding simple grid graphs
(define-syntax simple-graph
  (syntax-rules ()
    [(_ [wall? ...] ...)
     (simple-graph* (reverse '([wall? ...] ...)))]))
(define (simple-graph* rows-0-to-n)
  (define rows (length rows-0-to-n))
  (define cols (length (first rows-0-to-n)))
  (define (xy-okay? nx ny)
    (and
     (<= 0 nx (sub1 cols))
     (<= 0 ny (sub1 rows))
     (zero? (list-ref (list-ref rows-0-to-n ny) nx))))
  (make-graph
   (λ (n)
     (match-define (cons x y) n)
     (define-syntax-rule
       (try [nx* ny*] ...)
       (append 
        (let ([nx nx*] [ny ny*])
          (if (xy-okay? nx ny)
              (list (cons nx ny))
              empty))
        ...))
     (try [(sub1 x) y] [(add1 x) y]
          [x (sub1 y)] [x (add1 y)]))
   (λ (n1 n2)
     (match-define (cons x1 y1) n1)
     (match-define (cons x2 y2) n2)
     ; Note, we don't sqrt, because if we uniformly don't,
     ; it doesn't affect the heuristic
     (+ (sqr (- x2 x1)) (sqr (- y2 y1))))))

(test
 (let ()
   (define g
     (simple-graph
      [0 0]))
   (test
    (shortest-path g (cons 0 0) (cons 1 0)) => (cons 1 0)
    (shortest-path g (cons 1 0) (cons 0 0)) => (cons 0 0)))
 (let ()
   (define g
     (simple-graph
      [0 1 0]))
   (test
    (shortest-path g (cons 0 0) (cons 2 0)) => #f
    (shortest-path g (cons 2 0) (cons 0 0)) => #f))
 (let ()
   (define g
     (simple-graph
      [0 1 0]
      [0 0 0]))
   (test
    (shortest-path g (cons 0 1) (cons 2 1)) => (cons 0 0)
    (shortest-path g (cons 0 1) (cons 2 0)) => (cons 0 0)
    (shortest-path g (cons 0 1) (cons 1 0)) => (cons 0 0)
    (shortest-path g (cons 0 1) (cons 0 0)) => (cons 0 0)
    (shortest-path g (cons 2 1) (cons 0 1)) => (cons 2 0)))
 (let ()
   (define g
     (simple-graph
      [0 0 0]
      [0 1 0]))
   (test
    (shortest-path g (cons 0 0) (cons 2 0)) => (cons 0 1)
    (shortest-path g (cons 2 0) (cons 0 0)) => (cons 2 1)))
 ; From http://en.wikipedia.org/wiki/Pathfinding#Sample_algorithm
 (let ()
   (define g
     (simple-graph
      [1 1 1 1 1 1 1 1 1 1]
      [1 0 0 0 1 1 0 1 0 1]
      [1 0 1 0 0 1 0 0 0 1]
      [1 0 1 1 0 0 0 1 0 1]
      [1 0 1 0 0 1 0 0 0 1]
      [1 0 0 0 1 1 0 1 0 1]
      [1 0 1 0 0 1 0 1 0 1]
      [1 0 1 1 0 0 0 1 0 1]
      [1 0 0 0 0 1 0 0 0 1]
      [1 1 1 1 1 1 1 1 1 1]))
   (test
    (shortest-path g (cons 1 6) (cons 3 1)) => (cons 1 5)
    (shortest-path g (cons 1 5) (cons 3 1)) => (cons 1 4)
    (shortest-path g (cons 1 4) (cons 3 1)) => (cons 1 3)
    (shortest-path g (cons 1 3) (cons 3 1)) => (cons 1 2)
    (shortest-path g (cons 1 2) (cons 3 1)) => (cons 1 1)
    (shortest-path g (cons 1 1) (cons 3 1)) => (cons 2 1)
    (shortest-path g (cons 2 1) (cons 3 1)) => (cons 3 1)
    (shortest-path g (cons 3 1) (cons 3 1)) => (cons 3 1)
    
    (shortest-path g (cons 1 1) (cons 8 1)) => (cons 2 1)
    (shortest-path g (cons 2 1) (cons 8 1)) => (cons 3 1)
    (shortest-path g (cons 3 1) (cons 8 1)) => (cons 4 1)
    (shortest-path g (cons 4 1) (cons 8 1)) => (cons 4 2)
    (shortest-path g (cons 4 2) (cons 8 1)) => (cons 5 2)
    (shortest-path g (cons 5 2) (cons 8 1)) => (cons 6 2)
    (shortest-path g (cons 6 2) (cons 8 1)) => (cons 6 1)
    (shortest-path g (cons 6 1) (cons 8 1)) => (cons 7 1)
    (shortest-path g (cons 7 1) (cons 8 1)) => (cons 8 1)
    (shortest-path g (cons 8 1) (cons 8 1)) => (cons 8 1)
       
    ; To everything again to ensure the cache didn't get messed
    ; up.
    (shortest-path g (cons 1 6) (cons 3 1)) => (cons 1 5)
    (shortest-path g (cons 1 5) (cons 3 1)) => (cons 1 4)
    (shortest-path g (cons 1 4) (cons 3 1)) => (cons 1 3)
    (shortest-path g (cons 1 3) (cons 3 1)) => (cons 1 2)
    (shortest-path g (cons 1 2) (cons 3 1)) => (cons 1 1)
    (shortest-path g (cons 1 1) (cons 3 1)) => (cons 2 1)
    (shortest-path g (cons 2 1) (cons 3 1)) => (cons 3 1)
    (shortest-path g (cons 3 1) (cons 3 1)) => (cons 3 1))))