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

; A direction is 'left, 'right, 'up, 'down
(define dirs '(left right up down))
(define direction/c (apply symbols dirs))

; A graph is a 
;  - node-in-dir : (node direction -> maybe node)
;  - estimate : (node node -> integer)
(struct graph (node-in-dir estimate))

; A node is anything equal?-able

; shortest-path : graph node node -> maybe direction
; Given a graph, finds the direction to move along the shortest
; path from start to end
(define (A* g start goal)
  (let/ec return    
    (when (equal? start goal)
      (return #f))
    
    (match-define (graph node-in-dir estimate) g)
    (define g-score (make-hash))
    (hash-set! g-score start 0)
    (define h-score (make-hash))
    (hash-set! h-score start (estimate start goal))
    (define f-score (make-hash))
    (hash-set! f-score start (hash-ref h-score start))
    
    ; XXX Is it safe that this can change after the insertion?
    (define (f-score-<= a b)
      (<= (hash-ref f-score a
                    (λ ()
                      (error 'f-score "No f-score for a = ~a" a)))
          (hash-ref f-score b
                    (λ ()
                      (error 'f-score "No f-score for b = ~a" b)))))
    
    (define closed-set (make-hash))
    (define open-set (make-hash))
    (define open-queue (make-heap f-score-<=))    
    
    (hash-set! open-set start #t)
    (heap-add! open-queue start)
    (define came-from (make-hash))
    (define came-from/dir (make-hash))
    ; XXX cache all these
    (define (reconstruct start current-node)
      (define next-node (hash-ref came-from current-node #f))
      (cond
        [(equal? next-node start)
         current-node]
        [next-node
         (reconstruct start next-node)]
        [else
         current-node]))
    
    (while 
     (not (heap-empty? open-queue))
     (define x (heap-min open-queue))
     (heap-remove-min! open-queue)
     (hash-remove! open-set x)
     (when (equal? x goal)
       (define last-node
         (reconstruct start goal))
       (return (hash-ref came-from/dir last-node)))
     (hash-set! closed-set x #t)
     (for ([dir (in-list dirs)])
       (define y (node-in-dir x dir))
       (when (and y
                  (not (hash-has-key? closed-set y)))
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
           (hash-set! came-from/dir y dir)
           (hash-set! g-score y tentative-g-score)
           (hash-set! h-score y (estimate y goal))
           (hash-set! f-score y 
                      (+ (hash-ref g-score y)
                         (hash-ref h-score y)))
           (when add-to-open-set?
             (hash-set! open-set y #t)
             (heap-add! open-queue y))))))
    
    #f))

(define shortest-path A*)

(provide/contract
 [direction/c contract?]
 [struct graph
         ([node-in-dir
           (-> any/c direction/c
               (or/c any/c #f))]
          [estimate
           (-> any/c any/c
               number?)])]
 [shortest-path
  (-> graph? any/c any/c
      (or/c #f direction/c))])

; A simple macro for encoding simple grid graphs
(define-syntax simple-graph
  (syntax-rules ()
    [(_ [wall? ...] ...)
     (simple-graph* (reverse '([wall? ...] ...)))]))
(define (simple-graph* rows-0-to-n)
  (define rows (length rows-0-to-n))
  (define cols (length (first rows-0-to-n)))
  (graph
   (λ (n dir)
     (match-define (cons x y) n)
     (define-values (nx ny)
       (match dir
         ['left (values (sub1 x) y)]
         ['right (values (add1 x) y)]
         ['up (values x (add1 y))]
         ['down (values x (sub1 y))]
         [_ (error 'node-in-dir "Invalid direction")]))
     (if (and
          (<= 0 nx (sub1 cols))
          (<= 0 ny (sub1 rows))
          (zero? (list-ref (list-ref rows-0-to-n ny) nx)))
         (cons nx ny)
         #f))
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
    (shortest-path g (cons 0 0) (cons 1 0)) => 'right    
    (shortest-path g (cons 1 0) (cons 0 0)) => 'left))
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
    (shortest-path g (cons 0 1) (cons 2 1)) => 'down
    (shortest-path g (cons 0 1) (cons 2 0)) => 'down
    (shortest-path g (cons 0 1) (cons 1 0)) => 'down
    (shortest-path g (cons 0 1) (cons 0 0)) => 'down
    (shortest-path g (cons 2 1) (cons 0 1)) => 'down))
 (let ()
   (define g
     (simple-graph
      [0 0 0]
      [0 1 0]))
   (test
    (shortest-path g (cons 0 0) (cons 2 0)) => 'up
    (shortest-path g (cons 2 0) (cons 0 0)) => 'up))
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
    (shortest-path g (cons 1 6) (cons 3 1)) => 'down
    (shortest-path g (cons 1 5) (cons 3 1)) => 'down
    (shortest-path g (cons 1 4) (cons 3 1)) => 'down
    (shortest-path g (cons 1 3) (cons 3 1)) => 'down
    (shortest-path g (cons 1 2) (cons 3 1)) => 'down
    (shortest-path g (cons 1 1) (cons 3 1)) => 'right
    (shortest-path g (cons 2 1) (cons 3 1)) => 'right
    (shortest-path g (cons 3 1) (cons 3 1)) => #f)))