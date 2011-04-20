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
;  - cache : map start*goal -> (or/c node #f)
;  - node->neighbors : (node -> listof node)
;  - estimate : (node node -> integer)
(struct graph (cache node->neighbors estimate))
(define (make-graph n->n e)
  (graph (make-hash) n->n e))

; A node is anything equal?-able

; shortest-path : graph node node -> maybe direction
; Given a graph, finds the next node to move to along the shortest
; path from start to end

; XXX Collect performance numbers
(define (A* g start goal)
  (match-define (graph cache node->neighbors estimate) g)
  (cond
    [(hash-ref cache (cons start goal) #f)
     => (λ (ans) ans)]
    [else
     (let/ec return    
       (hash-set! cache (cons goal goal) goal)
       (when (equal? start goal)
         (return goal))
       
       (define-values
        (g-score h-score f-score closed-set)
        (values (make-hash) (make-hash) (make-hash)
                (make-hash)))
       
       ; XXX Is it safe that this can change after
       ;     the insertion? Can it?
       (define (f-score-<= a b)
         (define fa (hash-ref f-score a))
         (define fb (hash-ref f-score b))
         (if (= fa fb)
             ; If the heuristics are identical, prefer the new entry
             (>= (hash-ref open-set a)
                 (hash-ref open-set b))
             (<= fa fb)))
       (define-values
         (open-set open-queue came-from)
         (values (make-hash)
                 (make-heap f-score-<=)
                 (make-hash)))
       
       (define n 0)
       (define (open-set-put! e)
         (hash-set! open-set e n)
         (set! n (add1 n)))
       
       (hash-set! g-score start 0)
       (hash-set! h-score start (estimate start goal))
       (hash-set! f-score start (hash-ref h-score start))       
       (open-set-put! start)
       (heap-add! open-queue start)
       
       (define (reconstruct current-node later-nodes)
         (define next-node (hash-ref came-from current-node #f))
         (if next-node
             (begin
               (for ([later-node (in-list later-nodes)])
                 (hash-ref! cache (cons next-node later-node) current-node))
               (if (equal? next-node start)
                   current-node
                   (reconstruct next-node (cons current-node later-nodes))))
             current-node))
       
       (while 
        (not (heap-empty? open-queue))
        (define x (heap-min open-queue))
        (heap-remove-min! open-queue)
        (hash-remove! open-set x)
        ; If it is in the cache, then we know that there is already a shortest path found.
        (when (hash-has-key? cache (cons x goal)) #;(equal? x goal)
          (define last-node (reconstruct x empty))
          (return last-node))
        (hash-set! closed-set x #t)
        (for ([y (in-list (node->neighbors x))])
          (when (not (hash-has-key? closed-set y))
            (define tentative-g-score 
              (add1 
               (hash-ref g-score x)))
            (define tentative-is-better? #t)
            (define add-to-open-set? #f)
            (cond
              [(not (hash-has-key? open-set y))
               (set! add-to-open-set? #t)
               (set! tentative-is-better? #t)]
              [(< tentative-g-score
                  (hash-ref g-score y))
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
                (open-set-put! y)
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

; XXX look at AlphA*
; XXX look at http://webdocs.cs.ualberta.ca/~games/pathfind/publications/cig2005.pdf
; XXX look at http://idm-lab.org/bib/abstracts/Koen02g.html (this is for changing maps)
; XXX look at http://idm-lab.org/bib/abstracts/Koen09i.html

(define (manhattan-distance n1 n2)
  (match-define (cons x1 y1) n1)
  (match-define (cons x2 y2) n2)
  (+ (abs (- x1 x2)) (abs (- y1 y2))))
(define (euclidian-distance n1 n2)
  (match-define (cons x1 y1) n1)
  (match-define (cons x2 y2) n2)
  (sqrt
   (+ (sqr (- x2 x1)) (sqr (- y2 y1)))))
(define (tie-breaker dist max-len)
  (define p (/ 1 max-len))
  (define a (+ 1 p))
  (λ (n1 n2)
    (* a (dist n1 n2))))

(provide/contract
 [manhattan-distance ((cons/c integer? integer?) (cons/c integer? integer?) . -> . number?)]
 [euclidian-distance ((cons/c integer? integer?) (cons/c integer? integer?) . -> . number?)]
 [tie-breaker ((-> any/c any/c number?) number? . -> . (-> any/c any/c number?))])

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
   manhattan-distance))

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
    
    (shortest-path g (cons 1 7) (cons 3 1)) => (cons 1 6)
    (shortest-path g (cons 1 8) (cons 3 1)) => (cons 1 7)
    (shortest-path g (cons 2 8) (cons 3 1)) => (cons 1 8)
    
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