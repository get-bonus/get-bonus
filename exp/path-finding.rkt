#lang racket/base
(require racket/list
         racket/contract
         racket/math
         racket/match
         "lifo-heap.rkt"
         tests/eli-tester
         racket/package)

(define-syntax-rule
  (while c e ...)
  (let loop ()
    (when c
      e ...
      (loop))))

; A node is anything equal?-able

; shortest-path : graph node node -> maybe direction
; Given a graph, finds the next node to move to along the shortest
; path from start to end

; A graph is a 
  ;  - node->neighbors : (node -> listof node)
  ;  - estimate : (node node -> integer)

; Based on Wikipedia
(define-package A*-pkg 
  (graph? make-graph shortest-path)
  
  ; A graph is a 
  ;  ...
  ;  - cache : map start*goal -> (or/c node #f)
  (struct graph (cache node->neighbors estimate))
  (define (make-graph n->n e)
    (graph (make-hash) n->n e))
  
  ; XXX Collect performance numbers
  (define (shortest-path g start goal)
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
         (define (f a) (hash-ref f-score a))
         (define-values
           (open-queue came-from)
           (values (heap f)
                   (make-hash)))
         
         (hash-set! g-score start 0)
         (hash-set! h-score start (estimate start goal))
         (hash-set! f-score start (hash-ref h-score start))
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
          (define x (heap-remove-min! open-queue))
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
                [(not (heap-member? open-queue y))
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
                  (heap-add! open-queue y))))))
         
         #f)])))

; Based on http://idm-lab.org/bib/abstracts/Koen09i.html
(require racket/generator)
(define-package FRA*-pkg 
  (graph? make-graph shortest-path)
  
  (struct graph (gen))
  (define (make-graph n->n e)
    (graph (main n->n e)))
  
  ; XXX Collect performance numbers
  (define (shortest-path g start end)
    (match-define (graph gen) g)
    (gen start end))
  
  (define (main node->neighbors estimate)
    (define g (make-hash))
    (define start #f)
    (define goal #f)
    (define open #f)
    (define cell #f)
    (define previous-start #f)
    (define anchor #f)
    ; XXX
    (define (counter-clockwise ns start)
      empty)
    ; XXX
    (define (clockwise ns start)
      empty)
    ; XXX
    (define (search-tree-rooted-at n)
      error)
    ; XXX
    (define (outer-perimeter-of-closed a)
      error)
    ; XXX
    (define (unblocked? s)
      error)
    (define (initialize-cell s)
      ; 01
      (when (not (equal? (generated-iteration-ref s) iteration))
        ; 02
        (hash-set! g s +inf.0)
        ; 03
        (hash-set! generated-iteration s iteration)
        ; 04
        (hash-set! expanded s #f)))
    (define (test-closed-list s)
      ; 05
      (or (equal? s start)
          (and (expanded-ref s)
               (not (equal? (parent-ref s) #f)))))
    ; From 07
    (define (open-measure s)
      (+ (hash-ref g s)
         (estimate s goal)))
    (define (compute-shortest-path)
      (let/ec return
        ; 06
        (while (not (heap-empty? open))
               ; 07
               (define s (heap-remove-min! open))
               ; 08
               (hash-set! expanded s #t)
               ; 09
               (for ([sp (in-list (node->neighbors s))])
                 ; 10
                 (when (not (test-closed-list sp))
                   ; 11
                   (initialize-cell sp)
                   ; 12
                   (when ((hash-ref g sp) . > . (add1 (hash-ref g s)))
                     ; 13
                     (hash-set! g sp (add1 (hash-ref g s)))
                     ; 14
                     (hash-set! parent sp s)
                     ; 15
                     (unless (heap-member? open sp)
                       (heap-add! open sp)))))
               ; 16
               (when (equal? s goal)
                 (return #t)))
        ; 17
        (return #f)))
    (define (update-parent direction)
      (let/ec return
        ; 18
        (for ([s (in-list (direction (node->neighbors cell) (parent-ref cell)))])
          ; 19
          (when (and (equal? (hash-ref g s)
                             (add1 (hash-ref g cell)))
                     (test-closed-list s))
            ; 20
            (hash-set! parent s cell)
            ; 21
            (set! cell s)
            ; 22
            (return #t)))
        ; 23
        (return #f)))
    (define (step-2)
      ; 24
      (set! cell start)
      ; 25
      (while (update-parent counter-clockwise))
      ; 26
      (set! cell start)
      ; 27
      (while (update-parent clockwise)))
    (define (step-3)
      ; 28
      (hash-ref! parent start #f)
      ; 29
      (for ([s (search-tree-rooted-at previous-start)])
        ; 30
        (hash-set! parent s #f)
        ; 31
        (heap-remove! open s)))
    (define (step-5)
      ; 32
      (for ([s (outer-perimeter-of-closed anchor)])
        ; 33
        (when (and (unblocked? s)
                   (heap-member? open s))
          (heap-add! open s)))
      ; 34
      (for ([s (in-heap open)])
        ; 35
        (initialize-cell s))
      ; 36
      (for ([s (in-heap open)])
        ; 37
        (for ([sp (in-list (node->neighbors s))])
          ; 38
          (when (and (test-closed-list sp)
                     ((hash-ref g s) . > . (add1 (hash-ref g sp))))
            ; 39
            (hash-set! g s (add1 (hash-ref g sp)))
            ; 40
            (hash-set! parent s sp)))))
    ; 42
    (define generated-iteration (make-hash))
    (define (generated-iteration-ref s)
      (hash-ref generated-iteration s 0))
    ; 43
    (define expanded (make-hash))
    (define (expanded-ref s)
      (hash-ref expanded s #f))
    ; 44
    (define parent (make-hash))
    (define (parent-ref s)
      (hash-ref parent s #f))
    ; 45
    (define iteration 1)
    (generator 
     (first-start first-goal)
     (set! start first-start)
     (set! goal first-goal)
     ; 46
     (initialize-cell start)
     ; 47
     (hash-set! g start 0)
     ; 48
     (set! open (heap open-measure))
     ; 49
     (heap-add! open start)
     (let/ec return
       ; 50
       (while (not (equal? start goal))
              ; 51
              (unless (compute-shortest-path) (return #f))
              ; 52
              (define openlist-incomplete? #f)
              ; 53
              (while (test-closed-list goal)
                     ; 54
                     (while (target-not-caught-and-target-is-on-shortest-path-from-start-to-goal)
                            ; 55
                            (follow-shortest-path-from-start-to-goal))
                     ; 56
                     (when (target-caught) (return #t))
                     ; 57
                     (set! previous-start start)
                     ; 58
                     (set! start current-hunter)
                     ; 59
                     (set! end current-target)
                     ; 60
                     (when (not (equal? previous-start start))
                       ; 61
                       (step-2)
                       ; 62
                       (set! anchor (parent-ref start))
                       ; 63
                       (step-3)
                       ; 64
                       (set! openlist-incomplete? #t)))
              ; 65
              (when openlist-incomplete?
                ; 66
                (set! iteration (add1 iteration))
                ; 67
                (step-5)))
       ; 68
       (return #t)))))

(open-package FRA*-pkg)

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