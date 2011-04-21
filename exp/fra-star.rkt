#lang typed/racket
(require/typed "lifo-heap.rkt"
               [opaque Heap heap?])
(require/typed "lifo-heap.rkt"
               [heap (All (A) ((A -> Number) -> (Heap A)))]
               [heap-empty? (All (A) ((Heap A) -> Boolean))]
               [heap-remove-min! (All (A) ((Heap A) -> A))]
               [heap-member? (All (A) ((Heap A) A -> Boolean))]
               [heap-add! (All (A) ((Heap A) A -> Void))]
               [heap-remove! (All (A) ((Heap A) A -> Void))]
               [in-heap (All (A) ((Heap A) -> (Listof A)))])

(define-syntax-rule
  (while c e ...)
  (let loop ()
    (when c
      e ...
      (loop))))
(define-syntax-rule (hash-ref# h k)
  (let ([kv k])
    (hash-ref h kv (λ () (error 'h "No entry for ~a" kv)))))

(: main
   (All (Node) 
        (Node -> (Listof Node))
        (Node Node -> Number)
        Node Node
        ->
        (Node Node -> Boolean)))
(define (main node->neighbors estimate first-start first-goal)
  (: g (HashTable Node Number))
  (define g (make-hash))
  (: g-ref (Node -> Number))
  (define (g-ref v)
    (hash-ref g v +inf.0))
  (define start first-start)
  (define goal first-goal)
  ; 48
  (: open Heap)
  (define open (heap open-measure))
  (: cell Node)
  (define cell first-start)
  (: previous-start Node)
  (define previous-start first-start)
  (: anchor Node)
  (define anchor first-start)
  ; Finds the nodes in counter-clockwise order starting at start
  (: counter-clockwise ((Listof Node) Node -> (Listof Node)))
  (define (counter-clockwise ns start)
    (clockwise (reverse ns) start))
  ; Finds the nodes in clockwise order starting at start
  (: clockwise ((Listof Node) Node -> (Listof Node)))
  (define (clockwise ns start)
    (let loop ([left empty] [right ns])
      (cond
        [(empty? right)
         empty
         ; XXX should this happen?
         #;(error 'clockwise "~a not in list" start)]
        [(equal? start (first right))
         ; We will start here, then everything else we saw before
         (append right (reverse left))]
        [else
         (loop (cons (first right) left)
               (rest right))])))
  (: search-tree-rooted-at (Node -> (Listof Node)))
  (define (search-tree-rooted-at n)
    (let loop ([n n])
      (list* n
             (append-map
              loop
              (hash-ref children n empty)))))
  ; XXX
  (: outer-perimeter-of-closed (Node -> (Listof Node)))
  (define (outer-perimeter-of-closed a)
    (if a
        (error 'xxx "~s" (list 'outer-perimeter-of-closed a))
        empty))
  (: unblocked? (Node -> Boolean))
  (define (unblocked? s)
    ; XXX I assume this is asking if the node can be entered. I don't show nodes to the algorithm
    ;     where this is not the case, so I'm always going to return #t
    #t)
  (: initialize-cell (Node -> Void))
  (define (initialize-cell s)
    ; 01
    (when (not (equal? (generated-iteration-ref s) iteration))
      ; 02
      (hash-set! g s +inf.0)
      ; 03
      (hash-set! generated-iteration s iteration)
      ; 04
      (hash-set! expanded s #f)))
  (: test-closed-list (Node -> Boolean))
  (define (test-closed-list s)
    ; 05
    (or (equal? s start)
        (and (expanded-ref s)
             (not (equal? (parent-ref s) #f)))))
  ; From 07
  (: open-measure (Node -> Number))
  (define (open-measure s)
    (+ (g-ref s)
       (estimate s goal)))
  (: compute-shortest-path (-> Boolean))
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
                 (when ((g-ref sp) . > . (add1 (g-ref s)))
                   ; 13
                   (hash-set! g sp (add1 (g-ref s)))
                   ; 14
                   (parent-set! sp s)
                   ; 15
                   (unless (heap-member? open sp)
                     (heap-add! open sp)))))
             ; 16
             (when (equal? s goal)
               (return #t)))
      ; 17
      (return #f)))
  (: update-parent 
     (((Listof Node) Node -> (Listof Node)) -> Boolean))
  (define (update-parent direction)
    (let/ec return
      ; 18
      (for ([s (in-list (direction (node->neighbors cell) (parent-ref cell)))])
        ; 19
        (when (and (equal? (g-ref s)
                           (add1 (g-ref cell)))
                   (test-closed-list s))
          ; 20
          (parent-set! s cell)
          ; 21
          (set! cell s)
          ; 22
          (return #t)))
      ; 23
      (return #f)))
  (: step-2 (-> Void))
  (define (step-2)
    ; 24
    (set! cell start)
    ; 25
    (while (update-parent counter-clockwise))
    ; 26
    (set! cell start)
    ; 27
    (while (update-parent clockwise)))
  (: step-3 (-> Void))
  (define (step-3)
    ; 28
    (parent-unset! start)
    ; 29
    (for ([s (search-tree-rooted-at previous-start)])
      ; 30
      (parent-unset! s)
      ; 31
      (heap-remove! open s)))
  (: step-5 (-> Void))
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
                   ((g-ref s) . > . (add1 (g-ref sp))))
          ; 39
          (hash-set! g s (add1 (g-ref sp)))
          ; 40
          (parent-set! s sp)))))
  ; 42
  (: generated-iteration (HashTable Node Integer))
  (define generated-iteration (make-hash))
  (: generated-iteration-ref (Node -> Integer))
  (define (generated-iteration-ref s)
    (hash-ref generated-iteration s 0))
  ; 43
  (: expanded (HashTable Node Boolean))
  (define expanded (make-hash))
  (: expanded-ref (Node -> Boolean))
  (define (expanded-ref s)
    (hash-ref expanded s #f))
  ; 44
  (: parent (HashTable Node Node))
  (define parent (make-hash))
  (: children (HashTable Node (Listof Node)))
  (define children (make-hash))
  (: parent-unset! (Node -> Void))
  (define (parent-unset! s)
    (hash-update! children (parent-ref parent)
                  (λ (l) (remove s l))
                  empty)
    (hash-remove! parent s))
  (: parent-set! (Node Node -> Void))
  (define (parent-set! s p)
    (hash-update! children p (curry cons s) empty)
    (hash-set! parent s p))
  (: parent-ref (Node -> (Option Node)))
  (define (parent-ref s)
    (hash-ref parent s #f))
  ; 45
  (: iteration Integer)
  (define iteration 1)
    (let/ec return
      ; 46
      (initialize-cell start)
      ; 47
      (hash-set! g start 0)
      ; 49
      (heap-add! open start)
      ; 50
      (while (not (equal? start goal))
             ; 51
             (unless (compute-shortest-path) 
               ; XXX allow another search
               (return #f))
             ; 52
             (: openlist-incomplete? Boolean)
             (define openlist-incomplete? #f)
             ; 53
             (while (test-closed-list goal)
                    ; 54-56
                    (define-values (current-hunter current-target)
                      ; XXX actually return the path, is this correct?
                      (return 
                       (first (hash-ref children start))))
                    ; 57
                    (set! previous-start start)
                    ; 58
                    (set! start current-hunter)
                    ; 59
                    (set! goal current-target)
                    ; 60
                    (when (not (equal? previous-start start))
                      ; 61
                      (step-2)
                      ; 62
                      (: ps Node)
                      (define ps (assert (parent-ref start)))
                      (set! anchor ps)
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
      (error 'shortest-path "Got outside the loop somehow")))