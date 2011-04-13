#lang typed/racket/base
(require (prefix-in rk: racket/base))
(provide filter remove reverse
         list ->list empty? cons empty head tail
         (rename-out [first* first] [rest* rest] [list-map map]
                     [list-foldr foldr] [list-foldl foldl]
                     [list-ormap ormap] [list-andmap andmap]
                     [list-second second] [list-third third]
                     [list-fourth fourth] [list-fifth fifth]
                     [list-sixth sixth] [list-seventh seventh]
                     [list-eighth eighth] [list-ninth ninth]
                     [list-tenth tenth] [list-last last]
                     [list-length length]) build-list make-list
         list-ref list-set drop List)

(struct: (A) Leaf ([elem  : A]) #:transparent)
(struct: (A) Node ([elem  : A]
                   [left  : (Tree A)]
                   [right : (Tree A)]) #:transparent)

(define-type (Tree A) (U (Leaf A) (Node A)))
(define-type (Root A) (Pair Integer (Tree A)))

(define-type (List A) (Listof (Root A)))

;; An empty list
(define empty null)

;; Checks for empty
(: empty? : (All (A) ((List A) -> Boolean)))
(define (empty? ralist)
  (null? ralist))

;; Similar to list cons function
(: cons  : (All (A) (A (List A) -> (List A))))
(define (cons elem ralist)
  (if (or (null? ralist) (null? (cdr ralist)))
      (rk:cons (rk:cons 1 (Leaf elem)) ralist)
      (let* ([first   (car ralist)]
             [rest    (cdr ralist)]
             [weight1 (car first)]
             [weight2 (car (car rest))])
        (if (eq? weight1 weight2)
            (rk:cons (rk:cons (+ 1 weight1 weight2)
                              (Node elem (cdr first) (cdr (car rest))))
                     (cdr rest))
            (rk:cons (rk:cons 1 (Leaf elem)) ralist)))))

;; Similar to list car function
(: head : (All (A) ((List A) -> A)))
(define (head ralist)
  (if (null? ralist)
      (error 'head "given list is empty")
      (let ([first (cdr (car ralist))])
        (if (Leaf? first)
            (Leaf-elem first)
            (Node-elem first)))))

;; Similar to list cdr function
(: tail : (All (A) ((List A) -> (List A))))
(define (tail ralist)
  (if (null? ralist)
      (error 'tail "given list is empty")
      (let* ([first (cdr (car ralist))]
             [weight (arithmetic-shift (car (car ralist)) -1)]
             [rest-ra (cdr ralist)])
        (if (Leaf? first)
            rest-ra
            (rk:cons (rk:cons weight (Node-left first))
                     (rk:cons (rk:cons weight (Node-right first)) rest-ra))))))

;; Helper for list-ref
(: tree-lookup : (All (A) (Integer (Tree A) Integer -> A)))
(define (tree-lookup weight tree pos)
  (let ([pos0? (zero? pos)])
    (: tl-help : (All (A) (Integer (Node A) -> A)))
    (define (tl-help new-weight tree)
      (cond
        [pos0? (Node-elem tree)]
        [(<= pos new-weight)
         (tree-lookup new-weight (Node-left tree) (sub1 pos))]
        [else (tree-lookup new-weight (Node-right tree) (- pos 1 new-weight))]))
    (cond
      [(and (Leaf? tree) pos0?) (Leaf-elem tree)]
      [(Node? tree) (tl-help (arithmetic-shift weight -1) tree)]
      [else (error 'list-ref "given index out of bounds")])))


;; Helper for list-set
(: tree-update : (All (A) (Integer (Tree A) Integer A -> (Tree A))))
(define (tree-update weight tree pos elem)
  (let ([pos0? (zero? pos)])
    (: tu-help : (All (A) Integer (Node A) -> (Tree A)))
    (define (tu-help new-weight tree)
      (let ([left  (Node-left tree)]
            [right (Node-right tree)]
            [first (Node-elem tree)])
        (cond
          [pos0? (Node elem left right)]
          [(<= pos new-weight)
           (Node first (tree-update new-weight left (sub1 pos) elem) right)]
          [else (Node first left (tree-update new-weight right
                                              (- pos 1 new-weight) elem))])))
    (cond
      [(and (Leaf? tree) pos0?) (Leaf elem)]
      [(Node? tree) (tu-help (arithmetic-shift weight -1) tree)]
      [else (error 'list-set "given index out of bounds")])))




;; Similar to list list-ref function
(: list-ref : (All (A) ((List A) Integer -> A)))
(define (list-ref ralist pos)
  (cond
    [(null? ralist) (error 'list-ref "given index out of bounds")]
    [(< pos (car (car ralist)))
     (tree-lookup (car (car ralist)) (cdr (car ralist)) pos)]
    [else (list-ref (cdr ralist) (- pos (car (car ralist))))]))

;; Similar to list list-set function
(: list-set : (All (A) ((List A) Integer A -> (List A))))
(define (list-set ralist pos elem)
  (cond
    [(null? ralist) (error 'list-set "given index out of bounds")]
    [(< pos (car (car ralist)))
     (rk:cons (rk:cons (car (car ralist)) 
                    (tree-update (car (car ralist))
                                 (cdr (car ralist)) pos elem)) 
              (cdr ralist))]
    [else (rk:cons (car ralist)
                   (list-set (cdr ralist)
                             (- pos (car (car ralist)))
                             elem))]))

;; Helper for drop
(: tree-drop : (All (A) (Integer (Tree A) Integer (List A) -> (List A))))
(define (tree-drop size tre pos ralist)
  (let ([newsize (arithmetic-shift size -1)])
    (cond 
      [(zero? pos) (rk:cons (rk:cons size tre) ralist)]
      [(and (Leaf? tre) (= pos 1)) ralist]
      [(and (Node? tre) (<= pos newsize)) 
       (tree-drop newsize 
                  (Node-left tre) (- pos 1) 
                  (rk:cons (rk:cons newsize (Node-right tre)) ralist))]
      [(and (Node? tre) (> pos newsize)) 
       (tree-drop newsize 
                  (Node-right tre) (- pos 1 newsize) 
                  ralist)]
      [else (error 'drop "not enough elements to drop")])))

;; Similar to list drop function
(: drop : (All (A) (Integer (List A) -> (List A))))
(define (drop pos ralist)
  (cond
    [(zero? pos) ralist]
    [(null? ralist) (error 'drop "not enough elements to drop")]
    [else (drop-helper (car ralist) (cdr ralist) pos)]))

(: drop-helper : (All (A) ((Root A) (List A) Integer -> (List A))))
(define (drop-helper root rest pos)
  (let ([size (car root)]
        [tree (cdr root)])
    (if (< pos size)
        (tree-drop size tree pos rest)
        (drop (- pos size) rest))))

;; Similar to list length function
(: list-length : (All (A) ((List A) -> Integer)))
(define (list-length ralist)
  (foldl + 0 (map (inst car Integer (Tree A)) ralist)))


;; similar to list map function. apply is expensive so using case-lambda
;; in order to saperate the more common case
(: list-map : 
   (All (A B) 
        (case-lambda 
          ((A -> B) (List A) -> (List B))
          ((A A -> B) (List A) (List A) -> (List B))
          ((A A A -> B) (List A) (List A) (List A) -> (List B)))))
(define list-map
  (pcase-lambda: (A B)
                 [([func : (A -> B)]
                   [list : (List A)])
                  (if (empty? list)
                      empty
                      (cons (func (head list)) (list-map func (tail list))))]
                 [([func : (A A -> B)]
                   [list1 : (List A)]
                   [list2 : (List A)])
                  (if (or (empty? list1) (empty? list2))
                      empty
                      (cons (func (head list1) (head list2))
                            (list-map func (tail list1) (tail list2))))]
                 [([func : (A A A -> B)]
                   [list1 : (List A)]
                   [list2 : (List A)]
                   [list3 : (List A)])
                  (if (or (empty? list1) (empty? list2) (empty? list3))
                      empty
                      (cons (func (head list1) (head list2) (head list3))
                            (list-map func (tail list1) (tail list2) (tail list3))))]))


;; Similar to list foldr function. apply is expensive so using case-lambda
;; in order to saperate the more common case
(: list-foldr : 
   (All (A C B ...) 
        (case-lambda ((C A -> C) C (List A) -> C)
                     ((C A B ... B -> C) C (List A) (List B) ... B -> C))))
(define list-foldr
  (pcase-lambda: (A C B ...) 
                 [([func : (C A -> C)]
                   [base : C]
                   [list  : (List A)])
                  (if (empty? list)
                      base
                      (func (list-foldr func base (tail list))
                            (head list)))]
                 [([func : (C A B ... B -> C)]
                   [base : C]
                   [list  : (List A)] . [lists : (List B) ... B])
                  (if (or (empty? list) (ormap empty? lists))
                      base
                      (apply func (apply list-foldr func base (tail list)
                                         (map tail lists))
                             (head list) (map head lists)))]))

;; similar to list foldl function
(: list-foldl : 
   (All (A C B ...) 
        (case-lambda ((C A -> C) C (List A) -> C)
                     ((C A B ... B -> C) C (List A) (List B) ... B -> C))))
(define list-foldl
  (pcase-lambda: (A C B ...) 
                 [([func : (C A -> C)]
                   [base : C]
                   [list  : (List A)])
                  (if (empty? list)
                      base
                      (list-foldl func (func base (head list)) (tail list)))]
                 [([func : (C A B ... B -> C)]
                   [base : C]
                   [list  : (List A)] . [lists : (List B) ... B])
                  (if (or (empty? list) (ormap empty? lists))
                      base
                      (apply list-foldl func
                             (apply func base (head list) (map head lists))
                             (tail list) (map tail lists)))]))

;; RAList to normal list
(: ->list : (All (A) ((List A) -> (Listof A))))
(define (->list ralist)
  (if (empty? ralist)
      null
      (rk:cons (head ralist) (->list (tail ralist)))))

;; list constructor
(: list : (All (A) (A * -> (List A))))
(define (list . lst)
  (foldr (inst cons  A) null lst))

(define first* head)
(define rest* tail)

;; Similar to list filter function
(: filter : (All (A) ((A -> Boolean) (List A) -> (List A))))
(define (filter func ral)
  (if (empty? ral)
      empty
      (let ([head (head ral)]
            [tail (tail ral)])
        (if (func head)
            (cons head (filter func tail))
            (filter func tail)))))

;; Similar to list filter function
(: remove : (All (A) ((A -> Boolean) (List A) -> (List A))))
(define (remove func ral)
  (if (empty? ral)
      empty
      (let ([head (head ral)]
            [tail (tail ral)])
        (if (func head)
            (remove func tail)
            (cons head (remove func tail))))))

;; Similar to list reverse function
(: reverse : (All (A) ((List A) -> (List A))))
(define (reverse ral)
  (: local-reverse : (All (A) ((List A) (List A) -> (List A))))
  (define (local-reverse ral accum)
    (if (empty? ral)
        accum
        (local-reverse (tail ral) (cons (head ral) accum))))
  (local-reverse ral empty))

;; Similar to build-list function of racket list
(: build-list : (All (A) (Natural (Natural -> A) -> (List A))))
(define (build-list size func)
  (let: loop : (List A) ([n : Natural size] [accum : (List A) empty])
        (if (zero? n)
            accum 
            (loop (sub1 n) (cons (func (sub1 n)) accum)))))

;; Similar to make-list function of racket list
(: make-list : (All (A) (Natural A -> (List A))))
(define (make-list size elem)
  (let: loop : (List A) ([n : Natural size] [accum : (List A) empty])
        (if (zero? n)
            accum 
            (loop (sub1 n) (cons elem accum)))))


;; similar to list andmap function
(: list-andmap : 
   (All (A B ...) 
        (case-lambda ((A -> Boolean) (List A) -> Boolean)
                     ((A B ... B -> Boolean) (List A) (List B) ... B -> Boolean))))
(define list-andmap
  (pcase-lambda: (A B ... ) 
                 [([func : (A -> Boolean)]
                   [list  : (List A)])
                  (or (empty? list)
                      (and (func (head list))
                           (list-andmap func (tail list))))]
                 [([func : (A B ... B -> Boolean)]
                   [list  : (List A)] . [lists : (List B) ... B])
                  (or (empty? list) (ormap empty? lists)
                      (and (apply func (head list) (map head lists))
                           (apply list-andmap func (tail list) 
                                  (map tail lists))))]))


;; similar to list ormap function
(: list-ormap : 
   (All (A B ...) 
        (case-lambda ((A -> Boolean) (List A) -> Boolean)
                     ((A B ... B -> Boolean) (List A) (List B) ... B -> Boolean))))
(define list-ormap
  (pcase-lambda: (A B ... ) 
                 [([func : (A -> Boolean)]
                   [list  : (List A)])
                  (and (not (empty? list))
                       (or (func (head list))
                           (list-ormap func (tail list))))]
                 [([func : (A B ... B -> Boolean)]
                   [list  : (List A)] . [lists : (List B) ... B])
                  (and (not (or (empty? list) (ormap empty? lists)))
                       (or (apply func (head list) (map head lists))
                           (apply list-ormap func (tail list) 
                                  (map tail lists))))]))



(: list-second : (All (A) (List A) -> A))
(define (list-second ls) (list-ref ls 1))

(: list-third : (All (A) (List A) -> A))
(define (list-third ls) (list-ref ls 2))

(: list-fourth : (All (A) (List A) -> A))
(define (list-fourth ls) (list-ref ls 3))

(: list-fifth : (All (A) (List A) -> A))
(define (list-fifth ls) (list-ref ls 4))

(: list-sixth : (All (A) (List A) -> A))
(define (list-sixth ls) (list-ref ls 5))

(: list-seventh : (All (A) (List A) -> A))
(define (list-seventh ls) (list-ref ls 6))

(: list-eighth : (All (A) (List A) -> A))
(define (list-eighth ls) (list-ref ls 7))

(: list-ninth : (All (A) (List A) -> A))
(define (list-ninth ls) (list-ref ls 8))

(: list-tenth : (All (A) (List A) -> A))
(define (list-tenth ls) (list-ref ls 9))

(: list-last : (All (A) (List A) -> A))
(define (list-last ls) (list-ref ls (sub1 (list-length ls))))
