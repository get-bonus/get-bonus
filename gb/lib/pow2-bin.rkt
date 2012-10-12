#lang racket/base
(require racket/match
         racket/contract)
(module+ test
  (require rackunit))

(struct tree (pow) #:transparent)
(struct tree-empty () #:transparent)
(struct tree-singleton tree (data) #:transparent)
(struct tree-branch tree (ul ur ll lr) #:transparent)

(define tree-size
  (match-lambda
   [(tree-empty) 0]
   [(tree pow) pow]))
(module+ test
  (check-equal? (tree-size (tree-empty)) 0)
  (check-equal? (tree-size (tree-singleton 1 #t)) 1)
  (check-equal? (tree-size (tree-singleton 2 #t)) 2)
  (check-equal? (tree-size (tree-branch 2
                                        (tree-empty) (tree-empty)
                                        (tree-empty) (tree-empty))) 2))

(define (tree-has-space-for? left-t right-size)
  (match left-t
    [(tree-empty) #t]
    [(tree-singleton _ _) #f]
    [(tree-branch left-size ul ur ll lr)
     (cond
       [(> right-size left-size) #f]
       [(= right-size left-size) #f]
       [else
        (ormap (λ (x) (tree-has-space-for? x right-size))
               (list ul ur ll lr))])]))
(module+ test
  (check-equal?
   (tree-has-space-for?
    (tree-branch 7
                 (tree-singleton 6 '(6 . 50))
                 (tree-singleton 6 '(14 . 49))
                 (tree-singleton 6 '(4 . 41))
                 (tree-branch 6
                              (tree-singleton 5 '(31 . 20))
                              (tree-empty)
                              (tree-empty)
                              (tree-empty)))
    (tree-size
     (tree-branch 6
                  (tree-singleton 5 '(15 . 29))
                  (tree-empty)
                  (tree-empty)
                  (tree-empty))))
   #f)
  (check-equal?
   (tree-has-space-for?
    (tree-branch 7
                 (tree-singleton 6 '(6 . 50))
                 (tree-singleton 6 '(14 . 49))
                 (tree-singleton 6 '(4 . 41))
                 (tree-branch 6
                              (tree-singleton 5 '(31 . 20))
                              (tree-empty)
                              (tree-empty)
                              (tree-empty)))
    (tree-size
     (tree-singleton 5 '(15 . 29))))
   #t)

  (check-equal? (tree-has-space-for? (tree-empty)
                                     (random 100)) #t)
  (check-equal? (tree-has-space-for? (tree-singleton (random 100) #f)
                                     0) #f)
  (check-equal? (tree-has-space-for? (tree-singleton (random 100) #f)
                                     (random 100)) #f)
  (check-equal? (tree-has-space-for?
                 (tree-branch 1
                              (tree-empty) (tree-empty)
                              (tree-empty) (tree-empty))
                 2)
                #f)
  (check-equal? (tree-has-space-for?
                 (tree-branch 1
                              (tree-empty) (tree-empty)
                              (tree-empty) (tree-empty))
                 1)
                #f)
  (check-equal? (tree-has-space-for?
                 (tree-branch 2
                              (tree-empty) (tree-empty)
                              (tree-empty) (tree-empty))
                 1)
                #t)
  (check-equal? (tree-has-space-for?
                 (tree-branch 2
                              (tree-singleton 1 #f) (tree-empty)
                              (tree-empty) (tree-empty))
                 1)
                #t)
  (check-equal? (tree-has-space-for?
                 (tree-branch 2
                              (tree-singleton 1 #f) (tree-singleton 1 #f)
                              (tree-empty) (tree-empty))
                 1)
                #t)
  (check-equal? (tree-has-space-for?
                 (tree-branch 2
                              (tree-singleton 1 #f) (tree-singleton 1 #f)
                              (tree-singleton 1 #f) (tree-empty))
                 1)
                #t)
  (check-equal? (tree-has-space-for?
                 (tree-branch 2
                              (tree-singleton 1 #f) (tree-singleton 1 #f)
                              (tree-singleton 1 #f) (tree-singleton 1 #f))
                 1)
                #f))

(define (tree-insert-at expected-size left-t right-t)
  (match left-t
    [(tree-empty)
     (define right-size (tree-size right-t))
     (cond
       [(< expected-size right-size)
        (error 'tree-insert-at "Expectation too big")]
       [(= expected-size right-size)
        right-t]
       [else
        (tree-branch expected-size
                     (tree-insert-at (sub1 expected-size)
                                     (tree-empty)
                                     right-t)
                     (tree-empty)
                     (tree-empty)
                     (tree-empty))])]
    [(tree-singleton _ _)
     (error 'tree-insert-at "Cannot insert into singleton")]
    [(tree-branch left-size _ _ _ _)
     (cond
       [(> expected-size left-size)
        (error 'tree-insert-at "Expectation too big")]
       [else
        (tree-insert left-t right-t)])]))

(module+ test
  (check-exn exn:fail?
             (λ ()
               (tree-insert-at 0 (tree-empty) (tree-singleton 1 #f))))

  (check-equal? (tree-insert-at 1 (tree-empty) (tree-singleton 1 #f))
                (tree-singleton 1 #f))
  (check-equal? (tree-insert-at 2 (tree-empty) (tree-singleton 1 #f))
                (tree-branch 2
                             (tree-singleton 1 #f)
                             (tree-empty)
                             (tree-empty)
                             (tree-empty)))
  (check-equal? (tree-insert-at 3 (tree-empty) (tree-singleton 1 #f))
                (tree-branch 3
                             (tree-branch 2
                                          (tree-singleton 1 #f)
                                          (tree-empty)
                                          (tree-empty)
                                          (tree-empty))
                             (tree-empty)
                             (tree-empty)
                             (tree-empty)))

  (check-exn exn:fail?
             (λ ()
               (tree-insert-at (random 100)
                               (tree-singleton (random 100) #f)
                               (tree-singleton (random 100) #f))))

  (check-exn exn:fail?
             (λ ()
               (tree-insert-at 3 (tree-branch 2
                                              (tree-empty) (tree-empty)
                                              (tree-empty) (tree-empty))
                               (tree-singleton 1 #f))))

  (check-equal? (tree-insert-at 1 (tree-branch 2
                                               (tree-empty) (tree-empty)
                                               (tree-empty) (tree-empty))
                                (tree-singleton 1 #f))
                (tree-branch 2
                             (tree-singleton 1 #f) (tree-empty)
                             (tree-empty) (tree-empty)))
  (check-equal? (tree-insert-at 1 (tree-branch 3
                                               (tree-empty) (tree-empty)
                                               (tree-empty) (tree-empty))
                                (tree-singleton 1 #f))
                (tree-branch 3
                             (tree-branch 2
                                          (tree-singleton 1 #f) (tree-empty)
                                          (tree-empty) (tree-empty))
                             (tree-empty)
                             (tree-empty) (tree-empty))))

(define (tree-insert left-t right-t)
  (match left-t
    [(tree-empty)
     right-t]
    [(tree-singleton _ _)
     (error 'tree-insert "Cannot insert into singleton")]
    [(tree-branch left-size ul ur ll lr)
     (define right-size (tree-size right-t))
     (cond
       [(>= right-size left-size)
        (error 'tree-insert "No space in tree-branch: ~v" (list left-t right-t))]
       [(tree-has-space-for? ul right-size)
        (tree-branch left-size
                     (tree-insert-at (sub1 left-size) ul right-t)
                     ur ll lr)]
       [(tree-has-space-for? ur right-size)
        (tree-branch left-size
                     ul
                     (tree-insert-at (sub1 left-size) ur right-t)
                     ll lr)]
       [(tree-has-space-for? ll right-size)
        (tree-branch left-size
                     ul ur
                     (tree-insert-at (sub1 left-size) ll right-t)
                     lr)]
       [(tree-has-space-for? lr right-size)
        (tree-branch left-size
                     ul ur ll
                     (tree-insert-at (sub1 left-size) lr right-t))]
       [else
        (error 'tree-insert "No space in tree-branch: ~v" (list left-t right-t))])]))

(module+ test
  (check-equal? (tree-insert (tree-branch 3
                                          (tree-empty)
                                          (tree-empty)
                                          (tree-empty)
                                          (tree-empty))
                             (tree-singleton 1 #f))
                (tree-branch 3
                             (tree-branch 2
                                          (tree-singleton 1 #f)
                                          (tree-empty)
                                          (tree-empty)
                                          (tree-empty))
                             (tree-empty)
                             (tree-empty)
                             (tree-empty)))

  (check-equal?
   (tree-insert
    (tree-branch 7
                 (tree-singleton 6 '(6 . 50))
                 (tree-singleton 6 '(14 . 49))
                 (tree-singleton 6 '(4 . 41))
                 (tree-branch 6
                              (tree-singleton 5 '(31 . 20))
                              (tree-empty)
                              (tree-empty)
                              (tree-empty)))
    (tree-singleton 5 '(15 . 29)))
   (tree-branch 7
                (tree-singleton 6 '(6 . 50))
                (tree-singleton 6 '(14 . 49))
                (tree-singleton 6 '(4 . 41))
                (tree-branch 6
                             (tree-singleton 5 '(31 . 20))
                             (tree-singleton 5 '(15 . 29))
                             (tree-empty)
                             (tree-empty))))

  (check-exn
   exn:fail?
   (λ ()
     (tree-insert
      (tree-branch 7
                   (tree-singleton 6 '(6 . 50))
                   (tree-singleton 6 '(14 . 49))
                   (tree-singleton 6 '(4 . 41))
                   (tree-branch 6
                                (tree-singleton 5 '(31 . 20))
                                (tree-empty)
                                (tree-empty)
                                (tree-empty)))
      (tree-branch 6
                   (tree-singleton 5 '(15 . 29))
                   (tree-empty)
                   (tree-empty)
                   (tree-empty)))))

  (check-equal? (tree-insert (tree-empty) (tree-empty))
                (tree-empty))
  (check-equal? (tree-insert (tree-empty) (tree-singleton 100 #f))
                (tree-singleton 100 #f))
  (check-equal? (tree-insert (tree-empty)
                             (tree-branch 1
                                          (tree-empty)
                                          (tree-empty)
                                          (tree-empty)
                                          (tree-empty)))
                (tree-branch 1
                             (tree-empty)
                             (tree-empty)
                             (tree-empty)
                             (tree-empty)))

  (check-exn exn:fail?
             (λ () (tree-insert (tree-singleton (random 100) #f)
                                (tree-empty))))
  (check-exn exn:fail?
             (λ () (tree-insert (tree-singleton (random 100) #f)
                                (tree-singleton (random 100) #f))))
  (check-exn exn:fail?
             (λ () (tree-insert (tree-singleton (random 100) #f)
                                (tree-branch 1
                                             (tree-empty)
                                             (tree-empty)
                                             (tree-empty)
                                             (tree-empty)))))

  (check-exn exn:fail?
             (λ ()
               (tree-insert
                (tree-branch 1
                             (tree-empty) (tree-empty)
                             (tree-empty) (tree-empty))
                (tree-singleton 2 #f))))
  (check-exn exn:fail?
             (λ ()
               (tree-insert
                (tree-branch 1
                             (tree-empty) (tree-empty)
                             (tree-empty) (tree-empty))
                (tree-singleton 1 #f))))
  (check-equal? (tree-insert
                 (tree-branch 2
                              (tree-empty) (tree-empty)
                              (tree-empty) (tree-empty))
                 (tree-singleton 1 #f))
                (tree-branch 2
                             (tree-singleton 1 #f) (tree-empty)
                             (tree-empty) (tree-empty)))
  (check-equal? (tree-insert
                 (tree-branch 2
                              (tree-singleton 1 #f) (tree-empty)
                              (tree-empty) (tree-empty))
                 (tree-singleton 1 #f))
                (tree-branch 2
                             (tree-singleton 1 #f) (tree-singleton 1 #f)
                             (tree-empty) (tree-empty)))
  (check-equal? (tree-insert
                 (tree-branch 2
                              (tree-singleton 1 #f) (tree-singleton 1 #f)
                              (tree-empty) (tree-empty))
                 (tree-singleton 1 #f))
                (tree-branch 2
                             (tree-singleton 1 #f) (tree-singleton 1 #f)
                             (tree-singleton 1 #f) (tree-empty)))
  (check-equal? (tree-insert
                 (tree-branch 2
                              (tree-singleton 1 #f) (tree-singleton 1 #f)
                              (tree-singleton 1 #f) (tree-empty))
                 (tree-singleton 1 #f))
                (tree-branch 2
                             (tree-singleton 1 #f) (tree-singleton 1 #f)
                             (tree-singleton 1 #f) (tree-singleton 1 #f)))
  (check-exn exn:fail?
             (λ ()
               (tree-insert
                (tree-branch 2
                             (tree-singleton 1 #f) (tree-singleton 1 #f)
                             (tree-singleton 1 #f) (tree-singleton 1 #f))
                (tree-singleton 1 #f)))))

(define (tree-join left-t right-t)
  (cond
    [(tree-empty? left-t)
     right-t]
    [(tree-empty? right-t)
     left-t]
    [(tree-has-space-for? left-t (tree-size right-t))
     (tree-insert left-t right-t)]
    [else
     (define left-size (tree-size left-t))
     (define right-size (tree-size right-t))
     (define bigger (max left-size right-size))
     (define next (add1 bigger))
     (tree-join
      (tree-join
       (tree-branch next
                    (tree-empty) (tree-empty)
                    (tree-empty) (tree-empty))
       left-t)
      right-t)]))

(module+ test
  ;; empty idempotent
  (check-equal? (tree-join (tree-empty) (tree-empty))
                (tree-empty))
  (check-equal? (tree-join (tree-singleton 1 #t) (tree-empty))
                (tree-singleton 1 #t))
  (check-equal? (tree-join (tree-empty) (tree-singleton 1 #t))
                (tree-singleton 1 #t))

  ;; filling a branch
  (check-equal? (tree-join
                 (tree-branch 2
                              (tree-empty)
                              (tree-empty)
                              (tree-empty)
                              (tree-empty))
                 (tree-singleton 1 'ul))
                (tree-branch 2
                             (tree-singleton 1 'ul)
                             (tree-empty)
                             (tree-empty)
                             (tree-empty)))
  (check-equal? (tree-join
                 (tree-branch 2
                              (tree-singleton 1 'ul)
                              (tree-empty)
                              (tree-empty)
                              (tree-empty))
                 (tree-singleton 1 'ur))
                (tree-branch 2
                             (tree-singleton 1 'ul)
                             (tree-singleton 1 'ur)
                             (tree-empty)
                             (tree-empty)))
  (check-equal? (tree-join
                 (tree-branch 2
                              (tree-singleton 1 'ul)
                              (tree-singleton 1 'ur)
                              (tree-empty)
                              (tree-empty))
                 (tree-singleton 1 'll))
                (tree-branch 2
                             (tree-singleton 1 'ul)
                             (tree-singleton 1 'ur)
                             (tree-singleton 1 'll)
                             (tree-empty)))
  (check-equal? (tree-join
                 (tree-branch 2
                              (tree-singleton 1 'ul)
                              (tree-singleton 1 'ur)
                              (tree-singleton 1 'll)
                              (tree-empty))
                 (tree-singleton 1 'lr))
                (tree-branch 2
                             (tree-singleton 1 'ul)
                             (tree-singleton 1 'ur)
                             (tree-singleton 1 'll)
                             (tree-singleton 1 'lr)))

  ;; out of space
  (check-equal? (tree-join
                 (tree-branch 2
                              (tree-singleton 1 'ul)
                              (tree-singleton 1 'ur)
                              (tree-singleton 1 'll)
                              (tree-singleton 1 'lr))
                 (tree-singleton 1 'more))
                (tree-branch 3
                             (tree-branch 2
                                          (tree-singleton 1 'ul)
                                          (tree-singleton 1 'ur)
                                          (tree-singleton 1 'll)
                                          (tree-singleton 1 'lr))
                             (tree-branch 2
                                          (tree-singleton 1 'more)
                                          (tree-empty)
                                          (tree-empty)
                                          (tree-empty))
                             (tree-empty)
                             (tree-empty)))

  ;; XXX joining branches

  ;; joining singletons
  ;;; same size
  (check-equal? (tree-join (tree-singleton 1 'left)
                           (tree-singleton 1 'right))
                (tree-branch 2
                             (tree-singleton 1 'left)
                             (tree-singleton 1 'right)
                             (tree-empty)
                             (tree-empty)))
  ;;; XXX left bigger
  ;;; XXX right bigger

  (check-equal?
   (tree-join
    (tree-branch 7
                 (tree-singleton 6 '(6 . 50))
                 (tree-singleton 6 '(14 . 49))
                 (tree-singleton 6 '(4 . 41))
                 (tree-branch 6
                              (tree-singleton 5 '(31 . 20))
                              (tree-empty)
                              (tree-empty)
                              (tree-empty)))
    (tree-singleton 5 '(15 . 29)))
   (tree-branch 7
                (tree-singleton 6 '(6 . 50))
                (tree-singleton 6 '(14 . 49))
                (tree-singleton 6 '(4 . 41))
                (tree-branch 6
                             (tree-singleton 5 '(31 . 20))
                             (tree-singleton 5 '(15 . 29))
                             (tree-empty)
                             (tree-empty))))
  (check-equal?
   (tree-join
    (tree-branch 7
                 (tree-singleton 6 '(6 . 50))
                 (tree-singleton 6 '(14 . 49))
                 (tree-singleton 6 '(4 . 41))
                 (tree-branch 6
                              (tree-singleton 5 '(31 . 20))
                              (tree-empty)
                              (tree-empty)
                              (tree-empty)))
    (tree-branch 6
                 (tree-singleton 5 '(15 . 29))
                 (tree-empty)
                 (tree-empty)
                 (tree-empty)))
   (tree-branch 8
                (tree-branch 7
                             (tree-singleton 6 '(6 . 50))
                             (tree-singleton 6 '(14 . 49))
                             (tree-singleton 6 '(4 . 41))
                             (tree-branch 6
                                          (tree-singleton 5 '(31 . 20))
                                          (tree-empty)
                                          (tree-empty)
                                          (tree-empty)))
                (tree-branch 7
                             (tree-branch 6
                                          (tree-singleton 5 '(15 . 29))
                                          (tree-empty)
                                          (tree-empty)
                                          (tree-empty))
                             (tree-empty)
                             (tree-empty)
                             (tree-empty))
                (tree-empty)
                (tree-empty)))

  (check-equal?
   (tree-join
    (tree-branch
     9
     (tree-branch
      8
      (tree-branch
       7
       (tree-singleton 6 '(39 . 23))
       (tree-singleton 6 '(53 . 34))
       (tree-singleton 6 '(24 . 58))
       (tree-empty))
      (tree-singleton 7 '(83 . 97))
      (tree-singleton 7 '(99 . 69))
      (tree-singleton 7 '(87 . 73)))
     (tree-branch
      8
      (tree-singleton 7 '(84 . 1))
      (tree-singleton 7 '(73 . 65))
      (tree-empty)
      (tree-empty))
     (tree-empty)
     (tree-empty))
    (tree-singleton 6 '(34 . 2)))
   (tree-branch
    9
    (tree-branch
     8
     (tree-branch
      7
      (tree-singleton 6 '(39 . 23))
      (tree-singleton 6 '(53 . 34))
      (tree-singleton 6 '(24 . 58))
      (tree-singleton 6 '(34 . 2)))
     (tree-singleton 7 '(83 . 97))
     (tree-singleton 7 '(99 . 69))
     (tree-singleton 7 '(87 . 73)))
    (tree-branch
     8
     (tree-singleton 7 '(84 . 1))
     (tree-singleton 7 '(73 . 65))
     (tree-empty)
     (tree-empty))
    (tree-empty)
    (tree-empty))))

(define (num->pow2 n)
  (inexact->exact
   (ceiling
    (/ (log n)
       (log 2)))))
(module+ test
  (check-equal? (num->pow2 1) 0)
  (check-equal? (num->pow2 2) 1)
  (check-equal? (num->pow2 3) 2)
  (check-equal? (num->pow2 4) 2)
  (check-equal? (num->pow2 5) 3)
  (check-equal? (num->pow2 10) 4))

(define (pack e-x e-y l)
  (for/fold ([t (tree-empty)])
      ([i (in-list l)])
    (define x (e-x i))
    (define y (e-y i))
    (define pow
      (max (num->pow2 x)
           (num->pow2 y)))
    (tree-join t
               (tree-singleton pow i))))

(provide
 (struct-out tree-singleton)
 (struct-out tree-empty)
 (struct-out tree-branch)
 (contract-out
  [tree-size
   (-> tree?
       number?)]
  [pack
   (-> (-> any/c number?)
       (-> any/c number?)
       (listof any/c)
       tree?)]))

(module+ main
  (define N 30)

  (define input
    (build-list N
                (λ (i)
                  (cons (add1 (random 100))
                        (add1 (random 100))))))

  (define ans
    (for/fold ([t (tree-empty)])
        ([i (in-list input)])
      (define x (car i))
      (define y (cdr i))
      (define pow
        (max (num->pow2 x)
             (num->pow2 y)))
      (printf "~ax~a < 2^~a = ~a\n"
              x y pow (expt 2 pow))

      (with-handlers ([exn:fail?
                       (λ (x)
                         (pretty-print
                          `(check-equal?
                            (tree-join ,t
                                       (tree-singleton ,pow ,i))
                            #f))
                         (raise x))])
        (tree-join t
                   (tree-singleton pow i)))))

  (require racket/pretty)
  (pretty-print ans))
