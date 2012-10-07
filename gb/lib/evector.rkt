#lang racket/base
(require racket/contract
         racket/match)

(struct evector (make-base
                 base-ref
                 base-set!
                 [base #:mutable]
                 [actual-len #:mutable]
                 [effective-len #:mutable]))

(define (make-evector make-base base-ref base-set! initial-len)
  (evector make-base base-ref base-set!
           (make-base initial-len)
           initial-len
           0))

(define (evector-length ev)
  (evector-effective-len ev))
(define (set-evector-length! ev new-len)
  (match-define (evector make-base base-ref base-set!
                         base actual-len effective-len)
                ev)
  (cond
    [(< new-len actual-len)
     (set-evector-effective-len! ev new-len)]
    [else
     (define next-len (max (* 2 actual-len) new-len))
     (define new-base (make-base next-len))
     (for ([i (in-range effective-len)])
       (base-set! new-base i (base-ref base i)))
     (set-evector-base! ev new-base)
     (set-evector-actual-len! ev next-len)
     (set-evector-effective-len! ev new-len)]))

(define (ensure-k ev k)
  (unless ((evector-length ev) . > . k)
    (error 'evector "index ~e out of bounds" k)))

(define (evector-ref ev k)
  (ensure-k ev k)
  ((evector-base-ref ev)
   (evector-base ev)
   k))
(define (evector-set! ev k val)
  (ensure-k ev k)
  ((evector-base-set! ev)
   (evector-base ev)
   k
   val))
(define (evector-safe-set! ev k val)
  (set-evector-length!
   ev
   (max (add1 k) (evector-length ev)))
  (evector-set! ev k val))

(provide
 (contract-out
  [evector?
   (-> any/c
       boolean?)]
  [make-evector
   (-> (-> exact-nonnegative-integer? any/c)
       (-> any/c exact-nonnegative-integer? any/c)
       (-> any/c exact-nonnegative-integer? any/c void)
       exact-nonnegative-integer?
       evector?)]
  [evector-length
   (-> evector?
       exact-nonnegative-integer?)]
  [set-evector-length!
   (-> evector?
       exact-nonnegative-integer?
       void)]
  [evector-ref
   (-> evector?
       exact-nonnegative-integer?
       any/c)]
  [evector-set!
   (-> evector?
       exact-nonnegative-integer?
       any/c
       void)]
  [evector-safe-set!
   (-> evector?
       exact-nonnegative-integer?
       any/c
       void)]))

(module+ test
  (require rackunit
           racket/function)
  (define N 100)
  (define e (make-evector make-bytes bytes-ref bytes-set! 10))
  (for* ([try (in-range 2)]
         [i (in-range N)])
    (set-evector-length! e (add1 i))
    (check-equal? (evector-length e) (add1 i)
                  (format "~a len" i))
    (evector-set! e i i)
    (for ([j (in-range N)])
      (if (j . <= . i)
        (check-equal? (evector-ref e j) j
                      (format "~a ~a valid" i j))
        (check-exn exn:fail?
                   (λ () (evector-ref  e j))
                   (format "~a ~a invalid" i j)))))
  (define ep (make-evector make-bytes bytes-ref bytes-set! 10))
  (define order (build-list N (λ (i) (random N))))
  (for ([i (in-list order)]
        [which (in-naturals)])
    (evector-safe-set! ep i i)
    (for ([j (in-list order)]
          [_ (in-range which)])
      (check-equal? (evector-ref ep j) j
                    (format "~a ~a valid" i j)))))
