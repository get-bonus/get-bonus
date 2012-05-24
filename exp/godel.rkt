#lang racket/base
(require racket/list
         racket/function
         racket/match)

;; XXX Need to review the paper to double check each representation

(module+ test
  (require rackunit)
  (define SHOW? #f)
  (define (show v bs)
    (define-values (bs-n bs-p) (decode-binary-nat (length bs) bs))
    (printf "~v = ~v = ~v\n"
            v
            bs
            bs-n))
  (define (random-bits)
    (if (zero? (random 2))
      empty
      (cons (random 2)
            (random-bits))))
  (define (number->bits n)
    (encode-binary-nat (integer-length n) n))
  (define-syntax-rule (round-trips encode-e decode-e example ...)
    (begin (define encode encode-e)
           (define (encode/show v)
             (define ans (encode v))
             (show v ans)
             ans)
           (define decode decode-e)
           (define (decode/show bs)
             (define-values (v bs-p) (decode bs))
             (show v bs)
             (values v bs-p))
           (test-not-exn (format "~a / empty" 'decode-e)
                         (λ () (decode/show empty)))
           (let ()
             (define input example)
             (define bs (encode/show input))
             (define-values (output bs-p) (decode/show bs))
             (test-equal? (format "~a -> ~a / ~a" 'encode-e 'decode-e input)
                          output input))
           ...
           (test-not-exn (format "~a / random list" 'decode-e)
                         (λ () (decode/show (random-bits))))
           (for ([i (in-range 32)])
             (test-not-exn (format "~a / number: ~a" 'decode-e i)
                           (λ () (decode/show (number->bits i)))))
           (define r (random 1024))
           (test-not-exn (format "~a / random number: ~a" 'decode-e r)
                         (λ () (decode/show (number->bits r)))))))

(define (encode-unary-nat n)
  (if (zero? n)
    (list 0)
    (cons 1 (encode-unary-nat (sub1 n)))))
(define (decode-unary-nat bs)
  (match bs
    ;; XXX is this correct?
    [(or (and bs (list)) (cons 0 bs))
     (values 0 bs)]
    [(cons 1 bs)
     (define-values (n bs-p) (decode-unary-nat bs))
     (values (add1 n) bs-p)]))

(module+ test
  (round-trips encode-unary-nat decode-unary-nat
               0 1 2 3 4 5 6 7 8 9 10
               (random 1024)))

(define (encode-binary-nat len n)
  (cond
    [(zero? len)
     empty]
    [else
     (define-values (q r) (quotient/remainder n 2))
     (cons
      (if (= r 1) 1 0)
      (encode-binary-nat (sub1 len) q))]))
(define (decode-binary-nat len bs)
  (cond
    [(zero? len)
     (values 0 bs)]
    [else
     (match bs
       ;; XXX is this correct?
       [(or (and (list) bs-p
                 (app (λ _ 0) bit))
            (cons bit bs-p))
        (define-values (n bs-pp) (decode-binary-nat (sub1 len) bs-p))
        (values (+ (* 2 n) bit) bs-pp)])]))

(module+ test
  (require racket/function)
  (round-trips (curry encode-binary-nat 10) (curry decode-binary-nat 10)
               0 1 2 3 4 5 6 7 8 9 10
               (random 1024)))

(define (encode-integer n)
  (define len (integer-length n))
  (append
   (encode-unary-nat len)
   (encode-binary-nat len n)))
(define (decode-integer bs)
  (define-values (len bs-p) (decode-unary-nat bs))
  (define-values (n bs-pp) (decode-binary-nat len bs-p))
  (values n bs-pp))

(module+ test
  (round-trips encode-integer decode-integer
               0 1 2 3 4 5 6 7 8 9 10
               (random 1024)))

(define (encode-pair encode-l encode-r p)
  (match-define (cons l r) p)
  (fuse (encode-l l)
        (encode-r r)))
(define (fuse evens odds)
  (match* (evens odds)
    [((list) (list))
     (list)]
    [((list) (cons o odds))
     (list* 0 o (fuse evens odds))]
    [((cons e evens) (list))
     (list* e 0 (fuse evens odds))]
    [((cons e evens) (cons o odds))
     (list* e o (fuse evens odds))]))

(define (decode-pair decode-l decode-r bs)
  (define-values (l-bs r-bs) (defuse bs))
  (define-values (l l-bs-p) (decode-l l-bs))
  (define-values (r r-bs-p) (decode-r r-bs))
  (values (cons l r) (fuse l-bs-p r-bs-p)))
(define (defuse bs)
  (match bs
    [(list)
     (values empty empty)]
    ;; XXX is this correct?
    [(or (and (list e)
              (app (λ _ 0) o)
              (app (λ _ empty) bs))
         (list* e o bs))
     (define-values (evens odds) (defuse bs))
     (values (list* e evens) (list* o odds))]))

(module+ test
  (round-trips (curry encode-pair encode-integer encode-integer)
               (curry decode-pair decode-integer decode-integer)
               (cons 0 0) (cons 0 1) (cons 1 1)
               (cons 3 0) (cons 7 1) (cons 9 1)
               (cons 5 0) (cons 3 1) (cons 10 1)
               (cons 4 0) (cons 1 1) (cons 8 1)
               (cons (random 1024) (random 1024))))

(define (encode-union l? encode-l r? encode-r l-or-r)
  (cond
    [(l? l-or-r)
     (cons 0 (encode-l l-or-r))]
    [else
     (cons 1 (encode-r l-or-r))]))
(define (decode-union decode-l decode-r bs)
  (match bs
    ;; XXX is this correct?
    [(or (and (list) bs) (cons 0 bs))
     (decode-l bs)]
    [(cons 1 bs)
     (decode-r bs)]))

(module+ test
  (round-trips (curry encode-union
                      pair? (curry encode-pair encode-integer encode-integer)
                      exact-integer? encode-integer)
               (curry decode-union
                      (curry decode-pair decode-integer decode-integer)
                      decode-integer)
               0 1 2 3 4 5 6 7 8 9 10 (random 1024)
               (cons 0 0) (cons 0 1) (cons 1 1)
               (cons 3 0) (cons 7 1) (cons 9 1)
               (cons 5 0) (cons 3 1) (cons 10 1)
               (cons 4 0) (cons 1 1) (cons 8 1)
               (cons (random 1024) (random 1024))))

(define (encode-const val some-val)
  empty)
(define (decode-const val some-bs)
  (values val some-bs))
(module+ test
  (round-trips (curry encode-const empty) (curry decode-const empty)
               empty))

(define (encode-list encode-e l)
  (encode-union empty? (curry encode-const empty)
                pair? (curry encode-pair encode-e (curry encode-list encode-e))
                l))
(define (decode-list decode-e bs)
  (decode-union (curry decode-const empty)
                (curry decode-pair decode-e (curry decode-list decode-e))
                bs))
(module+ test
  (round-trips (curry encode-list encode-integer)
               (curry decode-list decode-integer)
               empty
               (list 1)
               (list (random 1024))
               (list 1 2)
               (list 1 2 3 4)
               (list 5 9 10 23 24 18 9 20)))

;; XXX other numbers

;; XXX strings

;; XXX bytes

;; XXX characters

;; XXX symbols

;; XXX vectors

;; XXX hash tables

;; XXX structs

;; XXX void

;; XXX data-types

;; XXX TRT
