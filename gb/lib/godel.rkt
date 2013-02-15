#lang racket/base
(require racket/match
         racket/contract
         racket/function
         racket/list
         math/number-theory)
(module+ test
  (require rackunit)
  (define N 10))

;; The core: Pairing functions
(define (core-nat-cons x y)
  (arithmetic-shift (bitwise-ior 1 (arithmetic-shift y 1))
                    x))

(define (core-nat-hd n)
  (unless (> n 0)
    (error 'core-nat-hd "Cannot take the head of 0"))
  (if (= 1 (bitwise-and n 1))
    0
    (add1 (core-nat-hd (arithmetic-shift n -1)))))

(define (core-nat-tl n)
  (arithmetic-shift n (* -1 (add1 (core-nat-hd n)))))

(define (nat-cons x y)
  (sub1 (core-nat-cons x y)))
(define (nat-hd z)
  (core-nat-hd (add1 z)))
(define (nat-tl z)
  (core-nat-tl (add1 z)))

(define (pair hd-k tl-k hd tl)
  (match* (hd-k tl-k)
    [(+inf.0 +inf.0)
     (nat-cons hd tl)]
    [(+inf.0 tl-k)
     (+ (* hd tl-k) tl)]
    [(hd-k +inf.0)
     (+ hd (* tl hd-k))]
    [(hd-k tl-k)
     (+ hd (* tl hd-k))]))
(define (pair-hd hd-k tl-k n)
  (match* (hd-k tl-k)
    [(+inf.0 +inf.0)
     (nat-hd n)]
    [(+inf.0 tl-k)
     (quotient n tl-k)]
    [(hd-k +inf.0)
     (remainder n hd-k)]
    [(hd-k tl-k)
     (remainder n hd-k)]))
(define (pair-tl hd-k tl-k n)
  (match* (hd-k tl-k)
    [(+inf.0 +inf.0)
     (nat-tl n)]
    [(+inf.0 tl-k)
     (remainder n tl-k)]
    [(hd-k +inf.0)
     (quotient n hd-k)]
    [(hd-k tl-k)
     (quotient n hd-k)]))

(module+ test
  (for ([i (in-range N)])
    (define fst (random (* N N)))
    (define snd (random (* N N)))
    (define n (nat-cons fst snd))
    (test-equal? (format "~a,~a" fst snd) (nat-hd n) fst)
    (test-equal? (format "~a,~a" fst snd) (nat-tl n) snd)))

;; Encoding
(struct spec (k in out) #:transparent)

(define (encode spec v)
  ((spec-out spec) v))
(define (decode spec n)
  ((spec-in spec) n))
(module+ test
  (define-syntax-rule (test-en/de s-e v-e)
    (let ()
      (define n 's-e)
      (define s s-e)
      (define v v-e)
      (test-equal? (format "s=~a v=~a" n v)
                   (decode s (encode s v))
                   v)))
  (define-syntax-rule (test-spec s-e)
    (let ()
      (define n 's-e)
      (define s s-e)
      (for ([i (in-range (min N (spec-k s)))])
        (define v (decode s i))
        (test-equal? (format "n=~a i=~a v=~a" n i v)
                     (encode s v) i))))
  (define-syntax-rule (test-spec-ex s-e v-e n-e)
    (let ()
      (define v v-e)
      (define n n-e)
      (define s s-e)
      (test-equal? (format "encode ~a ~a = ~a" 's-e v n) (encode s v) n)
      (test-equal? (format "decode ~a ~a = ~a" 's-e n v) (decode s n) v)))
  (define-syntax-rule (test-spec-exs s-e [v n] ...)
    (let ()
      (test-spec-ex s-e v n)
      ...)))

;; Specs
(define null/s
  (spec 0 error error))
(define (unit/s v)
  (spec 1 (λ (n) v) (λ (v) 0)))
(module+ test
  (define empty/s (unit/s empty))
  (test-spec empty/s)
  (for ([i (in-range N)])
    (test-en/de empty/s empty)))

(define nat/s
  (spec +inf.0 identity identity))
(module+ test
  (test-spec nat/s)
  (for ([i (in-range N)])
    (test-en/de nat/s (random (* N N)))))

(define (nat-range/s k)
  (spec k identity identity))
(module+ test
  (test-spec (nat-range/s N))
  (for ([i (in-range N)])
    (test-en/de (nat-range/s N) i)))

(define (cons/s hd/s tl/s)
  (match-define (spec hd-k hd-in hd-out) hd/s)
  (match-define (spec tl-k tl-in tl-out) tl/s)
  (spec (* hd-k tl-k)
        (λ (n)
          (cons (hd-in (pair-hd hd-k tl-k n))
                (tl-in (pair-tl hd-k tl-k n))))
        (λ (v)
          (pair hd-k tl-k
                (hd-out (car v))
                (tl-out (cdr v))))))
(module+ test
  (define 2nats/s (cons/s nat/s nat/s))
  (test-spec 2nats/s)
  (for ([i (in-range N)])
    (test-en/de 2nats/s
                (cons (random (* N N))
                      (random (* N N))))))

(define (or/s left? left/s right? right/s)
  (match-define (spec left-k left-in left-out) left/s)
  (match-define (spec right-k right-in right-out) right/s)
  (match* (left-k right-k)
    [(+inf.0 +inf.0)
     (spec +inf.0
           (λ (n)
             (match (pair-hd 2 +inf.0 n)
               [0
                (left-in (pair-tl 2 +inf.0 n))]
               [1
                (right-in (pair-tl 2 +inf.0 n))]))
           (λ (v)
             (match v
               [(? left?)
                (pair 2 +inf.0 0 (left-out v))]
               [(? right?)
                (pair 2 +inf.0 1 (right-out v))])))]
    [(+inf.0 right-k)
     (or/s right? right/s left? left/s)]
    [(left-k right-k)
     (spec (+ left-k right-k)
           (λ (n)
             (if (< n left-k)
               (left-in n)
               (right-in (- n left-k))))
           (λ (v)
             (match v
               [(? left?)
                (left-out v)]
               [(? right?)
                (+ (right-out v) left-k)])))]))

(module+ test
  (define int/s
    (or/s exact-nonnegative-integer? nat/s
          negative? (wrap/s (wrap/s nat/s
                                    (λ (n) (* -1 n))
                                    (λ (n) (* -1 n)))
                            (λ (n) (- n 1))
                            (λ (n) (+ n 1)))))
  (test-spec int/s)
  (for ([i (in-range N)])
    (test-en/de int/s
                (if (zero? (random 2))
                  (add1 (random (* N N)))
                  (* -1 (add1 (random (* N N)))))))

  (define weird-nat/s
    (or/s (λ (i) (<= 0 i 3)) (enum/s '(0 1 2 3))
          (λ (i) (< 3 i)) (wrap/s nat/s
                                  (λ (n) (+ n 4))
                                  (λ (n) (- n 4)))))
  (test-spec weird-nat/s)
  (for ([i (in-range N)])
    (test-en/de weird-nat/s
                (random (* N N))))

  (define weird-nat/s-2
    (or/s (λ (i) (< 3 i)) (wrap/s nat/s
                                  (λ (n) (+ n 4))
                                  (λ (n) (- n 4)))
          (λ (i) (<= 0 i 3)) (enum/s '(0 1 2 3))))
  (test-spec weird-nat/s-2)
  (for ([i (in-range N)])
    (test-en/de weird-nat/s-2
                (random (* N N))))

  (define weird-nat/s-3
    (or/s (λ (i) (<= 0 i 3)) (enum/s '(0 1 2 3))
          (λ (i) (<= 4 i 6)) (enum/s '(4 5 6))))
  (test-spec weird-nat/s-3)
  (for ([i (in-range N)])
    (test-en/de weird-nat/s-3
                (random 7))))

(define (enum/s elems)
  (define elem->i
    (for/hash ([e (in-list elems)]
               [i (in-naturals)])
      (values e i)))
  (spec (length elems)
        (λ (n) (list-ref elems n))
        (λ (v) (hash-ref elem->i v))))
(define bool/s
  (enum/s (list #f #t)))

(module+ test
  (define (test-enum/s os)
    (define bool/s (enum/s os))

    (test-spec bool/s)
    (for ([x (in-list os)])
      (test-en/de bool/s x))

    (define b*b/s (cons/s bool/s bool/s))
    (test-spec b*b/s)
    (for* ([x (in-list os)]
           [y (in-list os)])
      (test-en/de b*b/s (cons x y)))

    (define n*b/s (cons/s nat/s bool/s))
    (test-spec n*b/s)
    (for* ([x (in-range N)]
           [y (in-list os)])
      (test-en/de n*b/s (cons (random (* N N)) y)))

    (define b*n/s (cons/s bool/s nat/s))
    (test-spec b*n/s)
    (for* ([y (in-range N)]
           [x (in-list os)])
      (test-en/de b*n/s (cons x (random (* N N))))))

  (test-enum/s '(#f #t))
  (test-enum/s '(0 1 2)))

(define (flist/s k elem/s)
  (match k
    [0
     (unit/s empty)]
    [n
     (cons/s elem/s (flist/s (sub1 k) elem/s))]))
(module+ test
  (test-spec-exs (flist/s 0 (enum/s '(0 1 2)))
                 [empty 0])
  (test-spec-exs (flist/s 1 (enum/s '(0 1 2)))
                 [(cons 0 empty) 0]
                 [(cons 1 empty) 1]
                 [(cons 2 empty) 2])
  (test-spec-exs (flist/s 2 (enum/s '(0 1 2)))
                 [(cons 0 (cons 0 empty)) 0]
                 [(cons 1 (cons 0 empty)) 1]
                 [(cons 2 (cons 0 empty)) 2]

                 [(cons 0 (cons 1 empty)) 3]
                 [(cons 1 (cons 1 empty)) 4]
                 [(cons 2 (cons 1 empty)) 5]

                 [(cons 0 (cons 2 empty)) 6]
                 [(cons 1 (cons 2 empty)) 7]
                 [(cons 2 (cons 2 empty)) 8])

  (define 3nats/s (flist/s 3 nat/s))
  (test-spec 3nats/s)
  (for ([i (in-range N)])
    (test-en/de 3nats/s
                (cons (random (* N N))
                      (cons (random (* N N))
                            (cons (random (* N N))
                                  empty))))))

(define (wrap/s inner/s wrap-in wrap-out)
  (match-define (spec inner-k inner-in inner-out) inner/s)
  (spec inner-k
        (λ (n) (wrap-in (inner-in n)))
        (λ (v) (inner-out (wrap-out v)))))

(define (hetero-vector/s vector-of-spec)
  (define list-spec
    (foldr (λ (elem/s s) (cons/s elem/s s))
           (unit/s empty)
           (vector->list vector-of-spec)))
  (wrap/s list-spec list->vector vector->list))
(module+ test
  (define weird-vector/s
    (hetero-vector/s
     (vector (nat-range/s 2)
             (nat-range/s 3)
             (nat-range/s 4))))
  (test-spec weird-vector/s)
  (for ([i (in-range N)])
    (test-en/de weird-vector/s
                (vector (random 2)
                        (random 3)
                        (random 4)))))

(define (k*k-bind/s fst/s fst->rst/s
                    #:count [count #f]
                    #:rst-k [given-rst-k #f])
  ;; XXX check
  (match-define (spec fst-k fst-in fst-out) fst/s)

  (spec (or count
            (if (= +inf.0 fst-k)
              +inf.0
              (for/sum ([i (in-range fst-k)])
                       (define fst (fst-in i))
                       ;; XXX check
                       (spec-k (fst->rst/s fst)))))
        (λ (n)
          (define fst (fst-in (pair-hd fst-k (or given-rst-k rst-k) n)))
          ;; XXX check
          (match-define (spec rst-k rst-in rst-out) (fst->rst/s fst))

          (cons fst
                (rst-in (pair-tl fst-k (or given-rst-k rst-k) n))))
        (λ (v)
          (match-define (cons fst rst) v)
          ;; XXX check
          (match-define (spec rst-k rst-in rst-out) (fst->rst/s fst))

          (pair fst-k rst-k
                (fst-out fst)
                (rst-out rst)))))
(module+ test
  (define 3+less-than-three/s
    (k*k-bind/s (enum/s '(0 1 2 3)) (λ (i) (nat-range/s (add1 i)))))
  (test-spec
   3+less-than-three/s)
  (test-spec-exs
   3+less-than-three/s
   [(cons 0 0) 0]
   [(cons 1 0) 1]
   [(cons 1 1) 5]
   [(cons 2 0) 2]
   [(cons 2 1) 6]
   [(cons 2 2) 10]
   [(cons 3 0) 3]
   [(cons 3 1) 7]
   [(cons 3 2) 11]
   [(cons 3 3) 15]))

(define (k*inf-bind/s fst/s fst->rst/s)
  (k*k-bind/s fst/s fst->rst/s))
(module+ test
  (define 3+more-than-three/s
    (k*inf-bind/s (enum/s '(0 1 2 3))
                  (λ (i)
                    (wrap/s nat/s
                            (λ (out) (+ out i))
                            (λ (in) (- in i))))))
  (test-spec
   3+more-than-three/s)
  (test-spec-exs
   3+more-than-three/s
   [(cons 0 0) 0]
   [(cons 1 1) 1]
   [(cons 2 2) 2]
   [(cons 2 3) 6]
   [(cons 3 3) 3]))

;; XXX Add an optional function arg that tells you how many elements
;; there are for each n
(define (inf*k-bind/s fst/s fst->rst/s)
  ;; XXX check
  (match-define (spec fst-k fst-in fst-out) fst/s)

  (define (bind-in i n)
    (define fst (fst-in i))
    ;; XXX check
    (match-define (spec rst-k rst-in rst-out) (fst->rst/s fst))
    (cond
      [(>= n rst-k)
       (bind-in (add1 i) (- n rst-k))]
      [else
       (cons fst (rst-in n))]))
  (define (bind-out-sum i)
    (cond
      [(< i 0)
       0]
      [else
       (define fst (fst-in i))
       ;; XXX check
       (match-define (spec rst-k rst-in rst-out) (fst->rst/s fst))
       (+ rst-k (bind-out-sum (sub1 i)))]))

  (spec +inf.0
        (λ (n) (bind-in 0 n))
        (λ (v)
          (define fst (car v))
          (define fst-n (fst-out fst))
          ;; XXX check
          (match-define (spec rst-k rst-in rst-out) (fst->rst/s fst))
          (+ (bind-out-sum (sub1 fst-n)) (rst-out (cdr v))))))

(module+ test
  (define nat+less-than-n/s
    (inf*k-bind/s nat/s (λ (i) (nat-range/s (add1 i)))))
  (test-spec-exs
   nat+less-than-n/s
   [(cons 0 0) 0]
   [(cons 1 0) 1]
   [(cons 1 1) 2]
   [(cons 2 0) 3]
   [(cons 2 1) 4]
   [(cons 2 2) 5]
   [(cons 3 0) 6]
   [(cons 3 1) 7]
   [(cons 3 2) 8]
   [(cons 3 3) 9]))

(define (inf*inf-bind/s fst/s fst->rst/s)
  (k*k-bind/s fst/s #:rst-k +inf.0 fst->rst/s))
(module+ test
  (define nat+greater-than-n/s
    (inf*inf-bind/s
     nat/s (λ (i)
             (wrap/s nat/s
                     (λ (out) (+ out i))
                     (λ (in) (- in i))))))
  (test-spec nat+greater-than-n/s))

;; XXX
;; (define (union/s pred->spec)
;;   (define pred/s (enum/s (hash-keys pred->spec)))
;;   (wrap/s
;;    (bind/s pred/s (λ (pred) ((hash-ref pred->spec pred))))
;;    (λ (de) (cdr de))
;;    (λ (en) (cons (for/or ([pred (in-hash-keys pred->spec)])
;;                    (and (pred en)
;;                         pred))
;;                  en))))

;; (define (union-list/s elem/s)
;;   (define this/s
;;     (union/s (hash empty? (λ () (unit/s empty))
;;                    cons? (λ () (cons/s elem/s this/s)))))
;;   this/s)

(define (flist-prep/s inner/s)
  (wrap/s
   inner/s
   (λ (v) (cdr v))
   (λ (l) (cons (length l) l))))

(define nat-greater-than-1/s
  (wrap/s nat/s
          (λ (out) (+ out 1))
          (λ (in) (- in 1))))

(define (nelist/s elem/s)
  (define f-elem-list/s
    (λ (len) (flist/s len elem/s)))
  (cond
    [(= +inf.0 (spec-k elem/s))
     (flist-prep/s
      (inf*inf-bind/s
       nat-greater-than-1/s
       f-elem-list/s))]
    [else
     (flist-prep/s
      (inf*k-bind/s
       nat-greater-than-1/s f-elem-list/s))]))

(define (bind-list/s elem/s)
  (define f-elem-list/s
    (λ (len) (flist/s len elem/s)))
  (cond
    [(= +inf.0 (spec-k elem/s))
     (or/s empty?
           (unit/s empty)
           cons?
           (flist-prep/s
            (inf*inf-bind/s
             nat-greater-than-1/s
             f-elem-list/s)))]
    [else
     (flist-prep/s
      (inf*k-bind/s
       nat/s f-elem-list/s))]))

(define list/s bind-list/s)

(module+ test
  (define 012-list/s (list/s (enum/s '(0 1 2))))
  (test-spec 012-list/s)
  (for ([i (in-range N)])
    (test-en/de 012-list/s
                (build-list (random (* N N))
                            (λ (_) (random 3)))))

  (define nat-list/s (list/s nat/s))
  (test-spec nat-list/s)
  (for ([i (in-range N)])
    (test-en/de nat-list/s
                (build-list (random (* N N))
                            (λ (_) (random (* N N)))))))

;; Permutations
(define (remove-at l i)
  (for/list ([e (in-list l)]
             [j (in-naturals)]
             #:unless (= i j))
    e))
(define (list-index l v)
  (for/or ([e (in-list l)]
           [i (in-naturals)]
           #:when (equal? e v))
    i))
(define (permutations/s l)
  (cond
    [(empty? l)
     (unit/s empty)]
    [else
     (k*k-bind/s
      #:count (factorial (length l))
      (wrap/s (nat-range/s (length l))
              (λ (i) (list-ref l i))
              (λ (v) (list-index l v)))
      (λ (v)
        ;; XXX That's ugly
        (define i (list-index l v))
        (define l-p (remove-at l i))
        (permutations/s l-p)))]))

;; XXX
(define (spec/c result/c)
  spec?)

;; XXX
(provide (all-defined-out))
