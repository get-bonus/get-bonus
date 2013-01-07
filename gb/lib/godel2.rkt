#lang racket/base
(require racket/match
         racket/contract
         racket/function
         racket/list)
(module+ test
  (require rackunit)
  (define N 10))

;; XXX New idea: Assign values to numbers, keep track of count per spec

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
     (+ (* hd tl-k) tl)]))
(define (pair-hd hd-k tl-k n)
  (match* (hd-k tl-k)
    [(+inf.0 +inf.0)
     (nat-hd n)]
    [(+inf.0 tl-k)
     (quotient n tl-k)]
    [(hd-k +inf.0)
     (remainder n hd-k)]
    [(hd-k tl-k)
     (quotient n tl-k)]))
(define (pair-tl hd-k tl-k n)
  (match* (hd-k tl-k)
    [(+inf.0 +inf.0)
     (nat-tl n)]
    [(+inf.0 tl-k)
     (remainder n tl-k)]
    [(hd-k +inf.0)
     (quotient n hd-k)]
    [(hd-k tl-k)
     (remainder n tl-k)]))

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
      (test-equal? (format "~a ~a,~a" n s v)
                   (decode s (encode s v))
                   v)))
  (define-syntax-rule (test-spec s-e)
    (let ()
      (define n 's-e)
      (define s s-e)
      (for ([i (in-range (min N (spec-k s)))])
        (test-equal? (format "~a ~a,~a" n s i)
                     (encode s (decode s i)) i)))))

;; Specs
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

(define (inf*k-bind/s fst/s fst->rst/s)
  ;; XXX check
  (match-define (spec fst-k fst-in fst-out) fst/s)

  (define (bind-in i n)
    (define fst (fst-in i))
    ;; XXX check
    (match-define (spec rst-k rst-in rst-out) (fst->rst/s fst))
    (if (>= n rst-k)
      (bind-in (add1 i) (- n rst-k))
      (cons fst (rst-in n))))
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
          (+ (sub1 (bind-out-sum fst-n)) (rst-out (cdr v))))))

(define (inf*inf-bind/s fst/s fst->rst/s)
  ;; XXX check
  (match-define (spec fst-k fst-in fst-out) fst/s)
  (spec +inf.0
        (λ (n)
          (define fst (fst-in (pair-hd +inf.0 +inf.0 n)))
          ;; XXX check
          (match-define (spec rst-k rst-in rst-out) (fst->rst/s fst))
          (cons fst
                (rst-in (pair-tl +inf.0 +inf.0 n))))
        (λ (v)
          (define fst (car v))
          ;; XXX check
          (match-define (spec rst-k rst-in rst-out) (fst->rst/s fst))
          (pair +inf.0 +inf.0
                (fst-out fst)
                (rst-out (cdr v))))))

;; XXX test bind

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

(define (bind-list/s elem/s)
  (if (= +inf.0 (spec-k elem/s))
    (inf*inf-bind/s nat/s (λ (len) (flist/s len elem/s)))
    (inf*k-bind/s nat/s (λ (len) (flist/s len elem/s)))))

(define list/s bind-list/s)

(module+ test
  (define nat-list/s (list/s nat/s))
  (test-spec nat-list/s)
  (for ([i (in-range N)])
    (test-en/de nat-list/s
                (build-list (random (* N N))
                            (λ (_) (random (* N N))))))

  (define 012-list/s (list/s (enum/s '(0 1 2))))
  (test-spec 012-list/s)
  (for ([i (in-range N)])
    (test-en/de 012-list/s
                (build-list (random (* N N))
                            (λ (_) (random 3))))))
