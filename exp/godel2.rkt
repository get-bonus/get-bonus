#lang racket/base
(require racket/match
         racket/function
         racket/list)
(module+ test
  (require rackunit)
  (define-syntax-rule (check-as from to from-e to-e)
    (let ()
      (define from-v from-e)
      (define to-v to-e)
      (check-equal? (as from to from-v)
                    to-v)
      (check-equal? (as to from to-v)
                    from-v))))

(struct iso (to from))

(define (iso-compose a-b b-c)
  (match-define (iso f g) a-b)
  (match-define (iso fp gp) b-c)
  (iso (compose fp f)
       (compose g gp)))

(define itself
  (iso identity identity))

(define invert
  (match-lambda
   [(iso f g)
    (iso g f)]))

(define nats
  itself)

(define (as that this x)
  (match-define (iso _ g) (iso-compose that (invert this)))
  (g x))

(define (nat-cons x y)
  (arithmetic-shift (bitwise-ior 1 (arithmetic-shift y 1))
                    x))

(define (nat-hd n)
  (unless (> n 0)
    (error 'nat-hd "Cannot take the head of 0"))
  (if (= 1 (bitwise-and n 1))
    0
    (add1 (nat-hd (arithmetic-shift n -1)))))

(define (nat-tl n)
  (arithmetic-shift n (* -1 (add1 (nat-hd n)))))

(define (as-nats-nat n)
  (if (zero? n)
    empty
    (cons (nat-hd n)
          (as-nats-nat (nat-tl n)))))
(define (as-nat-nats l)
  (if (empty? l)
    0
    (nat-cons (first l) (as-nat-nats (rest l)))))

(define nat1 (iso as-nats-nat as-nat-nats))

(module+ test
  (check-as nat1 nats
            (list 50 20 50)
            5316911983139665852799595575850827776))

(define (unpair z)
  (values (nat-hd (add1 z))
          (nat-tl (add1 z))))
(define (pair x y)
  (sub1 (nat-cons x y)))

(module+ test
  (check-equal? (map (λ (i) (call-with-values (λ () (unpair i)) cons)) (range 8))
                (list (cons 0 0)
                      (cons 1 0)
                      (cons 0 1)
                      (cons 2 0)
                      (cons 0 2)
                      (cons 1 1)
                      (cons 0 3)
                      (cons 3 0))))

(define (from-base base l)
  (match l
    [(list)
     0]
    [(cons x xs)
     (unless (and (>= x 0) (< x base))
       (error 'from-base "Numbers not within base"))
     (+ x (* base (from-base base xs)))]))

(define (transpose l-of-l)
  (if (empty? l-of-l)
    empty
    (for/list ([i (in-range (length (first l-of-l)))])
      (for/list ([l (in-list l-of-l)])
        (list-ref l i)))))

(define (bitcount n)
  (for/or ([x (in-naturals)]
           #:when (> (expt 2 x) n))
    x))

(define (max-bitcount ns)
  (for/fold ([ans 0])
      ([n (in-list ns)])
    (max ans (bitcount n))))

(define (to-maxbits maxbits n)
  (define bs (to-base 2 n))
  (append bs (make-list (max 0 (- maxbits (length bs))) 0)))

(define (to-base base n)
  (unless (> base 1)
    (error 'to-base "Cannot use unary"))
  (define-values (q d) (quotient/remainder n base))
  (cons d
        (if (= q 0)
          empty
          (to-base base q))))

(define (to-tuple k n)
  (map (curry from-base 2)
       (transpose
        (map (curry to-maxbits k)
             (to-base (expt 2 k) n)))))
(define (from-tuple ns)
  (define k (length ns))
  (define l (max-bitcount ns))
  (from-base (expt 2 k)
             (map (curry from-base 2)
                  (transpose
                   (map (curry to-maxbits l) ns)))))

(module+ test
  (check-equal? (to-tuple 3 42)
                (list 2 1 2))
  (check-equal? (from-tuple (list 2 1 2))
                42))

(define (ftuple2nat ns)
  (cond
    [(empty? ns)
     0]
    [else
     (define k (length ns))
     (define t (from-tuple ns))
     (add1 (pair (sub1 k) t))]))
(define (nat2ftuple kf)
  (cond
    [(zero? kf)
     empty]
    [else
     (define-values (k f) (unpair (sub1 kf)))
     (to-tuple (add1 k) f)]))

(define nat
  (iso nat2ftuple ftuple2nat))

(define n2
  (iso-compose (iso pair unpair)
               nat))

(module+ test
  (check-as nats nat
            2008
            (list 3 2 3 1))
  (check-as nat nats
            (list 2009 2010 4000 0 5000 42)
            4855136191239427404734560))

(struct term () #:transparent)
(struct id term (var) #:transparent)
(struct fun term (const args) #:transparent)

(define nterm2code
  (match-lambda
   [(id i)
    (* 2 i)]
   [(fun name args)
    (define cs (map nterm2code args))
    (define fc (as nat nats (cons name cs)))
    (sub1 (* 2 fc))]))
(define code2nterm
  (match-lambda
   [(? even? n)
    (id (quotient n 2))]
   [n
    (define k (quotient (add1 n) 2))
    (match-define (cons name cs) (as nats nat k))
    (define args (map code2nterm cs))
    (fun name args)]))

(define nterm
  (iso-compose (iso nterm2code code2nterm) nat))

(module+ test
  (check-as nterm nat
            55
            (fun 1 (list (fun 0 empty) (id 0)))))

(define (from-bbase base xs)
  (define (from-bbase-p base xs)
    (match xs
      [(list) 0]
      [(cons x xs)
       (unless (and (> x 0) (<= x base))
         (error 'from-bbase "Numbers must be within base"))
       (+ x (* base (from-bbase-p base xs)))]))
  (from-bbase-p base (map add1 xs)))

(define (to-bbase base n)
  (define (to-bbase-p base n)
    (cond
      [(zero? n)
       empty]
      [else
       (define-values (q d) (quotient/remainder n base))
       (define d-p
         (if (zero? d)
           base
           d))
       (define q-p
         (if (zero? d)
           (sub1 q)
           q))
       (define ds
         (if (zero? q-p)
           empty
           (to-bbase-p base q-p)))
       (cons d-p ds)]))
  (map sub1 (to-bbase-p base n)))

(define (bijnat a)
  (iso-compose (iso (curry from-bbase a) (curry to-bbase a)) nat))

(module+ test
  (check-as (bijnat 3) nat
            2009
            (list 1 2 2 0 2 0 1))
  (check-as (bijnat 10) nat
            2009
            (list 8 9 8 0))
  (check-equal? (map (curry as (bijnat 3) nat) (range 13))
                '(()
                  (0)
                  (1)
                  (2)
                  (0 0)
                  (1 0)
                  (2 0)
                  (0 1)
                  (1 1)
                  (2 1)
                  (0 2)
                  (1 2)
                  (2 2))))

(define bot-char #\a)
(define top-char #\z)
(define char-base
  (+ 1 (- (char->integer top-char)
          (char->integer bot-char))))

(define (chr2ord c)
  (- (char->integer c)
     (char->integer bot-char)))
(define (ord2chr n)
  (integer->char
   (+ n
      (char->integer bot-char))))

(define (string2nat cs)
  (from-bbase char-base (map chr2ord (string->list cs))))
(define (nat2string n)
  (list->string (map ord2chr (to-bbase char-base n))))

(define string
  (iso-compose (iso string2nat nat2string) nat))

(module+ test
  (check-as nat string
            "hello"
            7073802))

(define sterm2code
  (match-lambda
   [(id i)
    (* 2 i)]
   [(fun name args)
    (define cs (map sterm2code args))
    (define cname (as nat string name))
    (define fc (as nat nats (cons cname cs)))
    (sub1 (* 2 fc))]))
(define code2sterm
  (match-lambda
   [(? even? n)
    (id (quotient n 2))]
   [n
    (define k (quotient (add1 n) 2))
    (match-define (cons cname cs) (as nats nat k))
    (define name (as string nat cname))
    (define args (map code2sterm cs))
    (fun name args)]))

(define sterm
  (iso-compose (iso sterm2code code2sterm) nat))

(module+ test
  (check-as nat sterm
            (fun "b" (list (fun "a" empty) (id 0)))
            2215)
  (check-as nat sterm
            (fun "forall" (list (id 0) (fun "f" (list (id 0)))))
            38696270040102961756579399)
  (check-equal?
   (map (curry as sterm nat) (range 8))
   (list (id 0)
         (fun "" empty)
         (id 1)
         (fun "" (list (id 0)))
         (id 2)
         (fun "a" empty)
         (id 3)
         (fun "" (list (id 0) (id 0))))))

(define bits
  (bijnat 2))

(module+ test
  (check-as bits nat 42
            (list 1 1 0 1 0)))

(define nterm2bits
  (curry as bits nterm))
(define bits2nterm
  (curry as nterm bits))

(define sterm2bits
  (curry as bits sterm))
(define bits2sterm
  (curry as sterm bits))

(module+ test
  (check-as nterm bits
            (list 0 0 0 1 0 1 0 0 1 1 0 1 0 0 0 0 0 1 0 0 0 0 0 1 0
                  0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
            (fun 0 (list (fun 1 (list (id 0) (fun 1 (list (id 0))))) (id 1)))))

;; A level is... an NxM matrix with 0s for sky and 1 for blocks
(define (level2nats l)
  (define (row2nat l)
    (as (bijnat 2) nat l))
  (map row2nat l))
(define (nats2level ns)
  (define (nat2row n)
    (as nat (bijnat 2) n))
  (map nat2row 
       ns))

(define level
  (iso level2nats nats2level))

(module+ test
  (check-as level nats
            (list (list 0 0 0)
                  (list 0 1 0)
                  (list 0 0 0)
                  (list 1 1 1))
            (list 7 9 7 14))
  (check-equal? (as nat nats
                    (as level nats
                        (list (list 0 0 0)
                              (list 0 1 0)
                              (list 0 0 0)
                              (list 1 1 1))))
                712056))
