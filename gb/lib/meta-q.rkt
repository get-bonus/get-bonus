#lang racket/base
(require racket/contract
         racket/function
         racket/list
         racket/string)
(module+ test
  (require rackunit))

(define (safe-split k s)
  (define i 
    (apply max
           (for/list ([c (in-string s)]
                      [i (in-range (add1 k))])
             (if (or (char-blank? c)
                     (eq? #\- c))
               i
               0))))
  (values (substring s 0 (add1 i))
          (substring s (add1 i))))

(define (meta-q-line k s)
  (cond
    [(<= (string-length s) k)
     (list s)]
    [else
     (define-values (before after) (safe-split k s))
     (list* (string-trim before)
            (meta-q-line k (string-trim after)))]))

(define (meta-q k lop)
  (append* (add-between (map (curry meta-q-line k) lop) (list ""))))

(module+ test
  (check-equal? (meta-q 1 (list "a b c d"))
                (list "a" "b" "c" "d"))
  (check-equal? (meta-q 10 (list "alpha beta gamma delta zeta alpha beta gamma delta zeta"))
                (list "alpha beta" "gamma" "delta zeta"
                      "alpha beta" "gamma" "delta zeta"))
  (check-equal? (meta-q 10 (list "alpha-beta-gamma-delta-zeta-alpha-beta-gamma-delta-zeta"))
                (list "alpha-beta-" "gamma-" "delta-zeta-"
                      "alpha-beta-" "gamma-" "delta-zeta"))
  (check-equal? (meta-q 9 (list "a b c d e f g h i j"))
                (list "a b c d e" "f g h i j"))
  (check-equal? (meta-q 80 (list "a b c d" "a b c d"))
                (list "a b c d" "" "a b c d")))

(provide
 (contract-out
  [meta-q (-> exact-nonnegative-integer?
              (listof string?)
              (listof string?))]))
