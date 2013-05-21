#lang racket/base
(require gb/data/psn
         racket/math
         racket/contract)
(module+ test
  (require rackunit))

(define (clamp bot x top)
  (max bot (min x top)))

(define (cardinate p)
  (if (= 0. (psn-x p))
    p
    (psn (psn-x p) 0.)))

(define (distance p1 p2)
  (sqrt
   (+ (sqr (- (psn-x p1) (psn-x p2)))
      (sqr (- (psn-y p1) (psn-y p2))))))

(define (bytes->integer bs)
  (for/fold ([n 0])
      ([i (in-naturals)]
       [b (in-bytes bs)])
    (* b (expt 256 i))))

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

(provide/contract
 [num->pow2 (-> real? exact-nonnegative-integer?)]
 [distance (-> psn? psn?
               real?)]
 [cardinate (-> psn?
                psn?)]
 [bytes->integer
  (-> bytes?
      exact-integer?)]
 [clamp (-> number? number? number?
            number?)])
