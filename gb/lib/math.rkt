#lang racket/base
(require gb/data/psn
         racket/math
         racket/contract)

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

(provide/contract
 [distance (-> psn? psn?
               real?)]
 [cardinate (-> psn?
                psn?)]
 [bytes->integer
  (-> bytes?
      exact-integer?)]
 [clamp (-> number? number? number?
            number?)])
