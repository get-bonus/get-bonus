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

(define (sum l)
  (apply + l))

(provide/contract
 [sum (-> list? number?)]
 [distance (-> psn? psn?
               real?)]
 [cardinate (-> psn?
                psn?)]
 [clamp (-> number? number? number?
            number?)])
