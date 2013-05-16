#lang racket/base
(require racket/list)
(module+ test
  (require rackunit))

(define (list-slice center sides len)
  (let/ec return
    (define total (+ 1 (* 2 sides)))
    (define il (build-list len (λ (x) x)))
    (when (< len total)
      (return il))
    (when (< center sides)
      (return (take il total)))
    (when (<= (- len center) sides)
      (return (take-right il total)))
    (for/list ([i (in-range (- center sides) (add1 (+ center sides)))])
      i)))
(module+ test
  (define ex 5)
  (check-equal? (list-slice 0 1 ex)
                (list 0 1 2))
  (check-equal? (list-slice 1 1 ex)
                (list 0 1 2))
  (check-equal? (list-slice 2 1 ex)
                (list 1 2 3))
  (check-equal? (list-slice 3 1 ex)
                (list 2 3 4))
  (check-equal? (list-slice 4 1 ex)
                (list 2 3 4))
  (check-equal? (list-slice 2 2 ex)
                (list 0 1 2 3 4))
  (check-equal? (list-slice 2 8 ex)
                (list 0 1 2 3 4)))

(define (calculate-visible-options options max-visible-options current-option)
  (define how-many-options (length options))
  (define how-many-displayed-options
    (min max-visible-options how-many-options))
  (define visible-option-indexes
    (list-slice current-option
                (inexact->exact
                 (quotient how-many-displayed-options 2))
                how-many-options))
  (define more-before?
    (> (first visible-option-indexes) 0))
  (define more-after?
    (< (last visible-option-indexes) (sub1 how-many-options)))
  (define display-indexes
    (append (if more-before? '(before) empty)
            (cond
              [(and more-before? more-after?)
               (drop-right (drop visible-option-indexes 1) 2)]
              [more-before?
               (drop visible-option-indexes 2)]
              [more-after?
               (drop-right visible-option-indexes 2)]
              [else
               visible-option-indexes])
            (if more-after? '(after) empty)))
  (define how-many-display-indexes (length display-indexes))
  (values display-indexes how-many-display-indexes))

(module+ test
  (define-values (dis how-many-dis)
    (calculate-visible-options
     (build-list 30 (λ (i) i))
     10
     15))
  (eprintf "~a\n" (list dis how-many-dis)))

(provide calculate-visible-options)
