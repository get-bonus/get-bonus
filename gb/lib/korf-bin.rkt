#lang racket/base
(require racket/list
         racket/match
         racket/contract)
(module+ test
  (require rackunit))

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

(struct placement (x y e) #:transparent)

(struct layout () #:transparent)
(struct space layout (w h) #:transparent)
(struct occupied layout (e) #:transparent)
(struct quad layout (ul ur ll lr) #:transparent)

(define (pack e-w e-h l)
  (define (layout->placements x y ly)
    (match ly
      [#f
       (error 'layout->placements "#f")]
      [(space _ _)
       empty]
      [(occupied e)
       (list (placement x y e))]
      [(quad ul ur ll lr)
       (append
        (layout->placements x y ul)
        (layout->placements (+ x (layout-w ul)) y ur)
        (layout->placements x (+ y (layout-h ul)) ll)
        (layout->placements (+ x (layout-w ll)) (+ y (layout-h ur)) lr))]))

  (define layout-w
    (match-lambda
     [#f
      (error 'layout-w "#f")]
     [(space w h)
      0]
     [(occupied e)
      (e-w e)]
     [(quad ul ur ll lr)
      (max (+ (layout-w ul) (layout-w ur))
           (+ (layout-w ll) (layout-w lr)))]))
  (define layout-h
    (match-lambda
     [#f
      (error 'layout-h "#f")]
     [(space w h)
      0]
     [(occupied e)
      (e-h e)]
     [(quad ul ur ll lr)
      (max (+ (layout-h ul) (layout-h ll))
           (+ (layout-h ur) (layout-h lr)))]))

  (define (insert s e)
    (match s
      [#f
       (error 'insert "Cannot insert into #f")]
      [(occupied _)
       #f]
      [(space w h)
       (if (and (<= (e-w e) w)
                (<= (e-h e) h))
         (quad (occupied e)
               (space (- w (e-w e))
                      (e-h e))
               (space (e-w e)
                      (- h (e-h e)))
               (space (- w (e-w e))
                      (- h (e-h e))))
         #f)]
      [(quad ul ur ll lr)
       (cond
         [(insert ul e)
          => (λ (ul-p)
               (quad ul-p ur ll lr))]
         [(insert ur e)
          => (λ (ur-p)
               (quad ul ur-p ll lr))]
         [(insert ll e)
          => (λ (ll-p)
               (quad ul ur ll-p lr))]
         [(insert lr e)
          => (λ (lr-p)
               (quad ul ur ll lr-p))]
         [else
          #f])]))

  (define (place w h l)
    (let/ec esc
      (for/fold ([s (space w h)])
          ([e (in-list l)])
        (match (insert s e)
          [#f (esc #f)]
          [(? layout? ly) ly]))))

  (cond
    [(empty? l)
     (values 0 empty)]
    [else
     (define l/sorted
       (sort l > #:key e-h))
     (define (e-area e)
       (* (e-w e) (e-h e)))
     (define total-width
       (apply + (map e-w l/sorted)))
     (define total-area
       (apply + (map e-area l/sorted)))
     (define narrowest-w
       (apply min (map e-w l/sorted)))

     (define-values
       (best-pow2 best-ly)
       (let loop ([best-pow2 +inf.0]
                  [best-ly #f]
                  [try-pow2
                   (num->pow2
                    (max total-width
                         (e-h (first l/sorted))))])
         (printf "~a\n" (list best-pow2 try-pow2))
         (cond
           [(< (expt 2 try-pow2) narrowest-w)
            (printf "narrowest\n")
            (values best-pow2 best-ly)]
           [(< total-area
               (expt 2 (add1 try-pow2)))
            (printf "total-area\n")
            (values best-pow2 best-ly)]
           [(> try-pow2
               best-pow2)
            (printf "try > best\n")
            (loop best-pow2 best-ly
                  (sub1 try-pow2))]
           [else
            (match (place (expt 2 try-pow2) (expt 2 try-pow2) l/sorted)
              [#f
               (printf "try not feasible\n")
               (values best-pow2 best-ly)]
              [(? layout? result-ly)
               (define result-w (layout-w result-ly))
               (define result-h (layout-h result-ly))
               (define result-pow2
                 (num->pow2
                  (max result-w result-h)))
               (cond
                 [(< result-pow2 best-pow2)
                  (loop result-pow2 result-ly
                        (sub1 result-pow2))]
                 [else
                  (printf "try not better\n")
                  (values best-pow2 best-ly)])])])))

     (values (expt 2 best-pow2)
             (layout->placements 0 0 best-ly))]))

(provide
 (struct-out placement)
 (contract-out
  [pack
   (-> (-> any/c exact-positive-integer?)
       (-> any/c exact-positive-integer?)
       (listof any/c)
       (values number? (listof placement?)))]))

(module+ main
  (define N 10)
  (pack car cdr
        (build-list N
                    (λ (i) (cons (add1 (random 100))
                                 (add1 (random 100)))))))
