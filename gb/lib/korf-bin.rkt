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
     [(space w h)
      0]
     [(occupied e)
      (e-w e)]
     [(quad ul ur ll lr)
      (max (+ (layout-w ul) (layout-w ur))
           (+ (layout-w ll) (layout-w lr)))]))
  (define layout-h
    (match-lambda
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
       (error 'insert "Cannot insert into #f\n")]
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
       (best-w best-h best-ly)
       (let loop ([best-w +inf.0]
                  [best-h +inf.0]
                  [best-ly #f]
                  [try-w total-width]
                  [try-h (e-h (first l/sorted))])
         (printf "~a\n" (list best-w best-h try-w try-h))
         (cond
           [(< try-w narrowest-w)
            (values best-w best-h best-ly)]
           [(> total-area
               (* try-w try-h))
            (loop best-w best-h best-ly
                  try-w (add1 try-h))]
           [(> (* try-w try-h)
               (* best-w best-h))
            (loop best-w best-h best-ly
                  (sub1 try-w) try-h)]
           [else
            (match (place try-w try-h l/sorted)
              [#f
               (loop best-w best-h best-ly
                     try-w (add1 try-h))]
              [(? layout? result-ly)
               (define result-w (layout-w result-ly))
               (define result-h (layout-h result-ly))
               (if (< (* result-w result-h)
                      (* best-w best-h))
                 (loop result-w result-h result-ly
                       (sub1 result-w) result-h)
                 (loop best-w best-h best-ly
                       (sub1 result-w) result-h))])])))

     (values (max best-w best-h)
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
