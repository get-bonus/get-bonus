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

(struct layout (w h) #:transparent)
(struct space layout () #:transparent)
(struct occupied layout (e) #:transparent)
(struct quad layout (ul ur ll lr) #:transparent)

(define (insert s e-w e-h e)
  (match s
    [(occupied _ _ _)
     #f]
    [(space w h)
     (define e_w (e-w e))
     (define e_h (e-h e))
     (and (<= e_w w)
          (<= e_h h)
          (quad w h
                (occupied e_w e_h e)
                (space (- w e_w) e_h)
                (space e_w (- h e_h))
                (space (- w e_w) (- h e_h))))]
    [(quad w h ul ur ll lr)
     (cond
       [(insert ul e-w e-h e)
        => (λ (ul-p)
             (quad w h ul-p ur ll lr))]
       [(insert ur e-w e-h e)
        => (λ (ur-p)
             (quad w h ul ur-p ll lr))]
       [(insert ll e-w e-h e)
        => (λ (ll-p)
             (quad w h ul ur ll-p lr))]
       [(insert lr e-w e-h e)
        => (λ (lr-p)
             (quad w h ul ur ll lr-p))]
       [else
        #f])]))

(module+ test
  (define ecc
    (occupied 1 1 '(1 . 1)))
  (define occ
    (quad 1 1
          ecc
          (space 0 1)
          (space 1 0)
          (space 0 0)))
  (define spc
    (space 1 1))
  
  (check-equal?
   (insert (occupied (random 100) (random 100) #f)
           car cdr
           (cons (random 100) (random 100)))
   #f)
  (check-equal?
   (insert spc car cdr '(1 . 1))
   occ)
  (check-equal?
   (insert (space 2 1) car cdr '(1 . 1))
   (quad 2 1
         (occupied 1 1 '(1 . 1))
         (space 1 1)
         (space 1 0)
         (space 1 0)))
  (check-equal?
   (insert (space 1 2) car cdr '(1 . 1))
   (quad 1 2
         (occupied 1 1 '(1 . 1))
         (space 0 1)
         (space 1 1)
         (space 0 1)))
  (check-equal?
   (insert (space 2 2) car cdr '(1 . 1))
   (quad 2 2
         (occupied 1 1 '(1 . 1))
         (space 1 1)
         (space 1 1)
         (space 1 1)))

  (define (check-insert e-w e-h e . qs)
    (let loop ([qs qs])
      (match qs
        [(list last)
         (void)]
        [(list* fst snd more)
         (check-equal? (insert fst e-w e-h e)
                       snd)
         (loop (list* snd more))])))

  (check-insert
   car cdr
   '(1 . 1)
   (quad 2 2 spc spc spc spc)
   (quad 2 2 occ spc spc spc)
   (quad 2 2 occ occ spc spc)
   (quad 2 2 occ occ occ spc)
   (quad 2 2 occ occ occ occ)
   #f)
  (check-insert
   car cdr
   '(1 . 1)
   (space 2 2)
   (quad 2 2 ecc spc spc spc)
   (quad 2 2 ecc occ spc spc)
   (quad 2 2 ecc occ occ spc)
   (quad 2 2 ecc occ occ occ)
   #f))

(define (place w h e-w e-h l)
  (let/ec esc
    (for/fold ([s (space w h)])
        ([e (in-list l)])
      (match (insert s e-w e-h e)
        [#f (esc #f)]
        [(? layout? ly) ly]))))
(module+ test
  (check-equal?
   (place 2 2 car cdr '((1 . 1) (1 . 1) (1 . 1) (1 . 1)))
   (quad 2 2 ecc occ occ occ))
  (check-equal?
   (place 2 2 car cdr '((1 . 1) (1 . 1) (1 . 1) (1 . 1) (1 . 1)))
   #f))

(define (cons* x y)
  (and x y (cons x y)))
(module+ test
  (check-equal? (cons* #f #f) #f)
  (check-equal? (cons* 't #f) #f)
  (check-equal? (cons* #f 't) #f)
  (check-equal? (cons* 't 't) (cons 't 't)))

(struct placement (x y e) #:transparent)

(define (layout->placements x y ly)
  (match ly
    [(space _ _)
     empty]
    [(occupied _ _ e)
     (list (placement x y e))]
    [(quad _ _ ul ur ll lr)
     (append
      (layout->placements x y ul)
      (layout->placements (+ x (layout-w ul)) y ur)
      (layout->placements x (+ y (layout-h ul)) ll)
      (layout->placements (+ x (layout-w ll)) (+ y (layout-h ur)) lr))]))
(module+ test
  (check-equal?
   (layout->placements 
    0 0
    (quad 2 2 ecc occ occ occ))
   (list (placement 0 0 '(1 . 1))
         (placement 1 0 '(1 . 1))
         (placement 0 1 '(1 . 1))
         (placement 1 1 '(1 . 1)))))

(define (pack e-w e-h l)
  (define (e-area e)
    (* (e-w e) (e-h e)))
  (define total-area
    (apply + (map e-area l)))
  (define optimal-pow2
    (num->pow2
     (sqrt total-area)))

  (define l/sorted
    (sort l > #:key e-area))

  (match-define
   (cons best-pow2 best-ly)
   (for/or ([try-pow2 (in-naturals optimal-pow2)])
     (define len (expt 2 try-pow2))
     (printf "trying pow2 = ~a\n" try-pow2)
     (cons* try-pow2 (place len len e-w e-h l/sorted))))

  (values (expt 2 best-pow2)
          (layout->placements 0 0 best-ly)))

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
  (define l
    (build-list N
                (λ (i) (cons (add1 (random 100))
                             (add1 (random 100))))))
  (printf "~v\n" l)
  (pack car cdr l))
