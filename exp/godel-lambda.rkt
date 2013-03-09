#lang racket/base
(require gb/lib/godel
         racket/match)
(module+ test
  (require rackunit))

(struct lambda () #:prefab)
(struct var lambda (i) #:prefab)
(struct app lambda (rator rand) #:prefab)
(struct abs lambda (body) #:prefab)

(module+ test
  ;; \x.x
  (define ID (abs (var 0)))
  ;; \f.\z.z
  (define ZERO (abs (abs (var 0))))
  ;; \f.\z.f z
  (define ONE (abs (abs (app (var 1) (var 0))))))

(define lambda-depth
  (match-lambda
   [(struct var (i))
    1]
   [(app rator rand)
    (add1 (max (lambda-depth rator)
               (lambda-depth rand)))]
   [(abs body)
    (add1 (lambda-depth body))]))

(module+ test
  (check-equal? (lambda-depth ID) 2)
  (check-equal? (lambda-depth ZERO) 3)
  (check-equal? (lambda-depth ONE) 4))

(define format-lambda
  (match-lambda
   [(struct var (i))
    (format "~a" i)]
   [(app rator rand)
    (format "(~a ~a)"
            (format-lambda rator)
            (format-lambda rand))]
   [(abs body)
    (format "(\\.~a)" 
            (format-lambda body))]))

(define (var/s max-depth how-many-vars)
  (wrap/s
   (nat-range/s how-many-vars)
   (λ (de) (var de))
   (λ (en) (var-i en))))

(define (app/s max-depth how-many-vars)
  (wrap/s
   (cons/s (lambda/s (sub1 max-depth) how-many-vars)
           (lambda/s (sub1 max-depth) how-many-vars))
   (λ (de) (app (car de) (cdr de)))
   (λ (en) (cons (app-rator en)
                 (app-rand en)))))

(define (abs/s max-depth how-many-vars)
  (wrap/s
   (lambda/s (sub1 max-depth) (add1 how-many-vars))
   (λ (de) (abs de))
   (λ (en) (abs-body en))))

(define (lambda/s max-depth how-many-vars)
  (if (< max-depth 0)
    (var/s (sub1 max-depth) how-many-vars)
    (or/s var?
          (var/s (sub1 max-depth) how-many-vars)
          (λ (x) (or (app? x) (abs? x)))
          (or/s app? (app/s (sub1 max-depth) how-many-vars)
                abs? (abs/s (sub1 max-depth) how-many-vars)))))

(define LAMBDA/s
  (wrap/s
   (inf*k-bind/s
    nat/s
    (λ (max-depth)
      (lambda/s max-depth 0)))
   (λ (de) (cdr de))
   (λ (en) (cons (lambda-depth en) en))))

(module+ test
  (define (show-encode v)
    (printf "~a -> ~a\n" (format-lambda v) (encode LAMBDA/s v)))

  (show-encode ID)
  (show-encode ZERO)
  (show-encode ONE)

  (for ([i (in-range 100)])
    (printf "~a -> ~a\n" i (format-lambda (decode LAMBDA/s i)))))
