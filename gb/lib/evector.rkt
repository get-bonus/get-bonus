#lang racket/base
(require racket/contract
         racket/match
         racket/unit
         (for-syntax racket/base
                     syntax/strip-context))

(define-signature vector^
  (make-vector vector-ref vector-set!))
(define-signature evector^ extends vector^
  (vector? vector-length set-vector-length! vector-safe-set!))

;; These are broken because contracts can't refer to other
;; bidings... which is totally useless and stupid.
;; (define-signature vector^
;;   ((contracted
;;     [vector?
;;      (-> any/c boolean?)]
;;     [make-vector
;;      (-> exact-nonnegative-integer?
;;          vector?)]
;;     [vector-ref
;;      (-> vector?
;;          exact-nonnegative-integer?
;;          any/c)]
;;     [vector-set!
;;      (-> vector?
;;          exact-nonnegative-integer?
;;          any/c
;;          void)]
;;     [vector-length
;;      (-> vector?
;;          exact-nonnegative-integer?)])))
;; (define-signature evector^ extends vector^
;;   ((contracted
;;     [set-vector-length!
;;      (-> vector?
;;          exact-nonnegative-integer?
;;          void)]
;;     [vector-safe-set!
;;      (-> vector?
;;          exact-nonnegative-integer?
;;          any/c
;;          void)])))

(define-unit evector@
  (import (prefix base: vector^))
  (export evector^)

  (struct evector (base actual-len effective-len) #:mutable)

  (define vector?
    evector?)

  (define (make-vector initial-len)
    (evector (base:make-vector initial-len)
             initial-len
             0))

  (define (vector-length ev)
    (evector-effective-len ev))
  (define (set-vector-length! ev new-len)
    (match-define (evector base actual-len effective-len)
                  ev)
    (cond
      [(< new-len actual-len)
       (set-evector-effective-len! ev new-len)]
      [else
       (define next-len (max (* 2 actual-len) new-len))
       (define new-base (base:make-vector next-len))
       (for ([i (in-range effective-len)])
         (base:vector-set! new-base i (base:vector-ref base i)))
       (set-evector-base! ev new-base)
       (set-evector-actual-len! ev next-len)
       (set-evector-effective-len! ev new-len)]))

  (define (ensure-k ev k)
    (unless ((vector-length ev) . > . k)
      (error 'evector "index ~e out of bounds" k)))

  (define (vector-ref ev k)
    (ensure-k ev k)
    (base:vector-ref
     (evector-base ev)
     k))
  (define (vector-set! ev k val)
    (ensure-k ev k)
    (base:vector-set!
     (evector-base ev)
     k
     val))
  (define (vector-safe-set! ev k val)
    (set-vector-length!
     ev
     (max (add1 k) (vector-length ev)))
    (vector-set! ev k val)))

(define-syntax (define-evector stx)
  (syntax-case stx ()
    [(_ prefix:
        prefix:make-vector prefix:vector-ref prefix:vector-set!)
     (replace-context
      stx
      (syntax/loc stx
        (define-values/invoke-unit
          (compound-unit
           (import) (export OUTPUT)
           (link (((PREFIX : vector^))
                  (unit (import)
                        (export vector^)
                        (define make-vector prefix:make-vector )
                        (define vector-ref prefix:vector-ref)
                        (define vector-set! prefix:vector-set!)))
                 (((OUTPUT : evector^)) evector@ PREFIX)))
          (import)
          (export (prefix prefix: evector^)))))]))

(provide
 vector^
 evector^
 evector@
 define-evector)

(module+ test
  (require rackunit)

  (define-evector e:
    make-bytes bytes-ref bytes-set!)
  (define-evector v:
    make-vector vector-ref vector-set!)

  (define N 100)
  (define e (e:make-vector 10))
  (for* ([try (in-range 2)]
         [i (in-range N)])
    (e:set-vector-length! e (add1 i))
    (check-equal? (e:vector-length e) (add1 i)
                  (format "~a len" i))
    (e:vector-set! e i i)
    (for ([j (in-range N)])
      (if (j . <= . i)
        (check-equal? (e:vector-ref e j) j
                      (format "~a ~a valid" i j))
        (check-exn exn:fail?
                   (λ () (e:vector-ref  e j))
                   (format "~a ~a invalid" i j)))))
  (define ep (e:make-vector 10))
  (define order (build-list N (λ (i) (random N))))
  (for ([i (in-list order)]
        [which (in-naturals)])
    (e:vector-safe-set! ep i i)
    (for ([j (in-list order)]
          [_ (in-range which)])
      (check-equal? (e:vector-ref ep j) j
                    (format "~a ~a valid" i j)))))
