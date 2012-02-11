#lang racket/base
(require racket/match
         racket/list
         racket/function
         unstable/generics)

(define-syntax-rule (define/for/fold ([id init] ...) . more)
  (define-values (id ...)
    (for/fold ([id init] ...) . more)))

;; Objects
(generics reactable
          (react reactable evts))

;; Meshs
(struct mesh (objs)
        #:transparent
        #:property prop:reactable
        (define-methods reactable
          (define/generic gen-react react)
          (define (react this events)
            (match-define (mesh objs) this)
            (define/for/fold ([new-objects (hasheq)]
                              [new-events empty])
              ([(oid obj) (in-hash objs)])
              (define these-events
                (append
                 (filter (curry mesh-visible-to? oid) events)
                 (mesh-collisions oid)))
              (define-values (new-obj these-new-events)
                (gen-react obj these-events))
              (values (hash-set new-objects oid new-obj)
                      (append these-new-events new-events)))
            (values (mesh new-objects)
                    new-events))))

(define (new-mesh)
  (mesh (hasheq)))

(define (mesh-visible-to? oid evt)
  #f)
(define (mesh-collisions oid)
  empty)

(define (mesh-add this obj)
  (match-define (mesh objs) this)
  (define oid (gensym))
  (mesh (hash-set objs oid obj)))

;; Examples
(struct tick (frames)
        #:transparent)

(struct counter (n)
        #:transparent
        #:property prop:reactable
        (define-methods reactable
          (define (react this events)
            (match-define (counter n) this)
            (match events
                   [(list)
                    (values this
                            empty)]
                   [(list evt_1 ... (tick frames) evt_2 ...)
                    (values (counter (add1 n))
                            empty)]))))

(check-equal?
 (react (mesh-add (new-mesh)
                  (counter 0))
        (list (tick 0))))

