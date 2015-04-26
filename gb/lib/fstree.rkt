#lang racket/base
(require racket/match
         racket/list
         racket/contract
         racket/string)

(struct tdir ([f #:mutable] n->t) #:transparent)

(define (make-fstree)
  (tdir #f (make-hash)))
(define (fstree-insert! t p v)
  (fstree-insert/path! t (p->pl p) v))
(define (fstree-ref t p)
  (fstree-ref/path t (p->pl p)))

(provide
 (contract-out
  [rename
   tdir? fstree?
   (-> any/c boolean?)]
  [make-fstree
   (-> tdir?)]
  [fstree-insert!
   (-> tdir? string? any/c
       void?)]
  [fstree-ref
   (-> tdir? string?
       (listof any/c))]))

(define (p->pl p)
  (string-split p "/" #:trim? #f))

(define (fstree-ref/path t pl)
  (match pl
    [(list)
     (filter-map tdir-f (hash-values (tdir-n->t t)))]
    [(list-rest this next)
     (define old (hash-ref! (tdir-n->t t) this make-fstree))
     (fstree-ref/path old next)]))

(define (fstree-insert/path! t pl v)
  (match pl
    [(list)
     (when (tdir-f t)
       (error 'fstree-insert! "Cannot overwrite paths"))
     (set-tdir-f! t v)]
    [(list-rest this next)
     (define old (hash-ref! (tdir-n->t t) this make-fstree))
     (fstree-insert/path! old next v)]))
