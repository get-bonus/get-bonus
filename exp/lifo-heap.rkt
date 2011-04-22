#lang racket/base
(require racket/list
         racket/contract
         racket/math
         racket/match
         (prefix-in h: data/heap)
         tests/eli-tester)

; XXX Implement my own typed lifo fib heap: https://github.com/evansenter/f_heap/blob/master/lib/f_heap.rb

(struct heap (c ht h) #:mutable)

(define (heap-empty? h)
  (zero? (h:heap-count (heap-h h))))

(define (make-heap f)
  (define ht (make-hash))
  (define (lifo-<= a b)
    (define fa (f a))
    (define fb (f b))
    (if (= fa fb)
        ; If the heuristics are identical, prefer the new entry
        (>= (hash-ref ht a)
            (hash-ref ht b))
        (<= fa fb)))
  (heap 0 ht (h:make-heap lifo-<=)))

(define (heap-add! lh v)
  (match-define (heap c ht h) lh)
  (hash-set! ht v c)
  (h:heap-add! h v)
  (set-heap-c! lh (add1 c)))

(define (heap-member? lh v)
  (match-define (heap _ ht _) lh)
  (hash-has-key? ht v))

(define (heap-remove! lh v)
  (match-define (heap _ ht h) lh)
  (hash-remove! ht v))

(define (heap-remove-min! lh)
  (match-define (heap _ ht h) lh)
  (define (pull)
    (define x (h:heap-min h))
    (h:heap-remove-min! h)
    x)
  (let loop ()
    (define x (pull))
    ; I'm not actually removing things with heap-remove-min!, instead I'm checking here
    (if (hash-has-key? ht x)
        (begin
          (hash-remove! ht x)
          x)
        (loop))))

(define (in-heap lh)
  (match-define (heap _ ht h) lh)
  (hash-keys ht))

(provide
 heap?
 (rename-out [make-heap heap])
 heap-add!
 heap-member?
 heap-remove!
 heap-remove-min!
 heap-empty?
 in-heap)