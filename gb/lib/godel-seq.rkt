#lang racket/base
(require data/enumerate
         gb/lib/pi
         math/number-theory
         racket/generator)

(define (infinite-sequence/e inner/e)
  (define seed/e nat/e)
  (define K (size inner/e))
  (define (seed->seq N)
    (define K-seq
      (10-sequence->K-sequence K (in-generator (BPP-digits N))))
    (in-generator
     (for ([k K-seq])
       (yield (from-nat inner/e k)))))
  (map/e seed->seq error seed/e))

(define (sequence-first s)
  (let/ec esc (for ([e s]) (esc e))))

(module+ test
  (define sevens/e (infinite-sequence/e (below/e 7)))
  (define s (from-nat sevens/e 42))
  (for ([e s]
        [i (in-range 10)])
    (printf "~a = ~a\n" i e)))

(define PERMS (make-hasheq))
(define (permutations-of-n/e n)
  (hash-ref!
   PERMS n
   (λ ()
     (cond
      [(zero? n)
       (const/e '())]
      [else
       (dep2/e
        (factorial n)
        (below/e n)
        (λ (v)
          (map/e
           (λ (l)
             (for/list ([i (in-list l)])
               (if (= i v)
                   (sub1 n)
                   i)))
           (λ (l)
             (for/list ([i (in-list l)])
               (if (= i (sub1 n))
                   v
                   i)))
           (permutations-of-n/e (sub1 n)))))]))))

(module+ test
  (define perms/e (permutations-of-n/e 3))
  (for ([i (in-range (size perms/e))])
    (define l (from-nat perms/e i))
    (printf "~a = ~a = ~a\n" i
            l
            (to-nat perms/e l))))

(define (permutations/e l)
  (define idx->e (list->vector l))
  (define e->idx
    (for/hash ([e (in-list l)]
               [i (in-naturals)])
      (values e i)))
  (map/e
   (λ (l)
     (for/list ([idx (in-list l)])
       (vector-ref idx->e idx)))
   (λ (l)
     (for/list ([e (in-list l)])
       (hash-ref e->idx e)))
   (permutations-of-n/e (vector-length idx->e))))

(module+ test
  (define abcds/e (permutations/e '(a b c d)))
  (for ([i (in-range 10)])
    (define l (from-nat abcds/e i))
    (printf "~a = ~a = ~a\n" i l (to-nat abcds/e l))))

(provide infinite-sequence/e
         sequence-first
         permutations/e
         permutations-of-n/e)
