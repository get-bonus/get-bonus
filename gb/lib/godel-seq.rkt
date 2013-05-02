#lang racket/base
(require gb/lib/godel
         gb/lib/pi
         racket/generator)

(define (infinite-sequence/s inner/s)
  (define seed/s nat/s)
  (define K (spec-k inner/s))
  (define (seed->seq N)
    (define K-seq
      (10-sequence->K-sequence K (in-generator (BPP-digits N))))
    (in-generator
     (for ([k K-seq])
       (yield (decode inner/s k)))))
  (wrap/s seed/s seed->seq error))

(define (sequence-first s)
  (let/ec esc (for ([e s]) (esc e))))

(provide infinite-sequence/s
         sequence-first)
