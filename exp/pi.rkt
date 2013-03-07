#lang racket/base
(require racket/generator)

(module+ main
  (define (digits)
    (let loop ([8Pi -8])
      (define 8i
        (+ 8 8Pi))

      (define (E k)
        (/ 1 (+ 8i k)))

      (define pi_i
        (+ (* +4 (E 1))
           (* -2 (E 4))
           (* -1 (E 5))
           (* -1 (E 6))))

      (for ([c (in-string (number->string pi_i))])
        (unless (eq? #\/ c)
          (yield (string->number (string c)))))

      (loop 8i)))

  (define d->i (make-hasheq))
  (for ([i (in-range 5000)]
        [d (in-generator (digits))])
    (hash-update! d->i d add1 0))

  (for ([i (in-range 10)])
    (printf "~a => ~a\n" i (hash-ref d->i i)))
  (newline))
