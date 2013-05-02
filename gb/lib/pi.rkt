#lang racket/base
(require racket/generator)

(define (BPP-digits N)
  (let loop ([8Pi -8])
    (define 8i
      (+ 8 8Pi))

    (define (E k)
      (/ 1 (+ 8i k)))

    (define pi_i
      (* N
         (+ (* +4 (E 1))
            (* -2 (E 4))
            (* -1 (E 5))
            (* -1 (E 6)))))

    (for ([c (in-string (number->string pi_i))])
      (unless (eq? #\/ c)
        (yield (- (char->integer c) (char->integer #\0)))))

    (loop 8i)))

(define (bits-of k)
  (/ (log k) (log 2)))

;; XXX just subtract k if greater than k and then push the digit to
;; left and go on
(define (10-sequence->K-sequence k seq)
  (cond
    [(< k 10)
     (10-sequence->sub10-sequence k seq)]
    [(= k 10)
     seq]
    [else
     (10-sequence->sup10-sequence k seq)]))

(define (10-sequence->sub10-sequence k seq)
  (in-generator
   (for ([d seq])
     (when (< d k)
       (yield d)))))

(define (10-sequence->sup10-sequence k seq)
  (in-generator
   (let loop ()
     (define d
       (for/sum ([i (in-range (ceiling (/ (log k) (log 10))))]
                 [sub-d seq])
                (* sub-d (expt 10 i))))
     (yield (modulo d k))
     (loop))))

(module+ main
  (define HOW-MANY 5000)

  (define (test-seq K seq)
    (define d->i (make-hasheq))
    (for ([i (in-range HOW-MANY)]
          [d seq])
      (hash-update! d->i d add1 0))

    (define total
      (for/fold ([cnt 0]) ([i (in-range K)])
        (define i-cnt (hash-ref d->i i 0))
        (printf "\t~a => ~a" i i-cnt)
        (when (and (= 4 (modulo i 5)) (not (= i (sub1 K)))) (newline))
        (+ cnt i-cnt)))
    (newline)

    (unless (= HOW-MANY total)
      (error 'digits "Missed some: ~a" total)))

  (define (test-digits N)
    (printf "BPP ~a\n" N)
    (test-seq 10 (in-generator (BPP-digits N))))

  (test-digits 1)
  (test-digits 9)

  (define (test-tetris K N)
    (printf "BPP ~a -> ~a\n" N K)
    (test-seq K (10-sequence->K-sequence K (in-generator (BPP-digits N)))))

  (test-tetris 7 1)
  (test-tetris 7 2)
  (test-tetris 15 1)
  (test-tetris 15 2)

  (test-tetris 100 2))

(provide 10-sequence->K-sequence
         BPP-digits)

