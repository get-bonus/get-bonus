#lang racket/base
(require file/gzip
         file/gunzip)

(define (gzip-bytes bs)
  (define out (open-output-bytes))
  (gzip-through-ports (open-input-bytes bs) out
                      #f (current-seconds))
  (get-output-bytes out))

(define (gunzip-bytes bs)
  (define out (open-output-bytes))
  (gunzip-through-ports (open-input-bytes bs) out)
  (get-output-bytes out))

(provide (all-defined-out))
