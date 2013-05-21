#lang racket/base
(require racket/file
         gb/lib/gzip)

(module+ main
  (define bs (gunzip-bytes (file->bytes "../r.idx.bin.gz")))
  (define k 4)
  (printf "~a\n" (/ (bytes-length bs) k))
  (for ([i (in-range (/ (bytes-length bs) k))])
    (define s (* k i))
    (when (zero? (modulo i 4))
      (newline)
      (printf "~a: " (/ i 4)))
    (printf "~a " (floating-point-bytes->real bs (system-big-endian?)
                                              s (+ s k)))
    ))
