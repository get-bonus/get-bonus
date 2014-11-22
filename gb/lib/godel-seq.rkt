#lang racket/base
(define (sequence-first s)
  (let/ec esc (for ([e s]) (esc e))))

(provide sequence-first)
