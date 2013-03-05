#lang racket/base
(require racket/list
         racket/match
         (for-syntax racket/base))

(define-syntax performance-log!
  (syntax-rules ()
    [(_ id)
     (performance-log! 'id id)]
    [(_ id val)
     (*performance-log! id val)]))

(define log-op #f)

(define (*performance-log! id val)
  (when log-op
    (write id log-op)
    (write-char #\space log-op)
    (write val log-op)
    (write-char #\space log-op)))

(define (performance-log-init! pth)
  (set! log-op (open-output-file pth #:exists 'replace)))

(define (performance-log-done!)
  (when log-op
    (write #f log-op)
    (write-char #\space log-op)
    (flush-output log-op)))

(define (performance-log-read p)
  (define (read-log)
    (define v (read-frame))
    (if v
      (cons v (read-log))
      empty))
  (define (read-frame)
    (define ht (make-hasheq))
    (let loop ()
      (match (read)
        [#f
         ht]
        [(? eof-object?)
         #f]
        [k
         (hash-set! ht k (read))
         (loop)])))
  (with-input-from-file p
    (λ ()
      (read-log))))

(provide
 performance-log-read
 performance-log-init!
 performance-log!
 performance-log-done!)
