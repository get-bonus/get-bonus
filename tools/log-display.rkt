#lang racket/base
(require slideshow
         plot
         plot/utils
         racket/pretty
         gb/lib/performance-log)

(define (max-map f l)
  (f (argmax f l)))

(define (log-display vs)
  (printf "Samples: ~a\n" (length vs))

  (for ([ht (in-list vs)])
    (hash-set! ht 'memory-diff
               (- (hash-ref ht 'after-memory)
                  (hash-ref ht 'before-memory))))

  (plot-width (current-para-width))
  (plot-height 600)

  (pretty-print (first vs))

  (define maximum-frame-time
    (max-map (λ (ht) (hash-ref ht 'frame-time)) vs))
  (define how-many-frames
    (length vs))
  (define (frame-times-under t)
    (/ (count (λ (ht) (<= (hash-ref ht 'frame-time) t))
              vs)
       how-many-frames))

  (define last-% 0)
  (define last-t 0)
  (define frame-time-histo
    (for/list ([t (in-range 0
                            (inexact->exact
                             (ceiling maximum-frame-time)))]
               #:final (or (>= last-% 99/100)
                           (>= t 17)))
      (define % (frame-times-under t))
      (set! last-% %)
      (set! last-t t)
      (vector t %)))

  (for ([i (in-list '(drawn-count this-count SpriteData-count memory-diff))])
    (define-values (min-i max-i ps)
      (for/fold ([min-i +inf.0]
                 [max-i -inf.0]
                 [ps empty])
          ([ht (in-list vs)])
        (define this-i (hash-ref ht i))
        (values
         (min min-i this-i)
         (max max-i this-i)
         (cons (vector this-i
                       (hash-ref ht 'frame-time))
               ps))))
    (slide
     (plot-pict
      #:title (format "Frame Time vs. ~a" i)
      #:x-label (symbol->string i)
      #:x-min (* 0.999 min-i)
      #:x-max (* 1.001 max-i)
      #:y-label "frame time (ms)"
      #:y-min -1.0
      #:y-max (* 1.1 last-t)
      (list
       (points
        ps)))))

  (slide
   (plot-pict
    #:title
    (format "Percentage of Frame Times under ms (max frame time = ~v)"
            maximum-frame-time)
    #:x-label "frame time (ms)"
    #:x-min 0.0
    #:x-max last-t
    #:y-label "% of frames"
    #:y-min 0.0
    #:y-max 1.0
    (list
     (discrete-histogram
      frame-time-histo)))))

#hasheq((after-memory . 103161024)
        (drawn-count . 0)
        (this-count . 0)
        (SpriteData-count . 1024)
        (before-memory . 85377792)
        (frame-time . 383.837890625))

(module+ main
  (require racket/cmdline)

  (command-line
   #:program "log-display"
   #:args (log-file)
   (log-display
    (performance-log-read
     log-file))))
