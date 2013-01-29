#lang racket/base
(require racket/runtime-path
         racket/match
         racket/file
         racket/list
         gb/lib/random
         (only-in gb/ai/path-finding
                  manhattan-distance)
         racket/function)

(define-runtime-path template-map "template.map")
(match-define
 (list hall wall power-up fruit ghost-entry player-entry conn)
 (bytes->list #"0123456"))
(define (path->quadrant p)
  (define lines (file->bytes-lines p))
  (values (* 2 (bytes-length (first lines)))
          (* 2 (length lines))
          (apply bytes-append lines)))
(define-values (width height quad:template)
  (path->quadrant template-map))

(define h-width
  (/ width 2))
(define h-height
  (/ height 2))
(define (r*c->i vr vc)
  (+ (* vr h-width) vc))
(define (quad-ref quad vr vc)
  (bytes-ref quad (r*c->i vr vc)))
(define (quad-set! quad vr vc v)
  (bytes-set! quad (r*c->i vr vc) v))

(define quad:templates
  (for*/list ([top? (in-list '(#t #f))]
              [doubled (in-range 5)])
    (define t (bytes-copy quad:template))
    (for ([row (in-list (list 2 (if top? 3 4) 5))])
      (let loop ([i 2] [which 0])
        (unless (= i 15)
          (cond
            [(= which doubled)
             (quad-set! t row (+ 0 i) wall)
             (quad-set! t row (+ 1 i) wall)
             (loop (+ i 3) (+ 1 which))]
            [else
             (quad-set! t row (+ 0 i) wall)
             (loop (+ i 2) (+ 1 which))]))))
    t))

(define (locate-cell quad value)
  (for*/or ([r (in-range h-height)]
            [c (in-range h-width)]
            #:when (= (quad-ref quad r c) value))
    (cons r c)))

;; XXX incorporate ghost position (so that I don't put a ghost in a
;; wall if I'm switching the state of a quad)
(define (generate-quad)
  (define the-template (list-ref/random quad:templates))
  (define new-quad (bytes-copy the-template))
  (define cells
    (for*/list ([r (in-range h-height)]
                [c (in-range h-width)])
      (cons r c)))
  (define (cell-neighbors r*c)
    (match-define (cons r c) r*c)
    (list (cons r (sub1 c))
          (cons (add1 r) c)
          (cons r (add1 c))
          (cons (sub1 r) c)))
  (define (inside-maze? r*c)
    (match-define (cons r c) r*c)
    (and (<= 0 r (sub1 h-height))
         (<= 0 c (sub1 h-width))))
  (define (quad-cell-ref r*c)
    (match-define (cons r c) r*c)
    (if (inside-maze? r*c)
      (quad-ref new-quad r c)
      hall))
  (define (quad-cell-set! r*c v)
    (match-define (cons r c) r*c)
    (quad-set! new-quad r c v))
  (define (original-wall? c)
    (= wall (quad-ref the-template (car c) (cdr c))))
  (define (not-wall? c)
    (not (= wall (quad-cell-ref c))))
  (define (non-wall-neighbors cn)
    (filter not-wall?
            (cell-neighbors cn)))

  ;; For every cell, consider turning it into a wall if
  ;; all its non-wall neighbors have more than 2 exits
  ;; i.e. doing so does not create a dead-end.
  (for ([r*c (in-list (shuffle cells))])
    (when (= hall (quad-cell-ref r*c))
      (define cns (non-wall-neighbors r*c))
      (when
          (for/and ([cn (in-list cns)])
            (define how-many-halls
              (length (non-wall-neighbors cn)))
            (how-many-halls . > . 2))
        (quad-cell-set! r*c wall))))

  (define seen? (make-hash))
  (define (visit r*c)
    (match-define (cons r c) r*c)
    (unless (hash-has-key? seen? r*c)
      (hash-set! seen? r*c #t)
      (for-each visit
                (filter inside-maze?
                        (non-wall-neighbors r*c)))))

  (define player-entry-cell (locate-cell new-quad player-entry))

  (visit player-entry-cell)

  (let ()
    (local-require data/heap
                   unstable/function)
    (define (dig-until-seen c0)
      (define h
        (make-heap (λ (c1 c2)
                     (<= (manhattan-distance c1 c0)
                         (manhattan-distance c2 c0)))))
      (define came-from (make-hash))
      (define (add-neighbors! c)
        (define ns
          (filter (conjoin (λ (c) (not (hash-has-key? came-from c)))
                           inside-maze?
                           (negate original-wall?))
                  (cell-neighbors c)))
        (for-each (λ (nc) (hash-set! came-from nc c)) ns)
        (heap-add-all! h ns))
      (hash-set! came-from c0 #f)
      (heap-add! h c0)
      (define last-c
        (let/ec done
          (let loop ()
            (define c (heap-min h))
            (heap-remove-min! h)
            (if (hash-has-key? seen? c)
              (done c)
              (begin
                (add-neighbors! c)
                (loop))))))
      (let loop ([c last-c])
        (define next-c (hash-ref came-from c #f))
        (when (= wall (quad-cell-ref c))
          (quad-cell-set! c hall))
        (when next-c
          (loop next-c))))

    (for ([r*c (in-list (shuffle cells))])
      (unless (= wall (quad-cell-ref r*c))
        (unless (hash-has-key? seen? r*c)
          (dig-until-seen r*c)
          (visit r*c)))))

  ;; Turn all the "conn" blocks into "hall".
  ;; We had them different in the template to protect them from
  ;; being walled.
  (for ([r*c (in-list cells)])
    (when (= conn (quad-cell-ref r*c))
      (quad-cell-set! r*c hall)))

  (for ([new-val (in-list (list power-up fruit))])
    (for/or ([r*c (shuffle cells)])
      (and (= hall (quad-cell-ref r*c))
           (quad-cell-set! r*c new-val))))
  new-quad)

(module+ main
  (define (display-maze q)
    (for ([r (in-range h-height)])
      (for ([c (in-range h-width)])
        (display
         (match (quad-ref q r c)
           [(== hall)         "."]
           [(== wall)         "#"]
           [(== power-up)     "!"]
           [(== fruit)        "%"]
           [(== ghost-entry)  "g"]
           [(== player-entry) "@"]
           [(== conn)         "+"])))
      (newline))
    (newline))

  (display-maze quad:template)

  (for-each display-maze quad:templates)

  (display-maze (generate-quad)))

(provide (all-defined-out))
