#lang racket/base
(require racket/runtime-path
         racket/match
         racket/file
         (except-in racket/list
                    permutations)
         gb/lib/random
         data/enumerate
         data/enumerate/lib
         gb/lib/godel-seq
         math/number-theory
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

(define wall-visit-order
  (build-list 63 (λ (x) x)))

(define (generate-quad)
  (generate-quad/template
   (list-ref/random quad:templates)
   (shuffle wall-visit-order)))

(define (generate-quad/template
         the-template
         wall-visit-i)
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

  (define hall-cells
    (filter (λ (r*c) (= hall (quad-cell-ref r*c))) cells))

  (unless (= (length hall-cells)
             (length wall-visit-i))
    (error 'generate-quad "Wall visit order wrong size!"))

  (define visit-order
    (map (curry list-ref hall-cells) wall-visit-i))

  ;; For every cell, consider turning it into a wall if
  ;; all its non-wall neighbors have more than 2 exits
  ;; i.e. doing so does not create a dead-end.
  (for ([r*c (in-list visit-order)])
    (define cns (non-wall-neighbors r*c))
    (when (for/and ([cn (in-list cns)])
            (define how-many-halls
              (length (non-wall-neighbors cn)))
            (how-many-halls . > . 2))
      (quad-cell-set! r*c wall)))

  (define seen? (make-hash))
  (define (visit r*c)
    (match-define (cons r c) r*c)
    (unless (hash-has-key? seen? r*c)
      (hash-set! seen? r*c #t)
      (for-each visit
                (filter inside-maze?
                        (non-wall-neighbors r*c)))))

  (define (set-first-hall-to! new-val)
    (for/or ([r*c visit-order])
      (and (= hall (quad-cell-ref r*c))
           (quad-cell-set! r*c new-val)
           r*c)))

  (define player-entry-cell
    (set-first-hall-to! player-entry))

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

    (for ([r*c (in-list visit-order)])
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

  ;; XXX incorporate ghost position (so that I don't put a ghost in a
  ;; wall if I'm switching the state of a quad)
  (map set-first-hall-to!
       (list power-up fruit ghost-entry))

  new-quad)

(define maze/e
  (map/e (match-lambda
          [(cons template visit-order)
           (generate-quad/template template visit-order)])
         (match-lambda
          [_
           (error 'maze/s "Encoding not supported")])
         (cons/e
          (from-list/e quad:templates)
          (permutations-of-n/e (length wall-visit-order)))))

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

  #;
  (display-maze quad:template)

  #;
  (for-each display-maze quad:templates)

  #;
  (display-maze
  (generate-quad))

  #;
  (display-maze
  (generate-quad/template
  (first quad:templates)
  wall-visit-order))

  #;
  (display-maze
  (generate-quad/template
  (second quad:templates)
  wall-visit-order))

  ;; 8 is the first time you notice
  ;; 9 is tough
  (define s (permutations/e '(0 1 2 3 4 5 6 7 8 9)))
  (for ([i (in-range 10)])
    (printf "~a = ~a\n" i (from-nat s i)))
  (newline)
  ;;(error 'done)

  (printf "k: ~a (~a)\n"
          (size maze/e)
          (/ (log (size maze/e))
             (log 10)))
  (for ([i (in-range 1)])
    (define m (from-nat maze/e i))
    (printf "~a =\n" i)
    (display-maze m))

  (require racket/generator gb/lib/pi)
  (define last (current-seconds))
  (for ([i (10-sequence->K-sequence (size maze/e) (in-generator (BPP-digits 100)))]
        [n (in-range 10)])
    (define m (from-nat maze/e i))
    (define now (current-seconds))
    (printf "(~a) ~a =\n" (- now last) i)
    (set! last now)
    (display-maze m)))

(provide (all-defined-out))
