#lang racket/base
(require racket/path
         racket/file
         racket/match
         racket/class
         racket/draw
         racket/cmdline
         racket/pretty)

(module+ main
  (command-line #:program "texture-atlas"
                #:args (png rkt . pngs)
                (main png rkt pngs)))

(define (main atlas.png atlas.rkt pngs)
  (define pngs+bms
    (for/list ([p (in-list pngs)])
      (define bm (make-object bitmap% p 'png/alpha))
      (define bm-w (send bm get-width))
      (define bm-h (send bm get-height))
      (vector p bm bm-w bm-h)))
  (define pngs+bms/sorted
    (sort pngs+bms <= #:key (λ (v) (vector-ref v 3))))

  (define how-many (length pngs))
  (define shelves (inexact->exact (ceiling (sqrt how-many))))

  (define-values (install! tot-w tot-h)
    (for/fold ([install! void]
               [max-w 0]
               [h 0])
        ([shelf-i (in-range shelves)])
      (define-values
        (shelf-install! shelf-w shelf-h)
        (for/fold ([shelf-install! void]
                   [w 0]
                   [max-h 0])
            ([p+bm (in-list pngs+bms/sorted)]
             [i (in-naturals)]
             #:when (= shelf-i (quotient i shelves)))
          (match-define (vector p bm bm-w bm-h) p+bm)
          (define p-id
            (string->symbol
             (regexp-replace
              #rx".png$"
              p
              "")))
          (values
           (λ (bm-dc)
             (send bm-dc draw-bitmap
                   bm w h)
             (pretty-display
              `(define-texture
                 ,p-id
                 the-texture-atlas
                 ,w ,h ,bm-w ,bm-h))
             #;(pretty-display
              `(define-texture
                 ,p-id
                 the-texture-atlas
                 (exact->inexact (/ ,w ,tot-w))
                 (exact->inexact (/ ,h ,tot-h))
                 (exact->inexact (/ ,bm-w ,tot-w))
                 (exact->inexact (/ ,bm-h ,tot-h))))
             (shelf-install! bm-dc))
           (+ w bm-w)
           (max max-h bm-h))))
      (values
       (λ (bm-dc)
         (shelf-install! bm-dc)
         (install! bm-dc))
       (max max-w shelf-w)
       (+ h shelf-h))))

  (define atlas-bm (make-object bitmap% tot-w tot-h #f #t))
  (define atlas-bm-dc (new bitmap-dc% [bitmap atlas-bm]))

  (with-output-to-file atlas.rkt
    #:exists 'replace
    (λ ()
      (printf "#lang racket/base\n")
      (pretty-display
       `(require gb/graphics/texture-atlas-lib))
      (pretty-display
       `(define the-texture-atlas
          (texture-atlas/size (add1 ,how-many))))
      (pretty-display
       `(define texture-atlas-width
          ,tot-w))
      (pretty-display
       `(define texture-atlas-height
          ,tot-h))
      (pretty-display
       `(provide the-texture-atlas
                 texture-atlas-width
                 texture-atlas-height))
      (printf "\n")

      (pretty-display
       `(define-texture
          none
          the-texture-atlas
          0 0 0 0))
      
      (install! atlas-bm-dc)))

  (send atlas-bm save-file
        atlas.png
        'png
        100)

  (void))
