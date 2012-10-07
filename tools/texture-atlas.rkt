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
                #:args (free-png png rkt base free-base . pngs)
                (main free-png png rkt base free-base pngs)))

(define (main atlas.free.png atlas.png atlas.rkt
              base free-base
              pngs)
  (define pngs+bms
    (for/list ([p (in-list pngs)])
      (define unfree-p (build-path base p))
      (define free-p (build-path free-base p))
      (define bm
        (if (file-exists? unfree-p)
          (make-object bitmap% unfree-p 'png/alpha)
          (error 'texture-atlas "Missing file: ~a" unfree-p)))
      (define bm-w (send bm get-width))
      (define bm-h (send bm get-height))
      (define free-bm
        (if (file-exists? free-p)
          (make-object bitmap% free-p 'png/alpha)
          bm))
      (vector p bm bm-w bm-h free-bm)))
  (define pngs+bms/sorted
    (sort pngs+bms <= #:key (λ (v) (vector-ref v 3))))

  (define how-many (length pngs))
  (define shelves (inexact->exact (ceiling (sqrt how-many))))

  (define-values (install! free-install! tot-w tot-h)
    (for/fold ([install! void]
               [free-install! void]
               [max-w 0]
               [h 0])
        ([shelf-i (in-range shelves)])
      (define-values
        (shelf-install! shelf-free-install! shelf-w shelf-h)
        (for/fold ([shelf-install! void]
                   [shelf-free-install! void]
                   [w 0]
                   [max-h 0])
            ([p+bm (in-list pngs+bms/sorted)]
             [i (in-naturals)]
             #:when (= shelf-i (quotient i shelves)))
          (match-define (vector p bm bm-w bm-h free-bm) p+bm)
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
                 ,w ,h ,bm-w ,bm-h))
             (shelf-install! bm-dc))
           (λ (bm-dc)
             (send bm-dc draw-bitmap
                   free-bm w h)
             (shelf-free-install! bm-dc))
           (+ w bm-w)
           (max max-h bm-h))))
      (values
       (λ (bm-dc)
         (shelf-install! bm-dc)
         (install! bm-dc))
       (λ (bm-dc)
         (shelf-free-install! bm-dc)
         (free-install! bm-dc))
       (max max-w shelf-w)
       (+ h shelf-h))))

  (define atlas-bm (make-object bitmap% tot-w tot-h #f #t))
  (define atlas-bm-dc (new bitmap-dc% [bitmap atlas-bm]))
  (define atlas.free-bm (make-object bitmap% tot-w tot-h #f #t))
  (define atlas.free-bm-dc (new bitmap-dc% [bitmap atlas.free-bm]))
  
  (with-output-to-file atlas.rkt
    #:exists 'replace
    (λ ()
      (printf "#lang racket/base\n")
      (pretty-display
       `(require gb/graphics/texture-atlas-lib))
      (pretty-display
       `(define texture-atlas-width
          ,tot-w))
      (pretty-display
       `(define texture-atlas-height
          ,tot-h))
      (pretty-display
       `(provide texture-atlas-width
                 texture-atlas-height))
      (printf "\n")

      (pretty-display
       `(define-texture
          none
          0 0 0 0))
      
      (install! atlas-bm-dc)
      (free-install! atlas.free-bm-dc)))

  (send atlas-bm save-file
        atlas.png
        'png
        100)
  (send atlas.free-bm save-file
        atlas.free.png
        'png
        100)

  (void))
