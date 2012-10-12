#lang racket/base
(require racket/path
         racket/file
         racket/match
         racket/class
         racket/draw
         racket/cmdline
         racket/pretty
         gb/lib/pow2-bin)

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

  (define (png-size v)
    (max (vector-ref v 2)
         (vector-ref v 3)))
  (define pngs+bms/sorted
    (sort pngs+bms >=
          #:key
          png-size))

  (define t
    (pack png-size
          pngs+bms/sorted))

  (define tex-size
    (expt 2 (tree-size t)))

  (define atlas-bm (make-object bitmap% tex-size tex-size #f #t))
  (define atlas-bm-dc (new bitmap-dc% [bitmap atlas-bm]))
  (define atlas.free-bm (make-object bitmap% tex-size tex-size #f #t))
  (define atlas.free-bm-dc (new bitmap-dc% [bitmap atlas.free-bm]))

  (define (install! dc free? w h t)
    (match t
      [(tree-empty)
       (void)]
      [(tree-branch pow ul ur ll lr)
       (define unit (expt 2 (sub1 pow)))
       (install! dc free? w h ul)
       (install! dc free? (+ w unit) h ur)
       (install! dc free? w (+ h unit) ll)
       (install! dc free? (+ w unit) (+ h unit) lr)]
      [(tree-singleton _ (vector p bm bm-w bm-h free-bm))
       (define p-id
         (string->symbol
          (regexp-replace
           #rx".png$"
           p
           "")))

       (unless free?
         (pretty-display
          `(define-texture
             ,p-id
             ,w ,h ,bm-w ,bm-h)))

       (send dc draw-bitmap
             (if free? free-bm bm)
             w h)]))

  (with-output-to-file atlas.rkt
    #:exists 'replace
    (λ ()
      (printf "#lang racket/base\n")
      (pretty-display
       `(require gb/graphics/texture-atlas-lib))
      (pretty-display
       `(define texture-atlas-size
          ,tex-size))
      (pretty-display
       `(provide texture-atlas-size))
      (printf "\n")

      (pretty-display
       `(define-texture
          none
          0 0 0 0))

      (install! atlas-bm-dc #f 0 0 t)
      (install! atlas.free-bm-dc #t 0 0 t)))

  (send atlas-bm save-file
        atlas.png
        'png
        100)
  (send atlas.free-bm save-file
        atlas.free.png
        'png
        100)

  (void))
