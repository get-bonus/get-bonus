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

  (define (png-w v)
    (vector-ref v 2))
  (define (png-h v)
    (vector-ref v 3))  

  (define-values
    (tex-size places)
    (pack png-w png-h
          pngs+bms))

  (define atlas-bm (make-object bitmap% tex-size tex-size #f #t))
  (define atlas-bm-dc (new bitmap-dc% [bitmap atlas-bm]))
  (define atlas.free-bm (make-object bitmap% tex-size tex-size #f #t))
  (define atlas.free-bm-dc (new bitmap-dc% [bitmap atlas.free-bm]))

  (define (install! dc free?)
    (for ([pl (in-list places)])
      (match-define (placement w h (vector p bm bm-w bm-h free-bm)) pl)
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
            w h)))

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

      (install! atlas-bm-dc #f)
      (install! atlas.free-bm-dc #t)))

  (send atlas-bm save-file
        atlas.png
        'png
        100)
  (send atlas.free-bm save-file
        atlas.free.png
        'png
        100)

  (void))
