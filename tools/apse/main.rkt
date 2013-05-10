#lang racket/base
(require racket/gui/base
         racket/file
         racket/format
         racket/class
         racket/match
         racket/list)

(define messages%
  (class* object% ()
    (init parent)
    (init how-many)

    (define vp (new vertical-panel% [parent parent]))
    (define messages
      (for/list ([i (in-range how-many)])
        (new message% [parent vp] [label ""] [auto-resize #t])))

    (define/public (set-messages! ls)
      (for ([m (in-list messages)]
            [l (in-list ls)])
        (send m set-label l)))
    (define/public (set-highlight! hi)
      ;; XXX
      (void))

    (super-new)))

(define (apse db-path)
  ;; Run the state machine
  (define sprite #f)
  (define sprite-dir #f)

  (define image-names #f)
  (define image-pixels #f)
  (define image-bms #f)
  (define image-i 0)

  (define animation-i 0)

  (define palette-names #f)
  (define palette-vectors #f)
  (define palette-i 0)

  (define (load-sprite! new-sprite new-image-i new-palette-i)
    (write-to-file (list new-sprite new-image-i new-palette-i) last-path
                   #:exists 'replace)

    (set! sprite new-sprite)
    (send name-m set-label sprite)

    (set! sprite-dir (build-path db-path "sprites" sprite))

    (match-define (cons new-w new-h)
                  (file->value (build-path sprite-dir "meta")))
    (set! w new-w)
    (set! h new-h)

    (set! image-names
          (filter (λ (p) (regexp-match #rx".img$" (path->string p)))
                  (directory-list sprite-dir)))
    (set! image-pixels
          (for/vector ([i (in-list image-names)])
            (file->bytes
             (build-path sprite-dir i))))

    (set! palette-names (file->value (build-path sprite-dir "palettes")))
    (send palette-list set-messages! palette-names)
    (set! palette-vectors
          (for/vector ([p (in-list palette-names)])
            (define cs
              (file->value
               (build-path db-path "palettes"
                           (format "~a.pal" p))))
            (for/vector ([c (in-vector cs)])
              (match-define (vector a r g b) c)
              (make-object color% r g b (/ a 255)))))
    (update-palette! new-palette-i)

    (update-image! new-image-i))

  (define (update-image! new-image-i)
    (set! image-i new-image-i)
    (send label-m set-label
          (format "~a of ~a" (add1 image-i) (length image-names)))
    (set-cursor! x y))

  (define (update-palette! new-palette-i)
    (set! palette-i new-palette-i)
    (send palette-list set-highlight! palette-i)

    (define palette (vector-ref palette-vectors palette-i))

    ;; XXX update colors

    (set! image-bms
          (for/vector ([ips (in-vector image-pixels)])
            (define bm (make-object bitmap% w h #f #t))
            (define bm-dc (send bm make-dc))

            (for* ([x (in-range w)]
                   [y (in-range h)])
              (define p (bytes-ref ips (+ (* y w) x)))
              (send bm-dc set-pixel x y (vector-ref palette p)))

            bm)))

  (define x 0)
  (define y 0)
  (define w 0)
  (define h 0)

  (define (update-cursor! dx dy)
    (set-cursor! (+ dx x) (+ dy y)))

  (define (set-cursor! nx ny)
    (set! x (modulo nx w))
    (set! y (modulo ny h))
    (update-canvases!)
    ;; XXX record on undo stack
    (format "(~a,~a)" x y))

  (define (handle-key! e)
    (define start (current-inexact-milliseconds))
    (define new-status
      (match* ((send e get-key-code))
        [('up)
         (update-cursor!  0 -1)]
        [('down)
         (update-cursor!  0 +1)]
        [('left)
         (update-cursor! -1  0)]
        [('right)
         (update-cursor! +1  0)]
        ;; xxx add more commands
        [(kc)
         (printf "ignored: ~a\n" kc)
         #f]))
    (define end (current-inexact-milliseconds))
    (when new-status
      (send mw set-status-text
            (~a (~a (- end start)
                    #:min-width 3
                    #:max-width 4
                    #:align 'right)
                "ms: "
                new-status))))

  (define outline-c (make-object color% 255 255 255 1))
  (define (paint-zoomed! c dc #:image-i [the-image-i image-i])
    (define it (send dc get-transformation))
    (send dc set-smoothing 'unsmoothed)

    (define cw (send c get-width))
    (define ch (send c get-height))
    (define the-scale
      (floor (min (/ cw w) (/ ch h))))
    (send dc translate
          (/ (- cw (* w the-scale)) 2)
          (/ (- ch (* h the-scale)) 2))

    (send dc set-scale the-scale the-scale)

    (define bm (vector-ref image-bms the-image-i))
    (send dc draw-bitmap bm 0 0)

    (define bm-dc (send bm make-dc))
    (define the-c (make-object color% 0 0 0 0))
    (send bm-dc get-pixel x y the-c)
    (send dc set-brush the-c 'solid)
    (send dc set-pen outline-c 0 'solid)
    (send dc draw-rectangle x y 1 1)

    (send dc set-transformation it))
  (define (paint-animation! c dc)
    (paint-zoomed! c dc #:image-i animation-i))

  ;; Interact with UI
  (define (update-canvases!)
    (for-each (λ (c) (send c refresh-now))
              (list* zoomed-c animation-c scaled-cs)))

  ;; Set up the UI
  (define apse-frame%
    (class* frame% ()
      (define/override (on-subwindow-char r e)
        (handle-key! e)
        #t)
      (super-new)))

  (define mw (new apse-frame% [label "apse"]
                  [style '(no-resize-border
                           no-caption
                           hide-menu-bar
                           no-system-menu)]))
  (define hp (new horizontal-panel% [parent mw]))
  (define left-vp (new vertical-panel% [parent hp]))
  (define right-vp (new vertical-panel% [parent hp]))
  (define label-m
    (new message% [parent left-vp]
         [label "<Image Index> of <Image Max>"]))
  (define zoomed-c
    (new canvas% [parent left-vp]
         [paint-callback paint-zoomed!]))
  (define name-m
    (new message% [parent right-vp]
         [label "<Sprite Name>"]))
  (define palette-hp (new horizontal-panel% [parent right-vp]
                          [stretchable-height #f]))
  (define palette-list
    ;; XXX indicate that there may be more than 8 and add scrolling
    (new messages% [parent palette-hp]
         [how-many 8]))
  (define palette-info
    (new messages% [parent palette-hp]
         [how-many 8]))
  (send palette-info set-messages!
        (list "<Color 2>"
              "<Color 3>"
              "<Color 4>"
              "<Color 5>"
              "<Color 6>"
              "<Color 7>"
              "<Color 8>"
              "<Color 9>"))
  (send palette-info set-highlight! 2)

  (define ((make-scaled-panel panel%) me them the-parent i)
    (cond
      [(zero? i)
       empty]
      [else
       (define p (new panel% [parent the-parent]))
       (define first-c
         (new canvas% [parent p]
              [paint-callback paint-zoomed!]))
       (list* first-c
              (them them me p (sub1 i)))]))
  (define make-scaled-horizontal
    (make-scaled-panel horizontal-panel%))
  (define make-scaled-vertical
    (make-scaled-panel vertical-panel%))

  (define scaled-cs
    (make-scaled-horizontal
     make-scaled-horizontal
     make-scaled-horizontal
     ;; make-scaled-vertical
     right-vp 4))

  (define animation-c
    (new canvas% [parent right-vp]
         [paint-callback paint-animation!]))

  (define animation-timer
    (new timer%
         [notify-callback
          (λ ()
            (set! animation-i
                  (modulo (add1 animation-i)
                          (length image-names)))
            (send animation-c refresh-now))]))

  (send mw create-status-line)
  (send mw show #t)
  (send zoomed-c focus)

  (define last-path (build-path db-path "last.rktd"))
  (match-define (list last-sprite last-image-i last-palette-i)
                (if (file-exists? last-path)
                  (file->value last-path)
                  (list #f #f #f)))
  (unless last-sprite
    (set! last-sprite
          (first
           (sort
            (map path->string
                 (directory-list (build-path db-path "sprites")))
            string-ci<=?)))
    (set! last-image-i 0)
    (set! last-palette-i 0))

  (load-sprite! last-sprite last-image-i last-palette-i)
  (send animation-timer start (floor (* 1000 1/15))))

(module+ main
  (require racket/cmdline)
  (define db-path "db")
  (command-line #:program "apse"
                #:args ()
                (printf "Starting on db(~v)...\n" db-path)
                (apse db-path)))
