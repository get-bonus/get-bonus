#lang racket/base
(require racket/gui/base
         racket/format
         racket/list
         racket/class
         "mb-frame.rkt"
         "lib.rkt"
         "db.rkt")

(define (sprite-cut db ss-p)
  (define ss-bm (make-object bitmap% ss-p 'png/alpha))
  (define w (send ss-bm get-width))
  (define h (send ss-bm get-height))

  (define inverted? #t)
  (define query-color #f)
  (define query-sprite #f)
  (define the-scale 1)

  (define palette #f)
  (define locked? #f)
  (define ignored-sprites empty)
  (define chosen-sprites empty)

  (define sprite-w 16)
  (define sprite-h 16)
  (define (sprite-box! dw dh)
    (set! sprite-w (+ sprite-w dw))
    (set! sprite-h (+ sprite-h dh))
    (~a "w: " sprite-w " h: " sprite-h))

  (define sprite-dx 0)
  (define sprite-dy 0)
  (define (sprite-offset! dx dy)
    (set! sprite-dx (modulo (+ dx sprite-dx) w))
    (set! sprite-dy (modulo (+ dy sprite-dy) h))
    (~a "dx: " sprite-dx " dy: " sprite-dy))

  (define view-dx 0)
  (define view-dy 0)
  (define (view-offset! dx dy)
    (set! view-dx (clamp 0 (+ dx view-dx) w))
    (set! view-dy (clamp 0 (+ dy view-dy) h))
    (~a "x: " view-dx " dy: " view-dy))

  (define (handle-key! ke)
    (define k (send ke get-key-code))
    (begin0
      (cond
        [(eq? k #\i)
         (set! inverted? (not inverted?))
         (~a "inverted? = " inverted?)]
        [(eq? k #\-)
         (set! the-scale (* the-scale 0.5))
         (~a "scale = " the-scale)]
        [(eq? k #\=)
         (set! the-scale (* the-scale 2.0))
         (~a "scale = " the-scale)]
        [(and palette (not query-sprite) (eq? k #\c))
         (set! query-sprite 0)
         (set! locked? #t)
         (~a "beginning cutting...")]
        [(and query-sprite (eq? k #\space))
         (begin0 (~a "ignored " query-sprite)
                 (push! ignored-sprites query-sprite)
                 (increment-query-sprite!))]
        [(and query-sprite (eq? k #\return))
         (begin0 (~a "chosen " query-sprite)
                 (push! chosen-sprites query-sprite)
                 (increment-query-sprite!))]
        [(and (not (empty? chosen-sprites))
              (eq? k #\s))
         (define pxs (make-bytes (* sprite-w sprite-h 4)))
         (define old-query-sprite query-sprite)
         (for/fold ([last-name ""])
             ([cs (in-list (reverse chosen-sprites))])
           (set! query-sprite cs)
           (refresh-canvases!)
           (let loop ()
             (define all-sprite-names (db-sprites db))
             (define name
               (minibuffer-read "Name"
                                #:completions all-sprite-names
                                #:init last-name
                                #:valid-char? valid-name-char?
                                #:accept-predicate? name?))
             (cond
               [(member name all-sprite-names)
                (loop)]
               [else
                (define img0 (make-bytes (* sprite-w sprite-h) 0))

                (send ss-bm get-argb-pixels
                      (query-sprite-x) (query-sprite-y)
                      sprite-w sprite-h pxs)
                (for* ([x (in-range sprite-w)]
                       [y (in-range sprite-h)])
                  (define c (pixels-color pxs sprite-w sprite-h x y))
                  (bytes-set! img0
                              (byte-xy-offset sprite-w sprite-h x y)
                              (hash-ref palette c)))

                (define new-sprite
                  (sprite name sprite-w sprite-h
                          (vector img0)
                          ;; xxx save the palette they chose?
                          (list "grayscale")))

                (sprite-save! db new-sprite)

                name])))
         (set! query-sprite old-query-sprite)
         (begin0
           (~a "saved " (length chosen-sprites) " sprites")
           (set! chosen-sprites empty))]
        [(and (not palette) (eq? k #\p))
         ;; find every color in the image
         (define colors (make-hash))

         (define pxs (make-bytes (* w h 4)))
         (send ss-bm get-argb-pixels 0 0 w h pxs)
         (for* ([x (in-range w)]
                [y (in-range h)])
           (hash-update! colors (pixels-color pxs w h x y) add1 0))

         ;; assign it a number
         (define color->index
           (for/hash ([(c count) (in-hash colors)])
             (set! query-color (palette-color->color% c))
             (refresh-canvases!)
             (define label
               (string->number
                (minibuffer-read "Label color"
                                 #:valid-char? color-key?
                                 #:auto-accept? #t
                                 #:accept-predicate?
                                 (λ (s) (= (string-length s) 1)))))
             (set! query-color #f)
             (values c label)))
         (refresh-canvases!)

         (set! palette color->index)

         (~a "palette set")]

        [(and (not locked?) (send ke get-control-down))
         (cond [(eq? k 'left)  (sprite-offset! -1 +0)]
               [(eq? k 'right) (sprite-offset! +1 +0)]
               [(eq? k 'up)    (sprite-offset! +0 -1)]
               [(eq? k 'down)  (sprite-offset! +0 +1)]
               [else #f])]
        [(and (not locked?) (send ke get-shift-down))
         (cond [(eq? k 'left)  (sprite-box! -1 +0)]
               [(eq? k 'right) (sprite-box! +1 +0)]
               [(eq? k 'up)    (sprite-box! +0 -1)]
               [(eq? k 'down)  (sprite-box! +0 +1)]
               [else #f])]
        [(eq? k 'left)  (view-offset! -1 +0)]
        [(eq? k 'right) (view-offset! +1 +0)]
        [(eq? k 'up)    (view-offset! +0 -1)]
        [(eq? k 'down)  (view-offset! +0 +1)]

        [(eq? k 'home)  (view-offset! (* -1 0.1 w) +0)]
        [(eq? k 'end)   (view-offset! (* +1 0.1 w) +0)]
        [(eq? k 'next)  (view-offset! +0 (* +1 0.1 h))]
        [(eq? k 'prior) (view-offset! +0 (* -1 0.1 h))]
        [(eq? k #\q)
         (exit 0)]
        [else
         (eprintf "ignored: ~a\n" k)
         #f])
      (refresh-canvases!)))

  (define (refresh-canvases!)
    (send query-c refresh-now)
    (send ss-c refresh-now))

  (define (paint-query c dc)
    (send dc set-background base-c)
    (send dc clear)

    (cond
      [query-sprite
       (scale-and-center
        c dc all-white-c
        sprite-w sprite-h
        (λ ()
          (send dc draw-bitmap-section
                ss-bm
                0 0
                (query-sprite-x) (query-sprite-y)
                sprite-w sprite-h)))]
      [query-color
       (send dc set-background query-color)
       (send dc clear)]))

  (define (query-sprite-y)
    (sprite-y query-sprite))
  (define (query-sprite-x)
    (sprite-x query-sprite))
  (define (increment-query-sprite!)
    (set! query-sprite (add1 query-sprite))
    (when (>= (query-sprite-y) h)
      (set! query-sprite #f)))

  (define (sprites-per-row)
    (quotient w sprite-w))
  (define (sprite-y i)
    (+ sprite-dy (* sprite-h (quotient i (sprites-per-row)))))
  (define (sprite-x i)
    (+ sprite-dx (* sprite-w (remainder i (sprites-per-row)))))

  (define (paint-ss c dc)
    ;; Make a copy of the bitmap with the grid in place
    (define ss/grid-bm (make-object bitmap% w h #f #t))
    (define ss/grid-dc (send ss/grid-bm make-dc))
    (send ss/grid-dc draw-bitmap ss-bm 0 0)
    (define grid-c (if inverted? all-white-c all-black-c))
    (send ss/grid-dc set-pen grid-c 0 'solid)
    (for ([x (in-range sprite-dx (add1 w) sprite-w)])
      (send ss/grid-dc draw-line x sprite-dy x h))
    (for ([y (in-range sprite-dy (add1 h) sprite-h)])
      (send ss/grid-dc draw-line sprite-dx y w y))

    (for ([sprite-i (in-list ignored-sprites)])
      (send ss/grid-dc set-pen base-c 0 'solid)
      (send ss/grid-dc set-brush base-c 'solid)
      (send ss/grid-dc draw-rectangle
            (sprite-x sprite-i) (sprite-y sprite-i)
            sprite-w sprite-h))

    (when query-sprite
      (send ss/grid-dc set-pen outline-c 0 'solid)
      (send ss/grid-dc set-brush all-transparent-c 'solid)
      (send ss/grid-dc draw-rectangle
            (query-sprite-x) (query-sprite-y)
            sprite-w sprite-h))

    ;; Draw the copy zoomed in
    (send dc set-background base-c)
    (send dc clear)
    (define it (send dc get-transformation))
    (send dc set-smoothing 'unsmoothed)

    (define cw (send c get-width))
    (define ch (send c get-height))

    (define ew (* w the-scale))
    (define eh (* h the-scale))

    ;; Scale
    (send dc set-scale the-scale the-scale)

    (send dc draw-bitmap-section
          ss/grid-bm
          0 0
          view-dx view-dy
          (- w view-dx) (- h view-dy))


    (send dc set-transformation it))

  (define-values (mw model-lock
                     minibuffer-read throw-status
                     initialize!)
    (create-mb-frame
     "sprite-cut" (λ () #f)
     handle-key!))

  (define hp (new horizontal-panel% [parent mw]))
  (define ss-c
    (new canvas% [parent hp]
         [paint-callback paint-ss]))
  (define query-c
    (new canvas% [parent hp]
         [paint-callback paint-query]))

  (initialize! "ready"))

(module+ main
  (require racket/cmdline)
  (define db-path "db")
  (current-command-line-arguments
   #("/home/jay/Downloads/Scroll-o-Sprites.png"))
  (current-command-line-arguments
   #("/home/jay/Dev/scm/github.jeapostrophe/exp/anki-monster/anki-monster-pngs/36.png"))
  (command-line #:program "apse"
                #:once-each
                ["--db" some-path "Use database" (set! db-path some-path)]
                #:args (sprite-sheet-p)
                (sprite-cut (load-db db-path)
                            sprite-sheet-p)))
