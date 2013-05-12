#lang racket/base
(require racket/gui/base
         racket/format
         racket/class
         "mb-frame.rkt"
         "lib.rkt"
         "db.rkt")

(define (sprite-cut db ss-p)
  (define ss-bm (make-object bitmap% ss-p 'png/alpha))
  (define w (send ss-bm get-width))
  (define h (send ss-bm get-height))

  (define inverted? #t)
  (define label-color #f)
  (define the-scale 1)

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
        [(eq? k #\p)
         ;; find every color in the image
         (define colors (make-hash))

         (define pxs (make-bytes (* w h 4)))
         (send ss-bm get-argb-pixels 0 0 w h pxs)
         (for* ([x (in-range w)]
                [y (in-range h)])
           (define xy-offset (+ (* 4 y w) (* 4 x)))
           (define a (bytes-ref pxs (+ xy-offset 0)))
           (define r (bytes-ref pxs (+ xy-offset 1)))
           (define g (bytes-ref pxs (+ xy-offset 2)))
           (define b (bytes-ref pxs (+ xy-offset 3)))
           (define c (vector a r g b))
           (hash-update! colors c add1 0))

         ;; assign it a number
         (define color->index
           (for/hash ([(c count) (in-hash colors)])
             (set! label-color (palette-color->color% c))
             (send query-c refresh-now)
             (define label
               (string->number
                (minibuffer-read "Label color"
                                 #:valid-char? color-key?
                                 #:accept-predicate?
                                 (λ (s) (= (string-length s) 1)))))   
             (set! label-color #f)
             (values c label)))
         (send query-c refresh-now)

         (eprintf "~a\n" color->index)

         (~a "palette set")]

        [(send ke get-control-down)
         (cond [(eq? k 'left)  (sprite-offset! -1 +0)]
               [(eq? k 'right) (sprite-offset! +1 +0)]
               [(eq? k 'up)    (sprite-offset! +0 -1)]
               [(eq? k 'down)  (sprite-offset! +0 +1)]
               [else #f])]
        [(send ke get-shift-down)
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
      (send ss-c refresh-now)))

  (define (paint-query c dc)
    (send dc set-background base-c)
    (send dc clear)

    (cond
      [label-color
       (send dc set-background label-color)
       (send dc clear)]))

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
  (command-line #:program "apse"
                #:once-each
                ["--db" some-path "Use database" (set! db-path some-path)]
                #:args (sprite-sheet-p)
                (load-db db-path)
                (sprite-cut #f sprite-sheet-p)))
