#lang racket/base
(require racket/gui/base
         racket/file
         racket/format
         racket/class
         racket/match
         racket/list
         (only-in gb/sys/menu calculate-visible-options))

(define (directory-list* d)
  (sort
   (map path->string
        (directory-list d))
   string-ci<=?))

(define (color%->hex c)
  (apply string-append
         "#"
         (for/list ([b (in-list (list (inexact->exact (* 255 (send c alpha)))
                                      (send c red)
                                      (send c green)
                                      (send c blue)))])
           (~a (number->string b 16)
               #:min-width 2
               #:pad-string "0"))))

(define base-color (make-object color% #xFD #xF6 #xE3 1))

(define messages%
  (class* object% ()
    (init parent)
    (init-field how-many)

    (define (paint-messages! canvas dc)
      (send dc set-background base-color)
      (send dc clear)

      (unless (empty? messages)
        (define max-message-len
          (apply max (map (compose string-length cdr) messages)))
        (define-values
          (display-indexes how-many-display-indexes)
          (calculate-visible-options messages
                                     how-many
                                     message-i))

        (send dc set-text-foreground (make-object color% 0 0 0 1))
        (for ([di (in-list display-indexes)]
              [i (in-naturals)])
          (define m
            (match di
              ['after
               (cons void ">>>>")]
              ['before
               (cons void "<<<<")]
              [(? number? n)
               (list-ref messages n)]))
          (match-define (cons p mt) m)
          (define text
            (~a
             #:min-width (+ 2 max-message-len)
             #:align 'right
             (format "~a ~a"
                     (if (equal? di message-i)
                       "!"
                       " ")
                     mt)))
          (define start-h (* ch i))
          (send dc draw-text text 0 start-h)
          (define-values (w _0 _1 _2) (send dc get-text-extent text))
          (p dc (+ ch w) start-h ch))))

    (define c (new canvas% [parent parent]
                   [stretchable-height #f]
                   [paint-callback paint-messages!]))
    (define dc (send c get-dc))
    (send dc set-font normal-control-font)
    (define ch (send dc get-char-height))
    (define h (inexact->exact
               (ceiling (* how-many ch))))
    (send c min-height h)

    (define message-i 0)
    (define messages empty)

    (define/public (set-messages! ls)
      (set! messages ls)
      (send c refresh-now))
    (define/public (set-highlight! hi)
      (set! message-i hi)
      (send c refresh-now))

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

  (define color-i 0)

  (define (new-image! [copy-from #f])
    (define new-image-i (length image-names))
    (display-to-file
     (if copy-from
       (vector-ref image-pixels copy-from)
       (make-bytes (* w h) 0))
     (build-path sprite-dir (format "~a.img" new-image-i)))
    (load-sprite! sprite new-image-i palette-i))

  (define (save!)
    (cond
      [need-to-save?
       ;; dump pixels
       (for ([i (in-list image-names)]
             [ps (in-vector image-pixels)])
         (display-to-file ps (build-path sprite-dir i)
                          #:exists 'replace))
       ;; dump palettes
       (write-to-file palette-names
                      (build-path sprite-dir "palettes")
                      #:exists 'replace)
       (set! need-to-save? #f)
       (~a "changes saved")]
      [else
       (~a "no changes")]))

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
          (filter (λ (p) (regexp-match #rx".img$" p))
                  (directory-list* sprite-dir)))
    (set! image-pixels
          (for/vector ([i (in-list image-names)])
            (file->bytes
             (build-path sprite-dir i))))

    (set! palette-names (file->value (build-path sprite-dir "palettes")))
    (set! palette-vectors
          (for/vector ([p (in-list palette-names)])
            (define cs
              (file->value
               (build-path db-path "palettes"
                           (format "~a.pal" p))))
            (for/vector ([c (in-vector cs)])
              (match-define (vector a r g b) c)
              (make-object color% r g b (/ a 255)))))
    (send palette-list set-messages!
          (for/list ([pn (in-list palette-names)]
                     [pv (in-vector palette-vectors)])
            (cons (λ (dc x y ch)
                    (define bw (* ch 1/2))
                    (for ([c (in-vector pv)]
                          [i (in-naturals)])
                      (send dc set-brush c 'solid)
                      (send dc set-pen c 0 'solid)
                      (send dc draw-rectangle (+ x (* bw i)) y bw ch)))
                  pn)))

    (update-palette! new-palette-i)
    (update-color! color-i)
    (update-image! new-image-i)

    (~a "load " sprite " img#" new-image-i " pal#" new-palette-i))

  (define (update-image! new-image-i)
    (set! image-i (modulo new-image-i (length image-names)))
    (send label-m set-label
          (format "~a of ~a" (add1 image-i) (length image-names)))
    (set-cursor! x y)
    (~a "image = " image-i))

  (define (update-palette! new-palette-i)
    (set! palette-i (modulo new-palette-i (length palette-names)))
    (send palette-list set-highlight! palette-i)

    (define palette (vector-ref palette-vectors palette-i))
    (send palette-info set-messages!
          (for/list ([c (in-vector palette)]
                     [i (in-naturals)])
            (cons (λ (dc x y ch)
                    (send dc set-brush c 'solid)
                    (send dc set-pen c 0 'solid)
                    (send dc draw-rectangle x y ch ch))
                  (format "~a: ~a" i (color%->hex c)))))

    (set! image-bms (make-vector (length image-names) #f))
    (for ([image-i (in-range (length image-names))])
      (update-bitmap! image-i))

    (update-canvases!)

    (~a "palette = " palette-i))

  ;; transparent
  (define color0 (make-object color% 0 0 0 0))
  ;; black
  (define color1 (make-object color% 0 0 0 1))

  (define (update-bitmap! image-i)
    (define palette (vector-ref palette-vectors palette-i))
    (define ips (vector-ref image-pixels image-i))
    (define bm (make-object bitmap% w h #f #t))
    (define bm-dc (send bm make-dc))

    (for* ([x (in-range w)]
           [y (in-range h)])
      (define p (bytes-ref ips (xy->byte x y)))
      (send bm-dc set-pixel x y (vector-ref palette p)))

    (vector-set! image-bms image-i bm))

  (define (xy->byte x y)
    (+ (* y w) x))

  (define (insert-color! c)
    (update-color! c)
    (insert-current-color!))
  (define (update-color! new-color-i)
    (set! color-i (modulo new-color-i 10))
    (send palette-info set-highlight! color-i)
    (~a "color = " color-i))
  (define (insert-current-color!)
    (define ps (vector-ref image-pixels image-i))
    (define b (xy->byte x y))
    (define old (bytes-ref ps b))

    (define (change-pixel! new)
      (set! need-to-save? #t)
      (bytes-set! ps b new)
      (update-bitmap! image-i)
      (update-canvases!)
      (set-cursor! x y))

    (push-undo! (λ () (change-pixel! old)))
    (change-pixel! color-i))

  (define x 0)
  (define y 0)
  (define w 0)
  (define h 0)
  (define show-cursor? #t)
  (define show-grid? #t)
  (define need-to-save? #f)

  (define undos empty)
  (define (push-undo! t)
    (set! undos (cons t undos)))
  (define (undo!)
    (cond
      [(empty? undos)
       (~a "empty undo stack")]
      [else
       (define this (first undos))
       (set! undos (rest undos))
       (define msg (this))
       (~a "undo: " msg)]))

  (define (update-cursor! dx dy)
    (set-cursor! (+ dx x) (+ dy y)))

  (define (set-cursor! nx ny)
    (set! x (modulo nx w))
    (set! y (modulo ny h))

    (update-canvases!)

    (~a "("
        (~a x
            #:min-width (string-length (number->string w))
            #:align 'right)
        ","
        (~a y
            #:min-width (string-length (number->string h))
            #:align 'right)
        ") = "
        (bytes-ref (vector-ref image-pixels image-i) (xy->byte x y))))

  (define (color-key? c)
    (for/or ([some-c (in-string "0123456789")]
             [i (in-naturals)]
             #:when (eq? c some-c))
      i))
  (define (shifted-color-key? c)
    (for/or ([some-c (in-string ")!@#$%^&*(")]
             [i (in-naturals)]
             #:when (eq? c some-c))
      i))

  (define-syntax-rule (set-status! expr)
    (let ()
      (define start (current-inexact-milliseconds))
      (define new-status
        expr)
      (define end (current-inexact-milliseconds))
      (when new-status
        (send mw set-status-text
              (~a
               (if need-to-save?
                 "!"
                 " ")
               " "
               (~a (- end start)
                   #:min-width 3
                   #:max-width 4
                   #:align 'right)
               "ms: "
               new-status)))))

  (define (valid-char? c)
    (and (char? c)
         (or (char-alphabetic? c)
             (char-numeric? c)
             (char=? c #\-)
             (char=? c #\/))))
  (define minibuffer-prompt-tag (make-continuation-prompt-tag 'minibuffer))
  (define minibuffer-run! #f)
  (define-syntax-rule (with-minibuffer ke e)
    (call-with-continuation-prompt
     (λ () (if minibuffer-run!
             (minibuffer-run! ke)
             e))
     minibuffer-prompt-tag))
  (define (minibuffer-read prompt
                           #:accept-predicate? accept?
                           #:completions comps)
    (begin0
      (call/cc (λ (return-to-minibuffer-call)
                 (define input-so-far "")
                 (set! minibuffer-run!
                       (λ (ke)
                         (match (send ke get-key-code)
                           [#\return
                            (when (accept? input-so-far)
                              (return-to-minibuffer-call input-so-far))]
                           ;; xxx comps with tab
                           [(or #\backspace #\rubout)
                            (unless (string=? "" input-so-far)
                              (set! input-so-far
                                    (substring
                                     input-so-far 0
                                     (sub1
                                      (string-length input-so-far)))))]
                           [(? valid-char? c)
                            (set! input-so-far
                                  (string-append input-so-far
                                                 (string c)))]
                           [_ (void)])
                         (send mw set-status-text
                               (~a prompt " > " input-so-far))))
                 (abort-current-continuation minibuffer-prompt-tag void))
               minibuffer-prompt-tag)
      (set! minibuffer-run! #f)))

  (define (handle-key! e)
    (with-minibuffer
     e
     (set-status!
      (match (cons (send e get-shift-down) (send e get-key-code))
        [(cons #f #\g)
         (set! show-grid? (not show-grid?))
         (update-canvases!)
         (~a "show-grid? = " show-grid?)]
        [(cons #f #\c)
         (set! show-cursor? (not show-cursor?))
         (update-canvases!)
         (~a "show-cursor? = " show-cursor?)]
        [(cons #f #\s)
         (save!)]
        [(cons #f #\z)
         (undo!)]
        [(cons #f #\n)
         (new-image!)]
        [(cons #f #\t)
         (new-image! image-i)]
        [(cons #f 'up)
         (update-cursor!  0 -1)]
        [(cons #f 'down)
         (update-cursor!  0 +1)]
        [(cons #f 'left)
         (update-cursor! -1  0)]
        [(cons #f 'right)
         (update-cursor! +1  0)]
        [(cons #t 'up)
         (update-palette! (sub1 palette-i))]
        [(cons #t 'down)
         (update-palette! (add1 palette-i))]
        [(or (cons #t 'left) (cons #f #\[))
         (update-image! (sub1 image-i))]
        [(or (cons #t 'right) (cons #f #\]))
         (update-image! (add1 image-i))]
        [(or (cons _ 'escape) (cons _ #\q))
         (when need-to-save?
           (define yes/no-options '("y" "n" "Y" "N"))
           (define ret
             (minibuffer-read "Save your changes? [Yn]"
                              #:completions
                              yes/no-options
                              #:accept-predicate?
                              (λ (s) (or (string=? s "")
                                         (member s yes/no-options)))))
           (match (string-downcase ret)
             [(or "y" "")
              (save!)]
             ["n"
              (void)]))
         (exit 0)]
        [(cons #f #\space)
         (insert-current-color!)]
        [(cons #f (app color-key? (and (not #f) c)))
         (insert-color! c)]
        [(cons #t (app shifted-color-key? (and (not #f) c)))
         (update-color! c)]
        [kc
         #f]))))

  (define outline-c (make-object color% #xFF #x14 #x93 1))
  (define all-white (make-object color% 255 255 255 1))
  (define (paint-zoomed! c dc #:image-i [the-image-i image-i])
    (send dc set-background base-color)
    (send dc clear)
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

    (send dc set-pen all-white 0 'solid)
    (send dc set-brush all-white 'solid)
    (send dc draw-rectangle 0 0 w h)

    (when show-grid?
      (send dc set-pen color1 0 'solid)
      (for ([x (in-range (add1 w))])
        (send dc draw-line x 0 x h))
      (for ([y (in-range (add1 h))])
        (send dc draw-line 0 y w y)))

    (define bm (vector-ref image-bms the-image-i))
    (send dc draw-bitmap bm 0 0)

    (when show-cursor?
      (define bm-dc (send bm make-dc))
      (send dc set-brush color0 'solid)
      (send dc set-pen outline-c 0 'solid)
      (send dc draw-rectangle x y 1 1))

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
    (new messages% [parent palette-hp]
         [how-many 10]))
  (define palette-info
    (new messages% [parent palette-hp]
         [how-many 10]))

  (define ((make-scaled-panel panel%) me them the-parent i)
    (cond
      [(zero? i)
       empty]
      [else
       (define p (new panel% [parent the-parent]))
       (define first-c
         (new canvas% [parent p]
              [paint-callback
               (if (= 1 i)
                 (λ (c dc)
                   (send dc set-background base-color)
                   (send dc clear))
                 paint-zoomed!)]))
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
     right-vp 5))

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
  ;; XXX all sorts of broken on first opening with no sprites
  (unless last-sprite
    (set! last-sprite
          (first (directory-list* (build-path db-path "sprites"))))
    (set! last-image-i 0)
    (set! last-palette-i 0))

  (set-status! (load-sprite! last-sprite last-image-i last-palette-i))
  ;; xxx this is not a good rate
  (send animation-timer start (floor (* 1000 1/15))))

(module+ main
  (require racket/cmdline)
  (define db-path "db")
  ;; xxx create a db object and don't expose db-path
  (command-line #:program "apse"
                #:args ()
                (printf "Starting on db(~v)...\n" db-path)
                (apse db-path)))
