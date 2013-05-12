#lang racket/base
(require racket/gui/base
         racket/file
         racket/path
         racket/function
         racket/format
         racket/class
         racket/match
         racket/list
         (only-in gb/sys/menu calculate-visible-options)
         "db.rkt")
(module+ test
  (require rackunit))

(define (list-remove-at l i)
  (for/list ([e (in-list l)]
             [n (in-naturals)]
             #:unless (= n i))
    e))

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
(define (color-hex? s)
  (and (string? s)
       (= (string-length s) 9)
       (match s
         [(regexp #rx"^#(..)(..)(..)(..)$" (list _ as rs gs bs))
          (define a (string->number as 16))
          (define r (string->number rs 16))
          (define g (string->number gs 16))
          (define b (string->number bs 16))
          (and a r g b (vector a r g b))]
         [_
          #f])))
(module+ test
  (check-equal? (color-hex? #f) #f)
  (check-equal? (color-hex? "12345678") #f)
  (check-equal? (color-hex? "1234567890") #f)
  (check-equal? (color-hex? "123456789") #f)
  (check-equal? (color-hex? "#abcdefgh") #f)
  (check-equal? (color-hex? "#ffffffff") (vector 255 255 255 255)))

(define base-c (make-object color% #xFD #xF6 #xE3 1))
(define outline-c (make-object color% #xFF #x14 #x93 1))
(define all-white-c (make-object color% 255 255 255 1))
(define all-black-c (make-object color% 0 0 0 1))
(define all-transparent-c (make-object color% 0 0 0 0))

(define messages%
  (class* object% ()
    (init parent)
    (init-field how-many)

    (define (paint-messages! canvas dc)
      (send dc set-background base-c)
      (send dc clear)

      (unless (empty? messages)
        (define max-message-len
          (apply max (map (compose string-length cdr) messages)))
        (define-values
          (display-indexes how-many-display-indexes)
          (calculate-visible-options messages
                                     how-many
                                     message-i))

        (send dc set-text-foreground all-black-c)
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
          (define start-w (+ ch w))
          (define rw (max 0 (- (send canvas get-width) start-w)))
          (p dc start-w start-h ch rw))))

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

(define (string-prefix-of? pre)
  (define pre-re (regexp (format "^~a" (regexp-quote pre))))
  (λ (str)
    (regexp-match pre-re str)))
(define (longest-common-prefix l)
  (define empty-trie (hasheq))
  (define empty-entry (cons #f empty-trie))
  (define (trie-add t w)
    (if (empty? w)
      t
      (hash-update t (first w)
                   (lambda (the-e)
                     (match-define (cons word? rest-t) the-e)
                     (cons (or word? (empty? (rest w)))
                           (trie-add rest-t (rest w))))
                   empty-entry)))
  (define (trie-add* s t)
    (trie-add t (string->list s)))
  (define l-trie (foldr trie-add* empty-trie l))
  (list->string
   (let loop ([t l-trie])
     (define c (hash-count t))
     (cond
       [(= c 1)
        (match-define (list (cons k (cons word? nt))) (hash->list t))
        (if word?
          (list k)
          (cons k (loop nt)))]
       [else
        empty]))))
(module+ test
  (check-equal? (longest-common-prefix (list "36" "36-0" "36-1"
                                             "36-2" "36-3"))
                "36"))

(define-syntax-rule (push! l e ...)
  (set! l (list* e ... l)))
(define-syntax-rule (set-push! l e ...)
  (begin (push! l e ...)
         (set! l (remove-duplicates l))))

(define (dimension-string? s)
  (dimension? (string->number s)))

(define (apse db)
  ;; Run the state machine
  (define changed-sprites empty)
  (define changed-palettes empty)
  (define sprite-s #f)

  (define image-bms #f)

  (define image-i 0)
  (define animation-i 0)
  (define palette-i 0)
  (define color-i 0)

  (define (any-changes?)
    (not (and (empty? changed-palettes)
              (empty? changed-sprites))))

  (define (new-image! [copy-from #f])
    (define new-image-i (vector-length (sprite-images sprite-s)))
    (define images-v (sprite-images sprite-s))
    (define new-images-v
      (build-vector (add1 new-image-i)
                    (λ (i)
                      (if (= i new-image-i)
                        (if copy-from
                          (vector-ref images-v copy-from)
                          (make-bytes (* (sprite-width sprite-s)
                                         (sprite-height sprite-s))
                                      0))
                        (vector-ref images-v i)))))
    (set-sprite-images! sprite-s new-images-v)
    (set-push! changed-sprites sprite-s)
    (update-palette! palette-i)
    (update-image! new-image-i))

  (define (save!)
    (cond
      [(any-changes?)
       (for-each (curry sprite-save! db) changed-sprites)
       (set! changed-sprites empty)
       (for-each (curry palette-save! db) changed-palettes)
       (set! changed-palettes empty)
       (~a "changes saved")]
      [else
       (~a "no changes")]))

  (define (load-sprite! new-sprite new-image-i new-palette-i)
    (set-sprite! (load-sprite db new-sprite) new-image-i new-palette-i))
  (define (set-sprite! new-sprite-s new-image-i new-palette-i)
    (set! sprite-s new-sprite-s)
    (define name (sprite-name sprite-s))
    (send name-m set-label name)
    (update-palettes!)
    (update-palette! new-palette-i)
    (update-color! color-i)
    (update-image! new-image-i)
    (last-save! db name)
    (~a "load " name " img#" image-i " pal#" palette-i))

  (define palette-name->palette (make-weak-hash))
  (define (update-palettes!)
    (for ([pn (in-list (sprite-palettes sprite-s))])
      (hash-ref! palette-name->palette pn
                 (λ ()
                   (load-palette db pn))))
    (update-palette-vectors!))

  (define palette->color-vector (make-weak-hasheq))
  (define (update-palette-vectors!)
    (for ([(pn p) (in-hash palette-name->palette)])
      (hash-ref! palette->color-vector p
                 (λ ()
                   (for/vector ([c (in-vector (palette-colors p))])
                     (match-define (vector a r g b) c)
                     (make-object color% r g b (/ a 255))))))
    (send palette-list set-messages!
          (for/list ([pn (in-list (sprite-palettes sprite-s))])
            (define pv
              (hash-ref palette->color-vector
                        (hash-ref palette-name->palette pn)))
            (cons (λ (dc x y ch rw)
                    (define bw (/ rw 8))
                    (for ([i (in-range 2 9)])
                      (define c (vector-ref pv i))
                      (send dc set-brush c 'solid)
                      (send dc set-pen c 0 'solid)
                      (send dc draw-rectangle (+ x (* bw (- i 2))) y bw ch)))
                  pn))))

  (define (update-image! new-image-i)
    (define how-many-images
      (vector-length (sprite-images sprite-s)))
    (set! image-i (modulo new-image-i how-many-images))
    (send label-m set-label
          (format "~a of ~a" (add1 image-i)
                  how-many-images))
    (set-cursor! x y)
    (~a "image = " image-i))

  (define (current-palette-vector)
    (hash-ref
     palette->color-vector
     (hash-ref palette-name->palette
               (list-ref (sprite-palettes sprite-s) palette-i))))

  (define (update-palette! new-palette-i)
    (set! palette-i
          (modulo new-palette-i (length (sprite-palettes sprite-s))))
    (send palette-list set-highlight! palette-i)

    (define palette (current-palette-vector))
    (send palette-info set-messages!
          (for/list ([c (in-vector palette)]
                     [i (in-naturals)])
            (cons (λ (dc x y ch rw)
                    (send dc set-brush c 'solid)
                    (send dc set-pen c 0 'solid)
                    (send dc draw-rectangle x y rw ch))
                  (format "~a: ~a" i (color%->hex c)))))

    (define how-many-images
      (vector-length (sprite-images sprite-s)))
    (set! image-bms (make-vector how-many-images #f))
    (for ([image-i (in-range how-many-images)])
      (update-bitmap! image-i))

    (update-canvases!)

    (~a "palette = " palette-i))

  (define (update-bitmap! image-i)
    (define palette (current-palette-vector))
    (define ips (vector-ref (sprite-images sprite-s) image-i))
    (define w (sprite-width sprite-s))
    (define h (sprite-height sprite-s))
    (define bm
      (make-object bitmap% w h #f #t))
    (define bm-dc (send bm make-dc))

    (for* ([x (in-range w)]
           [y (in-range h)])
      (define p (bytes-ref ips (xy->byte x y)))
      (send bm-dc set-pixel x y (vector-ref palette p)))

    (vector-set! image-bms image-i bm))

  (define (xy->byte x y)
    (+ (* y (sprite-width sprite-s)) x))

  (define (insert-color! c)
    (update-color! c)
    (insert-current-color!))
  (define (update-color! new-color-i)
    (set! color-i (modulo new-color-i 10))
    (send palette-info set-highlight! color-i)
    (~a "color = " color-i))
  (define (insert-current-color!)
    (define ps (vector-ref (sprite-images sprite-s) image-i))
    (define b (xy->byte x y))
    (define old (bytes-ref ps b))

    (define (change-pixel! new)
      (set-push! changed-sprites sprite-s)
      (bytes-set! ps b new)
      (update-bitmap! image-i)
      (update-canvases!)
      (set-cursor! x y))

    (push! undos (λ () (change-pixel! old)))
    (change-pixel! color-i))

  (define x 0)
  (define y 0)
  (define show-cursor? #t)
  (define show-grid? #t)
  (define inverted? #f)

  (define undos empty)
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
    (define w (sprite-width sprite-s))
    (define h (sprite-height sprite-s))
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
        (bytes-ref (vector-ref (sprite-images sprite-s) image-i)
                   (xy->byte x y))))

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

  (define status-prompt-tag (make-continuation-prompt-tag 'status))
  (define-syntax-rule (set-status! expr)
    (let ()
      (define start (current-inexact-milliseconds))
      (define new-status
        (call-with-continuation-prompt
         (λ () expr)
         status-prompt-tag))
      (define end (current-inexact-milliseconds))
      (when new-status
        (send mw set-status-text
              (~a
               (if (any-changes?)
                 "!"
                 " ")
               " "
               (~a (- end start)
                   #:min-width 3
                   #:max-width 4
                   #:align 'right)
               "ms: "
               new-status)))))
  (define (throw-status v)
    (abort-current-continuation status-prompt-tag (λ () v)))

  (define minibuffer-prompt-tag (make-continuation-prompt-tag 'minibuffer))
  (define minibuffer-run! #f)
  (define-syntax-rule (with-minibuffer ke e)
    (call-with-continuation-prompt
     (λ () (if minibuffer-run!
             (minibuffer-run! ke)
             e))
     minibuffer-prompt-tag))
  (define (minibuffer-read prompt
                           #:valid-char? this-valid-char?
                           #:accept-predicate? accept?
                           #:completions [orig-comps empty])
    (define (valid-char? c)
      (and (char? c) (this-valid-char? c)))
    (define comps
      (sort (sort orig-comps string-ci<?) < #:key string-length))
    (begin0
      (call/cc (λ (return-to-minibuffer-call)
                 (define input-so-far "")
                 (set! minibuffer-run!
                       (λ (ke)
                         (define prefix-comps
                           (filter
                            (string-prefix-of? input-so-far)
                            comps))
                         (match (send ke get-key-code)
                           [#\return
                            (when (accept? input-so-far)
                              (return-to-minibuffer-call input-so-far))]
                           [#\tab
                            (if (empty? prefix-comps)
                              (bell)
                              (set! input-so-far
                                    (longest-common-prefix
                                     prefix-comps)))]
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
                               (~a prompt " > " input-so-far
                                   " ["
                                   (if (empty? prefix-comps)
                                     "no matches"
                                     (apply ~a
                                            (add-between prefix-comps " ")))
                                   "]"))
                         (when (or (eq? 'escape (send ke get-key-code))
                                   (and (send ke get-control-down)
                                        (eq? #\g (send ke get-key-code))))
                           (send mw set-status-text
                                 (~a prompt " > " "[CANCELLED]"))
                           (set! minibuffer-run! #f))))
                 (abort-current-continuation minibuffer-prompt-tag void))
               minibuffer-prompt-tag)
      (set! minibuffer-run! #f)))  

  (define (read-palette label)
    (define all-palette-names (db-palettes db))
    (define palette-name
      (minibuffer-read (~a label " palette")
                       #:completions all-palette-names
                       #:valid-char? valid-name-char?
                       #:accept-predicate? name?))
    (unless (member palette-name all-palette-names)
      (define new-palette
        (palette palette-name
                 (vector (vector 0 0 0 0)
                         (vector 255 0 0 0)

                         (vector 0 0 0 0)
                         (vector 0 0 0 0)
                         (vector 0 0 0 0)
                         (vector 0 0 0 0)

                         (vector 0 0 0 0)
                         (vector 0 0 0 0)
                         (vector 0 0 0 0)
                         (vector 0 0 0 0))))
      (hash-set! palette-name->palette palette-name new-palette)
      (set-push! changed-palettes new-palette))
    palette-name)

  (define (ensure-saved!)
    (when (any-changes?)
      (define yes/no-options '("y" "n" "Y" "N"))
      (define ret
        (minibuffer-read "Save your changes? [Yn]"
                         #:completions
                         yes/no-options
                         #:valid-char?
                         (λ (c)
                           (member (char-downcase c)
                                   '(#\y #\n)))
                         #:accept-predicate?
                         (λ (s) (or (string=? s "")
                                    (member s yes/no-options)))))
      (match (string-downcase ret)
        [(or "y" "")
         (throw-status (save!))]
        ["n"
         (void)])))

  (define (read-and-set-sprite!)
    (define all-sprite-names (db-sprites db))
    (define sprite-name
      (minibuffer-read "Sprite"
                       #:completions all-sprite-names
                       #:valid-char? valid-name-char?
                       #:accept-predicate? name?))
    (cond
      [(member sprite-name all-sprite-names)
       (load-sprite! sprite-name 0 0)]
      [else
       (define w
         (string->number
          (minibuffer-read (~a sprite-name "'s width")
                           #:valid-char? char-numeric?
                           #:accept-predicate? dimension-string?)))
       (define h
         (string->number
          (minibuffer-read (~a sprite-name "'s height (width = " w ")")
                           #:valid-char? char-numeric?
                           #:accept-predicate? dimension-string?)))
       (define init-palette (read-palette "Initial"))
       (define new-sprite
         (sprite sprite-name w h
                 (vector (make-bytes (* w h) 0))
                 (list init-palette)))
       (set-push! changed-sprites new-sprite)
       (set-sprite! new-sprite 0 0)]))

  (define (change-palettes! new-palettes)
    (set-sprite-palettes! sprite-s new-palettes)
    (set-push! changed-sprites sprite-s)
    (update-palettes!)
    (update-palette! palette-i))

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
        [(cons #f #\i)
         (set! inverted? (not inverted?))
         (update-canvases!)
         (~a "inverted? = " inverted?)]
        [(cons #t 'up)
         (update-palette! (sub1 palette-i))]
        [(cons #t 'down)
         (update-palette! (add1 palette-i))]
        [(or (cons #t 'left) (cons #f #\[))
         (update-image! (sub1 image-i))]
        [(or (cons #t 'right) (cons #f #\]))
         (update-image! (add1 image-i))]
        [(cons #f #\r)
         (cond
           [(= (length (sprite-palettes sprite-s)) 1)
            (~a "cannot remove last palette")]
           [else
            (begin0
              (~a "removed palette " palette-i)
              (change-palettes!
               (list-remove-at (sprite-palettes sprite-s)
                               palette-i)))])]
        [(cons #f #\a)
         (define new-palette (read-palette "Additional"))
         (change-palettes! (append (sprite-palettes sprite-s)
                                   (list new-palette)))
         (~a "added palette " new-palette)]
        [(cons #f #\f)
         (read-and-set-sprite!)]
        [(or (cons _ 'escape) (cons _ #\q))
         (ensure-saved!)
         (exit 0)]
        [(cons #f #\e)
         (cond
           [(<= color-i 1)
            (~a "cannot modify colors 0 and 1")]
           [else
            (define pn (list-ref (sprite-palettes sprite-s) palette-i))
            (define p (hash-ref palette-name->palette pn))
            (define c%v (hash-ref palette->color-vector p))
            (define (get-cx)
              (color%->hex
               (vector-ref c%v color-i)))
            (define old-cx (get-cx))
            (define color-hex
              (minibuffer-read (format "Change color ~a (~a) to "
                                       color-i
                                       old-cx)
                               #:valid-char?
                               (λ (c)
                                 (or (char=? #\# c)
                                     (string->number (string c) 16)))
                               #:accept-predicate? color-hex?))
            (define new-cv (color-hex? color-hex))
            (vector-set! (palette-colors p) color-i new-cv)
            (set-push! changed-palettes p)
            (hash-remove! palette->color-vector p)
            (update-palette-vectors!)
            (update-palette! palette-i)
            (define new-cx (get-cx))
            (~a "changed color "
                color-i
                " from "
                old-cx
                " to "
                new-cx)])]
        [(cons #f #\space)
         (insert-current-color!)]
        [(cons #f (app color-key? (and (not #f) c)))
         (insert-color! c)]
        [(cons #t (app shifted-color-key? (and (not #f) c)))
         (update-color! c)]
        [kc
         #f]))))

  (define (paint-zoomed! c dc #:image-i [the-image-i image-i])
    (send dc set-background base-c)
    (send dc clear)
    (define it (send dc get-transformation))
    (send dc set-smoothing 'unsmoothed)

    (define cw (send c get-width))
    (define ch (send c get-height))
    (define w (sprite-width sprite-s))
    (define h (sprite-height sprite-s))
    (define the-scale
      (floor (min (/ cw w) (/ ch h))))
    (send dc translate
          (/ (- cw (* w the-scale)) 2)
          (/ (- ch (* h the-scale)) 2))

    (send dc set-scale the-scale the-scale)

    (define bg-c (if inverted? all-black-c all-white-c))
    (send dc set-pen bg-c 0 'solid)
    (send dc set-brush bg-c 'solid)
    (send dc draw-rectangle 0 0 w h)

    (when show-grid?
      (define grid-c (if inverted? all-white-c all-black-c))
      (send dc set-pen grid-c 0 'solid)
      (for ([x (in-range (add1 w))])
        (send dc draw-line x 0 x h))
      (for ([y (in-range (add1 h))])
        (send dc draw-line 0 y w y)))

    (define bm (vector-ref image-bms the-image-i))
    (send dc draw-bitmap bm 0 0)

    (when show-cursor?
      (define bm-dc (send bm make-dc))
      (send dc set-brush all-transparent-c 'solid)
      (send dc set-pen outline-c 0 'solid)
      (send dc draw-rectangle x y 1 1))

    (send dc set-transformation it))
  (define (paint-animation! c dc)
    ;; The image may have been updated since the timer was called
    (set! animation-i
          (modulo animation-i
                  (vector-length (sprite-images sprite-s))))
    (paint-zoomed! c dc #:image-i animation-i))

  ;; Interact with UI
  (define (update-canvases!)
    (for-each (λ (c) (send c refresh-now))
              (list* zoomed-c animation-c scaled-cs)))

  ;; Set up the UI
  (define model-lock (make-semaphore 1))

  (define apse-frame%
    (class* frame% ()
      (define/override (on-subwindow-char r e)
        (call-with-semaphore
         model-lock
         (λ ()
           (handle-key! e)))
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
                   (send dc set-background base-c)
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
            (call-with-semaphore
             model-lock
             (λ ()
               (set! animation-i (add1 animation-i))
               (send animation-c refresh-now))))]))

  (send mw create-status-line)
  (send mw show #t)
  (send zoomed-c focus)

  (set-status! (load-sprite! (load-last db) 0 0))
  (send animation-timer start (floor (* 1000 1/15))))

(module+ main
  (require racket/cmdline)
  (define db-path "db")
  (command-line #:program "apse"
                #:args ()
                (apse (load-db db-path))))