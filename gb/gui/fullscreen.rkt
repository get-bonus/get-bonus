#lang racket/base
(require racket/class
         racket/contract
         racket/gui/base)

(define (make-fullscreen-canvas ON-PAINT ON-CHAR)
  (define-values (w h) (get-display-size #t))
  (define-values (x y) (get-display-left-top-inset #t))

  (define frame
    (new frame%
         [label ""]
         [x 0] [y (* -1 y)]
         [width w] [height h]
         [style '(hide-menu-bar no-resize-border no-caption no-system-menu)]))
  (define this-canvas%
    (class canvas%
      (define/override (on-paint)
        (ON-PAINT this))
      (define/override (on-char k)
        (ON-CHAR k))

      (super-new)))

  (define config
    (new gl-config%))
  (send config set-double-buffered #t)

  (define canvas
    (new this-canvas%
         [parent frame]
         [min-width w]
         [min-height h]
         [gl-config config]
         [style '(gl no-autoclear)]))

  (send frame show #t)
  (send canvas focus)

  (values frame canvas))

(provide/contract
 [make-fullscreen-canvas
  (-> ((is-a?/c canvas<%>) . -> . void)
      ((is-a?/c key-event%) . -> . void)
      (values (is-a?/c frame%)
              (is-a?/c canvas<%>)))])
