#lang racket/base
(require racket/class
         racket/contract
         racket/gui/base)

(define (make-fullscreen-canvas ON-PAINT ON-CHAR)
  (define-values (w h) (get-display-size #t))
  (define-values (x y) (get-display-left-top-inset #t))

  (parameterize ([current-eventspace (make-eventspace)])
    (define frame
      (new frame%
           [label ""]
           [x 0] [y (* -1 y)]
           [width w] [height h]
           [style '(hide-menu-bar
                    no-resize-border
                    no-caption
                    no-system-menu)]))
    (define this-canvas%
      (class canvas%
        (define/override (on-paint)
          (define dc (send this get-dc))
          (define glctx (send dc get-gl-context))
          (unless glctx
            (error 'on-paint "Could not initialize OpenGL!")
            ;; XXX should bring down the whole thing
            (exit 1))
          (send glctx call-as-current
                (λ ()
                  (ON-PAINT
                   (send this get-width)
                   (send this get-height)
                   (λ () (send glctx swap-buffers))))))
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

    (define done-sema
      (make-semaphore))
    (thread
     (λ ()
       (yield done-sema)
       (send frame on-exit)))

    (values (λ (s) (send frame set-label s))
            (λ () (send canvas refresh-now))
            done-sema)))

(provide/contract
 [make-fullscreen-canvas
  (-> (-> number?
          number?
          (-> void)
          void)
      (-> (is-a?/c key-event%)
          void)
      (values (-> string? void)
              (-> void)
              semaphore?))])
