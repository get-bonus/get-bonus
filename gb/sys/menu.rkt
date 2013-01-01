#lang racket/base
(require (for-syntax racket/base)
         racket/list
         racket/match
         gb/audio/3s
         gb/gui/os
         gb/input/controller
         gb/graphics/ngl-main)

(struct menu:music (sound sub))
(struct menu:modal (modes))
(struct menu:list (options))
(struct menu:option (text fun))

(define render-menu
  (match-lambda
   [(menu:music sound sub)
    (os/write
     (list (cons 'sound sound)))
    (render-menu sub)]
   [(menu:modal modes)
    ;; XXX this isn't what I want it to be
    (render-menu
     (menu:list
      (for/list ([m (in-list modes)])
        (menu:option (car m) (λ () (render-menu (cdr m)))))))]
   [(menu:list options)
    (define modern-12-char
      (make-char-factory modern 12))
    (define char-height
      (texture-height (modern-12-char #\a)))
    (define char-width
      (texture-width (modern-12-char #\a)))
    (define string->sprites
      (make-string-factory modern-12-char))

    (define (option-entry-height pos)
      (+ (/ char-height 2.0) (* char-height pos)))
    (define options-display
      (for/list ([o (in-list options)]
                 [i (in-naturals)])
        (transform
         #:d (* 2.0 char-width) (option-entry-height i)
         (string->sprites (menu:option-text o)))))

    (let/cc return
      (let loop ([input? #f] [pos 0])
        (define c (os/read* 'controller))
        (define mod
          (cond
            [(controller-up c)   +1]
            [(controller-down c) -1]
            [else                 0]))
        (define pos+
          (modulo (+ pos mod)
                  (length options)))

        ;; XXX ugly input? hack
        (when (not (controller-any-button? c))
          (set! input? #t))

        (when (and input?
                   (or (controller-a c)
                       (controller-select c)))
          (return 'done))

        (when (and input?
                   (controller-any-button? c))
          (set! input? #f)
          ((menu:option-fun (list-ref options pos+))))

        (for ([frame
               (in-range
                0
                (if (= pos pos+)
                  1
                  ;; How many frames to wait for more input
                  8))])
          (os/write
           (list
            (cons 'graphics
                  (cons 0
                        (cons
                         options-display
                         (transform
                          #:dy (option-entry-height pos)
                          (string->sprites ">>"))))))))

        (loop input? pos+)))]))

(provide menu:music
         menu:modal
         menu:list
         menu:option
         render-menu)
