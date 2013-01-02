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
(struct menu:list* (back? auto options))
(struct menu:option (text fun))
(struct menu:split (top bottom))
(struct menu:info (strings))

(define (menu:list options
                   #:back? [back? #t]
                   #:auto [auto #f])
  (menu:list* back? auto options))

(define char
  (make-char-factory modern 10))
(define char-height
  (texture-height (char #\a)))
(define char-width
  (texture-width (char #\a)))
(define string->sprites
  (make-string-factory char))

(define render-menu
  (match-lambda
   [(menu:music sound sub)
    (os/write
     (list (cons 'sound sound)))
    (render-menu sub)]
   [(menu:modal modes)
    (render-menu
     (menu:list
      (for/list ([m (in-list modes)])
        (menu:option (car m) (λ () (render-menu (cdr m)))))))]
   [(menu:split top bottom)
    (render-menu bottom)]
   [(menu:list* back? auto options)
    (define (option-entry-height pos)
      (+ (/ char-height 2.0) (* char-height (- (length options) pos))))
    (define options-display
      (for/list ([o (in-list options)]
                 [i (in-naturals)])
        (transform
         #:d (* 2.0 char-width) (option-entry-height i)
         (string->sprites (menu:option-text o)))))

    (let/cc return
      (let loop ([input? #f] [pos 0] [auto-count 20])
        (define c (os/read* 'controller))
        (define mod
          (cond
            [(controller-up c)   -1]
            [(controller-down c) +1]
            [else                 0]))
        (define pos+
          (modulo (+ pos mod)
                  (length options)))

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

        ;; XXX ugly input? hack
        (when (not (controller-any-button? c))
          (set! input? #t))

        (when (and input? back?
                   (or (controller-a c)
                       (controller-select c)))
          (return 'done))

        (define selected
          (cond
            [(and input?
                  (controller-start c)
                  (controller-b c))
             pos+]
            [(and auto (zero? auto-count))
             auto]
            [else
             #f]))        

        (when selected
          (set! input? #f)
          ((menu:option-fun (list-ref options selected))))

        (loop input? pos+ (sub1 auto-count))))]))

(provide menu:music
         menu:modal
         menu:list
         menu:option
         menu:split
         menu:info
         render-menu)
