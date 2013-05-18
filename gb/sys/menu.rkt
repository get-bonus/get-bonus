#lang racket/base
(require (for-syntax racket/base)
         racket/list
         racket/match
         gb/audio/3s
         gb/gui/os
         gb/sys/menu-lib
         gb/input/controller
         gb/graphics/ngl-main)
(module+ test
  (require rackunit))

(struct menu:top (code options) #:transparent)
(struct menu:list (code options) #:transparent)
(struct menu:option (text fun) #:transparent)
(struct menu:info (str) #:transparent)
(struct menu:status (str) #:transparent)
(struct menu:action (fun) #:transparent)
(struct menu:auto (code fun) #:transparent)

(define char-height
  (texture-height tex:sos/font:0))
(define char-width
  (texture-width tex:sos/font:0))
(define string->sprites
  (make-string-factory spr:sos/font))
(define cursor ">>")
(define padding 2)

(define (flatten-menu st m)
  (match m
    [(cons fst rst)
     (append (flatten-menu st fst)
             (flatten-menu st rst))]
    [(? empty?)
     empty]
    [(? procedure? p)
     (flatten-menu st (p))]
    [(menu:action _)
     (list m)]
    [(menu:auto _ _)
     (list m)]
    [(menu:top code options)
     (define i (hash-ref st code 0))
     (match-define (menu:option text fun) (list-ref options i))
     (list* m (flatten-menu st (fun)))]
    [(menu:list code options)
     (define i (hash-ref st code 0))
     (match-define (menu:option text fun) (list-ref options i))
     (list* m (flatten-menu st (fun)))]
    [(menu:status text)
     (list m)]
    [(menu:info texts)
     (list m)]))

(define (react-menu c st m)
  (match m
    [(cons fst rst)
     (react-menu c (react-menu c st fst) rst)]
    [(? empty?)
     st]
    [(menu:action fun)
     (when (or (controller-start-down c)
               (controller-b-down c))
       (fun))

     st]
    [(menu:auto id fun)
     (define frames (hash-ref st id 60))
     (when (zero? frames)
       (fun))
     (hash-set st id (sub1 frames))]
    [(menu:top code options)
     (define pos (hash-ref st code 0))
     (define mod
       (cond
         [(controller-l-down c) -1]
         [(controller-r-down c) +1]
         [else                 0]))
     (define pos+
       (modulo (+ pos mod)
               (length options)))

     (hash-set st code pos+)]
    [(menu:list code options)
     (define pos (hash-ref st code 0))
     (define mod
       (cond
         [(controller-up-down c)   -1]
         [(controller-down-down c) +1]
         [else                 0]))
     (define pos+
       (modulo (+ pos mod)
               (length options)))

     (hash-set st code pos+)]
    [(menu:status text)
     st]
    [(menu:info text)
     st]))

(define (draw-menu:top st cm)
  (match (filter menu:top? cm)
    [(list)
     (values 0 empty)]
    ;; XXX check that doesn't go off the right
    [(list (menu:top code options))
     (define top-dy (- crt-height char-height))
     (transform
      #:dy top-dy
      (define-values (final-offset l)
        (for/fold ([offset 0]
                   [l empty])
            ([o (in-list options)]
             [i (in-naturals)])
          (match-define (menu:option text fun) o)
          (define pre-offset
            (+ offset padding (string-length cursor)))
          (define obj
            (transform
             #:dx (* char-width pre-offset)
             (string->sprites text)))
          (define post-offset
            (+ pre-offset (string-length text)))

          (define selected
            (cond
              [(equal? (hash-ref st code #f) i)
               (transform
                #:dx (* char-width (+ offset padding))
                (string->sprites cursor))]
              [else
               empty]))

          (values post-offset (list* selected obj l))))
      (values char-height
              (cons l
                    (draw-menu-box
                     (/ (* char-width (+ final-offset 1)) 2)
                     (/ (* char-height .25) 2)
                     (* char-width (- final-offset padding))
                     char-height))))]))

(define (draw-menu:bot st cm)
  (for/fold ([bot-offset 0]
             [bot-os empty])
      ([m (in-list cm)])
    (match m
      [(menu:status text)
       (define this-dy
         (+ bot-offset char-height))
       (values this-dy
               (cons (transform
                      #:dy this-dy
                      #:dx (- crt-width
                              (* char-width
                                 (+ padding
                                    (string-length text))))
                      (string->sprites text))
                     bot-os))]
      [_
       (values bot-offset bot-os)])))

(define (draw-menu:list top-offset bot-offset st cm)
  (match (filter menu:list? cm)
    [(list)
     (values 0 empty)]
    ;; XXX check that doesn't go off the right
    [(list (menu:list code options))
     (define max-visible-options
       (floor
        (- (/ (- crt-height (+ top-offset bot-offset))
              char-height)
           (add1 padding))))
     (define current-option (hash-ref st code 0))

     (define-values
       (display-indexes how-many-display-indexes)
       (calculate-visible-options options
                                  max-visible-options
                                  current-option))

     (define (option-entry-height pos)
       (* -1 (+ (* char-height (+ padding pos)))))
     (define longest-option-len
       (+ (string-length cursor)
          (apply max
                 (map (compose string-length
                               menu:option-text)
                      options))))

     (values
      (* char-width
         (+ padding
            longest-option-len))
      (transform
       #:dy (- crt-height top-offset)
       #:dx (* padding char-width)
       (cons (for/list ([di (in-list display-indexes)]
                        [i (in-naturals)])
               (transform
                #:dy (option-entry-height i)
                (match di
                  ['before
                   (string->sprites "<<<<")]
                  ['after
                   (string->sprites ">>>>")]
                  [(? number? oi)
                   (define o (list-ref options oi))
                   (match-define (menu:option text fun) o)
                   (cons (transform
                          #:dx (* (string-length cursor) char-width)
                          (string->sprites text))
                         (cond
                           [(equal? current-option oi)
                            (string->sprites cursor)]
                           [else
                            empty]))])))
             (draw-menu-box
              (/ (* char-width (- longest-option-len .5)) 2)
              (/ (* -1 char-height
                    (+ how-many-display-indexes 3)) 2)
              (* char-width longest-option-len)
              (* -1 char-height how-many-display-indexes)))))]))

(define (draw-menu-box cx cy w h)
  (define hw (/ w 2.0))
  (define hh (/ h 2.0))
  (transform #:d cx cy
             #:rgba (/ 207.0 255.0) (/ 227.0 255.0) (/ 255.0 255.0) 1.0
             (rectangle hw hh)))

(define (draw-menu:info top-offset bot-offset left-offset st cm)
  (match (filter menu:info? cm)
    [(list)
     empty]
    ;; XXX make sure don't go too far down via bot-offset
    ;; XXX make sure don't go off the screen to right
    [(list (menu:info texts))
     (transform
      #:dy (- crt-height top-offset)
      #:dx (+ left-offset (* char-width padding))
      (for/list ([text (in-list texts)]
                 [i (in-naturals)])
        (transform
         #:dy (* -1 char-height (+ padding i))
         #:dx (* char-width padding)
         (string->sprites text))))]))

(define (render-menu #:back [back-f #f] m)
  (let loop ([st (hasheq)])
    (define c (os/read* 'controller))

    (define cm
      (flatten-menu st m))

    (define-values (top-offset top-os) (draw-menu:top st cm))
    (define-values (bot-offset bot-os) (draw-menu:bot st cm))
    (when (< (- crt-height top-offset) bot-offset)
      (error 'menu "Top and bot overlap"))
    (define-values (left-offset left-os)
      (draw-menu:list top-offset bot-offset st cm))
    (define right-os
      (draw-menu:info top-offset bot-offset left-offset st cm))

    (os/write
     (list
      (cons 'graphics
            (cons 0 (list* top-os bot-os left-os right-os)))))

    (define new-st
      (react-menu c st cm))

    (when (and back-f
               (or (controller-a-down c)
                   (controller-select-down c)))
      (back-f))

    (loop new-st)))

(provide menu:auto
         menu:top
         menu:list
         menu:option
         menu:info
         menu:status
         menu:action
         render-menu
         calculate-visible-options)
