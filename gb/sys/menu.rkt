#lang racket/base
(require (for-syntax racket/base)
         racket/list
         racket/match
         gb/audio/3s
         gb/gui/os
         gb/input/controller
         gb/graphics/ngl-main)

(struct menu:top (options) #:transparent)
(struct menu:list (options) #:transparent)
(struct menu:option (text fun) #:transparent)
(struct menu:info (str) #:transparent)
(struct menu:status (str) #:transparent)
(struct menu:action (fun) #:transparent)
(struct menu:auto (code fun) #:transparent)

(define char
  (make-char-factory modern 10))
(define char-height
  (texture-height (char #\a)))
(define char-width
  (texture-width (char #\a)))
(define string->sprites
  (make-string-factory char))
(define cursor ">>")
(define top-padding 2)

(define (draw-menu st m)
  (match m
    [(cons fst rst)
     (cons (draw-menu st fst) (draw-menu st rst))]
    [(? empty?)
     empty]
    [(menu:action _)
     empty]
    [(menu:auto _ _)
     empty]
    [(menu:top options)
     (define top-dy (- crt-height char-height))
     (define-values (_offset l)
       (for/fold ([offset 0]
                  [l empty])
           ([o (in-list options)]
            [i (in-naturals)])
         (match-define (menu:option text fun) o)
         (define pre-offset
           (+ offset top-padding (string-length cursor)))
         (define obj
           (transform
            #:dy top-dy
            #:dx (* char-width pre-offset)
            (string->sprites text)))
         (define post-offset
           (+ pre-offset (string-length text)))

         (define selected
           (cond
             [(equal? (hash-ref st 'top #f) i)
              (list* (transform
                      #:dy top-dy
                      #:dx (* char-width (+ offset top-padding))
                      (string->sprites cursor))
                     (draw-menu st (fun)))]
             [else
              empty]))

         (values post-offset (list* selected obj l))))
     l]
    [(menu:list options)
     (define (option-entry-height pos)
       (+ (* char-height top-padding)
          (* char-height (- (length options) pos))))
     (for/list ([o (in-list options)]
                [i (in-naturals)])
       (match-define (menu:option text fun) o)
       (cons (transform
              #:dy (option-entry-height i)
              #:dx (* top-padding char-width)
              #:dx (* (string-length cursor) char-width)
              (string->sprites text))
             (cond
               [(equal? (hash-ref st 'list #f) i)
                (cons (transform
                       #:dx (* top-padding char-width)
                       #:dy (option-entry-height i)
                       (string->sprites cursor))
                      (draw-menu st (fun)))]
               [else
                empty])))]
    [(menu:status text)
     (transform
      #:dy char-height
      #:dx (- crt-width (* char-width (+ top-padding (string-length text))))
      (string->sprites text))]
    [(menu:info texts)
     (for/list ([text (in-list texts)]
                [i (in-naturals)])
       (transform
        #:dy (- crt-height (* char-height (+ 3 i)))
        #:dx (* char-width top-padding)
        (string->sprites text)))]))

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
    [(menu:top options)
     (define pos (hash-ref st 'top 0))
     (define mod
       (cond
         [(controller-l-down c) -1]
         [(controller-r-down c) +1]
         [else                 0]))
     (define pos+
       (modulo (+ pos mod)
               (length options)))

     (react-menu c (hash-set st 'top pos+)
                 ((menu:option-fun (list-ref options pos+))))]

    [(menu:list options)
     (define pos (hash-ref st 'list 0))
     (define mod
       (cond
         [(controller-up-down c)   -1]
         [(controller-down-down c) +1]
         [else                 0]))
     (define pos+
       (modulo (+ pos mod)
               (length options)))

     (react-menu c (hash-set st 'list pos+)
                 ((menu:option-fun (list-ref options pos+))))]
    [(menu:status text)
     st]
    [(menu:info text)
     st]))

(define (render-menu #:back [back-f #f] m)
  (let loop ([st (hasheq)])
    (define c (os/read* 'controller))

    (os/write
     (list
      (cons 'graphics
            (cons 0 (draw-menu st m)))))

    (define new-st
      (react-menu c st m))

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
         render-menu)
