#lang racket/base
(require racket/contract
         racket/math
         racket/match
         racket/set
         racket/list
         (for-syntax racket/base
                     racket/syntax
                     syntax/parse)
         gb/data/psn
         gb/input/joystick
         gb/input/keyboard)

;; XXX add something to communicate capabilties (like no third button,
;; home, etc)

(define-syntax (structy stx)
  (syntax-parse stx
    [(_ id (field ...) . more)
     (with-syntax*
      ([id? (format-id #'id "~a?" #'id)]
       [(field-down ...)
        (for/list ([f (in-list (syntax->list #'(field ...)))])
          (format-id f "~a-down" f))]
       [(field-up ...)
        (for/list ([f (in-list (syntax->list #'(field ...)))])
          (format-id f "~a-up" f))]
       [(id-field-all ...)
        (for/list ([f (in-list (syntax->list #'(field ... field-down ... field-up ...)))])
          (format-id f "~a-~a" #'id f))]
       [(set-id-field*! ...)
        (for/list ([f (in-list (syntax->list #'(field ...)))])
          (format-id f "set-~a-~a*!" #'id f))]
       [((id-field set-id-field! set-id-field-down! set-id-field-up!) ...)
        (for/list ([f (in-list (syntax->list #'(field ...)))])
          (list (format-id f "~a-~a" #'id f)
                (format-id f "set-~a-~a!" #'id f)
                (format-id f "set-~a-~a-down!" #'id f)
                (format-id f "set-~a-~a-up!" #'id f)))])
      (syntax/loc stx
        (begin (struct id (field ... field-down ... field-up ...) . more)
               (define (set-id-field*! c nv)
                 (define old (id-field c))
                 (set-id-field! c nv)
                 (cond
                   [(and old             nv)
                    (set-id-field-down! c #f)
                    (set-id-field-up! c #f)]
                   [(and (not old)       nv)
                    (set-id-field-down! c #t)
                    (set-id-field-up! c #f)]
                   [(and (not old) (not nv))
                    (set-id-field-down! c #f)
                    (set-id-field-up! c #f)]
                   [(and       old (not nv))
                    (set-id-field-down! c #f)
                    (set-id-field-up! c #t)]))
               ...
               (provide id? id id-field-all ...))))]))

(structy controller
         (left right up down b a y x select start l r)
         #:mutable
         #:transparent)

(struct controller-monitor (js-mon key-mon how-many-joysticks buf))

(define-syntax (define-mapping stx)
  (syntax-parse stx
    [(_ id (name [c-field j-field] ...) ...)
     (with-syntax
         ([((set-controller-field! ...) ...)
           (for/list ([fs (in-list (syntax->list #'((c-field ...) ...)))])
             (for/list ([f (in-list (syntax->list fs))])
               (format-id f "set-controller-~a*!" f)))]
          [((joystick-state-field ...) ...)
           (map generate-temporaries (syntax->list #'((j-field ...) ...)))])
       (syntax/loc stx
         (begin
           (define joystick-state-field j-field)
           ... ...
           (define (id j c)
             (match (joystick-state-name j)
               [name
                (set-controller-field! c (joystick-state-field j))
                ...]
               ...
               [other
                (error 'mapping "Controller ~e not supported (yet)" other)])))))]))

(define ((neg-axis i) js)
  (negative? (vector-ref (joystick-state-axes js) i)))
(define ((pos-axis i) js)
  (positive? (vector-ref (joystick-state-axes js) i)))
(define ((button i) js)
  (= 1 (vector-ref (joystick-state-buttons js) i)))
(define (mt js)
  #f)

(define-syntax (define-controller-dpad stx)
  (syntax-case stx ()
    [(_
      ldpad
      left right down up)
     (with-syntax ([controller-ldpad (format-id #'ldpad "controller-~a" #'ldpad)]
                   [controller-ldpad-x (format-id #'ldpad "controller-~a-x" #'ldpad)]
                   [controller-ldpad-y (format-id #'ldpad "controller-~a-y" #'ldpad)]
                   [controller-left (format-id #'left "controller-~a" #'left)]
                   [controller-right (format-id #'right "controller-~a" #'right)]
                   [controller-down (format-id #'up "controller-~a" #'down)]
                   [controller-up (format-id #'down "controller-~a" #'up)])
       (syntax/loc stx
         (begin
           (define (controller-ldpad-x cs)
             (cond
               [(controller-left cs)  -1.0]
               [(controller-right cs) +1.0]
               [else                   0.0]))
           (define (controller-ldpad-y cs)
             (cond
               [(controller-down cs)  -1.0]
               [(controller-up cs)    +1.0]
               [else                   0.0]))
           (define (controller-ldpad cs)
             (psn (controller-ldpad-x cs)
                  (controller-ldpad-y cs)))
           (provide/contract
            [controller-ldpad
             (-> controller? psn?)]
            [controller-ldpad-x
             (-> controller? inexact?)]
            [controller-ldpad-y
             (-> controller? inexact?)]))))]))

(define-controller-dpad ldpad
  left right down up)
(define-controller-dpad rdpad
  y a b x)

;; XXX customize with files/gui?
(define-mapping mapping
  (#"Generic X-Box pad"
   [left (neg-axis 6)]
   [right (pos-axis 6)]
   [down (pos-axis 7)]
   [up (neg-axis 7)]
   [a (button 0)]
   [b (button 1)]
   [x (button 2)]
   [y (button 3)]
   [select (button 6)]
   [start (button 7)]
   [l (button 4)]
   [r (pos-axis 2)])
  (#"RetroUSB.com RetroPad"
   [left (neg-axis 0)]
   [right (pos-axis 0)]
   [down (pos-axis 1)]
   [up (neg-axis 1)]
   [a (button 0)]
   [b (button 1)]
   [select (button 2)]
   [start (button 3)]
   ;; XXX try with SNES controller to find x,y,l,r
   [x mt]
   [y mt]
   [l mt]
   [r mt])
  (#"RetroUSB.com SNES RetroPort"
   [left (neg-axis 0)]
   [right (pos-axis 0)]
   [down (pos-axis 1)]
   [up (neg-axis 1)]
   [a (button 5)]
   [b (button 1)]
   [select (button 2)]
   [start (button 3)]
   [x (button 4)]
   [y (button 0)]
   [l (button 6)]
   [r (button 7)]))

(define-syntax (define-keyboard-mapping stx)
  (syntax-parse stx
    [(_ id ([c-field j-field] ...))
     (with-syntax
         ([(controller-field ...)
           (for/list ([f (in-list (syntax->list #'(c-field ...)))])
             (format-id f "controller-~a" f))]
          [(set-controller-field! ...)
           (for/list ([f (in-list (syntax->list #'(c-field ...)))])
             (format-id f "set-controller-~a*!" f))]
          [(joystick-state-field ...)
           (generate-temporaries #'(j-field ...))])
       (syntax/loc stx
         (begin
           (define joystick-state-field j-field)
           ...
           (define (id ks c)
             (let ()
               (define f (joystick-state-field ks))
               (set-controller-field! c f))
             ...))))]))

(define ((key k) ks)
  (set-member? ks k))

(define-keyboard-mapping keyboard-mapping
  ([left (key 'left)]
   [right (key 'right)]
   [down (key 'down)]
   [up (key 'up)]
   [b (key #\z)]
   [a (key #\x)]
   [y (key #\a)]
   [x (key #\s)]
   [select (key 'escape)]
   [start (key #\return)]
   [l (key #\q)]
   [r (key #\w)]))

(define (null-controller)
  (controller #f #f #f #f
              #f #f
              #f #f
              #f #f
              #f #f

              #f #f #f #f
              #f #f
              #f #f
              #f #f
              #f #f

              #f #f #f #f
              #f #f
              #f #f
              #f #f
              #f #f))

(define (clamp n)
  (max -1. (min 1. n)))

(define (clamp-psn p)
  (psn (clamp (psn-x p))
       (clamp (psn-y p))))

(define (controller-merge orig new)
  (define-syntax (merge-button stx)
    (syntax-parse stx
      [(_ b:id)
       (with-syntax
           ([set-controller-b! (format-id #'b "set-controller-~a!" #'b)]
            [controller-b (format-id #'b "controller-~a" #'b)])
         (syntax/loc stx
           (set-controller-b! orig
                              (or (controller-b orig)
                                  (controller-b new)))))]))
  (define-syntax-rule (merge-buttons b ...)
    (begin (merge-button b) ...))
  (merge-buttons
   left right down up
   a b
   x y
   select start
   l r))

(define (make-controller-monitor #:keyboard [km #f])
  (define js-mon (make-joystick-monitor))
  (define how-many-joysticks
    (length (joystick-monitor-state js-mon)))
  ;; If a keyboard is present, guarantee one controller
  (define how-many-controllers
    (if km
      (max 1 how-many-joysticks)
      how-many-joysticks))
  (controller-monitor
   js-mon km how-many-joysticks
   (for/list ([i (in-range how-many-controllers)])
     (null-controller))))

(define (controller-monitor-state cm)
  (match-define (controller-monitor js-mon km how-many-joysticks st) cm)
  (for ([j (in-list (joystick-monitor-state js-mon))]
        [c (in-list st)])
    (mapping j c))
  ;; If a keyboard is present, merge it with the first controller
  ;; input.
  (when km
    (cond
      [(zero? how-many-joysticks)
       (keyboard-mapping (keyboard-monitor-state km)
                         (first st))]
      [else
       (define c (null-controller))
       (keyboard-mapping (keyboard-monitor-state km)
                         c)
       (controller-merge (first st) c)]))
  st)

(define (controller-any-button? c)
  (or (controller-start c)
      (controller-a c)
      (controller-b c)
      (controller-x c)
      (controller-y c)))

(provide/contract
 [controller-any-button?
  (-> controller? boolean?)]
 [controller-monitor?
  (-> any/c boolean?)]
 ;; XXX remove make
 [make-controller-monitor
  (->* () (#:keyboard keyboard-monitor?) controller-monitor?)]
 [controller-monitor-state
  (-> controller-monitor? (listof controller?))])

(module+ main
  (define m (make-controller-monitor))
  (for ([i (in-range (* 60 15))])
    (printf "~a ~v\n"
            (real->decimal-string (/ (/ (current-memory-use) 1024) 1024))
            (controller-monitor-state m))
    (sleep 1/60)))
