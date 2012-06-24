#lang racket/base
(require racket/contract
         racket/math
         racket/match
         racket/set
         racket/list
         (for-syntax racket/base
                     racket/syntax
                     syntax/parse)
         "psn.rkt"
         "joystick.rkt"
         "keyboard.rkt")

;; XXX add something to communicate capabilties (like no third button,
;; home, etc)
(struct controller
        (dpad
         a b c
         x y z
         back home start
         l r)
        #:mutable
        #:transparent)

(struct controller-monitor (js-mon key-mon buf))

(define-syntax (define-mapping stx)
  (syntax-parse stx
    [(_ id (name [c-field j-field] ...) ...)
     (with-syntax
         ([((set-controller-field! ...) ...)
           (for/list ([fs (in-list (syntax->list #'((c-field ...) ...)))])
             (for/list ([f (in-list (syntax->list fs))])
               (format-id f "set-controller-~a!" f)))]
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

(define ((axis i) js)
  (vector-ref (joystick-state-axes js) i))
(define (reverse-axis i)
  (define a (axis i))
  (λ (js)
    (* -1 (a js))))
(define ((button i) js)
  (= 1 (vector-ref (joystick-state-buttons js) i)))
(define (button-axis i)
  (define a (axis i))
  (λ (js)
    (= 1 (a js))))
(define (mt js)
  #f)

(define (controller-dpad-x cs)
  (psn-x (controller-dpad cs)))
(define (controller-dpad-y cs)
  (psn-y (controller-dpad cs)))
(define (set-controller-dpad-x! cs x)
  (set-controller-dpad! cs (psn x (controller-dpad-y cs))))
(define (set-controller-dpad-y! cs y)
  (set-controller-dpad! cs (psn (controller-dpad-x cs) y)))

;; XXX customize with files/gui?
(define-mapping mapping
  (#"Generic X-Box pad"
   [dpad-x (axis 6)]
   [dpad-y (reverse-axis 7)]
   [a (button 0)]
   [b (button 1)]
   [c (button-axis 5)]
   [x (button 2)]
   [y (button 3)]
   [z (button 5)]
   [back (button 6)]
   [home (button 8)]
   [start (button 7)]
   [l (button 4)]
   [r (button-axis 2)])
  (#"RetroUSB.com RetroPad"
   [dpad-x (axis 0)]
   [dpad-y (reverse-axis 1)]
   [a (button 0)]
   [b (button 1)]
   [back (button 2)]
   [start (button 3)]
   ;; XXX try with SNES controller to find x,y,l,r
   [c mt]
   [x mt]
   [y mt]
   [z mt]
   [home mt]
   [l mt]
   [r mt]))

(define-syntax (define-keyboard-mapping stx)
  (syntax-parse stx
    [(_ id ([c-field j-field] ...))
     (with-syntax
         ([(set-controller-field! ...)
           (for/list ([f (in-list (syntax->list #'(c-field ...)))])
               (format-id f "set-controller-~a!" f))]
          [(joystick-state-field ...)
           (generate-temporaries #'(j-field ...))])
       (syntax/loc stx
         (begin
           (define joystick-state-field j-field)
           ...
           (define (id ks c)
             (let ()
               (define f (joystick-state-field ks))
               ;; XXX busted idea
               (when f
                 (set-controller-field! c f)))
             ...))))]))

(define ((key-axis neg pos) ks)
  (eprintf "~a\n" (list neg pos ks))
  (cond [(set-member? ks neg) -1.]
        [(set-member? ks pos) +1.]
        [else #f]))
(define ((key k) ks)
  (set-member? ks k))

(define-keyboard-mapping keyboard-mapping
  ([dpad-x (key-axis 'left 'right)]
   [dpad-y (key-axis 'down 'up)]   
   [a (key #\z)]
   [b (key #\x)]
   [c (key #\c)]
   [x (key #\a)]
   [y (key #\s)]
   [z (key #\d)]
   [back (key #\tab)]
   [home (key 'escape)]
   [start (key #\return)]
   [l (key #\q)]
   [r (key #\e)]))

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
   js-mon km
   (for/list ([i (in-range how-many-controllers)])
     (controller (psn 0. 0.)
                 #f #f #f
                 #f #f #f
                 #f #f #f
                 #f #f))))

(define (controller-monitor-state cm)
  (match-define (controller-monitor js-mon km st) cm)
  (for ([j (in-list (joystick-monitor-state js-mon))]
        [c (in-list st)])
    (mapping j c))
  ;; If a keyboard is present, merge it with the first controller
  ;; input.
  (when km
    (keyboard-mapping (keyboard-monitor-state km)
                      (first st)))
  (eprintf "~a\n" st)
  st)

(provide/contract
 ;; XXX don't provide mutators
 [struct controller
         ([dpad psn?]
          [a boolean?] [b boolean?] [c boolean?]
          [x boolean?] [y boolean?] [z boolean?]
          [back boolean?] [home boolean?] [start boolean?]
          [l boolean?] [r boolean?])]
 [controller-dpad-x
  (-> controller? inexact?)]
 [controller-dpad-y
  (-> controller? inexact?)]
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
