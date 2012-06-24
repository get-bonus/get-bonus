#lang racket/base
(require racket/contract
         racket/math
         racket/match
         (for-syntax racket/base
                     racket/syntax
                     syntax/parse)
         "psn.rkt"
         "joystick.rkt")

(struct controller
        (dpad
         a b c
         x y z
         back home start
         l r)
        #:mutable
        #:transparent)

(struct controller-monitor (js-mon buf))

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
   [back (button 2)]
   [start (button 3)]
   [a (button 0)]
   [b (button 1)]
   ;; XXX try with SNES controller to find x,y,l,r
   [c mt]
   [x mt]
   [y mt]
   [z mt]
   [home mt]   
   [l mt]
   [r mt]))

(define (make-controller-monitor)
  (define js-mon (make-joystick-monitor))
  (controller-monitor
   js-mon
   (for/list ([i (in-range (length (joystick-monitor-state js-mon)))])
     (controller (psn 0 0)
                 #f #f #f
                 #f #f #f
                 #f #f #f
                 #f #f))))

(define (controller-monitor-state cm)
  (match-define (controller-monitor js-mon st) cm)
  (for ([j (in-list (joystick-monitor-state js-mon))]
        [c (in-list st)])
    (mapping j c))
  st)

(provide/contract
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
 [make-controller-monitor
  (-> controller-monitor?)]
 [controller-monitor-state
  (-> controller-monitor? (listof controller?))])

(module+ main
  (define m (make-controller-monitor))
  (for ([i (in-range (* 60 15))])
    (printf "~a ~v\n"
            (real->decimal-string (/ (/ (current-memory-use) 1024) 1024))
            (controller-monitor-state m))
    (sleep 1/60)))
