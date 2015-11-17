#lang racket/base
(require racket/runtime-path
         racket/contract
         racket/list
         racket/format
         racket/match
         racket/class
         racket/generic
         lux
         lux/chaos
         lux/chaos/pair
         lux/chaos/gui
         lux/chaos/gui/key
         mode-lambda
         mode-lambda/backend/gl
         gb/input/keyboard
         gb/data/psn
         3s
         lux/chaos/3s
         racket/flonum
         racket/fixnum
         gb/graphics/main
         gb/input/controller)

(define the-layer-config
  (make-vector LAYERS
               (layer (fl/ (fx->fl crt-width) 2.0)
                      (fl/ (fx->fl crt-height) 2.0))))

(define current-frame (make-parameter 0))
(define RATE 1/60)
(struct gbword (tick drawb
                     sound-scale world->listener done? return
                     km cm
                     frame world
                     last-sprites last-sound)
  #:methods gen:word
  [(define (word-fps gbw)
     60.0)
   (define (word-label gbw ft)
     (lux-standard-label "Get Bonus!" ft))
   (define (word-event gbw e)
     (cond
       [(eq? e 'close)
        #f]
       [(key-event? e)
        (keyboard-monitor-submit! (gbword-km gbw) e)
        gbw]
       [else
        gbw]))
   (define (word-output gbw)
     (define wp (gbword-world gbw))
     (define ss-b (gbword-last-sound gbw))
     (define ss-v (unbox ss-b))
     ;; xxx this is a hack to make sure a sound command is only
     ;; sent once. it may be bad because the sound is still
     ;; messed up. i used to get two main menu bgm tracks, now
     ;; the maze music doesn't turn off
     (set-box! ss-b '())
     (define the-drawb (gbword-drawb gbw))
     (define the-draw (unbox the-drawb))
     (unless the-draw
       (set! the-draw (current-gb-draw))
       (set-box! the-drawb the-draw))
     (cons
      (the-draw
       the-layer-config #f
       (gbword-last-sprites gbw))
      (vector (gbword-sound-scale gbw)
              ((gbword-world->listener gbw) wp)
              wp
              ss-v)))
   (define (word-tick gbw)
     (define frame (gbword-frame gbw))
     (define-values (wp cmd ss)
       (parameterize ([current-frame frame])
         ((gbword-tick gbw)
          (gbword-world gbw)
          (controller-monitor-state
           (gbword-cm gbw)))))
     (if ((gbword-done? gbw) wp)
         #f
         (struct-copy gbword gbw
                      [frame (add1 frame)]
                      [world wp]
                      [last-sprites cmd]
                      [last-sound (box ss)])))
   (define (word-return gbw)
     ((gbword-return gbw)
      (gbword-world gbw)))])

(define current-gb-draw (make-parameter #f))
(define (call-with-gb t)
  (define the-chaos
    (make-pair (make-gui #:mode gui-mode
                         #:start-fullscreen? #t)
               (make-3s)))
  (define the-gb-draw
    (stage-draw/dc gb-csd crt-width crt-height))
  (call-with-chaos the-chaos
                   (λ ()
                     (parameterize ([current-gb-draw the-gb-draw])
                       (t)))))

(define (big-bang initial-world
                  #:tick tick
                  #:sound-scale [sound-scale 1.0]
                  #:listener [world->listener (λ (w) (psn 0. 0.))]
                  #:done? [done? (λ (w) #f)]
                  #:return [return (λ (w) w)])
  ;; xxx try to move these into gbchaos
  (define km
    (keyboard-monitor))
  (define cm
    (make-controller-monitor
     #:keyboard km))
  (fiat-lux
   (gbword tick (box #f)
           sound-scale world->listener done? return
           km cm
           0 initial-world
           #f (box '()))))

(provide/contract
 [RATE number?]
 [current-frame (-> exact-nonnegative-integer?)]
 [call-with-gb
  (-> (-> any)
      any)]
 [big-bang
  (->* (any/c
        #:tick (-> any/c (listof controller?)
                   (values any/c any/c sound-scape/c)))
       (#:sound-scale real?
        #:listener (-> any/c psn?)
        #:done? (-> any/c boolean?)
        #:return (-> any/c any/c))
       any/c)])
