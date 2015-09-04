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
         lux/chaos/gui
         lux/chaos/gui/key
         mode-lambda
         mode-lambda/backend/gl
         gb/input/keyboard
         gb/data/psn
         gb/audio/3s
         racket/flonum
         racket/fixnum
         gb/graphics/main
         gb/input/controller)

(define the-layer-config
  (make-vector LAYERS
               (layer (fl/ (fx->fl crt-width) 2.0)
                      (fl/ (fx->fl crt-height) 2.0))))

(struct gbchaos (gui sndctx [sndst #:mutable] draw)
        #:methods gen:chaos
        [(define/generic super-start! chaos-start!)
         (define/generic super-yield chaos-yield)
         (define/generic super-event chaos-event)
         (define/generic super-output! chaos-output!)
         (define/generic super-label! chaos-label!)
         (define/generic super-swap! chaos-swap!)
         (define/generic super-stop! chaos-stop!)
         (define (chaos-start! c)
           (super-start! (gbchaos-gui c)))
         (define (chaos-yield c e)
           (super-yield (gbchaos-gui c) e))
         (define (chaos-event c)
           (super-event (gbchaos-gui c)))
         (define (chaos-output! c o)
           (match-define (vector scale lp w cmds last-sprites) o)
           ;; XXX This is implies that we could switch sounds while
           ;; rendering the next sound... which is bad.
           (define stp (render-sound (gbchaos-sndst c) scale lp w cmds))
           (set-gbchaos-sndst! c stp)
           (super-output!
            (gbchaos-gui c)
            ((gbchaos-draw c) the-layer-config #f last-sprites)))
         (define (chaos-label! c l)
           (super-label! (gbchaos-gui c) l))
         (define (chaos-swap! c t)
           (super-swap!
            (gbchaos-gui c)
            (λ ()
              (define old-sndst (gbchaos-sndst c))
              (when old-sndst
                (sound-pause! old-sndst))
              (define new-sndst
                (initial-system-state
                 (gbchaos-sndctx c)))
              (set-gbchaos-sndst! c new-sndst)
              (begin0 (t)
                (sound-destroy! new-sndst)
                (set-gbchaos-sndst! c old-sndst)
                (when old-sndst
                  (sound-unpause! old-sndst))))))
         (define (chaos-stop! c)
           (super-stop! (gbchaos-gui c))
           (sound-context-destroy! (gbchaos-sndctx c)))])
(define (make-gbchaos)
  (gbchaos (make-gui #:mode gui-mode
                     #:start-fullscreen? #t)
           (make-sound-context)
           #f
           (stage-draw/dc gb-csd crt-width crt-height)))

(define current-frame (make-parameter 0))
(define RATE 1/60)
(struct gbword (tick sound-scale world->listener done? return
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
           (vector (gbword-sound-scale gbw)
                   ((gbword-world->listener gbw) wp)
                   wp
                   ss-v
                   (gbword-last-sprites gbw)))
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

(define (call-with-gb t)
  (call-with-chaos (make-gbchaos) t))

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
   (gbword tick sound-scale world->listener done? return
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
