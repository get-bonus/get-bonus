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
         gb/input/keyboard
         gb/data/psn
         gb/audio/3s
         gb/graphics/crt
         gb/graphics/ngl-main
         gb/lib/performance-log
         gb/input/controller)

(define-runtime-path texture-atlas-path "../../r.bin.gz")
(define-runtime-path texture-index-path "../../r.idx.bin.gz")
(define-runtime-path palette-atlas-path "../../pal.png")
(define-runtime-path performance-log-path "../../log")
(struct gbchaos (gui sndctx
                     [sndst #:mutable]
                     draw-on-crt-b draw-sprites-b)
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
           (define draw-on-crt-b (gbchaos-draw-on-crt-b c))
           (define draw-sprites-b (gbchaos-draw-sprites-b c))
           (match-define (vector scale lp w cmds last-sprites) o)
           ;; XXX This is implies that we could switch sounds while
           ;; rendering the next sound... which is bad.
           (define stp (render-sound (gbchaos-sndst c) scale lp w cmds))
           (set-gbchaos-sndst! c stp)
           (super-output!
            (gbchaos-gui c)
            (λ (w h dc)
              (define glctx (send dc get-gl-context))
              (unless glctx
                (error 'on-paint "Could not initialize OpenGL!")
                ;; XXX should bring down the whole thing
                (exit 1))
              (send glctx call-as-current
                    (λ ()
                      (performance-log! 'before-memory (current-memory-use))
                      ;; xxx just make it take w x h
                      (unless (and (unbox draw-on-crt-b)
                                   (= w (vector-ref (unbox draw-on-crt-b) 1))
                                   (= h (vector-ref (unbox draw-on-crt-b) 2)))
                        (define doc (make-draw-on-crt w h))
                        (set-box!
                         draw-on-crt-b
                         (vector doc w h)))
                      (unless (unbox draw-sprites-b)
                        (set-box!
                         draw-sprites-b
                         (make-draw
                          texture-atlas-path
                          texture-index-path
                          palette-atlas-path
                          crt-width
                          crt-height)))
                      (when last-sprites
                        ((vector-ref (unbox draw-on-crt-b) 0)
                         (λ () ((unbox draw-sprites-b)
                                last-sprites))))
                      (send glctx swap-buffers)
                      (performance-log! 'after-memory (current-memory-use))
                      (performance-log-done!))))))
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
  (performance-log-init! performance-log-path)
  (gbchaos (make-gui #:mode 'gl-core
                     #:start-fullscreen? #t)
           (make-sound-context)
           #f
           (box #f)
           (box #f)))

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
           (performance-log! ft)
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
           (vector (gbword-sound-scale gbw)
                   ((gbword-world->listener gbw) wp)
                   wp
                   (gbword-last-sound gbw)
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
                            [last-sound ss])))
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
           #f #f)))

(provide/contract
 [RATE number?]
 [current-frame (-> exact-nonnegative-integer?)]
 [call-with-gb
  (-> (-> any)
      any)]
 [big-bang
  (->* (any/c
        #:tick (-> any/c (listof controller?)
                   (values any/c sprite-tree/c sound-scape/c)))
       (#:sound-scale real?
                      #:listener (-> any/c psn?)
                      #:done? (-> any/c boolean?)
                      #:return (-> any/c any/c))
       any/c)])
