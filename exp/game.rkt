#lang racket/gui
(require racket/runtime-path
         "loop.rkt"
         (prefix-in gl: "gl.rkt")
         "sprites.rkt"
         "mvector.rkt"
         "fullscreen.rkt"
         "keyboard.rkt"
         "mapping.rkt"
         "controller.rkt"
         "joystick.rkt"
         "3s.rkt"
         "psn.rkt")

(define-runtime-path resource-path "../resources")

(define bgm 
  (path->audio 
   (build-path resource-path 
               "SMB-1-1.mp3")))
(define jump-se
  (path->audio
   (build-path resource-path 
               "SMB-SE-Jump.wav")))

(define map-text 
  (gl:path->texture (build-path resource-path "IMB" "mapsheet.png")))
(define map-sprites
  (sprite-sheet/row-major map-text 16))
(define width 320)
(define height 40)
(define map-bytes
  (file->bytes (build-path resource-path "IMB" "out.lvl")))

(define the-background
  (gl:for*/gl 
   ([r (in-range height)]
    [c (in-range width)])
   (define b
     (bytes-ref map-bytes
                (+ (* height c) r)))
   (if (zero? b)
       gl:blank
       (gl:translate c (- height r 1)
                     (map-sprites b)))))

(struct world (frame p))

; XXX Use cd-broad
(define start-time (current-seconds))
(big-bang
 (world 0 (make-rectangular 8 4.5))
 #:tick
 (λ (w cs)
   (match-define (world frame p*) w)
   (define p
     (for/fold ([p p*])
       ([s (in-list cs)])
       (+ p (/ (controller-dpad s) 5))))
   
   (define PX (real-part p))
   (define PY (imag-part p))
   
   (when (controller-start (first cs))
     (big-bang 0
               #:tick (λ (i cs)
                        (values (add1 i)
                                (gl:focus 1 1 1 1 0 0
                                          (gl:background 255 0 255 0))
                                empty))
               #:listener (λ (i) (psn 0. 0.))
               #:done? (λ (i) (i . > . 360))))
   
   (values (world (add1 frame) p)
           (gl:seqn
            (gl:focus 
             ;width height (* 16 20) (* 9 20) ; Show whole map
             width height (* 16 4) (* 9 4)
             PX PY
             (gl:background
              255 255 255 0
              the-background
              (gl:translate PX PY
                            (map-sprites 5))))
            (gl:focus 16 9 16 9 0 0
                      (gl:texture
                       (gl:string->texture
                        (format "~a FPS" 
                                (with-handlers ([exn? (λ (x) "n/a")])
                                  (real->decimal-string
                                   (/ frame
                                      (- (current-seconds) start-time)))))))))
           (if (zero? frame)
               (list (background (λ (w) bgm) #:gain 0.8)
                     (sound-on jump-se
                               #:looping? #t
                               (λ (w) (+ (psn -5.0 0.0)
                                         (modulo (floor (/ (world-frame w) 60)) 11)))))
               empty)))
 #:listener
 (λ (w)
   (world-p w))
 #:done?
 (λ (w)
   #f
   #;((world-frame w) . > . 60)))
