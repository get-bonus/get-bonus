#lang racket/base
(require racket/bool
         racket/contract
         racket/match
         (for-syntax racket/base)
         sgl
         sgl/gl
         sgl/gl-vectors)

; XXX Maybe implement clipping inside of this code, rather than relying on OpenGL
; XXX Do something to compile commands into lists with glNewList (maybe?)
; XXX Something funny might happen with (x,y) really being corners of pixels (not centers)
; XXX Register a finalizer that will run glDeleteTextures
; XXX The matrix stack can only be 32 deep

;; Basic data structures
(struct cmd (t) #:property prop:procedure (struct-field-index t))

(define (run t) (t))

(define-syntax-rule (λg e ...)
  (cmd (λ () e ...)))
(define-syntax-rule (c pre in post)
  (λg pre (for-each run in) post))

;; Basic shapes
(define (point x y)
  (λg (gl-begin 'points)
      (gl-vertex x y)
      (gl-end)))

(define (line x1 y1 x2 y2)
  (λg (gl-begin 'lines)
      (gl-vertex x1 y1)
      (gl-vertex x2 y2)
      (gl-end)))

(define (rectangle w h [mode 'solid])
  (λg (gl-begin (case mode
                  [(solid) 'quads]
                  [(outline) 'line-loop]))
      (begin
        (gl-vertex 0 0)
        (gl-vertex w 0)
        (gl-vertex w h)
        (gl-vertex 0 h))
      (gl-end)))

(define-syntax-rule (define-compile-time-vector i e)
  (begin
    (define-syntax (the-expander stx)
      (quasisyntax/loc stx
        (vector #,@e)))
    (define i (the-expander))))

(define-for-syntax circle-step 1)
(define-compile-time-vector circle-sins
  (for/list ([angle (in-range 0 360 circle-step)]) (sin angle)))
(define-compile-time-vector circle-coss
  (for/list ([angle (in-range 0 360 circle-step)]) (cos angle)))

(define (circle [mode 'solid])
  (λg (gl-begin (case mode
                  [(solid) 'triangle-fan]
                  [(outline) 'line-strip]))
      (begin
        (when (symbol=? mode 'solid)
          (gl-vertex 0 0))
        (for ([s (in-vector circle-sins)]
              [c (in-vector circle-coss)])
          (gl-vertex s c)))
      (gl-end)))

;; Transformations
(define-syntax-rule
  (define-stateful define-state pre post)
  (define-syntax-rule 
    (define-state ((id pre-a (... ...)) a (... ...))
      setup (... ...))
    (define (id a (... ...) . ics)
      (c (begin (pre pre-a (... ...))
                setup (... ...))
         ics
         (post)))))

(define-stateful define-matrix 
  gl-push-matrix gl-pop-matrix)

(define-matrix ((translate) x y)
  (gl-translate x y 0))
(define-matrix ((scale) x y)
  (gl-scale x y 0))
(define-matrix ((rotate) angle)
  (gl-rotate angle 0 0 1))

(define (mirror w . ics)
  (translate w 0.0
    (scale -1.0 1.0 
           (apply seqn ics))))

(define-stateful define-attrib
  gl-push-attrib gl-pop-attrib)

(define-attrib ((color 'current-bit) r g b a)
  (gl-color r g b a))
(define-attrib ((background 'color-buffer-bit) r g b a)
  (gl-clear-color r g b a)
  (gl-clear 'color-buffer-bit))

;; Combiners
(define blank
  (λg (void)))

(define (seqn . cs)
  (λg (for-each run cs)))

;; Textures
(require racket/draw
         racket/class)

(define (argb->rgba argb)
  (define length (bytes-length argb))
  (define rgba (make-gl-ubyte-vector length))
  (for ([i (in-range 0 length 4)])
    (gl-vector-set! rgba (+ i 0) (bytes-ref argb (+ i 1)))
    (gl-vector-set! rgba (+ i 1) (bytes-ref argb (+ i 2)))
    (gl-vector-set! rgba (+ i 2) (bytes-ref argb (+ i 3)))
    (gl-vector-set! rgba (+ i 3) (bytes-ref argb (+ i 0))))
  rgba)

(define (bytes->text-ref w h bs)
  (define texts (glGenTextures 1))
  (define text-ref (gl-vector-ref texts 0))
  
  (glBindTexture GL_TEXTURE_2D text-ref)
  (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_LINEAR)
  (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_LINEAR)
  (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S GL_CLAMP)
  (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T GL_CLAMP)
  (glTexImage2D GL_TEXTURE_2D 0 GL_RGBA w h 0
                GL_RGBA GL_UNSIGNED_BYTE bs)
  
  text-ref)

(define (bm->rgba-bytes bm)
  (define w (send bm get-width))
  (define h (send bm get-height))
  (define argb (make-bytes (* 4 w h) 255))
  (send bm get-argb-pixels 0 0 w h argb #f)
  (argb->rgba argb))

(struct texture (w h bs r))
(define (load-texture! t)
  (match-define (texture w h bs r) t)
  (unless (unbox r)
    (define text-ref (bytes->text-ref w h (unbox bs)))
    (set-box! bs #f)
    (set-box! r text-ref)))

(define (path->texture p)
  (define bm (make-object bitmap% p 'png/alpha #f #t))
  (define w (send bm get-width))
  (define h (send bm get-height))
  (define rgba (box (bm->rgba-bytes bm)))
  (define ref (box #f))
  (texture w h rgba ref))

(define (bind-texture-ref! t)
  (unless (equal? t (unbox (current-texture)))
    (glBindTexture GL_TEXTURE_2D t)
    (set-box! (current-texture) t)))

(define (draw-texture! text-ref w h tx1 ty1 tw th)
  (bind-texture-ref! text-ref)
  (gl-begin 'quads)
  (gl-tex-coord tx1 (+ ty1 th)) (gl-vertex 0 0)
  (gl-tex-coord (+ tx1 tw) (+ ty1 th)) (gl-vertex w 0) 
  (gl-tex-coord (+ tx1 tw) ty1) (gl-vertex w h)
  (gl-tex-coord tx1 ty1) (gl-vertex 0 h)
  (gl-end))
  
(define (texture* t 
                  [w (texture-w t)] [h (texture-h t)]
                  [tx1 0] [ty1 0]
                  [tw 1] [th 1])
  (λg 
   (load-texture! t)
   (glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA)
   (draw-texture! 
    (unbox (texture-r t)) 
    w h
    tx1 ty1
    tw th)
  (glBlendFunc GL_ONE GL_ZERO)))

;; Text
(define (string->bitmap f str)
  (define bdc
    (new bitmap-dc% [bitmap (make-object bitmap% 1 1 #f #t)]))
  (define transparent-c (make-object color% 0 0 0 0))
  (send bdc set-font f)
  (send bdc set-text-mode 'solid)
  
  (define-values (w h bot vert) 
    (send bdc get-text-extent str f #t))
  (define (floor* x)
    (inexact->exact (floor x)))
  (define w* (floor* w))
  (define h* (floor* h))
  
  (define bm (make-object bitmap% w* h* #f #t))
  (send bdc set-bitmap bm)    
  (send bdc clear)
  (send bdc draw-text str 0 0 #t)
  (send bdc flush)
  
  (values bm w* h*))

(define (text str 
              #:size [size 12]
              #:face [face #f]
              #:family [family 'default]
              #:style [style 'normal]
              #:weight [weight 'normal]
              #:underlined? [underline? #f])
  (define f 
    (make-font #:size size
               #:face face
               #:family family
               #:style style
               #:weight weight
               #:underlined? underline?
               #:smoothing 'smoothed))
  (define-values (bm w h)
     (string->bitmap f str))
  (λg
   (define text-ref
     (bytes->text-ref 
      w h
      (bm->rgba-bytes bm)))
   (glBlendFunc GL_DST_COLOR GL_ONE_MINUS_SRC_ALPHA)
   (draw-texture! text-ref (/ w h) 1 0 0 1 1)
   (glDeleteTextures (gl-uint-vector text-ref))
   (glBlendFunc GL_ONE GL_ZERO)
   (set-box! (current-texture) #f)))

;; Top-level
(define (gl-viewport/restrict mw mh
                              vw vh 
                              cx cy)
  (define x1 (- cx (/ vw 2)))
  (define x2 (+ cx (/ vw 2)))
  (define y1 (- cy (/ vh 2)))
  (define y2 (+ cy (/ vh 2)))
  
  ; Don't go off the screen
  (define x1p (max 0.0 x1))
  (define x2p (min mw x2)) 
  (define y1p (max 0.0 y1))
  (define y2p (min mh y2))
  
  (gluOrtho2D 
   ; If x2 has gone off, then add more to the left
   (if (= x2 x2p)
       x1p
       (+ x1p (- x2p x2)))
   ; etc
   (if (= x1 x1p)
       x2p
       (+ x2p (- x1p x1)))
   (if (= y2 y2p)
       y1p
       (+ y1p (- y2p y2)))
   (if (= y1 y1p)
       y2p
       (+ y2p (- y1p y1)))))

(struct focused (go))
(define (focus mw mh
               vw vh 
               cx cy
               cmd)
  (focused
   (λ () 
     (draw* mw mh
            vw vh 
            cx cy
            cmd))))
(define (draw f)
  ((focused-go f)))

(define current-texture (make-parameter #f))
(define (draw* mw mh
               vw vh 
               cx cy
               cmd)
  (gl-matrix-mode 'projection)
  (gl-load-identity)
  (gl-enable 'texture-2d)
  (gl-disable 'depth-test)
  (gl-disable 'lighting)
  (gl-disable 'dither)
  (gl-enable 'blend)
  (gl-viewport/restrict mw mh
                        vw vh 
                        cx cy)
  (gl-matrix-mode 'modelview)
  (gl-load-identity)
  (glTexEnvf GL_TEXTURE_ENV GL_TEXTURE_ENV_MODE GL_MODULATE)
  (parameterize ([current-texture (box #f)])
    (run cmd))
  (gl-flush))

;; Syntax
(define-syntax-rule (for/gl (clause ...) body ...)
  (apply seqn
         (for/list (clause ...) body ...)))
(define-syntax-rule (for*/gl (clause ...) body ...)
  (apply seqn
         (for*/list (clause ...) body ...)))

;; Contracts + provides
(define mode/c
  (symbols 'solid 'outline))

(define unit-integer?
  (between/c 0 1))

(provide
 for/gl
 for*/gl)
(provide/contract
 [focused? contract?]
 [focus (real? real? real? real? real? real? cmd? . -> . focused?)]
 [draw (focused? . -> . void?)]
 [cmd? contract?]
 [seqn (() () #:rest (listof cmd?) . ->* . cmd?)]
 [rotate ((real?) () #:rest (listof cmd?) . ->* . cmd?)]
 [mirror ((real?) () #:rest (listof cmd?) . ->* . cmd?)]
 [scale ((real? real?) () #:rest (listof cmd?) . ->* . cmd?)]
 [translate ((real? real?) () #:rest (listof cmd?) . ->* . cmd?)]
 [color ((real? real? real? real?) () #:rest (listof cmd?) . ->* . cmd?)]
 [background ((real? real? real? real?) () #:rest (listof cmd?) . ->* . cmd?)]
 [mode/c contract?]
 [blank cmd?]
 [circle (() (mode/c) . ->* . cmd?)]
 [rectangle ((real? real?) (mode/c) . ->* . cmd?)]
 [line (real? real? real? real? . -> . cmd?)] 
 [point (real? real? . -> . cmd?)]
 [path->texture (path? . -> . texture?)]
 [texture? contract?]
 [texture-w (texture? . -> . real?)]
 [texture-h (texture? . -> . real?)]
 [rename texture* texture 
         ((texture?) 
          (real? real? unit-integer? unit-integer? unit-integer? unit-integer?)
          . ->* . cmd?)]
 [text 
  (->* (string?)
       (#:size (integer-in 1 255)
               #:face (or/c string? #f)
               #:family (one-of/c 'default 'decorative 'roman 'script
                                  'swiss 'modern 'symbol 'system)
               #:style (one-of/c 'normal 'italic 'slant) 
               #:weight (one-of/c 'normal 'bold 'light)
               #:underlined? boolean?)
       cmd?)])