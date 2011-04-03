#lang racket/base
(require racket/bool
         racket/contract
         racket/match
         (for-syntax racket/base)
         sgl
         sgl/gl
         sgl/gl-vectors)

; XXX Do something to compile commands into lists with glNewList (maybe?)

;; Basic data structures
(struct cmd ())
(struct atomic cmd (thnk))
(struct wrap cmd (pre in post))

(define run
  (match-lambda
    [(atomic t)
     (t)]
    [(wrap pre in post)
     (pre)
     (for-each run in)
     (post)]))

(define-syntax-rule (c pre in post)
  (wrap (λ () pre) in (λ () post)))
(define-syntax-rule (λg e ...)
  (atomic (λ () e ...)))

;; Basic shapes
; XXX Something funny might happen with (x,y) really being corners of pixels (not centers)
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
; XXX The matrix stack can only be 32 deep
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
(define (seqn . cs)
  (wrap void cs void))

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

(define (draw mw mh
              vw vh 
              cx cy
              cmd)
  (gl-matrix-mode 'projection)
  (gl-load-identity)
  (gl-enable 'texture-2d)
  (gl-disable 'depth-test)
  (gl-disable 'lighting)
  (gl-disable 'dither)
  (gl-disable 'blend)
  (gl-viewport/restrict mw mh
                        vw vh 
                        cx cy)
  (gl-matrix-mode 'modelview)
  (gl-load-identity)
  (glTexEnvf GL_TEXTURE_ENV GL_TEXTURE_ENV_MODE GL_MODULATE)
  (run cmd)
  (gl-flush))

;; Textures
; XXX

;; Contracts + provides
(define mode/c
  (symbols 'solid 'outline))

(provide/contract
 [draw (real? real? real? real? real? real? cmd? . -> . void?)]
 [cmd? contract?]
 [seqn (() () #:rest (listof cmd?) . ->* . cmd?)]
 [rotate ((real?) () #:rest (listof cmd?) . ->* . cmd?)]
 [mirror ((real?) () #:rest (listof cmd?) . ->* . cmd?)]
 [scale ((real? real?) () #:rest (listof cmd?) . ->* . cmd?)]
 [translate ((real? real?) () #:rest (listof cmd?) . ->* . cmd?)]
 [color ((real? real? real? real?) () #:rest (listof cmd?) . ->* . cmd?)]
 [background ((real? real? real? real?) () #:rest (listof cmd?) . ->* . cmd?)]
 [mode/c contract?]
 [circle (() (mode/c) . ->* . cmd?)]
 [rectangle ((real? real?) (mode/c) . ->* . cmd?)]
 [line (real? real? real? real? . -> . cmd?)] 
 [point (real? real? . -> . cmd?)])

