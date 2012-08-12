#lang typed/racket/base
(require racket/match)

;; A "modern" OpenGL 2D graphics engine.

;; Features
;;  - Implicit texture atlas creation
;;  - Only allows rectangular sprites
;;  - Multi-pass shading

(define-type Fill-Function
  (;; Translation
   (Vectorof (Vector Float Float Float))
   ;; Rotation
   (Vectorof Float)
   ;; Scaling
   (Vectorof (Vector Float Float))
   ;; Color
   (Vectorof (Vector Float Float Float Float))
   ;; Texture
   (Vectorof (Vector Float Float))
   ;; Rectangle
   (Vectorof (Vector (Vector Float Float)
                     (Vector Float Float)
                     (Vector Float Float)
                     (Vector Float Float)))
   Exact-Nonnegative-Integer
   ->
   Void))

;; A command is a function that fills a vector of objects
(struct: cmd ([min-x : Float]
              [max-x : Float]
              [min-y : Float]
              [max-y : Float]
              [min-z : Float]
              [max-z : Float]
              [count : Exact-Nonnegative-Integer]
              [fill! : Fill-Function]))

(: rectangle (Float Float -> cmd))
(define (rectangle width height)
  (: new-fill! Fill-Function)
  (define (new-fill!
           translate-matrix-vec
           rotate-matrix-vec
           scale-matrix-vec
           color-matrix-vec
           texture-matrix-vec
           vertex-matrix-vec
           offset)
    (vector-set! vertex-matrix-vec offset 
                 (vector (vector 0.0 0.0)
                         (vector 0.0 height)
                         (vector width height)
                         (vector width height))))
  (cmd 0.0 width
       0.0 height
       0.0 0.0
       1
       new-fill!))

(: rotate (Float cmd -> cmd))
(define (rotate angle inner)
  (match-define (cmd min-x max-x min-y max-y min-z max-z count fill!) inner)
  (: new-fill! Fill-Function)
  (define (new-fill!
           translate-matrix-vec
           rotate-matrix-vec
           scale-matrix-vec
           color-matrix-vec
           texture-matrix-vec
           vertex-matrix-vec
           offset)
    ;; XXX Would it be better to set this at the leaves?
         (for: ([i (in-range count)])
               (vector-set! rotate-matrix-vec (+ i offset)
                            (+ angle (vector-ref rotate-matrix-vec (+ i offset)))))
         (fill! 
          translate-matrix-vec
          rotate-matrix-vec
          scale-matrix-vec
          color-matrix-vec
          texture-matrix-vec
          vertex-matrix-vec
          offset))
  ;; XXX Is this correct?
  (cmd (* min-x (sin angle))
       (* max-x (sin angle))
       (* min-y (cos angle))
       (* max-y (cos angle))
       min-z max-x
       count
       new-fill!))

(provide
 (struct-out cmd)
 rotate
 rectangle)

