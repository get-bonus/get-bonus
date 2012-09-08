#lang racket/base
(require racket/gui
         ffi/vector
         racket/runtime-path
         xml
         gb/gui/fullscreen
         (planet stephanh/RacketGL/rgl))

(define VaoId #f)
(define VboId #f)
(define IndexBufferId #f)
(define ColorBufferId #f)
(define TexIndexesBufferId #f)
(define TransformBufferId #f)

(define ProgramId #f)
(define VertexShaderId #f)
(define FragmentShaderId #f)

(define TextureAtlasIndex_UniformId #f)
(define TextureAtlasId #f)

(module+ main
  (define-values
    (the-frame the-canvas)
    (make-fullscreen-canvas/ratio
     ""
     16 9
     (λ (c)
       (define dc (send c get-dc))
       (define glctx (send dc get-gl-context))       
       (send glctx call-as-current
             (λ ()               
               (do-drawing (send c get-width) 
                           (send c get-height))               
               (send glctx swap-buffers))))
     (λ (k)
       (void))))

  (let loop ()
    (define before
      (current-inexact-milliseconds))
    (define next-time
      (+ before (* 1/60 1000)))
    (send the-canvas refresh-now)
    (define after
      (current-inexact-milliseconds))
    (send the-frame set-label 
          (format "~a FPS"
                  (real->decimal-string
                   (/ 1 (/ (- after before) 1000))
                   2)))
    (sync (alarm-evt next-time))
    (loop)))

(define init? #f)
(define (do-drawing w h)
  (unless init?
    (Initialize)
    (set! init? #t))

  (ResizeFunction w h)
  (RenderFunction))

(define-runtime-path texture-atlas-path "../resources/SMB-Tiles.png")

(define (Initialize)
  (printf "INFO: OpenGL Version ~a\n"
          (glGetString GL_VERSION))  

  (set! TextureAtlasId
        (load-texture texture-atlas-path
                      #:mipmap #f))
  ;; load-texture does this
  #;(glBindTexture GL_TEXTURE_2D
                 TextureAtlasId)

  (CreateShaders)
  (CreateVBO)

  (set! TextureAtlasIndex_UniformId
        (glGetUniformLocation ProgramId "TextureAtlasIndex"))
  (glUniform4fv TextureAtlasIndex_UniformId
                (/ (f32vector-length TextureAtlasIndex) 4)
                TextureAtlasIndex)

  (glClearColor 1.0 1.0 1.0 0.0))

(define (ResizeFunction Width Height)
  (glViewport 0 0 Width Height))

(define (RenderFunction)
  (glClear GL_COLOR_BUFFER_BIT)

  (when #f
    (define (fmodulo x y)
      (- x (* y (floor (/ x y)))))
    
    (define rot 
      (* (* 2 pi)
         (/ (fmodulo (current-inexact-milliseconds)
                     360)
            360)))
    (for ([i (in-range HowManySprites)])
      (f32vector-set! Transforms (+ (* i 3) 2)
                      rot))

    (glBindBuffer GL_ARRAY_BUFFER TransformBufferId)
    (glBufferData GL_ARRAY_BUFFER
                  (gl-vector-sizeof Transforms)
                  Transforms
                  GL_STREAM_DRAW))

  (glDrawElements GL_TRIANGLES (* HowManySprites IndicesPerSprite)
                  GL_UNSIGNED_INT 0))

(define (random-in lo hi)
  (define rng (- hi lo))
  (+ lo (* (random) (+ rng 1))))

(struct sprite (x y hw hh r g b a tex mx my theta) #:transparent)

(define (install-object! i o)
  (printf "~a\n" i)
  (match-define (sprite x y hw hh r g b a tex mx my theta) o)

  (define-syntax-rule
    (v! vector-set! vector 
        how-many-per-index index fudge
        element ...)
    (begin
      (printf "\t~a ~a\n" 'vector index)
      (for ([e (in-list (list element ...))]
            [i (in-naturals)])
        (define idx (+ fudge (+ (* how-many-per-index index) i)))
        (printf "\t\t~a -> ~a <- ~a\n" i idx e)
        (vector-set! vector idx e))))

  (define ul-i (+ (* 4 i) 0))
  (define ur-i (+ (* 4 i) 1))
  (define ll-i (+ (* 4 i) 2))
  (define lr-i (+ (* 4 i) 3))
  (v! u32vector-set! Indices
      6 i 0
      ul-i ur-i ll-i
      ll-i ur-i lr-i)

  (v! f32vector-set! Vertices
      (* 4 4) i 0
      ;; ul
      (- x hw) (+ y hh) 0.0 1.0
      ;; ur
      (+ x hw) (+ y hh) 0.0 1.0
      ;; ll
      (- x hw) (- y hh) 0.0 1.0
      ;; lr
      (+ x hw) (- y hh) 0.0 1.0)
  (v! f32vector-set! Colors
      (* 4 4) i 0
      ;; ul
      r g b a
      ;; ur
      r g b a
      ;; ll
      r g b a
      ;; lr
      r g b a)

  ;; XXX
  (u32vector-set! TexIndexes i tex)

  (f32vector-set! Transforms (+ (* i 3) 0) mx)
  (f32vector-set! Transforms (+ (* i 3) 1) my)
  (f32vector-set! Transforms (+ (* i 3) 2) theta))

(define HowManySprites 
  #;4 
  (* 1 512))
(define IndicesPerSprite 6)
(define VertsPerSprite 4)

(define Indices
  (make-u32vector (* HowManySprites IndicesPerSprite)))

(define Vertices
  (make-f32vector (* (* HowManySprites VertsPerSprite) 4)))
(define Colors
  (make-f32vector (* (* HowManySprites VertsPerSprite) 4)))
(define TexIndexes
  (make-u32vector (* HowManySprites VertsPerSprite)))
(define Transforms
  (make-f32vector (* HowManySprites VertsPerSprite 3)))

(define objects
  #;(list (sprite 0.0 0.0 0.8 0.8
                1.0 0.0 0.0 1.0
                0 1.0 1.0 0.0)
        (sprite 0.0 0.0 0.4 0.4
                0.0 1.0 0.0 1.0
                0 1.0 1.0 0.0)
        (sprite 0.0 0.0 0.2 0.2
                0.0 0.0 1.0 1.0
                0 1.0 1.0 0.0)
        (sprite 0.0 0.0 0.1 0.1
                0.0 0.0 0.0 1.0
                0 1.0 1.0 0.0))
  
  (for/list ([i (in-range HowManySprites)])
    (sprite (random-in -1.0 1.0)
            (random-in -1.0 1.0)
            (random)
            (random)
            
            (random)
            (random)
            (random)
            (random)
            
            (random 2)

            (random)
            (random)
            (random))))

(define (install-objects! t)
  (let loop ([offset 0] [t t])
    (match t
      [(list)
       offset]
      [(cons b a)
       (loop (loop offset b) a)]
      [(? sprite? o)
       (install-object! offset o)
       (add1 offset)])))

(install-objects! objects)

(begin
  (define one-frame-size
    (apply +
           (map gl-vector-sizeof
                (list Indices Vertices Colors 
                      TexIndexes Transforms))))

  (printf "               Sprites: ~a\n"
          HowManySprites)
  (printf "               1 frame: ~a Kb\n"
          (real->decimal-string
           (/ one-frame-size 1024)))
  (printf "                60 FPS: ~a Gb/s\n"
          (real->decimal-string
           (/ (* 60 one-frame-size) (* 1024 1024 1024))))
  (printf "     1 sprite @ 60 FPS: ~a Kb/s\n"
          (real->decimal-string
           (/ (/ (* 60 one-frame-size) HowManySprites)
              1024)))
  (printf "GeForce 320M bandwidth: ~a Gb/s\n"
          (real->decimal-string
           17.056))
  (printf "           Max sprites: ~a\n"
          (real->decimal-string
           (/ (* 17.056 1024 1024 1024)
              (/ (* 60 one-frame-size) HowManySprites))))
  (printf "  Intel 4000 bandwidth: ~a Gb/s\n"
          (real->decimal-string
           25.6))
  (printf "           Max sprites: ~a\n"
          (real->decimal-string
           (/ (* 25.6 1024 1024 1024)
              (/ (* 60 one-frame-size) HowManySprites)))))

(define TextureAtlasIndex
  (f32vector
   0.0 0.0 0.0 0.0
   0.0 0.0 1.0 1.0))

(define-syntax-rule (define-shader-source id path)
  (begin (define-runtime-path id-path path)
         (define id (file->string id-path))))

(define-shader-source VertexShader-p "../gb/graphics/ngl.vertex.glsl")
(define VertexShader 
  (format VertexShader-p 
          (/ (f32vector-length TextureAtlasIndex) 4)))
(define-shader-source FragmentShader "../gb/graphics/ngl.fragment.glsl")

(define-syntax-rule
  (define-vertex-attrib-array VboId Vertices Index HowMany)
  (begin (set! VboId
               (u32vector-ref (glGenBuffers 1) 0))
         (glBindBuffer GL_ARRAY_BUFFER VboId)
         (glBufferData GL_ARRAY_BUFFER
                       (gl-vector-sizeof Vertices)
                       Vertices
                       GL_STATIC_DRAW)
         (define type (gl-vector->type Vertices))
         (cond
           [(= type GL_FLOAT)
            (glVertexAttribPointer Index HowMany type
                                   #f 0 0)]
           [else
            (glVertexAttribIPointer Index HowMany type
                                    0 0)])
         (glEnableVertexAttribArray Index)))

(define (CreateVBO)  
  (set! VaoId
        (u32vector-ref (glGenVertexArrays 1) 0))
  (glBindVertexArray VaoId)

  (define-vertex-attrib-array VboId Vertices 0 4)
  (define-vertex-attrib-array ColorBufferId Colors 1 4)
  #;(define-vertex-attrib-array TexIndexesBufferId TexIndexes 2 1)
  #;(define-vertex-attrib-array TransformBufferId Transforms 3 3)

  (set! IndexBufferId
        (u32vector-ref (glGenBuffers 1) 0))
  (glBindBuffer GL_ELEMENT_ARRAY_BUFFER IndexBufferId)
  (glBufferData GL_ELEMENT_ARRAY_BUFFER
                (gl-vector-sizeof Indices)
                Indices
                GL_STATIC_DRAW))

(define (print-shader-log glGetShaderInfoLog shader-name shader-id)
  (define-values (infoLen infoLog)
    (glGetShaderInfoLog shader-id 1024))
  (unless (zero? infoLen)
    (eprintf "~a: ~a\n"
             shader-name
             (subbytes infoLog 0 infoLen))
    (exit 1)))

(define (CreateShaders)
  (set! ProgramId (glCreateProgram))

  (define-syntax-rule 
    (compile-shader VertexShaderId
                    GL_VERTEX_SHADER
                    VertexShader)
    (begin (set! VertexShaderId (glCreateShader GL_VERTEX_SHADER))
           (glShaderSource VertexShaderId 1 (vector VertexShader)
                           (s32vector))
           (glCompileShader VertexShaderId)
           (print-shader-log glGetShaderInfoLog 'VertexShader VertexShaderId)
           (glAttachShader ProgramId VertexShaderId)))

  (compile-shader VertexShaderId GL_VERTEX_SHADER
                  VertexShader)
  (compile-shader FragmentShaderId GL_FRAGMENT_SHADER
                  FragmentShader)
  
  (glLinkProgram ProgramId)
  (print-shader-log glGetProgramInfoLog 'Program ProgramId)
  
  (glUseProgram ProgramId))
