#lang racket/base
(require racket/gui
         ffi/vector
         racket/runtime-path
         xml
         gb/gui/fullscreen
         (planet stephanh/RacketGL/rgl))

(define VaoId #f)
(define VboId #f)
(define ColorBufferId #f)
(define TexIndexesBufferId #f)
(define TransformBufferId #f)

(define ProgramId #f)
(define VertexShaderId #f)
(define FragmentShaderId #f)
(define GeometryShaderId #f)

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
                GL_STREAM_DRAW)

  (glDrawArrays GL_POINTS 0 (/ (f32vector-length Vertices) 4)))

;; One sprite = 4*(4 + 4 + 1 + 3) bytes
;; GeForce 320M bandwidth = 17 Gb / s
;; = 6.3 million sprites at 60FPS

(define (random-in lo hi)
  (define rng (- hi lo))
  (+ lo (* (random) (+ rng 1))))

(define HowManySprites 512)
(define Vertices (make-f32vector (* HowManySprites 4)))
(define Colors (make-f32vector (* HowManySprites 4)))
(define TexIndexes (make-u32vector HowManySprites))
(define Transforms (make-f32vector (* HowManySprites 3)))

(for ([i (in-range HowManySprites)])
  (f32vector-set! Vertices (+ (* i 4) 0) (random-in -1.0 1.0))
  (f32vector-set! Vertices (+ (* i 4) 1) (random-in -1.0 1.0))
  (f32vector-set! Vertices (+ (* i 4) 2) (random))
  (f32vector-set! Vertices (+ (* i 4) 3) (random))

  (f32vector-set! Colors (+ (* i 4) 0) (random))
  (f32vector-set! Colors (+ (* i 4) 1) (random))
  (f32vector-set! Colors (+ (* i 4) 2) (random))
  (f32vector-set! Colors (+ (* i 4) 3) (random))

  (u32vector-set! TexIndexes i (random 2))

  (f32vector-set! Transforms (+ (* i 3) 0) (random))
  (f32vector-set! Transforms (+ (* i 3) 1) (random))
  (f32vector-set! Transforms (+ (* i 3) 2) (random)))

(printf "Total size: ~a kb\n"
        (real->decimal-string
         (/ (apply + 
                   (map gl-vector-sizeof
                        (list Vertices Colors 
                              TexIndexes Transforms)))
            1024)))

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
(define-shader-source GeometryShader "../gb/graphics/ngl.geometry.glsl")

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
  (define-vertex-attrib-array TexIndexesBufferId TexIndexes 2 1)
  (define-vertex-attrib-array TransformBufferId Transforms 3 3))

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
  (compile-shader GeometryShaderId GL_GEOMETRY_SHADER
                  GeometryShader)

  (glLinkProgram ProgramId)
  (print-shader-log glGetProgramInfoLog 'Program ProgramId)
  
  (glUseProgram ProgramId))
