#lang racket/base
(require racket/gui
         ffi/vector
         racket/runtime-path
         xml
         gb/graphics/texture-atlas-lib
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
                (texture-atlas-size the-texture-atlas)
                (texture-atlas-vector the-texture-atlas))

  (glEnable GL_DEPTH_TEST)
  (glClearColor 1.0 1.0 1.0 0.0))

(define (ResizeFunction Width Height)
  (glViewport 0 0 Width Height))

(define Frame 0)
(define (RenderFunction)
  (glClear (bitwise-ior GL_DEPTH_BUFFER_BIT GL_COLOR_BUFFER_BIT))

  (set! Frame (modulo (add1 Frame) 60))

  (when (zero? Frame)
    (for ([i (in-range HowManySprites)])
      (f32vector-set! Transforms (+ (* i 3) 2)
                      (add1 (f32vector-ref Transforms (+ (* i 3) 2))))))
  
  ;; Reload all data every frame
  (load-buffer-data VboId Vertices)
  (load-buffer-data ColorBufferId Colors)
  (load-buffer-data TexIndexesBufferId TexIndexes)
  (load-buffer-data TransformBufferId Transforms)

  (glDrawArrays GL_POINTS 0 (/ (f32vector-length Vertices) 4)))

(define the-texture-atlas
  (texture-atlas/size 2))

(define-texture texture:none
  the-texture-atlas 0.0 0.0 0.0 0.0)
(define-texture texture:everything
  the-texture-atlas 0.0 0.0 1.0 1.0)

(define (random-in lo hi)
  (define rng (- hi lo))
  (+ lo (* (random) (+ rng 1))))

(struct sprite (x y w h r g b a tex mx my theta) #:transparent)

(define (install-object! i o)
  (match-define (sprite x y w h r g b a tex mx my theta) o)

  (f32vector-set! Vertices (+ (* i 4) 0) x)
  (f32vector-set! Vertices (+ (* i 4) 1) y)
  (f32vector-set! Vertices (+ (* i 4) 2) w)
  (f32vector-set! Vertices (+ (* i 4) 3) h)

  (f32vector-set! Colors (+ (* i 4) 0) r)
  (f32vector-set! Colors (+ (* i 4) 1) g)
  (f32vector-set! Colors (+ (* i 4) 2) b)
  (f32vector-set! Colors (+ (* i 4) 3) a)

  (u32vector-set! TexIndexes i tex)

  (f32vector-set! Transforms (+ (* i 3) 0) mx)
  (f32vector-set! Transforms (+ (* i 3) 1) my)
  (f32vector-set! Transforms (+ (* i 3) 2) theta))

(define HowManySprites (* 5 512))
(define Vertices (make-f32vector (* HowManySprites 4)))
(define Colors (make-f32vector (* HowManySprites 4)))
(define TexIndexes (make-u32vector HowManySprites))
(define Transforms (make-f32vector (* HowManySprites 3)))

(define objects
  (for/list ([i (in-range HowManySprites)])
    (sprite (random-in -1.0 1.0)
            (random-in -1.0 1.0)
            (random)
            (random)
            
            (random)
            (random)
            (random)
            (random)
            
            (if (zero? (random 2))
              texture:none
              texture:everything)

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
                (list Vertices Colors 
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

(define-syntax-rule (define-shader-source id path)
  (begin (define-runtime-path id-path path)
         (define id (file->string id-path))))

(define-shader-source VertexShader-p "../gb/graphics/ngl.vertex.glsl")
(define VertexShader 
  (format VertexShader-p (texture-atlas-size the-texture-atlas)))
(define-shader-source FragmentShader "../gb/graphics/ngl.fragment.glsl")
(define-shader-source GeometryShader "../gb/graphics/ngl.geometry.glsl")

(define (load-buffer-data VboId Vertices)
  (glBindBuffer GL_ARRAY_BUFFER VboId)
  (glBufferData GL_ARRAY_BUFFER
                (gl-vector-sizeof Vertices)
                Vertices
                GL_STREAM_DRAW))

(define-syntax-rule
  (define-vertex-attrib-array VboId Vertices Index HowMany)
  (begin (set! VboId
               (u32vector-ref (glGenBuffers 1) 0))
         (load-buffer-data VboId Vertices)
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
