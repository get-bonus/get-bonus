#lang racket/base
(require racket/match
         racket/file
         ffi/vector
         racket/runtime-path
         gb/lib/evector
         gb/graphics/texture-atlas-lib
         (planet stephanh/RacketGL/rgl))
(provide draw
         (struct-out sprite))

(define-evector f32:
  make-f32vector f32vector-ref f32vector-set!)
(define-evector u32:
  make-u32vector u32vector-ref u32vector-set!)

(define VaoId #f)
(define VboId #f)
(define ColorBufferId #f)
(define TexCoordsBufferId #f)
(define TransformBufferId #f)

(define ProgramId #f)
(define VertexShaderId #f)
(define FragmentShaderId #f)
(define GeometryShaderId #f)

(define TextureAtlasWidth_UniformId #f)
(define TextureAtlasHeight_UniformId #f)
(define TextureAtlasId #f)

(define init? #f)
(define
  (draw texture-atlas-path
        the-texture-atlas texture-atlas-width texture-atlas-height
        w h
        objects)
  (unless init?
    (Initialize texture-atlas-path
                texture-atlas-width texture-atlas-height)
    (set! init? #t))

  (ResizeFunction w h)
  (RenderFunction the-texture-atlas objects))

(define (Initialize texture-atlas-path
                    texture-atlas-width texture-atlas-height)
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

  (set! TextureAtlasWidth_UniformId
        (glGetUniformLocation ProgramId "TextureAtlasWidth"))
  (glUniform1i TextureAtlasWidth_UniformId texture-atlas-width)
  (set! TextureAtlasHeight_UniformId
        (glGetUniformLocation ProgramId "TextureAtlasHeight"))
  (glUniform1i TextureAtlasHeight_UniformId texture-atlas-height)

  (glEnable GL_DEPTH_TEST)
  (glClearColor 1.0 1.0 1.0 0.0))

(define (ResizeFunction Width Height)
  (glViewport 0 0 Width Height))

(define (RenderFunction the-texture-atlas  objects)
  (glClear (bitwise-ior GL_DEPTH_BUFFER_BIT GL_COLOR_BUFFER_BIT))

  (install-objects! the-texture-atlas objects)

  ;; Reload all data every frame
  (load-buffer-data f32:vector-length f32:vector-base
                    VboId Vertices)
  (load-buffer-data f32:vector-length f32:vector-base
                    ColorBufferId Colors)
  (load-buffer-data u32:vector-length u32:vector-base 
                    TexCoordsBufferId TexCoords)
  (load-buffer-data f32:vector-length f32:vector-base
                    TransformBufferId Transforms)

  (glDrawArrays GL_POINTS 0 (/ (f32:vector-length Vertices) 4)))

(struct sprite (x y w h r g b a tex mx my theta) #:transparent)

(define (install-object! the-texture-atlas i o)
  (match-define (sprite x y w h r g b a tex mx my theta) o)

  (f32:vector-safe-set! Vertices (+ (* i 4) 0) x)
  (f32:vector-safe-set! Vertices (+ (* i 4) 1) y)
  (f32:vector-safe-set! Vertices (+ (* i 4) 2) w)
  (f32:vector-safe-set! Vertices (+ (* i 4) 3) h)

  (f32:vector-safe-set! Colors (+ (* i 4) 0) r)
  (f32:vector-safe-set! Colors (+ (* i 4) 1) g)
  (f32:vector-safe-set! Colors (+ (* i 4) 2) b)
  (f32:vector-safe-set! Colors (+ (* i 4) 3) a)

  (for ([j (in-range 4)])
    (u32:vector-safe-set! TexCoords (+ (* i 4) j)
                     (u32vector-ref
                      (texture-atlas-vector
                       the-texture-atlas)
                      (+ (* tex 4) j))))

  (f32:vector-safe-set! Transforms (+ (* i 3) 0) mx)
  (f32:vector-safe-set! Transforms (+ (* i 3) 1) my)
  (f32:vector-safe-set! Transforms (+ (* i 3) 2) theta))

(define InitialSprites (* 2 512))
(define Vertices (f32:make-vector (* InitialSprites 4)))
(define Colors (f32:make-vector (* InitialSprites 4)))
(define TexCoords (u32:make-vector (* InitialSprites 4)))
(define Transforms (f32:make-vector (* InitialSprites 3)))

(begin
  (define one-frame-size
    (apply +
           (u32:vector-length TexCoords)
           (map f32:vector-length
                (list Vertices Colors
                      Transforms))))
  (unless (zero? one-frame-size)
    (printf "               Sprites: ~a\n"
            InitialSprites)
    (printf "               1 frame: ~a Kb\n"
            (real->decimal-string
             (/ one-frame-size 1024)))
    (printf "                60 FPS: ~a Gb/s\n"
            (real->decimal-string
             (/ (* 60 one-frame-size) (* 1024 1024 1024))))
    (printf "     1 sprite @ 60 FPS: ~a Kb/s\n"
            (real->decimal-string
             (/ (/ (* 60 one-frame-size) InitialSprites)
                1024)))
    (printf "GeForce 320M bandwidth: ~a Gb/s\n"
            (real->decimal-string
             17.056))
    (printf "           Max sprites: ~a\n"
            (real->decimal-string
             (/ (* 17.056 1024 1024 1024)
                (/ (* 60 one-frame-size) InitialSprites))))
    (printf "  Intel 4000 bandwidth: ~a Gb/s\n"
            (real->decimal-string
             25.6))
    (printf "           Max sprites: ~a\n"
            (real->decimal-string
             (/ (* 25.6 1024 1024 1024)
                (/ (* 60 one-frame-size) InitialSprites))))))

(define (install-objects! the-texture-atlas t)
  (let loop ([offset 0] [t t])
    (match t
      [(list)
       offset]
      [(cons b a)
       (loop (loop offset b) a)]
      [(? sprite? o)
       (install-object! the-texture-atlas offset o)
       (add1 offset)])))

(define-syntax-rule (define-shader-source id path)
  (begin (define-runtime-path id-path path)
         (define id (file->string id-path))))

(define-shader-source VertexShader "ngl.vertex.glsl")
(define-shader-source FragmentShader "ngl.fragment.glsl")
(define-shader-source GeometryShader "ngl.geometry.glsl")

(define (load-buffer-data f32:vector-length f32:vector-base VboId Vertices)
  (glBindBuffer GL_ARRAY_BUFFER VboId)
  (glBufferData GL_ARRAY_BUFFER
                (f32:vector-length Vertices)
                (f32:vector-base Vertices)
                GL_STREAM_DRAW))

(define-syntax-rule
  (define-vertex-attrib-array f32:vector-length f32:vector-base
    VboId Vertices Index HowMany type)
  (begin (set! VboId
               (u32vector-ref (glGenBuffers 1) 0))
         (load-buffer-data f32:vector-length f32:vector-base
                           VboId Vertices)
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

  (define-vertex-attrib-array f32:vector-length f32:vector-base
    VboId Vertices 0 4 GL_FLOAT)
  (define-vertex-attrib-array f32:vector-length f32:vector-base
    ColorBufferId Colors 1 4 GL_FLOAT)
  (define-vertex-attrib-array u32:vector-length u32:vector-base
    TexCoordsBufferId TexCoords 2 4 GL_INT)
  (define-vertex-attrib-array f32:vector-length f32:vector-base
    TransformBufferId Transforms 3 3 GL_FLOAT))

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
