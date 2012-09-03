#lang racket/base
(require racket/gui
         ffi/vector
         racket/runtime-path
         xml
         (planet stephanh/RacketGL/rgl))

(define CurrentWidth 800)
(define CurrentHeight 600)

(define FrameCount 0)

(define VaoId #f)
(define VboId #f)
(define ColorBufferId #f)
(define TexCoordBufferId #f)

(define ProgramId #f)
(define VertexShaderId #f)
(define FragmentShaderId #f)
(define GeometryShaderId #f)

(define-syntax-rule (define-shader-source id path)
  (begin (define-runtime-path id-path path)
         (define id (file->bytes id-path))))

(define-shader-source VertexShader "../gb/graphics/ngl.vertex.glsl")
(define-shader-source FragmentShader "../gb/graphics/ngl.fragment.glsl")
(define-shader-source GeometryShader "../gb/graphics/ngl.geometry.glsl")

(module+ main
  (define frame
    (new frame% [label "Example"]))
  (define this-canvas%
    (class canvas%
      (define/override (on-size width height)
        (define dc
          (send this get-dc))
        (define gc
          (send dc get-gl-context))
        (send gc
              call-as-current
              (λ ()
                (do-drawing width height)
                (send gc swap-buffers))))

      (super-new)))

  (define config
    (new gl-config%))
  (send config set-double-buffered #t)

  (define c
    (new this-canvas%
         [parent frame]
         [gl-config config]
         [style '(gl no-autoclear)]))

  (send frame show #t))

(define init? #f)
(define (do-drawing w h)
  (unless init?
    (Initialize)
    (set! init? #t))

  (ResizeFunction w h)
  (RenderFunction))

(define (Initialize)
  (printf "INFO: OpenGL Version ~a\n"
          (glGetString GL_VERSION))  

  (CreateShaders)
  (CreateVBO)

  (glClearColor 1.0 1.0 1.0 0.0))

(define (ResizeFunction Width Height)
  (set! CurrentWidth Width)
  (set! CurrentHeight Height)
  (glViewport 0 0 CurrentWidth CurrentHeight))

(define (RenderFunction)
  (set! FrameCount (add1 FrameCount))

  (glClear GL_COLOR_BUFFER_BIT)

  (glDrawArrays GL_POINTS 0 (/ (f32vector-length Vertices) 4)))

(define Vertices
  (f32vector
    0.0 0.0 0.8 0.8
    0.0 0.0 0.4 0.4
    0.0 0.0 0.2 0.2))

(define Colors
  (f32vector
   1.0 0.0 0.0 1.0
   0.0 1.0 0.0 1.0
   0.0 0.0 1.0 1.0))

;; XXX Actually do this
(define TexCoords
  (f32vector
   0.0 0.0 0.0 0.0
   0.0 0.0 0.0 0.0
   0.0 0.0 0.0 0.0))

(define-syntax-rule (define-vertex-attrib-array VboId Vertices Index)
  (begin (set! VboId
               (u32vector-ref (glGenBuffers 1) 0))
         (glBindBuffer GL_ARRAY_BUFFER VboId)
         (glBufferData GL_ARRAY_BUFFER
                       (gl-vector-sizeof Vertices)
                       Vertices
                       GL_STATIC_DRAW)
         (glVertexAttribPointer Index 4 GL_FLOAT #f 0 0)
         (glEnableVertexAttribArray Index)))

(define (CreateVBO)  
  (set! VaoId
        (u32vector-ref (glGenVertexArrays 1) 0))
  (glBindVertexArray VaoId)

  (define-vertex-attrib-array VboId Vertices 0)
  (define-vertex-attrib-array ColorBufferId Colors 1)
  (define-vertex-attrib-array TexCoordBufferId TexCoords 2))

(define (print-shader-log shader-name shader-id)
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
           (print-shader-log 'VertexShader VertexShaderId)
           (glAttachShader ProgramId VertexShaderId)))

  (compile-shader VertexShaderId GL_VERTEX_SHADER
                  VertexShader)
  (compile-shader FragmentShaderId GL_FRAGMENT_SHADER
                  FragmentShader)
  (compile-shader GeometryShaderId GL_GEOMETRY_SHADER
                  GeometryShader)

  (glLinkProgram ProgramId)
  
  (glUseProgram ProgramId))
