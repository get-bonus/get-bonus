#lang racket/base
(require racket/match
         racket/file
         ffi/vector
         racket/runtime-path
         gb/lib/evector
         gb/graphics/texture-atlas-lib
         (planet stephanh/RacketGL/rgl))
(provide make-draw
         (struct-out sprite))

(define-evector f32:
  make-f32vector f32vector-ref f32vector-set!)
(define-evector u32:
  make-u32vector u32vector-ref u32vector-set!)

(struct sprite (x y w h r g b a tex mx my theta))

(define-syntax-rule (define-shader-source id path)
  (begin (define-runtime-path id-path path)
         (define id (file->string id-path))))

(define-shader-source VertexShader "ngl.vertex.glsl")
(define-shader-source FragmentShader "ngl.fragment.glsl")
(define-shader-source GeometryShader "ngl.geometry.glsl")

(define (make-draw texture-atlas-path
                   the-texture-atlas
                   texture-atlas-width
                   texture-atlas-height
                   w h)
  (define InitialSprites (* 2 512))
  (define Vertices (f32:make-vector (* InitialSprites 4)))
  (define Colors (f32:make-vector (* InitialSprites 4)))
  (define TexCoords (u32:make-vector (* InitialSprites 4)))
  (define Transforms (f32:make-vector (* InitialSprites 3)))

  (define (install-object! i o)
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

  (define TextureAtlasId
    (load-texture texture-atlas-path
                  #:mipmap #f))

  ;; Create Shaders
  (define ProgramId (glCreateProgram))

  (define (print-shader-log glGetShaderInfoLog shader-name shader-id)
    (define-values (infoLen infoLog)
      (glGetShaderInfoLog shader-id 1024))
    (unless (zero? infoLen)
      (eprintf "~a: ~a\n"
               shader-name
               (subbytes infoLog 0 infoLen))
      (exit 1)))

  (define-syntax-rule
    (define&compile-shader VertexShaderId
      GL_VERTEX_SHADER
      VertexShader)
    (begin (define VertexShaderId (glCreateShader GL_VERTEX_SHADER))
           (glShaderSource VertexShaderId 1 (vector VertexShader)
                           (s32vector))
           (glCompileShader VertexShaderId)
           (print-shader-log glGetShaderInfoLog 'VertexShader VertexShaderId)
           (glAttachShader ProgramId VertexShaderId)))

  (define&compile-shader VertexShaderId GL_VERTEX_SHADER
    VertexShader)
  (define&compile-shader FragmentShaderId GL_FRAGMENT_SHADER
    FragmentShader)
  (define&compile-shader GeometryShaderId GL_GEOMETRY_SHADER
    GeometryShader)

  (glLinkProgram ProgramId)
  (print-shader-log glGetProgramInfoLog 'Program ProgramId)

  (glUseProgram ProgramId)
  (glUniform1i (glGetUniformLocation ProgramId "TextureAtlasWidth")
               texture-atlas-width)
  (glUniform1i (glGetUniformLocation ProgramId "TextureAtlasHeight")
               texture-atlas-height)
  (glUseProgram 0)

  ;; Create VBOs
  (define VaoId
    (u32vector-ref (glGenVertexArrays 1) 0))
  (glBindVertexArray VaoId)

  (define (load-buffer-data f32:vector-length f32:vector-base VboId Vertices)
    (glBindBuffer GL_ARRAY_BUFFER VboId)
    (glBufferData GL_ARRAY_BUFFER
                  (f32:vector-length Vertices)
                  (f32:vector-base Vertices)
                  GL_STREAM_DRAW))

  (define-syntax-rule
    (define-vertex-attrib-array f32:vector-length f32:vector-base
      VboId Vertices Index HowMany type)
    (begin (define VboId
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

  (define-vertex-attrib-array f32:vector-length f32:vector-base
    VboId Vertices 0 4 GL_FLOAT)
  (define-vertex-attrib-array f32:vector-length f32:vector-base
    ColorBufferId Colors 1 4 GL_FLOAT)
  (define-vertex-attrib-array u32:vector-length u32:vector-base
    TexCoordsBufferId TexCoords 2 4 GL_INT)
  (define-vertex-attrib-array f32:vector-length f32:vector-base
    TransformBufferId Transforms 3 3 GL_FLOAT)
  (glBindBuffer GL_ARRAY_BUFFER 0)

  (glBindVertexArray 0)

  (define (draw objects)
    (glViewport 0 0 w h)

    (glUseProgram ProgramId)

    (glBindVertexArray VaoId)

    (glEnableVertexAttribArray 0)
    (glEnableVertexAttribArray 1)
    (glEnableVertexAttribArray 2)
    (glEnableVertexAttribArray 3)

    (glBindTexture GL_TEXTURE_2D
                   TextureAtlasId)

    (glPushAttrib (bitwise-ior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))

    (glEnable GL_DEPTH_TEST)
    (glClearColor 1.0 1.0 1.0 0.0)

    (glClear (bitwise-ior GL_DEPTH_BUFFER_BIT GL_COLOR_BUFFER_BIT))

    (install-objects! objects)

    ;; Reload all data every frame
    (load-buffer-data f32:vector-length f32:vector-base
                      VboId Vertices)
    (load-buffer-data f32:vector-length f32:vector-base
                      ColorBufferId Colors)
    (load-buffer-data u32:vector-length u32:vector-base
                      TexCoordsBufferId TexCoords)
    (load-buffer-data f32:vector-length f32:vector-base
                      TransformBufferId Transforms)
    (glBindBuffer GL_ARRAY_BUFFER 0)

    (glDrawArrays GL_POINTS 0 (/ (f32:vector-length Vertices) 4))

    (glPopAttrib)

    (glBindTexture GL_TEXTURE_2D 0)

    (glDisableVertexAttribArray 3)
    (glDisableVertexAttribArray 2)
    (glDisableVertexAttribArray 1)
    (glDisableVertexAttribArray 0)

    (glBindVertexArray 0)

    (glUseProgram 0))

  draw)
