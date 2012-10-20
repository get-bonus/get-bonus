#lang racket/base
(require racket/match
         ffi/vector
         gb/graphics/gl-util
         gb/lib/evector
         racket/function
         gb/graphics/texture-atlas-lib
         (planet stephanh/RacketGL/rgl))

(provide make-draw
         (struct-out sprite-info))

(define-evector f32:
  make-f32vector f32vector-ref f32vector-set!)

(struct sprite-info (x y w h r g b a tex mx my theta))

(define-shader-source VertexShader "ngl.vertex.glsl")
(define-shader-source FragmentShader "ngl.fragment.glsl")
(define-shader-source GeometryShader "ngl.geometry.glsl")

(define (make-draw texture-atlas-path
                   texture-atlas-size
                   width height)
  (define InitialSprites (* 2 512))
  (define SpriteData-components
    (+ 4 4 4 3))
  (match-define 
   (list SpriteData-X SpriteData-Y SpriteData-HW SpriteData-HH
         SpriteData-R SpriteData-G SpriteData-B SpriteData-A
         SpriteData-TX SpriteData-TY SpriteData-TW SpriteData-TH
         SpriteData-MX SpriteData-MY SpriteData-ROT)
   (build-list SpriteData-components identity))
  (define SpriteData 
    (f32:make-vector 
     (* InitialSprites 
        SpriteData-components)))

  (define (install-object! i o)
    (match-define (sprite-info x y w h r g b a tex mx my theta) o)
    ;; XXX Would it be faster to do a vector-copy! ?
    (define-syntax-rule
      (install! [SpriteData-X x] ...)
      (begin
        (f32:vector-safe-set! 
         SpriteData 
         (+ (* i SpriteData-components) SpriteData-X)
         x)
        ...))
    (install! [SpriteData-X x]
              [SpriteData-Y y]
              [SpriteData-HW w]
              [SpriteData-HH h]
              [SpriteData-R r]
              [SpriteData-G g]
              [SpriteData-B b]
              [SpriteData-A a]
              [SpriteData-TX (f32vector-ref tex 0)]
              [SpriteData-TY (f32vector-ref tex 1)]
              [SpriteData-TW (f32vector-ref tex 2)]
              [SpriteData-TH (f32vector-ref tex 3)]
              [SpriteData-MX mx]
              [SpriteData-MY my]
              [SpriteData-ROT theta]))

  (define (install-objects! t)
    (let loop ([offset 0] [t t])
      (match t
        [(list)
         offset]
        [(cons b a)
         (loop (loop offset b) a)]
        [o
         (install-object! offset o)
         (add1 offset)])))

  (define TextureAtlasId
    (load-texture texture-atlas-path
                  #:mipmap #f))

  ;; Create Shaders
  (define ProgramId (glCreateProgram))  

  (define&compile-shader VertexShaderId GL_VERTEX_SHADER
    ProgramId VertexShader)
  (define&compile-shader FragmentShaderId GL_FRAGMENT_SHADER
    ProgramId FragmentShader)
  (define&compile-shader GeometryShaderId GL_GEOMETRY_SHADER
    ProgramId GeometryShader)

  (glLinkProgram ProgramId)
  (print-shader-log glGetProgramInfoLog 'Program ProgramId)

  (glUseProgram ProgramId)
  (glUniform1i (glGetUniformLocation ProgramId "TextureAtlasSize")
               texture-atlas-size)
  (glUniform1f (glGetUniformLocation ProgramId "ViewportWidth")
               width)
  (glUniform1f (glGetUniformLocation ProgramId "ViewportHeight")
               height)
  (glUseProgram 0)

  ;; Create VBOs
  (define VaoId
    (u32vector-ref (glGenVertexArrays 1) 0))
  (glBindVertexArray VaoId)

  (define (load-buffer-data gl-type VboId Vertices)
    (glBindBuffer GL_ARRAY_BUFFER VboId)
    (glBufferData GL_ARRAY_BUFFER
                  (* (gl-type-sizeof gl-type) (f32:vector-length Vertices))
                  (f32:vector-base Vertices)
                  GL_STREAM_DRAW))

  (define-syntax-rule
    (define-vertex-attrib-array 
      Index SpriteData-start SpriteData-end type)
    (begin 
      (define HowMany
        (add1 (- SpriteData-end SpriteData-start)))
      (glVertexAttribPointer 
       Index HowMany type
       #f 
       (* (gl-type-sizeof type)
          SpriteData-components)
       (* (gl-type-sizeof type)
          SpriteData-start))
      (glEnableVertexAttribArray Index)))

  (define VboId
    (u32vector-ref (glGenBuffers 1) 0))
  (load-buffer-data GL_FLOAT VboId SpriteData)

  (define-vertex-attrib-array 0 SpriteData-X SpriteData-HH GL_FLOAT)
  (define-vertex-attrib-array 1 SpriteData-R SpriteData-A GL_FLOAT)
  (define-vertex-attrib-array 2 SpriteData-TX SpriteData-TH GL_FLOAT)
  (define-vertex-attrib-array 3 SpriteData-MX SpriteData-ROT GL_FLOAT)
  (glBindBuffer GL_ARRAY_BUFFER 0)

  (glBindVertexArray 0)

  (define (draw objects)
    (glBindVertexArray VaoId)

    (glEnableVertexAttribArray 0)
    (glEnableVertexAttribArray 1)
    (glEnableVertexAttribArray 2)
    (glEnableVertexAttribArray 3)

    (glBindTexture GL_TEXTURE_2D
                   TextureAtlasId)

    (define offset (install-objects! objects))

    ;; Reload all data every frame
    (load-buffer-data GL_FLOAT VboId SpriteData)
    (glBindBuffer GL_ARRAY_BUFFER 0)

    (glUseProgram ProgramId)

    (glPushAttrib (bitwise-ior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))

    (glEnable GL_DEPTH_TEST)
    (glClearColor 1.0 1.0 1.0 0.0)

    (glEnable GL_BLEND)
    (glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA)
    (glEnable GL_ALPHA_TEST)
    (glAlphaFunc GL_GREATER 0.0)

    (glClear (bitwise-ior GL_DEPTH_BUFFER_BIT GL_COLOR_BUFFER_BIT))

    (define count 
      (/ (f32:vector-length SpriteData) 
         SpriteData-components))
    (glDrawArrays GL_POINTS 0 count)

    (glPopAttrib)

    (glBindTexture GL_TEXTURE_2D 0)

    (glDisableVertexAttribArray 3)
    (glDisableVertexAttribArray 2)
    (glDisableVertexAttribArray 1)
    (glDisableVertexAttribArray 0)

    (glBindVertexArray 0)

    (glUseProgram 0))

  draw)
