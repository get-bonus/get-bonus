
(define-shader-source VertexShader "ngl.vertex.glsl")
(define-shader-source FragmentShader "ngl.fragment.glsl")
(define-shader-source GeometryShader "ngl.geometry.glsl")

(define (make-draw/330 texture-atlas-path
                       texture-atlas-size
                       width height)
  (define SpriteData-components
    (+ 4 4 4 3))
  (match-define
   (list SpriteData-X SpriteData-Y SpriteData-HW SpriteData-HH
         SpriteData-R SpriteData-G SpriteData-B SpriteData-A
         SpriteData-TX SpriteData-TY SpriteData-TW SpriteData-TH
         SpriteData-MX SpriteData-MY SpriteData-ROT)
   (build-list SpriteData-components identity))
  (define SpriteData-count
    0)
  (define SpriteData-count:new
    (* 2 512))
  (define SpriteData #f)

  (define (install-object! i o)
    (match-define (sprite-info x y w h r g b a tex mx my theta) o)
    ;; XXX Would it be faster to do a vector-copy! ?
    (define-syntax-rule
      (install! [SpriteData-X x] ...)
      (begin
        (cvector-set!
         SpriteData
         (+ (* i SpriteData-components) SpriteData-X)
         x)
        ...))
    (when (i . < . SpriteData-count)
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
                [SpriteData-ROT theta])))

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

  (glBindBuffer GL_ARRAY_BUFFER VboId)
  (define-vertex-attrib-array 0 SpriteData-X SpriteData-HH GL_FLOAT)
  (define-vertex-attrib-array 1 SpriteData-R SpriteData-A GL_FLOAT)
  (define-vertex-attrib-array 2 SpriteData-TX SpriteData-TH GL_FLOAT)
  (define-vertex-attrib-array 3 SpriteData-MX SpriteData-ROT GL_FLOAT)
  (glBindBuffer GL_ARRAY_BUFFER 0)

  (glBindVertexArray 0)

  (define-draw draw
    VaoId TextureAtlasId VboId ProgramId
    SpriteData SpriteData-count SpriteData-count:new SpriteData-components
    install-objects!
    GL_POINTS 1 4)

  draw)
