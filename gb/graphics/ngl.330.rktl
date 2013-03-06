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
         SpriteData-MX SpriteData-MY SpriteData-ROT
         )
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
         (+ (* i SpriteData-components)

            SpriteData-X)
         x)
        ...))
    (when (i . < . SpriteData-count)

      (install!
       [SpriteData-X x]
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
       [SpriteData-ROT theta]

       ))





    )

  ;; Create Shaders
  (define ProgramId (glCreateProgram))






  (define&compile-shader VertexShaderId GL_VERTEX_SHADER
    ProgramId VertexShader)
  (define&compile-shader FragmentShaderId GL_FRAGMENT_SHADER
    ProgramId FragmentShader)
  (define&compile-shader GeometryShaderId GL_GEOMETRY_SHADER
    ProgramId GeometryShader)

  (define-draw draw
    texture-atlas-path texture-atlas-size width height
    ProgramId
    SpriteData SpriteData-count SpriteData-count:new SpriteData-components
    install-object!
    #:attrib
    ([0 SpriteData-X SpriteData-HH]
     [1 SpriteData-R SpriteData-A]
     [2 SpriteData-TX SpriteData-TH]
     [3 SpriteData-MX SpriteData-ROT]
     )
    #:render
    [GL_POINTS 1 4]))
