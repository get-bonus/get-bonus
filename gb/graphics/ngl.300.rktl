(define-shader-source Old-VertexShader "ngl.vertex.old.glsl")
(define-shader-source Old-FragmentShader "ngl.fragment.old.glsl")

(define (make-draw/300 texture-atlas-path
                       texture-atlas-size
                       width height)
  (define SpriteData-components
    (+ 4 4 4 3 2))
  (match-define
   (list SpriteData-X SpriteData-Y SpriteData-HW SpriteData-HH
         SpriteData-R SpriteData-G SpriteData-B SpriteData-A
         SpriteData-TX SpriteData-TY SpriteData-TW SpriteData-TH
         SpriteData-MX SpriteData-MY SpriteData-ROT
         SpriteData-Horiz SpriteData-Vert)
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
      (install! j [SpriteData-X x] ...)
      (begin
        (cvector-set!
         SpriteData
         (+ (* i 6 SpriteData-components)
            (* j SpriteData-components)
            SpriteData-X)
         x)
        ...))
    (when (i . < . SpriteData-count)
      (define-syntax-rule (point-install! Horiz Vert j)
        (install! j
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
                  [SpriteData-Horiz Horiz]
                  [SpriteData-Vert Vert]))
      (point-install! -1.0 +1.0 0)
      (point-install! +1.0 +1.0 1)
      (point-install! -1.0 -1.0 2)
      (point-install! -1.0 -1.0 3)
      (point-install! +1.0 +1.0 4)
      (point-install! +1.0 -1.0 5)))

  ;; Create Shaders
  (define ProgramId (glCreateProgram))
  (glBindAttribLocation ProgramId 0 "in_Position")
  (glBindAttribLocation ProgramId 1 "in_Color")
  (glBindAttribLocation ProgramId 2 "in_TexCoord")
  (glBindAttribLocation ProgramId 3 "in_Transforms")
  (glBindAttribLocation ProgramId 4 "in_VertexSpecification")

  (define&compile-shader Old-VertexShaderId GL_VERTEX_SHADER
    ProgramId Old-VertexShader)
  (define&compile-shader Old-FragmentShaderId GL_FRAGMENT_SHADER
    ProgramId Old-FragmentShader)

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
     [4 SpriteData-Horiz SpriteData-Vert])
    #:render
    [GL_TRIANGLES 6 5]))
