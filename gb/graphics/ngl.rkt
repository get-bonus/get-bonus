#lang racket/base
(require racket/match
         ffi/vector
         ffi/cvector
         (only-in ffi/unsafe
                  _float)
         ffi/unsafe/cvector
         gb/graphics/gl-util
         racket/function
         racket/contract
         gb/graphics/texture-atlas-lib
         gb/lib/performance-log
         opengl)

(define sprite-tree/c
  ;; XXX really a tree of sprite-info?, but that's expensive to check
  any/c)

(provide
 (contract-out
  [sprite-tree/c
   contract?]
  ;; XXX these functions really take single-flonum? not flonum? but
  ;; most things with .0 in them are really double flonums in Racket.
  [make-draw
   (-> path-string? fixnum? flonum? flonum?
       (-> sprite-tree/c void))]
  (struct
   sprite-info
   ([x flonum?]
    [y flonum?]
    [hw flonum?]
    [hh flonum?]
    [r flonum?]
    [g flonum?]
    [b flonum?]
    [a flonum?]
    [tex texture?]
    [mx flonum?]
    [my flonum?]
    [theta flonum?]))))

(struct sprite-info (x y hw hh r g b a tex mx my theta) #:transparent)

(define (make-draw texture-atlas-path
                   texture-atlas-size
                   width height)
  (cond
    [(gl-version-at-least? (list 3 3))
     (make-draw/330 texture-atlas-path
                    texture-atlas-size
                    width height)]
    [(gl-version-at-least? (list 3 0))
     (make-draw/300 texture-atlas-path
                    texture-atlas-size
                    width height)]
    [else
     (error 'ngl "Your version of OpenGL ~a is too old to support NGL"
            (gl-version))]))

;; Old version w/o geometry shaders
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
  (glBindAttribLocation ProgramId 0 "in_Position")
  (glBindAttribLocation ProgramId 1 "in_Color")
  (glBindAttribLocation ProgramId 2 "in_TexCoord")
  (glBindAttribLocation ProgramId 3 "in_Transforms")
  (glBindAttribLocation ProgramId 4 "in_VertexSpecification")

  (define&compile-shader Old-VertexShaderId GL_VERTEX_SHADER
    ProgramId Old-VertexShader)
  (define&compile-shader Old-FragmentShaderId GL_FRAGMENT_SHADER
    ProgramId Old-FragmentShader)

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
  (define-vertex-attrib-array 4 SpriteData-Horiz SpriteData-Vert GL_FLOAT)
  (glBindBuffer GL_ARRAY_BUFFER 0)

  (glBindVertexArray 0)

  (define (draw objects)
    (glBindVertexArray VaoId)

    (glEnableVertexAttribArray 0)
    (glEnableVertexAttribArray 1)
    (glEnableVertexAttribArray 2)
    (glEnableVertexAttribArray 3)
    (glEnableVertexAttribArray 4)

    (glBindTexture GL_TEXTURE_2D
                   TextureAtlasId)

    (glBindBuffer GL_ARRAY_BUFFER VboId)

    (unless (>= SpriteData-count SpriteData-count:new)
      (define SpriteData-count:old SpriteData-count)
      (set! SpriteData-count
            (max (* 2 SpriteData-count)
                 SpriteData-count:new))
      ;; (printf "~a -> max(~a,~a) = ~a\n"
      ;;         SpriteData-count:old
      ;;         (* 2 SpriteData-count)
      ;;         SpriteData-count:new
      ;;         SpriteData-count)
      (glBufferData GL_ARRAY_BUFFER
                    (* SpriteData-count
                       6
                       SpriteData-components
                       (gl-type-sizeof GL_FLOAT))
                    #f
                    GL_STREAM_DRAW))

    (performance-log! SpriteData-count)

    (set! SpriteData
          (make-cvector*
           (glMapBufferRange
            GL_ARRAY_BUFFER
            0
            (* SpriteData-count
               6
               SpriteData-components
               (gl-type-sizeof GL_FLOAT))
            (bitwise-ior
             ;; We are overriding everything (this would be wrong if
             ;; we did the cachinge "optimization" I imagine)
             GL_MAP_INVALIDATE_RANGE_BIT
             GL_MAP_INVALIDATE_BUFFER_BIT

             ;; We are not doing complex queues, so don't block other
             ;; operations (but it doesn't seem to improve performance
             ;; by having this option)
             ;; GL_MAP_UNSYNCHRONIZED_BIT

             ;; We are writing
             GL_MAP_WRITE_BIT))
           _float
           (* SpriteData-count
              6
              SpriteData-components)))

    ;; Reload all data every frame
    (define this-count (install-objects! objects))
    (performance-log! this-count)
    (set! SpriteData-count:new this-count)
    (glUnmapBuffer GL_ARRAY_BUFFER)
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

    (define drawn-count
      (min this-count SpriteData-count))
    (glDrawArrays
     GL_TRIANGLES 0
     (* 6 drawn-count))

    (performance-log! drawn-count)

    (glPopAttrib)

    (glBindTexture GL_TEXTURE_2D 0)

    (glDisableVertexAttribArray 4)
    (glDisableVertexAttribArray 3)
    (glDisableVertexAttribArray 2)
    (glDisableVertexAttribArray 1)
    (glDisableVertexAttribArray 0)

    (glBindVertexArray 0)

    (glUseProgram 0))

  draw)

;; New version w/ geometry shaders

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

  (define (draw objects)
    (glBindVertexArray VaoId)

    (glEnableVertexAttribArray 0)
    (glEnableVertexAttribArray 1)
    (glEnableVertexAttribArray 2)
    (glEnableVertexAttribArray 3)

    (glBindTexture GL_TEXTURE_2D
                   TextureAtlasId)

    (glBindBuffer GL_ARRAY_BUFFER VboId)

    (unless (>= SpriteData-count SpriteData-count:new)
      (define SpriteData-count:old SpriteData-count)
      (set! SpriteData-count
            (max (* 2 SpriteData-count)
                 SpriteData-count:new))
      ;; (printf "~a -> max(~a,~a) = ~a\n"
      ;;         SpriteData-count:old
      ;;         (* 2 SpriteData-count)
      ;;         SpriteData-count:new
      ;;         SpriteData-count)
      (glBufferData GL_ARRAY_BUFFER
                    (* SpriteData-count
                       SpriteData-components
                       (gl-type-sizeof GL_FLOAT))
                    #f
                    GL_STREAM_DRAW))

    (performance-log! SpriteData-count)

    (set! SpriteData
          (make-cvector*
           (glMapBufferRange
            GL_ARRAY_BUFFER
            0
            (* SpriteData-count
               SpriteData-components
               (gl-type-sizeof GL_FLOAT))
            (bitwise-ior
             ;; We are overriding everything (this would be wrong if
             ;; we did the cachinge "optimization" I imagine)
             GL_MAP_INVALIDATE_RANGE_BIT
             GL_MAP_INVALIDATE_BUFFER_BIT

             ;; We are not doing complex queues, so don't block other
             ;; operations (but it doesn't seem to improve performance
             ;; by having this option)
             ;; GL_MAP_UNSYNCHRONIZED_BIT

             ;; We are writing
             GL_MAP_WRITE_BIT))
           _float
           (* SpriteData-count
              SpriteData-components)))

    ;; Reload all data every frame
    (define this-count (install-objects! objects))
    (performance-log! this-count)
    (set! SpriteData-count:new this-count)
    (glUnmapBuffer GL_ARRAY_BUFFER)
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

    (define drawn-count
      (min this-count SpriteData-count))
    (glDrawArrays
     GL_POINTS 0
     drawn-count)

    (performance-log! drawn-count)

    (glPopAttrib)

    (glBindTexture GL_TEXTURE_2D 0)

    (glDisableVertexAttribArray 3)
    (glDisableVertexAttribArray 2)
    (glDisableVertexAttribArray 1)
    (glDisableVertexAttribArray 0)

    (glBindVertexArray 0)

    (glUseProgram 0))

  draw)
