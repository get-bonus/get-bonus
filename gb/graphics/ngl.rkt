#lang racket/base
(require racket/match
         ffi/vector
         racket/file
         ffi/cvector
         (only-in ffi/unsafe
                  _float)
         ffi/unsafe/cvector
         gb/graphics/gl-util
         racket/function
         racket/contract
         gb/graphics/texture-atlas-lib
         gb/lib/performance-log
         gb/lib/gzip
         opengl)

(define debug? #f)

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
   (-> path-string? fixnum?
       path-string? fixnum? fixnum?
       flonum? flonum?
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
    [pal palette?]
    [mx flonum?]
    [my flonum?]
    [theta flonum?]))))

(struct sprite-info (x y hw hh r g b a tex pal mx my theta) #:transparent)

(define (make-draw . args)
  (cond
    [(gl-version-at-least? (list 3 0))
     (apply make-draw/300 args)]
    [else
     (error 'ngl "Your version of OpenGL ~a is too old to support NGL"
            (gl-version))]))

(define-shader-source VertexShader "ngl.vertex.glsl")
(define-shader-source FragmentShader "ngl.fragment.glsl")

(define (make-draw/300 sprite-atlas-path
                       sprite-atlas-size
                       palette-atlas-path 
                       palette-atlas-count palette-atlas-depth
                       width height)
  (define SpriteData-components
    (+ 4 4 4 1 3 2))
  (match-define
   (list SpriteData-X SpriteData-Y SpriteData-HW SpriteData-HH
         SpriteData-R SpriteData-G SpriteData-B SpriteData-A
         SpriteData-TX SpriteData-TY SpriteData-TW SpriteData-TH
         SpriteData-Pal
         SpriteData-MX SpriteData-MY SpriteData-ROT
         SpriteData-Horiz SpriteData-Vert)
   (build-list SpriteData-components identity))
  (define SpriteData-count
    0)
  (define SpriteData #f)

  (define (install-object! i o)
    (match-define (sprite-info x y w h r g b a tex pal mx my theta) o)
    ;; XXX If I change to using cstructs, then I can do cvector-set
    ;; and plop everything at once, except I don't want the user to
    ;; know about Horiz and Vert, so I'd really need to make a
    ;; sub-struct with two more fields.
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
                  ;; xxx Can I change this to a single float and do a
                  ;; lookup in a nX4 Sampler2D for the other
                  ;; information? Can I get enough information in each
                  ;; color?
                  ;;
                  ;; GL_RGBA32F or GL_RGBA32UI is what I want
                  [SpriteData-TX (f32vector-ref tex 0)]
                  [SpriteData-TY (f32vector-ref tex 1)]
                  [SpriteData-TW (f32vector-ref tex 2)]
                  [SpriteData-TH (f32vector-ref tex 3)]
                  [SpriteData-Pal (exact->inexact pal)]
                  [SpriteData-MX mx]
                  [SpriteData-MY my]
                  [SpriteData-ROT theta]
                  [SpriteData-Horiz Horiz]
                  [SpriteData-Vert Vert]))

      ;; XXX look at http://developer.apple.com/library/ios/#documentation/3ddrawing/conceptual/opengles_programmingguide/TechniquesforWorkingwithVertexData/TechniquesforWorkingwithVertexData.html to create degenerative triangle strips and send less information? It seems like I would send D twice rather than C AND B twice


      ;; A
      (point-install! -1.0 +1.0 0)
      ;; B
      (point-install! +1.0 +1.0 1)
      ;; C
      (point-install! -1.0 -1.0 2)
      ;; C
      (point-install! -1.0 -1.0 3)
      ;; B
      (point-install! +1.0 +1.0 4)
      ;; D
      (point-install! +1.0 -1.0 5)))

  ;; Create Shaders
  (define ProgramId (glCreateProgram))
  (glBindAttribLocation ProgramId 0 "in_Position")
  (glBindAttribLocation ProgramId 1 "in_Color")
  (glBindAttribLocation ProgramId 2 "in_TexCoord")
  (glBindAttribLocation ProgramId 3 "in_Transforms")
  (glBindAttribLocation ProgramId 4 "in_VertexSpecification")
  (glBindAttribLocation ProgramId 5 "in_Palette")

  (define&compile-shader VertexShaderId GL_VERTEX_SHADER
    ProgramId VertexShader)
  (define&compile-shader FragmentShaderId GL_FRAGMENT_SHADER
    ProgramId FragmentShader)

  (define DrawType GL_TRIANGLES)
  (define DrawnMult 6)
  (define AttributeCount 6)

  (define *initialize-count*
    (* 2 512))

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
  (define (count-objects t)
    (match t
      [(list)
       0]
      [(cons b a)
       (+ (count-objects b) (count-objects a))]
      [o
       1]))

  (define SpriteAtlasId (u32vector-ref (glGenTextures 1) 0))
  (glBindTexture GL_TEXTURE_2D SpriteAtlasId)
  (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S GL_CLAMP)
  (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T GL_CLAMP)
  (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_LINEAR)
  (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_LINEAR)
  (glTexImage2D GL_TEXTURE_2D
                0 GL_R8
                sprite-atlas-size sprite-atlas-size 0
                GL_RED GL_UNSIGNED_BYTE
                (gunzip-bytes (file->bytes sprite-atlas-path)))

  ;; xxx Maybe GL_RGBA8 directly?
  (define PaletteAtlasId
    (load-texture palette-atlas-path
                  #:mipmap #f))

  (glLinkProgram ProgramId)
  (print-shader-log glGetProgramInfoLog 'Program ProgramId)

  (glUseProgram ProgramId)
  (glUniform1i (glGetUniformLocation ProgramId "SpriteAtlasTex")
               0)
  (glUniform1i (glGetUniformLocation ProgramId "SpriteAtlasSize")
               sprite-atlas-size)
  (glUniform1i (glGetUniformLocation ProgramId "PaletteAtlasTex")
               1)
  (glUniform1i (glGetUniformLocation ProgramId "PaletteAtlasCount")
               palette-atlas-count)
  (glUniform1i (glGetUniformLocation ProgramId "PaletteAtlasDepth")
               palette-atlas-depth)
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

  (define-syntax-rule
    (define-vertex-attrib-array*
      [AttribId AttribStart AttribEnd] ...)
    (begin
      (define-vertex-attrib-array AttribId AttribStart AttribEnd GL_FLOAT)
      ...))

  ;; xxx this is awkward, but ctype->layout might help on the next
  ;; version
  (define-vertex-attrib-array*
    [0 SpriteData-X SpriteData-HH]
    [1 SpriteData-R SpriteData-A]
    [2 SpriteData-TX SpriteData-TH]
    [3 SpriteData-MX SpriteData-ROT]
    [4 SpriteData-Horiz SpriteData-Vert]
    [5 SpriteData-Pal SpriteData-Pal])

  (glBindBuffer GL_ARRAY_BUFFER 0)

  (glBindVertexArray 0)

  (define (draw objects)
    (glBindVertexArray VaoId)

    (for ([i (in-range AttributeCount)])
      (glEnableVertexAttribArray i))
    
    (glActiveTexture GL_TEXTURE0)
    (glBindTexture GL_TEXTURE_2D SpriteAtlasId)
    (glActiveTexture GL_TEXTURE1)
    (glBindTexture GL_TEXTURE_2D PaletteAtlasId)

    (glBindBuffer GL_ARRAY_BUFFER VboId)

    (define early-count (count-objects objects))
    (when debug?
      (printf "early count is ~a\n" early-count))
    (define SpriteData-count:new (max *initialize-count* early-count))

    (unless (>= SpriteData-count SpriteData-count:new)
      (define SpriteData-count:old SpriteData-count)
      (set! SpriteData-count
            (max (* 2 SpriteData-count)
                 SpriteData-count:new))
      (when debug?
        (printf "~a -> max(~a,~a) = ~a\n"
                SpriteData-count:old
                (* 2 SpriteData-count)
                SpriteData-count:new
                SpriteData-count))
      (glBufferData GL_ARRAY_BUFFER
                    (* SpriteData-count
                       DrawnMult
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
               DrawnMult
               SpriteData-components
               (gl-type-sizeof GL_FLOAT))
            (bitwise-ior
             ;; We are overriding everything (this would be wrong if
             ;; we did the caching "optimization" I imagine)
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
              DrawnMult
              SpriteData-components)))

    ;; Reload all data every frame
    (install-objects! objects)
    (define this-count early-count)
    (performance-log! this-count)
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

    (define drawn-count this-count)
    (glDrawArrays
     DrawType 0
     (* DrawnMult drawn-count))

    (performance-log! drawn-count)

    (glPopAttrib)

    ;; GL_TEXTURE1 is active
    (glBindTexture GL_TEXTURE_2D 0)
    (glActiveTexture GL_TEXTURE0)
    (glBindTexture GL_TEXTURE_2D 0)

    (for ([i (in-range AttributeCount)])
      (glDisableVertexAttribArray i))

    (glBindVertexArray 0)

    (glUseProgram 0))

  draw)
