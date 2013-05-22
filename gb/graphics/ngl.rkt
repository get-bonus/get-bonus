#lang racket/base
(require racket/match
         ffi/vector
         racket/file
         racket/list
         ffi/cvector
         (only-in ffi/unsafe
                  ctype-sizeof
                  ctype->layout
                  define-cstruct
                  _float
                  _sint8)
         ffi/unsafe/cvector
         gb/graphics/gl-util
         racket/function
         (only-in math/base sum)
         racket/contract
         gb/graphics/r
         gb/lib/performance-log
         gb/lib/math
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
       path-string?
       path-string? fixnum? fixnum?
       flonum? flonum?
       (-> sprite-tree/c void))])
 ;; xxx contract
 sprite-info?
 (rename-out
  [create-sprite-info sprite-info]))

(define (create-sprite-info x y hw hh r g b a spr pal mx my theta)
  (make-sprite-info x y hw hh r g b a
                    (exact->inexact spr)
                    (exact->inexact pal)
                    mx my theta
                    0.0 0.0))

(define-cstruct _sprite-info
  ([x _float]     ;; 0
   [y _float]     ;; 1
   [hw _float]    ;; 2
   [hh _float]    ;; 3
   [r _float]     ;; 4
   [g _float]     ;; 5
   [b _float]     ;; 6
   [a _float]     ;; 7
   [spr _float]   ;; 8
   [pal _float]   ;; 9
   [mx _float]    ;; 10
   [my _float]    ;; 11
   [theta _float] ;; 12
   [horiz _float] ;; 13
   [vert _float]  ;; 14
   )
  #:alignment 4)

(module+ test
  (eprintf "sprite-info is ~a bytes\n"
           (ctype-sizeof _sprite-info))
  (eprintf "sprite takes sprite-info is ~a bytes\n"
           (* DrawnMult (ctype-sizeof _sprite-info))))

(define ctype-name->bytes
  (match-lambda
   ['sint8 1]
   ['float 4]))
(define (ctype-offset _type offset)
  (sum (map ctype-name->bytes (take (ctype->layout _type) offset))))

(define (sublist l s e)
  (for/list ([x (in-list l)]
             [i (in-naturals)]
             #:when (<= s i)
             #:when (<= i e))
    x))

(define (list-only l)
  (define v (first l))
  (for ([x (in-list (rest l))])
    (unless (eq? v x) (error 'list-only "List is not uniform: ~e" l)))
  v)

(define (ctype-range-type _type s e)
  (list-only (sublist (ctype->layout _type) s e)))

(define ctype->gltype
  (match-lambda
   ['float GL_FLOAT]))

(define (make-draw . args)
  (cond
    [(gl-version-at-least? (list 3 0))
     (apply make-draw/300 args)]
    [else
     (error 'ngl "Your version of OpenGL ~a is too old to support NGL"
            (gl-version))]))

(define-shader-source VertexShader "ngl.vertex.glsl")
(define-shader-source FragmentShader "ngl.fragment.glsl")

(define DrawnMult 6)

(define (make-draw/300 sprite-atlas-path
                       ;; xxx can remove these given below
                       sprite-atlas-size
                       sprite-index-path
                       palette-atlas-path
                       ;; xxx can remove these
                       palette-atlas-count palette-atlas-depth
                       width height)
  (define SpriteData-count
    0)
  (define SpriteData #f)

  (define (install-object! i o)
    (define-syntax-rule (point-install! Horiz Vert j ...)
      (begin
        (set-sprite-info-horiz! o Horiz)
        (set-sprite-info-vert! o Vert)
        (cvector-set! SpriteData (+ (* i 6) j) o)
        ...))
    ;; I once thought I could use a degenerative triangle strip, but
    ;; that adds 2 additional vertices on all but the first and last
    ;; triangles, which would save me exactly 2 vertices total.
    (point-install! -1.0 +1.0 0)
    (point-install! +1.0 +1.0 1 4)
    (point-install! -1.0 -1.0 2 3)
    (point-install! +1.0 -1.0 5))

  ;; Create Shaders
  (define ProgramId (glCreateProgram))
  (glBindAttribLocation ProgramId 0 "in_Position")
  (glBindAttribLocation ProgramId 1 "in_Color")
  (glBindAttribLocation ProgramId 2 "in_TexIndex")
  (glBindAttribLocation ProgramId 3 "in_Transforms")
  (glBindAttribLocation ProgramId 4 "in_VertexSpecification")
  (glBindAttribLocation ProgramId 5 "in_Palette")

  (define&compile-shader VertexShaderId GL_VERTEX_SHADER
    ProgramId VertexShader)
  (define&compile-shader FragmentShaderId GL_FRAGMENT_SHADER
    ProgramId FragmentShader)

  (define DrawType GL_TRIANGLES)
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

  (define (2D-defaults)
    (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S GL_CLAMP)
    (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T GL_CLAMP)
    (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_LINEAR)
    (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_LINEAR))

  (define SpriteAtlasId (u32vector-ref (glGenTextures 1) 0))
  (glBindTexture GL_TEXTURE_2D SpriteAtlasId)
  (2D-defaults)
  (glTexImage2D GL_TEXTURE_2D
                0 GL_R8
                ;; xxx could figure this out from the size of the
                ;; data, since we know it is a power of two squared
                sprite-atlas-size sprite-atlas-size 0
                GL_RED GL_UNSIGNED_BYTE
                (gunzip-bytes (file->bytes sprite-atlas-path)))

  (define PaletteAtlasId
    (load-texture palette-atlas-path
                  #:mipmap #f))

  (define SpriteIndexId (u32vector-ref (glGenTextures 1) 0))
  (glBindTexture GL_TEXTURE_2D SpriteIndexId)
  (2D-defaults)
  (define sprite-index-data
    (gunzip-bytes (file->bytes sprite-index-path)))
  (define sprite-index-bytes 4)
  (define sprite-index-count (/ (bytes-length sprite-index-data)
                                (* 4 sprite-index-bytes)))
  (define effective-sprite-index-count
    (expt 2 (num->pow2 sprite-index-count)))
  (glTexImage2D GL_TEXTURE_2D
                0 GL_RGBA32F
                1 effective-sprite-index-count 0
                GL_RGBA GL_FLOAT
                sprite-index-data)

  (glLinkProgram ProgramId)
  (print-shader-log glGetProgramInfoLog 'Program ProgramId)

  (glUseProgram ProgramId)
  (glUniform1i (glGetUniformLocation ProgramId "SpriteAtlasTex")
               0)
  (glUniform1i (glGetUniformLocation ProgramId "PaletteAtlasTex")
               1)
  (glUniform1i (glGetUniformLocation ProgramId "SpriteIndexTex")
               2)
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
      Index SpriteData-start SpriteData-end)
    (begin
      (define type
        (ctype->gltype (ctype-range-type _sprite-info SpriteData-start SpriteData-end)))
      (define HowMany
        (add1 (- SpriteData-end SpriteData-start)))
      (eprintf "~v\n"
               `(glVertexAttribPointer
                 ,Index ,HowMany ,type
                 #f
                 ,(ctype-sizeof _sprite-info)                 
                 old
                 ,(* (gl-type-sizeof type)
                     SpriteData-start)
                 old-end
                 ,(* (gl-type-sizeof type)
                     SpriteData-end)
                 new
                 ,(ctype-offset _sprite-info SpriteData-start)))
      (glVertexAttribPointer
       Index HowMany type
       #f
       (ctype-sizeof _sprite-info)
       (ctype-offset _sprite-info SpriteData-start))
      (glEnableVertexAttribArray Index)))

  (define VboId
    (u32vector-ref (glGenBuffers 1) 0))

  (glBindBuffer GL_ARRAY_BUFFER VboId)

  (define-syntax-rule
    (define-vertex-attrib-array*
      [AttribId AttribStart AttribEnd] ...)
    (begin
      (define-vertex-attrib-array AttribId AttribStart AttribEnd)
      ...))

  ;; xxx this is awkward, but ctype-layout might help?
  (define-vertex-attrib-array*
    [0  0  3]
    [1  4  7]
    [2  8  8]
    [3 10 12]
    [4 13 14]
    [5  9  9])

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
    (glActiveTexture GL_TEXTURE2)
    (glBindTexture GL_TEXTURE_2D SpriteIndexId)

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
                       (ctype-sizeof _sprite-info))
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
               (ctype-sizeof _sprite-info))
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
           _sprite-info
           (* SpriteData-count
              DrawnMult)))

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

    ;; xxx turn off this?
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

    ;; This is actually already active
    (glActiveTexture GL_TEXTURE2)
    (glBindTexture GL_TEXTURE_2D 0)
    (glActiveTexture GL_TEXTURE1)
    (glBindTexture GL_TEXTURE_2D 0)
    (glActiveTexture GL_TEXTURE0)
    (glBindTexture GL_TEXTURE_2D 0)

    (for ([i (in-range AttributeCount)])
      (glDisableVertexAttribArray i))

    (glBindVertexArray 0)

    (glUseProgram 0))

  draw)
