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
                  _sint32
                  _uint32
                  _sint16
                  _uint16
                  _sint8
                  _uint8)
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

(define-cstruct _sprite-info
  ([x _float]     ;; 0
   [y _float]     ;; 1
   [hw _float]    ;; 2
   [hh _float]    ;; 3
   [r _uint8]     ;; 4
   [g _uint8]     ;; 5
   [b _uint8]     ;; 6
   [a _uint8]     ;; 7
   [mx _float]    ;; 8
   [my _float]    ;; 9
   [theta _float] ;; 10

   ;; xxx This is a hack because we need to ensure we are aligned for
   ;; OpenGL, so we're ignoring _palette and _sprite-index. At this
   ;; moment, pal is "too" large and spr is just right. When we have
   ;; more than 65k palettes or 65k sprites, there will be a
   ;; problem. (BTW, because of normal alignment, if we change pal to
   ;; just be a byte, it will still take up the same amount of space
   ;; total.)
   [pal _uint16]   ;; 11
   [spr _uint16]   ;; 12

   [horiz _sint8]  ;; 13
   [vert _sint8])) ;; 14

(define (create-sprite-info x y hw hh r g b a spr pal mx my theta)
  (make-sprite-info x y hw hh
                    r g b a
                    mx my theta
                    pal spr
                    0 0))

(module+ test
  (define vert-size (ctype-sizeof _sprite-info))
  (eprintf "One vert is ~a bytes\n" vert-size)
  (eprintf "One sprite is ~a verts\n" DrawnMult)
  (eprintf "One sprite is ~a bytes\n" (* DrawnMult vert-size))
  (eprintf "One sprite @ 60 FPS is ~a bytes per second\n" (* 60 DrawnMult vert-size))
  (eprintf "Intel HD Graphics 4000 would give ~a sprites at 60 FPS (considering only memory)\n"
           (real->decimal-string
            (/ (* 25.6 1024 1024 1024)
               (* 60 DrawnMult vert-size)))))

(define ctype-name->bytes
  (match-lambda
   ['uint8 1]
   ['int8 1]
   ['int16 2]
   ['uint16 2]
   ['int32 4]
   ['uint32 4]
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
   ['uint8 (values #t GL_UNSIGNED_BYTE)]
   ['int8 (values #t GL_BYTE)]
   ['uint16 (values #t GL_UNSIGNED_SHORT)]
   ['int16 (values #t GL_SHORT)]
   ['uint32 (values #t GL_UNSIGNED_INT)]
   ['int32 (values #t GL_INT)]
   ['float (values #f GL_FLOAT)]))

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
    (point-install! -1 +1 0)
    (point-install! +1 +1 1 4)
    (point-install! -1 -1 2 3)
    (point-install! +1 -1 5))

  ;; Create Shaders
  (define ProgramId (glCreateProgram))
  (glBindAttribLocation ProgramId 0 "in_Position")
  (glBindAttribLocation ProgramId 1 "in_iColor")
  (glBindAttribLocation ProgramId 2 "in_iTexIndex")
  (glBindAttribLocation ProgramId 3 "in_Transforms")
  (glBindAttribLocation ProgramId 4 "in_VertexSpecification")
  (glBindAttribLocation ProgramId 5 "in_iPalette")

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

  (define (glVertexAttribIPointer* index size type normalized stride pointer)
    (glVertexAttribIPointer index size type stride pointer))

  (define-syntax-rule
    (define-vertex-attrib-array
      Index SpriteData-start SpriteData-end)
    (begin
      (define-values (int? type)
        (ctype->gltype (ctype-range-type _sprite-info SpriteData-start SpriteData-end)))
      (define byte-offset
        (ctype-offset _sprite-info SpriteData-start))
      (define HowMany
        (add1 (- SpriteData-end SpriteData-start)))
      (when debug?
        (eprintf "~v\n"
                 `(glVertexAttribPointer
                   ,Index ,HowMany ,type
                   #f
                   ,(ctype-sizeof _sprite-info)
                   ,byte-offset)))
      ((if int? glVertexAttribIPointer* glVertexAttribPointer)
       Index HowMany type
       #f
       (ctype-sizeof _sprite-info)
       byte-offset)
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

  ;; XXX This is gross that I can't use names of either the attribute
  ;; tables or the start/end
  (define-vertex-attrib-array*
    [0  0  3] ;; x--hh
    [1  4  7] ;; r--a
    [2 12 12] ;; spr
    [3  8 10] ;; mx--theta
    [4 13 14] ;; horiz--vert
    [5 11 11]) ;; pal

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
