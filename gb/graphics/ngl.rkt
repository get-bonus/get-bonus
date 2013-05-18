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
    [(gl-version-at-least? (list 3 0))
     (make-draw/300 texture-atlas-path
                    texture-atlas-size
                    width height)]
    [else
     (error 'ngl "Your version of OpenGL ~a is too old to support NGL"
            (gl-version))]))

(require racket/include)

;; Old version w/o geometry shaders
(include "ngl.300.rktl")

(define-syntax-rule
  (define-draw draw
    texture-atlas-path texture-atlas-size width height
    ProgramId
    SpriteData SpriteData-count SpriteData-count:new SpriteData-components
    install-object!
    #:attrib
    ([AttribId AttribStart AttribEnd] ...)
    #:render
    (DrawType DrawnMult AttributeCount))
  (begin
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
    (define-vertex-attrib-array AttribId AttribStart AttribEnd GL_FLOAT)
    ...
    (glBindBuffer GL_ARRAY_BUFFER 0)

    (glBindVertexArray 0)

    (define (draw objects)
      (glBindVertexArray VaoId)

      (for ([i (in-range AttributeCount)])
        (glEnableVertexAttribArray i))

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

      ;; xxx it would be nice to get more consistency by counting twice
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
       DrawType 0
       (* DrawnMult drawn-count))

      (performance-log! drawn-count)

      (glPopAttrib)

      (glBindTexture GL_TEXTURE_2D 0)

      (for ([i (in-range AttributeCount)])
        (glDisableVertexAttribArray i))

      (glBindVertexArray 0)

      (glUseProgram 0))

    draw))
