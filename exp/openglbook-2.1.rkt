#lang racket/base
(require racket/gui
         ffi/vector
         ffi/unsafe/cvector
         racket/runtime-path
         xml
         cg
         (planet stephanh/RacketGL/rgl))

(define WINDOW_TITLE_PREFIX "Chapter 2")

(define CurrentWidth 800)
(define CurrentHeight 600)
(define WindowHandle 0)

(define FrameCount 0)

(define VaoId #f)
(define VboId #f)
(define ColorBufferId #f)

(define-runtime-path cg-path "../gb/graphics/ngl.cg")

(define (check-for-cgError situation)
  (define-values (err str) (cgGetLastErrorString))
  (unless (= err CG_NO_ERROR)
    (if (= err CG_COMPILER_ERROR)
      (error 'cg "~a: ~a: ~a\n~a" situation err str (cgGetLastListing id:cgContext))
      (error 'cg "~a: ~a: ~a" situation err str))))

(define id:cgContext #f)
(define id:cgVertexProfile #f)
(define id:cgVertexProgram #f)
(define id:cgFragmentProfile #f)
(define id:cgFragmentProgram #f)
(define id:cgGeometryProfile #f)
(define id:cgGeometryProgram #f)

(module+ main
  (define frame
    (new frame% [label "Example"]))
  (define this-canvas%
    (class canvas%
      (define/override (on-size width height)
        (define dc
          (send this get-dc))
        (define gc
          (send dc get-gl-context))
        (send gc
              call-as-current
              (λ ()
                (do-drawing width height)
                (send gc swap-buffers))))

      (super-new)))

  (define config
    (new gl-config%))
  (send config set-double-buffered #t)

  (define c
    (new this-canvas%
         [parent frame]
         [gl-config config]
         [style '(gl no-autoclear)]))

  (send frame show #t))

(define init? #f)
(define (do-drawing w h)
  (unless init?
    (Initialize)
    (set! init? #t))

  (ResizeFunction w h)
  (RenderFunction))

(define (Initialize)
  (printf "INFO: OpenGL Version ~a\n"
          (glGetString GL_VERSION))  

  (CreateShaders)
  (CreateVBO)

  (glClearColor 0.0 0.0 0.0 0.0))

(define (ResizeFunction Width Height)
  (set! CurrentWidth Width)
  (set! CurrentHeight Height)
  (glViewport 0 0 CurrentWidth CurrentHeight))

(define (RenderFunction)
  (set! FrameCount (add1 FrameCount))

  (glClear (bitwise-ior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))

  (glDrawArrays GL_TRIANGLE_STRIP 0
                (/ (f32vector-length Vertices) 4)))

(define (Cleanup)  
  (DestroyVBO))

(define Vertices
  (f32vector
   -0.8  0.8 0.0 1.0
    0.8  0.8 0.0 1.0
   -0.8 -0.8 0.0 1.0
    0.8 -0.8 0.0 1.0))

(define Colors
  (f32vector
   1.0 0.0 0.0 1.0
   0.0 1.0 0.0 1.0
   0.0 0.0 1.0 1.0
   1.0 1.0 1.0 1.0))

(define (CreateVBO)  
  (set! VaoId
    (u32vector-ref (glGenVertexArrays 1) 0))
  (glBindVertexArray VaoId)

  (set! VboId
    (u32vector-ref (glGenBuffers 1) 0))
  (glBindBuffer GL_ARRAY_BUFFER VboId)
    (glBufferData GL_ARRAY_BUFFER
                (gl-vector-sizeof Vertices)
                Vertices
                GL_STATIC_DRAW)
  (glVertexAttribPointer 0 4 GL_FLOAT #f 0 0)
  (glEnableVertexAttribArray 0)

  (set! ColorBufferId
    (u32vector-ref (glGenBuffers 1) 0))
  (glBindBuffer GL_ARRAY_BUFFER ColorBufferId)  
  (glBufferData GL_ARRAY_BUFFER
                (gl-vector-sizeof Colors)
                Colors
                GL_STATIC_DRAW)
  (glVertexAttribPointer 1 4 GL_FLOAT #f 0 0)
  (glEnableVertexAttribArray 1))

(define (DestroyVBO)
  (glDisableVertexAttribArray 1)
  (glDisableVertexAttribArray 0)

  (glBindBuffer GL_ARRAY_BUFFER 0)

  (glDeleteBuffers 1 (u32vector ColorBufferId))
  (glDeleteBuffers 1 (u32vector VboId))

  (glBindVertexArray 0)
  (glDeleteVertexArrays 1 (u32vector VaoId)))

(define (CreateShaders)
  (set! id:cgContext (cgCreateContext))
  (check-for-cgError "creating context")
  (cgGLSetDebugMode CG_TRUE)
  (cgSetParameterSettingMode id:cgContext CG_DEFERRED_PARAMETER_SETTING)

  (set! id:cgVertexProfile (cgGLGetLatestProfile CG_GL_VERTEX))
  (cgGLSetOptimalOptions id:cgVertexProfile)
  (check-for-cgError "selecting vertex profile")
  (set! id:cgVertexProgram
        (cgCreateProgramFromFile
         id:cgContext CG_SOURCE
         cg-path id:cgVertexProfile
         "vertex_main"
         #f))
  (check-for-cgError "creating vertex program from file")
  (cgGLLoadProgram id:cgVertexProgram)
  (check-for-cgError "loading vertex program")  

  (set! id:cgFragmentProfile (cgGLGetLatestProfile CG_GL_FRAGMENT))
  (check-for-cgError "selecting fragment profile")
  (cgGLSetOptimalOptions id:cgFragmentProfile)
  (set! id:cgFragmentProgram
        (cgCreateProgramFromFile
         id:cgContext CG_SOURCE
         cg-path id:cgFragmentProfile
         "fragment_main"
         #f))
  (check-for-cgError "creating fragment program from file")
  (cgGLLoadProgram id:cgFragmentProgram)
  (check-for-cgError "loading fragment program")

  (set! id:cgGeometryProfile (cgGLGetLatestProfile CG_GL_GEOMETRY))
  (check-for-cgError "selecting geometry profile")
  (cgGLSetOptimalOptions id:cgGeometryProfile)
  (set! id:cgGeometryProgram
        (cgCreateProgramFromFile
         id:cgContext CG_SOURCE
         cg-path id:cgGeometryProfile
         "geometry_main"
         #f))
  (check-for-cgError "creating geometry program from file")
  (cgGLLoadProgram id:cgGeometryProgram)
  (check-for-cgError "loading geometry program")

  (cgGLBindProgram id:cgVertexProgram)
  (check-for-cgError "binding vertex program")
  (cgGLEnableProfile id:cgVertexProfile)
  (check-for-cgError "enabling vertex profile")
  (cgGLBindProgram id:cgFragmentProgram)
  (check-for-cgError "binding fragment program")
  (cgGLEnableProfile id:cgFragmentProfile)
  (check-for-cgError "enabling fragment profile")
  (cgGLBindProgram id:cgGeometryProgram)
  (check-for-cgError "binding geometry program")
  (cgGLEnableProfile id:cgGeometryProfile)
  (check-for-cgError "enabling geometry profile"))
