#lang racket/gui
(require ffi/vector
         ffi/unsafe/cvector
         ffi/unsafe
         racket/runtime-path
         xml
         cg
         (planet stephanh/RacketGL/rgl))

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
                (do-drawing (* 1. width) (* 1. height))
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

(define (f32vector-set*! vec offset . vs)
  (for ([v (in-list vs)]
        [i (in-naturals)])
    (cvector-set! vec (+ offset i) v)))
(define (cpointer->f32vector cptr)
  (make-cvector* cptr _float (* how-many-objects how-many-verts-per-obj how-many-elements-per-vert)))

(define-runtime-path cg-path "../gb/graphics/ngl.cg")

(define (check-for-cgError situation)
  (define-values (err str) (cgGetLastErrorString))
  (unless (= err CG_NO_ERROR)
    (if (= err CG_COMPILER_ERROR)
      (error 'cg "~a: ~a: ~a\n~a" situation err str (cgGetLastListing id:cgContext))
      (error 'cg "~a: ~a: ~a" situation err str))))

(define id:vbo #f)
(define id:cgContext #f)
(define id:cgVertexProfile #f)
(define id:cgVertexProgram #f)
(define id:cgFragmentProfile #f)
(define id:cgFragmentProgram #f)
(define id:cgGeometryProfile #f)
(define id:cgGeometryProgram #f)
(define id:cgVertexParam_modelViewProj #f)

(define how-many-objects 3)
(define how-many-verts-per-obj 6)
(define how-many-elements-per-vert (+ 4 4))

(define init? #f)
(define (init)
  (unless init?

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

    (set! id:cgVertexParam_modelViewProj
          (cgGetNamedParameter id:cgVertexProgram "modelViewProj"))
    (check-for-cgError "could not get modelViewProj parameter")

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

    (set! id:vbo (u32vector-ref (glGenBuffers 1) 0))
    (glBindBuffer GL_ARRAY_BUFFER id:vbo)
    (glBufferData GL_ARRAY_BUFFER (* (* how-many-objects how-many-verts-per-obj how-many-elements-per-vert) (ctype-sizeof _float)) #f GL_STREAM_DRAW)
    (glBindBuffer GL_ARRAY_BUFFER 0)

    (set! init? #t)))

(define (do-drawing w h)
  (init)

  (glPushAttrib GL_CURRENT_BIT)
  (glPushMatrix)

  (glClearColor 1. 1. 1. 1.)
  (glClear (bitwise-ior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))

  (glViewport 0 0 (inexact->exact w) (inexact->exact h))
  (glMatrixMode GL_PROJECTION)
  (glLoadIdentity)
  (glOrtho (* -1 w) (* 1 w) 
           (* -1 h) (* 1 h)
           0. -10.)
  (glMatrixMode GL_MODELVIEW)
  (glLoadIdentity)
  ;;(glEnable GL_DEPTH_TEST)
  ;;(glDepthFunc GL_LEQUAL)

  (glBindBuffer GL_ARRAY_BUFFER id:vbo)
  (define ptr (cpointer->f32vector (glMapBuffer GL_ARRAY_BUFFER GL_WRITE_ONLY)))
  (f32vector-set*! ptr  0 
                   ;; X        Y        Z   W  R  G  B  A
                            0. (/ h 2.) -10. 1. 1. 0. 0. 1.
                      (/ w 2.) (/ h 2.) -10. 1. 1. 0. 0. 1.
                            0.       0. -10. 1. 1. 0. 0. 1.
                            0.       0. -10. 1. 1. 0. 0. 1.
                      (/ w 2.) (/ h 2.) -10. 1. 1. 0. 0. 1.
                      (/ w 2.)       0. -10. 1. 1. 0. 0. 1.

                            0. (/ h 4.)  -5. 1. 0. 0. 1. 1.
                      (/ w 4.) (/ h 4.)  -5. 1. 0. 0. 1. 1.
                            0.       0.  -5. 1. 0. 0. 1. 1.
                            0.       0.  -5. 1. 0. 0. 1. 1.
                      (/ w 4.) (/ h 4.)  -5. 1. 0. 0. 1. 1.
                      (/ w 4.)       0.  -5. 1. 0. 0. 1. 1.

                            0. (/ h 8.)  0. 1. 0. 1. 0. 1.
                      (/ w 8.) (/ h 8.)  0. 1. 0. 1. 0. 1.
                            0.       0.  0. 1. 0. 1. 0. 1.
                            0.       0.  0. 1. 0. 1. 0. 1.
                      (/ w 8.) (/ h 8.)  0. 1. 0. 1. 0. 1.
                      (/ w 8.)       0.  0. 1. 0. 1. 0. 1.
                      )
  (glUnmapBuffer GL_ARRAY_BUFFER)

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
  (check-for-cgError "enabling geometry profile")
  (glBindBuffer GL_ARRAY_BUFFER id:vbo)  
  (glEnableClientState GL_VERTEX_ARRAY)
  (glEnableClientState GL_COLOR_ARRAY)
  (glVertexPointer 4 GL_FLOAT (* 4 (ctype-sizeof _float)) 0)
  (glColorPointer  4 GL_FLOAT (* 4 (ctype-sizeof _float)) (* 4 (ctype-sizeof _float)))
  (glShadeModel GL_FLAT)

  (cgGLSetStateMatrixParameter id:cgVertexParam_modelViewProj
                               CG_GL_MODELVIEW_PROJECTION_MATRIX
                               CG_GL_MATRIX_IDENTITY)
  (glDrawArrays GL_TRIANGLES 0 (* how-many-verts-per-obj how-many-objects))

  (glDisableClientState GL_VERTEX_ARRAY)
  (glDisableClientState GL_COLOR_ARRAY)
  (glBindBuffer GL_ARRAY_BUFFER 0)
  ;;(cgGLDisableProfile id:cgVertexProfile)
  (check-for-cgError "disabling vertex profile")
  ;;(cgGLDisableProfile id:cgFragmentProfile)
  (check-for-cgError "disabling fragment profile")
  ;;(cgGLDisableProfile id:cgGeometryProfile)
  (check-for-cgError "disabling geometry profile")

  (glPopMatrix)
  (glPopAttrib))

