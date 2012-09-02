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

(define VertexShaderId #f)
(define FragmentShaderId #f)
(define ProgramId #f)
(define VaoId #f)
(define VboId #f)
(define ColorBufferId #f)

(define VertexShader 
  (string-append
   	"#version 330\n"

	"layout(location=0) in vec4 in_Position;\n"
	"layout(location=1) in vec4 in_Color;\n"
	"out vec4 ex_Color;\n"

	"void main(void)\n"
	"{\n"
	"	gl_Position = in_Position;\n"
	"	ex_Color = in_Color;\n"
	"}\n"
    ))
(define FragmentShader 
  (string-append
	"#version 330\n"

	"in vec4 ex_Color;\n"
	"out vec4 out_Color;\n"

	"void main(void)\n"
	"{\n"
	"	out_Color = ex_Color;\n"
	"}\n"))

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

  (glDrawArrays GL_TRIANGLES 0 6))

(define (Cleanup)
  (DestroyShaders)
  (DestroyVBO))

(define (CreateVBO)
  (define Vertices
    (f32vector
     -0.8  0.8 0.0 1.0
      0.8  0.8 0.0 1.0
     -0.8 -0.8 0.0 1.0

     -0.8 -0.8 0.0 1.0
      0.8  0.8 0.0 1.0
      0.8 -0.8 0.0 1.0))

  (define Colors
    (f32vector
     1.0 0.0 0.0 1.0
     0.0 1.0 0.0 1.0
     0.0 0.0 1.0 1.0
     
     0.0 0.0 1.0 1.0
     0.0 1.0 0.0 1.0
     1.0 1.0 1.0 1.0))

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
  (set! VertexShaderId (glCreateShader GL_VERTEX_SHADER))
  (glShaderSource VertexShaderId 1 (vector VertexShader)
                  (s32vector))
  (glCompileShader VertexShaderId)  

  (set! FragmentShaderId (glCreateShader GL_FRAGMENT_SHADER))
  (glShaderSource FragmentShaderId 1 (vector FragmentShader)
                  (s32vector))
  (glCompileShader FragmentShaderId)  

  (set! ProgramId (glCreateProgram))
  (glAttachShader ProgramId VertexShaderId)
  (glAttachShader ProgramId FragmentShaderId)
  (glLinkProgram ProgramId)
  
  (glUseProgram ProgramId))

(define (DestroyShaders)
  (glUseProgram 0)

  (glDetachShader ProgramId VertexShaderId)
  (glDetachShader ProgramId FragmentShaderId)

  (glDeleteShader FragmentShaderId)
  (glDeleteShader VertexShaderId)  

  (glDeleteProgram ProgramId))
