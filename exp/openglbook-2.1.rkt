#lang racket/base
(require ffi/vector
         ffi/unsafe/cvector
         ffi/unsafe
         racket/runtime-path
         xml
         cg
         #;glew
         freeglut
         #;RacketGL/glew
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
  (Initialize)
  (glutMainLoop)
  (exit 0))

(define (Initialize)
  (InitWindow)

  (printf "INFO: OpenGL Version ~a\n"
          (glGetString GL_VERSION))  

  (CreateShaders)
  (CreateVBO)

  (glClearColor 0.0 0.0 0.0 0.0))

(define (InitWindow)
  (glutInit (vector-length (current-command-line-arguments))
            (current-command-line-arguments))

  (glutInitContextVersion 3 3)
  (glutInitContextFlags GLUT_FORWARD_COMPATIBLE)
  (glutInitContextProfile GLUT_CORE_PROFILE)

  (glutSetOption GLUT_ACTION_ON_WINDOW_CLOSE
                 GLUT_ACTION_GLUTMAINLOOP_RETURNS)

  (glutInitWindowSize CurrentWidth CurrentHeight)

  (glutInitDisplayMode (bitwise-ior GLUT_DEPTH GLUT_DOUBLE GLUT_RGBA))

  (set! WindowHandle (glutCreateWindow WINDOW_TITLE_PREFIX))

  (when (< WindowHandle 1)
    (error 'glutCreateWindow "ERROR: Could not create a new rendering window."))

  (glutReshapeFunc ResizeFunction)
  (glutDisplayFunc RenderFunction)
  (glutIdleFunc IdleFunction)
  (glutTimerFunc 0 TimerFunction 0)
  (glutCloseFunc Cleanup))

(define (ResizeFunction Width Height)
  (set! CurrentWidth Width)
  (set! CurrentHeight Height)
  (glViewport 0 0 CurrentWidth CurrentHeight))

(define tCount 0)
(define (RenderFunction)
  (set! FrameCount (add1 FrameCount))
  (set! tCount (add1 tCount))
  (when (= tCount 60)
    (exit 1))

  (glClear (bitwise-ior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))

  (glDrawArrays GL_TRIANGLES 0 3)

  (glutSwapBuffers)
  (glutPostRedisplay))

(define (IdleFunction)
  (glutPostRedisplay))

(define (TimerFunction Value)
  (unless (zero? Value)
    (define TempString
      (format "~a: ~a Frames Per Second @ ~a x ~a"
              WINDOW_TITLE_PREFIX
              (* 4 FrameCount)
              CurrentWidth
              CurrentHeight))

    (glutSetWindowTitle TempString))

  (set! FrameCount 0)
  (glutTimerFunc 250 TimerFunction 1))

(define (Cleanup)
  (DestroyShaders)
  (DestroyVBO))

(require file/sha1)
(define (debug-f32vector v)
  (define bs
    (make-sized-byte-string
     (f32vector->cpointer v)
     (* (ctype-sizeof _float) (f32vector-length v))))
  (debug-f32vector/bs bs)
  v)
(define (debug-f32vector/bs bs)  
  (for ([pt (in-range 3)])
    (for ([fl (in-range 4)])
      (display
       (bytes->hex-string
        (subbytes bs 
                  (+ (* pt 4 4)
                     (* fl 4))
                  (+ (* pt 4 4)
                     (* fl 4)
                     4))))
      (printf " "))
    (printf "\n"))
  (printf "\n\n"))

(define (CreateVBO)
  (define Vertices
    (f32vector
     -0.8 -0.8 0.0 1.0
      0.0  0.8 0.0 1.0
      0.8 -0.8 0.0 1.0))

  (define Colors
    (f32vector
     1.0 0.0 0.0 1.0
     0.0 1.0 0.0 1.0
     0.0 0.0 1.0 1.0))

  (set! VaoId
    (u32vector-ref (glGenVertexArrays 1) 0))
  (glBindVertexArray VaoId)

  (set! VboId
    (u32vector-ref (glGenBuffers 1) 0))
  (glBindBuffer GL_ARRAY_BUFFER VboId)
  (eprintf "~v ~v\n"
           VboId
           (gl-vector-sizeof Vertices))
  (glBufferData GL_ARRAY_BUFFER
                (gl-vector-sizeof Vertices)
                (debug-f32vector Vertices)
                GL_STATIC_DRAW)
  (glVertexAttribPointer 0 4 GL_FLOAT #f 0 0)
  (glEnableVertexAttribArray 0)

  (set! ColorBufferId
    (u32vector-ref (glGenBuffers 1) 0))
  (glBindBuffer GL_ARRAY_BUFFER ColorBufferId)
  (eprintf "~v ~v\n"
           ColorBufferId
           (gl-vector-sizeof Colors))
  (glBufferData GL_ARRAY_BUFFER
                (gl-vector-sizeof Colors)
                (debug-f32vector Colors)
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
