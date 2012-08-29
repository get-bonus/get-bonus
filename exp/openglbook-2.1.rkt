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

(define VertexShader #<<END
  #version 330

  layout(location=0) in vec4 in_Position;
  layout(location=1) in vec4 in_Color;
  out vec4 ex_Color;

  void main(void)
  {
   gl_Position = in_Position;
   ex_Color = in_Color;
  }
END
)
(define FragmentShader #<<END
  #version 330

in vec4 ex_Color;
out vec4 out_Color;

void main(void)
{
       out_Color = ex_Color;
}
END
)

(define (CreateVBO)
  (define Vertices
    (f32vector
     -0.8 -0.8 0.0 1.0
      0.0  0.8 0.0 1.0
     -0.8 -0.8 0.0 1.0))
  (define Colors
    (f32vector
     1.0 0.0 0.0 1.0
     0.0 1.0 0.0 1.0
     0.0 0.0 1.0 1.0))
  (define VaoId
    (u32vector-ref (glGenVertexArrays 1) 0))
  (glBindVertexArray VaoId)
  (define VboId
    (u32vector-ref (glGenBuffers 1) 0))
  (glBindBuffer GL_ARRAY_BUFFER VboId)
  (eprintf "~v ~v\n"
           VboId (* (ctype-sizeof _float) (f32vector-length Vertices)))
  (glBufferData GL_ARRAY_BUFFER 
                (* (ctype-sizeof _float) (f32vector-length Vertices))
                Vertices
                GL_STATIC_DRAW)
  (glVertexAttribPointer 0 4 GL_FLOAT #f 0 0)
  (glEnableVertexAttribArray 0)

  (define ColorBufferId
    (u32vector-ref (glGenBuffers 1) 0))
  (glBindBuffer GL_ARRAY_BUFFER ColorBufferId)
  (eprintf "~v ~v\n"
           ColorBufferId (* (ctype-sizeof _float) (f32vector-length Colors)))
  (glBufferData GL_ARRAY_BUFFER 
                (* (ctype-sizeof _float) (f32vector-length Colors))
                Colors
                GL_STATIC_DRAW)
  (glVertexAttribPointer 1 4 GL_FLOAT #f 0 0)
  (glEnableVertexAttribArray 1))

(define (CreateShaders)
  (define VertexShaderId (glCreateShader GL_VERTEX_SHADER))
  (glShaderSource VertexShaderId 1 (vector VertexShader)
                  (s32vector (string-length VertexShader)))
  (glCompileShader VertexShaderId)
  (define-values (VinfoLen VinfoLog)
    (glGetShaderInfoLog VertexShaderId 1024))
  (eprintf "~a\n"
           (subbytes VinfoLog 0 VinfoLen))

  (define FragmentShaderId (glCreateShader GL_FRAGMENT_SHADER))
  (glShaderSource FragmentShaderId 1 (vector FragmentShader)
                  (s32vector (string-length FragmentShader)))
  (glCompileShader FragmentShaderId)
  (define-values (infoLen infoLog)
    (glGetShaderInfoLog FragmentShaderId 1024))
  (eprintf "~a\n"
           (subbytes infoLog 0 infoLen))

  (define ProgramId (glCreateProgram))
  (glAttachShader ProgramId VertexShaderId)
  (glAttachShader ProgramId FragmentShaderId)
  (glLinkProgram ProgramId)
  (define-values (PinfoLen PinfoLog)
    (glGetProgramInfoLog ProgramId 1024))
  (eprintf "~a\n"
           (subbytes PinfoLog 0 PinfoLen))
  (glUseProgram ProgramId))

(define init? #f)
(define (init)
  (unless init?

    (printf "INFO: OpenGL Version ~a\n"
            (glGetString GL_VERSION))
    (CreateShaders)
    (CreateVBO)
    (glClearColor 1.0 1.0 1.0 0.0)

    (set! init? #t)))

(define (do-drawing w h)
  (init)

  (printf "~v\n" (list w h))

  (glViewport 0 0 w h)
  (glClear (bitwise-ior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))
  (glDrawArrays GL_TRIANGLES 0 3)

  )

