#lang racket/base
(require racket/runtime-path
         racket/file
         ffi/vector
         (planet stephanh/RacketGL/rgl))

(define-syntax-rule (define-shader-source id path)
  (begin (define-runtime-path id-path path)
         (define id (file->string id-path))))

(define (print-shader-log glGetShaderInfoLog shader-name shader-id)
    (define-values (infoLen infoLog)
      (glGetShaderInfoLog shader-id 1024))
    (unless (zero? infoLen)
      (eprintf "~a: ~a\n"
               shader-name
               (subbytes infoLog 0 infoLen))
      (exit 1)))

(define-syntax-rule
    (define&compile-shader VertexShaderId
      GL_VERTEX_SHADER
      ProgramId VertexShader)
    (begin (define VertexShaderId (glCreateShader GL_VERTEX_SHADER))
           (glShaderSource VertexShaderId 1 (vector VertexShader)
                           (s32vector))
           (glCompileShader VertexShaderId)
           (print-shader-log glGetShaderInfoLog 'VertexShader VertexShaderId)
           (glAttachShader ProgramId VertexShaderId)))

(provide (all-defined-out)
         (for-syntax #%datum))
