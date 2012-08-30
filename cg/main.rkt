#lang racket/base
(require gb/lib/ffi
         ffi/unsafe
         ffi/unsafe/define)

(define-ffi-definer define-cg (ffi-lib "libCg"))
(define-ffi-definer define-cgGL (ffi-lib "libCgGL"))

(define-cpointer-type* _CGcontext _CGprofile _CGprogram _CGparameter)

(define _CGbool _bool)
(define* CG_TRUE #t)
(define* CG_FALSE #t)

(define _CGenum _uint32)
(define* CG_SOURCE                             4112)
(define* CG_DEFERRED_PARAMETER_SETTING         4133)

(define _CGGLenum _uint32)
(define-enumy
  CG_GL_MATRIX_IDENTITY             = 0
  CG_GL_MATRIX_TRANSPOSE            = 1
  CG_GL_MATRIX_INVERSE              = 2
  CG_GL_MATRIX_INVERSE_TRANSPOSE    = 3
  CG_GL_MODELVIEW_MATRIX            = 4
  CG_GL_PROJECTION_MATRIX           = 5
  CG_GL_TEXTURE_MATRIX              = 6
  CG_GL_MODELVIEW_PROJECTION_MATRIX = 7
  CG_GL_VERTEX                      = 8
  CG_GL_FRAGMENT                    = 9
  CG_GL_GEOMETRY                    = 10
  CG_GL_TESSELLATION_CONTROL        = 11
  CG_GL_TESSELLATION_EVALUATION     = 12)

(define _CGerror _uint32)
(define* CG_NO_ERROR 0)
(define* CG_COMPILER_ERROR 1)

(d (cg
    [cgCreateContext
     ()
     (_CGcontext/null)]
    [cgSetParameterSettingMode
     (_CGcontext _CGenum) (_void)]
    [cgCreateProgramFromFile
     (_CGcontext _CGenum _path _CGprofile _string _pointer)
     (_CGprogram/null)]
    [cgGetLastErrorString
     ((err : (_ptr o _CGerror)))
     ((str : _string) ->
      (values err str))]
    [cgGetLastListing
     (_CGcontext)
     (_string)]
    [cgGetNamedParameter
     (_CGprogram _string)
     (_CGparameter)])
   (cgGL
    [cgGLSetDebugMode (_CGbool) (_void)]
    [cgGLGetLatestProfile (_CGGLenum) (_CGprofile/null)]
    [cgGLSetOptimalOptions (_CGprofile) (_void)]
    [cgGLLoadProgram (_CGprogram) (_void)]
    [cgGLBindProgram (_CGprogram) (_void)]
    [cgGLEnableProfile (_CGprofile) (_void)]
    [cgGLDisableProfile (_CGprofile) (_void)]
    [cgGLSetStateMatrixParameter
     (_CGparameter _CGGLenum _CGGLenum) (_void)]))

