#lang racket/base
(require gb/lib/ffi
         ffi/unsafe
         ffi/unsafe/define)

(define-ffi-definer define-glut (ffi-lib "libglut"))

(d (glut
    [glutInit
     ((_ptr i _int)
      (_vector i _string))
     (_void)]
    [glutInitContextVersion
     (_int _int)
     (_void)]
    [glutInitContextFlags
     (_int)
     (_void)]
    [glutInitContextProfile
     (_int)
     (_void)]
    [glutSetOption
     (_uint _int)
     (_void)]
    [glutMainLoop
     ()
     (_void)]
    [glutInitWindowSize
     (_int _int)
     (_void)]
    [glutInitDisplayMode
     (_uint)
     (_void)]
    [glutCreateWindow
     (_string)
     (_int)]
    [glutReshapeFunc
     ((_fun #:keep #t
            _int _int -> _void))
     (_void)]
    [glutDisplayFunc
     ((_fun #:keep #t
            -> _void))
     (_void)]
    [glutIdleFunc
     ((_fun #:keep #t
            -> _void))
     (_void)]
    [glutTimerFunc
     (_uint (_fun #:keep #t _int -> _void) _int)
     (_void)]
    [glutCloseFunc
     ((_fun #:keep #t -> _void))
     (_void)]
    [glutSwapBuffers
     ()
     (_void)]
    [glutPostRedisplay
     ()
     (_void)]
    [glutSetWindowTitle
     (_string)
     (_void)]))

(define-#define
  define  GLUT_DEBUG                         #x0001
  define  GLUT_FORWARD_COMPATIBLE            #x0002
  define GLUT_CORE_PROFILE                   #x0001
  define   GLUT_COMPATIBILITY_PROFILE          #x0002
  define GLUT_ACTION_EXIT                         0
  define GLUT_ACTION_GLUTMAINLOOP_RETURNS         1
  define GLUT_ACTION_CONTINUE_EXECUTION           2
  define  GLUT_INIT_STATE                    #x007C

  define  GLUT_ACTION_ON_WINDOW_CLOSE        #x01F9

  define  GLUT_WINDOW_BORDER_WIDTH           #x01FA
  define  GLUT_WINDOW_HEADER_HEIGHT          #x01FB

  define  GLUT_VERSION                       #x01FC

  define  GLUT_RENDERING_CONTEXT             #x01FD
  define  GLUT_DIRECT_RENDERING              #x01FE

  define  GLUT_FULL_SCREEN                   #x01FF
  define  GLUT_RGB                           #x0000
  define  GLUT_RGBA                          #x0000
  define  GLUT_INDEX                         #x0001
  define  GLUT_SINGLE                        #x0000
  define  GLUT_DOUBLE                        #x0002
  define  GLUT_ACCUM                         #x0004
  define  GLUT_ALPHA                         #x0008
  define  GLUT_DEPTH                         #x0010
  define  GLUT_STENCIL                       #x0020
  define  GLUT_MULTISAMPLE                   #x0080
  define  GLUT_STEREO                        #x0100
  define  GLUT_LUMINANCE                     #x0200
  )
