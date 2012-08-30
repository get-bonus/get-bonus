#lang racket/base
(require gb/lib/ffi
         ffi/unsafe
         ffi/unsafe/define)

(define libglew (ffi-lib "libGLEW"))

(define-ffi-definer define-glew libglew)

(define-c glewExperimental libglew _bool)
(provide glewExperimental)

(d (glew
    [glewInit
     ()
     (_uint)]
    [glewGetErrorString
     (_uint)
     (_string)]))

(fake )

