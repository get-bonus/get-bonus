#lang racket/base
(require (except-in racket/contract ->)
         (prefix-in c: racket/contract)
         racket/runtime-path
         ffi/unsafe
         "openal.rkt")

(define (alBufferData/path b p)
  (eprintf "XXX alBufferData/path not implemented on Linux")
  (void))

(provide/contract
 [alBufferData/path (c:-> integer? path? void)])
