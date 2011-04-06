#lang racket/base
(require ffi/unsafe
         ffi/unsafe/objc
         mred/private/wx/cocoa/types)

(define openal (ffi-lib "OpenAL.framework/OpenAL"))
(define openal-support (ffi-lib "libOpenALSupport"))

(define-syntax-rule (define-ffi-definer define-openal define-openal* openal)
  (begin
    (define-syntax-rule (define-openal* (id ffi-id) ty)
      (define id (get-ffi-obj 'ffi-id openal ty)))
    (define-syntax-rule (define-openal id ty)
      (define-openal* (id id) ty))))
(define-ffi-definer define-support define-support* openal-support)
(define-ffi-definer define-openal define-openal* openal)

(define-openal alGetError
  (_fun -> _uint))
(define AL_NO_ERROR 0)
(define (check-error i)
  (define ec (alGetError))
  (unless (equal? ec AL_NO_ERROR)
    (error i "error ~a" ec)))

(define-cpointer-type _ALCdevice)
(define-openal alcOpenDevice
  (_fun [devicename : _string]
        -> [d : _ALCdevice/null]
        -> (or d
               (error 'alcOpenDevice "error opening device ~a" devicename))))
(define-openal alcCloseDevice
  (_fun [d : _ALCdevice]
        -> [e : _bool]
        -> (unless e
             (error 'alcCloseDevice "error closing device ~a" d))))

(define-cpointer-type _ALCcontext)
(define-openal alcCreateContext
  (_fun [device : _ALCdevice]
        [p : _pointer = #f]
        -> [c : _ALCcontext/null]
        -> (or c
               (error 'alcCreateContext "error creating context from device ~a" d))))
(define-openal alcMakeContextCurrent
  (_fun [c : _ALCcontext]
        -> [e : _bool]
        -> (unless e
             (error 'alcMakeContextCurrent "error making context current ~a" c))))
(define-openal alcDestroyContext
  (_fun [c : _ALCcontext]
        -> _void))

(define _ALbuffer _uint)
(define-openal alGenBuffers
  (_fun [n : _uint] ; XXX ALsizei
        [b : (_vector o _ALbuffer n)]
        -> _void
        -> (begin (check-error 'alGenBuffers)
                  b)))
(define-openal alDeleteBuffers
  (_fun (b) ::
        [n : _uint = (vector-length b)]
        [b : (_vector i _ALbuffer)]
        -> _void
        -> (check-error 'alDeleteBuffers)))

(define-support MyGetOpenALAudioData
  (_fun (p) ::
        [s : _NSString = 
           (path->string 
            (path->complete-path p))]
        ; XXX types might be wrong
        [size : (_ptr o _uint)]
        [format : (_ptr o _uint)]
        [rate : (_ptr o _uint)]
        ->
        [data : _pointer]
        ->
        (if data
            (values size format rate data)
            (error 'MyGetOpenALAudioData "failed to load audio data from ~a" p))))

(define-openal alBufferData
  (_fun [b : _ALbuffer]
        [format : _uint]
        [data : _pointer]
        [size : _uint]
        [freq : _uint]
        -> _void
        -> (check-error 'alBufferData)))

(define (alBufferData/path b p)
  (define-values
    (size format rate data)
    (MyGetOpenALAudioData p))
  (alBufferData b format data size rate)
  (free data))

(define _ALsource _uint)
(define-openal alGenSources
  (_fun [n : _uint] ; XXX ALsizei
        [b : (_vector o _ALsource n)]
        -> _void
        -> (begin (check-error 'alGenSources)
                  b)))
(define-openal alDeleteSources
  (_fun (b) ::
        [n : _uint = (vector-length b)]
        [b : (_vector i _ALsource)]
        -> _void
        -> (check-error 'alDeleteSources)))

(define AL_POSITION #x1004)
(define AL_LOOPING #x1007)
(define AL_BUFFER #x1009)
(define AL_GAIN #x100A)

(define-openal alSourcei
  (_fun [source : _ALsource]
        [param : _uint]
        [value : _int]
        -> _void
        -> (check-error 'alSourcei)))
(define-openal* (alSourceb alSourcei)
  (_fun [source : _ALsource]
        [param : _uint]
        [value : _bool]
        -> _void
        -> (check-error 'alSourceb)))
(define-openal alSourcef
  (_fun [source : _ALsource]
        [param : _uint]
        [value : _float]
        -> _void
        -> (check-error 'alSourcef)))
(define-openal alSource3f
  (_fun [source : _ALsource]
        [param : _uint]
        [v1 : _float]
        [v2 : _float]
        [v3 : _float]
        -> _void
        -> (check-error 'alSource3f)))

(define-openal alSourcePlay
  (_fun [source : _ALsource]
        -> _void))
(define-openal alSourceStop
  (_fun [source : _ALsource]
        -> _void))
(define-openal alSourcePause
  (_fun [source : _ALsource]
        -> _void))

;; Example

; XXX Deal with listeners
; XXX Figure out how to handle the "scale", because the defaults may not be right for a game
; XXX Figure out a functional interface

(require racket/runtime-path)
(define-runtime-path resource-path "../resources")

(define d (alcOpenDevice #f))
(define ctxt (alcCreateContext d))
(alcMakeContextCurrent ctxt)

(define b (alGenBuffers 2))
(alBufferData/path 
 (vector-ref b 0)
 (build-path resource-path 
             "SMB-1-1.mp3"))
(alBufferData/path
 (vector-ref b 1)
 (build-path resource-path 
             "SMB-SE-Jump.wav"))

(define s (alGenSources 2))

(alSourcef (vector-ref s 0) AL_GAIN 0.8)
(alSourceb (vector-ref s 0) AL_LOOPING #t)
(alSourcei (vector-ref s 0) AL_BUFFER (vector-ref b 0))

(alSourcei (vector-ref s 1) AL_BUFFER (vector-ref b 1))

(alSourcePlay (vector-ref s 0))

(for ([i (in-range 10)])
  (alSource3f (vector-ref s 1) AL_POSITION
              (+ -5.0 i) 0.0 0.0)
  (alSourcePlay (vector-ref s 1))
  (sleep 5))

(alSourceStop (vector-ref s 0))

(alDeleteSources s)
(alDeleteBuffers b)
(alcDestroyContext ctxt)
(alcCloseDevice d)