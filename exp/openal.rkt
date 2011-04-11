#lang racket/base
(require (except-in racket/contract ->)
         (for-syntax racket/base)
         (prefix-in c: racket/contract)
         racket/runtime-path
         ffi/unsafe
         ffi/unsafe/objc
         mred/private/wx/cocoa/types)

(define openal (ffi-lib "OpenAL.framework/OpenAL"))
(define-runtime-path support-lib 
  "../dist/OpenALSupport/build/Release/libOpenALSupport")
(define openal-support (ffi-lib support-lib))

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
  (_fun [d : _ALCdevice]
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

(define AL_SOURCE_RELATIVE                        #x202)
(define AL_SOURCE_STATE                           #x1010)
(define AL_INITIAL                                #x1011)
(define AL_PLAYING                                #x1012)
(define AL_PAUSED                                 #x1013)
(define AL_STOPPED                                #x1014)

(define-openal alSourcei
  (_fun [source : _ALsource]
        [param : _uint]
        [value : _int]
        -> _void
        -> (check-error 'alSourcei)))
(define-openal alGetSourcei
  (_fun [source : _ALsource]
        [param : _uint]
        [value : (_ptr o _int)]
        -> _void
        -> (begin (check-error 'alGetSourcei)
                  value)))
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
        -> _void
        -> (check-error 'alSourcePlay)))
(define-openal alSourcePlayv
  (_fun [n : _uint = (vector-length b)]
        [b : (_vector i _ALsource)]
        -> _void
        -> (check-error 'alSourcePlayv)))
(define-openal alSourceStop
  (_fun [source : _ALsource]
        -> _void
        -> (check-error 'alSourceStop)))
(define-openal alSourcePause
  (_fun [source : _ALsource]
        -> _void
        -> (check-error 'alSourcePause)))
(define-openal alSourcePausev
  (_fun [n : _uint = (vector-length b)]
        [b : (_vector i _ALsource)]
        -> _void
        -> (check-error 'alSourcePausev)))

(define-openal alListener3f
  (_fun [param : _uint]
        [v1 : _float]
        [v2 : _float]
        [v3 : _float]
        -> _void
        -> (check-error 'alListener3f)))

; XXX Figure out how to handle the "scale", because the defaults may not be right for a game

; XXX These could be stricter with the allowable property names
(provide/contract
 [alGenBuffers (c:-> integer? (vectorof integer?))]
 [alBufferData/path (c:-> integer? path? void)]
 [alGenSources (c:-> integer? (vectorof integer?))]
 [alListener3f (c:-> integer? inexact? inexact? inexact? void)]
 [alGetSourcei (c:-> integer? integer? integer?)]
 [alSourceStop (c:-> integer? void)]
 [alSourcePlay (c:-> integer? void)]
 [alSourcePause (c:-> integer? void)]
 [alSourcePlayv (c:-> (vectorof integer?) void)]
 [alSourcePausev (c:-> (vectorof integer?) void)]
 [alDeleteSources (c:-> (vectorof integer?) void)]
 [alSourceb (c:-> integer? integer? boolean? void)]
 [alSourcef (c:-> integer? integer? inexact? void)]
 [alSourcei (c:-> integer? integer? integer? void)]
 [alSource3f (c:-> integer? integer? inexact? inexact? inexact? void)]
 [alcOpenDevice (c:-> (or/c #f string?) ALCdevice?)]
 [alcCreateContext (c:-> ALCdevice? ALCcontext?)]
 [alcMakeContextCurrent (c:-> ALCcontext? void)]
 [AL_POSITION integer?]
 [AL_SOURCE_STATE integer?]
 [AL_SOURCE_RELATIVE integer?]
 [AL_STOPPED integer?]
 [AL_LOOPING integer?]
 [AL_PLAYING integer?]
 [AL_GAIN integer?]
 [AL_BUFFER integer?])
