#lang racket/base
(require (except-in racket/contract ->)
         (prefix-in c: racket/contract)
         racket/runtime-path
         ffi/unsafe
         ffi/unsafe/objc
         mred/private/wx/cocoa/types         
         "openal.rkt")

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

(define (alBufferData/path b p)
  (define-values
    (size format rate data)
    (MyGetOpenALAudioData p))
  (alBufferData b format data size rate)
  (free data))

(provide/contract
 [alBufferData/path (c:-> integer? path? void)])
