#lang racket/base
(require racket/contract
         racket/match
         racket/list
         gb/data/psn
         gb/audio/openal
         gb/audio/openal-path)

; xxx gc buffers some how
(struct audio (buffer))
(define (path->audio p)
  (define b (vector-ref (alGenBuffers 1) 0))
  (alBufferData/path b p)
  (audio b))

(struct sound-state (audio posn gain relative? looping? paused?))

(define sound-state/c
  (or/c #f sound-state?))
(define sound/c
  (-> any/c sound-state/c))
(define sound-scape/c
  (listof sound/c))

(define (background a-f 
                    #:gain [gain 1.0]
                    #:pause-f [pause-f (λ (x) #f)])
  (λ (w)
    (sound-state (a-f w) (psn 0.0 0.0) gain #t #t (pause-f w))))
(define (sound-at a p
                  #:gain [gain 1.0]
                  #:looping? [looping? #f] 
                  #:pause-f [pause-f (λ (x) #f)])
  (λ (w)
    (sound-state a p gain #f looping? (pause-f w))))
(define (sound-on a p-f 
                  #:gain [gain 1.0]
                  #:looping? [looping? #f]
                  #:pause-f [pause-f (λ (x) #f)])
  (λ (w)
    (sound-state a (p-f w) gain #f looping? (pause-f w))))
(define (sound-until s until-f)
  (λ (w)
    (if (until-f w)
        #f
        (s w))))

(struct source (srci old-state update-f))
(struct system-state (dead?-box listener-posn-f srcs))
(define (initial-system-state lpf)
  (system-state (box #f) lpf empty))

(define (sound->source-vector s)
  (list->vector (map source-srci (system-state-srcs s))))
(define (sound-pause! s)
  (sound-dead-error s 'sound-pause!)
  (alSourcePausev (sound->source-vector s)))
(define (sound-unpause! s)
  (sound-dead-error s 'sound-unpause!)
  (alSourcePlayv (sound->source-vector s)))
(define (sound-destroy! s)
  (match-define (system-state dead-box _ _) s)
  (sound-dead-error s 'sound-destroy!)
  (set-box! dead-box #t)
  (alDeleteSources (sound->source-vector s)))

(define (sound-dead-error s sym)
  (when (unbox (system-state-dead?-box s))
    (error sym "Sound already dead")))
  
(define (render-sound scale sst cmds w)
  (match-define (system-state db lpf srcs) sst)
  (sound-dead-error sst 'render-sound)
  (define all-srcs
    (append 
     (map (λ (f) 
            (define srci (vector-ref (alGenSources 1) 0))
            (source srci #f f))
          cmds)
     srcs))
  
  (define lp (lpf w))
  (alListener3f AL_POSITION 
                (/ (psn-x lp) scale)
                (/ (psn-y lp) scale)
                0.0)
  
  (define srcs*
    (for/fold ([srcs* empty])
      ([src (in-list all-srcs)])
      (match-define (source srci old f) src)
      (define src-st
        (alGetSourcei srci AL_SOURCE_STATE))
      (match
          (and 
           ; This causes sources to be stopped and 
           ; deleted once the stop naturally
           ; or the function returns #f
           (not (= src-st AL_STOPPED))
           (f w))
        [(and new (sound-state a p gain relative? looping? paused?))
         (alSource3f srci AL_POSITION
                     (/ (psn-x p) scale)
                     (/ (psn-y p) scale)
                     0.0)
         (alSourcef srci AL_GAIN gain)
         (alSourceb srci AL_LOOPING looping?)
         (alSourceb srci AL_SOURCE_RELATIVE relative?)
         
         (unless (and old (eq? (sound-state-audio old) a))
           (match-define (audio b) a)
           (alSourcei srci AL_BUFFER b))
         
         ; If the pause signal occurs, we should pause it;
         ; otherwise it is either AL_INITIAL, AL_PLAYING, or
         ; AL_PAUSED, in all but the middle cause, we 
         ; should start playing
         (if paused?
             (alSourcePause srci)
             (unless (= AL_PLAYING src-st)
               (alSourcePlay srci)))
         
         (cons (source srci new f)
               srcs*)]
        [#f
         ; XXX are sources scarce? should i try to reuse old ones, rather than delete them?
         (alSourceStop srci)
         (alDeleteSources (vector srci))
         srcs*])))
  
  (system-state db lpf srcs*))

(provide/contract
 [audio? contract?]
 [path->audio (-> path? audio?)] 
 [struct sound-state
         ([audio audio?]
          [posn psn?]
          [gain inexact?]
          [relative? boolean?]
          [looping? boolean?]
          [paused? boolean?])]
 [sound-state/c contract?]
 [sound/c contract?]
 [sound-scape/c contract?]
 [background 
  (->* ((-> any/c audio?))
       (#:gain inexact? #:pause-f (-> any/c boolean?))
       sound/c)]
 [sound-at
  (->* (audio? psn?)
       (#:gain inexact? #:looping? boolean? #:pause-f (-> any/c boolean?))
       sound/c)]
 [sound-on
  (->* (audio? (-> any/c psn?))
       (#:gain inexact? #:looping? boolean? #:pause-f (-> any/c boolean?))
       sound/c)]
 [sound-until
  (-> sound/c (-> any/c boolean?)
      sound/c)]
 [system-state? contract?]
 [initial-system-state 
  (-> (-> any/c psn?)
      system-state?)]
 [sound-pause! (-> system-state? void)]
 [sound-unpause! (-> system-state? void)]
 [sound-destroy! (-> system-state? void)]
 [render-sound
  (-> real? system-state? sound-scape/c any/c
      system-state?)])

; -----------------------------------

(define d (alcOpenDevice #f))
(define ctxt (alcCreateContext d))
(alcMakeContextCurrent ctxt)
