#lang racket/base
(require "psn.rkt"
         "openal.rkt")

; Goals
;  - functionally update the posn of the listener
;  - functionally update the background music
;  - functionally add new located sound effects (with a starting velocity, because I won't support movement)
; Non goals (currently)
;  - moving or pausing old located sound effects
; Unknown
;  - how do I handle a pause screen? I want to turn off the background music and temporarily pause all effects, then switch to a different BGM and maybe even use different effects (while they are in the menu) for a bit, but then once they un-pause, go back


; Xxxx maybe it would be okay to allow stopping/moving looping sounds
; xxx maybe i should just allow stopping... with a stop condition?
;     i'm imaging the problem when an enemy has just started talking, but then they are destroyed, you don't want their sound wav to continue to play

; xxx are sources scarce? should i try to reuse old ones, rather than delete them?
(struct audio (b))
(struct sound-scape (resolved? label->source cmd))
(define (create-sound-scape)
  (sound-scape (box #f) (hasheq) (λ (x) x)))

(define listener (gensym 'listener))

; xxx disallow overwriting other sounds
; xxx disallow doing certain things to the listener
; xxx what should I do if the given source doesn't exist, because it was deleted/stop?
; xxx do i really want a long-lived object to update its position for a sound for all time, even though the sound stop playing long ago?

(define (source a l p s 
                #:gain [gain 1.0] #:looping? [looping? #f] #:relative? [relative? #f])
  (match-define (audio b) a)
  (struct-copy sound-scape s
               [cmd
                (λ (l->s*)
                  (define l->s ((sound-scape-cmd s) l->s*))
                  (define src (vector-ref (alGenSources 1) 0))
                  (alSourcef src AL_GAIN gain)
                  (alSourceb src AL_LOOPING looping?)
                  ;xxx relative
                  (alSourcei src AL_BUFFER b)
                  (alSource3f src AL_POSITION
                              (psn-x p) (psn-y p) 0.0)
                  (hash-set l->s l src))]))
  
(define (move l p s)
  ; XXX handle l = listner specially
  (struct-copy sound-scape s
               [cmd 
                (λ (l->s*)
                  (define l->s ((sound-scape-cmd s) l->s*))
                  (alSource3f (hash-ref l->s l)
                              AL_POSITION
                              (psn-x p) (psn-y p) 0.0)
                  l->s)]))

(define (play l s)
  (struct-copy sound-scape s
               [cmd 
                (λ (l->s*)
                  (define l->s ((sound-scape-cmd s) l->s*))
                  (alSourcePlay (hash-ref l->s l))
                  l->s)]))
(define (pause l s)
  (struct-copy sound-scape s
               [cmd 
                (λ (l->s*)
                  (define l->s ((sound-scape-cmd s) l->s*))
                  (alSourcePause (hash-ref l->s l))
                  l->s)]))
(define (stop l s)
  (struct-copy sound-scape s
               [cmd 
                (λ (l->s*)
                  (define l->s ((sound-scape-cmd s) l->s*))
                  (define src (hash-ref l->s l))
                  (alSourceStop src)
                  (alDeleteSources (vector src))
                  (hash-remove l->s l))]))

(define (unresolved-sound-scape? x)
  (and (sound-scape? x)
       (not (unbox (sound-scape-resolved? x)))))

(define (resolve s)
  (match-define (sound-scape resolved?-b label->source cmd) s)
  (set-box! resolved?-b #t)
  (define l->s (cmd label->source))
  ; xxx should I loop through l->s and delete the ones that are stopped?
  ;     if so, if i do it every time, then that's a lot of bandwidth over the openal library
  ;     maybe I should only do it if I detect a change
  (sound-scape (box #f) l->s (λ (x) x)))

(provide/contract
 [path->audio (-> path?
                  audio?)]
 [sound-scape? contract?]
 [rename create-sound-scape sound-scape (-> sound-scape?)]
 [listener symbol?]
 [move (-> symbol? psn? sound-scape?
           sound-scape?)]
 [source (->* (audio? symbol? psn? sound-scape?)
              (#:gain float? #:looping? boolean? #:relative? boolean?)
              sound-scape?)]
 [play (-> symbol? sound-scape?
           sound-scape?)]
 [pause (-> symbol? sound-scape?
            sound-scape?)]
 [stop (-> symbol? sound-scape?
           sound-scape?)]
 ; XXX these should be in some sort of "internal" module not available to the games
 [unresolved-sound-scape? contract?]
 [resolve (-> unresolved-sound-scape?
              sound-scape?)])
 
; Example:
(require racket/runtime-path)
(define-runtime-path resource-path "../resources")

(define bgm 
  (path->audio 
   (build-path resource-path 
               "SMB-1-1.mp3")))
(define jump-se
  (path->audio
   (build-path resource-path 
               "SMB-SE-Jump.wav")))

(define s 
  (source
   bgm 'bgm 0+0i #:gain 0.8 #:looping? #t #:relative? #t
   (move listener 0+0i
         (sound-scape))))

(define
  s+
  (for/fold ([s (resolve s)])
    ([i (in-range 10)])
    (sleep 5)
    (resolve
     (source
      jump-se 'jumper (+ -5+0i i)
      s))))

(resolve (stop 'bgm s+))
