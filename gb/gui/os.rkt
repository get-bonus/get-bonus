#lang racket/base
(require racket/runtime-path
         racket/match
         racket/contract
         racket/function
         racket/math
         racket/list
         gb/gui/world
         (prefix-in gl:
                    (combine-in gb/graphics/gl
                                gb/graphics/gl-ext))
         gb/graphics/sprites
         gb/data/mvector
         gb/gui/fullscreen
         gb/input/controller
         gb/audio/3s
         gb/lib/math
         gb/data/psn
         gb/meta
         (prefix-in cd: gb/physics/cd-narrow))

(define kernel-prompt-tag (make-continuation-prompt-tag 'kernel))
(define (run-process-until-syscall p)
  (call-with-continuation-barrier
   (λ ()
     (call-with-continuation-prompt
      (λ ()
        (os/exit (p))
        (error 'kernel "Process did not run to system call"))
      kernel-prompt-tag
      (λ (x) x)))))
(define (trap-syscall k->syscall)
  ;; First we capture our context back to the OS
  (call-with-current-continuation
   (λ (k)
     ;; Then we abort, give it to the OS, along with a syscall
     ;; specification
     (abort-current-continuation kernel-prompt-tag (k->syscall k)))
   kernel-prompt-tag))

(define-syntax-rule
  (define-syscall (call k call-arg ...) state-args
    body ...)
  (define call
    (let ()
      (struct call (k call-arg ...)
              #:property prop:procedure
              (λ (the-struct . state-args)
                (match-define (call k call-arg ...) the-struct)
                body ...)
              #:transparent)
      (λ (call-arg ...)
        (trap-syscall
         (λ (k)
           (call k call-arg ...)))))))

(define-syntax-rule
  (define-syscalls state-args
    [call-spec . body]
    ...)
  (begin
    (define-syscall call-spec state-args
      . body)
    ...))

(define (snoc* beginning . end)
  (append beginning end))

(struct process (pid k) #:transparent)

(struct os (cur-heap next-heap cur-procs next-procs))

(define-syscalls (pid current)
  [(os/exit k v)
   current]
  [(os/read k id)
   (match-define (os cur-h next-h cur-ps next-ps) current)
   (os cur-h next-h
       (snoc* cur-ps
              (process pid (λ () (k (hash-ref cur-h id empty)))))
       next-ps)]
  [(os/write k id*vals)
   (match-define (os cur-h next-h cur-ps next-ps) current)
   (for ([id*val (in-list id*vals)])
     (match-define (cons id val) id*val)
     (hash-update! next-h id (curry cons val) (λ () empty)))
   (os cur-h next-h cur-ps
       (snoc* next-ps
              (process pid (λ () (k (void))))))]
  [(os/thread k t)
   (match-define (os cur-h next-h cur-ps next-ps) current)
   (define t-pid (gensym 'pid))
   (os cur-h next-h
       (snoc* cur-ps
              (process pid (λ () (k t-pid)))
              (process t-pid t))
       next-ps)])

(define nothing (gensym))
(define (os/read* k [def nothing])
  (core-read* k (os/read k) def))

(define (core-read* k vs def)
  (match vs
    [(list v)
     v]
    [else
     (if (eq? nothing def)
       (error 'os/read* "~e does not have one value, has ~e" k else)
       def)]))

(define (os-sound-reader k [def nothing])
  (λ (os)
    (core-read* k (hash-ref (os-cur-heap os) k empty) def)))

(define boot
  (match-lambda
   [(os cur-h next-h (list) next-ps)
    (os next-h (make-hasheq) next-ps (list))]
   [(os cur-h next-h (list* (process pid now) cur-ps) next-ps)
    (define syscall (run-process-until-syscall now))
    (boot (syscall pid
                   (os cur-h next-h cur-ps next-ps)))]))

(define (big-bang/os width height center-pos
                     #:sound-scale sound-scale
                     main-t)
  (big-bang
   (os (make-hasheq) (make-hasheq)
       (list (process (gensym 'pid) main-t))
       empty)
   #:sound-scale sound-scale
   #:tick
   (λ (w cs)
     ;; XXX this is not a good place
     (printf "FPS: ~a\n"
             (real->decimal-string
              (current-rate) 1))
     (match-define (os cur-h next-h cur-ps next-ps) w)
     (hash-set! cur-h 'controller (list (first cs)))
     (define new-w
       (boot
        (os cur-h next-h cur-ps next-ps)))
     (match-define (os new-cur-h _ _ _) new-w)
     (values new-w
             (gl:focus
              width height width height
              (psn-x center-pos) (psn-y center-pos)
              (gl:background 0. 0. 0. 1.0
                             (apply gl:seqn
                                    (hash-ref new-cur-h 'graphics empty))))
             (hash-ref new-cur-h 'sound empty)))
   #:listener
   (λ (w)
     ;; XXX
     center-pos)
   #:done?
   (λ (w)
     (match-define (os cur-h _ _ _) w)
     (first (hash-ref cur-h 'done? (list #f))))))

(provide
 (contract-out
  [os/exit
   (-> any/c any)]
  [os/read
   (-> symbol? list?)]
  [os/write
   (-> (listof (cons/c symbol? any/c))
       any)]
  [os/thread
   (-> (-> any)
       any)]
  [os/read*
   (->* (symbol?)
        (any/c)
        any/c)]
  [big-bang/os
   (-> number? number? psn?
       #:sound-scale number?
       (-> any)
       any)]
  [os-sound-reader
   (->* (symbol?) (any/c)
        (-> os? any/c))]))
