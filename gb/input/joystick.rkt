#lang racket/base
(require racket/list
         racket/port
         racket/vector
         (except-in racket/contract ->)
         (prefix-in c racket/contract)
         racket/match)

(define (js-event-time bs)
  (integer-bytes->integer bs #f #f 0 4))
(define (js-event-value bs)
  (integer-bytes->integer bs #t #f 4 6))
(define (js-event-type bs)
  (bytes-ref bs 6))
(define (js-event-number bs)
  (bytes-ref bs 7))
(define (js-event-init? bs)
  (not (zero? (bitwise-and (js-event-type bs) #x80))))
(define (js-event-button? bs)
  (not (zero? (bitwise-and (js-event-type bs) #x01))))
(define (js-event-axis? bs)
  (not (zero? (bitwise-and (js-event-type bs) #x02))))

(require ffi/unsafe)

(define c-lib (ffi-lib #f))

;; Calculated from a short C program
(define JSIOCGAXES #x80016a11)
(define JSIOCGBUTTONS #x80016a12)
(define JSIOCGNAME_128 #x80806a13)

(define scheme_get_port_fd
  (get-ffi-obj "scheme_get_port_fd" c-lib
               (_fun _scheme -> _int)))
(define ioctl_char
  (get-ffi-obj
   "ioctl" c-lib
   (_fun (p code) ::
         (fd : _int = (scheme_get_port_fd p))
         (code : _intptr)
         (char : (_ptr o _byte))
         ->
         (err : _int)
         ->
         (if (not (= err -1))
           char
           (error 'ioctl "error code: ~e" err)))))
(define ioctl_str128
  (get-ffi-obj
   "ioctl" c-lib
   (_fun (p code) ::
         (fd : _int = (scheme_get_port_fd p))
         (code : _intptr)
         (bs : (_bytes o 128))
         ->
         (err : _int)
         ->
         (if (not (= err -1))
           (subbytes bs 0 (sub1 err))
           (error 'ioctl "error code: ~e" err)))))

(struct joystick-state (name axes buttons) #:transparent)
(define deep-copy
  (match-lambda
   [(joystick-state n as bs)
    (joystick-state n (vector-copy as) (vector-copy bs))]))
(define (deep-update! inl outl)
  (for ([in (in-list inl)]
        [out (in-list outl)])
    (vector-copy! (joystick-state-axes out) 0 (joystick-state-axes in))
    (vector-copy! (joystick-state-buttons out) 0 (joystick-state-buttons in))))

(define IGNORED-JOYSTICKS
  '(#"applesmc"))

(struct joystick-monitor (t in-sem out-sem out-state))
(define (make-joystick-monitor)
  (define js-ports
    (map open-input-file
         (filter (λ (p) (regexp-match #rx#"^/dev/input/js" (path->bytes p)))
                 (directory-list "/dev/input/" #:build? #t))))
  (define p*state-s
    (filter (λ (x) x)
            (for/list ([p (in-list js-ports)])
              (define axes (ioctl_char p JSIOCGAXES))
              (define buttons (ioctl_char p JSIOCGBUTTONS))
              (define name (ioctl_str128 p JSIOCGNAME_128))
              (and (not (member name IGNORED-JOYSTICKS))
                   (cons p
                         (joystick-state name 
                                         (make-vector axes 0.0)
                                         (make-vector buttons 0)))))))
  (define state-ports (map car p*state-s))
  (define state (map cdr p*state-s))
  (define out-state
    (map deep-copy state))
  (define event-bs
    (make-bytes 8))

  (define in-sem (make-semaphore))
  (define out-sem (make-semaphore))
  (define monitor-t
    (thread
     (λ ()
       (define event-evts
         (for/list ([p (in-list state-ports)]
                    [s (in-list state)])
           (handle-evt (read-bytes!-evt event-bs p)
                       (λ (how-many-or-eof)
                         (unless (eof-object? how-many-or-eof)
                           (if (js-event-button? event-bs)
                             (vector-set! (joystick-state-buttons s)
                                          (js-event-number event-bs)
                                          (js-event-value event-bs))
                             (vector-set! (joystick-state-axes s)
                                          (js-event-number event-bs)
                                          (exact->inexact
                                           (/ (js-event-value event-bs) 32767)))))))))
       (define read-evt
         (handle-evt
          in-sem
          (λ (_)
            (deep-update! state out-state)
            (semaphore-post out-sem))))
       (define evts
         (apply choice-evt read-evt event-evts))
       (let loop ()
         (sync evts)
         (loop)))))
  (joystick-monitor monitor-t in-sem out-sem out-state))

(define (joystick-monitor-state m)
  (match-define (joystick-monitor t in-sem out-sem st) m)
  (thread-resume t)
  (semaphore-post in-sem)
  (semaphore-wait out-sem)
  st)

(provide/contract
 [struct joystick-state
         ([name bytes?]
          [axes (vectorof inexact?)]
          [buttons (vectorof number?)])]
 [joystick-monitor?
  (c-> any/c boolean?)]
 ;; XXX remove make
 [make-joystick-monitor
  (c-> joystick-monitor?)]
 [joystick-monitor-state
  (c-> joystick-monitor? (listof joystick-state?))])

(module+ main
  (define m (make-joystick-monitor))
  (for ([i (in-range (* 60 15))])
    (printf "~a ~v\n"
            (real->decimal-string (/ (/ (current-memory-use) 1024) 1024))
            (joystick-monitor-state m))
    (sleep 1/60)))
