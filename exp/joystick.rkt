#lang racket/base
(require racket/match
         racket/contract
         racket/runtime-path
         (except-in ffi/unsafe ->)
         ffi/unsafe/objc
         mred/private/wx/cocoa/utils
         "mvector.rkt")

; XXX This probably leaks memory, because sometimes I get crashes and weird error messages

(define-runtime-path ddhid-lib
  "../dist/DDHidLib-1.1/build/Release/DDHidLib.framework/DDHidLib")
(define DDHidLib
  (ffi-lib ddhid-lib))

(import-class NSObject DDHidJoystick)

(define DDHID_JOYSTICK_VALUE_MIN -65536)
(define DDHID_JOYSTICK_VALUE_MAX 65536)
(define (normalize n)
  (exact->inexact
     (/ n DDHID_JOYSTICK_VALUE_MAX)))

(define deep-vector->immutable
  (match-lambda
   [(? vector? v)
    ; XXX build-immutable-vector should exist
    (apply vector-immutable
           (for/list ([i (in-vector v)])
             (deep-vector->immutable i)))]
   [x x]))

; A joystick-state is a
;  sticks : stick -> (axis 0 / pov 1) -> which -> int
;  buttons : button -> boolean
(struct joystick-state (sticks buttons) #:transparent)

(define-objc-class JoystickWatcher NSObject
  [js sticks buttons]
  (- _void (startWatching: [_id the-js])
     (set! js the-js)
     (set! sticks 
           (build-vector
            (tell #:type _uint js countOfSticks)
            (λ (sn)
              (define s (tell js objectInSticksAtIndex: #:type _uint sn))
              (vector
               (make-vector (+ 2 (tell #:type _uint s countOfStickElements)) 0)
               (make-vector (tell #:type _uint s countOfPovElements) 0)))))
     (set! buttons (make-vector (tell #:type _uint js numberOfButtons) #f))
     
     (retain js)
     (tell js startListening)
     (tell js setDelegate: self))
  (- _void (dealloc)
     (release js))
  
  ; XXX Ideally this would be atomic
  (- _racket (snapshot)
     (joystick-state (deep-vector->immutable sticks) 
                     (deep-vector->immutable buttons)))
  
  (- _void (ddhidJoystick: js stick: [_uint sn] xChanged: [_int v])
     (mvector-set! sticks sn 0 0 (normalize v)))
  (- _void (ddhidJoystick: js stick: [_uint sn] yChanged: [_int v])
     (mvector-set! sticks sn 0 1 (* -1 (normalize v))))
  (- _void (ddhidJoystick: js stick: [_uint sn] otherAxis: [_uint axis] valueChanged: [_int v])
     (mvector-set! sticks sn 0 axis (normalize v)))
  (- _void (ddhidJoystick: js stick: [_uint sn] povNumber: [_uint pov] valueChanged: [_int v])
     (mvector-set! sticks sn 1 pov v))
  (- _void (ddhidJoystick: js buttonDown: [_uint button-n])
     (mvector-set! buttons button-n #t))
  (- _void (ddhidJoystick: js buttonUp: [_uint button-n])
     (mvector-set! buttons button-n #f)))
  
(define (get-all-joystick-snapshot-thunks)
  (with-autorelease
      (define js (tell DDHidJoystick allJoysticks))
    (retain js)
    (begin0
      (for/list ([i (in-range (tell #:type _uint js count))])
        (define w (tell (tell JoystickWatcher alloc) init))
        (retain w)
        (tell w startWatching:
              (tell js objectAtIndex: #:type _uint i))
        (λ () (tell #:type _racket w snapshot)))
      (release js))))

(provide/contract
 [get-all-joystick-snapshot-thunks
  (-> (listof (-> joystick-state?)))]
 [struct joystick-state
         ([sticks 
           (vector-immutableof
            (vector/c
             #:immutable #t
             (vector-immutableof number?)
             (vector-immutableof number?)))]
          [buttons
           (vector-immutableof boolean?)])])