#lang racket/base
(require racket/match
         ffi/unsafe
         ffi/unsafe/objc
         mred/private/wx/cocoa/utils)

(define DDHidLib
  (ffi-lib "DDHidLib.framework/DDHidLib"))

(import-class NSObject DDHidJoystick)

(define-syntax mvector-ref
  (syntax-rules ()
    [(_ v i) (vector-ref v i)]
    [(_ v i j ...) (mvector-ref (vector-ref v i) j ...)]))
(define-syntax mvector-set!
  (syntax-rules ()
    [(_ v i e) (vector-set! v i e)]
    [(_ v i j ... e) (mvector-set! (vector-ref v i) j ... e)]))
(define deep-vector->immutable
  (match-lambda
   [(? vector? v)
    ; XXX build-immutable-vector should exist
    (apply vector-immutable
           (for/list ([i (in-vector v)])
             (deep-vector->immutable v)))]
   [x x]))

; A joystick-state is a
;  sticks : stick -> (axis 0 / pov 1) -> which -> int
;  buttons : button -> boolean
(struct joystick-state (sticks buttons))

(define-objc-class JoystickWatcher NSObject
  [sticks buttons]
  (- _void (startWatching: js)
     
     (set! sticks 
           (build-vector
            (get-ivar js countOfSticks)
            (λ (sn)
              (define s (tell js objectInSticksAtIndex: sn))
              (vector
               (make-vector (get-ivar s countOfStickElements) 0)
               (make-vector (get-ivar s countOfPovElements) 0)))))
     (set! buttons (make-vector (get-ivar js numberOfButtons) #f))
     
     (tell js startListening)
     (tell js setDelegate: self))
  (-a _racket (snapshot)
      (joystick-state (deep-vector->immutable sticks) 
                      (deep-vector->immutable buttons)))
  
  (- _void (ddhidJoystick: [_id joystick] stick: [_uint stick-n] xChanged: [_int v])
     (mvector-set! sticks stick-n 0 0 v))
  (- _void (ddhidJoystick: [_id joystick] stick: [_uint stick-n] yChanged: [_int v])
     (mvector-set! sticks stick-n 0 1 v))
  (- _void (ddhidJoystick: [_id joystick] stick: [_uint stick-n] otherAxis: [_uint axis] valueChanged: [_int v])
     (mvector-set! sticks stick-n 0 axis v))
  (- _void (ddhidJoystick: [_id joystick] stick: [_uint stick-n] povNumber: [_uint pov] valueChanged: [_int v])
     (mvector-set! sticks stick-n 1 pov v))
  (- _void (ddhidJoystick: [_id joystick] buttonDown: [_uint button-n])
     (mvector-set! buttons button-n #t))
  (- _void (ddhidJoystick: [_id joystick] buttonUp: [_uint button-n])
     (mvector-set! buttons button-n #f)))
  
(define (get-all-joystick-snapshots)
  (with-autorelease
      (define js (tell DDHidJoystick allJoysticks))
    (for/list ([i (in-range (tell #:type _uint js count))])
      (define w (tell JoystickWatcher alloc))
      (tell w startWatching:
            (tell js objectAtIndex: i))
      (λ () (tell w snapshot)))
    (tell js dealloc)))

#;(thread-wait (go!))

(provide
 mvector-ref
 get-all-joystick-snapshots
 (struct-out joystick-state))