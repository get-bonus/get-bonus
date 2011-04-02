#lang racket
(require ffi/unsafe
         ffi/unsafe/objc
         mred/private/wx/cocoa/utils)

(define DDHidLib
  (ffi-lib "DDHidLib.framework/DDHidLib"))

(import-class NSObject DDHidJoystick)

(define-objc-class joystick-watcher% NSObject
  [joysticks]
  (- _racket (start-watching-joysticks!)
     (set! joysticks (tell DDHidJoystick allJoysticks))
     (retain joysticks)
     
     (tell joysticks makeObjectsPerformSelector: 
           #:type _SEL
           (selector startListening))
     (tell joysticks makeObjectsPerformSelector: 
           #:type _SEL
           (selector setDelegate:)
           withObject: self)
     
     (printf "I am now the delegate of ~a objects\n"
             (tell #:type _uint joysticks count)))
  (- _void (dealloc)
     (release joysticks)
     (set! joysticks #f))
  
  (- _void (ddhidJoystick: [_id joystick] buttonDown: [_uint button-n])
     (printf "Button ~a of Joystick ~a went down\n"
             button-n joystick))
  (- _void (ddhidJoystick: [_id joystick] buttonUp: [_uint button-n])
     (printf "Button ~a of Joystick ~a went up\n"
             button-n joystick)))
  
(define (go!)
  (thread
   (λ ()
     (with-autorelease
         (define watcher (tell joystick-watcher% alloc))
       (tell watcher start-watching-joysticks!)
       
       (semaphore-wait (make-semaphore 0))
       
       (tell watcher dealloc)))))

#;(thread-wait (go!))

(provide go!)