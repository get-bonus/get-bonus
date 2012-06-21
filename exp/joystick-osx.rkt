#lang racket/base
(require racket/contract
         racket/list
         "joystick-shared.rkt")

(provide (all-from-out "joystick-shared.rkt"))

(define (get-all-joystick-snapshot-thunks)
  empty)

(provide/contract
 [get-all-joystick-snapshot-thunks
  (-> (listof (-> joystick-state?)))])
