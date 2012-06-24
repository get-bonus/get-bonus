#lang racket/base
(require racket/match
         racket/gui/base
         racket/set
         racket/contract
         racket/class)

(struct keyboard-monitor (ks-b) 
        #:transparent)

(define (keyboard-monitor*)
  (keyboard-monitor (box (seteq))))

(define (keyboard-monitor-submit! km ke)
  (match-define (keyboard-monitor b) km)
  (define c (send ke get-key-code))
  (set-box! b 
            (if (eq? c 'release)
                (set-remove (unbox b) (send ke get-key-release-code))
                (set-add (unbox b) c))))

(define (keyboard-monitor-state km)
  (match-define (keyboard-monitor b) km)
  (unbox b))

(provide/contract
 [keyboard-monitor? contract?]
 [rename keyboard-monitor* keyboard-monitor
         (-> keyboard-monitor?)]
 [keyboard-monitor-submit! 
  (keyboard-monitor? (is-a?/c key-event%) . -> . void)]
 [keyboard-monitor-state
  (keyboard-monitor? . -> . (set/c (or/c char? key-code-symbol?)))])
