#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     racket/syntax)
         ffi/vector)

(define-syntax (define-sprite-atlas-size stx)
  (syntax-parse stx
    [(_ size:nat)
     (quasisyntax/loc stx
       (define #,(datum->syntax stx 'sprite-atlas-size) size))]))

(define-syntax (define-palette-atlas-size stx)
  (syntax-parse stx
    [(_ k:nat depth:nat)
     (quasisyntax/loc stx
       (begin
         (define #,(datum->syntax stx 'palette-atlas-count) k)
         (define #,(datum->syntax stx 'palette-atlas-depth) depth)))]))

(define-syntax (define-texture stx)
  (syntax-parse stx
    [(_ name:id Tllx:number Tlly:number Tw:number Th:number)
     (with-syntax ([tex:name (format-id #'name "tex:~a" #'name)])
       (syntax/loc stx
         (define tex:name
           (f32vector Tllx Tlly Tw Th))))]))

(define-syntax (define-sprite-image stx)
  (syntax-parse stx
    [(_ name:id imgi:nat Tllx:number Tlly:number Tw:number Th:number)
     (with-syntax ([name:imgi (format-id #'name "~a:~a"
                                         #'name
                                         (syntax->datum #'imgi))])
       (syntax/loc stx
         (define-texture name:imgi Tllx Tlly Tw Th)))]))

(define-syntax (define-sprite stx)
  (syntax-parse stx
    [(_ name:id imgk:nat)
     (with-syntax
         ([spr:name
           (format-id #'name "spr:~a" #'name)]
          [(tex:name:imgi ...)
           (build-list (syntax->datum #'imgk)
                       (λ (i)
                         (format-id #'name "tex:~a:~a" #'name i)))])
       (syntax/loc stx
         (define spr:name (vector tex:name:imgi ...))))]))

(define texturev?
  ;; xxx also, of texture?
  vector?)
(define texturev-ref vector-ref)

(define-syntax (define-palette stx)
  (syntax-parse stx
    [(_ name:id index:nat)
     (with-syntax ([pal:name (format-id #'name "pal:~a" #'name)])
       (syntax/loc stx
         (define pal:name index)))]))

(define palette?
  exact-nonnegative-integer?)

(define (texture x y w h)
  (f32vector x y w h))
;; XXX also 4 long
(define texture? f32vector?)
(define (texture-x v)
  (f32vector-ref v 0))
(define (texture-y v)
  (f32vector-ref v 1))
(define (texture-width v)
  (f32vector-ref v 2))
(define (texture-height v)
  (f32vector-ref v 3))

(provide (all-defined-out))
