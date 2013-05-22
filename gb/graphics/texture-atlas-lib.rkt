#lang racket/base
(require ffi/vector
         (for-syntax racket/base
                     syntax/parse
                     racket/match
                     gb/lib/math
                     racket/syntax)
         gb/lib/math
         gb/lib/fstree
         ffi/unsafe
         ffi/vector)

(define-syntax (define-sprite-atlas stx)
  (syntax-parse stx
    [(_ count:nat size:nat)
     (with-syntax*
      ([sas (datum->syntax stx 'sprite-atlas-size)]
       [s (datum->syntax stx 'sprited)]
       [s-r (datum->syntax stx 'sprited-ref)]
       [i? (datum->syntax stx 'sprite-index?)]
       [_i (datum->syntax stx '_sprite-index)]
       [st (datum->syntax stx 'sprite-tree)]
       [s-images (format-id stx "~a-images" #'s)]
       [(i?_impl _idx idxvector)
        (match (num->bytes-to-store (syntax->datum #'count))
          [1 #'(_uint8? _uint8 u8vector)]
          [2 #'(_uint16? _uint16 u16vector)]
          [4 #'(_uint32? _uint32 u32vector)]
          [_ (raise-syntax-error 'define-sprite-atlas
                                 "Too many sprites to store indexes"
                                 #'count)])]
       [idxvector-ref (format-id #'idxvector "~a-ref" #'idxvector)]
       [ds (datum->syntax stx 'define-sprite)])
      (syntax/loc stx
        (begin
          (define sas size)
          (define _i _idx)
          (define i? i?_impl)
          (struct s (width height images))
          (define (s-r an-s i)
            (idxvector-ref (s-images an-s) i))
          (define st (make-fstree))
          (provide sas _i i? (struct-out s) s-r st)
          (define-syntax (ds stx)
            (syntax-parse stx
              [(_ name:id Tw:nat Th:nat (Iidx:nat (... ...)))
               (with-syntax
                   ([spr:name (format-id #'name "spr:~a" #'name)]
                    [name-str (symbol->string (syntax-e #'name))])
                 (syntax/loc stx
                   (begin
                     (define spr:name
                       (s Tw Th (idxvector Iidx (... ...))))
                     (fstree-insert! st name-str spr:name)
                     (provide spr:name))))])))))]))

(define-syntax (define-palette-atlas stx)
  (syntax-parse stx
    [(_ count:nat depth:nat)
     (with-syntax
         ([pac (datum->syntax stx 'palette-atlas-count)]
          [pad (datum->syntax stx 'palette-atlas-depth)]
          [p? (datum->syntax stx 'palette?)]
          [_p (datum->syntax stx '_palette)]
          [(p?_impl _pal)
           (match (num->bytes-to-store (syntax->datum #'count))
             [1 #'(_uint8? _uint8)]
             [2 #'(_uint16? _uint16)]
             [4 #'(_uint32? _uint32)]
             [_ (raise-syntax-error 'define-palette-atlas
                                    "Too many palettes to store indexes"
                                    #'count)])])
       (syntax/loc stx
         (begin
           (define _p _pal)
           (define p? p?_impl)
           (define pac count)
           (define pad depth)
           (provide _p p? pac pad))))]))

(define-syntax (define-palette stx)
  (syntax-parse stx
    [(_ name:id index:nat)
     (with-syntax ([pal:name (format-id #'name "pal:~a" #'name)])
       (syntax/loc stx
         (begin
           (define pal:name index)
           (provide pal:name))))]))

(provide (all-defined-out))
