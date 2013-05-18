#lang racket/base
(require racket/contract
         racket/draw
         racket/class
         racket/file
         racket/list
         racket/match
         racket/runtime-path)
(module+ test
  (require rackunit))

;; Library Code

(define (directory-list* d)
  (sort
   (map path->string
        (directory-list d))
   string-ci<=?))
(define (all-from root suffix)
  (define suffix-re
    (regexp (format "\\.~a$" (regexp-quote suffix))))
  (define root-re
    (regexp (format "^~a/" (regexp-quote (path->string root)))))
  (map (λ (p)
         (regexp-replace root-re
                         (regexp-replace suffix-re
                                         (path->string p)
                                         "")
                         ""))
       (find-files (λ (p) (regexp-match suffix-re (path->string p)))
                   root)))

;; Real Code

(struct db (pth))
(define-runtime-path default-db-pth "default-db")
(define (load-db db-pth)
  (unless (directory-exists? db-pth)
    (copy-directory/files default-db-pth db-pth))
  (db db-pth))
(define (db-palettes a-db)
  (match-define (db db-path) a-db)
  (all-from (build-path db-path "palettes") "pal"))
(define (db-sprites a-db)
  (match-define (db db-path) a-db)
  (all-from (build-path db-path "sprites") "spr"))

(define (load-last a-db)
  (match-define (db db-path) a-db)
  (define last-path (build-path db-path "last.rktd"))
  (define last-sprite
    (if (file-exists? last-path)
      (file->value last-path)
      #f))
  (unless (file-exists?
           (build-path db-path "sprites" (format "~a.spr" last-sprite)))
    (set! last-sprite #f))
  (unless last-sprite
    (set! last-sprite (first (db-sprites a-db))))
  last-sprite)

(define (last-save! a-db last-sprite)
  (match-define (db db-path) a-db)
  (define last-path (build-path db-path "last.rktd"))
  (write-to-file last-sprite last-path
                 #:exists 'replace))

(struct sprite (name width height
                     [images #:mutable]
                     [palettes #:mutable]))
(define (load-sprite a-db sprite-name)
  (match-define (db db-path) a-db)
  (define sprite-dir
    (build-path db-path "sprites" (format "~a.spr" sprite-name)))
  (match-define (cons w h)
                (file->value (build-path sprite-dir "meta")))
  (define how-many-images
    (count (λ (p) (regexp-match #rx".img$" p))
           (directory-list* sprite-dir)))
  (define images
    (for/vector ([i (in-range how-many-images)])
      (file->bytes
       (build-path sprite-dir (format "~a.img" i)))))
  (define palettes (file->value (build-path sprite-dir "palettes")))
  (sprite sprite-name w h images palettes))
(define (sprite-save! a-db a-sprite)
  (match-define (db db-path) a-db)
  (match-define (sprite sprite-name w h images palettes) a-sprite)
  (define sprite-dir
    (build-path db-path "sprites" (format "~a.spr" sprite-name)))
  (make-directory* sprite-dir)
  (write-to-file (cons w h) (build-path sprite-dir "meta")
                 #:exists 'replace)
  (for ([i (in-naturals)]
        [iv (in-vector images)])
    (display-to-file iv
                     (build-path sprite-dir (format "~a.img" i))
                     #:exists 'replace))
  (write-to-file palettes (build-path sprite-dir "palettes")
                 #:exists 'replace))

(struct palette (name colors))
(define (palette-color->color% c)
  (match-define (vector a r g b) c)
  (make-object color% r g b (/ a 255)))
(define (palette-color%s p)
  (for/vector ([c (in-vector (palette-colors p))])
    (palette-color->color% c)))
(define (load-palette a-db pn)
  (match-define (db db-path) a-db)
  (palette pn
           (file->value
            (build-path db-path "palettes"
                        (format "~a.pal" pn)) )))
(define (palette-save! a-db p)
  (match-define (db db-path) a-db)
  (match-define (palette pn cvs) p)
  (write-to-file
   cvs
   (build-path db-path "palettes"
               (format "~a.pal" pn))
   #:exists 'replace))

(define (name? x)
  (and (string? x)
       (not (string=? "" x))
       (andmap valid-name-char? (string->list x))))
(define (valid-name-char? c)
  (or (char-alphabetic? c)
      (char-numeric? c)
      (char=? #\/ c)
      (char=? #\- c)))

(define (power-of-two? i)
  (define x (/ (log i) (log 2)))
  (= x (ceiling x) (floor x)))
(module+ test
  (for ([i (in-range 10)])
    (check-true (power-of-two? (expt 2 i))))
  (for ([i '(3 5 6 7 9 11 12 13 14 15)])
    (check-false (power-of-two? i))))
(define (dimension? a)
  (and (exact-nonnegative-integer? a)
       (power-of-two? a)
       (not (= a 1))))

(define (image? a)
  (and (bytes? a)
       (for/and ([i (in-bytes a)])
         (<= 0 i 9))))

(define (sized-vector/c len element/c)
  (apply vector/c (make-list len element/c)))
(define palette-color/c
  (sized-vector/c 4 byte?))

(provide/contract
 [dimension? (-> any/c boolean?)]
 [name? (-> any/c boolean?)]
 [valid-name-char? (-> char? boolean?)]
 [db? (-> any/c boolean?)]
 [load-db (-> path-string? db?)]
 [db-palettes (-> db? (listof name?))]
 [db-sprites (-> db? (listof name?))]
 [load-last (-> db? name?)]
 [last-save! (-> db? name? void?)]
 [struct sprite
         ([name name?]
          [width dimension?]
          [height dimension?]
          [images (vectorof image?)]
          [palettes (listof name?)])]
 [load-sprite (-> db? name? sprite?)]
 [sprite-save! (-> db? sprite? void?)]
 [struct palette
         ([name name?]
          [colors (sized-vector/c 10 palette-color/c)])]
 [palette-color->color% (-> palette-color/c (is-a?/c color%))]
 [palette-color%s (-> palette? (sized-vector/c 10 (is-a?/c color%)))]
 [load-palette (-> db? name? palette?)]
 [palette-save! (-> db? palette? void?)])
