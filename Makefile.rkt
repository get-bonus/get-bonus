#lang racket/base
(require jake
         racket/list
         racket/match
         racket/path
         racket/port
         racket/system
         racket/file
         setup/dirs
         gb/graphics/font-lib)

(define (systemf fmt . args)
  (define cmd (apply format fmt args))
  (displayln cmd)
  (system cmd))

(jake
 (define r-pth "r")
 (define r.free-pth "r.free")

 (rule "all"
       (list "r.free.png" "r.png" "gb/graphics/r.rkt"))

 (define racket-pth
   (find-executable-path "racket"))
 (define raco-pth
   (find-executable-path "raco"))

 (define (compiled pth)
   (build-path (path-only pth)
               "compiled"
               (path-add-suffix (file-name-from-path pth) #".zo")))

 (rule (and (app path-only
                 (and (? path-string?)
                      (app explode-path
                           (and (app (compose path->bytes last)
                                     #"compiled")
                                (app (compose (λ (x) (apply build-path x))
                                              reverse rest reverse)
                                     src)))))
            (app (compose bytes->path
                          (λ (x) (regexp-replace* #rx#"_" x #"."))
                          path->bytes
                          (λ (x) (path-replace-suffix x #""))
                          file-name-from-path)
                 file)
            zo-pth)
       (list (build-path src file)
             (let ()
               (define dep-pth
                 (path-replace-suffix zo-pth #".dep"))
               (if (file-exists? dep-pth)
                 (map (match-lambda
                       [(? bytes? b)
                        (compiled (bytes->path b))]
                       [(list-rest 'collects cp)
                        (compiled
                         (apply build-path (find-collects-dir)
                                (map bytes->path cp)))
                        empty])
                      (list-tail (file->value dep-pth) 2))
                 empty))
             racket-pth)
       (system* raco-pth "make" (build-path src file)))

 (define FONT-SIZES (list 7 8 10 12))
 (define FONT-FAMILIES (list "roman" "modern"))
 (define FONT-DIRS
   (for*/list ([size (in-list FONT-SIZES)]
               [family (in-list FONT-FAMILIES)])
     (build-path r-pth "fonts" family (number->string size))))

 (rule (app (compose (λ (x) (map path->string x)) explode-path)
            (list (== r-pth) "fonts" family size _))
       (list (compiled "tools/make-font.rkt"))
       (system* racket-pth "-t" "tools/make-font.rkt" r-pth family size))

 (define FONT-FILES
   (for*/list ([dir (in-list FONT-DIRS)]
               [letter (in-range CHAR-START (add1 CHAR-END))])
     (build-path dir (format "~a.png" letter))))

 (define r.src-pth
   "r.src")

 (define SPRITE-DEFS
   (find-files (λ (x) (equal? #"rkt" (filename-extension x))) r.src-pth))
 (define SPRITE-LISTS
   (map (λ (x) (path-replace-suffix x #".rktd")) SPRITE-DEFS))

 (rule (and (app (compose (λ (x) (map path->string x)) explode-path)
                 (list (== r.src-pth) inner ... (app filename-extension #"rktd")))
            sprite-list)
       (list (compiled "tools/sprite-digest.rkt")
             (compiled (path-replace-suffix sprite-list #".rkt"))
             (path-replace-suffix sprite-list #""))
       (system* racket-pth
                "-t"
                (path-replace-suffix sprite-list #".rkt")
                "--"
                (path-replace-suffix sprite-list #"")
                sprite-list
                r-pth)
       (when (member "copyrighted" inner)
         (for ([some-sprite (in-list (file->list sprite-list))])
           (system* racket-pth
                    "-t"
                    "tools/sprite-digest.rkt"
                    "--"
                    (build-path r-pth some-sprite)
                    (build-path r.free-pth some-sprite)))))

 (define ->path
   (match-lambda
    [(? path? x)
     x]
    [(? path-string? x)
     (string->path x)]
    [(? bytes? x)
     (bytes->path x)]))

 (rule (or "r.free.png" "r.png" "gb/graphics/r.rkt")
       (list (compiled "tools/texture-atlas.rkt")
             FONT-FILES
             SPRITE-LISTS)
       (apply system* racket-pth
              "-t"
              "tools/texture-atlas.rkt"
              "r.free.png" "r.png" "gb/graphics/r.rkt"
              r-pth r.free-pth
              (map (λ (x)
                     (regexp-replace #rx"^r/" (path->string (->path x)) ""))
                   (flatten (list FONT-FILES
                                  (map file->list
                                       SPRITE-LISTS)))))))
