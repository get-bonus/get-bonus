#lang racket/base
(require jake
         racket/list
         racket/runtime-path
         racket/match
         racket/path
         racket/port
         racket/system
         racket/file
         setup/dirs
         gb/graphics/font-lib)
(module+ test
  (require rackunit))

(define (systemf fmt . args)
  (define cmd (apply format fmt args))
  (displayln cmd)
  (system cmd))

(define (compiled pth)
  (build-path (path-only pth)
              "compiled"
              (path-add-suffix (file-name-from-path pth) #".zo")))
(module+ test
  (check-equal? (path->string (compiled "a/b/c/foo.rkt"))
                "a/b/c/compiled/foo_rkt.zo"))

(define (zo->deps zo-pth)
  (define dep-pth
    (path-replace-suffix zo-pth #".dep"))
  (if (file-exists? dep-pth)
    (flatten
     (map (match-lambda
           [(? bytes? b)
            (compiled (bytes->path b))]
           [(list-rest 'collects cp)
            (compiled
             (apply build-path (find-collects-dir)
                    (map bytes->path cp)))
            empty])
          (list-tail (file->value dep-pth) 2)))
    empty))
(module+ test
  (check-equal? (zo->deps "meh.zo") empty)
  (define-runtime-path here ".")
  (check-equal?
   (zo->deps (build-path here "compiled/Makefile_rkt.zo"))
   (list (normalize-path
          (build-path here "jake/compiled/main_rkt.zo"))
         (normalize-path
          (build-path here "gb/graphics/compiled/font-lib_rkt.zo")))))

(define racket-pth
  (find-executable-path "racket"))
(define raco-pth
  (find-executable-path "raco"))

(define (zo->file zo-pth)
  (bytes->path
   (regexp-replace*
    #rx#"_"
    (path->bytes (path-replace-suffix
                  (file-name-from-path zo-pth)
                  #""))
    #".")))
(module+ test
  (check-equal? (zo->file (build-path here "compiled/Makefile_rkt.zo"))
                (build-path "Makefile.rkt")))

(define (zo->src some-pth)
  (match some-pth
    [(app path-only
          (and (? path-string?)
               (app explode-path
                    (and (app (compose path->bytes last)
                              #"compiled")
                         (app (compose (λ (x) (apply build-path x))
                                       reverse rest reverse)
                              src)))))
     src]
    [else
     #f]))
(module+ test
  (check-equal? (zo->src "/Makefile.rkt")
                #f)
  (check-equal? (zo->src "/compiled/Makefile_rkt.zo")
                (build-path "/")))

(define ->path
  (match-lambda
   [(? path? x)
    x]
   [(? path-string? x)
    (string->path x)]
   [(? bytes? x)
    (bytes->path x)]))
(module+ test
  (check-equal? (->path (build-path "/"))
                (build-path "/"))
  (check-equal? (->path "/")
                (build-path "/"))
  (check-equal? (->path #"/")
                (build-path "/")))

(jake
 (define ATLAS-FILES (list "r.bin.gz" "pal.png" "gb/graphics/r.rkt" "r.idx.bin.gz"))
 (rule "all" ATLAS-FILES)

 (rule (and (app zo->src (and (not #f) src))
            (app zo->file file)
            zo-pth)
       (list (build-path src file)
             (zo->deps zo-pth)
             racket-pth)
       (system* raco-pth "make" (build-path src file)))

 (define DB-IMGS
   (find-files (λ (x) (equal? #"img" (filename-extension x))) "db"))

 (rule (? (λ (x) (member x ATLAS-FILES)))
       (list (compiled "tools/apse/compile.rkt")
             DB-IMGS)
       (apply 
        system* racket-pth
        "-t"
        "tools/apse/compile.rkt"
        "db"
        ATLAS-FILES)
       (apply system* "/usr/bin/du" "-bhac" ATLAS-FILES)))
