#lang racket/base
(require jake
         racket/list
         racket/match
         racket/path
         racket/system
         racket/file
         setup/dirs)

(define (systemf fmt . args)
  (define cmd (apply format fmt args))
  (displayln cmd)
  (system cmd))

(jake
 (define r-pth "r")

 (rule "all"
       (list "r.png"))

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
                          (λ (x) (regexp-replace #rx#"_rkt.zo$" x #".rkt"))
                          path->bytes
                          file-name-from-path)
                 file)
            zo-pth)
       (list (build-path src file)
             (let ()
               (define dep-pth
                 (path-replace-suffix zo-pth #".dep"))
               (if (file-exists? dep-pth)
                 (map (match-lambda
                       [(list-rest 'collects cp)
                        (compiled
                         (apply build-path (find-collects-dir)
                                (map bytes->path cp)))
                        empty])
                      (list-tail (file->value dep-pth) 2))
                 empty))
             racket-pth)
       (system* raco-pth "make" (build-path src file)))

 (define FONT-SIZES (list 8 10 12 14 16 20))
 (define FONT-FAMILIES (list "decorative" "roman" "script" "swiss" "modern"))
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
               [letter (in-string "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz")])
     (build-path dir (format "~a.png" letter))))

 (rule (or "r.png" "r.rkt")
       (list (compiled "tools/texture-atlas.rkt")
             FONT-FILES)
       (apply system* racket-pth
              "-t"
              "tools/texture-atlas.rkt"
              "r.png" "r.rkt"
              FONT-FILES)))
