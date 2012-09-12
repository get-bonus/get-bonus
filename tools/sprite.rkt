#lang racket/base
(require racket/draw
         racket/class
         racket/file
         racket/match
         racket/cmdline
         racket/list
         racket/path
         file/convertible
         racket/runtime-path
         web-server/servlet-env
         web-server/servlet
         gb/lib/component)

;; XXX Have the user select the background color
;; XXX Have the user name the sprites, tag them, and sequence them
;; XXX Save the sprite dictionary
;; XXX Start from an existing sprite dictionary
;; XXX Add keyboard shortcuts
;; XXX Maybe put in hit shapes

(define-runtime-path source-dir ".")

(define (xplode p)
  (define tmp (make-temporary-file "~a.out" 'directory #f))

  (define bm (make-object bitmap% p 'png/alpha #f #t))
  (define objs (components bm (λ (p) (zero? (pixel-a p)))))

  (for ([s (in-vector objs)]
        [i (in-naturals)])
    (match-define (aabb x1 y1 x2 y2) s)
    (define w (- x2 x1 -1))
    (define h (- y2 y1 -1))

    (define new-bm (make-bitmap w h))
    (define bm-dc (new bitmap-dc% [bitmap new-bm]))

    (send bm-dc draw-bitmap-section
          bm 0 0
          x1 y1 w h)

    (send new-bm save-file (build-path tmp (format "~a.png" i)) 'png 100))

  (define (page/main req)
    (response/xexpr
     `(html
       (head
        (link ([rel "stylesheet"]
               [type "text/css"]
               [href "/sprite.css"])))
       (body
        (table
         ,@(for/list ([s (in-vector objs)]
                      [i (in-naturals)])
             `(tr
               (td (img ([src ,(format "~a.png" i)])))
               (td ,(format "~a" s)))))))))

  (define-values (main-dispatch main-url)
    (dispatch-rules
     [("sprites")
      page/main]))

  (printf "Ready\n")
  (serve/servlet
   main-dispatch
   #:command-line? #t
   #:extra-files-paths (list source-dir tmp)
   #:servlet-path (main-url page/main)
   #:servlet-regexp #rx""))

(module+ main
  (command-line
   #:program "sprite-xplode"
   #:args (file)
   (xplode file)))
