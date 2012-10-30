#lang racket/base
(require racket/match
         racket/gui/base
         racket/class
         racket/system
         racket/file
         "fst.rkt")

(define (fst-graph f)
  (define png-pth (make-temporary-file "~a.png"))
  (define input-pth (make-temporary-file "~a.dot"))
  (with-output-to-file input-pth
    #:exists 'replace
    (λ ()
      (printf "digraph M {\n")
      (match-define (fst states input-alpha output-alpha start delta state->output) f)
      (for* ([(state input->next-state) (in-hash delta)]
             [(input next-state) (in-hash input->next-state)])
        (printf "~a -> ~a [label=\"~a\"]\n" state next-state input))
      (for ([(state output) (in-hash state->output)])
        (printf "~a [label=\"~a\"]\n" state output))
      (printf "~a [shape=doublecircle]\n" start)
      (printf "}\n")))
  (flush-output)
  (system* "/opt/local/bin/dot"
           "-Tpng"
           (path->string input-pth)
           "-o"
           (path->string png-pth))
  (begin0 (make-object image-snip% png-pth 'png)
          (delete-file png-pth)
          (delete-file input-pth)))

(provide fst-graph)
