#lang racket/base
(require (for-syntax racket/base)
         racket/list
         racket/contract
         racket/function
         racket/splicing
         racket/match
         racket/stxparam
         racket/port
         racket/path
         racket/file
         racket/system
         racket/cmdline
         racket/runtime-path)

(define target?
  path-string?)
(define deps?
  (or/c path-string?
        exact-nonnegative-integer?
        (listof (recursive-contract deps?))))

(define-syntax-parameter current-rules
  (λ (stx)
    (raise-syntax-error 'current-rules "Illegal outside jake" stx)))

(struct rule-defn (fun))

(define-syntax-rule (rule target deps . cmds)
  (set! current-rules
        (cons (rule-defn
               (contract
                (-> target? (or/c false/c (cons/c deps? (-> any))))
                (match-lambda
                 [target
                  (cons deps
                        (λ () (void) . cmds))]
                 [_
                  #f])
                'pos 'neg))
              current-rules)))

(define-syntax (jake stx)
  (syntax-case stx ()
    [(_ . body)
     (quasisyntax/loc stx
       (begin
         (define (run-main)
           (define rules empty)
           (splicing-syntax-parameterize
            ([current-rules (make-rename-transformer #'rules)])
            . body)
           (jake-main rules))
         (module+ main
           #,(datum->syntax stx (list #'define-runtime-path 'root #'"."))
           (parameterize ([current-directory #,(datum->syntax stx 'root)])
             (run-main)))))]))

(define (jake-main rules)
  (command-line
   #:program "jake"
   #:args targets
   (match targets
     [(list)
      (do-jake rules rules "all")]
     [(list target ...)
      (for-each (curry do-jake rules rules) target)])))

(define (mtime def p)
  (match p
    [(? exact-nonnegative-integer? n)
     n]
    [(? path-string? p)
     (if (or (file-exists? p)
             (directory-exists? p))
       (file-or-directory-modify-seconds p)
       def)]))

(define (tree-for-each f l)
  (match l
    [(list)
     (void)]
    [(list-rest l r)
     (tree-for-each f l)
     (tree-for-each f r)]
    [x
     (f x)]))

(define (tree-argmax f l)
  (apply max (map f (flatten l))))

(define IGNORED (make-hash))

(define (do-jake top-rs rs target)
  (match rs
    [(list)
     (unless (hash-has-key? IGNORED target)
       (hash-set! IGNORED target #t)
       (eprintf "jake: target ignored: ~e\n" target))
     (void)]
    [(list-rest (rule-defn f) rs)
     (match (f target)
       [(cons deps build)
        (tree-for-each
         (curry do-jake top-rs top-rs)
         deps)
        (define most-recent
          (tree-argmax (curry mtime +inf.0) deps))
        (define target-mtime
          (mtime -inf.0 target))
        (when (> most-recent target-mtime)
          (eprintf "jake: rebuilding ~e\n"
                   target)
          (build))]
       [#f
        (do-jake top-rs rs target)])]))

(define (shell fmt . args)
  (with-output-to-string
    (λ ()
      (system (apply format fmt args)))))

(provide
 jake
 rule
 shell)
