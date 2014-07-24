#lang racket/base
(require racket/list
         racket/class
         racket/gui/base
         racket/format
         racket/match)
(module+ test
  (require rackunit))

(define (string-prefix-of? pre)
  (define pre-re (regexp (format "^~a" (regexp-quote pre))))
  (λ (str)
    (regexp-match pre-re str)))
(define (longest-common-prefix l)
  (define empty-trie (hasheq))
  (define empty-entry (cons #f empty-trie))
  (define (trie-add t w)
    (if (empty? w)
      t
      (hash-update t (first w)
                   (lambda (the-e)
                     (match-define (cons word? rest-t) the-e)
                     (cons (or word? (empty? (rest w)))
                           (trie-add rest-t (rest w))))
                   empty-entry)))
  (define (trie-add* s t)
    (trie-add t (string->list s)))
  (define l-trie (foldr trie-add* empty-trie l))
  (list->string
   (let loop ([t l-trie])
     (define c (hash-count t))
     (cond
       [(= c 1)
        (match-define (list (cons k (cons word? nt))) (hash->list t))
        (if word?
          (list k)
          (cons k (loop nt)))]
       [else
        empty]))))
(module+ test
  (check-equal? (longest-common-prefix (list "36" "36-0" "36-1"
                                             "36-2" "36-3"))
                "36"))

(define (create-mb-frame frame-label status-notify?
                         custom-handle-key)
  (define status-prompt-tag (make-continuation-prompt-tag 'status))
  (define-syntax-rule (set-status! expr)
    (let ()
      (define start (current-inexact-milliseconds))
      (define new-status
        (call-with-continuation-prompt
         (λ () expr)
         status-prompt-tag))
      (define end (current-inexact-milliseconds))
      (when new-status
        (send mw set-status-text
              (~a
               (if (status-notify?)
                 "!"
                 " ")
               " "
               (~a (- end start)
                   #:min-width 3
                   #:max-width 4
                   #:align 'right)
               "ms: "
               new-status)))))
  (define (throw-status v)
    (abort-current-continuation status-prompt-tag (λ () v)))

  ;; xxx does not allow nested minibuffer calls?
  (define minibuffer-run! #f)
  (define-syntax-rule (with-minibuffer ke e)
    (call-with-continuation-prompt
     (λ () (if minibuffer-run!
             (minibuffer-run! ke)
             e))
     minibuffer-prompt-tag))
  (define minibuffer-prompt-tag (make-continuation-prompt-tag 'minibuffer))
  (define (minibuffer-read prompt
                           #:valid-char? this-valid-char?
                           #:init [init ""]
                           #:auto-accept? [auto? #f]
                           #:accept-predicate? accept?
                           #:completions [orig-comps empty])
    (define (valid-char? c)
      (and (char? c) (this-valid-char? c)))
    (define comps
      (sort (sort orig-comps string-ci<?) < #:key string-length))
    (begin0
      (call/cc (λ (return-to-minibuffer-call)
                 (define input-so-far init)
                 (set! minibuffer-run!
                       (λ (ke)
                         (define prefix-comps
                           (filter
                            (string-prefix-of? input-so-far)
                            comps))
                         (define try-to-accept? #f)
                         (match (send ke get-key-code)
                           [#\return
                            (set! try-to-accept? #t)]
                           [#\tab
                            (if (empty? prefix-comps)
                              (bell)
                              (set! input-so-far
                                    (longest-common-prefix
                                     prefix-comps)))]
                           [(or #\backspace #\rubout)
                            (unless (string=? "" input-so-far)
                              (set! input-so-far
                                    (substring
                                     input-so-far 0
                                     (sub1
                                      (string-length input-so-far)))))]
                           [(? valid-char? c)
                            (set! input-so-far
                                  (string-append input-so-far
                                                 (string c)))]
                           [_ (void)])
                         (when (or try-to-accept? auto?)
                           (when (accept? input-so-far)
                             (return-to-minibuffer-call input-so-far)))
                         (send mw set-status-text
                               (~a prompt " > " input-so-far
                                   " ["
                                   (if (empty? prefix-comps)
                                     "no matches"
                                     (apply ~a
                                            (add-between prefix-comps " ")))
                                   "]"))
                         (when (or (eq? 'escape (send ke get-key-code))
                                   (and (send ke get-control-down)
                                        (eq? #\g (send ke get-key-code))))
                           (send mw set-status-text
                                 (~a prompt " > " "[CANCELED]"))
                           (set! minibuffer-run! #f))))
                 (abort-current-continuation minibuffer-prompt-tag void))
               minibuffer-prompt-tag)
      (set! minibuffer-run! #f)))

  (define (handle-key! ke)
    (with-minibuffer
     ke
     (set-status!
      (custom-handle-key ke))))

  (define model-lock (make-semaphore 1))

  (define apse-frame%
    (class* frame% ()
      (define/override (on-subwindow-char r e)
        (call-with-semaphore
         model-lock
         (λ ()
           (handle-key! e)))
        #t)
      (super-new)))

  (define-values (w h) (get-display-size #t))
  (define-values (x y) (get-display-left-top-inset #t))
  (define mw
    (new apse-frame% [label frame-label]
         [x 0] [y (* -1 y)]
         [width w] [height h]
         [style '(no-resize-border
                  no-caption
                  hide-menu-bar
                  no-system-menu)]))

  (values mw model-lock
          minibuffer-read throw-status
          (λ (first-status)
            (send mw create-status-line)
            (send mw show #t)
            (send mw focus)
            (set-status! first-status))))

(provide create-mb-frame)
