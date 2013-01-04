#lang racket/base
(require racket/file
         racket/list
         racket/match
         racket/math
         racket/contract
         racket/pretty)

(struct db (pth id->generator [next-id #:mutable] [cards #:mutable]) #:prefab)

(define (insert c cs)
  (match cs
    [(list)
     (list c)]
    [(list-rest d cs)
     (if (<= (card-sort c)
             (card-sort d))
       (list* c d cs)
       (list* d (insert c cs)))]))

(struct card (id sort data history) #:prefab)

(struct attempt (start end score data) #:prefab)

(define (srs pth)
  (unless (file-exists? pth)
    (write-to-file empty pth))
  (define cards (file->value pth))
  (define next-id
    (add1
     (for/fold ([m -1])
         ([c (in-list cards)])
       (define i (card-id c))
       (if (number? i)
         (max m i)
         m))))
  (db pth (make-hasheq) next-id cards))

(define (srs-cards a-db)
  (match-define (db _ _ _ cards) a-db)
  cards)

(define (srs-generator-card a-db id)
  (match-define (db _ _ _ cards) a-db)
  (findf (λ (c) (eq? id (card-id c))) cards))

(define (set-srs-generator! a-db id fun)
  (match-define (db _ id->generator _ cards) a-db)
  (hash-set! id->generator id fun)
  (unless (srs-generator-card a-db id)
    (srs-add-card! a-db (card id 1.0 #f empty))))

(define (srs-set-cards! a-db new-cards)
  (match-define (db pth _ _ _) a-db)
  (set-db-cards! a-db new-cards)
  (with-output-to-file pth
    #:exists 'replace
    (λ ()
      (pretty-write new-cards))))

(define (srs-add-card! a-db a-card)
  (srs-set-cards! a-db (insert a-card (db-cards a-db))))

(define (srs-generate! a-db id get-time)
  (match-define (db _ id->generator next-id cards) a-db)
  (define id-card (srs-generator-card a-db id))
  (define generator (hash-ref id->generator id))
  (define start (get-time))
  (define-values (new-data gen-adata) (generator))
  (cond
    [(memf (λ (c) (equal? new-data (card-data c))) cards)
     (srs-generate! a-db id get-time)]
    [else
     (define end (get-time))
     (define new-id next-id)
     (define new-card (card new-id 1.0 new-data empty))
     (set-db-next-id! a-db (add1 next-id))
     (srs-card-attempt! a-db id-card (attempt start end 1.0 gen-adata))
     (srs-add-card! a-db new-card)
     new-card]))

(define (mapmax f l)
  (match l
    [(list)
     -inf.0]
    [(list-rest fst l)
     (max (f fst) (mapmax f l))]))

(define (srs-card-attempt! a-db a-card an-attempt)
  (match-define (db _ _ _ cards) a-db)
  (match-define (card id sort cdata history) a-card)
  (match-define (attempt _ _ this-score _) an-attempt)
  (define best-score-or-inf (mapmax attempt-score history))
  (define best-score
    (if (infinite? best-score-or-inf)
      this-score
      best-score-or-inf))
  (define cards/no-id (remove id cards (λ (v c) (eq? v (card-id c)))))
  (define new-sort
    (if (zero? best-score)
      (* 2.0 sort)
      (* (balance (/ this-score best-score))
         sort)))
  (define new-card (card id new-sort cdata (cons an-attempt history)))
  (define new-cards (insert new-card cards/no-id))
  (srs-set-cards! a-db new-cards))

(define (balance x)
  (cond
    [(<= x 0.5)
     (+ (* 1.0 x) 0.5)]
    [(<= x 1.0)
     (* 2.0 x)]
    [else
     (+ x 1.0)]))

(module+ test
  (require rackunit)

  (check-equal? (balance 0.0) 0.5)
  (check-equal? (balance 0.5) 1.0)
  (check-equal? (balance 1.0) 2.0)

  (define next
    (let ()
      (define x 0)
      (λ ()
        (begin0 x (set! x (add1 x))))))

  (define (srs-next a-db start k)
    (match-define (db _ _ _ (list-rest fst _)) a-db)
    (define it
      (match (card-id fst)
        [(? symbol? id)
         (srs-generate! a-db id (λ () start))]
        [_
         fst]))
    (srs-card-attempt! a-db it (k it)))

  (define t (make-temporary-file))
  (delete-file t)

  (define a-db (srs t))
  (set-srs-generator! a-db 'maze
                      (λ () (values (vector 'maze
                                            (next))
                                    #f)))
  (set-srs-generator! a-db 'tennis
                      (λ () (values (vector 'tennis (next))
                                    #f)))
  (test-equal? "Initial database"
               (srs-cards a-db)
               (list (card 'tennis 1.0 #f empty)
                     (card 'maze 1.0 #f empty)))

  (srs-next
   a-db 0
   (λ (c0)
     (test-equal? "First card" c0 (card 0 1.0 (vector 'tennis 0) empty))
     (test-equal? "Database after generate"
                  (srs-cards a-db)
                  (list (card 0 1.0 (vector 'tennis 0) empty)
                        (card 'maze 1.0 #f empty)
                        (card 'tennis 2.0 #f
                              (list (attempt 0 0 1.0 #f)))))
     (attempt 0 1 1.0 0)))
  (test-equal? "Database after first attempt"
               (srs-cards a-db)
               (list (card 'maze 1.0 #f empty)
                     (card 0 2.0 (vector 'tennis 0)
                           (list (attempt 0 1 1.0 0)))
                     (card 'tennis 2.0 #f
                           (list (attempt 0 0 1.0 #f)))))

  (define new-a-db (srs t))
  (set-srs-generator! new-a-db 'maze
                      (λ () (values (vector 'maze
                                            (next))
                                    #f)))
  (set-srs-generator! new-a-db 'tennis
                      (λ () (values (vector 'tennis (next))
                                    #f)))
  (test-equal? "Database after re-read"
               (srs-cards new-a-db)
               (list (card 'maze 1.0 #f empty)
                     (card 0 2.0 (vector 'tennis 0)
                           (list (attempt 0 1 1.0 0)))
                     (card 'tennis 2.0 #f
                           (list (attempt 0 0 1.0 #f))))))

(provide
 (contract-out
  [rename db? srs? flat-contract]
  [srs (-> path-string? db?)]
  [set-srs-generator! (-> db? symbol? (-> (values any/c any/c)) void?)]
  [struct card ([id (or/c symbol? exact-nonnegative-integer?)]
                [sort real?]
                [data any/c]
                [history (listof attempt?)])]
  [struct attempt ([start real?]
                   [end real?]
                   [score real?]
                   [data any/c])]
  [srs-cards (-> db? (listof card?))]
  [srs-generate! (-> db? symbol? (-> real?) card?)]
  [srs-card-attempt! (-> db? card? attempt? void?)]))
