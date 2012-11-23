#lang racket/base
(require racket/file
         racket/list
         racket/match)

(struct db (pth id->generator [cards #:mutable]) #:prefab)

(struct card (id sort data history) #:prefab)

(define (insert c cs)
  (match cs
    [(list)
     (list c)]
    [(list-rest d cs)
     (if (< (card-sort c)
            (card-sort d))
       (list* c d cs)
       (list* d (insert c cs)))]))

(struct attempt (start end score data) #:prefab)

(define (srs pth)
  (unless (file-exists? pth)
    (write-to-file empty pth))
  (db pth (make-hasheq) (file->value pth)))

(define (srs-set-cards! a-db new-cards)
  (match-define (struct db (pth _ _)) a-db)
  (set-db-cards! a-db new-cards)
  (write-to-file new-cards pth #:exists 'replace))

(define (srs-add-card! a-db card)
  (srs-set-cards! a-db (insert card (db-cards a-db))))

(define (set-srs-generator! a-db id fun)
  (match-define (struct db (_ id->generator cards)) a-db)
  (hash-set! id->generator id fun)
  (unless (findf (λ (c) (eq? id (card-id c))) cards)
    (srs-add-card! a-db (card id 1.0 #f empty))))

(define (srs-next a-db start k)
  (match-define
   (struct db (_ id->generator (and cards (list-rest fst rst))))
   a-db)
  (match-define (struct card (fst-id fst-sort fst-data fst-history)) fst)
  (cond
    ;; If the next card is a generator, then make the new card and
    ;; repeat
    [(symbol? fst-id)
     (define new-data ((hash-ref id->generator fst-id)))
     (define new-id (length cards))

     (define new-fst-card
       (card fst-id (* 2.0 fst-sort) fst-data
             (cons (attempt start start 1.0 #f)
                   fst-history)))

     (srs-set-cards! a-db
                     (cons (card new-id 1.0 new-data empty)
                           (insert new-fst-card rst)))
     (srs-next a-db start k)]
    [else
     (define an-attempt (k fst))
     (define new-fst-card
       (card fst-id
             (* (balance (attempt-score an-attempt))
                fst-sort)
             fst-data
             (cons an-attempt
                   fst-history)))
     (srs-set-cards! a-db (insert new-fst-card rst))]))

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

  (define t (make-temporary-file))
  (delete-file t)

  (define next
    (let ()
      (define x 0)
      (λ ()
        (begin0 x (set! x (add1 x))))))

  (define db (srs t))
  (set-srs-generator! db 'tennis (λ () (vector 'tennis (next))))
  (set-srs-generator! db 'maze (λ () (vector 'maze (next))))
  (check-equal? (db-cards db)
                (list (card 'tennis 1.0 #f empty)
                      (card 'maze 1.0 #f empty)))

  (srs-next
   db 0
   (λ (c0)
     (check-equal? c0 (card 2 1.0 (vector 'tennis 0) empty))
     (check-equal? (db-cards db)
                   (list (card 2 1.0 (vector 'tennis 0) empty)
                         (card 'maze 1.0 #f empty)
                         (card 'tennis 2.0 #f
                               (list (attempt 0 0 1.0 #f)))))
     (attempt 0 1 1.0 0)))
  (check-equal? (db-cards db)
                (list (card 'maze 1.0 #f empty)
                      (card 'tennis 2.0 #f
                            (list (attempt 0 0 1.0 #f)))
                      (card 2 2.0 (vector 'tennis 0)
                            (list (attempt 0 1 1.0 0)))))

  (define new-db (srs t))
  (check-equal? (db-cards new-db)
                (list (card 'maze 1.0 #f empty)
                      (card 'tennis 2.0 #f
                            (list (attempt 0 0 1.0 #f)))
                      (card 2 2.0 (vector 'tennis 0)
                            (list (attempt 0 1 1.0 0))))))
