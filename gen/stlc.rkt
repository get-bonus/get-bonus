#lang racket
(require redex/reduction-semantics)

;; XXX Use De Bruijin indices and maybe use that to cache the results
;; of generating terms of some size

(define-language STLC
  [e x
     ()
     i
     (e : T)
     (e e)
     (lambda (x) e)
     add1
     
     (inl e)
     (inr e)
     (case e
       [(inl x) => e]
       [(inr x) => e])
     
     (pair e e)
     (fst e)
     (snd e)]
  [i integer]
  [T (unit)
     (int)
     (var x)
     (-> T T)
     (* T T)
     (+ T T)]
  [G ([x T] ...)]
  [RC ([T T] ...)]
  [C RC
     (delayed G x e)
     (U C ...)]

  [MT #f
      T]
  [MG #f
      G]
  [x variable-not-otherwise-mentioned])

(define-metafunction STLC
  ty-cons : G x e -> C
  [(ty-cons G x_l x)
   ([(var x_l) (lookup G x)])]
  [(ty-cons G x_l ())
   ([(var x_l) (unit)])]
  [(ty-cons G x_l i)
   ([(var x_l) (int)])]
  [(ty-cons G x_l (e : T))
   (U (ty-cons G x_l e)
      ([(var x_l) T]))]
  [(ty-cons G x_l (e_1 e_2))
   (U ([(var x_l1) (-> (var x_l2) (var x_l))])
      (delayed G x_l1 e_1)
      (delayed G x_l2 e_2))
   (where x_l1 ,(gensym 'operator))
   (where x_l2 ,(gensym 'operand))]
  [(ty-cons G x_l (lambda (x_a) e_b))
   (U ([(var x_l) (-> (var x_aa) (var x_lb))])
      (delayed (extend G x_a (var x_aa)) x_lb e_b))
   (where x_aa ,(gensym (term x_a)))
   (where x_lb ,(gensym 'body))]
  [(ty-cons G x_l add1)
   ([(var x_l) (-> (int) (int))])]

  [(ty-cons G x_l (inl e))
   (U ([(var x_l) (+ (var x_le) (var x_lr))])
      (delayed G x_le e))
   (where x_le ,(gensym 'inl))
   (where x_lr ,(gensym 'inl-free))]
  [(ty-cons G x_l (inr e))
   (U ([(var x_l) (+ (var x_lr) (var x_le))])
      (delayed G x_le e))
   (where x_le ,(gensym 'inr))
   (where x_lr ,(gensym 'inr-free))]
  [(ty-cons G x_l
            (case e_+
              [(inl x_+l) => e_l]
              [(inr x_+r) => e_r]))
   (U ([(var x_l) (var x_ll)]
       [(var x_l) (var x_lr)]
       [(var x_l+) (+ (var x_n+l) (var x_n+r))])
      (delayed G x_l+ e_+)
      (delayed (extend G x_+l (var x_n+l)) x_ll e_l)
      (delayed (extend G x_+r (var x_n+r)) x_lr e_r))
   (where x_n+l ,(gensym (term x_+l)))
   (where x_n+r ,(gensym (term x_+r)))
   (where x_l+ ,(gensym 'case+))
   (where x_ll ,(gensym 'case_left))
   (where x_lr ,(gensym 'case_right))]

  [(ty-cons G x_l (pair e_l e_r))
   (U ([(var x_l) (* (var x_ll) (var x_lr))])
      (delayed G x_ll e_l)
      (delayed G x_lr e_r))
   (where x_ll ,(gensym 'pair_left))
   (where x_lr ,(gensym 'pair_right))]
  [(ty-cons G x_l (fst e_*))
   (U ([(var x_l*) (* (var x_l) (var x_r))])
      (delayed G x_l* e_*))
   (where x_l* ,(gensym 'pair))
   (where x_r ,(gensym 'pair_right))]
  [(ty-cons G x_r (snd e_*))
   (U ([(var x_l*) (* (var x_l) (var x_r))])
      (delayed G x_l* e_*))
   (where x_l* ,(gensym 'pair))
   (where x_l ,(gensym 'pair_left))])

(define-metafunction STLC
  extend : G x T -> G
  [(extend ([x_0 T_0] ... [x_i T_i] [x_i+1 T_i+1] ...) x_i T)
   ([x_0 T_0] ... [x_i T] [x_i+1 T_i+1] ...)]
  [(extend ([x_1 T_1] ...) x_0 T_0)
   ([x_0 T_0] [x_1 T_1] ...)])

(define-metafunction STLC
  lookup : G x -> T
  [(lookup ([x_0 T_0] ... [x_i T_i] [x_i+1 T_i+1] ...) x_i) 
   T_i]
  [(lookup G x)
   ,(error 'lookup "Unbound identifier ~e" (term x))])

(require (only-in unstable/match ==))
(define (doesnt-match x Ts)
  (for/and ([T (in-list Ts)])
           (match T
             [(list 'var (== x))
              #f]
             [_
              #t])))

(define-metafunction STLC
  dual-lookup : G x -> T
  [(dual-lookup ([x_0 T_0] ... [x_i T_i] [x_i+1 T_i+1] ...) x_i) 
   T_i]
  [(dual-lookup ([x_0 T_0] ... [x_t (var x_i)] [x_i+1 T_i+1] ...) x_i) 
   (var x_t)
   (side-condition (doesnt-match (term x_i) (term (T_0 ...))))]
  [(dual-lookup G x)
   ,(error 'lookup "Unbound identifier ~e" (term x))])

(define unbound-id-exn?
  (lambda (x)
    (and (exn:fail? x)
         (regexp-match #rx"^lookup: Unbound identifier "
                       (exn-message x)))))

;; Is ty-cons defined on the language, when there are no free identifiers
(redex-check STLC e
             (with-handlers ([unbound-id-exn?
                              (lambda (x)
                                #t)])
               (term (ty-cons () top e)))
             #:attempts 200)             

(define-metafunction STLC
  T-subst : x T T -> T
  [(T-subst x T (var x))
   T]
  [(T-subst x T (var x_y))
   (var x_y)]
  [(T-subst x T (any T_1 ...))
   (any (T-subst x T T_1)
        ...)])

(define-metafunction STLC
  T-subst* : G T -> T
  [(T-subst* () T) T]
  [(T-subst* ([x_1 T_1] [x_n T_n] ...) T)
   (T-subst* ([x_n T_n] ...) (T-subst x_1 T_1 T))])

(define-metafunction STLC
  RC-subst : G RC -> RC
  [(RC-subst G ([T_l T_r] ...))
   ([(T-subst* G T_l)
     (T-subst* G T_r)]
    ...)])

(define-metafunction STLC
  unify : G RC -> MG
  ;; Done?
  [(unify G ())
   G]

  ;; Ignore equal ids
  [(unify G ([(var x_1) (var x_1)]
             [T_ml T_mr] ...))
   (unify G ([T_ml T_mr] ...))]

  ;; Push substitution
  [(unify ([x_g T_g] ...)
          ([(var x) T]
           [T_ml T_mr] ...))
   (unify (extend ([x_g (T-subst x T T_g)] ...) x T)
          ([(T-subst x T T_ml) (T-subst x T T_mr)] ...))]

  ;; Swap sides
  [(unify G ([T (var x)]
             [T_ml T_mr] ...))
   (unify G ([(var x) T]
             [T_ml T_mr] ...))]

  ;; Constructory
  [(unify G ([(any_1 T_l ...)
              (any_1 T_r ...)]
             [T_ml T_mr]
             ...))
   (unify G ([T_l T_r] ...
             [T_ml T_mr]
             ...))]

  [(unify G RC)
   #f])

(define-metafunction STLC
  unify* : G C -> MG
  ;; Actually do unification
  [(unify* G RC)
   (unify G (RC-subst G RC))]
  ;; Force promises
  [(unify* G (delayed G_p x_p e_p))
   (unify* G (ty-cons G_p x_p e_p))]
  ;; Unroll unions
  [(unify* G (U))
   G]
  [(unify* G (U (U C_0 ...) C_1 ...))
   (unify* G (U C_0 ... C_1 ...))]
  [(unify* G (U C C_1 ...))
   (unify* G_0 (U C_1 ...))
   (where G_0 (unify* G C))]
  [(unify* G C)
   #f])

(define-metafunction STLC
  typeof : e -> MT
  [(typeof e)
   (dual-lookup G_0 x_top)
   (where x_top ,(gensym 'top))
   (where G_0 (unify* () (ty-cons () x_top e)))]
  [(typeof e)
   #f])

(define (alpha-equal? T1 T2)
  (define T1->T2 (make-hasheq))
  (let loop ([t1 T1] [t2 T2])
    (match* (t1 t2)
            [((? symbol?) (? symbol?))
             (if (hash-has-key? T1->T2 t1)
                 (eq? (hash-ref T1->T2 t1) t2)
                 (hash-set! T1->T2 t1 t2))]
            [((cons t1f t1r)
              (cons t2f t2r))
             (and (loop t1f t2f)
                  (loop t1r t2r))]
            [((list) (list))
             #t]
            [(_ _)
             #f])))

(define-syntax-rule (tt e T)
  (test-predicate
   (procedure-rename (curry alpha-equal? (term T))
                     (string->symbol (format "alpha-equal to ~v" (term T))))
   (with-handlers ([unbound-id-exn? (lambda (x) #f)])
     (term (typeof e)))))

(define-syntax-rule (tf e)
  (test-equal (term (typeof e))
              #f))

;; x
(tt ((lambda (x) x) 7) (int))

;; ()
(tt () (unit))

;; i
(tt 1 (int))

;; (e : T)
(tt (1 : (int)) (int))
(tf (1 : (-> (int) (int))))

;; (e e)
(tt (add1 1) (int))
(tf (add1 add1))
(tt ((lambda (f) (f 1)) add1) (int))

;; lambda
(tt (lambda (x) x)
    (-> (var A) (var A)))

;; add1
(tt add1 (-> (int) (int)))

;; inl
(tt (inl 1) (+ (int) (var A)))

;; inr
(tt (inr 1) (+ (var A) (int)))

;; case
(tt (case (inl 1)
      [(inl x) => (add1 x)]
      [(inr x) => (add1 (add1 x))])
    (int))
(tt (case (inr 1)
      [(inl x) => (add1 x)]
      [(inr x) => (add1 (add1 x))])
    (int))
(tf (case (inl add1)
      [(inl x) => (add1 x)]
      [(inr x) => (add1 (add1 x))]))
(tf (case (inr add1)
      [(inl x) => (add1 x)]
      [(inr x) => (add1 (add1 x))]))
(tt (case (inr add1)
      [(inl x) => x]
      [(inr x) => add1])
    (-> (int) (int)))
(tf (case (inr add1)
      [(inl x) => 1]
      [(inr x) => add1]))

;; pair
(tt (pair 1 1) (* (int) (int)))
(tt (pair 1 add1) (* (int) (-> (int) (int))))

;; fst
(tt (fst (pair 1 1)) (int))
(tt (fst (pair 1 add1)) (int))

;; snd
(tt (snd (pair 1 1)) (int))
(tt (snd (pair add1 1)) (int))

;; Implement lists
(define omega
  (term (lambda (x) (x x))))
(define Omega
  (term (,omega ,omega)))
(define diverge
  (term ,Omega))
(define !empty
  (term (inr ())))
(define !cons
  (term (lambda (x)
          (lambda (y)
            (inl (pair x y))))))
(define !car
  (term (lambda (x)
          (case x
            [(inl x) =>
             (fst x)]
            [(inr x) =>
             ,diverge]))))
(define !cdr
  (term (lambda (x)
          (case x
            [(inl x) =>
             (snd x)]
            [(inr x) =>
             ,diverge]))))
(define Y
  (term (lambda (rf)
          ((lambda (f)
            (rf (lambda (x)
                  ((f f) x))))
          (lambda (f)
            (rf (lambda (x)
                  ((f f) x))))))))            
(define !map
  (term (,Y
         (lambda (map)
           (lambda (f)
             (lambda (l)
               (case l
                 [(inl x) =>
                  (inl
                   (pair (f (fst x))
                         ((map f) (snd x))))]
                 [(inr x) =>
                  (inr ())])))))))

(when #f
  (tt ,diverge
      (var A))
  (tt (lambda (x) ,diverge)
      (-> (var A) (var B)))
  (tt (,!car ,!empty)
      (var A))
  (tt ((,!cons 1) ,!empty)
      (+ (* (int) (+ (var A) (unit))) (var B)))
  (tt (,!car ((,!cons 1) ,!empty))
      (int))
  (tt ((,!map add1) ((,!cons 1) ,!empty))
      (+ (* (int) (+ (* (int) (var A)) (unit))) (unit))))

;; Random generation
(require racket/stream)

(define (stream-append-map f s)
  (stream-of-stream-of-X->stream-of-X/breadth
   (stream-map f s)))

(define (random-list-ref l)
  (list-ref l (random (length l))))
(define (random-var G)
  (stream-map first (shuffle G)))

(define (random-int)
  (random 64))

(define-syntax-rule (random-choice j [0-opt ...] [n-opt ...])
  (random-choice* j
                  (list (lambda () 0-opt) ...)
                  (list (lambda () (list n-opt)) ...)))
(define (random-choice* j 0-opt-thunks n-opt-thunks)
  (stream-append-map
   (lambda (t) (t))
   (shuffle
    (if (positive? j)
        (append 0-opt-thunks n-opt-thunks)
        0-opt-thunks))))

(define (random-id)
  (gensym 'random))
(define (random-id? x)
  (and (symbol? x)
       (regexp-match #rx"^random" (symbol->string x))))

(define (unravel-random-e/once G juice)
  (random-choice
   juice

   [(random-var G )
    (list (term ()))
    (list (random-int))
    (list (term add1))]

   [(term (,(random-id) ,(random-id)))
    (term (lambda (,(gensym 'x)) ,(random-id)))
    ;; XXX handle random T
    ;;(term (,(random-id) : (var ,(random-id))))
    (term (inl ,(random-id)))
    (term (inr ,(random-id)))
    (term
     (case ,(random-id)
       [(inl x) => ,(random-id)]
       [(inr x) => ,(random-id)]))
    (term (pair ,(random-id) ,(random-id)))
    (term (fst ,(random-id)))
    (term (snd ,(random-id)))]))

(define-metafunction STLC
  unify*-modulo-delays : G C -> (MG (C ...))
  ;; Actually do unification
  [(unify*-modulo-delays G RC)
   ((unify G (RC-subst G RC)) ())]
  ;; Force promises
  [(unify*-modulo-delays G (delayed G_p x_p e_p))
   (G ((delayed G_p x_p e_p)))]
  ;; Unroll unions
  [(unify*-modulo-delays G (U))
   (G ())]
  [(unify*-modulo-delays G (U (U C_0 ...) C_1 ...))
   (unify*-modulo-delays G (U C_0 ... C_1 ...))]
  [(unify*-modulo-delays G (U C C_1 ...))
   (G_s (C_f ... C_s ...))
   (where (G_f (C_f ...)) (unify*-modulo-delays G C))
   (where (G_s (C_s ...)) (unify*-modulo-delays G_f (U C_1 ...)))]
  ;; Error
  [(unify*-modulo-delays G C)
   (#f ())])

(define (subst x t tp)
  (let loop ([tp tp])
    (cond
     [(eq? x tp) t]
     [(list? tp)
      (map (curry subst x t) tp)]
     [else tp])))
  
(define (random-terms juice)
  (define top (gensym 'top))
  (values top
          (stream-of-all-ok-complete-terms
           juice
           (term ())
           (term (delayed () ,top ,(random-id))))))

(define (stream-of-all-random-terms juice principal-typing constraint)
  (match-define (list 'delayed id->type label (? random-id?)) constraint)
  (unravel-random-e/once id->type juice))

(define (stream-of-all-ok-random-terms+typings+delays juice principal-typing constraint)
  (match-define (list 'delayed id->type label (? random-id?)) constraint)
  (stream-filter-map
   (lambda (new-term)
     (match
         (term 
          (unify*-modulo-delays ,principal-typing
                                (ty-cons ,id->type ,label ,new-term)))
       [(list #f _)
        #f]
       [(list new-principal-typing delays)
        (vector new-term new-principal-typing delays)]))
   (stream-of-all-random-terms juice principal-typing constraint)))

(define (stream-filter-map f s)
  (stream-filter (lambda (x) x)
                 (stream-map f s)))

;; Removes one delay
(define (stream-of-rt/t/ds->stream-of-stream-of-rt/t/ds juice s)
  (stream-map
   (match-lambda
    [(vector new-term new-principal-typing (list))
     (list (vector new-term new-principal-typing (list)))]
    [(vector new-term new-principal-typing (list-rest this-delay delays))
     (define this-random (fourth this-delay))
     (stream-map
      (match-lambda
       [(vector next-term next-principal-typing)
        (vector (subst this-random next-term new-term)
                next-principal-typing
                delays)])
      (stream-of-all-ok-complete-terms (sub1 juice) new-principal-typing this-delay))])
   s))

(define (stream-of-stream-of-X->stream-of-X/breadth s-s)
  (let loop ([fst-pass s-s]
             [snd-pass empty-stream])
    (cond
     [(stream-empty? fst-pass)
      (if (stream-empty? snd-pass)
          empty-stream
          (loop snd-pass empty-stream))]
     [else
      (define s-e (stream-first fst-pass))
      (if (stream-empty? s-e)
          (loop (stream-rest fst-pass)
                snd-pass)
          (stream-cons (stream-first s-e)
                       (loop (stream-rest fst-pass)
                             (stream-cons (stream-rest s-e)
                                          snd-pass))))])))

(define MAXIMUM-DELAYS 3) ;; XXX Really ugly

(define (stream-of-all-ok-complete-terms juice principal-typing constraint)
  (define s
    (stream-of-all-ok-random-terms+typings+delays juice principal-typing constraint))
  (define unfolded-s
    (for/fold ([s s])
        ([i (in-range MAXIMUM-DELAYS)])
      (stream-of-stream-of-X->stream-of-X/breadth
       (stream-of-rt/t/ds->stream-of-stream-of-rt/t/ds juice s))))
  (stream-map
   (match-lambda
    [(vector new-term new-principal-typing (list))
     (vector new-term new-principal-typing)])
   unfolded-s))

(printf "Random\n")
(define-values (top-id term-s) (random-terms 2))
(for/fold ([last (current-inexact-milliseconds)])
    ([a (in-stream term-s)]
     [i (in-range 10)])
     (match-define (vector t ty) a)
     (define this (current-inexact-milliseconds))
     (printf "~v>> ~v : ~v\n"
             (- this last)
             t
             (term (dual-lookup ,ty ,top-id)))
     this)
