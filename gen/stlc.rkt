#lang racket
(require redex/reduction-semantics)

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
     (U C ...)]
  [x variable-not-otherwise-mentioned])

;; XXX delay sub generate
;; XXX interleave unification and generation
;; XXX generating on rand terms forces picking them

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
   (U (ty-cons G x_l1 e_1)
      (ty-cons G x_l2 e_2)
      ([(var x_l1) (-> (var x_l2) (var x_l))]))
   (where x_l1 ,(gensym 'operator))
   (where x_l2 ,(gensym 'operand))]
  [(ty-cons G x_l (lambda (x_a) e_b))
   (U (ty-cons (extend G x_a (var x_aa)) x_lb e_b)
      ([(var x_l) (-> (var x_aa) (var x_lb))]))
   (where x_aa ,(gensym (term x_a)))
   (where x_lb ,(gensym 'body))]
  [(ty-cons G x_l add1)
   ([(var x_l) (-> (int) (int))])]

  [(ty-cons G x_l (inl e))
   (U (ty-cons G x_le e)
      ([(var x_l) (+ (var x_le) (var x_lr))]))
   (where x_le ,(gensym 'inl))
   (where x_lr ,(gensym 'inl-free))]
  [(ty-cons G x_l (inr e))
   (U (ty-cons G x_le e)
      ([(var x_l) (+ (var x_lr) (var x_le))]))
   (where x_le ,(gensym 'inr))
   (where x_lr ,(gensym 'inr-free))]
  [(ty-cons G x_l
            (case e_+
              [(inl x_+l) => e_l]
              [(inr x_+r) => e_r]))
   (U (ty-cons G x_l+ e_+)
      (ty-cons (extend G x_+l (var x_n+l)) x_ll e_l)
      (ty-cons (extend G x_+r (var x_n+r)) x_lr e_r)
      ([(var x_l) (var x_ll)]
       [(var x_l) (var x_lr)]
       [(var x_l+) (+ (var x_n+l) (var x_n+r))]))
   (where x_n+l ,(gensym (term x_+l)))
   (where x_n+r ,(gensym (term x_+r)))
   (where x_l+ ,(gensym 'case+))
   (where x_ll ,(gensym 'case_left))
   (where x_lr ,(gensym 'case_right))]

  [(ty-cons G x_l (pair e_l e_r))
   (U (ty-cons G x_ll e_l)
      (ty-cons G x_lr e_r)
      ([(var x_l) (* (var x_ll) (var x_lr))]))
   (where x_ll ,(gensym 'pair_left))
   (where x_lr ,(gensym 'pair_right))]
  [(ty-cons G x_l (fst e_*))
   (U (ty-cons G x_l* e_*)
      ([(var x_l*) (* (var x_l) (var x_r))]))
   (where x_l* ,(gensym 'pair))
   (where x_r ,(gensym 'pair_right))]
  [(ty-cons G x_r (snd e_*))
   (U (ty-cons G x_l* e_*)
      ([(var x_l*) (* (var x_l) (var x_r))]))
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

(define cant-unify-exn?
  (lambda (x)
    (and (exn:fail? x)
         (regexp-match #rx"^unify: Cannot unify "
                       (exn-message x)))))

(define-metafunction STLC
  unify : G RC -> G
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
   ,(error 'unify "Cannot unify ~e" (term RC))])

(define-metafunction STLC
  unify* : G C -> G
  [(unify* G RC)
   (unify G (RC-subst G RC))]
  [(unify* G (U))
   G]
  [(unify* G (U (U C_0 ...) C_1 ...))
   (unify* G (U C_0 ... C_1 ...))]
  [(unify* G (U RC C_1 ...))
   (unify* (unify* G RC) (U C_1 ...))])

(define-metafunction STLC
  typeof : e -> T
  [(typeof e)
   (dual-lookup (unify* () (ty-cons () x_top e)) x_top)
   (where x_top ,(gensym 'top))])

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
  (test-equal (with-handlers ([cant-unify-exn? (lambda (x) #f)])
                (term (typeof e)))
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
    (+ (* (int) (+ (* (int) (+ (* (int) (var A)) (unit))) (unit))) (unit)))

;; Random generation
