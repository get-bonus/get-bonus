#lang racket
(require redex/reduction-semantics)

(define-language STLC
  [e x
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
  [T int
     (-> T T)
     (* T T)
     (+ T T)]
  [G ([x T] ...)]
  [x variable-not-otherwise-mentioned])

(define-relation STLC
  typeof<= ⊆ G × e × T

  [(typeof<= G (lambda (x) e) (-> T_1 T_2))
   (typeof<= (extend G x T_1) e T_2)]

  [(typeof<= G (inl e) (+ T_e T_1))
   (typeof=> G e T_e)]
  [(typeof<= G (inr e) (+ T_1 T_e))
   (typeof=> G e T_e)]

  [(typeof<= G e T)
   (typeof=> G e T)])
  
(define-judgment-form STLC
  #:mode (typeof=> I I O)
  #:contract (typeof=> G e T)
  [(typeof=> G x T)
   (where T (lookup G x))]
  [(typeof=> G i int)]
  [(typeof=> G add1 (-> int int))]
  
  [(typeof=> G (e_1 e_2) T_2)
   (typeof=> G e_1 (-> T_1 T_2))
   (where #t (typeof<= G e_2 T_1))]
  
  [(typeof=> G
           (case e_+
             [(inl x_l) => e_l]
             [(inr x_r) => e_r])
           T_body)
   (typeof=> G e_+ (+ T_l T_r))
   (typeof=> (extend G x_l T_l) e_l T_body)
   (typeof=> (extend G x_r T_r) e_r T_body)]

  [(typeof=> G (pair e_l e_r) (* T_l T_r))
   (typeof=> G e_l T_l)
   (typeof=> G e_r T_r)]
  [(typeof=> G (fst e) T_l)
   (typeof=> G e (* T_l T_r))]
  [(typeof=> G (snd e) T_r)
   (typeof=> G e (* T_l T_r))]

  [(typeof=> G (e : T) T)
   (where #t (typeof<= G e T))])

(define-metafunction STLC
  extend : G x T -> G
  [(extend ([x_0 T_0] ... [x_i T_i] [x_i+1 T_i+1] ...) x_i T)
   ([x_0 T_0] ... [x_i T] [x_i+1 T_i+1] ...)]
  [(extend ([x_1 T_1] ...) x_0 T_0)
   ([x_0 T_0] [x_1 T_1] ...)])

(define-metafunction STLC
  lookup : G x -> T
  [(lookup ([x_0 T_0] ... [x_i T_i] [x_i+1 T_i+1] ...) x_i) 
   T_i])

(test-equal 
 (judgment-holds 
  (typeof=> () 
            7
            T)
  T)
 (list (term int)))
(test-equal 
 (term
  (typeof<= () 
            7
            int))
 #t)

(test-equal 
 (judgment-holds 
  (typeof=> () 
            add1
            T)
  T)
 (list (term (-> int int))))
(test-equal 
 (term
  (typeof<= () 
            add1
            (-> int int)))
 #t)

(test-equal 
 (judgment-holds 
  (typeof=> () 
            (add1 7)
            T)
  T)
 (list (term int)))
(test-equal 
 (judgment-holds 
  (typeof=> () 
            (lambda (x) 
              (lambda (x)
                (x (add1 7))))
            T)
  T)
 (list (term (-> int (-> (-> int int) int)))))
(test-equal 
 (judgment-holds
  (typeof=> () 
            (lambda (x)
              (lambda (x)
                (add1 x)))
            T))
 #f)
