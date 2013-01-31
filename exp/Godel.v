Require Import Arith.

Inductive nati : Set :=
| pos_inf : nati
| a_nat : nat -> nati.
Hint Constructors nati.

Inductive ni_lt : nat -> nati -> Prop :=
| ni_lt_inf : forall n, ni_lt n pos_inf
| ni_lt_n : forall n m, n < m -> ni_lt n (a_nat m).
Hint Constructors ni_lt.

Class FIso (X:Type) := {
 k : nati ;
 k_nz : ni_lt 0 k ;
 i : nat -> option X ;
 i_k : forall n, ni_lt n k -> exists x, i n = Some x ;
 o : X -> option nat ;
 o_k : forall x n, o x = Some n -> ni_lt n k ;
 oi : forall n, ni_lt n k -> forall x, (i n) = Some x -> o x = Some n ;
 eql : X -> X -> Prop ;
 io : forall x n, (o x) = Some n -> forall x', (i n) = Some x' -> eql x x'
}.

Instance nat_FIso : FIso nat:= {
 k := pos_inf ;
 i := fun n => Some n ;
 o := fun x => Some x ;
 eql := eq
}.
Proof.
 auto. 
 intros n n_LT. exists n. auto.
 auto.
 intros n n_LT x EQ. inversion_clear EQ. auto.
 intros x n EQ. inversion_clear EQ. intros x' EQ. inversion_clear EQ. auto.
Qed.

Instance bool_FIso : FIso bool := {
 k := a_nat 2 ;
 i := fun n => 
  match n with 
  | 0 => Some false
  | 1 => Some true
  | _ => None
  end ;
 o := fun x => if x then Some 1 else Some 0 ;
 eql := eq
}.
Proof.
 auto.

 intros [|[|]]; eauto.
 intros n n_LT. inversion_clear n_LT. inversion_clear H. inversion_clear H0. inversion_clear H.

 intros [|] n EQ; inversion_clear EQ; auto.

 intros [|[|]] n_LT [|]; auto; intros EQ; inversion EQ.

 intros [|] n EQ; inversion_clear EQ; intros x' EQ; inversion_clear EQ; auto.
Qed.

Inductive fnat (k:nat) : Set :=
| fnat_n : forall n, n < k -> fnat k.
Hint Constructors fnat. 

Inductive fnat_eq (k:nat) : (fnat k) -> (fnat k) -> Prop :=
| fnat_eq_refl : forall n p m p', n = m -> fnat_eq k (fnat_n k n p) (fnat_n k m p').
Hint Constructors fnat_eq. 

Instance fnat_FIso (fk:nat) (fk_z:fk > 0) : FIso (fnat fk) := {
 k := a_nat fk ;
 i := fun n => match lt_dec n fk with 
               | left n_LT => fnat_n fk n n_LT
               | right _ => fnat_n fk 0 _
               end ;
 o := fun x => match x with fnat_n n _ => n end ;
 eql := fnat_eq fk
}.
Proof.
 auto.

 intros [n n_LT]. auto.

 intros n n_iLT. destruct (lt_dec n fk) as [n_LT | n_NLT].
 reflexivity. absurd (n < fk). auto. inversion n_iLT. auto.

 intros [n n_LT]. destruct (lt_dec n fk) as [n_LT' | n_NLT]; auto.
 absurd (n < fk); auto.

 Grab Existential Variables.
 auto.
Qed.

(* XXX We need "oddbits", "evenbits" and "combinebits" to do this *)
Instance nat_nat_FIso : FIso (nat*nat) := {
 k := pos_inf
}.
Admitted.

(* XXX This is based on Z = X + Y * XK *)
Instance fnat_fnat_FIso 
          (xk:nat) (x:FIso (fnat xk))
          (yk:nat) (x:FIso (fnat yk)) 
         : FIso ((fnat xk) * (fnat yk)) := {
 k := a_nat (xk * yk)
}.
Admitted.

(* XXX This is based on Z = X + Y * XK *)
Instance fnat_nat_FIso 
          (xk:nat) (x:FIso (fnat xk)) 
         : FIso ((fnat xk) * nat) := {
 k := pos_inf
}.
Admitted.

Inductive flip_eql (X Y:Type) (eql:(Y*X) -> (Y*X) -> Prop) : (X*Y) -> (X*Y) -> Prop :=
| flip_eql_inner : forall x1 y1 x2 y2, eql (y1,x1) (y2,x2) -> flip_eql X Y eql (x1,y1) (x2,y2).
Hint Constructors flip_eql.

Instance flip_FIso 
           (X Y:Type) (YX_FIso:FIso (Y*X))
         : FIso (X*Y) := {
 k := k ;
 i := fun n => let (y,x) := (i n : (Y*X)) in (x,y) ;
 o := fun p => let (x,y) := p in (o (y,x)) ;
 eql := flip_eql X Y eql
}.
Proof.
 apply k_nz.
 
 intros [x y]; simpl. apply o_k.

 intros n n_LT. remember (i n) as p. destruct p as [y x].
 rewrite Heqp. apply oi. auto.

 intros [x y]; simpl. remember (i (o (y, x))) as p. destruct p as [y' x'].
 apply flip_eql_inner. rewrite Heqp. apply io.
Qed.

Instance nat_fnat_FIso 
          (yk:nat) (y:FIso (fnat yk)) 
         : FIso (nat * (fnat yk)).
Proof.
 apply flip_FIso.
 apply fnat_nat_FIso.
 auto.
Qed.

Instance unit_FIso (X:Type) (x:X) : FIso X := {
 k := a_nat 1 ;
 i := fun n => x ;
 o := fun x => 0 ;
 eql := eq
}.
Proof.
 auto.

 auto.
 
 intros n n_LT. inversion n_LT. inversion H1. reflexivity. inversion H3.

 reflexivity.
