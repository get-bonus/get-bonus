Require Import Arith.

Inductive nati : Set :=
| pos_inf : nati
| a_nat : nat -> nati.
Hint Constructors nati.

Inductive ni_lt : nat -> nati -> Prop :=
| ni_lt_inf : forall n, ni_lt n pos_inf
| ni_lt_n : forall n m, n < m -> ni_lt n (a_nat m).
Hint Constructors ni_lt.

Class FIso (X:Type) (k : nati) := {
 i : nat -> option X ;
 i_k : forall n, ni_lt n k -> exists x, i n = Some x ;
 o : X -> option nat ;
 o_k : forall x n, o x = Some n -> ni_lt n k ;
 oi : forall n, ni_lt n k -> forall x, (i n) = Some x -> o x = Some n ;
 eql : X -> X -> Prop ;
 io : forall x n, (o x) = Some n -> forall x', (i n) = Some x' -> eql x x'
}.

Instance nat_FIso : FIso nat pos_inf := {
 i := fun n => Some n ;
 o := fun x => Some x ;
 eql := eq
}.
Proof.
 intros n n_LT. exists n. auto.
 auto.
 intros n n_LT x EQ. inversion_clear EQ. auto.
 intros x n EQ. inversion_clear EQ. intros x' EQ. inversion_clear EQ. auto.
Qed.

Instance bool_FIso : FIso bool (a_nat 2) := {
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
 intros [|[|]]; eauto.
 intros n n_LT. inversion_clear n_LT. inversion_clear H. inversion_clear H0. inversion_clear H.

 intros [|] n EQ; inversion_clear EQ; auto.

 intros [|[|]] n_LT [|]; auto; intros EQ; inversion EQ.

 intros [|] n EQ; inversion_clear EQ; intros x' EQ; inversion_clear EQ; auto.
Qed.

Instance fnat_FIso (fk:nat) : FIso nat (a_nat fk) := {
 i := fun n => match lt_dec n fk with 
               | left _ => Some n
               | right _ => None
               end ;
 o := fun n => match lt_dec n fk with 
               | left _ => Some n
               | right _ => None
               end ;
 eql := eq
}.
Proof.
 intros n n_iLT. exists n. destruct (lt_dec n fk) as [n_LT | n_NLT].
 reflexivity. absurd (n < fk). auto. inversion n_iLT. auto.

 intros x n. destruct (lt_dec x fk) as [x_LT | x_NLT]; intro EQ; try inversion EQ.
 rewrite <- H0. auto.

 intros n n_iLT x. remember (lt_dec n fk) as p.
 destruct p as [n_LT | n_NLT]; intros EQ; try inversion EQ.
 rewrite <- H0. rewrite <- Heqp. auto.

 intros x n. remember (lt_dec x fk) as p.
 destruct p as [x_LT | x_NLT]; intros EQ; try inversion EQ.
 intros x'. 
 rewrite <- H0. rewrite <- Heqp. auto.
 intros EQ'; inversion EQ'. auto.
Qed.

(* XXX We need "oddbits", "evenbits" and "combinebits" to do this *)
Instance inat_inat_FIso 
          (x:FIso nat pos_inf)
          (y:FIso nat pos_inf)
 : FIso (nat*nat) pos_inf := {
}.
Admitted.

(* XXX This is based on Z = X + Y * XK *)
Instance fnat_fnat_FIso 
          (xk:nat) (x:FIso nat (a_nat xk))
          (yk:nat) (y:FIso nat (a_nat yk)) 
         : FIso (nat*nat) (a_nat (xk * yk)) := {
}.
Admitted.

(* XXX This is based on Z = X + Y * XK *)
Instance fnat_nat_FIso 
          (xk:nat) (x:FIso nat (a_nat xk))
          (y:FIso nat pos_inf) 
         : FIso (nat*nat) pos_inf := {
}.
Admitted.

Inductive flip_eql (X Y:Type) (eql:(Y*X) -> (Y*X) -> Prop) : (X*Y) -> (X*Y) -> Prop :=
| flip_eql_inner : forall x1 y1 x2 y2, eql (y1,x1) (y2,x2) -> flip_eql X Y eql (x1,y1) (x2,y2).
Hint Constructors flip_eql.

Instance flip_FIso 
           (X Y:Type) (k:nati) (YX_FIso:FIso (Y*X) k)
         : FIso (X*Y) k := {
 i := fun n => 
      match i n with
      | Some (y, x) =>
        Some (x, y)
      | None =>
        None
      end ;
 o := fun p => 
      let (x,y) := p in 
       (o (y,x)) ;
 eql := flip_eql X Y eql
}.
Proof. 
 intros n n_LT. destruct (i_k n n_LT) as [[y x] EQ].
 exists (x,y). rewrite EQ. auto.

 intros [x y]; simpl. apply o_k.

 intros n n_LT. remember (i n) as p. destruct p as [[y x]|];
  intros p EQ; inversion_clear EQ. apply oi; auto.

 intros [x y] n EQ [x' y']; simpl. remember (i n) as p. destruct p as [[y'' x'']|];
  intros EQ'; inversion EQ'.
 apply flip_eql_inner. eapply io. apply EQ.
 rewrite <- Heqp. inversion EQ'. auto.
Qed.

Instance nat_fnat_FIso 
          (x:FIso nat pos_inf) 
          (yk:nat) (y:FIso nat (a_nat yk)) 
         : FIso (nat * nat) pos_inf.
Proof.
 apply flip_FIso.
 eapply fnat_nat_FIso; auto.
 apply y.
Qed.

Instance unit_FIso (X:Type) (x:X) (is_x_dec : (forall x', { x' = x } + { x' <> x }))
 : FIso X (a_nat 1) := {
 i := fun n => match n with 0 => Some x | _ => None end ;
 o := fun x' => if is_x_dec x' then Some 0 else None ;
 eql := eq
}.
Proof.
 intros n n_LT. exists x.
 destruct n as [|n]; auto.
 inversion n_LT. inversion H1. inversion H3.

 intros x' n. destruct (is_x_dec x');
  intros EQ; inversion EQ. auto.

 intros n n_LT x'.
  destruct n as [|n]; intros EQ; symmetry in EQ; inversion_clear EQ.
  destruct (is_x_dec x); auto.
  absurd (x = x); auto.

 intros x' n. destruct (is_x_dec x'); intros EQ; inversion_clear EQ.
 intros x'' EQ. symmetry in EQ. inversion_clear EQ. auto.
Qed.

