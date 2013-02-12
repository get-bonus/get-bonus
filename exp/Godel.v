Require Import Arith.

Inductive nato : Set :=
| o_inf : nato
| o_fin : nato.
Hint Constructors nato.

Inductive nati : Set :=
| pos_inf : nati
| a_nat : nat -> nati.
Hint Constructors nati.

Inductive order : nati -> nato -> Prop :=
| order_inf : order pos_inf o_inf
| order_fin : forall n, order (a_nat n) o_fin.
Hint Constructors order.

Inductive ni_lt : nat -> nati -> Prop :=
| ni_lt_inf : forall n, ni_lt n pos_inf
| ni_lt_n : forall n m, n < m -> ni_lt n (a_nat m).
Hint Constructors ni_lt.

Class FIso (X:Type) (ko:nato) := {
 k : nati ;
 k_o : order k ko ;
 i : nat -> option X ;
 i_k : forall n, ni_lt n k -> exists x, i n = Some x ;
 o : X -> option nat ;
 o_k : forall x n, o x = Some n -> ni_lt n k ;
 oi : forall n, ni_lt n k -> forall x, (i n) = Some x -> o x = Some n ;
 eql : X -> X -> Prop ;
 io : forall x n, (o x) = Some n -> forall x', (i n) = Some x' -> eql x x'
}.

Instance nat_FIso : FIso nat o_inf := {
 i := fun n => Some n ;
 o := fun x => Some x ;
 eql := eq
}.
Proof.
 apply order_inf.
 intros n n_LT. exists n. auto.
 auto.
 intros n n_LT x EQ. inversion_clear EQ. auto.
 intros x n EQ. inversion_clear EQ. intros x' EQ. inversion_clear EQ. auto.
Qed.

Instance bool_FIso : FIso bool o_fin := {
 k := (a_nat 2) ;
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

Instance fnat_FIso (fk:nat) : FIso nat o_fin := {
 k := (a_nat fk) ;
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
 auto.
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
          (x:FIso nat o_inf)
          (y:FIso nat o_inf)
 : FIso (nat*nat) o_inf := {
}.
Admitted.

(* XXX This is based on Z = X + Y * XK *)
Instance fnat_fnat_FIso 
          (x:FIso nat o_fin)
          (y:FIso nat o_fin) 
         : FIso (nat*nat) o_fin := {
}.
Admitted.

(* XXX This is based on Z = X + Y * XK *)
Instance fnat_inat_FIso 
          (x:FIso nat o_fin)
          (y:FIso nat o_inf) 
         : FIso (nat*nat) o_inf := {
}.
Admitted.

Inductive flip_eql (X Y:Type) (eql:(Y*X) -> (Y*X) -> Prop) : (X*Y) -> (X*Y) -> Prop :=
| flip_eql_inner : forall x1 y1 x2 y2, eql (y1,x1) (y2,x2) -> flip_eql X Y eql (x1,y1) (x2,y2).
Hint Constructors flip_eql.

Instance flip_FIso 
           (X Y:Type) (ko:nato) (YX_FIso:FIso (Y*X) ko)
         : FIso (X*Y) ko := 
{
 k := let (ik,_,_,_,_,_,_,_,_) := YX_FIso in ik ;
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
 destruct YX_FIso as [k k_o i i_k o o_k oi eql io]. auto.

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

Instance inat_fnat_FIso 
          (x:FIso nat o_inf) 
          (y:FIso nat o_fin) 
         : FIso (nat * nat) o_inf.
Proof.
 apply flip_FIso.
 eapply fnat_inat_FIso; auto.
Qed.

Instance unit_FIso 
   (X:Type) (x:X)
   (is_x_dec : (forall x', { x' = x } + { x' <> x }))
 : FIso X o_fin := {
 k := (a_nat 1) ;
 i := fun n => match n with 0 => Some x | _ => None end ;
 o := fun x' => if is_x_dec x' then Some 0 else None ;
 eql := eq
}.
Proof.
  auto.

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

Instance lift_FIso 
 (X:Type) (xko:nato) (xF:FIso X xko)
 : (FIso nat xko).
Proof.
 destruct xko.
 apply nat_FIso.
 destruct xF as [x_k x_k_o x_i x_i_k x_o x_o_k x_oi x_eql x_io]. 
 destruct x_k as [|x_k].

 absurd (order pos_inf o_fin); auto.
 clear x_i x_i_k x_o x_o_k x_oi x_eql x_io X.
 inversion x_k_o.

 apply fnat_FIso.
 apply x_k.
Qed. 

Definition join xk yk :=
 match xk, yk with
 | o_inf, _ => o_inf
 | _, o_inf => o_inf
 | o_fin, o_fin => o_fin
 end.

Instance compose_FIso 
 (X:Type) (xko:nato) (xF:FIso X xko)
 (f:Type -> Type) (f_ko:nato -> nato)
  (fp_i: forall (X Y:Type), f Y -> X -> f X) 
  (fp_o: forall (Y:Type), (f Y) -> Y)
  (fF:(FIso (f nat) (f_ko xko))) :
 (FIso (f X) (f_ko xko)) := {
 k := let (fn_k,_,_,_,_,_,_,_,_) := fF in fn_k ;
 i := 
   let (_,_,fn_i,_,_,_,_,_,_) := fF in
     let (_,_,x_i,_,_,_,_,_,_) := xF in
       fun n => match fn_i n with 
                  | Some fn => 
                    match x_i (fp_o nat fn) with
                      | Some x =>
                        Some (fp_i X nat fn x)
                      | None =>
                        None
                    end
                  | None =>
                    None
                end ;
 o := fun fx => 
   let (_,_,fn_i,_,fn_o,_,_,_,_) := fF in
     let (_,_,x_i,_,x_o,_,_,_,_) := xF in
       let x := fp_o X fx in
         match x_o x with
           | Some xn =>
             fn_o (fp_i nat X fx xn)
           | None =>
             None
         end ;
 eql := eq
}.
Proof.
(**)
  destruct xF as [x_k x_k_o x_i x_i_k x_o x_o_k x_oi x_eql x_io]. 
  destruct fF as [fn_k fn_k_o fn_i fn_i_k fn_o fn_o_k fn_oi fn_eql fn_io]. 

  apply fn_k_o.
(**)
  destruct xF as [x_k x_k_o x_i x_i_k x_o x_o_k x_oi x_eql x_io]. 
  destruct fF as [fn_k fn_k_o fn_i fn_i_k fn_o fn_o_k fn_oi fn_eql fn_io]. 

  intros n n_LT.
  destruct (fn_i_k n n_LT) as [fn fn_EQ].
  remember (x_i (fp_o nat fn)) as x.
  destruct x.
  exists (fp_i X nat fn x).
  rewrite fn_EQ.
  rewrite <- Heqx.
  reflexivity.

  rewrite x_i_k in Heqx.

Admitted.

Instance pair_FIso (X Y:Type) (xko yko:nato) (xF:FIso X xko) (yF:FIso Y yko) :
          FIso (X*Y) (join xko yko).
Proof.
 apply compose_FIso.
 apply yF. 
 intros X' Y' [x y'] y. auto.
 intros Y' [x y']. auto.
 apply compose_FIso with (X:=X) (f:=fun (t:Type) => (prod t nat)) (f_ko:=fun tko => join tko yko). 
 apply xF.
 intros X' Y' [xn yn] x. auto.
 intros Y' [xn yn]. exact xn.
 
 destruct xko as [|]; destruct yko as [|]; simpl.
 apply inat_inat_FIso.
  apply (lift_FIso X). auto.
  apply (lift_FIso Y). auto.

 apply inat_fnat_FIso.
  apply (lift_FIso X). auto.
  apply (lift_FIso Y). auto.

 apply fnat_inat_FIso.
  apply (lift_FIso X). auto.
  apply (lift_FIso Y). auto.

 apply fnat_fnat_FIso.
  apply (lift_FIso X). auto.
  apply (lift_FIso Y). auto.
Qed.

(* bind : (FIso X xk) -> (X -> FIso Y yk) -> FIso (X*Y) (join xk yk) *)
