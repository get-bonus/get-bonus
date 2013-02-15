Require Import List.

Definition FiniteSet E := list E.
Hint Unfold FiniteSet.

Definition Subset {ET:Type} (LX:FiniteSet ET) (LY:FiniteSet ET) :=
  forall E,
    In E LX -> In E LY.
Hint Unfold Subset.

Record DFA (ST:Type) (QT:Type) : Type :=
  aDFA
  {
    Q : FiniteSet QT ;
    S : FiniteSet ST ;
    D : QT -> ST -> QT ;
    D_in : forall q s, In q Q -> In s S -> In (D q s) Q ;
    Q_0 : QT ;
    Q_0_in : In Q_0 Q ;
    F : FiniteSet QT ;
    F_in : Subset F Q
  }.
Hint Constructors DFA.

Inductive DFA_transitions (ST:Type) (QT:Type) : (DFA ST QT) -> QT -> (list ST) -> QT -> Prop :=
| Dt_nil :
  forall (M:DFA ST QT) q,    
    DFA_transitions ST QT M q nil q
| Dt_cons :
  forall (M:DFA ST QT) q s r q',
    DFA_transitions ST QT M (@D ST QT M q s) r q' ->
    DFA_transitions ST QT M q (s::r) q'.
Hint Constructors DFA_transitions.

Inductive DFA_accepts (ST:Type) (QT:Type) : (DFA ST QT) -> (list ST) -> Prop :=
| Da :
  forall M l Q_n,
    DFA_transitions ST QT M (@Q_0 ST QT M) l Q_n ->
    In Q_n (@F ST QT M) ->
    DFA_accepts ST QT M l.
Hint Constructors DFA_accepts.

Section Example.
  Inductive EQ : Set :=
  | q_1 : EQ
  | q_2 : EQ.

  Inductive ES : Set :=
  | s_0 : ES
  | s_1 : ES.

  Definition f := fun q s =>
      match q, s with
        | q_1, s_0 => q_2
        | q_1, s_1 => q_1
        | q_0, s_0 => q_1
        | q_0, s_1 => q_2
      end.

  Program Definition M : DFA ES EQ := {|
    Q   := (q_1 :: q_2 :: nil) ;
    S   := (s_0 :: s_1 :: nil) ;
    D   := f;
    Q_0 := q_1 ;
    F   := (q_1 :: nil)
  |}.
  Obligation 1.
    generalize dependent q. 
    generalize dependent s. 
    intros s  [ES | [ES | ES]] q [EQ | [EQ | EQ]]; 
      inversion_clear EQ;
        inversion_clear ES; auto.
  Qed.
  Obligation 3.
    unfold Subset. intros e. simpl.
    intros [E|E]. auto. inversion E.
  Qed.

  Ltac DFA_solve :=
    eapply Da ; 
     [ repeat (apply Dt_cons);  apply Dt_nil
     | simpl; auto ].

  Example M_1 : DFA_accepts ES EQ M (s_0 :: s_0 :: nil).
  Proof.    
    DFA_solve.
  Qed.

  Example M_2 : DFA_accepts ES EQ M (s_0 :: s_0 :: s_0 :: s_0 :: nil).
  Proof.    
    DFA_solve.
  Qed.

  Ltac rewrite_remove EQ x :=
    try rewrite EQ in *; clear EQ x.

  Example M_3 : ~ DFA_accepts ES EQ M (s_0 :: nil).
  Proof.    
    intros DA; inversion DA as [M l q_n DT IN EQ_M EQ_l];
      rewrite_remove EQ_M M;
      rewrite_remove EQ_l l.

    inversion DT as [|M q_0 s r q_1 DT' EQ_M EQ_q_0 EQ_s EQ_q_1].
    rewrite_remove EQ_M M.
    rewrite_remove EQ_q_0 q_0.
    rewrite_remove EQ_s s.
    rewrite_remove EQ_q_1 q_1.
    rewrite_remove H r.
    clear DT. rename DT' into DT.

    inversion DT as [M q_0 EQ_M EQ_q_0 EQ_l EQ_q_n_R|].
    rewrite_remove EQ_M M.
    rewrite_remove EQ_q_0 q_0.
    clear EQ_l.
    rewrite <- EQ_q_n_R in *. clear EQ_q_n_R q_n.

    simpl in IN.
    destruct IN as [H | H]; inversion H.
  Qed.

  Ltac DFA_nosolve_cons DT H :=
    inversion DT as [|M q_0 s r q_1 DT' EQ_M EQ_q_0 EQ_s EQ_q_1];
    rewrite_remove EQ_M M;
    rewrite_remove EQ_q_0 q_0;
    rewrite_remove EQ_s s;
    rewrite_remove EQ_q_1 q_1;
    rewrite_remove H r;
    clear DT; rename DT' into DT.

  Ltac DFA_nosolve_nil DT q_n := 
    inversion DT as [M q_0 EQ_M EQ_q_0 EQ_l EQ_q_n_R|];
    rewrite_remove EQ_M M;
    rewrite_remove EQ_q_0 q_0;
    clear EQ_l;
    rewrite <- EQ_q_n_R in *; clear EQ_q_n_R q_n.

  Ltac DFA_nosolve_IN IN := 
    simpl in IN;
      destruct IN as [H | H]; inversion H.

  Ltac DFA_nosolve H :=
    intros DA; inversion DA as [M l q_n DT IN EQ_M EQ_l];
      rewrite_remove EQ_M M;
      rewrite_remove EQ_l l;
      try (repeat DFA_nosolve_cons DT H);
        DFA_nosolve_nil DT q_n;
        DFA_nosolve_IN IN.

  Example M_3' : ~ DFA_accepts ES EQ M (s_0 :: nil).
  Proof. 
    DFA_nosolve H.
  Qed.
    
  Example M_4 : ~ DFA_accepts ES EQ M (s_0 :: s_1 :: nil).
  Proof.    
    DFA_nosolve H.
  Qed.
End Example.
