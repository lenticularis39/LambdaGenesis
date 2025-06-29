(* Notation *)
Notation "x -> y" := (forall _ : x, y) (at level 99, right associativity).

(* Natural numbers *)
Definition Nat : Prop :=
  forall A : Prop, (A -> A) -> A -> A.

Definition zero : Nat :=
  fun _ _ x => x.
Definition suc : Nat -> Nat :=
  fun n => (fun A f x => f (n A f x)).

(* Pairing *)
Definition Pair : Prop -> Prop -> Prop :=
  fun A B => forall C, (A -> B -> C) -> C.

Definition pair : forall {A : Prop}, forall {B : Prop}, A -> B -> Pair A B :=
  fun _ _ x y C f => f x y.

Definition proj1 : forall {A : Prop}, forall {B : Prop}, Pair A B -> A :=
  fun A B p => p A (fun x y => x).

Definition proj2 : forall {A : Prop}, forall {B : Prop}, Pair A B -> B :=
  fun A B p => p B (fun x y => y).

(* Recursion *)
Definition natStep : forall {A : Prop}, (Nat -> A -> A) -> Pair Nat A -> Pair Nat A :=
  fun A f p => pair (suc (proj1 p)) (f (proj1 p) (proj2 p)).

Definition natRec : forall {A : Prop}, A -> (Nat -> A -> A) -> (Nat -> A) :=
  fun A z s n => proj2 (n (Pair Nat A) (natStep s) (pair zero z)).

(* Recursive addition and multiplication *)
Definition add : Nat -> Nat -> Nat :=
  fun a => natRec a (fun _ r => suc r).
Notation "x + y" := (add x y) (at level 4, left associativity).

Definition mul : Nat -> Nat -> Nat :=
  fun a => natRec zero (fun _ r => r + a).
Notation "x * y" := (mul x y) (at level 2, left associativity).

(* Predecessor *)
Definition pred : Nat -> Nat :=
  natRec zero (fun n _ => n).

(* Factorial *)
Definition fact : Nat -> Nat :=
  natRec (suc zero) (fun n r => (suc n) * r).

(* Fibonacci *)
Definition fib2 : Nat -> Pair Nat Nat :=
  natRec (pair zero (suc zero)) (fun _ r => pair (proj2 r) ((proj1 r) + (proj2 r))).

Definition fib : Nat -> Nat :=
  fun n => proj1 (fib2 n).

(* Ackermann *)
Definition ack : Nat -> Nat -> Nat :=
  natRec (fun x => suc x) (fun x f => natRec (f (suc zero)) (fun n y => f y)).

(* Interactivity *)
Inductive NatRepr : Prop :=
| dec : NatRepr
| n0 : NatRepr -> NatRepr
| n1 : NatRepr -> NatRepr
| n2 : NatRepr -> NatRepr
| n3 : NatRepr -> NatRepr
| n4 : NatRepr -> NatRepr
| n5 : NatRepr -> NatRepr
| n6 : NatRepr -> NatRepr
| n7 : NatRepr -> NatRepr
| n8 : NatRepr -> NatRepr
| n9 : NatRepr -> NatRepr.

Notation "x .0" := (n0 x) (at level 10, left associativity).
Notation "x .1" := (n1 x) (at level 10, left associativity).
Notation "x .2" := (n2 x) (at level 10, left associativity).
Notation "x .3" := (n3 x) (at level 10, left associativity).
Notation "x .4" := (n4 x) (at level 10, left associativity).
Notation "x .5" := (n5 x) (at level 10, left associativity).
Notation "x .6" := (n6 x) (at level 10, left associativity).
Notation "x .7" := (n7 x) (at level 10, left associativity).
Notation "x .8" := (n8 x) (at level 10, left associativity).
Notation "x .9" := (n9 x) (at level 10, left associativity).

Fixpoint reprInc (r : NatRepr) : NatRepr :=
  match r with
  | dec    => (r .1)
  | (x .0) => (x .1)
  | (x .1) => (x .2)
  | (x .2) => (x .3)
  | (x .3) => (x .4)
  | (x .4) => (x .5)
  | (x .5) => (x .6)
  | (x .6) => (x .7)
  | (x .7) => (x .8)
  | (x .8) => (x .9)
  | (x .9) => (reprInc x .0)
  end.

Definition show (n : Nat) : NatRepr :=
  n NatRepr reprInc (dec .0).

Definition one := suc zero.
Definition two := suc one.
Definition three := suc two.
Definition four := suc three.
Definition five := suc four.
Definition six := suc five.
Definition seven := suc six.
Definition eight := suc seven.
Definition nine := suc eight.
Definition ten := suc nine.

Fixpoint read (r : NatRepr) : Nat :=
  match r with
  | dec => zero
  | (x .0) => ((read x) * ten + zero)
  | (x .1) => ((read x) * ten + one)
  | (x .2) => ((read x) * ten + two)
  | (x .3) => ((read x) * ten + three)
  | (x .4) => ((read x) * ten + four)
  | (x .5) => ((read x) * ten + five)
  | (x .6) => ((read x) * ten + six)
  | (x .7) => ((read x) * ten + seven)
  | (x .8) => ((read x) * ten + eight)
  | (x .9) => ((read x) * ten + nine)
  end.
  