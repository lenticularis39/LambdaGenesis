(* Notation *)
Notation "x -> y" := (forall _ : x, y) (at level 99, right associativity).

(* Natural numbers *)
Inductive Nat : Type :=
| zero : Nat
| suc  : Nat -> Nat.

(* Pairing *)
Inductive Pair (A B : Type) : Type :=
| pair : A -> B -> Pair A B.

Definition proj1 : forall {A B : Type}, Pair A B -> A :=
  fun A B p => Pair_rect A B (fun _ => A) (fun x _ => x) p.

Definition proj2 : forall {A B : Type}, Pair A B -> B :=
  fun A B p => Pair_rect A B (fun _ => B) (fun _ y => y) p.

(* Recursion *)
Definition natRec : forall {A : Type}, A -> (Nat -> A -> A) -> (Nat -> A) :=
  fun A x f => Nat_rect (fun _ => A) x f.

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
  natRec (pair Nat Nat zero (suc zero)) (fun _ r => pair Nat Nat (proj2 r) ((proj1 r) + (proj2 r))).

Definition fib : Nat -> Nat :=
  fun n => proj1 (fib2 n).

(* Ackermann *)
Definition ack : Nat -> Nat -> Nat :=
  natRec (fun x => suc x) (fun x f => natRec (f (suc zero)) (fun n y => f y)).

(* Interactivity *)
Inductive NatRepr : Type :=
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

Fixpoint show (n : Nat) : NatRepr :=
  match n with
  | zero  => (dec .0)
  | suc m => reprInc (show m)
  end.

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
