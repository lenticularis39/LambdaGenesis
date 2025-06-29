(* Natural numbers *)
datatype Nat = zero | suc of Nat;

(* Pairing *)
datatype ('a, 'b) Pair =
  pair of 'a * 'b;

val proj1 : ('a, 'b) Pair -> 'a =
  fn (pair (x, y)) => x;
val proj2 : ('a, 'b) Pair -> 'b =
  fn (pair (x, y)) => y;

(* Recursion *)
val rec natRec : 'a -> (Nat -> 'a -> 'a) -> Nat -> 'a =
  fn z => fn f => fn n => case n of
    zero  => z
  | suc m => f m (natRec z f m);

(* Recursive addition and multiplication *)
val add : Nat -> Nat -> Nat =
  fn a => natRec a (fn _ => fn r => suc r);

val mul : Nat -> Nat -> Nat =
  fn a => natRec zero (fn _ => fn r => add r a);

(* Factorial *)
val fact : Nat -> Nat =
  natRec (suc zero) (fn n => fn r => mul (suc n) r);

(* Fibonacci *)
val fib2 : Nat -> (Nat, Nat) Pair =
  natRec (pair (zero, (suc zero))) (fn _ => fn r => pair (proj2 r, (add (proj2 r) (proj1 r))));

val fib : Nat -> Nat =
  fn n => proj1 (fib2 n);

(* Ackermann *)
val ack : Nat -> Nat -> Nat =
  natRec (fn x => suc x) (fn x => fn f => natRec (f (suc zero)) (fn n => fn y => f y));

(* Interactivity *)
fun show zero    = 0
  | show (suc m) = 1 + show m;

fun read 0 = zero
  | read n = suc (read (n - 1));
