**LambdaGenesis** is a cross-language exploration of recursion built in
pure functional programming languages entirely from the lowest level of
primitives. By that, we do not mean the built-in types, but *the type system
itself.*

The constructions are demonstrated on the following mathematical functions:

- Addition and multiplication - prerequisite for all arithmetics.
- *Factorial.* This is a classical primitive recursive function [1] which is
a de facto Hello world of functional programming.
- *Fibonacci sequence.* Another classical primitive recursive function.

In all of the examples, the integer type is constructed from scratch using
core functionality of the type theory of the underlying lambda calculus.
All of the languages used support the declaration of inductive types, as
in Martin-Löf type theory. For languages with a type system strong enough to
implement computational Church encodings, we decided to implement both
that and an inductive implementation.

Additionally, a pair type is constructed in a similar way. Church encoding
variants use it to implement structural recursion, inductive variants use it
only to compute Fibonacci.

The project currently has implementations in the following languages:

- Rocq (Church, inductive)
- Agda (Church, inductive)
- Haskell (Church, inductive)
- Standard ML (inductive)

Focus has been paid to avoid constructions that explicitly or implicitly
use the fix point function, which is the underlying principle for
general recursion. If possible, opaque constructions like self-referential
defition of functions were also avoided, or, if not possible, were limited
to defining primitive eliminatation operations on types.

[1] [Primitive recursive function - Wikipedia](https://en.wikipedia.org/wiki/Primitive_recursive_function)

## Trying the code out

The provided code includes functions to convert between the language's
built-in types and the primitive implementation, named `show` and `read`.
Those can be used to easily test the functions in interactive mode, for
example:

```
~/LambdaGenesis/Agda $ agda --interactive LambdaGenesis-Church.agda
...
Checking LambdaGenesis-Church (/data/dev/LambdaGenesis/Agda/LambdaGenesis-Church.agda).
Main> show (fact (read 5))
120
```

Note that in the implementation of those, features out of the scope of
the examples may be used (e.g. inductive types in Church encoding examples
or fix point recursion declarations).

## Notes to specific languages

Some programming languages have specific quirks in relation with this
project, either in the implementation or in the underlying theory.

### Rocq

- Use `coqtop -noinit -l <file>` to test the examples, and
`Compute <expression>.` (mind the dot!) to evaluate expressions.
- We use an impredicative universe (`Prop`) to define the Church 
version of `Nat`, avoiding the problem with type in type seen in Agda
(which does not have an equivalent of the universe, due to having
a different type system). This is not needed for the inductive variant.
- Instead of converting to a native type, we use this encoding:
```
dec .0       = 0
dec .1 .2 .3 = 123
```
for `show` and `read`. You can also use numbers from "one" to "ten". Rocq
does support defining numerical syntax properly without any conversions [2]
(according to its philosophy of having a separate meta-world), but this is
currently not implemented.
- In the inductive variant, defining `Nat` and `Pair` automatically
defines `Nat_rect` and `Pair_rect` induction primitives [3]. You can see this
yourself with `Check Nat_rect.`, respectivelly `Check Pair_rect.`. Those
correspond to elimination rules in classical MLTT, and the former is used to
implement `natRec` (which is just a non-dependent variant of `Nat_rect`) in
the inductive variant.
- Note that unlike all other languages presented here, Rocq's philosophy
is that "Rocq" is theorem prover, composed of several languages. The
language for pure expressions is called "Gallina specification language";
the language that LambdaGenesis uses is properly called Vernac, and is
essentially a script language that operates in the Gallina world. You
can replace `-l` with `-lv` to see the commands being executed.
- The point above also means that `coqtop` is not a typical REPL, hence
the need for prefixing expressions with `Compute`.
- Unlike Agda, which mimicks Haskell, bare Rocq (with `-noinit`) only
knows the `forall` primitive from calculus of constructions for forming
types. We have to declare the familiar `->` ourselves as syntactic
sugar. (Rocq without `-noinit` imports the sugar from the standard
library.)

Example:
```
$ coqtop -noinit -l LambdaGenesis-Church.v
Coq < Compute show (ack three three).
     = dec .6 .1
     : NatRepr
Coq < Compute show (fact (read (dec .9))).
     = dec .3 .6 .2 .8 .8 .0
     : NatRepr
$ coqtop -noinit -l LambdaGenesis-Inductive.v
     = dec .1 .4 .4
     : NatRepr
```

[2] [Syntax extensions and notation scopes¶, section Number Notations - Rocq Prover documentation](https://rocq-prover.org/doc/V8.18.0/refman/user-extensions/syntax-extensions.html#number-notations)

[3] [Inductive types and recursive functions - Rocq Prover documentation¶](https://rocq-prover.org/doc/V8.18.0/refman/language/core/inductive.html)

### Agda

- Use `agda --interactive <file>` to test the examples. You can also type
the function name (e.g. `fact`) by itself to see the full expanded lambda
calculus form.
- Agda is based on a predicative type theory with strict universe levels,
providing challenges to implementing structural recursion on Church natural
numbers. The Church version thus uses the unsafe option --type-in-type
to bypass that.
- Unlike Rocq, Agda relies purely on pattern matching for doing computation
on inductive types. We define Nat-rec as a structural recursion primitive
using pattern matching, and implement the rest of the functions with that.
- No `show` and `read` is needed for the inductive implementation.

Example:
```
$ agda --interactive LambdaGenesis-Church.agda
Main> show (fib (read 12))
144
$ agda --interactive LambdaGenesis-Inductive.agda
Main> fib 12
144
```


### Haskell

- Use `ghci -XNoImplicitPrelude <file>` to test the examples. Optionally,
you can add `-fobject-code` to GHCi to make it compile the module for faster
computation.
- Haskell is based on System F, where types do not have types. This avoids
issues with defining the Church number type seen in the other languages.

Example:
```
$ ghci -XNoImplicitPrelude LambdaGenesis_Church.hs
ghci> show (fact (read 10))
3628800
$ ghci -XNoImplicitPrelude LambdaGenesis_Inductive.hs
ghci> show (ack (read 3) (read 4))
125
```

### Standard ML

- Use `sml LambdaGenesis-Inductive.sml` to test the examples.
- Like Haskell (which is a descendant of ML), general recursion has to be used
to implement structural recursion on an inductive type.
- Unlike Haskell, recursion is marked explicitly in Standard ML with the
keyword `rec`. Here, we mark only `natRec` as that's the recursive primitive
used in the rest of the program.
- Also unlike Haskell, Standard ML cannot do System F, only System HM; hence,
it is not strong enough to implement Church encoding, and the Church variant
is omitted.

Example:
```
$ sml  LambdaGenesis-Inductive.sml 
Standard ML of New Jersey [Version 110.99.8; 64-bit; April 25, 2025]
[opening LambdaGenesis-Inductive.sml]
datatype Nat = suc of Nat | zero
...
val read = fn : int -> Nat
- show (ack (read 3) (read 3))
= ;
val it = 61 : int
```
