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

The project currently has implementations in the following languages:

- ~~Rocq (Church, inductive)~~
- Agda (Church, inductive)
- Haskell (Church, inductive)
- ~~Standard ML (inductive)~~

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

## Notes to specific languages

Some programming languages have specific quirks in relation with this
project, either in the implementation or in the underlying theory.

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

### Haskell

- Use `ghci -XNoImplicitPrelude <file>` to test the examples. Optionally,
you can add `-fobject-code` to GHCi to make it compile the module for faster
computation.
