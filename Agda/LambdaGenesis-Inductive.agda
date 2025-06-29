module LambdaGenesis-Inductive where

-- Natural numbers
data Nat : Set where
    zero : Nat
    suc  : Nat → Nat
{-# BUILTIN NATURAL Nat #-}

-- Pairing
data Pair (A B : Set) : Set where
    pair : A → B → Pair A B

proj₁ : ∀ {A B : Set} → Pair A B → A
proj₁ (pair a _) = a

proj₂ : ∀ {A B : Set} → Pair A B → B
proj₂ (pair _ b) = b

-- Recursion
Nat-rec : {A : Set} → A → (Nat → A → A) → (Nat → A)
Nat-rec z _ zero =    z
Nat-rec z f (suc n) = f n (Nat-rec z f n)

-- Recursive addition and multiplication
_+_ : Nat → Nat → Nat
_+_ = λ a → Nat-rec a (λ _ r → suc r)

_*_ : Nat → Nat → Nat
_*_ = λ a → Nat-rec zero (λ _ r → r + a)

-- Predecessor
pred : Nat → Nat
pred = Nat-rec zero (λ n r → n)

-- Factorial
fact : Nat → Nat
fact = Nat-rec (suc zero) (λ n r → (suc n) * r)

-- Fibonacci
fib2 : Nat → Pair Nat Nat
fib2 = Nat-rec (pair {Nat} {Nat} (zero) (suc zero))
  (λ _ r → pair {Nat} {Nat} (proj₂ r) ((proj₁ r) + (proj₂ r)))

fib : Nat → Nat
fib = λ n → proj₁ (fib2 n)

-- Ackermann
ack : Nat → Nat → Nat
ack = Nat-rec {Nat → Nat} (λ x → suc x) (λ x f → Nat-rec {Nat} (f (suc zero)) (λ n y → f y))

-- Interactivity
show : Nat → Nat
show = λ x → x

read : Nat → Nat
read = λ x → x
