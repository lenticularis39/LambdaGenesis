{-# OPTIONS  --type-in-type #-}
module LambdaGenesis-Church where

-- Natural numbers
Nat : Set
Nat = {A : Set} → (A → A) → A → A

zero : Nat
zero = λ f → (λ x → x)

suc : Nat → Nat
suc n = λ f → (λ x → f (n f x) )

-- Pairing
Pair : (A B : Set) → Set
Pair A B = ∀ (C : Set) → (A → B → C) → C

pair : {A B : Set} → A → B → Pair A B
pair x y C = λ f → f x y

proj₁ : {A B : Set} → Pair A B → A
proj₁ {A} {B} p = p A (λ x y → x)

proj₂ : {A B : Set} → Pair A B → B
proj₂ {A} {B} p = p B (λ x y → y)

-- Recursion
Nat-step : {A : Set} → (Nat → A → A) → Pair Nat A → Pair Nat A
Nat-step {A} f p = pair {Nat} {A} (suc n) result
  where result = f (proj₁ p) (proj₂ p)
        n      = proj₁ p

Nat-rec : {A : Set} → A → (Nat → A → A) → (Nat → A)
Nat-rec {A} z s n = proj₂ (n (Nat-step s) (pair {Nat} {A} zero z))

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
open import Agda.Builtin.Nat using () renaming (Nat to INat; zero to izero; suc to isuc)

show : Nat → INat
show n = n isuc izero

read : INat → Nat
read izero = zero
read (isuc n) = suc (read n)
