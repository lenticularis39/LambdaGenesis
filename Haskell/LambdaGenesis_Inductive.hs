{-# LANGUAGE NoImplicitPrelude #-}
module LambdaGenesis_Inductive where

import Prelude(Integer, (+), (-))

-- Natural numbers
data Nat = Zero | Suc Nat

-- Pairing
data Pair a b = Pair a b

proj1 :: Pair a b -> a
proj1 (Pair a _) = a

proj2 :: Pair a b -> b
proj2 (Pair _ b) = b

-- Recursion
natRec :: a -> (Nat -> a -> a) -> Nat -> a
natRec z _ Zero    = z
natRec z f (Suc n) = f n (natRec z f n)

-- Recursive addition and multiplication
plus :: Nat -> Nat -> Nat
plus = \a -> natRec a (\_ r -> Suc r)

times :: Nat -> Nat -> Nat
times = \a -> natRec Zero (\_ r -> plus r a)

-- Factorial
fact :: Nat -> Nat
fact = natRec (Suc Zero) (\n r -> times (Suc n) r)

-- Fibonacci
fib2 :: Nat -> Pair Nat Nat
fib2 = natRec (Pair (Zero) (Suc Zero)) (\_ r -> Pair (proj2 r) (plus (proj2 r) (proj1 r)))

fib :: Nat -> Nat
fib = \n -> proj1 (fib2 n)

-- Ackermann
ack :: Nat -> Nat -> Nat
ack = natRec (\x -> Suc x) (\x f -> natRec (f (Suc Zero)) (\n y -> f y))

-- Interactivity
show :: Nat -> Integer
show Zero    = 0
show (Suc n) = (show n) + 1

read :: Integer -> Nat
read 0 = Zero
read x = Suc (read (x - 1))
