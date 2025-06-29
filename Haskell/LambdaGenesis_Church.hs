{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}
module LambdaGenesis_Church where

import Prelude(Integer, (+), (-))

-- Natural numbers
type Nat = forall a. (a -> a) -> a -> a

zero :: Nat
zero = \_ x -> x

suc :: Nat -> Nat
suc n = \f x -> f (n f x)

-- Pairing
type Pair a b = forall c. (a -> b -> c) -> c

pair :: forall a. forall b. a -> b -> Pair a b
pair x y = \f -> f x y

proj1 :: forall a. forall b. Pair a b -> a
proj1 p = p (\x _ -> x)

proj2 :: forall a. forall b. Pair a b -> b
proj2 p = p (\_ y -> y)

-- Recursion
natStep :: forall a. (Nat -> a -> a) -> Pair Nat a -> Pair Nat a
natStep f p = pair @Nat @a (suc n) (f n x)
  where
    n = proj1 @Nat @a p
    x = proj2 @Nat @a p

natRec :: forall a. a -> (Nat -> a -> a) -> Nat -> a
natRec z s n = proj2 @Nat @a (n (natStep s) (pair zero z))

-- Recursive addition and multiplication
plus :: Nat -> Nat -> Nat
plus = \a -> natRec @Nat a (\_ r -> suc r)

times :: Nat -> Nat -> Nat
times = \a -> natRec @Nat zero (\_ r -> plus r a)

-- Factorial
fact :: Nat -> Nat
fact = natRec (suc zero) (\n r -> times (suc n) r)

-- Fibonacci
fib2 :: Nat -> Pair Nat Nat
fib2 = natRec @(Pair Nat Nat) (pair @Nat @Nat (zero) (suc zero))
  (\_ r -> pair @Nat @Nat (proj2 @Nat @Nat r) (plus (proj2 @Nat @Nat r) (proj1 @Nat @Nat r)))

fib :: Nat -> Nat
fib = \n -> proj1 @Nat @Nat (fib2 n)

-- Ackermann
ack :: Nat -> Nat -> Nat
ack = natRec @(Nat -> Nat) (\x -> suc x) (\x f -> natRec @Nat (f (suc zero)) (\n y -> f y))

-- Interactivity
show :: Nat -> Integer
show n = n (+1) 0

read :: Integer -> Nat
read 0 = zero
read x = suc (read (x - 1))
