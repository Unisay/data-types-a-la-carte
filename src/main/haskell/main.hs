{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Alacarte where

-- Fixing the expression problem

data Expr f = In (f (Expr f))
data Val e = Val Int
data Add e = Add e e

type IntExpr = Expr Val
type AddExpr = Expr Add

data (f :+: g) e = Inl (f e) | Inr (g e)

addExample :: Expr (Val :+: Add )
addExample = In (Inr (Add (In (Inl (Val 118))) (In (Inl (Val 1219)))))

-- Evaluation

instance Functor Val where
  fmap f (Val x) = Val x

instance Functor Add where
  fmap f (Add e1 e2) = Add (f e1) (f e2)

instance (Functor f, Functor g) => Functor (f :+: g) where
  fmap f (Inl e1) = Inl (fmap f e1)
  fmap f (Inr e2) = Inr (fmap f e2)

foldExpr :: Functor f => (f a -> a) -> Expr f -> a
foldExpr f (In t) = f (fmap (foldExpr f) t)

class Functor f => Eval f where
  evalAlgebra :: f Int -> Int

instance Eval Val where
  evalAlgebra (Val x) = x

instance Eval Add where
  evalAlgebra (Add x y) = x + y

instance (Eval f, Eval g) => Eval (f :+: g) where
  evalAlgebra (Inl x) = evalAlgebra x
  evalAlgebra (Inr y) = evalAlgebra y

eval :: Eval f => Expr f -> Int
eval expr = foldExpr evalAlgebra expr

-- Automating injections

class (Functor sub, Functor sup) => sub :<: sup where
  inj :: sub a -> sup a

-- :<: is reflexive
instance Functor f => f :<: f where
  inj = id

-- how to inject any value of type f a to a value of type (f :+: g) a,
-- regardless of g
instance (Functor f, Functor g) => f :<: (f :+: g) where
  inj = Inl

-- provided we can inject a value of type f a into one of type g a,
-- we can also inject f a into a larger type (h :+: g) a by composing
-- the first injection with an additional Inr.
instance (Functor f, Functor g, Functor h, f :<: g) => f :<: (h :+: g) where
  inj = Inr . inj

inject :: (g :<: f) => g (Expr f) -> Expr f
inject = In . inj

val :: (Val :<: f) => Int -> Expr f
val x = inject (Val x)

(⊕) :: (Add :<: f) => Expr f -> Expr f -> Expr f
x ⊕ y = inject (Add x y)

-- Examples

data Mul x = Mul x x

instance Functor Mul where
  fmap f (Mul x y) = Mul (f x) (f y)
