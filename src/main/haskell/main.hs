{-# LANGUAGE TypeOperators #-}
module Alacarte where

data Expr f = In (f (Expr f))
data Val e = Val Int
data Add e = Add e e

type IntExpr = Expr Val
type AddExpr = Expr Add

data (f :+: g) e = Inl (f e) | Inr (g e)

instance Functor Val where
  fmap f (Val x) = Val x

instance Functor Add where
  fmap f (Add e1 e2) = Add (f e1) (f e2)

instance (Functor f, Functor g) => Functor (f :+: g) where
  fmap f (Inl e1) = Inl (fmap f e1)
  fmap f (Inr e2) = Inr (fmap f e2)

foldExpr :: Functor f => (f a -> a) -> Expr f -> a
foldExpr f (In t) = f (fmap (foldExpr f) t)
