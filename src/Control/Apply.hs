{-# Language MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, UndecidableInstances, TypeFamilies #-}
module Control.Apply where

class Apply1 f a r | f a -> r, a -> f where
  apply1 :: f -> a -> r

instance Apply1 f () f where
  apply1 = const

instance (Apply1 y z r) => Apply1 (x -> y) (x, z) r where
  apply1 f (x, z) = apply1 (f x) z

class (A f a ~ f, F f a ~ r) => Apply2 f a r where
  type F f a
  type A f a
  apply2 :: A f a -> a -> F f a

instance (f ~ F f (), f ~ A f ()) => Apply2 f () f where 
  type F f () = f
  type A f () = f
  apply2 = const
-- ~
