{-# Language MultiParamTypeClasses, FlexibleInstances, TypeFamilies #-}
module Control.CCA.Apply where

class (F f a ~ r, G a r ~ f) => Apply f a r where
  type F f a
  type G a r
  apply:: f -> a -> r

instance Apply f () f where
  type F f () = f
  type G () f = f
  apply = const

instance (Apply y z r, F y z ~ r, G z r ~ y) => Apply (x -> y) (x, z) r where
  type F (x -> y) (x, z) = F y z
  type G (x, z) r = x -> G z r
  apply f (x, z) = apply (f x) z

