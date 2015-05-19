{-# Language KindSignatures, GADTs, FlexibleContexts #-}
module Control.Shuffle
  ( Shuffling(..)
  , Shuffle()
  , assoc'
  , juggle'
  , flip
  ) where

import Control.Arrow
import Control.Category
import Prelude(Show(..), (++), (+))
import Data.Typeable

flip :: (a -> b -> c) -> (b -> a -> c)
flip f = \a b -> f b a

class Shuffling a where
  assoc :: a ((b, c), d) (b, (c, d))
  associ :: a (b, (c, d)) ((b, c), d)
  juggle :: a ((b, c), d) ((b, d), c)
  trace :: a (b, c) (d, c) -> a b d

instance Shuffling (->) where
  assoc ((x, y), z) = (x, (y, z))
  associ (x, (y, z)) = ((x, y), z)
  trace f x = let (y, z) = f (x, z) in y
  juggle ((x, y), z) = ((x, z), y)

assoc' :: (Category a, Shuffling a) =>
     a ((b, c), d) ((e, f), g) -> a (b, (c, d)) (e, (f, g))
assoc' f = assoc . f . associ

juggle' :: (Category a, Shuffling a) =>
     a ((b, d), c) ((e, f), g) -> a ((b, c), d) ((e, g), f)
juggle' f = juggle . f . juggle

instance Category a => Category (Shuffle a) where
  id = Id
  (.) = flip sequence

instance Arrow a => Arrow (Shuffle a) where
  arr = Arr . arr
  first = flip (Par) id

instance Shuffling (Shuffle a) where
  assoc = Assoc
  associ = Associ
  juggle = Juggle
  trace = Trace

instance Show (Shuffle a b c) where
  show (Arr f) = "arr <f>"
  show Id = "id"
  show (Cons f g) = show f ++ " >>> " ++ show g
  show (Par f g) = "(" ++ show f ++ ") *** (" ++ show g ++ ")"
  show Assoc = "assoc"
  show Associ = "associ"
  show Juggle = "juggle"

data Shuffle :: (* -> * -> *) -> * -> * -> * where
  Arr :: a b c -> Shuffle a b c
  Id :: Shuffle a b b
  Cons :: Shuffle a b c -> Shuffle a c d -> Shuffle a b d
  Par :: Shuffle a b c -> Shuffle a d e -> Shuffle a (b, d) (c, e)
  Assoc :: Shuffle a ((b, c), d) (b, (c, d))
  Associ :: Shuffle a (b, (c, d)) ((b, c), d)
  Juggle  :: Shuffle a ((b, c), d) ((b, d), c)
  Trace :: Shuffle a (b, c) (d, c) -> Shuffle a b d

sequence :: Category a =>
            Shuffle a b c -> Shuffle a c d -> Shuffle a b d
sequence Id a = a
sequence a Id = a
sequence Juggle Juggle = id
sequence Assoc Associ = id
sequence Associ Assoc = id
sequence (Par f g) (Par h e) = Par (sequence f h) (sequence g e)
sequence (Cons a b) c = sequence a (sequence b c)
sequence a (Cons b c) = case sequence a b of
                          Cons a' b' -> Cons a' (sequence b' c)
                          a' -> sequence a' c
sequence a b = Cons a b
