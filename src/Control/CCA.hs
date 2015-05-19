{-# Language KindSignatures, GADTs #-}
module Control.CCA where

import Control.Category
import Control.Arrow
import Prelude(undefined, show)

import Control.Shuffle

class Arrow a => ArrowInit a where
  init :: b -> a b b

instance (Arrow a, Category a, Shuffling a)
  => Category (CCA a) where
  id  = Arr id
  (.) = flip sequence

instance (Arrow a, Shuffling a) => Arrow (CCA a) where
  arr    = Arr . arr
  first  = parallell
  second = Second
  (***)  = (:***)
  (&&&)  = (:&&&)

instance (Arrow a, Shuffling a) => ArrowLoop (CCA a) where
  loop = looping

data CCA :: (* -> * -> *) -> * -> * -> * where
  (:.)   :: CCA a d e -> CCA a c d -> CCA a c e
  Arr    :: a b c -> CCA a b c
  Second :: CCA a b c -> CCA a (d, b) (d, c)
  (:***) :: CCA a b c -> CCA a b' c' -> CCA a (b, b') (c, c')
  (:&&&) :: CCA a b c -> CCA a b c' -> CCA a b (c, c')
  Loop   :: CCA a (b, d) (c, d) -> CCA a b c
  LoopD  :: d -> a (b, d) (c, d) -> CCA a b c

sequence :: (Arrow a, Shuffling a) => CCA a b c -> CCA a c d -> CCA a b d
sequence (Arr f) (Arr g) = Arr (g . f)
sequence (LoopD i f) (Arr g) = LoopD i ((first g) . f)
sequence (Arr f) (LoopD i g) = LoopD i (g . (first f))
sequence (LoopD i f) (LoopD j g) = LoopD (i, j) (assoc' (juggle' (first g) . (first f)))
sequence f g = g . f

parallell :: (Arrow a, Shuffling a) => CCA a b c -> CCA a (b, d) (c, d)
parallell (Arr f) = Arr (first f)
parallell (LoopD i f) = LoopD i (juggle' (first f))
parallell f = f *** id

looping :: (Arrow a, Shuffling a) => CCA a (b, d) (c, d) -> CCA a b c
looping (Arr f) = Arr (trace f)
looping (LoopD i f) = LoopD i (trace (juggle' f))
