{-# Language KindSignatures, GADTs #-}
module Control.CCA where

import Control.Category
import Control.Arrow
import Prelude(undefined, show)

class Arrow a => ArrowInit a where
  init :: b -> a b b

instance (Arrow a, Category a) => Category (CCA a) where
  id  = Id
  (Arr f) . (Arr g) = Arr (f . g)
  (LoopD i f) . (Arr g) = LoopD i (first g >>> f)
  (Arr f) . (LoopD i g) = LoopD i (g >>> first f )
  (LoopD i f) . (LoopD j g) = LoopD (i, j) () -- LoopD i (c, i) (d, i) -> LoopD j (b, j) (c, j) -> LoopD :: (i, j) -> a (b, (i, j)) (d, (i, j)) -> CCA b d
  f . g = f :. g

instance Arrow a => Arrow (CCA a) where
  arr    = Arr . arr
  first  = First
  second = Second
  (***)  = (:***)
  (&&&)  = (:&&&)

instance Arrow a => ArrowLoop (CCA a) where
  loop = Loop

data CCA :: (* -> * -> *) -> * -> * -> * where
  Id     :: CCA a b b
  (:.)   :: Category a => CCA a d e -> CCA a c d -> CCA a c e
  Arr    :: a b c -> CCA a b c
  First  :: CCA a b c -> CCA a (b, d) (c, d)
  Second :: CCA a b c -> CCA a (d, b) (d, c)
  (:***) :: CCA a b c -> CCA a b' c' -> CCA a (b, b') (c, c')
  (:&&&) :: CCA a b c -> CCA a b c' -> CCA a b (c, c')
  Loop   :: CCA a (b, d) (c, d) -> CCA a b c
  LoopD  :: d -> a (b, d) (c, d) -> CCA a b c

  {-

  loopD :: ArrowInit a => d -> ((b, d) -> (c, d)) -> a b c
  loopD i f = loop (f >>> second (init i))

  assoc ((x, y), z) = (x, (y, z))
  associ (x, (y, z)) = ((x, y), z)
  assoc' f = assoc . f . associ
  trace f x = let (y, z) = f (x, z) in y
  juggle ((x, y), z) = ((x, z), y)
  juggle' f = juggle . f . juggle

  f × g (x, y) = (f x, g y)
  swap (x, y) = (y, x)

  composition:      arr f >>> arr g            -> arr (g . f)
  left tightening:  arr f >>> loopD i g        -> loopD i (g . (f × id))
  right tightening: loopD i f >>> arr g        -> loopD i ((g × id) . f)
  sequencing:       loopD i f >>> loopD j g    -> loopD (i, j) (assoc' (juggle' (g × id) . (f × id)))
  extension:        first (arr f)              -> arr (f × id)
  superposing:      first (loopD i f)          -> loopD i (juggle' (f × id))
  loop-extension    loop (arr f)               -> arr (trace f)
  vanishing:        loop (loopD i f)           -> loopD i (trace (juggle' f)
  -}