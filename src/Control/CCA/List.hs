{-# Language TypeFamilies, MultiParamTypeClasses, FlexibleInstances #-}
module Control.CCA.List where

class List l
instance List ()
instance List l => List (e, l)

nil :: ()
nil =  ()

cons :: List l => e -> l -> (e, l)
cons e l = (e, l)

class (L l l' ~ l'') => Append l l' l'' where
  type L l l'
  append :: l -> l' -> l''

instance List l => Append () l l where
  type L () l = l
  append () l = l

instance (List l'', Append l l' l'') => Append (x, l) l' (x, l'') where
  type L (x, l) l' = (x, L l l')
  append (x, l) l' = cons x (append l l')

