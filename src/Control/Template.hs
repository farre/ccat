{-# Language KindSignatures, GADTs, TemplateHaskell #-}
module Control.Template where

import Control.Arrow
import Control.Category
import Control.Monad
import Control.CCA.Apply

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import Prelude(undefined, Show(..), (++))

data Template :: * -> * -> * where
  ArrTH :: ExpQ -> (a -> b) -> Template a b
  (:.) :: Template b c -> Template a b -> Template a c
  First :: Template a b -> Template (a, c) (b, c)

instance Category Template where
  id = ArrTH lambdaLift id
  (.) = (:.)

instance Arrow Template where
  arr = ArrTH lambdaLift
  first = First

lambdaLift = do
 n <- newName "f"
 lamE [varP n] (varE n)

foo = [| \x -> x |]

runArrow (ArrTH th f) fns = (th, \t -> (f, t))

test :: Template a a
test = arr id

(th, fns) = let (th', fns') = runArrow test (\x -> x) in (th', fns' ())
