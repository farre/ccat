{-# Language KindSignatures, GADTs, TemplateHaskell #-}
module Control.Template where

import Control.Arrow
import Control.Category
import Control.CCA

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import Prelude(undefined, Show(..))

type NameQ = Q Name

class Args a where

data Template :: * -> * -> *  where
  ArrTH :: Args args => NameQ -> args -> Template a a

instance Category Template where
  id = ArrTH newName (id, ())
  f . g = undefined

instance Arrow Template where
  arr = undefined
  first = undefined

foo = [| \x -> x |]

runArrow (ArrTH th) = runQ th
