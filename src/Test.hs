{-# Language KindSignatures, GADTs, TemplateHaskell #-}
import Control.Template

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import Control.CCA.Apply

foo = apply $(th) fns
