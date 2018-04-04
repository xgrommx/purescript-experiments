module Peano3 where

import Prelude (class Show, show, (+), (<<<))
import Matryoshka (class Corecursive, class Recursive, cata)

import MaybeC (MaybeC, just, nothing, unMaybeC)

data Peano = Zero | Succ Peano

instance recursivePeano :: Recursive Peano MaybeC where
  project :: Peano -> MaybeC Peano
  project = case _ of
    Zero -> nothing
    Succ a -> just a

instance corecursivePeano :: Corecursive Peano MaybeC where
  embed :: MaybeC Peano -> Peano
  embed m = unMaybeC m Zero Succ

instance showPeano :: Show Peano where
  show = show <<< cata alg where
    alg m = unMaybeC m 0 (_+1)