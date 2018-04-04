module Peano where

import Prelude
import Matryoshka (class Corecursive, class Recursive, Algebra, cata)

-- fixed point
data Peano = Zero | Succ Peano
-- base functor
data PeanoF a = ZeroF | SuccF a

derive instance functorPeanoF :: Functor PeanoF

instance recursivePeano :: Recursive Peano PeanoF where
  project :: Peano -> PeanoF Peano
  project = case _ of
    Zero -> ZeroF
    Succ a -> SuccF a

instance corecursivePeano :: Corecursive Peano PeanoF where
  embed :: PeanoF Peano -> Peano
  embed = case _ of
    ZeroF -> Zero
    SuccF a -> Succ a

instance showPeano :: Show Peano where
  show = show <<< cata alg where
    alg :: Algebra PeanoF Int
    alg = case _ of
      ZeroF -> 0
      SuccF a -> a + 1