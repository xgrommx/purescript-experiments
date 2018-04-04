module Peano2 where

import Prelude
import Data.Maybe (Maybe(..))
import Matryoshka (class Corecursive, class Recursive, Algebra, cata, embed)

data Peano = Zero | Succ Peano

instance recursivePeano :: Recursive Peano Maybe where
  project :: Peano -> Maybe Peano
  project = case _ of
    Zero -> Nothing
    Succ a -> Just a
  
instance corecursivePeano :: Corecursive Peano Maybe where
  embed :: Maybe Peano -> Peano
  embed = case _ of
    Nothing -> Zero
    Just a -> Succ a

instance showPeano :: Show Peano where
  show = show <<< cata alg where
    alg = case _ of
      Nothing -> 0
      Just a -> a + 1