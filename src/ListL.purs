module ListL where

import Data.List (List(..))
import Matryoshka (class Corecursive, class Recursive)

import ListF (ListF(..))

newtype List' a = List' (List a)

unList :: forall a. List' a -> List a
unList (List' l) = l

instance recursiveList' :: Recursive (List' a) (ListF a) where
  project = case _ of
    List' Nil -> NilF
    List' (Cons x xs) -> ConsF x (List' xs)

instance corecursiveList' :: Corecursive (List' a) (ListF a) where
  embed = case _ of
    NilF -> List' Nil
    ConsF x (List' xs) -> List' (Cons x xs)