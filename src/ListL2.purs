module ListL2 where

import Data.List (List(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Matryoshka (class Corecursive, class Recursive)

import MList (MList(..))

newtype List' a = List' (List a)

unList :: forall a. List' a -> List a
unList (List' l) = l

instance recursiveList' :: Recursive (List' a) (MList a) where
  project = case _ of
    List' Nil -> MList Nothing
    List' (Cons x xs) -> MList (Just (Tuple x (List' xs)))

instance corecursiveList' :: Corecursive (List' a) (MList a) where
  embed = case _ of
    MList Nothing -> List' Nil
    MList (Just (Tuple x (List' xs))) -> List' (Cons x xs)