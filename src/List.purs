module List where

import Prelude (class Functor, class Show, show, (<<<), (<>))
import Data.String as String
import Matryoshka (class Corecursive, class Recursive, Algebra, cata)

import ListF (ListF(..))

data List a = Nil | Cons a (List a)

derive instance functorList :: Functor List

instance recursiveList :: Recursive (List a) (ListF a) where
  project = case _ of
    Nil -> NilF
    Cons x xs -> ConsF x xs

instance corecursiveList :: Corecursive (List a) (ListF a) where
  embed :: forall a. ListF a (List a) -> List a
  embed = case _ of
    NilF -> Nil
    ConsF x xs -> Cons x xs

instance showList :: Show a => Show (List a) where
  show = (_<>"]") <<< ("["<>_) <<< cata alg where
    alg :: Algebra (ListF a) String
    alg = case _ of
      NilF -> ""
      ConsF x xs -> if String.null xs
        then show x 
        else show x <> "," <> xs