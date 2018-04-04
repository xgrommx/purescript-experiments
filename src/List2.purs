module List2 where

import Prelude (class Show, show, (<<<), (<>))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.String as String
import Matryoshka (class Corecursive, class Recursive, Algebra, cata)

import MList (MList(..))

data List a = Nil | Cons a (List a)

instance recursiveList :: Recursive (List a) (MList a) where
  project = case _ of
    Nil -> MList Nothing
    Cons x xs -> MList (Just (Tuple x xs))

instance corecursiveList :: Corecursive (List a) (MList a) where
  embed :: forall a. MList a (List a) -> List a
  embed = case _ of
    MList Nothing -> Nil
    MList (Just (Tuple x xs)) -> Cons x xs

instance showList :: Show a => Show (List a) where
  show = (_<>"]") <<< ("["<>_) <<< cata alg where
    alg :: Algebra (MList a) String
    alg = case _ of
      MList Nothing -> ""
      MList (Just (Tuple x xs)) -> if String.null xs
        then show x 
        else show x <> "," <> xs