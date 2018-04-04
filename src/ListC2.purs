module LictC2 where

import Prelude (class Functor, class Show, show, const, ($), (<<<), (+))
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (Tuple(..), uncurry)
import Data.String as String
import Data.Monoid ((<>))
import Matryoshka (class Corecursive, class Recursive, Algebra, cata)

import MList (MList(..), consM, nilM)

newtype ListC a = ListC (forall r. r -> (a -> r -> r) -> r)

unListC :: forall a r. ListC a -> r -> (a -> r -> r) -> r
unListC (ListC l) = l

instance functorListC :: Functor ListC where
  map f (ListC l) = ListC (\n c -> l n (c <<< f))

nil :: forall a. ListC a
nil = ListC const

cons :: forall a. a -> ListC a -> ListC a
cons h (ListC l) = ListC (\n c -> c h (l n c))

uncons :: forall a. ListC a -> Maybe (Tuple a (ListC a))
uncons m = unListC m Nothing $ \x xs -> Just (Tuple x (maybe nil (uncurry cons) xs))

instance recursiveListC :: Recursive (ListC a) (MList a) where
  project :: forall a. ListC a -> MList a (ListC a)
  project = maybe nilM (uncurry consM) <<< uncons

instance corecursiveListC :: Corecursive (ListC a) (MList a) where
  embed :: forall a. MList a (ListC a) -> ListC a
  embed = case _ of
    MList Nothing -> nil
    MList (Just (Tuple x xs)) -> cons x xs

instance showListC :: Show a => Show (ListC a) where
  show = (_<>"]") <<< ("["<>_) <<< cata alg where
    alg :: Algebra (MList a) String
    alg = case _ of
      MList Nothing -> ""
      MList (Just (Tuple x xs)) -> if String.null xs
        then show x 
        else show x <> "," <> xs 

lengthC :: forall a. ListC a -> Int
lengthC = cata alg where
  alg :: Algebra (MList a) Int
  alg = case _ of
    MList Nothing -> 0
    MList (Just (Tuple x xs)) -> xs + 1

sumC :: ListC Int -> Int
sumC = cata alg where
  alg :: Algebra (MList Int) Int
  alg = case _ of
    MList Nothing -> 0
    MList (Just (Tuple x xs)) -> x + xs       