module ListC where
  
import Data.Bifunctor (lmap)
import Data.Maybe (Maybe(..), maybe)
import Data.Monoid ((<>))
import Data.String as String
import Data.Tuple (Tuple(..), uncurry)
import ListF (ListF(..))
import Matryoshka (class Corecursive, class Recursive, Algebra, cata, embed)
import Prelude (class Functor, class Show, show, const, ($), (<<<), (+))

newtype ListC a = ListC (forall r. r -> (a -> r -> r) -> r)

unListC :: forall a r. ListC a -> r -> (a -> r -> r) -> r
unListC (ListC l) = l

instance functorListC :: Functor ListC where
  map f = cata (embed <<< lmap f)
--   map f (ListC l) = ListC (\n c -> l n (c <<< f))

nil :: forall a. ListC a
nil = ListC const

cons :: forall a. a -> ListC a -> ListC a
cons h (ListC l) = ListC (\n c -> c h (l n c))

uncons :: forall a. ListC a -> Maybe (Tuple a (ListC a))
uncons m = unListC m Nothing $ \x xs -> Just (Tuple x (maybe nil (uncurry cons) xs))

instance recursiveListC :: Recursive (ListC a) (ListF a) where
  project :: forall a. ListC a -> ListF a (ListC a)
  project = maybe NilF (uncurry ConsF) <<< uncons

instance corecursiveListC :: Corecursive (ListC a) (ListF a) where
  embed :: forall a. ListF a (ListC a) -> ListC a
  embed = case _ of
    NilF -> nil
    ConsF x xs -> cons x xs

instance showListC :: Show a => Show (ListC a) where
  show = (_<>"]") <<< ("["<>_) <<< cata alg where
    alg :: Algebra (ListF a) String
    alg = case _ of
      NilF -> ""
      ConsF x xs -> if String.null xs
        then show x 
        else show x <> "," <> xs 

lengthC :: forall a. ListC a -> Int
lengthC = cata alg where
  alg :: Algebra (ListF a) Int
  alg = case _ of
    NilF -> 0
    ConsF x xs -> xs + 1

sumC :: ListC Int -> Int
sumC = cata alg where
  alg :: Algebra (ListF Int) Int
  alg = case _ of
    NilF -> 0
    ConsF x xs -> x + xs   