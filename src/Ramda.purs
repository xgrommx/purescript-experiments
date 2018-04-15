module Ramda where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, logShow)
import Data.Foldable (traverse_)
import Data.Map (Map, alter, empty, lookup)
import Data.Map as M
import Data.Maybe (Maybe(..), maybe)
import Data.Profunctor.Strong ((&&&))
import Data.Tuple (Tuple(..))
import Fix (refix)
import List (List(..), fromFoldable)
import ListF (ListF(..))
import Matryoshka (class Corecursive, class Recursive, Algebra, cata, embed, hylo, project)
import Prelude (class Functor, class Ord, Unit, discard, id, map, ($), (+), (<$>), (<<<))

newtype XMap k v = XMap (Map k (List v))

unXMap :: forall k v. XMap k v -> Map k (List v)
unXMap (XMap m) = m

instance corecursiveXMap :: Ord k => Corecursive (XMap k v) (ListF (Tuple k v)) where
  embed = case _ of
    NilF -> XMap empty
    ConsF (Tuple key elt) acc -> XMap <<< alter (Just <<< maybe (Cons elt Nil) (\x -> Cons elt x)) key $ unXMap acc

reduceBy' :: forall a b c v k f. Functor f => Ord k => Recursive (f (Tuple c a)) (ListF (Tuple k v)) => (ListF v b -> b) -> (a -> c) -> f a -> M.Map k b
reduceBy' valueAlgebra keyFn = map (cata valueAlgebra) <<< unXMap <<< (hylo embed project) <<< map (keyFn &&& id)

reduceBy :: forall k t b. Ord k => Algebra (ListF t) b -> (t -> k) -> List t -> Map k b
reduceBy valueAlgebra keyFn x =
    cata valueAlgebra <$> cata groupAlgebra x where
        groupAlgebra = case _ of
            NilF -> empty
            ConsF elt acc -> alter (Just <<< maybe (Cons elt Nil) (\x -> Cons elt x)) (keyFn elt) acc

countBy :: forall a k. Ord k => (a -> k) -> List a -> Map k Int
countBy = reduceBy' $ case _ of
  NilF -> 0
  ConsF _ b -> b + 1

groupBy :: forall a k b. Ord k => Corecursive b (ListF a) => (a -> k) -> List a -> Map k b
-- groupBy :: forall k a. Ord k => (a -> k) -> List a -> Map k (List a)
groupBy = reduceBy' embed --(embed :: forall a. ListF a (List a) -> List a)

indexBy :: forall k a b. Ord k => (Map a b -> k) -> List (Map a b) -> Map k (Map a b)
indexBy = reduceBy' $ case _ of
  NilF -> empty
  ConsF a _ -> a

x :: Map String Int
x = countBy id $ fromFoldable ["f", "o", "o"]

y :: Map String (List String)
y = groupBy id $ fromFoldable ["f", "o", "o"]

z :: Map (Maybe String) (Map String String)
z = indexBy (lookup "id") $ fromFoldable [M.fromFoldable [Tuple "id" "xyz", Tuple "title" "A"], M.fromFoldable [Tuple "id" "abc", Tuple "title" "B"]]

test :: forall e. Eff (console :: CONSOLE | e) Unit
test = do
    traverse_ logShow (Tuple x y)
    logShow z            