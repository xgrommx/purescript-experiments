module RoseTree where

import Control.Monad.Writer (Writer, tell)
import Data.Bifunctor (class Bifunctor, lmap)
import Data.Bitraversable (class Bifoldable, class Bitraversable, bifoldMap, bifoldlDefault, bifoldrDefault, bisequenceDefault, bitraverse)

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)

import Data.Monoid (mempty)
import Data.Monoid.Additive (Additive(..))
import Data.Monoid.Endo (Endo(..))
import Data.Traversable (class Foldable, class Traversable, fold, foldMap, foldlDefault, foldr, foldrDefault, sequenceDefault, traverse)
import Prelude (class Functor, class Semiring, class Show, Unit, discard, id, map, pure, show, unit, ($), (+), (<$>), (<<<), (<>))

import Matryoshka (class Corecursive, class Recursive, Algebra, AlgebraM, cata, embed)

data RoseTree e = Branch (Array (RoseTree e)) | Leaf e
data RoseTreeF e r = BranchF (Array r) | LeafF e

derive instance genericTree :: Generic (RoseTree a) _

instance functorRoseTreeF :: Functor (RoseTreeF a) where
  map f (LeafF a) = LeafF a
  map f (BranchF as) = BranchF (map f as)

instance foldableRoseTreeF :: Foldable (RoseTreeF a) where
  foldr f = foldrDefault f
  foldl f = foldlDefault f
  foldMap f (LeafF a) = mempty
  foldMap f (BranchF as) = foldMap f as

instance traversableRoseTreeF :: Traversable (RoseTreeF a) where
  traverse f (LeafF a) = pure (LeafF a)
  traverse f (BranchF as) = map BranchF (traverse f as)
  sequence f = sequenceDefault f

instance bifoldableRoseTreeF :: Bifoldable RoseTreeF where
  bifoldr f g = bifoldrDefault f g
  bifoldl f g = bifoldlDefault f g
  bifoldMap f _ (LeafF e)    = f e
  bifoldMap _ g (BranchF xs) = foldMap g xs

instance foldableRoseTree :: Foldable RoseTree where
  foldr f = foldrDefault f
  foldl f = foldlDefault f
  foldMap f = cata (bifoldMap f id)

instance bitravsersableRoseTreeF :: Bitraversable RoseTreeF where
  bitraverse f _ (LeafF e)    = LeafF <$> f e
  bitraverse _ g (BranchF xs) = BranchF <$> traverse g xs
  bisequence = bisequenceDefault

instance traversableRoseTree :: Traversable RoseTree where
  traverse f = cata (map embed <<< bitraverse f id)
  sequence = sequenceDefault

instance recursiveRoseTree ∷ Recursive (RoseTree a) (RoseTreeF a) where
  project x = case x of
    Leaf a -> LeafF a
    Branch as -> BranchF as

instance corecursiveRoseTree ∷ Corecursive (RoseTree a) (RoseTreeF a) where
  embed x = case x of
    LeafF a -> Leaf a
    BranchF as -> Branch as

instance bifuctorRoseTreeF :: Bifunctor RoseTreeF where
    bimap f _ (LeafF e)    = LeafF (f e)
    bimap _ g (BranchF xs) = BranchF (map g xs)

instance functorRoseTree :: Functor RoseTree where
    map f = cata (embed <<< lmap f)

instance showTree :: Show a => Show (RoseTree a) where
  show x = genericShow x

alg :: RoseTreeF Int Int -> Int
alg = case _ of
    LeafF a -> a
    BranchF as -> foldr(+) 0 as

alg2 :: forall a. Semiring a => Algebra (RoseTreeF a) (Additive a)
alg2 = case _ of
    LeafF a -> Additive a
    BranchF as -> fold as  

alg3 :: forall a. Algebra (RoseTreeF a) (Endo (Array a))
alg3 = case _ of
    LeafF a -> Endo (\x -> x <> [a])
    BranchF as -> fold as

tree :: RoseTree Int
tree = Branch [Leaf 10, Leaf 20, Branch [Leaf 30, Leaf 40]]

evalM :: AlgebraM (Writer (Array String)) (RoseTreeF Int) Unit
evalM (LeafF a) = do
  tell $ [ "visiting leaf " <> show a]
  pure unit
evalM (BranchF as) = pure unit