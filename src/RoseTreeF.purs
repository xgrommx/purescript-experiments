module RoseTreeF where

import Data.Bifunctor (class Bifunctor)
import Data.Bitraversable (class Bifoldable, class Bitraversable, bifoldlDefault, bifoldrDefault, bisequenceDefault)

import Data.Monoid (mempty)
import Data.Traversable (class Foldable, class Traversable, foldMap, foldlDefault, foldrDefault, sequenceDefault, traverse)
import Prelude (class Functor, map, pure, (<$>))


data RoseTreeF e r = BranchF (Array r) | LeafF e

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

instance bitravsersableRoseTreeF :: Bitraversable RoseTreeF where
  bitraverse f _ (LeafF e)    = LeafF <$> f e
  bitraverse _ g (BranchF xs) = BranchF <$> traverse g xs
  bisequence = bisequenceDefault

instance bifuctorRoseTreeF :: Bifunctor RoseTreeF where
    bimap f _ (LeafF e)    = LeafF (f e)
    bimap _ g (BranchF xs) = BranchF (map g xs)