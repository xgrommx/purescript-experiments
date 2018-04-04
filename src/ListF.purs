module ListF where

import Prelude (class Functor, pure, (<$>), (<*>), (<>))
import Data.Traversable (class Foldable, class Traversable, foldlDefault, foldrDefault, sequenceDefault)
import Data.Monoid (mempty)
import Data.Bifunctor (class Bifunctor)
import Data.Bitraversable (class Bifoldable, class Bitraversable, bifoldlDefault, bifoldrDefault, bisequenceDefault)

data ListF a b = NilF | ConsF a b

derive instance functorListF :: Functor (ListF a)

instance bifunctorListF :: Bifunctor ListF where
  bimap f g = case _ of
    NilF -> NilF
    ConsF a b -> ConsF (f a) (g b)

instance foldableListF :: Foldable (ListF a) where
  foldr f = foldrDefault f
  foldl f = foldlDefault f
  foldMap _ NilF = mempty
  foldMap f (ConsF _ b) = f b

instance traversableListF :: Traversable (ListF a) where
  traverse _ NilF = pure NilF
  traverse f (ConsF a b) = ConsF a <$> f b
  sequence = sequenceDefault

instance bifoldableListF :: Bifoldable ListF where
  bifoldr f g = bifoldrDefault f g
  bifoldl f g = bifoldlDefault f g
  bifoldMap _ _ NilF = mempty
  bifoldMap f g (ConsF a b) = (f a) <> (g b)

instance bitraversavleListF :: Bitraversable ListF where
  bitraverse _ _ NilF = pure NilF
  bitraverse f g (ConsF a b) = ConsF <$> f a <*> g b
  bisequence = bisequenceDefault