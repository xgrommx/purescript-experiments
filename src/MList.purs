module MList where

import Prelude (class Functor)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))

newtype MList a b = MList (Maybe (Tuple a b))

derive instance functorMList :: Functor (MList a)

unMList :: forall a b. MList a b -> Maybe (Tuple a b)
unMList (MList m) = m

nilM :: forall a b. MList a b
nilM = MList Nothing

consM :: forall a b. a -> b -> MList a b
consM x xs = MList (Just (Tuple x xs))