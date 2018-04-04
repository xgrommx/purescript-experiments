module Fix where

import Prelude
import Matryoshka (class Corecursive, class Recursive, cata, embed)

newtype Fix f = Fix (f (Fix f))

unFix :: forall f. Fix f -> f (Fix f)
unFix (Fix f) = f

instance recursiveFix :: Functor f => Recursive (Fix f) f where
  project :: forall f. Fix f -> f (Fix f)
  project = unFix

instance corecursiveFix :: Functor f => Corecursive (Fix f) f where
  embed :: forall f. f (Fix f) -> Fix f
  embed = Fix

refix :: forall r c f. Functor f => Recursive r f => Corecursive c f => r -> c
refix = cata embed

toFix :: forall t f. Functor f => Recursive t f => t -> Fix f
toFix = refix

fromFix :: forall t f. Functor f => Corecursive t f => Fix f -> t
fromFix = refix