module MaybeC where

import Prelude

newtype MaybeC a = MaybeC (forall r. r -> (a -> r) -> r)

unMaybeC :: forall a r. MaybeC a -> r -> (a -> r) -> r
unMaybeC (MaybeC m) = m

nothing :: forall a. MaybeC a
nothing = MaybeC const

just :: forall a. a -> MaybeC a
just val = MaybeC (\_ f -> f val)

instance functorMaybeC :: Functor MaybeC where
  map f (MaybeC m) = MaybeC (\n j -> m n (j <<< f))