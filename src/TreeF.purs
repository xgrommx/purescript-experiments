module TreeF where

import Prelude

data TreeF a b = EmptyF | NodeF b a b

derive instance functorTreeF :: Functor (TreeF a)