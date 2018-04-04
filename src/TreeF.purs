module TreeF where

import Prelude

import Data.Bifunctor (class Bifunctor)

data TreeF a b = EmptyF | NodeF b a b

derive instance functorTreeF :: Functor (TreeF a)

instance bifunctorTreeF :: Bifunctor TreeF where
  bimap f g = case _ of
    EmptyF -> EmptyF
    NodeF l v r -> NodeF (g l) (f v) (g r)