module TreeC where

import Data.Bifunctor (lmap)
import Data.Maybe (Maybe(..), maybe)
import Matryoshka (class Corecursive, class Recursive, cata, embed)
import Prelude (class Functor, class Show, const, show, (<<<), (<>))
import TreeF (TreeF(..))

data Tuple3 a b c = Tuple3 a b c

newtype TreeC a = TreeC (forall r. r -> (r -> a -> r -> r) -> r)

unTree :: forall a r. TreeC a -> r -> (r -> a -> r -> r) -> r
unTree (TreeC t) = t

instance functorTreeC :: Functor TreeC where
    map f = cata (embed <<< lmap f)
--   map f (TreeC t) = TreeC (\n c -> t n (\x -> c x <<< f))

instance showTreeC :: Show a => Show (TreeC a) where
  show m = unTree m "Empty" (\l v r -> "(Node " <> l <> " " <> show v <> " " <> r <> ")")

instance recursiveTreeC :: Recursive (TreeC a) (TreeF a) where
  project :: forall a. TreeC a -> TreeF a (TreeC a)
  project = maybe EmptyF (\(Tuple3 l v r) -> NodeF l v r) <<< unNode

instance corecursiveTreeC :: Corecursive (TreeC a) (TreeF a) where
  embed :: forall a. TreeF a (TreeC a) -> TreeC a
  embed = case _ of
    EmptyF -> emptyC
    NodeF l v r -> nodeC l v r

emptyC :: forall a. TreeC a
emptyC = TreeC const

nodeC :: forall a. TreeC a -> a -> TreeC a -> TreeC a
nodeC l v r = TreeC (\e n -> n (unTree l e n) v (unTree r e n))

unNode :: forall a. TreeC a -> Maybe (Tuple3 (TreeC a) a (TreeC a))
unNode m = unTree m Nothing (\l v r -> Just (Tuple3
  (maybe emptyC (\(Tuple3 l1 v1 r1) -> nodeC l1 v1 r1) l)
  v
  (maybe emptyC (\(Tuple3 l2 v2 r2) -> nodeC l2 v2 r2) r)))