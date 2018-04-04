module Tree where

import Prelude (class Functor)
import TreeF (TreeF(..))

import Matryoshka (class Corecursive, class Recursive)

data Tree a = Empty | Node (Tree a) a (Tree a)

derive instance functorTree :: Functor Tree

instance recursiveTree :: Recursive (Tree a) (TreeF a) where
  project :: forall a. Tree a -> TreeF a (Tree a)
  project = case _ of
    Empty -> EmptyF
    Node l v r -> NodeF l v r

instance corecursiveTree :: Corecursive (Tree a) (TreeF a) where
  embed :: forall a. TreeF a (Tree a) -> Tree a
  embed = case _ of
    EmptyF -> Empty
    NodeF l v r -> Node l v r