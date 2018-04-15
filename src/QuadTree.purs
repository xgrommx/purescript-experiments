module QuadTree where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, logShow)
import Data.Array ((:))
import Data.Array as A
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.List (List(..), drop, length, take)
import Matryoshka (class Corecursive, class Recursive, cata, embed, hylo, project)
import Partial.Unsafe (unsafePartial)

-- https://jtobin.io/rotating-squares

data QuadTree a =
    Node (QuadTree a) (QuadTree a) (QuadTree a) (QuadTree a)
  | Leaf a
  | Empty

data QuadTreeF a r =
    NodeF r r r r
  | LeafF a
  | EmptyF

derive instance functorQuadTreeF :: Functor (QuadTreeF a)

derive instance genericQuadTree :: Generic (QuadTree a) _

instance showQuadTree :: Show a => Show (QuadTree a) where
  show x = genericShow x

instance recursiveQuadTree :: Recursive (QuadTree a) (QuadTreeF a) where
  project = case _ of
    Node x y z w -> NodeF x y z w
    Leaf x -> LeafF x
    Empty -> EmptyF

instance corecursiveQuadTree :: Corecursive (QuadTree a) (QuadTreeF a) where
  embed = case _ of
    NodeF x y z w -> Node x y z w
    LeafF x -> Leaf x
    EmptyF -> Empty

tree :: QuadTree Boolean
tree = Node ul ur lr ll where
  ul = Node (Leaf false) (Leaf true) (Leaf false) (Leaf false)
  ur = Node (Leaf false) (Leaf false) (Leaf false) (Leaf true)
  lr = Node (Leaf true) (Leaf false) (Leaf false) (Leaf false)
  ll = Node (Leaf true) (Leaf true) (Leaf false) (Leaf false)

tree' :: QuadTree Int
tree' = Node ul ur lr ll where
  ul = Node (Leaf 1) (Leaf 2) (Leaf 3) (Leaf 4)
  ur = Node (Leaf 5) (Leaf 6) (Leaf 7) (Leaf 8)
  lr = Node (Leaf 9) (Leaf 10) (Leaf 11) (Leaf 12)
  ll = Node (Leaf 13) (Leaf 14) (Leaf 15) (Leaf 16)  

chunks :: forall a. Int -> Array a -> Array (Array a)
chunks _ [] = []
chunks n xs = pure (A.take n xs) <> (chunks n $ A.drop n xs)

rotate :: forall a. QuadTree a -> QuadTree a
rotate = cata $ case _ of
  NodeF ul ur lr ll -> Node ll ul ur lr
  LeafF a           -> Leaf a
  EmptyF            -> Empty

builder :: forall a. Partial => Array a -> QuadTreeF a (Array a)
builder = case _ of
  []  -> EmptyF
  [x] -> LeafF x
  xs -> let [a, b, c, d] = chunks (A.length xs `div` 4) xs in NodeF a b c d

consumer :: forall a. QuadTreeF a (Array a) -> Array a
consumer = case _ of
  EmptyF            -> []
  LeafF a           -> [a]
  NodeF ul ur lr ll -> A.concat [ll, ul, ur, lr]

rotateArray :: forall a. Array a -> Array a
rotateArray = hylo consumer (unsafePartial builder)

example2 :: forall e. Eff (console :: CONSOLE | e) Unit
example2 = do
  logShow $ rotate tree'
  logShow $ rotateArray [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16]
  logShow $ rotateArray [1,2,3,4]
  logShow $ A.drop 3 <> A.take 3 $ [1,2,3,4]