module ListL where

import Data.Array (foldr, (:))
import Data.Foldable (class Foldable)
import Data.List (List(..))
import Data.Newtype (class Newtype)
import Data.String as String
import ListF (ListF(..))
import Matryoshka (class Corecursive, class Recursive, Algebra, ana, cata, embed, hylo, project)
import Partial.Unsafe (unsafeCrashWith)
import Prelude (class Functor, class Show, Unit, compose, map, otherwise, show, unit, ($), (+), (-), (<<<), (<>), (==))
import StackSafe.Function (Func(..), run)

newtype List' a = List' (List a)

derive instance newtypeList' :: Newtype (List' a) _

instance recursiveList' :: Recursive (List' a) (ListF a) where
  project = case _ of
    List' Nil -> NilF
    List' (Cons x xs) -> ConsF x (List' xs)

instance corecursiveList' :: Corecursive (List' a) (ListF a) where
  embed = case _ of
    NilF -> List' Nil
    ConsF x (List' xs) -> List' (Cons x xs)

data L a = N | C a (Func Unit (L a))
data L' a b = N' | C' a (Func Unit b)

instance functorL' :: Functor (L' a) where
  map f N' = N'
  map f (C' a b) = C' a (Func \_ -> f $ run b unit)

instance recursiveL :: Recursive (L a) (L' a) where
  project = case _ of
    N -> N'
    C x xs -> C' x xs

instance correcursiveL :: Corecursive (L a) (L' a) where
  embed = case _ of
    N' -> N
    C' x xs -> C x xs

instance showL' :: Show a => Show (L a) where
  show = (_<>"]") <<< ("["<>_) <<< cata alg where
    alg :: Algebra (L' a) String
    alg = case _ of
      N' -> ""
      C' x xs -> if String.null (run xs unit)
        then show x 
        else show x <> "," <> (run xs unit)

take' :: forall a. Int -> L a -> Array a
take' x y = cata go y x where
  go N' _ = []
  go (C' a k) i
    | i == 0 = []
    | otherwise = a : (run k unit (i - 1))

range' :: Int -> L Int
range' = ana (\n -> C' n (Func \_ -> n + 1))

toArray' :: forall a. L a -> Array a
toArray' N = []
toArray' (C x xs) = x : toArray' (run xs unit)

-- data L a = N | C a (Unit -> L a)

-- head' :: forall a. L a -> a
-- head' = case _ of 
--   C x xs -> x
--   N -> unsafeCrashWith "N"

-- tail' :: forall a. L a -> L a
-- tail' = case _ of
--   C _ xs -> xs unit
--   N -> unsafeCrashWith "N"

-- cons' :: forall a. a -> L a -> L a
-- cons' x xs = C x (\_ -> xs)

range'' :: Int -> L Int
range'' n = go n where go n = C n (Func \_ -> go (n + 1))

-- take' :: forall a. Int -> L a -> Array a
-- take' _ N = unsafeCrashWith "N"
-- take' 0 xs = []
-- take' n (C x xs) = x : take' (n - 1) (xs unit)

-- map' :: forall a b. (a -> b) -> L a -> L b
-- map' f N = N
-- map' f (C x xs) = C (f x) (\_ -> map' f (xs unit))

-- filter' :: forall a. (a -> Boolean) -> L a -> L a
-- filter' _ N = N
-- filter' p (C x xs) = if p x then C x (\_ -> filter' p (xs unit)) else filter' p (xs unit)

-- iterate' :: forall a. (a -> a) -> a -> L a
-- iterate' f x = C x (\_ -> iterate' f (f x))

-- toArray' :: forall a. L a -> Array a
-- toArray' N = []
-- toArray' (C x xs) = x : toArray' (xs unit)

composeGo :: forall a. Func a a -> Func a a -> Int -> Func a a
composeGo f acc n = if n == 100000 then acc else composeGo f (compose acc f) (n + 1)

composeGo' :: forall a. (a -> a) -> (a -> a) -> Int -> (a -> a)
composeGo' f acc n = if n == 100000 then acc else composeGo' f (compose acc f) (n + 1)

-- fromArray' :: forall f a. Foldable f => f a -> L a
-- fromArray' = foldr cons' N
