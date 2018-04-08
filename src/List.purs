module List where

import Prelude

import Control.Comonad.Cofree as C
import Control.Monad.Free (liftF)
import Data.Bifoldable (bifoldMap)
import Data.Either (Either(..))
import Data.Foldable (class Foldable, foldlDefault, foldr, foldrDefault)
import Data.Maybe (Maybe(..))
import Data.Monoid.Endo (Endo(..))
import Data.Newtype (unwrap)
import Data.String as String
import Data.Tuple (Tuple(..))
import ListF (ListF(..))
import Matryoshka (class Corecursive, class Recursive, Algebra, ana, apo, cata, futu, histo, para, postpro, prepro)

data Tuple3 a b c = Tuple3 a b c

instance showTuple3 :: (Show a, Show b, Show c) => Show (Tuple3 a b c) where
  show (Tuple3 a b c) = "Tuple3 " <> show a <> " " <> show b <> " " <> show c

data List a = Nil | Cons a (List a)

derive instance functorList :: Functor List

instance foldableList :: Foldable List where
  foldr f = foldrDefault f
  foldl f = foldlDefault f
  foldMap f = cata (bifoldMap f id)

instance recursiveList :: Recursive (List a) (ListF a) where
  project = case _ of
    Nil -> NilF
    Cons x xs -> ConsF x xs

instance corecursiveList :: Corecursive (List a) (ListF a) where
  embed :: forall a. ListF a (List a) -> List a
  embed = case _ of
    NilF -> Nil
    ConsF x xs -> Cons x xs

instance showList :: Show a => Show (List a) where
  show = (_<>"]") <<< ("["<>_) <<< cata alg where
    alg :: Algebra (ListF a) String
    alg = case _ of
      NilF -> ""
      ConsF x xs -> if String.null xs
        then show x 
        else show x <> "," <> xs

fromFoldable :: forall f. Foldable f => f ~> List
fromFoldable = foldr Cons Nil

take :: forall a. Int -> List a -> List a
take x y = para go y x where
  go NilF _ = Nil
  go (ConsF a (Tuple _ k)) i
    | i == 0 = Nil
    | otherwise = Cons a (k (i - 1))

reverse :: forall a. List a -> List a
reverse as = unwrap (cata alg as) Nil where
  alg :: Algebra (ListF a) (Endo (List a))
  alg NilF = Endo id
  alg (ConsF x xs) = xs <> Endo (Cons x)

take' :: forall a. Int -> List a -> List a
take' x y = cata go y x where
  go NilF _ = Nil
  go (ConsF a k) i
    | i == 0 = Nil
    | otherwise = Cons a (k (i - 1))

drop :: forall a. Int -> List a -> List a
drop = flip (para go) where
  go NilF _ = Nil
  go (ConsF a (Tuple as k)) i
      | i == 0    = Cons a as
      | otherwise = k (i-1)

takeWhile :: forall a. (a -> Boolean) -> List a -> List a
takeWhile k = cata go where
  go NilF = Nil
  go (ConsF a as)
      | k a       = Cons a as
      | otherwise = Nil

dropWhile :: forall a. (a -> Boolean) -> List a -> List a
dropWhile p = para go where
  go NilF = Nil
  go (ConsF a (Tuple as fas))
      | p a       = fas
      | otherwise = Cons a as

delete :: forall a. Eq a => a -> List a -> List a
delete x = para go where
  go NilF = Nil
  go (ConsF a (Tuple as fas))
      | a == x    = as
      | otherwise = Cons a fas

append :: forall a. List a -> List a -> List a
append xs vs = cata go xs where
  go NilF         = vs
  go (ConsF a as) = Cons a as

head :: forall a. List a -> Maybe a
head = cata go where
  go NilF        = Nothing
  go (ConsF a _) = Just a

tail :: forall a. List a -> Maybe (List a)
tail = para go where
  go NilF              = Nothing
  go (ConsF _ (Tuple xs _)) = Just xs

find :: forall a. (a -> Boolean) -> List a -> Maybe a
find k = cata go where
  go NilF = Nothing
  go (ConsF a aOpt)
      | k a       = Just a
      | otherwise = aOpt

member :: forall a. Eq a => a -> List a -> Boolean
member m = cata go where
  go NilF = false
  go (ConsF x xs)
    | m == x = true
    | otherwise = xs

last :: forall a. List a -> Maybe a
last = cata go where
  go NilF        = Nothing
  go (ConsF x a) = a

sum :: List Int -> Int
sum = cata $ case _ of
  NilF -> 0
  ConsF x xs -> x + xs

foldr' :: forall a b. (a -> b -> b) -> b -> List a -> b
foldr' k b = cata go where
  go NilF         = b
  go (ConsF a b1) = k a b1

foldl' :: forall a b. (b -> a -> b) -> b -> List a -> b
foldl' k b mu = foldr' go id mu b
  where
    go a f = f <<< flip k a

inits :: forall a. List a -> List (List a)
inits = cata go where
    go NilF = Cons Nil Nil
    go (ConsF a b) = Cons Nil (map (Cons a) b)

map' :: forall a b. (a -> b) -> List a -> List b
map' f = cata go where
  go NilF         = Nil
  go (ConsF a bs) = Cons (f a) bs

insert :: forall a. Ord a => a -> List a -> List a
insert v = para phi where
  phi = case _ of
    NilF -> Cons v Nil
    ConsF x (Tuple xs ys)
      | v <= x -> Cons v (Cons x xs)
      | otherwise -> Cons x ys

scanr :: forall a b. Partial => (a -> b -> b) -> b -> List a -> List b
scanr f a = cata $ case _ of
  NilF -> Cons a Nil
  ConsF x xs@(Cons x' _) -> Cons (f x x') xs

tails :: forall a. List a -> List (List a)
tails = para alg where
  alg = case _ of 
    NilF -> Cons Nil Nil
    ConsF a (Tuple as res) -> Cons (Cons a as) res

init :: forall a. List a -> List a
init = apo f where
  f = case _ of
    Nil -> NilF
    Cons x Nil -> NilF
    Cons x xs -> ConsF x (Right xs)

init' :: forall a. List a -> List a
init' = futu f where
  f = case _ of
    Nil       -> NilF
    Cons x Nil -> NilF
    Cons x s  -> ConsF x (pure s)

uniq :: forall a. Eq a => List a -> List a
uniq = para alg where
  alg = case _ of
    NilF -> Nil
    ConsF x (Tuple t a) -> if x `member` t then a else Cons x a

partitionByCond :: forall a. (a -> a -> Boolean) -> List a -> List (List a)
partitionByCond cond = para psi where
  psi = case _ of
    NilF -> Nil
    ConsF x (Tuple (Cons a _) (Cons a' b')) | cond x a -> Cons (Cons x a') b'
    ConsF x (Tuple _ r) -> Cons (Cons x Nil) r

zipAlg3 :: forall a b c a' b' c' f. Functor f => (f a -> a') -> (f b -> b') -> (f c -> c') -> f (Tuple3 a b c) -> Tuple3 a' b' c'
zipAlg3 f g h x = Tuple3 (f $ map (\(Tuple3 x' _ _) -> x') x) (g $ map (\(Tuple3 _ x' _) -> x') x) (h $ map (\(Tuple3 _ _ x') -> x') x)

h :: forall a. (a -> Boolean) -> (a -> Boolean) -> (a -> Boolean) -> List a -> Tuple3 (List a) (List a) (List a)
h p1 p2 p3 = cata (zipAlg3 (f p1) (f p2) (f p3)) where
  f :: (a -> Boolean) -> Algebra (ListF a) (List a)
  f p = case _ of
    NilF -> Nil
    ConsF a b -> if p a then (Cons a b) else b

h2 :: forall a. (a -> Boolean) -> (a -> Boolean) -> (a -> Boolean) -> List a -> Tuple3 Boolean Boolean Boolean
h2 p1 p2 p3 = cata (zipAlg3 (f p1) (f p2) (f p3)) where
  f :: (a -> Boolean) -> Algebra (ListF a) Boolean
  f p = case _ of
    NilF -> true
    ConsF a b -> p a && b

all :: forall a. (a -> Boolean) -> List a -> Boolean
all k = cata go where
  go NilF        = true
  go (ConsF a b) = k a && b

any :: forall a. (a -> Boolean) -> List a -> Boolean
any k = cata go where
  go NilF       = true
  go (ConsF a b) = k a || b

iterate :: forall a. (a -> a) -> a -> List a
iterate k = ana go where
  go a = ConsF a (k a)

repeat :: forall a. a -> List a
repeat = iterate id

replicate :: forall a. Int -> a -> List a
replicate start a = ana go start where
  go 0 = NilF
  go i = ConsF a (i-1)

-- minimum :: forall a. Ord a => List a -> a
-- minimum = foldr1 min

-- maximum :: forall a. Ord a => List a -> a
-- maximum = foldr1 max

insertionSort :: forall a. Ord a => List a -> List a
insertionSort = foldr (para <<< go) Nil where
  go x NilF = Cons x Nil
  go x (ConsF a (Tuple as fas))
      | x < a     = Cons x as
      | otherwise = Cons a fas

-- selectionSort :: Ord a => List a -> List a
-- selectionSort = ana go where
--   go xs =
--       let a   = minimum xs
--           xs1 = delete a xs in
--       if null xs
--           then Cons a xs1
--           else Nil

bubbleSort :: forall a. Ord a => List a -> List a
bubbleSort = ana (foldr go NilF) where
  go x NilF = ConsF x Nil
  go x (ConsF y ys)
      | x < y     = ConsF x (Cons y ys)
      | otherwise = ConsF y (Cons x ys)

bind :: forall a b. (a -> List b) -> List a -> List b
bind f = cata go
  where
    go NilF         = Nil
    go (ConsF a bs) = append (f a) bs

-- https://jtobin.io/time-traveling-recursion

oddIndices :: forall a. List a -> List a
oddIndices = histo go where
  go = case _ of
    NilF -> Nil
    ConsF h t -> case C.tail t of
      NilF -> Cons h Nil
      ConsF _ t' -> Cons h (C.head t')

evenIndices :: forall a. List a -> List a
evenIndices = histo go where
  go = case _ of
    NilF -> Nil
    ConsF _ t -> case C.tail t of
      NilF -> Nil
      ConsF h t' -> Cons h (C.head t')

oddIndicesF :: forall a. List a -> List a
oddIndicesF = futu coalg where
  coalg = case _ of
    Nil -> NilF
    Cons x s -> ConsF x $ do
      pure $ case s of
        Nil -> s
        Cons _ t -> t

evenIndicesF :: forall a. List a -> List a
evenIndicesF = futu coalg where
  coalg = case _ of
    Nil -> NilF
    Cons _ s -> case s of
      Nil -> NilF
      Cons h t -> ConsF h $ pure t

twiddle :: forall a. List a -> List a
twiddle = futu coalg where
  coalg = case _ of
    Nil -> NilF
    Cons x l -> case l of
      Nil -> ConsF x (liftF NilF)
      Cons h t -> ConsF h $ liftF (ConsF x t)

-- https://jtobin.io/promorphisms-pre-post

sumAlg :: ListF Int Int -> Int
sumAlg = case _ of
  ConsF h t -> h + t
  NilF -> 0

lenAlg :: forall a. ListF a Int -> Int
lenAlg = case _ of
  ConsF h t -> 1 + t
  NilF -> 0

-- sum :: List Int -> Int
-- sum = cata sumAlg

-- len :: List Int -> Int
-- len = cata lenAlg

-- smallSumAlg :: ListF Int Int -> Int
-- smallSumAlg = case _ of
--   ConsF h t ->
--     if   h <= 10
--     then h + t
--     else 0
--   NilF      -> 0

-- smallLenAlg :: ListF Int Int -> Int
-- smallLenAlg = case _ of
--   ConsF h t ->
--     if   h <= 10
--     then 1 + t
--     else 0
--   NilF -> 0

small :: forall a. ListF Int a -> ListF Int a
small NilF = NilF
small term@(ConsF h t)
  | h <= 10   = term
  | otherwise = NilF

smallSum :: List Int -> Int
smallSum = prepro small sumAlg

smallLen :: List Int -> Int
smallLen = prepro small lenAlg

streamCoalg :: Int -> ListF Int Int
streamCoalg n = ConsF n (n + 1)

smallStream :: Int -> List Int
smallStream = postpro small streamCoalg