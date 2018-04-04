module NaturalC where

import Prelude (class Semiring, class Show, const, flip, id, show, ($), (+), (<<<))
import Matryoshka (class Corecursive, class Recursive, Algebra, cata)

import MaybeC (MaybeC, just, nothing, unMaybeC)

newtype NaturalC = NaturalC (forall r. r -> (r -> r) -> r)

unNaturalC :: forall r. NaturalC -> r -> (r -> r) -> r
unNaturalC (NaturalC n) = n

zeroC :: NaturalC
zeroC = NaturalC const

oneC :: NaturalC
oneC = NaturalC (\x f -> f x)

twoC :: NaturalC
twoC = NaturalC (\x f -> f (f x))

threeC :: NaturalC
threeC = NaturalC (\x f -> f (f (f x)))

succ' :: NaturalC -> NaturalC
succ' n = NaturalC (\x f -> f(unNaturalC n x f))

pred' :: NaturalC -> NaturalC
pred' n = NaturalC (\x f -> unNaturalC n (const x) (\g h -> h (g f)) id)

instance recursiveNaturalC :: Recursive NaturalC MaybeC where
  project :: NaturalC -> MaybeC NaturalC
  project x = unNaturalC x nothing (const (just $ pred' x))

instance corecursiceNaturalC :: Corecursive NaturalC MaybeC where
  embed :: MaybeC NaturalC -> NaturalC
  embed m = unMaybeC m zeroC succ'

instance semiringNaturalC :: Semiring NaturalC where
  add = addC
  zero = zeroC
  mul = cata <<< algN zeroC <<< addC
  one = oneC

instance showNaturalC :: Show NaturalC where
  show = show <<< cata (algN 0 (_+1))

algN :: forall a. a -> (a -> a) -> Algebra MaybeC a
algN n j m = unMaybeC m n j

addC :: NaturalC -> NaturalC -> NaturalC
addC = cata <<< flip algN succ'