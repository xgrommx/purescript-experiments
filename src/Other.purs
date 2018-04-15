module Other where

import Data.Array ((..))
import ListF (ListF(..))
import Prelude (class Functor, class Semiring, Unit, discard, map, (*), (+), (-))

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, logShow)
import Matryoshka (hylo)

coalgL :: Int -> ListF Int Int
coalgL 0 = NilF
coalgL n = ConsF n (n - 1)

algL :: ListF Int Int -> Int
algL NilF = 1
algL (ConsF a b) = a * b

data PreTree a x = L a | N x x

derive instance functorPreTree :: Functor (PreTree a)

alg' :: forall a. Semiring a => PreTree a a -> a
alg' (L a) = a
alg' (N a b) = a + b

coalg' :: Int -> PreTree Int Int
coalg' 0 = L 1
coalg' 1 = L 1
coalg' n = N (n-1) (n-2)

run :: forall e. Eff (console :: CONSOLE | e) Unit
run = do
  let facL = map (hylo algL coalgL) (0..10)
  let fibT = map (hylo alg' coalg') (0..10)

  logShow facL
  logShow fibT