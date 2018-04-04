module Main where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, logShow)
import Control.Monad.Writer (runWriter)
import Data.Array ((..))
import Data.Functor.Mu (Mu)
import Data.Functor.Nu (Nu)
import Data.List as L
import Data.Newtype (unwrap)
import Data.Semiring ((*))
import Fix (refix)
import List (List(..))
import ListC (ListC, cons, lengthC, nil, sumC)
import ListL (List'(..))
import Matryoshka (cata, cataM)
import NaturalC (NaturalC, oneC, threeC)
import Peano3 (Peano(..))
import Prelude (class Functor, Unit, discard, ($), (+), (<$>))
import RoseTree (alg3, evalM, tree)
import Tree (Tree)
import TreeC (TreeC, emptyC, nodeC)

---------------------------------------------------------------------------

fromNuToMu :: forall f. Functor f => Nu f -> Mu f
fromNuToMu = refix

fromMuToNu :: forall f. Functor f => Mu f -> Nu f
fromMuToNu = refix

---------------------------------------------------------------------------

---------------------------------------------------------------------------

fromNaturalC :: NaturalC -> Peano
fromNaturalC = refix

toNaturalC :: Peano -> NaturalC
toNaturalC = refix

---------------------------------------------------------------------------

---------------------------------------------------------------------------

fromList' :: forall a. List' a -> ListC a
fromList' = refix

fromList :: forall a. List a -> ListC a
fromList = refix

toList :: forall a. ListC a -> List a
toList = refix

---------------------------------------------------------------------------

---------------------------------------------------------------------------

fromTree :: forall a. Tree a -> TreeC a
fromTree = refix

toTree :: forall a. TreeC a -> Tree a
toTree = refix

---------------------------------------------------------------------------

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  logShow $ lengthC (cons 10 nil)
  logShow $ cons 10 (cons 20 nil)
  logShow $ lengthC $ fromList $ Cons 10 (Cons 20 Nil)
  logShow $ sumC $ fromList' $ List' $ L.fromFoldable (1..10)
  logShow $ oneC + oneC * threeC
  logShow $ Succ (Succ Zero)
  logShow $ fromNaturalC $ oneC + oneC * threeC
  logShow $ toNaturalC $ Succ (Succ Zero)
  logShow $ runWriter $ cataM evalM tree
  logShow $ unwrap (cata alg3 tree) []
  logShow $ (_*10) <$> cons 10 nil
  logShow $ (_*10) <$> nodeC (nodeC emptyC 30 emptyC) 10 emptyC
