module Main where

import Expr

import Control.Lazy as Z
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, logShow)
import Control.Monad.Writer (runWriter)
import Data.Array ((..))
import Data.Foldable (traverse_)
import Data.Functor.Mu (Mu)
import Data.Functor.Nu (Nu)
import Data.Int (odd)
import Data.List as L
import Data.List.Lazy as LL
import Data.Newtype (unwrap)
import Data.Semiring ((*))
import Data.Tuple (snd)
import Fix (refix)
import List (List(..), evenIndices, evenIndicesF, fromFoldable, h, h2, init, init', inits, oddIndices, oddIndicesF, partitionByCond, reverse, smallLen, smallStream, smallSum, take, twiddle, uniq)
import ListC (ListC, cons, lengthC, nil, sumC)
import ListL (List'(..))
import Matryoshka (cata, cataM)
import NaturalC (NaturalC, oneC, threeC)
import Peano3 (Peano(..))
import Prelude (class Functor, type (~>), Unit, discard, flip, map, pure, unit, void, ($), (+), (<), (<$>), (<<<), (<=), (>))
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

iterate' :: forall a. (a -> a) -> a -> LL.List a
iterate' f x = x LL.: (Z.defer \_ -> iterate' f (f x))

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
  logShow $ take 2 $ fromFoldable (1..10)
  logShow $ reverse $ fromFoldable (1..10)
  logShow $ inits $ fromFoldable (1..10)
  logShow $ init $ fromFoldable (1..10)  
  logShow $ init' $ fromFoldable (1..10)
  logShow $ uniq $ fromFoldable [1,2,3,1,2,3]
  logShow $ partitionByCond (\x y -> x <= y) $ fromFoldable [1,2,3,1,2,3,4,3,5]
  logShow $ h (_<10) (_>4) odd $ fromFoldable (1..10)
  logShow $ oddIndices $ fromFoldable (1..10)
  logShow $ oddIndicesF $ fromFoldable (1..10)
  logShow $ evenIndices $ fromFoldable (1..10)
  logShow $ evenIndicesF $ fromFoldable (1..10)
  logShow $ twiddle $ fromFoldable (1..100)
  logShow $ smallSum $ fromFoldable (1..100)
  logShow $ smallLen $ fromFoldable (1..100)
  -- logShow $ smallStream 3
  logShow $ diff smallExpression
  traverse_ (logShow <<< flip eval (diff smallExpression)) [0.0009, 1.0, 1.0001]
  traverse_ (logShow <<< flip eval (diff bigExpression)) [0.0009, 1.0, 1.0001]
  logShow $ map (snd <<< (_ `ad` smallExpression)) [0.0009, 1.0, 1.0001]
  logShow $ map (snd <<< (_ `ad` bigExpression)) [0.0009, 1.0, 1.0001]
