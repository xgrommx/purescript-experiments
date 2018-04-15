module Expr2 where

import Prelude

import Control.Apply (lift2)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, logShow)
import Control.Monad.Reader (ReaderT(..), ask, lift, runReaderT)
import Data.Either (Either(..))
import Data.Foldable (class Foldable, foldMap, foldlDefault, foldr, foldrDefault)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Map (Map, fromFoldable, lookup, singleton)
import Data.Maybe (Maybe(..))
import Data.Monoid (mempty, (<>))
import Data.Newtype (traverse)
import Data.Traversable (class Traversable, sequence, sequenceDefault)
import Data.Tuple (Tuple(..))
import Matryoshka (class Corecursive, class Recursive, cata, cataM, embed, project)
import Partial.Unsafe (unsafeCrashWith)

-- https://jtobin.io/monadic-recursion-schemes

data Expr = Var String | Lit Int | Add Expr Expr

data ExprF r = VarF String | LitF Int | AddF r r

data Error = FreeVar String

derive instance genericExpr :: Generic Expr _

derive instance genericError :: Generic Error _

derive instance functorExprF :: Functor ExprF

instance foldableExprF :: Foldable ExprF where
  foldr f = foldrDefault f
  foldl f = foldlDefault f
  foldMap f = case _ of
    VarF _ -> mempty
    LitF _ -> mempty
    AddF x y -> f x <> f y

instance traversableExprF :: Traversable ExprF where
  sequence f = sequenceDefault f
  traverse f = case _ of
    VarF x -> pure (VarF x)
    LitF x -> pure (LitF x)
    AddF x y -> lift2 AddF (f x) (f y)

instance showExpr :: Show Expr where
  show x = genericShow x

instance showError :: Show Error where
  show x = genericShow x

instance recursiveExpr :: Recursive Expr ExprF where
  project = case _ of
    Var x -> VarF x
    Lit x -> LitF x
    Add x y -> AddF x y

instance corecursiveExpr :: Corecursive Expr ExprF where
  embed = case _ of
    VarF x -> Var x
    LitF x -> Lit x
    AddF x y -> Add x y

-- monadicAlgebra :: ExprF Int -> Either Error Int
-- monadicAlgebra = case _ of
--   LitF j   -> pure j
--   AddF i j -> pure (i + j)
--   VarF v   -> Left (FreeVar v)

-- eval :: Expr -> Either Error Int
-- eval = cataM $ case _ of
--   LitF j   -> pure j
--   AddF i j -> pure (i + j)
--   VarF v   -> Left (FreeVar v)

eval' :: Expr -> Int
eval' = cata $ case _ of
  LitF j   -> j
  AddF i j -> i + j
  VarF _   -> unsafeCrashWith "free variable in expression"

eval :: Expr -> ReaderT (Map String Int) (Either Error) Int
eval = cataM $ case _ of
  LitF j   -> pure j
  AddF i j -> pure (i + j)
  VarF v   -> do
    env <- ask
    case lookup v env of
      Nothing -> lift (Left (FreeVar v))
      Just j  -> pure j

example :: forall e. Eff (console :: CONSOLE | e) Unit
example = do
  let open = Add (Var "x") (Var "y")
  logShow $ eval' open
  logShow $ runReaderT (eval open) (singleton "x" 1)
  logShow $ runReaderT (eval open) (fromFoldable [Tuple "x" 1, Tuple "y" 5])