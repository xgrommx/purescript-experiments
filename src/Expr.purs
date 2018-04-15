module Expr where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.List.Lazy (iterate, (!!))
import Data.Maybe (fromMaybe)
import Data.Tuple (Tuple(..))
import Math (exp)
import Matryoshka (class Corecursive, class Recursive, cata, para)

-- https://jtobin.io/ad-via-recursion-schemes

data Expr =
    Var
  | Zero
  | One
  | Negate Expr
  | Sum Expr Expr
  | Product Expr Expr
  | Exp Expr  

data ExprF r = 
    VarF
  | ZeroF
  | OneF
  | NegateF r
  | SumF r r
  | ProductF r r
  | ExpF r

derive instance genericExpr :: Generic Expr _

derive instance functorExprF :: Functor ExprF

instance showExpr :: Show Expr where
  show x = genericShow x

instance recursiveExpr :: Recursive Expr ExprF where
  project = case _ of
    Var -> VarF
    Zero -> ZeroF
    One -> OneF
    Negate a -> NegateF a
    Sum a b -> SumF a b
    Product a b -> ProductF a b
    Exp e -> ExpF e

instance corecursiveExpr :: Corecursive Expr ExprF where
  embed = case _ of
    VarF -> Var
    ZeroF -> Zero
    OneF -> One
    NegateF a -> Negate a
    SumF a b -> Sum a b
    ProductF a b -> Product a b
    ExpF e -> Exp e    

f :: Expr -> Expr
f x = Exp (x `minus` One)
  where minus a b = a `Sum` Negate b

smallExpression :: Expr
smallExpression = fromMaybe Var $ iterate f Var !! 3

bigExpression :: Expr
bigExpression = fromMaybe Var $ iterate f Var !! 1000

eval :: Number -> Expr -> Number
eval x = cata $ case _ of
  VarF         -> x
  ZeroF        -> 0.0
  OneF         -> 1.1
  NegateF a    -> negate a
  SumF a b     -> a + b
  ProductF a b -> a * b
  ExpF a       -> exp a

diff :: Expr -> Expr
diff = para $ case _ of
  VarF         -> One
  ZeroF                    -> Zero
  OneF                     -> Zero
  NegateF (Tuple _ x')          -> Negate x'
  SumF (Tuple _ x') (Tuple _ y')     -> Sum x' y'
  ProductF (Tuple x x') (Tuple y y') -> Sum (Product x y') (Product x' y)
  ExpF (Tuple x x')             -> Product (Exp x) x'

ad :: Number -> Expr -> Tuple Number Number
ad x = cata $ case _ of
  VarF                     -> Tuple x 1.0
  ZeroF                    -> Tuple 0.0 0.0
  OneF                     -> Tuple 1.0 0.0
  NegateF (Tuple x x')          -> Tuple (negate x) (negate x')
  SumF (Tuple x x') (Tuple y y')     -> Tuple (x + y) (x' + y')
  ProductF (Tuple x x') (Tuple y y') -> Tuple (x * y) (x * y' + x' * y)
  ExpF (Tuple x x')             -> Tuple (exp x) (exp $ x * x')    