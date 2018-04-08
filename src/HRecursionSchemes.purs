module HRecursionSchemes where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, logShow)
import Data.Const (Const(..))
import Data.Identity (Identity(..))
import Data.Newtype (unwrap)
import Partial.Unsafe (unsafePartial)
import Prettier.Printer (DOC, pretty, text, (<+>))
import Data.Leibniz (type (~), symm, coerce, coerceSymm)

type HAlgebra h f = h f ~> f
type HCoalgebra h f = f ~> h f

class HFunctor (h :: (Type -> Type) -> (Type -> Type)) where
  hfmap :: forall f g. (f ~> g) -> (h f ~> h g)

class HFunctor f <= HRecursive t f | t -> f where
  hproject ∷ t ~> f t

class HFunctor f <= Corecursive t f | t -> f where
  hembed ∷ f t ~> t

hcata :: forall h f t. HFunctor h => HRecursive t h => HAlgebra h f -> t ~> f
hcata algebra = algebra <<< hfmap (hcata algebra) <<< hproject

hhylo :: forall f a b. HFunctor f => HAlgebra f b -> HCoalgebra f a -> a ~> b
hhylo f g = f <<< hfmap (hhylo f g) <<< g

parens :: DOC -> DOC
parens p = text "(" <> p <> text ")"

data Value a
  = VInt Int (a ~ Int)
  | VBool Boolean (a ~ Boolean)

instance showValue :: Show a => Show (Value a) where
  show = case _ of
    VInt a p -> "VInt " <> show (coerceSymm p a)
    VBool a p -> "VBool " <> show (coerceSymm p a)

data Expr a
  = Add (Expr Int) (Expr Int) (a ~ Int)
  | Mult (Expr Int) (Expr Int) (a ~ Int)
  | Equal (Expr Int) (Expr Int) (a ~ Boolean)
  | Not (Expr Boolean) (a ~ Boolean)
  | Val Int (a ~ Int)
  | Bool Boolean (a ~ Boolean)
  | LessThan (Expr Int) (Expr Int) (a ~ Boolean)
  | If (Expr Boolean) (Expr a) (Expr a)


data ExprF h a
  = AddF (h Int) (h Int) (a ~ Int)
  | MultF (h Int) (h Int) (a ~ Int)
  | EqualF (h Int) (h Int) (a ~ Boolean)
  | NotF (h Boolean) (a ~ Boolean)
  | ValF Int (a ~ Int)
  | BoolF Boolean (a ~ Boolean)
  | LessThanF (h Int) (h Int) (a ~ Boolean)
  | IfF (h Boolean) (h a) (h a)

instance hfunctorExpr :: HFunctor ExprF where
  hfmap f = case _ of
    AddF x y p -> AddF (f x) (f y) p
    MultF x y p -> MultF (f x) (f y) p
    EqualF x y p -> EqualF (f x) (f y) p
    ValF x p -> ValF x p
    BoolF x p -> BoolF x p
    NotF x p -> NotF (f x) p
    LessThanF x y p -> LessThanF (f x) (f y) p
    IfF c x y -> IfF (f c) (f x) (f y)

instance hrecursiveExpr :: HRecursive Expr ExprF where
  hproject = case _ of
    Add x y p -> AddF x y p
    Mult x y p -> MultF x y p
    Equal x y p -> EqualF x y p
    Val x p -> ValF x p
    Bool x p -> BoolF x p
    Not x p -> NotF x p
    LessThan x y p -> LessThanF x y p
    If c x y -> IfF c x y

eval :: forall a. Expr a -> a
eval (Val x p) = coerceSymm p x
eval (Bool x p) = coerceSymm p x
eval (Add x y p) = coerceSymm p (eval x + eval y)
eval (Mult x y p) = coerceSymm p (eval x * eval y)
eval (Equal x y p) = coerceSymm p (eval x == eval y)
eval (Not x p) = coerceSymm p (not (eval x))
eval (LessThan x y p) = coerceSymm p (eval x < eval y)
eval (If c x y) = if (eval c) then (eval x) else (eval y)

evalAlgebra :: ExprF Identity ~> Identity
evalAlgebra = case _ of
  ValF x p -> Identity $ coerceSymm p x
  BoolF x p -> Identity $ coerceSymm p x
  MultF x y p -> Identity $ coerceSymm p $ unwrap x * unwrap y
  AddF x y p -> Identity $ coerceSymm p $ unwrap x + unwrap y
  EqualF x y p -> Identity $ coerceSymm p $ unwrap x == unwrap y
  NotF x p -> Identity $ coerceSymm p $ not $ unwrap x
  LessThanF x y p -> Identity $ coerceSymm p $ unwrap x < unwrap y
  IfF c x y -> Identity $ if (unwrap c) then (unwrap x) else (unwrap y)

evalValueAlgebra :: Partial => ExprF Value ~> Value
evalValueAlgebra = case _ of
  ValF x p -> VInt x p
  BoolF x p -> VBool x p
  MultF (VInt x _) (VInt y _) p -> VInt (x * y) p
  AddF (VInt x _) (VInt y _) p -> VInt (x + y) p
  EqualF (VInt x _) (VInt y _) p -> VBool (x == y) p
  NotF (VBool x _) p -> VBool (not x) p
  LessThanF (VInt x _) (VInt y _) p -> VBool (x < y) p
  IfF (VBool c _) x y -> if c then x else y

evalDocAlgebra :: ExprF (Const DOC) ~> Const DOC
evalDocAlgebra = case _ of
  ValF x _ -> Const <<< text $ show x
  BoolF x _ -> Const <<< text $ show x
  AddF (Const x) (Const y) _ -> Const <<< parens $ x <+> text "+" <+> y
  MultF (Const x) (Const y) _ -> Const <<< parens $ x <+> text "*" <+> y
  EqualF (Const x) (Const y) _ -> Const <<< parens $ x <+> text "==" <+> y
  NotF (Const x) _ -> Const $ text "not" <+> x
  LessThanF (Const x) (Const y) _ -> Const <<< parens $ x <+> text "<" <+> y
  IfF (Const c) (Const x) (Const y) -> Const $ text "if" <+> c <+> text "then" <+> x <+> text "else" <+> y

value :: Expr Boolean
value = Not (Equal (Mult (Val 10 id) (Val 1 id) id) (Add (Val 0 id) (Val 1 id) id) id) id

value2 :: Expr Int
value2 = If (Bool false id) (Val 1 id) (Add (Val 42 id) (Val 45 id) id)

example :: forall e. Eff (console :: CONSOLE | e) Unit
example = do
  logShow $ unwrap $ hcata evalAlgebra value
  logShow $ unwrap $ hcata evalAlgebra value2
  logShow $ hcata (unsafePartial evalValueAlgebra) value
  logShow $ hcata (unsafePartial evalValueAlgebra) value2
  logShow $ pretty 1 $ unwrap $ hcata evalDocAlgebra value
  logShow $ pretty 1 $ unwrap $ hcata evalDocAlgebra value2