module HRecursionSchemes where

import Prelude

import Conditional ((?))
import Control.Apply (lift2, lift3)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, logShow)
import Control.Monad.Writer (Writer, WriterT(..), execWriter, lift, runWriter, tell)
import Data.Const (Const(..))
import Data.Either (Either(..))
import Data.Exists (Exists)
import Data.Functor.Product (Product(..))
import Data.Identity (Identity(..))
import Data.Leibniz (type (~), coerce, coerceSymm, liftLeibniz, liftLeibniz1of2, lowerLeibniz, symm)
import Data.Monoid (class Monoid, mempty)
import Data.Monoid.Endo (Endo(..))
import Data.Newtype (unwrap)
import Data.Traversable (class Traversable)
import Data.Tuple (fst)
import Matryoshka (cataM)
import Partial.Unsafe (unsafePartial)
import Prettier.Printer (DOC, pretty, text, (<+>))
import Unsafe.Coerce (unsafeCoerce)

type NatM m f g = forall a. f a -> m (g a)

type HAlgebra h f = h f ~> f
type HAlgebraM m h f = NatM m (h f) f 
type HCoalgebra h f = f ~> h f

class HFunctor (h :: (Type -> Type) -> (Type -> Type)) where
  hfmap :: forall f g. (f ~> g) -> (h f ~> h g)

class HFunctor h <= HFoldable (h :: (Type -> Type) -> (Type -> Type)) where
  hfoldMap :: forall m f a. Monoid m => (forall b. f b -> m) -> h f a -> m

class HFoldable h <= HTraversable (h :: (Type -> Type) -> (Type -> Type))  where
  htraverse :: forall e f g. Applicative e => NatM e f g -> NatM e (h f) (h g)
  -- htraverse :: forall f a b. Applicative f => (forall i. a i -> f (b i)) -> (forall i. h a i -> f (h b i))

class HFunctor f <= HRecursive t f | t -> f where
  hproject ∷ t ~> f t

class HFunctor f <= Corecursive t f | t -> f where
  hembed ∷ f t ~> t

hcata :: forall h f t. HFunctor h => HRecursive t h => HAlgebra h f -> t ~> f
hcata algebra = algebra <<< hfmap (hcata algebra) <<< hproject

hcataM :: forall f t m a. HTraversable f => Monad m => HRecursive t f => HAlgebraM m f a -> NatM m t a
hcataM f = f <=< htraverse (hcataM f) <<< hproject

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
  | If (Expr Boolean) (Expr a) (Expr a) (Either (a ~ Int) (a ~ Boolean))

data ExprF h a
  = AddF (h Int) (h Int) (a ~ Int)
  | MultF (h Int) (h Int) (a ~ Int)
  | EqualF (h Int) (h Int) (a ~ Boolean)
  | NotF (h Boolean) (a ~ Boolean)
  | ValF Int (a ~ Int)
  | BoolF Boolean (a ~ Boolean)
  | LessThanF (h Int) (h Int) (a ~ Boolean)
  | IfF (h Boolean) (h a) (h a) (Either (a ~ Int) (a ~ Boolean))

instance exprShow :: Show a => Show (Expr a) where
  show = pretty 1 <<< unwrap <<< hcata evalDocAlgebra

instance hfunctorExpr :: HFunctor ExprF where
  hfmap f = case _ of
    AddF x y p -> AddF (f x) (f y) p
    MultF x y p -> MultF (f x) (f y) p
    EqualF x y p -> EqualF (f x) (f y) p
    ValF x p -> ValF x p
    BoolF x p -> BoolF x p
    NotF x p -> NotF (f x) p
    LessThanF x y p -> LessThanF (f x) (f y) p
    IfF c x y p -> IfF (f c) (f x) (f y) p

instance hfoldableExpr :: HFoldable ExprF where
  hfoldMap f = case _ of
    AddF x y p -> (f x) <> (f y)
    MultF x y p -> (f x) <> (f y)
    EqualF x y p -> (f x) <> (f y)
    ValF x p -> mempty
    BoolF x p -> mempty
    NotF x p -> f x
    LessThanF x y p -> (f x) <> (f y)
    IfF c x y p -> (f c) <> (f x) <> (f y)

instance htraversableExpr :: HTraversable ExprF where
  htraverse f = case _ of
    AddF x y p -> lift2 (\x' y' -> AddF x' y' p) (f x) (f y)
    MultF x y p -> lift2 (\x' y' -> MultF x' y' p) (f x) (f y)
    EqualF x y p -> lift2 (\x' y' -> EqualF x' y' p) (f x) (f y)
    ValF x p -> pure (ValF x p)
    BoolF x p -> pure (BoolF x p)
    NotF x p -> (\x' -> NotF x' p) <$> f x
    LessThanF x y p -> lift2 (\x' y' -> LessThanF x' y' p) (f x) (f y)
    IfF c x y p -> lift3 (\x' y' z' -> IfF x' y' z' p) (f c) (f x) (f y)

instance hrecursiveExpr :: HRecursive Expr ExprF where
  hproject = case _ of
    Add x y p -> AddF x y p
    Mult x y p -> MultF x y p
    Equal x y p -> EqualF x y p
    Val x p -> ValF x p
    Bool x p -> BoolF x p
    Not x p -> NotF x p
    LessThan x y p -> LessThanF x y p
    If c x y p -> IfF c x y p

eval :: forall a. Expr a -> a
eval (Val x p) = coerceSymm p x
eval (Bool x p) = coerceSymm p x
eval (Add x y p) = coerceSymm p (eval x + eval y)
eval (Mult x y p) = coerceSymm p (eval x * eval y)
eval (Equal x y p) = coerceSymm p (eval x == eval y)
eval (Not x p) = coerceSymm p (not (eval x))
eval (LessThan x y p) = coerceSymm p (eval x < eval y)
eval (If c x y p) = case p of
  Left p' -> (eval c) ? (eval x) $ (eval y)
  Right p' -> (eval c) ? (eval x) $ (eval y)

evalAlgebra :: ExprF Identity ~> Identity
evalAlgebra = case _ of
  ValF x p -> Identity $ coerceSymm p x
  BoolF x p -> Identity $ coerceSymm p x
  MultF x y p -> Identity $ coerceSymm p $ unwrap x * unwrap y
  AddF x y p -> Identity $ coerceSymm p $ unwrap x + unwrap y
  EqualF x y p -> Identity $ coerceSymm p $ unwrap x == unwrap y
  NotF x p -> Identity $ coerceSymm p $ not $ unwrap x
  LessThanF x y p -> Identity $ coerceSymm p $ unwrap x < unwrap y
  IfF c x y p -> Identity $ (unwrap c) ? (unwrap x) $ (unwrap y)

evalAlgebraM'' :: forall a. ExprF Identity a -> Writer (Array (Array String)) (Identity a)
evalAlgebraM'' = case _ of
  ValF x p -> do
    tell $ [[show x]]
    pure $ Identity $ coerceSymm p x
  BoolF x p -> do
    tell $ [[show x]]
    pure $ Identity $ coerceSymm p x
  MultF x y p -> do
    tell $ [[show x <> " * " <> show y]]
    pure $ Identity $ coerceSymm p $ unwrap x * unwrap y
  AddF x y p -> do
    tell $ [[show (unwrap x) <> " + " <> show (unwrap y)]]
    pure $ Identity $ coerceSymm p $ unwrap x + unwrap y
  EqualF x y p -> do
    tell $ [[show x <> " == " <> show y]]
    pure $ Identity $ coerceSymm p $ unwrap x == unwrap y
  NotF x p -> do
    tell $ [["not " <> show x]]
    pure $ Identity $ coerceSymm p $ not $ unwrap x
  LessThanF x y p -> do
    tell $ [[show x <> " < " <> show y]]
    pure $ Identity $ coerceSymm p $ unwrap x < unwrap y
  IfF (Identity c) (Identity x) (Identity y) p -> do
    let build x' y' = tell $ [["if " <> show c <> " then " <> x' <> " else " <> y']]
    case p of
      Left p' -> build (show $ coerce p' x) (show $ coerce p' y)
      Right p' -> build (show $ coerce p' x) (show $ coerce p' y)
    pure $ Identity $ c ? x $ y

evalAlgebraM :: forall a. ExprF Identity a -> Writer DOC (Identity a)
evalAlgebraM = case _ of
  ValF x p -> do
    tell $ text $ show x
    pure $ Identity $ coerceSymm p x
  BoolF x p -> do
    tell $ text $ show x
    pure $ Identity $ coerceSymm p x
  MultF x y p -> do
    tell $ parens $ (text $ show $ unwrap x) <+> text "+" <+> (text $ show $ unwrap y)
    pure $ Identity $ coerceSymm p $ unwrap x * unwrap y
  AddF x y p -> do
    tell $ parens $ (text $ show $ unwrap x) <+> text "*" <+> (text $ show $ unwrap y)
    pure $ Identity $ coerceSymm p $ unwrap x + unwrap y
  EqualF x y p -> do
    tell $ parens $ (text $ show $ unwrap x) <+> text "==" <+> (text $ show $ unwrap y)
    pure $ Identity $ coerceSymm p $ unwrap x == unwrap y
  NotF x p -> do
    tell $ text "not" <+> (text $ show $ unwrap x)
    pure $ Identity $ coerceSymm p $ not $ unwrap x
  LessThanF x y p -> do
    tell $ parens $ (text $ show $ unwrap x) <+> text "<" <+> (text $ show $ unwrap y)
    pure $ Identity $ coerceSymm p $ unwrap x < unwrap y
  IfF (Identity c) (Identity x) (Identity y) p -> do
    let build x' y' = tell $ text "if" <+> (text $ show c) <+> text "then" <+> x' <+> text "else" <+> y'
    case p of
      Left p' -> build (text $ show $ coerce p' x) (text $ show $ coerce p' y)
      Right p' -> build (text $ show $ coerce p' x) (text $ show $ coerce p' y)
    pure $ Identity $ c ? x $ y

evalValueAlgebra :: Partial => ExprF Value ~> Value
evalValueAlgebra = case _ of
  ValF x p -> VInt x p
  BoolF x p -> VBool x p
  MultF (VInt x _) (VInt y _) p -> VInt (x * y) p
  AddF (VInt x _) (VInt y _) p -> VInt (x + y) p
  EqualF (VInt x _) (VInt y _) p -> VBool (x == y) p
  NotF (VBool x _) p -> VBool (not x) p
  LessThanF (VInt x _) (VInt y _) p -> VBool (x < y) p
  IfF (VBool c _) x y p -> c ? x $ y

evalDocAlgebra :: ExprF (Const DOC) ~> Const DOC
evalDocAlgebra = case _ of
  ValF x _ -> Const <<< text $ show x
  BoolF x _ -> Const <<< text $ show x
  AddF (Const x) (Const y) _ -> Const <<< parens $ x <+> text "+" <+> y
  MultF (Const x) (Const y) _ -> Const <<< parens $ x <+> text "*" <+> y
  EqualF (Const x) (Const y) _ -> Const <<< parens $ x <+> text "==" <+> y
  NotF (Const x) _ -> Const $ text "not" <+> x
  LessThanF (Const x) (Const y) _ -> Const <<< parens $ x <+> text "<" <+> y
  IfF (Const c) (Const x) (Const y) _ -> Const $ text "if" <+> c <+> text "then" <+> x <+> text "else" <+> y

-- evalAlgebraM' :: forall a. ExprF Identity (Writer DOC a) -> Writer DOC (Identity a)
-- evalAlgebraM' = case _ of
--   ValF x p -> do
--     x' <- x
--     tell $ text "1"
--     pure $ Identity $ coerce (liftLeibniz p) x'

value :: Expr Boolean
value = Not (Equal (Mult (Val 10 id) (Val 1 id) id) (Add (Val 0 id) (Val 1 id) id) id) id

value2 :: Expr Int
value2 = If (Bool false id) (Val 1 id) (Add (Val 42 id) (Val 45 id) id) (Left id)

example :: forall e. Eff (console :: CONSOLE | e) Unit
example = do
  logShow $ unwrap $ hcata evalAlgebra value
  logShow $ unwrap $ hcata evalAlgebra value2
  logShow $ hcata (unsafePartial evalValueAlgebra) value
  logShow $ hcata (unsafePartial evalValueAlgebra) value2
  logShow $ pretty 1 $ unwrap $ hcata evalDocAlgebra value
  logShow $ pretty 1 $ unwrap $ hcata evalDocAlgebra value2
  logShow $ pretty 1 $ execWriter $ hcataM evalAlgebraM value2
  logShow $ execWriter $ hcataM evalAlgebraM'' value2