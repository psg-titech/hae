{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE AutoDeriveTypeable #-}

module Hae.Internal.Expr where

import Data.Typeable
import Hae.Internal.Util.Ref
import Hae.Internal.Util.TypedList
import Hae.Types

newtype OE f a =
  OE (Ref (OpenExpr f a))

refE :: OpenExpr f a -> OE f a
refE = OE . ref

derefE :: OE f a -> OpenExpr f a
derefE (OE r) = deref r

data TimingDef
  = EveryMs Int
  | EveryS Int
  | EveryMin Int
  | AsyncInp
  deriving (Show)

data OpenExpr (f :: * -> *) t where
  Inp :: HType t => String -> TimingDef -> OpenExpr f t -- named dummy input node
  Lit :: HType t => t -> OpenExpr f t
  Pair :: (Typeable a, Typeable b) => OE f a -> OE f b -> OpenExpr f (a, b)
  PrimOp
    :: (Typeable a, Typeable b) => PrimOpId -> (a -> b) -> OpenExpr f (a -> b)
  IfThenElse :: (Typeable t) => OE f Bool -> OE f t -> OE f t -> OpenExpr f t
  Var :: f t -> OpenExpr f t
  Lam :: (Typeable a, Typeable b) => (f a -> OE f b) -> OpenExpr f (a -> b)
  App :: (Typeable a, Typeable b) => OE f (a -> b) -> OE f a -> OpenExpr f b
  LetRec
    :: (CList ts, Typeable t)
    => (TList f ts -> TList (OE f) ts)
    -> (TList f ts -> OE f t)
    -> OpenExpr f t
  SampleOnChange :: Typeable a => OE f a -> OE f b -> OpenExpr f a
  PrevVal :: Typeable t => OE f t -> OpenExpr f t
  Foldp :: (Typeable a, Typeable b) => OE f (a -> b) -> OpenExpr f (a -> b)

fst_ :: (HType a, HType b) => OE f (a, b) -> OE f a
fst_ = primitiveFunction "fst" fst

snd_ :: (HType a, HType b) => OE f (a, b) -> OE f b
snd_ = primitiveFunction "snd" snd

-- |Helper typeclass to convert between (Expr, Expr) and Expr :* Expr automatically
class HType (Internal a) =>
      Computable f a where
  type Internal a
  internalize :: a -> OE f (Internal a)
  externalize :: OE f (Internal a) -> a

instance Computable f () where
  type Internal () = ()
  internalize () = lit_ ()
  externalize e = ()

instance HType a => Computable f (OE f a) where
  type Internal (OE f a) = a
  internalize = id
  externalize = id

instance (Computable f a, Computable f b) => Computable f (a, b) where
  type Internal (a, b) = (Internal a, Internal b)
  internalize (a, b) = refE $ Pair (internalize a) (internalize b)
  externalize t = (externalize (fst_ t), externalize (snd_ t))

lit_ :: HType t => t -> OE f t
lit_ = refE . Lit

-- |Helper functions to define primitive functions on one argument
primitiveFunction ::
     (HType a, HType b) => String -> (a -> b) -> (OE f a -> OE f b)
primitiveFunction fid f arg =
  case derefE arg of
    Lit x -> lit_ (f x) -- Compute primitive function on pure value at compile time
    _ -> refE $ App (refE (PrimOp (PrimOpId1 fid) f)) arg

-- |Helper functions to define primitive functions on two arguments
primitiveFunction2 ::
     (HType a, HType b, HType c)
  => String
  -> (a -> b -> c)
  -> (OE f a -> OE f b -> OE f c)
primitiveFunction2 fid f arg1 arg2 =
  case (derefE arg1, derefE arg2) of
    (Lit x, Lit y) -> lit_ $ f x y -- Compute primitive function on pure value at compile time
    _ -> refE $ App (refE (PrimOp (PrimOpId2 fid) f')) (refE (Pair arg1 arg2))
      where f' (a, b) = f a b

letrec_ ::
     (CList ts, Typeable t)
  => (TList (OE f) ts -> TList (OE f) ts)
  -> (TList (OE f) ts -> OE f t)
  -> OE f t
letrec_ es e = refE $ LetRec (es . tmap (refE . Var)) (e . tmap (refE . Var))

let_ :: Typeable b => OE f a -> (OE f a -> OE f b) -> OE f b
let_ e1 e2 = letrec_ (\_ -> e1 ::: TNil) (\(x ::: TNil) -> e2 x)

mu_ :: (Typeable a, Typeable b) => (OE f (a -> b) -> OE f (a -> b)) -> OE f (a -> b)
mu_ f =
  letrec__ (\(recf ::: TNil) -> f recf ::: TNil) (\(recf ::: TNil) -> recf)
  where
    letrec__ :: (Typeable a, Typeable b) =>
         (TList (OE f) (a -> b, ()) -> TList (OE f) (a -> b, ()))
      -> (TList (OE f) (a -> b, ()) -> OE f (a -> b))
      -> OE f (a -> b)
    letrec__ = letrec_
