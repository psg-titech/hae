{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hae.Expr
  ( OE
  , E
  , (.*)
  , Expr
  , TList(TNil, (:::))
  , TimingDef(..)
  , input
  , foldp
  , lam
  , switch
  , first
  , second
  , app
  , (|$|)
  , mu
  , let'
  , letrec
  , letrec_
  , prev
  , sampleOnChange
  , fps
  ) where

import Data.Typeable
import Hae.Internal.Expr
import Hae.Internal.Util.TypedList
import Hae.Types

infix 1 .*

a .* b = refE $ Pair a b

type E t = forall f. OE f t

type Expr t = forall f. OpenExpr f t

input :: HType t => String -> TimingDef -> E t
input s m = case s of -- TODO: more restrictions
  '_':_ -> error "Invalid input name"
  _ -> refE $ Inp s m

fps :: TimingDef -> E Int -- TODO: use E () instead
fps tm = refE $ Inp "_fps" tm


switch :: Typeable t => OE f Bool -> OE f t -> OE f t -> OE f t
switch p t f = refE $ IfThenElse p t f

foldp ::
     forall a b f. (HType a, HType b)
  => (OE f b -> OE f a -> OE f b)
  -> OE f b
  -> OE f a
  -> OE f b
foldp step zero arg =
  refE $ App (refE (Foldp (lam step'))) (refE (Pair zero arg))
  where
    step' :: OE f (b, a) -> OE f b
    step' x = step (fst_ x) (snd_ x)

lam :: (Typeable a, Typeable b) => (OE f a -> OE f b) -> OE f (a -> b)
lam e = refE $ Lam (e . refE . Var)

app :: (Typeable a, Typeable b) => OE f (a -> b) -> OE f a -> OE f b
app f arg = refE (App f arg)

(|$|) :: (Typeable a, Typeable b) => OE f (a -> b) -> OE f a -> OE f b
(|$|) = app

prev :: Typeable a => OE f a -> OE f a
prev = refE . PrevVal

first :: (HType a, HType b) => OE f (a, b) -> OE f a
first = fst_

second :: (HType a, HType b) => OE f (a, b) -> OE f b
second = snd_

mu ::
     (Typeable a, Typeable b)
  => (OE f (a -> b) -> OE f (a -> b))
  -> OE f (a -> b)
mu = mu_

let' :: Typeable b => OE f a -> (OE f a -> OE f b) -> OE f b
let' = let_

letrec ::
     (CList ts, Typeable t)
  => (TList (OE f) ts -> TList (OE f) ts)
  -> (TList (OE f) ts -> OE f t)
  -> OE f t
letrec = letrec_

sampleOnChange :: Typeable a => OE f a -> OE f b -> OE f a
sampleOnChange target sam = refE $ SampleOnChange target sam
