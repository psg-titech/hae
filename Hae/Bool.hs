module Hae.Bool where

import Hae.Internal.Expr (refE, OE, OpenExpr(Lit), primitiveFunction, primitiveFunction2)
import Hae.Types (HType)

infix 4 .==

infix 4 ./=

infix 4 .<

infix 4 .>

infix 4 .<=

infix 4 .>=

-- infix 1 .?

true = refE $ Lit True

false = refE $ Lit False

(.==) :: (HType a, Eq a) => OE f a -> OE f a -> OE f Bool
(.==) = primitiveFunction2 "(==)" (Prelude.==)

(./=) :: (HType a, Eq a) => OE f a -> OE f a -> OE f Bool
(./=) = primitiveFunction2 "(/=)" (Prelude./=)

(.<) :: (HType a, Ord a) => OE f a -> OE f a -> OE f Bool
(.<) = primitiveFunction2 "(<)" (Prelude.<)

(.>) :: (HType a, Ord a) => OE f a -> OE f a -> OE f Bool
(.>) = primitiveFunction2 "(>)" (Prelude.>)

(.<=) :: (HType a, Ord a) => OE f a -> OE f a -> OE f Bool
(.<=) = primitiveFunction2 "(<=)" (Prelude.<=)

(.>=) :: (HType a, Ord a) => OE f a -> OE f a -> OE f Bool
(.>=) = primitiveFunction2 "(>=)" (Prelude.>=)

not_ ::  OE f Bool -> OE f Bool
not_ = primitiveFunction "not" Prelude.not

-- Selects the elements of the pair depending on the condition
-- (.?) :: Computable a => Data Bool -> (a, a) -> a
-- bCond .? (dTrue, dFalse) = switch bCond dTrue dFalse

(.&&) :: OE f Bool -> OE f Bool -> OE f Bool
(.&&) = primitiveFunction2 "(&&)" (Prelude.&&)

(.||) :: OE f Bool -> OE f Bool -> OE f Bool
(.||) = primitiveFunction2 "(||)" (Prelude.||)