module Hae.Internal.PrimOp where

import           Hae.Internal.Expr (OE, primitiveFunction, primitiveFunction2)
import           Hae.Internal.FOAS (TypeTag (..))
import           Hae.Types

primTyping1 :: String -> TypeTag -> TypeTag
primTyping1 "abs" t    = t
primTyping1 "signum" t = t
primTyping1 "not" TBool   = TBool
primTyping1 _ _           = error "Typeing Error"

-- FIXME: cumbersome, replace analyzeType in Deref.hs alltogether
primTyping2 :: String -> TypeTag -> TypeTag -> TypeTag
primTyping2 "(+)" t1 t2 | t1 == t2 = t1
primTyping2 "(-)" t1 t2 | t1 == t2 = t1
primTyping2 "(*)" t1 t2 | t1 == t2 = t1
primTyping2 "(/)" t1 t2 | t1 == t2 = t1
primTyping2 "(>)" t1 t2 | t1 == t2 = TBool
primTyping2 "(<)" t1 t2 | t1 == t2 = TBool
primTyping2 "(==)" t1 t2 | t1 == t2 = TBool
primTyping2 "(/=)" t1 t2 | t1 == t2 = TBool
primTyping2 "(<=)" t1 t2 | t1 == t2 = TBool
primTyping2 "(>=)" t1 t2 | t1 == t2 = TBool
primTyping2 "(||)" TBool TBool = TBool
primTyping2 "(&&)" TBool TBool = TBool
primTyping2 _ _ _              = error "Typing Error"

primAbs :: (HType t, Num t) => OE f t -> OE f t
primAbs = primitiveFunction "abs" abs

primSignum :: (HType t, Num t) => OE f t -> OE f t
primSignum = primitiveFunction "signum" signum

primPlus :: (HType t, Num t) => OE f t -> OE f t -> OE f t
primPlus = primitiveFunction2 "(+)" (+)

primMinus :: (HType t, Num t) => OE f t -> OE f t -> OE f t
primMinus = primitiveFunction2 "(-)" (-)

primMult :: (HType t, Num t) => OE f t -> OE f t -> OE f t
primMult = primitiveFunction2 "(*)" (*)

primDiv :: (HType t, Fractional t) => OE f t -> OE f t -> OE f t
primDiv = primitiveFunction2 "(/)" (/)
