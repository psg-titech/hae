{-# LANGUAGE FlexibleInstances #-}

module Hae.Num where

import Hae.Internal.Expr (OE, lit_)
import Hae.Internal.PrimOp
import Hae.Types

instance Num (OE f Int) where
  fromInteger = lit_ . fromInteger
  abs = primAbs
  signum = primSignum
  (+) = primPlus
  (-) = primMinus
  (*) = primMult

instance Num (OE f Float) where
  fromInteger = lit_ . fromInteger
  abs = primAbs
  signum = primSignum
  (+) = primPlus
  (-) = primMinus
  (*) = primMult

instance Num (OE f Double) where
  fromInteger = lit_ . fromInteger
  abs = primAbs
  signum = primSignum
  (+) = primPlus
  (-) = primMinus
  (*) = primMult

instance Fractional (OE f Float) where
  fromRational = lit_ . fromRational
  (/) = primDiv

instance Fractional (OE f Double) where
  fromRational = lit_ . fromRational
  (/) = primDiv
-- TODO: instance Integral (OE f Int) where ...
