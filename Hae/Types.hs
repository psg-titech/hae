module Hae.Types where

import Data.Typeable

-- |Tag built-in ops for the compiler
data PrimOpId = PrimOpId1 String | PrimOpId2 String deriving Show


class (Show a, Typeable a) =>
      HType a

instance HType ()

instance (HType a, HType b) => HType (a, b)

instance HType Int

instance HType Bool

instance HType Double

instance HType Float