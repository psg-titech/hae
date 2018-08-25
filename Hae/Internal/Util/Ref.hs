-- |
-- Copyright : Copyright (c) 2009, Koen Claessen
--
-- A simple implementation of \"observable sharing\". See
--
-- * Koen Claessen, David Sands,
-- \"/Observable sharing for functional circuit description/\",
-- Asian Computing Science Conference, 1999.
--
-- for more details.

module Hae.Internal.Util.Ref
  ( Ref
  , refId
  , deref
  , ref
  ) where

import Data.Unique
import System.IO.Unsafe

data Ref a = Ref
  { refId :: Unique
  , deref :: a
  }

instance Eq (Ref a) where
  Ref x _ == Ref y _ = x == y

instance Ord (Ref a) where
  Ref x _ `compare` Ref y _ = x `compare` y

ref :: a -> Ref a
ref x =
  unsafePerformIO $ do
    u <- newUnique
    return (Ref u x)
