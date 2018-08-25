{-# LANGUAGE RankNTypes #-}
module Hae.Vector where

import Data.DList as DL
import Hae.Types
import Hae.Expr

data Vector a = Indexed
  { length :: E Int
  , index :: E Int -> a
  }

indexed :: E Int -> (E Int -> a) -> Vector a
indexed = Indexed