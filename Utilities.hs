module Utilities
  ( (!.)
  , pheo
  , owl
  , on) where

import Data.List
import Data.Function

-- |Deals with historical accident of lists requiring Int indexes
list !. index = list `genericIndex` index

-- |Phoenix combinator. Combines two unary functions and a binary function.
pheo f g h x = f (g x) (h x)
-- |Owl or "boobs" combinator. Equivalent to: owl f g x y = f (g x y)
owl = ((.).(.))
