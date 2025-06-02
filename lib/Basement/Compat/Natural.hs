module Basement.Compat.Natural
  ( integerToNatural
  ) where

import Numeric.Natural ( Natural )
import Prelude (Integer, Num (..))

integerToNatural :: Integer -> Natural
integerToNatural i = fromInteger (abs i)
