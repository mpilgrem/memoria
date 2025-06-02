{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies      #-}

module Basement.Compat.ExtList
  ( length
  , sum
  ) where

import           Basement.Numerical.Additive ( Additive (..) )
import           Basement.Types.OffsetSize ( CountOf (..) )
import qualified GHC.List as List
import           Prelude ( (.) )

-- | Compute the size of the list
length :: [a] -> CountOf a
length = CountOf . List.foldl' (\c _ -> c + 1) 0

-- | Sum the element in a list
sum :: Additive n => [n] -> n
sum []     = azero
sum (i:is) = loop i is
  where
    loop !acc [] = acc
    loop !acc (x:xs) = loop (acc+x) xs
    {-# INLINE loop #-}
