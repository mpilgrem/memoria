{-# LANGUAGE FlexibleInstances #-}

-- |
-- Module      : Basement.Compat.NumLiteral
-- License     : BSD-style
-- Maintainer  : Foundation
--
-- Literal support for Integral
module Basement.Compat.NumLiteral
  ( Integral (..)
  , HasNegation (..)
  ) where

import           Data.Int ( Int8, Int16, Int32, Int64 )
import           Data.Word ( Word8, Word16, Word32, Word64, Word )
import           Foreign.C.Types
                   ( CBool, CChar, CClock, CDouble, CFloat, CInt, CIntMax
                   , CIntPtr,CLLong, CLong, CPtrdiff, CSChar, CSUSeconds
                   , CShort, CSigAtomic, CSize, CTime, CUChar, CUInt, CUIntMax
                   , CUIntPtr, CULLong, CULong, CUSeconds, CUShort, CWchar
                   )
import           Foreign.Ptr ( IntPtr )
import           Numeric.Natural ( Natural )
import qualified Prelude
import           Prelude ( Int, Integer, Float, Double )
import           System.Posix.Types ( COff )

-- | Integral Literal support
--
-- e.g. 123 :: Integer
--      123 :: Word8
class Integral a where
    fromInteger :: Integer -> a

-- | Negation support
--
-- e.g. -(f x)
class HasNegation a where
    negate :: a -> a

instance Integral Integer where
    fromInteger a = a
instance Integral Natural where
    fromInteger = Prelude.fromInteger
instance Integral Int where
    fromInteger = Prelude.fromInteger
instance Integral Word where
    fromInteger = Prelude.fromInteger
instance Integral Word8 where
    fromInteger = Prelude.fromInteger
instance Integral Word16 where
    fromInteger = Prelude.fromInteger
instance Integral Word32 where
    fromInteger = Prelude.fromInteger
instance Integral Word64 where
    fromInteger = Prelude.fromInteger
instance Integral Int8 where
    fromInteger = Prelude.fromInteger
instance Integral Int16 where
    fromInteger = Prelude.fromInteger
instance Integral Int32 where
    fromInteger = Prelude.fromInteger
instance Integral Int64 where
    fromInteger = Prelude.fromInteger
instance Integral IntPtr where
    fromInteger = Prelude.fromInteger

instance Integral Float where
    fromInteger = Prelude.fromInteger
instance Integral Double where
    fromInteger = Prelude.fromInteger

instance Integral CChar where
    fromInteger = Prelude.fromInteger
instance Integral CSChar where
    fromInteger = Prelude.fromInteger
instance Integral CUChar where
    fromInteger = Prelude.fromInteger
instance Integral CShort where
    fromInteger = Prelude.fromInteger
instance Integral CUShort where
    fromInteger = Prelude.fromInteger
instance Integral CInt where
    fromInteger = Prelude.fromInteger
instance Integral CUInt where
    fromInteger = Prelude.fromInteger
instance Integral CLong where
    fromInteger = Prelude.fromInteger
instance Integral CULong where
    fromInteger = Prelude.fromInteger
instance Integral CPtrdiff where
    fromInteger = Prelude.fromInteger
instance Integral CSize where
    fromInteger = Prelude.fromInteger
instance Integral CWchar where
    fromInteger = Prelude.fromInteger
instance Integral CSigAtomic where
    fromInteger = Prelude.fromInteger
instance Integral CLLong where
    fromInteger = Prelude.fromInteger
instance Integral CULLong where
    fromInteger = Prelude.fromInteger
instance Integral CBool where
    fromInteger = Prelude.fromInteger
instance Integral CIntPtr where
    fromInteger = Prelude.fromInteger
instance Integral CUIntPtr where
    fromInteger = Prelude.fromInteger
instance Integral CIntMax where
    fromInteger = Prelude.fromInteger
instance Integral CUIntMax where
    fromInteger = Prelude.fromInteger
instance Integral CClock where
    fromInteger = Prelude.fromInteger
instance Integral CTime where
    fromInteger = Prelude.fromInteger
instance Integral CUSeconds where
    fromInteger = Prelude.fromInteger
instance Integral CSUSeconds where
    fromInteger = Prelude.fromInteger
instance Integral COff where
    fromInteger = Prelude.fromInteger
instance Integral CFloat where
    fromInteger = Prelude.fromInteger
instance Integral CDouble where
    fromInteger = Prelude.fromInteger

instance HasNegation Integer where
    negate = Prelude.negate
instance HasNegation Int where
    negate = Prelude.negate
instance HasNegation Int8 where
    negate = Prelude.negate
instance HasNegation Int16 where
    negate = Prelude.negate
instance HasNegation Int32 where
    negate = Prelude.negate
instance HasNegation Int64 where
    negate = Prelude.negate
instance HasNegation Word where
    negate = Prelude.negate
instance HasNegation Word8 where
    negate = Prelude.negate
instance HasNegation Word16 where
    negate = Prelude.negate
instance HasNegation Word32 where
    negate = Prelude.negate
instance HasNegation Word64 where
    negate = Prelude.negate

instance HasNegation Float where
    negate = Prelude.negate
instance HasNegation Double where
    negate = Prelude.negate

instance HasNegation CChar where
    negate = Prelude.negate
instance HasNegation CSChar where
    negate = Prelude.negate
instance HasNegation CShort where
    negate = Prelude.negate
instance HasNegation CInt where
    negate = Prelude.negate
instance HasNegation CLong where
    negate = Prelude.negate
instance HasNegation CPtrdiff where
    negate = Prelude.negate
instance HasNegation CWchar where
    negate = Prelude.negate
instance HasNegation CLLong where
    negate = Prelude.negate
instance HasNegation CIntMax where
    negate = Prelude.negate

instance HasNegation CFloat where
    negate = Prelude.negate
instance HasNegation CDouble where
    negate = Prelude.negate
