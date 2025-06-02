{-# LANGUAGE CPP               #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies      #-}

module Basement.Numerical.Number
    ( IsIntegral (..)
    , IsNatural (..)
    ) where

import           Data.Int ( Int, Int16, Int32, Int64, Int8 )
import           Data.Word ( Word, Word16, Word32, Word64, Word8 )
import           Foreign.C.Types
import           Numeric.Natural ( Natural )
import qualified Prelude
import           Prelude ( Eq (..), Integer, Num, Ord (..) )

-- | Number literals, convertible through the generic Integer type.
--
-- all number are Enum'erable, meaning that you can move to
-- next element
class (Num a, Eq a, Ord a) => IsIntegral a where
    {-# MINIMAL toInteger #-}
    toInteger :: a -> Integer

-- | Non Negative Number literals, convertible through the generic Natural type
class IsIntegral a => IsNatural a where
    {-# MINIMAL toNatural #-}
    toNatural :: a -> Natural

instance IsIntegral Integer where
    toInteger i = i
instance IsIntegral Int where
    toInteger = Prelude.toInteger
instance IsIntegral Int8 where
    toInteger = Prelude.toInteger
instance IsIntegral Int16 where
    toInteger = Prelude.toInteger
instance IsIntegral Int32 where
    toInteger = Prelude.toInteger
instance IsIntegral Int64 where
    toInteger = Prelude.toInteger
instance IsIntegral Natural where
    toInteger = Prelude.toInteger
instance IsIntegral Word where
    toInteger = Prelude.toInteger
instance IsIntegral Word8 where
    toInteger = Prelude.toInteger
instance IsIntegral Word16 where
    toInteger = Prelude.toInteger
instance IsIntegral Word32 where
    toInteger = Prelude.toInteger
instance IsIntegral Word64 where
    toInteger = Prelude.toInteger

instance IsIntegral CChar where
    toInteger = Prelude.toInteger
instance IsIntegral CSChar where
    toInteger = Prelude.toInteger
instance IsIntegral CUChar where
    toInteger = Prelude.toInteger
instance IsIntegral CShort where
    toInteger = Prelude.toInteger
instance IsIntegral CUShort where
    toInteger = Prelude.toInteger
instance IsIntegral CInt where
    toInteger = Prelude.toInteger
instance IsIntegral CUInt where
    toInteger = Prelude.toInteger
instance IsIntegral CLong where
    toInteger = Prelude.toInteger
instance IsIntegral CULong where
    toInteger = Prelude.toInteger
instance IsIntegral CPtrdiff where
    toInteger = Prelude.toInteger
instance IsIntegral CSize where
    toInteger = Prelude.toInteger
instance IsIntegral CWchar where
    toInteger = Prelude.toInteger
instance IsIntegral CSigAtomic where
    toInteger = Prelude.toInteger
instance IsIntegral CLLong where
    toInteger = Prelude.toInteger
instance IsIntegral CULLong where
    toInteger = Prelude.toInteger
instance IsIntegral CBool where
    toInteger = Prelude.toInteger
instance IsIntegral CIntPtr where
    toInteger = Prelude.toInteger
instance IsIntegral CUIntPtr where
    toInteger = Prelude.toInteger
instance IsIntegral CIntMax where
    toInteger = Prelude.toInteger
instance IsIntegral CUIntMax where
    toInteger = Prelude.toInteger

instance IsNatural Natural where
    toNatural i = i
instance IsNatural Word where
    toNatural = Prelude.fromIntegral
instance IsNatural Word8 where
    toNatural = Prelude.fromIntegral
instance IsNatural Word16 where
    toNatural = Prelude.fromIntegral
instance IsNatural Word32 where
    toNatural = Prelude.fromIntegral
instance IsNatural Word64 where
    toNatural = Prelude.fromIntegral

instance IsNatural CUChar where
    toNatural = Prelude.fromIntegral
instance IsNatural CUShort where
    toNatural = Prelude.fromIntegral
instance IsNatural CUInt where
    toNatural = Prelude.fromIntegral
instance IsNatural CULong where
    toNatural = Prelude.fromIntegral
instance IsNatural CSize where
    toNatural = Prelude.fromIntegral
instance IsNatural CULLong where
    toNatural = Prelude.fromIntegral
instance IsNatural CUIntPtr where
    toNatural = Prelude.fromIntegral
instance IsNatural CUIntMax where
    toNatural = Prelude.fromIntegral
