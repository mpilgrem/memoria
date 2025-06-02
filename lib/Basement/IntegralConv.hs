{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Basement.IntegralConv
  ( IntegralUpsize (..)
  , charToInt
  ) where

import           Basement.Compat.Primitive
                   ( int16ToInt32#, int8ToInt16#, int8ToInt32#, word16ToWord32#
                   , word16ToWord8#, word32ToWord16#, word32ToWord8#
                   , word8ToInt16#, word8ToInt32#, word8ToWord16#
                   , word8ToWord32#
                   )
import           Basement.Numerical.Conversion
                   ( charToInt, intToInt64, int64ToInt, wordToWord64 )
import           Basement.Numerical.Number
import           GHC.Int
import           GHC.Prim hiding (word64ToWord#)
import qualified GHC.Prim
import           GHC.Types
import           GHC.Word
import           Prelude ( Integer, Ord (..), fromIntegral, id, otherwise )
import           Numeric.Natural ( Natural )

-- | Downsize an integral value
class IntegralDownsize a b where
    integralDownsize :: a -> b
    default integralDownsize :: a ~ b => a -> b
    integralDownsize = id

-- | Upsize an integral value
--
-- The destination type 'b' size need to be greater or equal
-- than the size type of 'a'
class IntegralUpsize a b where
    integralUpsize      :: a -> b

instance IsIntegral a => IntegralUpsize a Integer where
    integralUpsize = toInteger
instance IsNatural a => IntegralUpsize a Natural where
    integralUpsize = toNatural

instance IntegralUpsize Int8 Int16 where
    integralUpsize (I8# i) = I16# (int8ToInt16# i)
instance IntegralUpsize Int8 Int32 where
    integralUpsize (I8# i) = I32# (int8ToInt32# i)
instance IntegralUpsize Int8 Int64 where
    integralUpsize (I8# i) = intToInt64 (I# (int8ToInt# i))
instance IntegralUpsize Int8 Int where
    integralUpsize (I8# i) = I# (int8ToInt# i)

instance IntegralUpsize Int16 Int32 where
    integralUpsize (I16# i) = I32# (int16ToInt32# i)
instance IntegralUpsize Int16 Int64 where
    integralUpsize (I16# i) = intToInt64 (I# (int16ToInt# i))
instance IntegralUpsize Int16 Int where
    integralUpsize (I16# i) = I# (int16ToInt# i)

instance IntegralUpsize Int32 Int64 where
    integralUpsize (I32# i) = intToInt64 (I# (int32ToInt# i))
instance IntegralUpsize Int32 Int where
    integralUpsize (I32# i) = I# (int32ToInt# i)

instance IntegralUpsize Int Int64 where
    integralUpsize = intToInt64

instance IntegralUpsize Word8 Word16 where
    integralUpsize (W8# i) = W16# (word8ToWord16# i)
instance IntegralUpsize Word8 Word32 where
    integralUpsize (W8# i) = W32# (word8ToWord32# i)
instance IntegralUpsize Word8 Word64 where
    integralUpsize (W8# i) = wordToWord64 (W# (word8ToWord# i))
instance IntegralUpsize Word8 Word where
    integralUpsize (W8# i) = W# (word8ToWord# i)
instance IntegralUpsize Word8 Int16 where
    integralUpsize (W8# w) = I16# (word8ToInt16# w)
instance IntegralUpsize Word8 Int32 where
    integralUpsize (W8# w) = I32# (word8ToInt32# w)
instance IntegralUpsize Word8 Int64 where
    integralUpsize (W8# w) = intToInt64 (I# (word2Int# (word8ToWord# w)))
instance IntegralUpsize Word8 Int where
    integralUpsize (W8# w) = I# (word2Int# (word8ToWord# w))

instance IntegralUpsize Word16 Word32 where
    integralUpsize (W16# i) = W32# (word16ToWord32# i)
instance IntegralUpsize Word16 Word64 where
    integralUpsize (W16# i) = wordToWord64 (W# (word16ToWord# i))
instance IntegralUpsize Word16 Word where
    integralUpsize (W16# i) = W# (word16ToWord# i)

instance IntegralUpsize Word32 Word64 where
    integralUpsize (W32# i) = wordToWord64 (W# (word32ToWord# i))
instance IntegralUpsize Word32 Word where
    integralUpsize (W32# i) = W# (word32ToWord# i)

instance IntegralUpsize Word Word64 where
    integralUpsize = wordToWord64

instance IntegralDownsize Int Int8 where
    integralDownsize      (I# i) = I8# (intToInt8# i)
instance IntegralDownsize Int Int16 where
    integralDownsize      (I# i) = I16# (intToInt16# i)
instance IntegralDownsize Int Int32 where
    integralDownsize      (I# i) = I32# (intToInt32# i)

instance IntegralDownsize Int64 Int8 where
    integralDownsize      i = integralDownsize (int64ToInt i)
instance IntegralDownsize Int64 Int16 where
    integralDownsize      i = integralDownsize (int64ToInt i)
instance IntegralDownsize Int64 Int32 where
    integralDownsize      i = integralDownsize (int64ToInt i)
instance IntegralDownsize Int64 Int where
    integralDownsize      = int64ToInt

instance IntegralDownsize Word64 Word8 where
#if __GLASGOW_HASKELL__ >= 904
    integralDownsize      (W64# i) = W8# (wordToWord8# (GHC.Prim.word64ToWord# i))
#else
    integralDownsize      (W64# i) = W8# (wordToWord8# (word64ToWord# i))
#endif
instance IntegralDownsize Word64 Word16 where
#if __GLASGOW_HASKELL__ >= 904
    integralDownsize      (W64# i) = W16# (wordToWord16# (GHC.Prim.word64ToWord# i))
#else
    integralDownsize      (W64# i) = W16# (wordToWord16# (word64ToWord# i))
#endif
instance IntegralDownsize Word64 Word32 where
#if __GLASGOW_HASKELL__ >= 904
    integralDownsize      (W64# i) = W32# (wordToWord32# (GHC.Prim.word64ToWord# i))
#else
    integralDownsize      (W64# i) = W32# (wordToWord32# (word64ToWord# i))
#endif

instance IntegralDownsize Word Word8 where
    integralDownsize (W# w) = W8# (wordToWord8# w)
instance IntegralDownsize Word Word16 where
    integralDownsize (W# w) = W16# (wordToWord16# w)
instance IntegralDownsize Word Word32 where
    integralDownsize (W# w) = W32# (wordToWord32# w)

instance IntegralDownsize Word32 Word8 where
    integralDownsize      (W32# i) = W8# (word32ToWord8# i)
instance IntegralDownsize Word32 Word16 where
    integralDownsize      (W32# i) = W16# (word32ToWord16# i)

instance IntegralDownsize Word16 Word8 where
    integralDownsize      (W16# i) = W8# (word16ToWord8# i)

instance IntegralDownsize Integer Int8 where
    integralDownsize = fromIntegral
instance IntegralDownsize Integer Int16 where
    integralDownsize = fromIntegral
instance IntegralDownsize Integer Int32 where
    integralDownsize = fromIntegral
instance IntegralDownsize Integer Int64 where
    integralDownsize = fromIntegral

instance IntegralDownsize Integer Word8 where
    integralDownsize = fromIntegral
instance IntegralDownsize Integer Word16 where
    integralDownsize = fromIntegral
instance IntegralDownsize Integer Word32 where
    integralDownsize = fromIntegral
instance IntegralDownsize Integer Word64 where
    integralDownsize = fromIntegral
instance IntegralDownsize Integer Natural where
    integralDownsize i
        | i >= 0    = fromIntegral i
        | otherwise = 0

instance IntegralDownsize Natural Word8 where
    integralDownsize = fromIntegral
instance IntegralDownsize Natural Word16 where
    integralDownsize = fromIntegral
instance IntegralDownsize Natural Word32 where
    integralDownsize = fromIntegral
instance IntegralDownsize Natural Word64 where
    integralDownsize = fromIntegral
