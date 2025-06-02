{-# LANGUAGE CPP               #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE MagicHash         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies      #-}

-- |
-- Module      : Basement.UTF8.Helper
-- License     : BSD-style
-- Maintainer  : Foundation
--
-- Some low level helpers to use UTF8
--
-- Most helpers are lowlevel and unsafe, don't use
-- directly.
module Basement.UTF8.Helper
  ( charToBytes
  , toChar1
  , toChar2
  , toChar3
  , toChar4
  , toChar#
  ) where

import           Basement.Compat.Primitive ( word8ToChar# )
import           Basement.Types.OffsetSize
import           Basement.UTF8.Types ( StepASCII (..) )
import           GHC.Prim
import           GHC.Types
import           GHC.Word
import           Prelude ( Monoid (..), Ord (..), Show (..), error, otherwise )

-- mask an UTF8 continuation byte (stripping the leading 10 and returning 6 valid bits)
maskContinuation# :: Word# -> Word#
maskContinuation# v = and# v 0x3f##
{-# INLINE maskContinuation# #-}

-- mask a UTF8 header for 2 bytes encoding (110xxxxx and 5 valid bits)
maskHeader2# :: Word# -> Word#
maskHeader2# h = and# h 0x1f##
{-# INLINE maskHeader2# #-}

-- mask a UTF8 header for 3 bytes encoding (1110xxxx and 4 valid bits)
maskHeader3# :: Word# -> Word#
maskHeader3# h = and# h 0xf##
{-# INLINE maskHeader3# #-}

-- mask a UTF8 header for 4 bytes encoding (11110xxx and 3 valid bits)
maskHeader4# :: Word# -> Word#
maskHeader4# h = and# h 0x7##
{-# INLINE maskHeader4# #-}

or3# :: Word# -> Word# -> Word# -> Word#
or3# a b c = or# a (or# b c)
{-# INLINE or3# #-}

or4# :: Word# -> Word# -> Word# -> Word# -> Word#
or4# a b c d = or# (or# a b) (or# c d)
{-# INLINE or4# #-}

toChar# :: Word# -> Char
toChar# w = C# (chr# (word2Int# w))
{-# INLINE toChar# #-}

toChar1 :: StepASCII -> Char
toChar1 (StepASCII (W8# w)) = C# (word8ToChar# w)

toChar2 :: StepASCII -> Word8 -> Char
toChar2 (StepASCII (W8# b1)) (W8# b2) =
    toChar# (or# (uncheckedShiftL# (maskHeader2# w1) 6#) (maskContinuation# w2))
  where
    w1 = word8ToWord# b1
    w2 = word8ToWord# b2

toChar3 :: StepASCII -> Word8 -> Word8 -> Char
toChar3 (StepASCII (W8# b1)) (W8# b2) (W8# b3) =
    toChar# (or3# (uncheckedShiftL# (maskHeader3# w1) 12#)
                  (uncheckedShiftL# (maskContinuation# w2) 6#)
                  (maskContinuation# w3)
            )
  where
    w1 = word8ToWord# b1
    w2 = word8ToWord# b2
    w3 = word8ToWord# b3

toChar4 :: StepASCII -> Word8 -> Word8 -> Word8 -> Char
toChar4 (StepASCII (W8# b1)) (W8# b2) (W8# b3) (W8# b4) =
    toChar# (or4# (uncheckedShiftL# (maskHeader4# w1) 18#)
                  (uncheckedShiftL# (maskContinuation# w2) 12#)
                  (uncheckedShiftL# (maskContinuation# w3) 6#)
                  (maskContinuation# w4)
            )
  where
    w1 = word8ToWord# b1
    w2 = word8ToWord# b2
    w3 = word8ToWord# b3
    w4 = word8ToWord# b4

charToBytes :: Int -> CountOf Word8
charToBytes c
    | c < 0x80     = CountOf 1
    | c < 0x800    = CountOf 2
    | c < 0x10000  = CountOf 3
    | c < 0x110000 = CountOf 4
    | otherwise    = error ("invalid code point: " `mappend` show c)
