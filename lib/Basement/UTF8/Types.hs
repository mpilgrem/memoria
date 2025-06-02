module Basement.UTF8.Types
  ( -- * Stepper
    Step (..)
  , StepASCII (..)
  ) where

import           Basement.Types.OffsetSize
import           Data.Word ( Word8 )

-- | Step when walking a String
--
-- this is a return value composed of :
-- * the unicode code point read (Char) which need to be
--   between 0 and 0x10ffff (inclusive)
-- * The next offset to start reading the next unicode code point (or end)
data Step = Step {-# UNPACK #-} !Char {-# UNPACK #-} !(Offset Word8)

-- | Step when processing ASCII character
newtype StepASCII = StepASCII { stepAsciiRawValue :: Word8 }
