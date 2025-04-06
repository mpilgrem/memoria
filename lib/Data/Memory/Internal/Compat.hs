-- |
-- Module      : Data.Memory.Internal.Compat
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : stable
-- Portability : Good
--
-- This module try to keep all the difference between versions of base
-- or other needed packages, so that modules don't need to use CPP
--
{-# LANGUAGE CPP #-}
module Data.Memory.Internal.Compat
    ( unsafeDoIO
    , popCount
    , unsafeShiftL
    , unsafeShiftR
    , byteSwap64
    , byteSwap32
    , byteSwap16
    ) where

import System.IO.Unsafe
import Data.Word
import Data.Bits

-- | perform io for hashes that do allocation and ffi.
-- unsafeDupablePerformIO is used when possible as the
-- computation is pure and the output is directly linked
-- to the input. we also do not modify anything after it has
-- been returned to the user.
unsafeDoIO :: IO a -> a
unsafeDoIO = unsafeDupablePerformIO
