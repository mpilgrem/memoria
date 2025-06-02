{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE TypeFamilies               #-}

-- |
-- Module      : Basement.Endianness
-- License     : BSD-style
-- Maintainer  : Haskell Foundation
-- Stability   : experimental
-- Portability : portable
--
-- Set endianness tag to a given primitive. This will help for serialising
-- data for protocols (such as the network protocols).
--

module Basement.Endianness
    ( -- * Big Endian
      BE (..)
      -- * Little Endian
    , LE (..)
    ) where

import           Data.Bits
import           Data.Word
                   ( Word16, Word32, Word64, byteSwap16, byteSwap32
                   , byteSwap64
                   )
import           Data.Typeable ( Typeable )
import           Prelude ( Eq (..), Ord (..), Show (..) )

#if defined(ARCH_IS_LITTLE_ENDIAN) || defined(ARCH_IS_BIG_ENDIAN)
#else
import           Data.Word ( Word8 )
import           Foreign.Marshal.Alloc (alloca)
import           Foreign.Ptr (castPtr)
import           Foreign.Storable (poke, peek)
import           System.IO.Unsafe (unsafePerformIO)
#endif

data Endianness =
      LittleEndian
    | BigEndian
    deriving (Eq, Show)

-- | Little Endian value
newtype LE a = LE { unLE :: a }
  deriving (Show, Eq, Typeable, Bits)
instance (ByteSwap a, Ord a) => Ord (LE a) where
    compare e1 e2 = compare (fromLE e1) (fromLE e2)

-- | Big Endian value
newtype BE a = BE { unBE :: a }
  deriving (Show, Eq, Typeable, Bits)
instance (ByteSwap a, Ord a) => Ord (BE a) where
    compare e1 e2 = compare (fromBE e1) (fromBE e2)

-- | Convert from a big endian value to the cpu endianness
fromBE :: ByteSwap a => BE a -> a
#ifdef ARCH_IS_LITTLE_ENDIAN
fromBE (BE a) = byteSwap a
#elif ARCH_IS_BIG_ENDIAN
fromBE (BE a) = a
#else
fromBE (BE a) = if endianness == LittleEndian then byteSwap a else a
#endif
{-# INLINE fromBE #-}

-- | Convert from a little endian value to the cpu endianness
fromLE :: ByteSwap a => LE a -> a
#ifdef ARCH_IS_LITTLE_ENDIAN
fromLE (LE a) = a
#elif ARCH_IS_BIG_ENDIAN
fromLE (LE a) = byteSwap a
#else
fromLE (LE a) = if endianness == LittleEndian then a else byteSwap a
#endif
{-# INLINE fromLE #-}

-- | Class of types that can be byte-swapped.
--
-- e.g. Word16, Word32, Word64
class ByteSwap a where
    byteSwap :: a -> a
instance ByteSwap Word16 where
    byteSwap = byteSwap16
instance ByteSwap Word32 where
    byteSwap = byteSwap32
instance ByteSwap Word64 where
    byteSwap = byteSwap64
