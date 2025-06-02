{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE UnboxedTuples       #-}

-- |
-- Module      : Basement.Block.Mutable
-- License     : BSD-style
-- Maintainer  : Haskell Foundation
--
-- A block of memory that contains elements of a type,
-- very similar to an unboxed array but with the key difference:
--
-- * It doesn't have slicing capability (no cheap take or drop)
-- * It consume less memory: 1 Offset, 1 CountOf, 1 Pinning status trimmed
-- * It's unpackable in any constructor
-- * It uses unpinned memory by default
--
-- It should be rarely needed in high level API, but
-- in lowlevel API or some data structure containing lots
-- of unboxed array that will benefit from optimisation.
--
-- Because it's unpinned, the blocks are compactable / movable,
-- at the expense of making them less friendly to interop with the C layer
-- as address.
--
-- Note that sadly the bytearray primitive type automatically create
-- a pinned bytearray if the size is bigger than a certain threshold
--
-- GHC Documentation associated:
--
-- includes/rts/storage/Block.h
--   * LARGE_OBJECT_THRESHOLD ((uint32_t)(BLOCK_SIZE * 8 / 10))
--   * BLOCK_SIZE   (1<<BLOCK_SHIFT)
--
-- includes/rts/Constant.h
--   * BLOCK_SHIFT  12
--
module Basement.Block.Mutable
  ( -- * Foreign
    copyToPtr
  ) where

import           Basement.Block.Base
                   ( Block (..), MutableBlock (..), mutableLengthBytes
                   , unsafeFreeze
                   )
import           Basement.Exception ( OutOfBoundOperation (..), primOutOfBound )
import           Basement.Monad ( PrimMonad (..) )
import           Basement.PrimType ( PrimType (..), offsetInBytes )
import           Basement.Types.OffsetSize
                   ( CountOf (..), Offset (..), offsetPlusE, sizeAsOffset )
import           GHC.Int ( Int (..) )
import           GHC.Prim ( copyByteArrayToAddr# )
import           GHC.Ptr ( Ptr (..) )
import           Prelude ( Ord (..), ($), otherwise )

-- | Copy all the block content to the memory starting at the destination address
--
-- If the destination pointer is invalid (size or bad allocation), bad things will happen
copyToPtr ::
     forall ty prim . (PrimType ty, PrimMonad prim)
  => MutableBlock ty (PrimState prim)
     -- ^ The source mutable block to copy
  -> Offset ty
     -- ^ The source offset in the mutable block
  -> Ptr ty
     -- ^ The destination address where the copy is going to start
  -> CountOf ty
     -- ^ The number of bytes
  -> prim ()
copyToPtr mb ofs (Ptr dst#) _
    | srcEnd > sizeAsOffset arrSz = primOutOfBound OOB_MemCopy srcEnd arrSz
    | otherwise                = do
        blk <- unsafeFreeze mb
        let !(Block ba) = blk
        primitive $ \s1 -> (# copyByteArrayToAddr# ba os# dst# szBytes# s1, () #)
  where
    srcEnd = os `offsetPlusE` arrSz
    !os@(Offset (I# os#)) = offsetInBytes ofs
    !arrSz@(CountOf (I# szBytes#)) = mutableLengthBytes mb
