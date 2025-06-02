{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UnboxedTuples         #-}

-- |
-- Module      : Basement.UArray
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : portable
--
-- An unboxed array of primitive types
--
-- All the cells in the array are in one chunk of contiguous
-- memory.
module Basement.UArray
  ( -- * internal methods
    recast
  , unsafeRecast
    -- * Creation
  , copyToPtr
  , withPtr
    -- * Functions
  , null
  , splitAt
  , all
  , builderAppend
  , builderBuild
  ) where

import           Basement.Alg.Class ( Indexable (..) )
import           Basement.Types.OffsetSize
import           Basement.Compat.MonadTrans
import           Basement.Monad
import           Basement.PrimType
import           Basement.FinalPtr ( castFinalPtr, withFinalPtr )
import           Basement.Exception
                   ( InvalidRecast (..), RecastDestinationSize (..)
                   , RecastSourceSize (..)
                   )
import           Basement.UArray.Base
import           Basement.Block.Base ( Block (..) )
import qualified Basement.Block.Base as BLK ( withPtr )
import           Basement.Numerical.Additive
import           Basement.Numerical.Subtractive
import           Basement.Numerical.Multiplicative ( IDivisible (..) )
import           Basement.MutableBuilder
import           Control.Exception ( throw )
import           Data.Proxy ( Proxy (..) )
import           Foreign.Marshal.Utils ( copyBytes )
import           GHC.Prim
import           GHC.Ptr
import           GHC.Types
import           GHC.Word
import           Prelude
                   ( Applicative (..), Either (..), Eq (..), Maybe (..)
                   , Monad (..), Ord (..), ($), (<$>), otherwise, snd
                   )

-----------------------------------------------------------------------
-- higher level collection implementation
-----------------------------------------------------------------------

-- | Copy all the block content to the memory starting at the destination address
copyToPtr :: forall ty prim . (PrimType ty, PrimMonad prim)
          => UArray ty -- ^ the source array to copy
          -> Ptr ty    -- ^ The destination address where the copy is going to start
          -> prim ()
copyToPtr arr dst@(Ptr dst#) = onBackendPrim copyBa copyPtr arr
  where
    !(Offset os@(I# os#)) = offsetInBytes $ offset arr
    !(CountOf szBytes@(I# szBytes#)) = sizeInBytes $ length arr
    copyBa (Block ba) = primitive $ \s1 -> (# copyByteArrayToAddr# ba os# dst# szBytes# s1, () #)
    copyPtr fptr = unsafePrimFromIO $ withFinalPtr fptr $ \ptr -> copyBytes dst (ptr `plusPtr` os) szBytes

-- | Get a Ptr pointing to the data in the UArray.
--
-- Since a UArray is immutable, this Ptr shouldn't be
-- to use to modify the contents
--
-- If the UArray is pinned, then its address is returned as is,
-- however if it's unpinned, a pinned copy of the UArray is made
-- before getting the address.
withPtr :: forall ty prim a . (PrimMonad prim, PrimType ty)
        => UArray ty
        -> (Ptr ty -> prim a)
        -> prim a
withPtr a f =
    onBackendPrim (\blk  -> BLK.withPtr  blk  $ \ptr -> f (ptr `plusPtr` os))
                  (\fptr -> withFinalPtr fptr $ \ptr -> f (ptr `plusPtr` os))
                  a
  where
    !sz          = primSizeInBytes (Proxy :: Proxy ty)
    !(Offset os) = offsetOfE sz $ offset a
{-# INLINE withPtr #-}

-- | Recast an array of type a to an array of b
--
-- a and b need to have the same size otherwise this
-- raise an async exception
recast :: forall a b . (PrimType a, PrimType b) => UArray a -> UArray b
recast array
    | aTypeSize == bTypeSize = unsafeRecast array
    | missing   == 0         = unsafeRecast array
    | otherwise = throw $ InvalidRecast
                      (RecastSourceSize      alen)
                      (RecastDestinationSize $ alen + missing)
  where
    aTypeSize = primSizeInBytes (Proxy :: Proxy a)
    bTypeSize@(CountOf bs) = primSizeInBytes (Proxy :: Proxy b)
    (CountOf alen) = sizeInBytes (length array)
    missing = alen `mod` bs

-- | Unsafely recast an UArray containing 'a' to an UArray containing 'b'
--
-- The offset and size are converted from units of 'a' to units of 'b',
-- but no check are performed to make sure this is compatible.
--
-- use 'recast' if unsure.
unsafeRecast :: (PrimType a, PrimType b) => UArray a -> UArray b
unsafeRecast (UArray start len backend) = UArray (primOffsetRecast start) (sizeRecast len) $
    case backend of
        UArrayAddr fptr     -> UArrayAddr (castFinalPtr fptr)
        UArrayBA (Block ba) -> UArrayBA (Block ba)
{-# INLINE [1] unsafeRecast #-}
{-# SPECIALIZE [3] unsafeRecast :: PrimType a => UArray Word8 -> UArray a #-}

null :: UArray ty -> Bool
null arr = length arr == 0

-- | Split an array into two, with a count of at most N elements in the first one
-- and the remaining in the other.
splitAt :: CountOf ty -> UArray ty -> (UArray ty, UArray ty)
splitAt nbElems arr@(UArray start len backend)
    | nbElems <= 0                               = (empty, arr)
    | Just nbTails <- len - nbElems, nbTails > 0 = (UArray start                         nbElems backend
                                                   ,UArray (start `offsetPlusE` nbElems) nbTails backend)
    | otherwise                                  = (arr, empty)

all :: forall ty. PrimType ty => (ty -> Bool) -> UArray ty -> Bool
all predicate arr = onBackendPure' arr $ all'
 where
  all' ::
       Indexable container ty
    => container -> Offset ty -> Offset ty -> Bool
  all' ba start end = loop start
   where
    loop !i
      | i == end               = True
      | predicate (index ba i) = loop (i + 1)
      | otherwise              = False
{-# SPECIALIZE [3] all :: (Word8 -> Bool) -> UArray Word8 -> Bool #-}

builderAppend ::
     (PrimType ty, PrimMonad state)
  => ty -> Builder (UArray ty) (MUArray ty) ty state err ()
builderAppend v = Builder $ State $ \(i, st, e) ->
    if offsetAsSize i == chunkSize st
      then do
        cur <- unsafeFreeze (curChunk st)
        newChunk <- new (chunkSize st)
        unsafeWrite newChunk 0 v
        pure ((), (Offset 1, st { prevChunks     = cur : prevChunks st
                                , prevChunksSize = chunkSize st + prevChunksSize st
                                , curChunk       = newChunk
                                }, e))
      else do
        unsafeWrite (curChunk st) i v
        pure ((), (i + 1, st, e))

builderBuild ::
     (PrimType ty, PrimMonad m)
  => Int
  -> Builder (UArray ty) (MUArray ty) ty m err ()
  -> m (Either err (UArray ty))
builderBuild sizeChunksI ab
    | sizeChunksI <= 0 = builderBuild 64 ab
    | otherwise        = do
        first      <- new sizeChunks
        (i, st, e) <- snd <$> runState (runBuilder ab) (Offset 0, BuildingState [] (CountOf 0) first sizeChunks, Nothing)
        case e of
          Just err -> pure (Left err)
          Nothing -> do
            cur <- unsafeFreezeShrink (curChunk st) (offsetAsSize i)
            -- Build final array
            let totalSize = prevChunksSize st + offsetAsSize i
            bytes <- new totalSize >>= fillFromEnd totalSize (cur : prevChunks st) >>= unsafeFreeze
            pure (Right bytes)
  where
      sizeChunks = CountOf sizeChunksI

      fillFromEnd _    []     mua = pure mua
      fillFromEnd !end (x:xs) mua = do
          let sz = length x
          let start = end `sizeSub` sz
          unsafeCopyAtRO mua (sizeAsOffset start) x (Offset 0) sz
          fillFromEnd start xs mua
