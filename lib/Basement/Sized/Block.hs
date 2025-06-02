{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE NoStarIsType               #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}

-- |
-- Module      : Basement.Sized.Block
-- License     : BSD-style
-- Maintainer  : Haskell Foundation
--
-- A Nat-sized version of Block
module Basement.Sized.Block
    ( BlockN
    , lengthBytes
    , toBlock
    , new
    , freeze
    , withPtr
    , withMutablePtrHint
    ) where

import           Basement.Block.Base ( Block, MutableBlock (..) )
import qualified Basement.Block as B ( freeze )
import qualified Basement.Block.Base as B
import           Basement.Monad ( PrimMonad, PrimState )
import           Basement.Nat
import           Basement.NormalForm
import           Basement.PrimType ( PrimType )
import           Basement.Types.OffsetSize
import           Data.Data ( Data (..) )
import           Data.Proxy ( Proxy (..) )
import           Data.Word ( Word8 )
import           Foreign.Ptr ( Ptr )
import           Prelude ( Bool (..), Eq (..), Ord (..), Show (..), (.), (<$>) )

-- | Sized version of 'Block'
--
newtype BlockN (n :: Nat) a = BlockN { unBlock :: Block a }
  deriving (NormalForm, Eq, Show, Data, Ord)

newtype MutableBlockN (n :: Nat) ty st =
  MutableBlockN { unMBlock :: MutableBlock ty st }

lengthBytes :: forall n ty
             . PrimType ty
            => BlockN n ty
            -> CountOf Word8
lengthBytes = B.lengthBytes . unBlock

toBlock :: BlockN n ty -> Block ty
toBlock = unBlock

-- | Create a new unpinned mutable block of a specific N size of 'ty' elements
--
-- If the size exceeds a GHC-defined threshold, then the memory will be
-- pinned. To be certain about pinning status with small size, use 'newPinned'
new :: forall n ty prim
     . (PrimType ty, KnownNat n, Countable ty n, PrimMonad prim)
    => prim (MutableBlockN n ty (PrimState prim))
new = MutableBlockN <$> B.new (toCount @n)

freeze ::
     (PrimMonad prim, PrimType ty, Countable ty n)
  => MutableBlockN n ty (PrimState prim) -> prim (BlockN n ty)
freeze b = BlockN <$> B.freeze (unMBlock b)

toCount :: forall n ty . (KnownNat n, Countable ty n) => CountOf ty
toCount = natValCountOf (Proxy @n)

-- | Get a Ptr pointing to the data in the Block.
--
-- Since a Block is immutable, this Ptr shouldn't be
-- to use to modify the contents
--
-- If the Block is pinned, then its address is returned as is,
-- however if it's unpinned, a pinned copy of the Block is made
-- before getting the address.
withPtr :: (PrimMonad prim, KnownNat n)
        => BlockN n ty
        -> (Ptr ty -> prim a)
        -> prim a
withPtr b = B.withPtr (unBlock b)

-- | Same as 'withMutablePtr' but allow to specify 2 optimisations
-- which is only useful when the MutableBlock is unpinned and need
-- a pinned trampoline to be called safely.
--
-- If skipCopy is True, then the first copy which happen before
-- the call to 'f', is skipped. The Ptr is now effectively
-- pointing to uninitialized data in a new mutable Block.
--
-- If skipCopyBack is True, then the second copy which happen after
-- the call to 'f', is skipped. Then effectively in the case of a
-- trampoline being used the memory changed by 'f' will not
-- be reflected in the original Mutable Block.
--
-- If using the wrong parameters, it will lead to difficult to
-- debug issue of corrupted buffer which only present themselves
-- with certain Mutable Block that happened to have been allocated
-- unpinned.
--
-- If unsure use 'withMutablePtr', which default to *not* skip
-- any copy.
withMutablePtrHint ::
     forall n ty prim a . (PrimMonad prim, KnownNat n)
  => Bool
     -- ^ hint that the buffer doesn't need to have the same value as the
     -- mutable block when calling f
  -> Bool
     -- ^ hint that the buffer is not supposed to be modified by call of f
  -> MutableBlockN n ty (PrimState prim)
  -> (Ptr ty -> prim a)
  -> prim a
withMutablePtrHint skipCopy skipCopyBack (MutableBlockN mb) =
  B.withMutablePtrHint skipCopy skipCopyBack mb
