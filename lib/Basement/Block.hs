{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

-- |
-- Module      : Basement.Block
-- License     : BSD-style
-- Maintainer  : Haskell Foundation
--
-- A block of memory that contains elements of a type,
-- very similar to an unboxed array but with the key difference:
--
-- * It doesn't have slicing capability (no cheap take or drop)
-- * It consume less memory: 1 Offset, 1 CountOf
-- * It's unpackable in any constructor
-- * It uses unpinned memory by default
--
module Basement.Block
    ( -- * Lowlevel functions
      freeze
    , unsafeCast
    ) where

import           Basement.Block.Base
                   ( Block (..), MutableBlock, unsafeFreeze, unsafeNew )
import qualified Basement.Block.Base as M
import           Basement.Compat.Primitive ( PinnedStatus (..) )
import           Basement.Monad
import           Basement.PrimType

-- | Freeze a MutableBlock into a Block, copying all the data
--
-- If the data is modified in the mutable block after this call, then
-- the immutable Block resulting is not impacted.
freeze :: (PrimType ty, PrimMonad prim) => MutableBlock ty (PrimState prim) -> prim (Block ty)
freeze ma = do
    ma' <- unsafeNew Unpinned len
    M.unsafeCopyBytes ma' 0 ma 0 len
    --M.copyAt ma' (Offset 0) ma (Offset 0) len
    unsafeFreeze ma'
  where
    len = M.mutableLengthBytes ma

-- | Unsafely recast an UArray containing 'a' to an UArray containing 'b'
--
-- The offset and size are converted from units of 'a' to units of 'b',
-- but no check are performed to make sure this is compatible.
--
-- use 'cast' if unsure.
unsafeCast :: PrimType b => Block a -> Block b
unsafeCast (Block ba) = Block ba
