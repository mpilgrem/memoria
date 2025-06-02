{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

-- |
-- Module      : Basement.UArray.Mutable -- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : portable
--
-- A simple array abstraction that allow to use typed
-- array of bytes where the array is pinned in memory
-- to allow easy use with Foreign interfaces, ByteString
-- and always aligned to 64 bytes.
--
module Basement.UArray.Mutable
  ( -- * Property queries
    mutableOffset
    -- * Reading and Writing cells
  , withMutablePtrHint
  ) where

import qualified Basement.Block.Base as Base ( withMutablePtrHint )
import           Basement.FinalPtr ( withFinalPtr )
import           Basement.Monad ( PrimMonad (..) )
import           Basement.PrimType ( PrimType (..) )
import           Basement.Types.OffsetSize
import           Basement.UArray.Base hiding (empty)
import           Data.Proxy ( Proxy (..) )
import           GHC.Ptr
import           Prelude ( Bool, ($) )

mutableOffset :: MUArray ty st -> Offset ty
mutableOffset (MUArray ofs _ _) = ofs

withMutablePtrHint :: forall ty prim a . (PrimMonad prim, PrimType ty)
                   => Bool
                   -> Bool
                   -> MUArray ty (PrimState prim)
                   -> (Ptr ty -> prim a)
                   -> prim a
withMutablePtrHint skipCopy skipCopyBack (MUArray start _ back) f =
  case back of
    MUArrayAddr fptr -> withFinalPtr fptr (\ptr -> f (ptr `plusPtr` os))
    MUArrayMBA mb -> Base.withMutablePtrHint skipCopy skipCopyBack mb $ \ptr ->
      f (ptr `plusPtr` os)
 where
  sz           = primSizeInBytes (Proxy :: Proxy ty)
  !(Offset os) = offsetOfE sz start
