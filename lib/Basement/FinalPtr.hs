{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE TypeFamilies       #-}

-- |
-- Module      : Basement.FinalPtr
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : portable
--
-- A smaller ForeignPtr reimplementation that work in any prim monad.
--
-- Here be dragon.
--
module Basement.FinalPtr
    ( FinalPtr (..)
    , castFinalPtr
    , withFinalPtr
    , withUnsafeFinalPtr
    ) where

import           Basement.Monad
import           Control.Monad.ST ( runST )
import           GHC.IO
import           GHC.Ptr
import qualified GHC.ForeignPtr as GHCF
import           Prelude
                   ( Bool (..), Applicative (..), Eq (..), Ord (..)
                   , Ordering (..), Show (..), ($), (.)
                   )

-- | Create a pointer with an associated finalizer
data FinalPtr a = FinalPtr (Ptr a)
                | FinalForeign (GHCF.ForeignPtr a)
instance Show (FinalPtr a) where
    show f = runST $ withFinalPtr f (pure . show)
instance Eq (FinalPtr a) where
    (==) f1 f2 = runST (equal f1 f2)
instance Ord (FinalPtr a) where
    compare f1 f2 = runST (compare_ f1 f2)

-- | Cast a finalized pointer from type a to type b
castFinalPtr :: FinalPtr a -> FinalPtr b
castFinalPtr (FinalPtr a)     = FinalPtr (castPtr a)
castFinalPtr (FinalForeign a) = FinalForeign (GHCF.castForeignPtr a)

-- | Looks at the raw pointer inside a FinalPtr, making sure the
-- data pointed by the pointer is not finalized during the call to 'f'
withFinalPtr :: PrimMonad prim => FinalPtr p -> (Ptr p -> prim a) -> prim a
withFinalPtr (FinalPtr ptr) f = do
    r <- f ptr
    primTouch ptr
    pure r
withFinalPtr (FinalForeign fptr) f = do
    r <- f (GHCF.unsafeForeignPtrToPtr fptr)
    unsafePrimFromIO (GHCF.touchForeignPtr fptr)
    pure r
{-# INLINE withFinalPtr #-}

-- | Unsafe version of 'withFinalPtr'
withUnsafeFinalPtr :: PrimMonad prim => FinalPtr p -> (Ptr p -> prim a) -> a
withUnsafeFinalPtr fptr f = unsafePerformIO (unsafePrimToIO (withFinalPtr fptr f))
{-# NOINLINE withUnsafeFinalPtr #-}

equal :: PrimMonad prim => FinalPtr a -> FinalPtr a -> prim Bool
equal f1 f2 =
    withFinalPtr f1 $ \ptr1 ->
    withFinalPtr f2 $ \ptr2 ->
        pure $ ptr1 == ptr2
{-# INLINE equal #-}

compare_ :: PrimMonad prim => FinalPtr a -> FinalPtr a -> prim Ordering
compare_ f1 f2 =
    withFinalPtr f1 $ \ptr1 ->
    withFinalPtr f2 $ \ptr2 ->
        pure $ ptr1 `compare` ptr2
{-# INLINE compare_ #-}
